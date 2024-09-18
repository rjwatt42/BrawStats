#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


debug<-FALSE
debugExitOnly<-FALSE
debugMainOnly<-TRUE
debugMem<-FALSE

debugNow<-Sys.time()
debugNowLocation<<-"Start"
debugStart<-Sys.time()
if(debugMem) memUse<-sum(gc()[,2])


debugPrint<-function(s) {
  if (debugMainOnly && substr(s,1,1)==" ") return()
  elapsedStart<-Sys.time()-debugStart
  
  z<-regexpr("[ ]*[a-zA-Z0-9]*",s)
  caller<-regmatches(s,z)
  
  if (grepl("exit",s)==1) {
      use<-which(debugNowLocation==caller)
      use<-use[length(use)]
      elapsedLocal<-as.numeric(difftime(Sys.time(),debugNow[use],units="secs"))
      # str<-paste(format(elapsedStart),s," (",format(elapsedLocal),") ")
      str<-paste(s," (",format(elapsedLocal),") ")
      if(debugMem)  str<-paste(str,"  ",format(sum(gc()[,2])-memUse[use]))
      print(str)
  } else {
    if (!debugExitOnly || s=="Opens")
      # print(paste(format(elapsedStart),s))
        print(s)
    debugNow<<-c(debugNow,Sys.time())
    debugNowLocation<<-c(debugNowLocation,caller)
    if(debugMem)  memUse<<-c(memUse,sum(gc()[,2]))
  }
  }

#because numericInput with "0." returns NA
checkNumber<-function(a,b=a,c=0) {
  if (!isempty(a)) {
    if (is.na(a) || is.null(a)) {a<-c}
  }
  a
}

if (debug) debugPrint("Opens")

####################################

shinyServer(function(input, output, session) {
  if (debug) debugPrint("Start")
  
  # a variable that is used to trigger re-analysis
  editVar<-reactiveValues(data=0)
  
####################################
# BASIC SET UP that cannot be done inside ui.R  
  
  # controls for when data is imported
  shinyjs::hideElement(id= "EvidenceHypothesisApply")
  shinyjs::hideElement(id= "Using")

####################################
# ui fancy stuff
  
  if (debug) debugPrint("ServerKeys")
  
  source("uiExtras.R")
  source("serverKeys.R",local=TRUE)
  source("serverQuickHypotheses.R")
  source("serverTypeCombinations.R",local=TRUE)
  
  observeEvent(input$LoadExtras, {
                 loadExtras(session,input,input$LoadExtras)
               })
  
  observeEvent(c(input$LargeGraphs,input$WhiteGraphs), {
    currentGraph<-input$Graphs
    currentReport<-input$Reports
    
    BrawOpts(BW=input$WhiteGraphs,fontScale=globalFontScale*(1+input$LargeGraphs),reducedOutput = TRUE,reportHTML=braw.env$reportHTML)

    if (input$LargeGraphs) {
      output$mainColumns <- renderUI({
        tagList(
          column(width=12,
                 style = paste("margin-left: 4px;padding-left: 0px;margin-right: -10px;padding-right: -10px;"),
                 MainGraphs1(),
                 MainReports1()
          )
        )
      }
      )
      hideElement("HypothesisPopulation")
    } else {
      output$mainColumns <- renderUI({
        tagList(
          column(width=4, id="HypothesisPopulation",
                 style = paste("margin-left: 4px;padding-left: 0px;margin-right: -10px;padding-right: -10px;"),
                 HypothesisDiagram(),
                 PopulationDiagram()
          ),
          column(width=8,
                 style = paste("margin-left: 4px;padding-left: 0px;margin-right: -10px;padding-right: -10px;"),
                 MainGraphs(),
                 MainReports()
          )
        )
      }
      )
      showElement("HypothesisPopulation")
    }
    updateTabsetPanel(session, "Graphs",selected = currentGraph)
    updateTabsetPanel(session, "Reports",selected = currentReport)
  })
  
####################################
# other housekeeping
  if (debug) debugPrint("Housekeeping")

  # make the sample size/power labels appropriate
  observeEvent(input$sN, {
    before<-paste0("<div style='",localStyle,"'>")
    after<-"</div>"
    n<-input$sN
    if (!is.null(n) && !is.na(n)) {
      if (n<1 && n>0) {
        html("sNLabel",paste0(before,"Sample Power:",after))
      } else {
        html("sNLabel",paste0(before,"Sample Size:",after))
      }
    }
  }
  )
  
  observeEvent(input$Hypothesis,{
    if (input$Hypothesis=="World") {
      updateTabsetPanel(session,"HypothesisDiagram",selected = "World")
    }
  })
  
  observeEvent(input$Evidence,{
    if (input$Evidence=="Multiple") {
      updateTabsetPanel(session,"Graphs",selected = "Expect")
      updateTabsetPanel(session,"Reports",selected = "Expect")
    }
  })
  
  observeEvent(input$Explore,{
    if (input$Explore!="Explore") {
      updateTabsetPanel(session,"Graphs",selected = "Explore")
      updateTabsetPanel(session,"Reports",selected = "Explore")
    }
  })
  
  observeEvent(c(input$STMethod,input$alpha), {
    assign("STMethod",input$STMethod,braw.env)
    assign("alphaSig",input$alpha,braw.env)
  })

  observeEvent(c(input$pScale,input$wScale,input$nScale),{
    assign("pPlotScale",input$pScale,braw.env)
    assign("wPlotScale",input$wScale,braw.env)
    assign("nPlotScale",input$nScale,braw.env)
  })
  
  observeEvent(input$RZ,{
    assign("RZ",input$RZ,braw.env)
  })
  

####################################
# generic warning dialogue
  
  hmm<-function (cause) {
    showModal(
      modalDialog(style = paste("background: ",subpanelcolours$hypothesisC,";",
                                "modal {background-color: ",subpanelcolours$hypothesisC,";}"),
                  title="Careful now!",
                  size="s",
                  cause,
                  
                  footer = tagList( 
                    actionButton("MVproceed", "OK")
                  )
      )
    )
  }
  
  observeEvent(input$MVproceed, {
    removeModal()
  })
  
####################################
# QUICK HYPOTHESES
  
  if (debug) debugPrint("QuickHypotheses")
  
  observeEvent(input$Hypchoice,{
    newHyp<-getTypecombination(input$Hypchoice)

    updateSelectInput(session,"IVtype",selected=newHyp$IV$type)
    updateNumericInput(session,"IVncats",value=newHyp$IV$ncats)
    updateTextInput(session,"IVcases",value=newHyp$IV$cases)
    updateTextInput(session,"IVprop",value=newHyp$IV$proportions)
    updateSelectInput(session,"sIV1Use",selected=newHyp$IV$deploy)
    
    if (!is.null(newHyp$IV2)) {
      updateTextInput(session,"IV2name",value="IV2new")
      updateSelectInput(session,"IV2choice",selected="IV2")
      updateSelectInput(session,"IV2type",selected=newHyp$IV2$type)
      updateNumericInput(session,"IV2ncats",value=newHyp$IV2$ncats)
      updateTextInput(session,"IV2cases",value=newHyp$IV2$cases)
      updateTextInput(session,"IV2prop",value=newHyp$IV2$proportions)
      updateSelectInput(session,"sIV2Use",selected=newHyp$IV2$deploy)
      updateTextInput(session,"IV2name",value="IV2")
    } else {
      updateSelectInput(session,"IV2choice",selected="none")
    }
    
    updateSelectInput(session,"DVtype",selected=newHyp$DV$type)
    updateNumericInput(session,"DVncats",value=newHyp$DV$ncats)
    updateTextInput(session,"DVcases",value=newHyp$DV$cases)
    updateTextInput(session,"DVprop",value=newHyp$DV$proportions)
    
    # 3 variable hypotheses look after themselves
    #
    if (!is.null(newHyp$IV2)) {
      editVar$data<<-editVar$data+1
    }    
  })
  
  observeEvent(input$Effectchoice,{
    switch (input$Effectchoice,
            "e0"={
              updateNumericInput(session,"rIV",value=0)    
              updateNumericInput(session,"rIV2",value=0)    
              updateNumericInput(session,"rIVIV2",value=0)    
              updateNumericInput(session,"rIVIV2DV",value=0)    
            },
            "e1"={
              updateNumericInput(session,"rIV",value=0.3)    
              updateNumericInput(session,"rIV2",value=-0.3)    
              updateNumericInput(session,"rIVIV2",value=0.0)    
              updateNumericInput(session,"rIVIV2DV",value=0.5)    
            },
            "e2"={
              updateNumericInput(session,"rIV",value=0.2)    
              updateNumericInput(session,"rIV2",value=0.4)    
              updateNumericInput(session,"rIVIV2",value=-0.8)    
              updateNumericInput(session,"rIVIV2DV",value=0.0)    
            }
    )
    
  })
  
####################################
# VARIABLES  
  if (debug) debugPrint("Variables")

  editVar<-reactiveValues(data=0)
  
  source("sourceInspectVariables.R",local=TRUE)

  source("sourceUpdateSystem.R",local=TRUE)
  source("sourceSystemDiagrams.R",local=TRUE)
  
  source("sourceSingle.R",local=TRUE)
  source("sourceExpected.R",local=TRUE)
  
  source("sourceExplore.R",local=TRUE)
  
  # source("sourcePossible.R",local=TRUE)
  source("sourceFiles.R",local=TRUE)
  # end of everything        
  
  if (debug) debugPrint("Opens - exit")
})

