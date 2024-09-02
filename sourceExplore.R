# EXPLORE    
# UI changes  
# set explore variable from UI
# calculations
# outputs (graph and report)

# local variables

runningExplore<-FALSE

# UI changes
# go to the explore tabs 
# and set the explore run valid
observeEvent(c(input$exploreRunH,input$exploreRunD,input$exploreRunA),{
  if (any(c(input$exploreRunH,input$exploreRunD,input$exploreRunA))>0) {
    updateTabsetPanel(session,"Graphs",selected = "Explore")
    updateTabsetPanel(session,"Reports",selected = "Explore")
    
    runningExplore<<-TRUE
  }
},priority=100)

# and watch for IV2 appearing
observeEvent(input$IV2choice,{
  if (switches$doWorlds) {
    use2<-hypothesisChoicesV2Extra
    use3<-hypothesisChoicesV3Extra
  }  else {
    use2<-hypothesisChoicesV2
    use3<-hypothesisChoicesV3
  }
  
  if (input$IV2choice=="none") {
    updateSelectInput(session,"Explore_typeH", choices=use2)
  }
  else {
    updateSelectInput(session,"Explore_typeH", choices=use3)
  }
})

observeEvent(c(input$Explore_typeD,input$Explore_minValD,input$Explore_maxValD,input$Explore_NPointsD,input$Explore_xlogD,
               input$Explore_typeH,input$Explore_minValH,input$Explore_maxValH,input$Explore_NPointsH,input$Explore_xlogH,
               input$Explore_typeA,input$Explore_minValA,input$Explore_maxValA,input$Explore_NPointsA,input$Explore_xlogA
),{
  runningExplore<<-FALSE
})

getStep<-function(val,np) {
  if (np!=13) return(1)
  if (val==0) return(0.1)
  return(10^(ceiling(log10(abs(val)))-1))
}

# watch for changes to hypothesis
observeEvent(input$Explore_typeH,{
  range<-getExploreRange(list(exploreType=input$Explore_typeH))

  updateNumericInput(session,"Explore_minValH",value=range$minVal,step=getStep(range$minVal,range$np))
  updateNumericInput(session,"Explore_maxValH",value=range$maxVal,step=getStep(range$maxVal,range$np))
  updateNumericInput(session,"Explore_NPointsH",value=range$np,step=1)
  updateCheckboxInput(session,"Explore_xlogH",value=range$logScale)
})

# watch for changes to design
observeEvent(input$Explore_typeD,{
  range<-getExploreRange(list(exploreType=input$Explore_typeD))

  updateNumericInput(session,"Explore_minValD",value=range$minVal,step=getStep(range$minVal,range$np))
  updateNumericInput(session,"Explore_maxValD",value=range$maxVal,step=getStep(range$maxVal,range$np))
  updateNumericInput(session,"Explore_NPointsD",value=range$np,step=1)
  updateCheckboxInput(session,"Explore_xlogD",value=range$logScale)
})

# watch for changes to design
observeEvent(input$Explore_typeA,{
  range<-getExploreRange(list(exploreType=input$Explore_typeA))

  updateNumericInput(session,"Explore_minValA",value=range$minVal,step=getStep(range$minVal,range$np))
  updateNumericInput(session,"Explore_maxValA",value=range$maxVal,step=getStep(range$maxVal,range$np))
  updateNumericInput(session,"Explore_NPointsA",value=range$np,step=1)
  updateCheckboxInput(session,"Explore_xlogA",value=range$logScale)
})

# set explore variable from UI    
# update explore values    
updateExplore<-function(){
    switch (input$Explore,
            "Hypothesis"={
              l<-list(exploreType=input$Explore_typeH,
                      exploreNPoints = input$Explore_NPointsH,
                      minVal=input$Explore_minValH,maxVal=input$Explore_maxValH,
                      xlog = input$Explore_xlogH
              )
            },
            "Design"={
              l<-list(exploreType=input$Explore_typeD,
                      exploreNPoints = input$Explore_NPointsD,
                      minVal=input$Explore_minValD,maxVal=input$Explore_maxValD,
                      xlog = input$Explore_xlogD
                      )
            },
            "Analysis"={
              l<-list(exploreType=input$Explore_typeA,
                      exploreNPoints = input$Explore_NPointsA,
                      minVal=input$Explore_minValA,maxVal=input$Explore_maxValA,
                      xlog = input$Explore_xlogA
              )
            }
    )
  explore<-makeExplore(exploreType=l$exploreType,
                       exploreNPoints=l$exploreNPoints,
                       minVal=l$minVal,maxVal=l$maxVal,
                       xlog=l$xlog)

  explore
} 
updateExploreShow<-function() {
  switch (input$Explore,
          "Hypothesis"={
            l<-list(showType=input$Explore_showH, 
                    par1=input$Explore_par1H,
                    par2=input$Explore_par2H,
                    dimension=input$Explore_dimH,
                    whichEffect=input$whichEffectH,
                    effectType=input$EvidenceEffect_type
            )
          },
          "Design"={
            l<-list(showType=input$Explore_showD, 
                    par1=input$Explore_par1D,
                    par2=input$Explore_par2D,
                    dimension=input$Explore_dimD,
                    whichEffect=input$whichEffectD
            )
            },
            "Analysis"={
              l<-list(showType=input$Explore_showA, 
                      par1=input$Explore_par1A,
                      par2=input$Explore_par2A,
                      dimension=input$Explore_dimA,
                      whichEffect=input$whichEffectA,
                      nsims=as.numeric(input$Explore_lengthA)
              )
            }
  )
  return(l)
}
# Main calculations    
makeExploreResult <- function() {
  doit<-c(input$Explore_showH,input$Explore_showD,input$Explore_showA,
          input$STMethod,input$alpha,input$likelihoodUsePrior)

  if (runningExplore) {
    if (debug) {debugPrint(". makeExpectedResult - start")}
    
  oldHypothesis<-braw.def$hypothesis
  hypothesis<-updateHypothesis()
  assign("hypothesis",hypothesis,braw.def)
  if (!is.equalLists(oldHypothesis,hypothesis)) assign("explore",NULL,braw.res)
  
  oldDesign<-braw.def$design
  design<-updateDesign()
  assign("design",design,braw.def)
  if (!is.equalLists(oldDesign,design)) assign("explore",NULL,braw.res)
  
  oldEvidence<-braw.def$evidence
  evidence<-updateEvidence()
  assign("evidence",evidence,braw.def)
  if (!is.equalLists(oldEvidence,evidence)) assign("explore",NULL,braw.res)
  
  oldExplore<-braw.def$explore
  explore<-updateExplore()
  assign("explore",explore,braw.def)
  if (!is.equalLists(oldExplore,explore)) assign("explore",NULL,braw.res)

  switch (input$Explore,
          "Hypothesis"={nsims<-input$Explore_lengthH},
          "Design"={nsims<-input$Explore_lengthD},
          "Analysis"={nsims<-input$Explore_lengthA}
  )
  
  # doingNull<- (explore$showType=="NHSTErrors" && 
  #                (!hypothesis$effect$world$worldOn || (hypothesis$effect$world$worldOn && hypothesis$effect$world$populationNullp==0)))

  if (switches$showProgress) {
    if (is.null(braw.res$explore))
      showNotification(paste0("Expected: starting (",nsims,")"),id="counting",duration=Inf,closeButton=FALSE,type="message")
    else 
      showNotification(paste0("Expected: adding (",nsims,")"),id="counting",duration=Inf,closeButton=FALSE,type="message")
  }
  exploreResult<-doExplore(explore,nsims=nsims,exploreResult=braw.res$explore,doingNull=TRUE)
  if (switches$showProgress) removeNotification(id = "counting")
  runningExplore<<-FALSE
  
  } else exploreResult<-braw.res$explore
  
  return(exploreResult)
}

# graph explore analysis        
makeExploreGraph<-function() {
  
  exploreResult<-makeExploreResult()
  if (is.null(exploreResult)) return(ggplot()+braw.env$blankTheme())

  exploreShow<-updateExploreShow()
  if (exploreShow$showType=="Custom") 
    exploreShow$showType<-paste0(exploreShow$par1,";",exploreShow$par2)
  g<-showExplore(exploreResult,showType=exploreShow$showType,
                 dimension=exploreShow$dimension,
                 whichEffect=exploreShow$whichEffect,
                 effectType=exploreShow$effectType
    )
  return(g)
}

output$ExplorePlot <- renderPlot( {
  doIt<-c(input$exploreRunH,input$exploreRunD,input$exploreRunA)
  g<-makeExploreGraph()
  
  g  
})

output$ExplorePlot1 <- renderPlot( {
  doIt<-c(input$exploreRunH,input$exploreRunD,input$exploreRunA)
  g<-makeExploreGraph()
  
  g  
})

# report explore analysis        
makeExploreReport<-function() {
  
  exploreResult<-makeExploreResult()
  if (is.null(exploreResult)) return(ggplot()+braw.env$blankTheme())
  
  exploreShow<-updateExploreShow()
  if (exploreShow$showType=="Custom") exploreShow$showType<-paste0(exploreShow$par1,";",exploreShow$par2)
  g<-reportExplore(exploreResult,showType=exploreShow$showType,
                 whichEffect=exploreShow$whichEffect,effectType=exploreShow$effectType
  )
  return(g)
}

output$ExploreReport <- renderPlot({
  doIt<-c(input$exploreRunH,input$exploreRunD,input$exploreRunA,
          input$STMethod,input$alpha)
  makeExploreReport()
})
output$ExploreReport1 <- renderPlot({
  doIt<-c(input$exploreRunH,input$exploreRunD,input$exploreRunA,
          input$STMethod,input$alpha)
  makeExploreReport()
})

##################################################################################    
