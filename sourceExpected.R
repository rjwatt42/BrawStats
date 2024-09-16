##################################################################################    
# EXPECTED    
# UI changes  
# set expected variable from UI
# calculations
# outputs (2 graphs and report)
# 

runningExpected<-FALSE

# here's where we start a run
observeEvent(c(input$EvidenceExpectedRun),{
  if (input$EvidenceExpectedRun>0) {
    runningExpected<<-TRUE
    updateTabsetPanel(session,"Graphs",selected = "Expect")
    updateTabsetPanel(session,"Reports",selected = "Expect")
  }
},priority=100
)

# set expected variable from UI

updateExpectedShow<-function(){
  expectedShow<-list(
    nsims=input$EvidenceExpected_length,
    showType=input$EvidenceExpected_type,
    par1=input$EvidenceExpected_par1,
    par2=input$EvidenceExpected_par2,
    dimension=input$EvidenceExpectedDim,
    whichEffect=input$whichEffectExpected,
    effectType=input$EvidenceEffect_type
  )
  expectedShow
}    

# Expected outputs
# show expected result    
makeExpectedResult <- function() {
  doit<-c(input$EvidenceExpected_type,input$EvidenceExpected_par1,input$EvidenceExpected_par2,
          input$EvidenceEffect_type,
          input$evidenceTheory,
          input$STMethod,input$alpha,
          input$world_distr,input$world_distr_rz,input$world_distr_k,input$world_distr_Nullp,
          input$EvidenceExpectedRun)
  
  if (runningExpected) {
    if (debug) {debugPrint(". makeExpectedResult - start")}

    oldHypothesis<-braw.def$hypothesis
    hypothesis<-updateHypothesis()
    assign("hypothesis",hypothesis,braw.def)
    if (!is.equalLists(oldHypothesis,hypothesis)) assign("expected",NULL,braw.res)

    oldDesign<-braw.def$design
    design<-updateDesign()
    assign("design",design,braw.def)
    if (!is.equalLists(oldDesign,design)) assign("expected",NULL,braw.res)

    oldEvidence<-braw.def$evidence
    evidence<-updateEvidence()
    assign("evidence",evidence,braw.def)
    if (!is.equalLists(oldEvidence,evidence)) assign("expected",NULL,braw.res)

    nsims<-input$EvidenceExpected_length

    if (variablesHeld=="Data" && doResample && switches$doBootstrap) {design$sMethod<-"Resample"}
    
    if (switches$showProgress) {
      if (is.null(braw.res$expected))
        showNotification(paste0("Expected: starting (",nsims,")"),id="counting",duration=Inf,closeButton=FALSE,type="message")
      else 
        showNotification(paste0("Expected: adding (",nsims,")"),id="counting",duration=Inf,closeButton=FALSE,type="message")
    }
    expectedResult<-doExpected(nsims=nsims,expectedResult=braw.res$expected,
                               hypothesis=hypothesis,design=design,evidence=evidence)
    if (switches$showProgress) {removeNotification(id = "counting")}
    runningExpected<<-FALSE
    
  #   nsims<-expected$nsims
  #   if (is.null(expectedResult)) count<-0
  #   else count<-expectedResult$count
  #   
  #   min_ns<-max(1,floor(log10(nsims/100)))
  #   if (switches$showAnimation) {
  #     ns<-ceil(10^(floor(max(min_ns,log10(count)))))
  #   } else {
  #     if (switches$showProgress) {
  #       ns<-10^min_ns
  #       ns<-nsims
  #     } else {
  #       ns<-nsims
  #     }
  #   }
  #   if (count+ns>nsims) ns<-nsims-count
  # 
  #   if (ns>0) {
  #     doingNull<- (input$EvidenceExpected_type=="NHSTErrors" && 
  #                    (!hypothesis$effect$world$worldOn || (hypothesis$effect$world$worldOn && hypothesis$effect$world$populationNullp==0)))
      # if (switches$showProgress && count==0)
      #   showNotification("Expected: starting",id="counting",duration=Inf,closeButton=FALSE,type="message")
  #     expectedResult<-makeExpected(ns,expectedResult,hypothesis,design,evidence,doingNull)
  #     if (switches$showProgress) 
  #       showNotification(paste0("Expected: ",format(expectedResult$count),"/",format(nsims)),id="counting",duration=Inf,closeButton=FALSE,type="message")
  #   }
  #   
  #   # ? stop running
  #   if (expectedResult$count>=nsims) {
      # if (switches$showProgress) {removeNotification(id = "counting")}
  #     runningExpected<<-FALSE
  #   } else {
  #     invalidateLater(1)
  #   }
  } else expectedResult<-braw.res$expected
  return(expectedResult)
}


makeExpectedGraph <- function() {
  expectedResult<-makeExpectedResult()
  if (is.null(expectedResult)) return(ggplot()+braw.env$blankTheme())
  
  expectedShow<-updateExpectedShow()
  showType<-expectedShow$showType
  if (showType=="Custom") showType<-paste0(expectedShow$par1,";",expectedShow$par2)
  showExpected(expectedResult,showType=showType,dimension = expectedShow$dimension,
               whichEffect=expectedShow$whichEffect,effectType=expectedShow$effectType)
}

output$ExpectedPlot <- renderPlot({
  if (debug) {debugPrint("ExpectedPlot - start")}
  doit<-c(input$EvidenceExpected_type,input$EvidenceExpected_par1,input$EvidenceExpected_par2,
          input$EvidenceEffect_type,
          input$EvidenceExpectedRun)
  g<-makeExpectedGraph()
  if (debug) {debugPrint("ExpectedPlot - exit")}
  g
})

output$ExpectedPlot1 <- renderPlot({
  if (debug) {debugPrint("ExpectedPlot1 - start")}
  doit<-c(input$EvidenceExpected_type,input$EvidenceExpected_par1,input$EvidenceExpected_par2,
          input$EvidenceEffect_type,
          input$EvidenceExpectedRun)
  g<-makeExpectedGraph()
  if (debug) {debugPrint("ExpectedPlot1 - exit")}
  g
})

makeExpectedReport<-function() {
  expectedResult<-makeExpectedResult()
  if (is.null(expectedResult)) return(ggplot()+braw.env$blankTheme())
  
  expectedShow<-updateExpectedShow()
  showType<-expectedShow$showType
  if (showType=="Custom") showType<-paste0(expectedShow$par1,";",expectedShow$par2)
  reportExpected(expectedResult,showType=showType,whichEffect=expectedShow$whichEffect,effectType=expectedShow$effectType)
}

# expected report
output$ExpectedReport <- renderPlot({
  if (debug) debugPrint("ExpectedReport - start")
  doIt<-input$EvidenceExpectedRun
  g<-makeExpectedReport()
  if (debug) {debugPrint("ExpectedReport - exit")}
  g
})

output$ExpectedReport1 <- renderPlot({
  if (debug) debugPrint("ExpectedReport1 - start")
  doIt<-input$EvidenceExpectedRun
  g<-makeExpectedReport()
  if (debug) {debugPrint("ExpectedReport1 - exit")}
  g
})

##################################################################################    
