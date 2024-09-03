##################################################################################    
# SINGLE SAMPLE
# UI changes
# calculations
# graphs (sample, describe, infer)
# report (sample, describe, infer)
#    

# when we change one of the evidence options, we don't make a new sample, 
#  we just re-analyse the existing sample
onlyAnalysis<-FALSE
observeEvent(c(input$Welch,input$Transform,input$evidenceCaseOrder,input$analysisType,input$dataType,input$rInteractionOn),{
  onlyAnalysis<<-TRUE
},priority=100)
observeEvent(c(input$EvidencenewSample),{
  onlyAnalysis<<-FALSE
},priority=100)

# UI changes
# go to the sample tabs 
sampleUpdate<-observeEvent(c(input$Single,input$EvidencenewSample,input$EvidenceHypothesisApply),{
  if (any(c(input$Single,input$EvidencenewSample))>0) {
    if (!is.element(input$Graphs,c("Sample","Describe","Infer","Likelihood")))
     updateTabsetPanel(session, "Graphs", selected = "Sample")
    if (!is.element(input$Reports,c("Sample","Describe","Infer","Likelihood")))
        updateTabsetPanel(session, "Reports",
                        selected = "Sample")
  }
}
)

# if we are analysing actual data and press the new sample button
# then we do a resample of the data
doResample<-FALSE
whichAnalysisSample<-observeEvent(input$EvidencenewSample,{
  doResample<<-TRUE
},priority=100)
whichAnalysisApply<-observeEvent(input$EvidenceHypothesisApply,{
  doResample<<-FALSE
},priority=100)

# single sample calculations
doSampleAnalysis<-function(){
  if (debug) debugPrint(". doSampleAnalysis")
  
    if (!is.null(braw.def$hypothesis$IV2) && (braw.def$hypothesis$IV$type=="Ordinal" || braw.def$hypothesis$IV2$type=="Ordinal")) {
      if (warnOrd==FALSE) {
        hmm("Ordinal IVs will be treated as Interval")
        warnOrd<<-TRUE
      }
    }
  if (!onlyAnalysis) doSample()
  result<-doAnalysis()
  if (debug) debugPrint(". doSampleAnalysis - exit")
  
  result
}

# eventReactive wrapper
sampleAnalysis<-eventReactive(c(input$EvidenceHypothesisApply,input$EvidencenewSample,
                                input$Welch,input$Transform,input$evidenceCaseOrder,
                                input$analysisType,input$dataType,input$rInteractionOn,
                                input$world_distr,input$world_distr_k,input$world_distr_rz,input$world_distr_Nullp
                                ),{
  if (any(c(input$EvidenceHypothesisApply,input$EvidencenewSample)>0)){
    
    assign("hypothesis",updateHypothesis(),braw.def)
    assign("design",updateDesign(),braw.def)
    assign("evidence",updateEvidence(),braw.def)

    if (switches$showProgress)
      showNotification("Sample: starting",id="counting",duration=Inf,closeButton=FALSE,type="message")
    result<-doSampleAnalysis()

    # set the result into likelihood: populations
    if (!is.na(result$rIV)) {
      updateNumericInput(session,"possiblePSampRho",value=result$rIV)
      updateNumericInput(session,"possibleSampRho",value=result$rIV)
    }
    if (switches$showProgress)  removeNotification(id = "counting")
    
  } else {
    result<-NULL
  }
  return(result)
})


# SINGLE graphs
# single sample graph
makeSampleGraph <- function () {
  doIt<-editVar$data

  # make the sample
  result<-sampleAnalysis()
  if (is.null(result))  {return(ggplot()+braw.env$blankTheme())}
  
  # draw the sample
  g<-showSample(result)
  return(g)
}

# single descriptive graph
makeDescriptiveGraph <- function(){
  doIt<-editVar$data

  # make the sample
  result<-sampleAnalysis()
  if (is.null(result))  {return(ggplot()+braw.env$blankTheme())}
  
  # draw the description
  g<-showDescription(result)
  return(g)
}

# single inferential graph
makeInferentialGraph <- function() {
  doit<-c(input$EvidenceInfer_type,input$evidenceTheory,
          input$Welch,input$Transform,input$evidenceCaseOrder,input$analysisType,input$dataType,input$rInteractionOn)
  doIt<-editVar$data
  
  result<-sampleAnalysis()
  if (is.null(result))  {return(ggplot()+braw.env$blankTheme())}
  
  showType<-input$EvidenceInfer_type
  if (showType=="Custom") showType<-paste0(input$EvidenceInfer_par1,";",input$EvidenceInfer_par2)
  
  g<-showInference(result,showType=showType,dimension=input$EvidenceSingleDim,effectType=input$EvidenceEffect_type,showTheory=input$evidenceTheory)
  return(g)
}

# single likelihood graph
makeLikelihoodGraph <- function() {
  doit<-c(input$evidenceTheory,input$Transform,
          input$world_distr,input$world_distr_k,input$world_distr_rz,input$world_distr_Nullp,
          input$Prior_distr,input$Prior_distr_k,input$Prior_distr_rz,input$Prior_distr_Nullp)
doIt<-editVar$data

  result<-sampleAnalysis()
  if (is.null(result))  {return(ggplot()+braw.env$blankTheme())}
  
  evidence<-updateEvidence()
  g<-showPossible(
    doPossible(
      makePossible(
        typePossible=input$likelihoodType,
        targetSample<-result$rIV,
        hypothesis=updateHypothesis(),design=updateDesign(),
        UsePrior=input$likelihoodUsePrior,
        prior=evidence$prior
        )
      ),
    cutaway=(input$possible_cutaway=="cutaway")
    )
  return(g)
}

output$SamplePlot <- renderPlot({
  if (debug) debugPrint("SamplePlot")
  doIt<-editVar$data
  g<-makeSampleGraph()
  if (debug) debugPrint("SamplePlot - exit")
  g
})

output$DescriptivePlot <- renderPlot({
  if (debug) debugPrint("DescriptivePlot")
  doIt<-editVar$data
  g<-makeDescriptiveGraph()
  if (debug) debugPrint("DescriptivePlot - exit")
  g
})

output$InferentialPlot <- renderPlot({
  if (debug) debugPrint("InferentialPlot")
  doIt<-editVar$data
  g<-makeInferentialGraph()
  if (debug) debugPrint("InferentialPlot - exit")
  g
})

output$LikelihoodPlot <- renderPlot({
  if (debug) debugPrint("LikelihoodPlot")
  doIt<-editVar$data
  g<-makeLikelihoodGraph()
  if (debug) debugPrint("LikelihoodPlot - exit")
  g
})

output$SamplePlot1 <- renderPlot({
  if (debug) debugPrint("SamplePlot")
  doIt<-editVar$data
  g<-makeSampleGraph()
  if (debug) debugPrint("SamplePlot - exit")
  g
})

output$DescriptivePlot1 <- renderPlot({
  if (debug) debugPrint("DescriptivePlot")
  doIt<-editVar$data
  g<-makeDescriptiveGraph()
  if (debug) debugPrint("DescriptivePlot - exit")
  g
})

output$InferentialPlot1 <- renderPlot({
  if (debug) debugPrint("InferentialPlot")
  doIt<-editVar$data
  g<-makeInferentialGraph()
  if (debug) debugPrint("InferentialPlot - exit")
  g
})

output$LikelihoodPlot1 <- renderPlot({
  if (debug) debugPrint("LikelihoodPlot")
  doIt<-editVar$data
  g<-makeLikelihoodGraph()
  if (debug) debugPrint("LikelihoodPlot - exit")
  g
})

# SINGLE reports    
# single sample report
makeSampleReport <- function()  {
  doIt<-editVar$data

  result<-sampleAnalysis()        
  if (is.null(result))  {return(ggplot()+braw.env$blankTheme())}
  
  g<-reportSample(result)
  g
}

# single descriptive report
makeDescriptiveReport <- function()  {
  doIt<-c(editVar$data,input$input$evidenceCaseOrder,input$rInteractionOn,input$dataType)

  result<-sampleAnalysis()
  if (is.null(result))  {return(ggplot()+braw.env$blankTheme())}

  g<-reportDescription(result)
  g
}

# single inferential report
makeInferentialReport <- function()  {
  doIt<-c(editVar$data,input$Welch,input$Transform,input$evidenceCaseOrder,input$analysisType,input$dataType,input$rInteractionOn)
  
  result<-sampleAnalysis()
  if (is.null(result))  {return(ggplot()+braw.env$blankTheme())}
  
  g<-reportInference(result,analysisType = input$analysisType)        
  g
}

# single likelihood report
makeLikelihoodReport <- function() {
  doit<-c(input$evidenceTheory,input$Transform,
          input$world_distr,input$world_distr_k,input$world_distr_rz,input$world_distr_Nullp,
          input$Prior_distr,input$Prior_distr_k,input$Prior_distr_rz,input$Prior_distr_Nullp)
  doIt<-editVar$data
  
  result<-sampleAnalysis()
  if (is.null(result))  {return(ggplot()+braw.env$blankTheme())}
  
  evidence<-updateEvidence()
  g<-reportLikelihood(
    doPossible(
      makePossible(
        typePossible="Populations",
        targetSample<-result$rIV,
        hypothesis=updateHypothesis(),design=updateDesign(),
        UsePrior=input$likelihoodUsePrior,
        prior=evidence$prior
      )
    )
  )
  return(g)
}


output$SampleReport <- renderPlot({
  if (debug) debugPrint("SampleReport")
  doIt<-editVar$data
  g<-makeSampleReport()
  if (debug) debugPrint("SampleReport - exit")
  g
})

output$DescriptiveReport <- renderPlot({
  if (debug) debugPrint("DescriptiveReport")
  doIt<-editVar$data
  g<-makeDescriptiveReport()
  if (debug) debugPrint("DescriptiveReport - exit")
  g
})

output$InferentialReport <- renderPlot({
  if (debug) debugPrint("InferentialReport")
  doIt<-editVar$data
  g<-makeInferentialReport()
  if (debug) debugPrint("InferentialReport - exit")
  g
})

output$LikelihoodReport <- renderPlot({
  if (debug) debugPrint("InferentialReport")
  doIt<-editVar$data
  g<-makeLikelihoodReport()
  if (debug) debugPrint("InferentialReport - exit")
  g
})

output$SampleReport1 <- renderPlot({
  if (debug) debugPrint("SampleReport")
  doIt<-editVar$data
  g<-makeSampleReport()
  if (debug) debugPrint("SampleReport - exit")
  g
})

output$DescriptiveReport1 <- renderPlot({
  if (debug) debugPrint("DescriptiveReport")
  doIt<-editVar$data
  g<-makeDescriptiveReport()
  if (debug) debugPrint("DescriptiveReport - exit")
  g
})

output$InferentialReport1 <- renderPlot({
  if (debug) debugPrint("InferentialReport")
  doIt<-editVar$data
  g<-makeInferentialReport()
  if (debug) debugPrint("InferentialReport - exit")
  g
})

output$LikelihoodReport1 <- renderPlot({
  if (debug) debugPrint("InferentialReport")
  doIt<-editVar$data
  g<-makeLikelihoodReport()
  if (debug) debugPrint("InferentialReport - exit")
  g
})

##################################################################################    
