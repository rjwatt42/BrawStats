##################################################################################    
# SYSTEM diagrams   
# hypothesis diagram
# population diagram
# prediction diagram

output$HypothesisPlot<-renderPlot({
  doIt<-c(editVar$data,input$WhiteGraphs,input$RZ)
  if (debug) debugPrint("HypothesisPlot")
  
  hypothesis<-updateHypothesis()
  if (is.null(hypothesis$IV2))
    g<-showHypothesis(hypothesis,xgain=1)
  else 
    g<-showHypothesis(hypothesis,xgain=0.63)
  
  if (debug) debugPrint("HypothesisPlot - exit")
  g
}
)

# world diagram
output$WorldPlot<-renderPlot({
  doIt<-c(editVar$data,input$WhiteGraphs,input$RZ)
  if (debug) debugPrint("WorldPlot")
  
  hypothesis<-updateHypothesis()
  g<-showWorld(hypothesis)
  
  if (debug) debugPrint("WorldPlot - exit")
  g
}
)


output$DesignPlot<-renderPlot({
  doIt<-c(editVar$data,input$WhiteGraphs)
  design<-updateDesign()
  if (debug) debugPrint("WorldPlot2")
  
  g<-showDesign(design)
  
  if (debug) debugPrint("WorldPlot2 - exit")
  g
}
)

# population diagram
output$PopulationPlot <- renderPlot({
  doIt<-c(editVar$data,input$WhiteGraphs,input$RZ)
  if (debug) debugPrint("PopulationPlot")

  hypothesis<-updateHypothesis()
  g<-showPopulation(hypothesis)
  
  if (debug) debugPrint("PopulationPlot - exit")
  g
})  

# prediction diagram
output$PredictionPlot <- renderPlot({
  doIt<-c(editVar$data,input$WhiteGraphs,input$RZ)
  if (debug) debugPrint("PredictionPlot")

  hypothesis<-updateHypothesis()
  design<-updateDesign()
  evidence<-updateEvidence()
  g<-showPrediction(hypothesis,design,evidence)
  
  if (debug) debugPrint("PredictionPlot - exit")
  g
})  
##################################################################################    
