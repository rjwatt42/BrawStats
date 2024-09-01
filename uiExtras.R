

loadExtras<-function(session,input,addingExtras=TRUE,all=TRUE){
  
  if (!addingExtras && all) {
    # switches$doReplications<<-FALSE
    # removeTab("Design","Replicate",session)
    # 
    # switches$doWorlds<<-FALSE
    # removeTab("Hypothesis","World",session)
    # removeTab("HypothesisDiagram","World",session)
    # updateSelectInput(session,"possibleUseSource",choices=c("null","hypothesis","prior"))
    # updateSelectInput(session,"likelihoodUsePrior",choices=c("none","prior"))
    # 
    # switches$doCheating<<-FALSE
    # switches$doLikelihoodInfer<<-FALSE

    # updateSelectInput(session,"EvidenceInfer_type",choices=singleTypeChoices)
    # updateSelectInput(session,"EvidenceExpected_par1",choices=inferShowParams,selected="r")
    # updateSelectInput(session,"EvidenceExpected_par2",choices=inferShowParams,selected="p")
  } else {
    
    # replications
    if (!switches$doReplications) {
      switches$doReplications<<-TRUE
      insertTab("Design",replicationTab(),"Anomalies","after",select=FALSE,session)
    }
    
    # worlds
    if (!switches$doWorlds) {
      switches$doWorlds<<-TRUE
      insertTab("Hypothesis",worldPanel(),"Effects","after",select=FALSE,session)
      insertTab("HypothesisDiagram",worldDiagram(),"Hypothesis","after",select=FALSE,session)
      updateSelectInput(session,"possibleUseSource",choices=c("null","world","prior"))
      updateSelectInput(session,"likelihoodUsePrior",choices=c("none","world","prior"))
    }
    
    # cheating
    if (!switches$doCheating) {
      switches$doCheating<<-TRUE
    }
    # likelihood inferences
    if (!switches$doLikelihoodInfer) {
      switches$doLikelihoodInfer<<-TRUE
    }
    
    # explore
    updateSelectInput(session,"Explore_typeH",choices=hypothesisChoicesV3Extra)
    updateSelectInput(session,"Explore_typeD",choices=designChoicesExtra)
    
    updateSelectInput(session,"Explore_par1H",choices=exploreShowParamsExtra,selected=input$Explore_par1H)
    updateSelectInput(session,"Explore_par2H",choices=exploreShowParamsExtra,selected=input$Explore_par2H)
    updateSelectInput(session,"Explore_par1D",choices=exploreShowParamsExtra,selected=input$Explore_par1D)
    updateSelectInput(session,"Explore_par2D",choices=exploreShowParamsExtra,selected=input$Explore_par2D)
    
    updateSelectInput(session,"EvidenceInfer_par1",choices=inferShowParamsExtra,selected=input$EvidenceInfer_par1)
    updateSelectInput(session,"EvidenceInfer_par2",choices=inferShowParamsExtra,selected=input$EvidenceInfer_par2)
    updateSelectInput(session,"EvidenceExpected_par1",choices=inferShowParamsExtra,selected=input$EvidenceExpected_par1)
    updateSelectInput(session,"EvidenceExpected_par2",choices=inferShowParamsExtra,selected=input$EvidenceExpected_par2)
  }
}


