################################################################        
# update basic functions
# set prediction, design, evidence variables from UI
#
updateHypothesis<-function() {
  hypothesis<-makeHypothesis(
    IV=updateIV(),IV2=updateIV2(),DV<-updateDV(),effect<-updateEffect()
  )
  if (variablesHeld=="Data") {
    if (hypothesis$IV$deploy=="Within" && !grepl(paste0(",",hypothesis$DV$name,","),hypothesis$IV$targetDeploys)) {
      hmm(paste0("Warning: ", hypothesis$IV$name," requires matched DV (",substr(hypothesis$IV$targetDeploys,2,nchar(hypothesis$IV$targetDeploys)-1),")"))
    }
    if (hypothesis$DV$deploy=="Within") {
      hmm(paste0("Warning: ", hypothesis$DV$name," is a within-participants IV and cannot be used as a DV"))
    }
  }
  return(hypothesis)
}

######################################################  
## update variables functions

updateIV<-function(){
  if (debug) debugPrint("     updateIV")
  
  IV<-makeVariable(name=input$IVname,type=input$IVtype,
                   mu=input$IVmu,sd=input$IVsd,skew=input$IVskew,kurtosis=input$IVkurt, 
                   nlevs=input$IVnlevs,iqr=input$IViqr,
                   ncats=input$IVncats,cases=input$IVcases,proportions=input$IVprop)
 
  if (IV$type!="Interval" && input$shortHand) {
    hmm("Please switch to longhand calculations: IV not Interval")
  }
  if (any(c(IV$skew,IV$kurtosis-3)!=0) && input$shortHand) {
    hmm("Please switch to longhand calculations: IV skew/kurtosis")
  }
  
  if (debug) debugPrint("     updateIV - exit")
  return(IV)
}

updateIV2<-function(){
  if (debug) debugPrint("     updateIV2")
  
  if (input$IV2choice=="none" || input$IV2name==""){
    if (debug) debugPrint("     updateIV2 - exit unused")
    return(NULL)
  } 
  
  IV2<-makeVariable(name=input$IV2name,type=input$IV2type,
                   mu=input$IV2mu,sd=input$IV2sd,skew=input$IV2skew,kurtosis=input$IV2kurt, 
                   nlevs=input$IV2nlevs,iqr=input$IV2iqr,
                   ncats=input$IV2ncats,cases=input$IV2cases,proportions=input$IV2prop)

  if (debug) debugPrint("     updateIV2 - exit")
  return(IV2)
}

updateDV<-function(){
  if (debug) debugPrint("     updateDV")
  
  DV<-makeVariable(name=input$DVname,type=input$DVtype,
                   mu=input$DVmu,sd=input$DVsd,skew=input$DVskew,kurtosis=input$DVkurt, 
                   nlevs=input$DVnlevs,iqr=input$DViqr,
                   ncats=input$DVncats,cases=input$DVcases,proportions=input$DVprop)

  if (DV$type=="Ordinal" && input$IV2choice!="none") {
    if (warn3Ord==FALSE) {
      hmm("Ordinal DV with more than 1 IV. It will be treated as Interval.")
      warn3Ord<<-TRUE
    }
  }
  if (DV$type!="Interval" && input$shortHand) {
    hmm("Please switch to longhand calculations: DV not Interval")
  }
  if (any(c(DV$skew,DV$kurtosis-3)!=0) && input$shortHand) {
    hmm("Please switch to longhand calculations: DV skew/kurtosis")
  }
  
  if (debug) debugPrint("     updateDV - exit")
  return(DV)
}

# UI changes    
observeEvent(c(input$rIV,input$rIV2,input$rIVIV2,input$rIVIV2DV,
               input$sN,input$sMethod,input$sIV1Use,input$sIV2Use),{
                 if (debug) debugPrint("     effectChanged")
                 
                 # remove out of date sample and other 
                 validSample<<-FALSE
                 validExpected<<-FALSE
                 validExplore<<-FALSE
                 
                 # expectedResult<-c()
                 exploreResultHold<-list(Hypothesis=c(),Design=c())
                 possiblePResultHold<-c()
                 possibleSResultHold<-c()
                 
                 updateCheckboxInput(session,"ExploreAppendH",value=FALSE)
                 updateCheckboxInput(session,"ExploreAppendD",value=FALSE)
                 updateCheckboxInput(session,"ExploreAppendM",value=FALSE)
                 
                 if (debug) debugPrint("     effectChanged - exit")
               },priority=100)

observeEvent(c(input$IVchoice),
             {
               IV<-getVariable(input$IVchoice)
               updateTextInput(session,"IVname",value=IV$name)
               updateSelectInput(session,"IVtype",selected=IV$type)
               updateNumericInput(session,"IVmu",value=IV$mu)    
               updateNumericInput(session,"IVsd",value=IV$sd)    
               updateNumericInput(session,"IVskew",value=IV$skew)    
               updateNumericInput(session,"IVkurt",value=IV$kurtosis)    
               updateNumericInput(session,"IVnlevs",value=IV$nlevs)    
               updateNumericInput(session,"IVmedian",value=IV$median)    
               updateNumericInput(session,"IViqr",value=IV$iqr)    
               updateSelectInput(session,"IVordSource",selected=IV$ordSource)
               updateNumericInput(session,"IVncats",value=IV$ncats)
               updateTextInput(session,"IVcases",value=IV$cases)
               updateTextInput(session,"IVprop",value=paste(IV$proportions,collapse=","))
               updateSelectInput(session,"IVcatSource",selected=IV$catSource)
               
               switch (IV$type,
                       "Interval"={
                         shinyjs::disable(id= "sIV1Use")
                       },
                       "Ordinal"={
                         shinyjs::disable(id= "sIV1Use")
                       },
                       "Categorical"={
                         shinyjs::enable(id= "sIV1Use")
                       }
               )
               validSample<<-FALSE
               validExpected<<-FALSE
               validExplore<<-FALSE
             })

observeEvent(c(input$IV2choice),
             {
               if (input$IV2choice=="none") {
                 shinyjs::hide(id= "inspectIV2")
                 shinyjs::hide(id= "editIV2")
                 shinyjs::hide(id= "editIV2T")
               } else {
                 
                 IV2<-getVariable(input$IV2choice)
                 updateNumericInput(session,"IV2mu",value=IV2$mu)    
                 updateNumericInput(session,"IV2sd",value=IV2$sd)    
                 updateNumericInput(session,"IV2skew",value=IV2$skew)    
                 updateNumericInput(session,"IV2kurt",value=IV2$kurtosis)    
                 updateNumericInput(session,"IV2nlevs",value=IV2$nlevs)    
                 updateNumericInput(session,"IV2median",value=IV2$median)    
                 updateNumericInput(session,"IV2iqr",value=IV2$iqr)    
                 updateSelectInput(session,"IV2ordSource",selected=IV2$ordSource)
                 updateNumericInput(session,"IV2ncats",value=IV2$ncats)
                 updateTextInput(session,"IV2cases",value=IV2$cases)
                 updateTextInput(session,"IV2prop",value=paste(IV2$proportions,collapse=","))
                 updateSelectInput(session,"IV2catSource",selected=IV2$catSource)
                 updateSelectInput(session,"IV2type",selected=IV2$type)
                 updateTextInput(session,"IV2name",value=IV2$name)
                 
                 shinyjs::show(id= "inspectIV2")
                 shinyjs::show(id= "editIV2")
                 shinyjs::show(id= "editIV2T")
                 switch (braw.def$IV$type,
                         "Interval"={
                           shinyjs::disable(id= "sIV2Use")
                         },
                         "Ordinal"={
                           shinyjs::disable(id= "sIV2Use")
                         },
                         "Categorical"={
                           shinyjs::enable(id= "sIV2Use")
                         }
                 )
               }
               
               validSample<<-FALSE
               validExpected<<-FALSE
               validExplore<<-FALSE
             },priority = 100)

observeEvent(c(input$DVchoice),
             {
               DV<-getVariable(input$DVchoice)
               updateTextInput(session,"DVname",value=DV$name)
               updateSelectInput(session,"DVtype",selected=DV$type)
               updateNumericInput(session,"DVmu",value=DV$mu)    
               updateNumericInput(session,"DVsd",value=DV$sd)    
               updateNumericInput(session,"DVskew",value=DV$skew)    
               updateNumericInput(session,"DVkurt",value=DV$kurtosis)    
               updateNumericInput(session,"DVnlevs",value=DV$nlevs)    
               updateNumericInput(session,"DVmedian",value=DV$median)    
               updateNumericInput(session,"DViqr",value=DV$iqr)    
               updateSelectInput(session,"DVordSource",selected=DV$ordSource)
               updateNumericInput(session,"DVncats",value=DV$ncats)
               updateTextInput(session,"DVcases",value=DV$cases)
               updateTextInput(session,"DVprop",value=paste(DV$proportions,collapse=","))
               updateSelectInput(session,"DVcatSource",selected=DV$catSource)
               
               validSample<<-FALSE
               validExpected<<-FALSE
               validExplore<<-FALSE
             })



# PREDICTION & DESIGN & EVIDENCE
updateEffect<-function(type=0){
  if (debug) debugPrint("     updateEffect")
  
  if (switches$doWorlds) {
    world<-list(worldOn=input$world_on,populationPDF=input$world_distr,
                populationPDFk=input$world_distr_k,populationRZ=input$world_distr_rz,
                populationNullp=input$world_distr_Nullp,
                worldAbs=input$world_abs)
  } else {
    world<-list(worldOn=FALSE,populationPDF="Single",populationPDFk=NA,populationRZ=NA,populationNullp=NA,worldAbs=FALSE)
  }

  if (is.null(world$worldOn)) {world$worldOn<-FALSE}
  
  if (is.null(type)) {
    effect<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,
                 Heteroscedasticity=input$Heteroscedasticity,ResidDistr=input$ResidDistr,
                 world=world
    )
  } else {
    effect<-list(rIV=input$rIV,rIV2=input$rIV2,rIVIV2=input$rIVIV2,rIVIV2DV=input$rIVIV2DV,
                 Heteroscedasticity=input$Heteroscedasticity,ResidDistr=input$ResidDistr,
                 world=world
    )
  }
  if (effect$Heteroscedasticity!=0 && input$shortHand) {
    hmm("Please switch to longhand calculations: heteroscedasticity")
  }
  if (effect$ResidDistr!="normal" && input$shortHand) {
    hmm("Please switch to longhand calculations: heteroscedasticity")
  }
  
  if (effect$world$worldOn==FALSE) {
    effect$world$populationPDF<-"Single"
    effect$world$populationRZ<-"r"
    effect$world$populationPDFk<-effect$rIV
    effect$world$populationNullp<-0
    effect$world$worldAbs<-FALSE
  }
  effect$Heteroscedasticity<-checkNumber(effect$Heteroscedasticity)
  effect$world$populationPDFk<-checkNumber(effect$world$populationPDFk)
  effect$world$populationNullp<-checkNumber(effect$world$populationNullp)
  
  if (debug) debugPrint("     updateEffect - exit")
  effect
}

updateDesign<-function(){
  if (debug) debugPrint("     updateDesign")

  design<-makeDesign(sN=input$sN, sNRand=input$sNRand,sNRandK=input$sNRandK,
               sBudgetOn=input$sBudgetOn,sNBudget=input$sNBudget,
               sMethod=makeSampling(input$sMethod),
               sIV1Use=input$sIV1Use,sIV2Use=input$sIV2Use, sWithinCor=0.5,
               sRangeOn=input$sRangeOn, sIVRange=input$sIVRange, sDVRange=input$sDVRange, 
               sDependence=input$sDependence, sOutliers=input$sOutliers,
               sCheating=input$sCheating,sCheatingLimit=input$sCheatingLimit,sCheatingAttempts=input$sCheatingAttempts,sCheatingBudget=input$sCheatingBudget,
               Replication=makeReplication(
                 On=input$sReplicationOn,Repeats=input$sReplRepeats,Keep=input$sReplKeep,
                 PowerOn=input$sReplPowerOn,Power=input$sReplPower,Tails=as.numeric(input$sReplTails),PowerPrior="None",
                 forceSigOriginal=(input$sReplSigOnly=="Yes"),forceSign=TRUE,
                 BudgetType=input$sReplType,Budget=1000,
                 RepAlpha=input$sReplAlpha
                 )
  )
  if (is.element(design$sCheating,c("Grow","Replace")) && input$shortHand) {
    hmm("Please switch to longhand calculations: cheating")
  }
  
  if (design$sMethod!="Random" && shortHand) {
    hmm("Please switch to longhand calculations: sampling")
  }
  
  if (any(c(design$sDependence,design$sOutliers)!=0) && input$shortHand) {
    hmm("Please switch to longhand calculations: anomalies")
  }
  
  design$sN<-checkNumber(design$sN,c=10)
    design$sNRandK<-checkNumber(design$sNRandK)
    design$Replication$Power<-checkNumber(design$Replication$Power)
  if (variablesHeld=="Data" && doResample && switches$doBootstrap) {design$sMethod<-"Resample"}
    
  if (debug) debugPrint("     updateDesign - exit")
    
  return(design)
}

updateEvidence<-function(){
  if (debug) debugPrint("     updateEvidence")
  evidence<-makeEvidence(
    rInteractionOn=input$rInteractionOn,
    ssqType=input$ssqType,
    caseOrder=input$evidenceCaseOrder,
    shortHand=input$shortHand,
    llr=list(e1=input$llr1,e2=input$llr2),
    Welch=!input$Welch,Transform=input$Transform,
    prior=makeWorld()
  )
  
  if (input$Evidence=="Single") {
    evidence$showType<-input$EvidenceEffect_type1
  }
    
  switch(input$likelihoodUsePrior,
         "none"={
           evidence$prior=makeWorld(worldOn=TRUE,populationPDF="Uniform",populationRZ="r")
         },
         "world"={
           evidence$prior=makeWorld(worldOn=input$world_on,populationPDF=input$world_distr,
                               populationPDFk=input$world_distr_k,populationRZ=input$world_distr_rz,
                               populationNullp=input$world_distr_Nullp)
         },
         "prior"={
           evidence$prior=makeWorld(worldOn=TRUE,populationPDF=input$Prior_distr,
                               populationPDFk=input$Prior_distr_k,populationRZ=input$Prior_distr_rz,
                               populationNullp=input$Prior_Nullp)
         }
  )

  if (debug) debugPrint("     updateEvidence - exit")
  return(evidence)
}

##################################################################################  
