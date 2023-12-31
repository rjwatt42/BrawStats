reportExplore<-function(Iv,IV2,DV,effect,design,explore,exploreResult){

  max_cols<-8
  
  vals<-exploreResult$vals
  if (length(vals)>max_cols)  {
    use<-seq(1,length(vals),2)
  } else {
    use<-1:length(vals)
  }
  nc<-length(use)

  extra_y_label<-explore$Explore_show
  if (is.null(IV2)){
    rVals<-exploreResult$rIVs
    pVals<-exploreResult$pIVs
  } else {
    if (explore$Explore_typeShow=="all") {explore$Explore_typeShow<-"direct"}
    if (explore$Explore_whichShow=="All") {explore$Explore_whichShow<-"Main 1"}
    switch (explore$Explore_whichShow,
            "Main 1"={
              rVals<-exploreResult$r1[[explore$Explore_typeShow]]
              pVals<-exploreResult$p1[[explore$Explore_typeShow]]
              extra_y_label<-paste("Main Effect 1:",explore$Explore_typeShow)
            },
            "Main 2"={
              rVals<-exploreResult$r2[[explore$Explore_typeShow]]
              pVals<-exploreResult$p2[[explore$Explore_typeShow]]
              extra_y_label<-paste("Main Effect 2:",explore$Explore_typeShow)
            },
            "Interaction"={
              rVals<-exploreResult$r3[[explore$Explore_typeShow]]
              pVals<-exploreResult$p3[[explore$Explore_typeShow]]
              extra_y_label<-paste("Interaction:",explore$Explore_typeShow)
            }
    )
  }
  
  switch (explore$Explore_show,
          "EffectSize"={
            showVals<-rVals
          },
          "p"={
            showVals<-pVals
          },
          "w"={
            showVals<-rn2w(rVals,exploreResult$nvals)
          },
          "p(sig)"={
            y75<-c()
            y50<-c()
            y25<-c()
            for (i in 1:length(exploreResult$vals)){
              p<-mean(pVals[,i]<alpha,na.rm=TRUE)
              y50[i]<-p
              y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
              y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
            }
          },
          "NHSTErrors"={
            y50<-c()
            y25<-c()
            y75<-c()
            for (i in 1:length(exploreResult$vals)){
              p<-mean(pVals[,i]<alpha,na.rm=TRUE)
              y50[i]<-p
              y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
              y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
            }
            y75<-1-y75
            y50<-1-y50
            y25<-1-y25
          },
          "log(lr)"={
            ns<-exploreResult$nvals
            showVals<-r2llr(rVals,ns)
          },
          "p(str)"={
            y75<-c()
            y50<-c()
            y25<-c()
            ns<-exploreResult$nvals
            showVals<-r2llr(rVals,ns)
            for (i in 1:length(exploreResult$vals)){
              p<-mean(showVals[,i]>alphaLLR,na.rm=TRUE)
              y50[i]<-p
              y75[i]<-p+sqrt(p*(1-p)/length(showVals[,i]))
              y25[i]<-p-sqrt(p*(1-p)/length(showVals[,i]))
            }
          },
  )

  if (is.element(explore$Explore_show,c("EffectSize","p","w","log(lr)"))) {
    y75<-c()
    y50<-c()
    y25<-c()
    for (i in 1:length(exploreResult$vals)) {
      y75[i]<-quantile(showVals[,i],0.75,na.rm=TRUE)
      y50[i]<-quantile(showVals[,i],0.50,na.rm=TRUE)
      y25[i]<-quantile(showVals[,i],0.25,na.rm=TRUE)
    }
  }
  
  outputText<-rep("",nc+1)
  outputText[1]<-"\bExplore:"
  outputText[2]<-explore$Explore_type
  outputText[3]<-paste("nsims=",format(nrow(exploreResult$rIVs)),sep="")
  outputText<-c(outputText,rep("",nc+1))
  
  outputText<-c(outputText,paste("\b", extra_y_label,":  "))
  for (i in 1:nc) {
    outputText<-c(outputText,paste("\b",format(vals[use[i]],digits=report_precision),sep=""))
  }
  outputText<-c(outputText,"lower 25%")
  for (i in 1:nc) {
    outputText<-c(outputText,format(y25[use[i]],digits=report_precision))
  }
  outputText<-c(outputText,"\bmedian")
  for (i in 1:nc) {
    outputText<-c(outputText,format(y50[use[i]],digits=report_precision))
  }
  outputText<-c(outputText,"upper 25%")
  for (i in 1:nc) {
    outputText<-c(outputText,format(y75[use[i]],digits=report_precision))
  }
  
  nc=nc+1
  nr=length(outputText)/nc

  reportPlot(outputText,nc,nr)        

}
