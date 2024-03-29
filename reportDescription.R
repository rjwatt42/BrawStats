
makeFormula<-function(IV,IV2,DV,evidence,result,an_vars){

  assign_string = "<<"  
  when_string = "="
  times_string = "x"
  
  switch (evidence$dataType,
          "Norm"={a<-result$normModel},
          "Raw"={a<-result$rawModel},
          "NormC"={a<-result$normModelC},
          "RawC"={a<-result$rawModelC}
  )

  if (any(class(a)[1]==c("lmerMod","glmerMod")))
  { coeffs<-colMeans(coef(a)$participant)
    use<-!grepl("participant",an_vars)
    an_vars<-an_vars[use]
  } else {
    coeffs<-a$coefficients
  }

  if (1==2){
  if (DV$type=="Interval") {
    dvm<-mean(result$dv,na.rm=TRUE)
    dvs<-sd(result$dv,na.rm=TRUE)
  } else {
    dvm<-0
    dvs<-1
  }
  
  if (IV$type=="Interval") {
    iv1m<-mean(result$iv,na.rm=TRUE)
    iv1s<-sd(result$iv,na.rm=TRUE)
    iv1nc<-1
  } else {
    iv1m<-0
    iv1s<- 1
    iv1nc<-IV$ncats-1
  }
    
    if (!is.null(IV2)){
      if (IV2$type=="Interval") {
        iv2m<-mean(result$iv2,na.rm=TRUE)
        iv2s<-sd(result$iv2,na.rm=TRUE)
        iv2nc<-1
      } else {
        iv2m<-0
        iv2s<- 1
        iv2nc<-IV2$ncats-1
      }
    }
    
    if (is.null(IV2)){
      intercept<-1
      iv1<-intercept+(1:iv1nc)
      c0<-c(
        -coeffs[intercept]-sum(coeffs[iv1])*iv1m/iv1s,
        coeffs[iv1]/iv1s
      )
      c1<- c(dvm,rep(0,length(c0)-1))+c0*dvs
      coeffs<-c1
    } else {
      intercept<-1
      iv1<-intercept+(1:iv1nc)
      iv2<-max(iv1)+(1:iv2nc)
      iv1iv2<-max(iv2)+(1:iv1nc*iv2nc)
      if (length(coeffs)<4) {
        c1<-c(
          -coeffs[intercept]-coeffs[iv1]*iv1m/iv1s-coeffs[iv2]*iv2m/iv2s,
          coeffs[iv1]/iv1s,
          coeffs[iv2]/iv2s
        )
      } else {
        c1<-c(
          coeffs[intercept]-coeffs[iv1]*iv1m/iv1s-coeffs[iv2]*iv2m/iv2s+coeffs[iv1iv2]*iv1m/iv1s*iv2m/iv2s,
          coeffs[iv1]/iv1s-coeffs[iv1iv2]*iv2m/iv1s/iv2s,
          coeffs[iv2]/iv2s-coeffs[iv1iv2]*iv1m/iv1s/iv2s,
          coeffs[iv1iv2]/iv1s/iv2s
        )
      }
      c1<- c(dvm,rep(0,length(c1)-1))+c1*dvs
      coeffs<-c1
    }
  }
  switch (DV$type,
          "Interval"={
            an_model<-paste(DV$name,assign_string,sep="")
          },
          "Ordinal"={
            an_model<-paste(DV$name,assign_string,sep="")
          },
          "Categorical"={
            an_model<-paste("logit(", DV$name, when_string, DV$cases[2], ") ", assign_string, sep="")
          }
  )
  
  if (coeffs[1]>=0){join<-" +"}else{join<-" -"}
  an_model<-paste(an_model, join, format(abs(coeffs[1]),digits=report_precision)   ,sep="")
  
  
  for (i in 2:length(coeffs)){
    if (!is.na(coeffs[i])) {
      if (coeffs[i]>=0){join<-" +"}else{join<-" -"}
      an_model<-paste(an_model, join, format(abs(coeffs[i]),digits=report_precision)   ,times_string,an_vars[i])
    }
  }
  
  an_model
}



reportDescription<-function(IV,IV2,DV,evidence,result){
  
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
  
  if (IV$type=="Categorical" && is.null(IV2)) {
    nc<-max(4,IV$ncats+1)
  } else {
    nc<-4
  }
  
  switch (DV$type,
          "Interval"={
            outputText<-c("\bLinear Model", rep("",nc-1))
          },
          "Ordinal"={
            outputText<-c("\bLinear Model", rep("",nc-1))
          },
          "Categorical"={
            outputText<-c("\bGeneralized Linear Model",rep("",nc-1))
          }
  )
  
  
  a<-result$normModel
  if (any(class(a)[1]==c("lmerMod","glmerMod"))) {
    an_vars<-c("Intercept")
    if (IV$type=="Categorical") {
      for (i in 2:IV$ncats) {
      an_vars<-c(an_vars,paste0("iv1",IV$cases[i]))
      }
    } else {
      an_vars<-c(an_vars,IV$name)
    }
    if (!is.null(IV2$name)) {
      if (IV2$type=="Categorical") {
        for (i in 2:IV2$ncats) {
          an_vars<-c(an_vars,paste0("iv2",IV2$cases[i]))
        }
      } else {
        an_vars<-c(an_vars,IV2$name)
      }
    }
  } else {
    an_vars<-variable.names(a)
    an_vars<-sub("iv1$",IV$name,an_vars)
  }
  
  an_vars<-sub("iv1:",paste(IV$name,":",sep=""),an_vars)
  an_vars<-sub("iv1",paste(IV$name,"|",sep=""),an_vars)
  if (!is.null(IV2)) {
    an_vars<-sub("iv2$",IV2$name,an_vars)
    an_vars<-sub("iv2",paste(IV2$name,"|",sep=""),an_vars)
  } 
  
  an_model<-makeFormula(IV,IV2,DV,evidence,result,an_vars)
  if (nchar(an_model)>80) {
    breaks<-unlist(gregexpr("[+-]",an_model))
    use<-sum(breaks<80)
    an_model1<-substr(an_model,1,breaks[use])
    an_model2<-substr(an_model,breaks[use],nchar(an_model))
    outputText<-c(outputText,"Formula:",paste(an_model1),rep("",nc-2))
    outputText<-c(outputText,"  ",paste("         ",an_model2),rep("",nc-2))
  } else {
    outputText<-c(outputText,"Formula:",paste(an_model),rep("",nc-2))
  }
  outputText<-c(outputText,"R^2",paste(format(result$rFull^2,digits=report_precision),sep=""),rep("",nc-2))
  
  outputText<-c(outputText,rep("",nc))
  outputText<-c(outputText,"\bEffect Size ","\bNormalized",rep("",nc-2))
  
  switch (no_ivs,
          { result$rIVse<-r2se(result$rIV,result$nval)
          outputText<-c(outputText,paste0("\b!j",IV$name,":"),
                                   paste(format(result$rIV,digits=report_precision),
                                                             " +/- ",format(result$rIVse,digits=report_precision),
                                                             sep=""),
                        paste0("CI = (",format(result$rFullCI[1],digits=report_precision),
                               ",",format(result$rFullCI[2],digits=report_precision),
                               ")"),
                        rep("",nc-3)
          )
          },{
            outputText<-c(outputText,"\b!jVariable","\bdirect","\bunique","\btotal",rep("",nc-4))
            outputText<-c(outputText,paste0("!j",IV$name,":"),
                          format(result$r$direct[1],digits=report_precision),format(result$r$unique[1],digits=report_precision),format(result$r$total[1],digits=report_precision),
                          rep("",nc-4))
            outputText<-c(outputText,paste0("!j",IV2$name,":"),
                          format(result$r$direct[2],digits=report_precision),format(result$r$unique[2],digits=report_precision),format(result$r$total[2],digits=report_precision),
                          rep("",nc-4))
            if (evidence$rInteractionOn) {
              outputText<-c(outputText,paste0("!j",IV$name,"*",IV2$name,":"),
                          format(result$r$direct[3],digits=report_precision),format(result$r$unique[3],digits=report_precision),format(result$r$total[3],digits=report_precision),
                          rep("",nc-4))
            }
  
            outputText<-c(outputText,rep("",nc))
            
            an_rt<-format(result$rFull,digits=report_precision) 
            an_rset<-format(result$rFullse,digits=report_precision)
            outputText<-c(outputText,
                          "\b!jFull model:",
                          paste(an_rt,"+/-",an_rset),
                          paste0("CI = (",format(result$rFullCI[1],digits=report_precision),
                                 ",",format(result$rFullCI[2],digits=report_precision),
                                 ")"),
                          rep("",nc-3)
            )
          }
  )
  
  switch (no_ivs,
          {
            if (IV$type=="Categorical" && DV$type=="Interval"){
              outputText<-c(outputText,rep("",nc))
              mn<-c()
              ss<-c()
              cases<-levels(result$iv)
              if (reportGroupMeans) {
                outputText<-c(outputText,paste0("\b",IV$name,"="))
                for (i in 1:IV$ncats){
                  outputText<-c(outputText,paste0("\b",IV$cases[i]))
                }
                outputText<-c(outputText,rep("",nc-(IV$ncats+1)))
                outputText<-c(outputText,"\bMean")
                for (i in 1:IV$ncats){
                  use<-(result$iv==cases[i])
                v<-result$dv[use]
                mn[i]<-mean(v,na.rm=TRUE)
                ss[i]<-sd(v,na.rm=TRUE)
                outputText<-c(outputText,format(mn[i],digits=report_precision))
                }
                outputText<-c(outputText,rep("",nc-(IV$ncats+1)))
                outputText<-c(outputText,"\bSD")
                for (i in 1:IV$ncats){
                  outputText<-c(outputText,format(ss[i],digits=report_precision))
                }
                outputText<-c(outputText,rep("",nc-(IV$ncats+1)))
              }
              if (any(class(result$model)[1]==c("lmerMod","glmerMod"))) {
                fitted<-fitted(result$model)
                residuals<-residuals(result$model)
              } else {
                fitted<-result$model$fitted
                residuals<-result$model$residuals
              }
              rsd<-sd(residuals,na.rm=TRUE)
              outputText<-c(outputText,rep("",nc))
              if (IV$ncats==2){
                outputText<-c(outputText,"Difference(means):",format(diff(mn),digits=report_precision),"sd(residuals):",format(rsd,digits=report_precision),
                              rep("",nc-4))
              } else {
                outputText<-c(outputText,"sd(means):",format(sd(fitted),digits=report_precision),"sd(residuals):",format(rsd,digits=report_precision),
                              rep("",nc-4))
              }
            }
            if (IV$type=="Categorical" && DV$type=="Categorical"){
              outputText<-c(outputText,rep("",nc))
              obs<-matrix(nrow=IV$ncats,ncol=DV$ncats)
              for (i in 1:IV$ncats){
                for (j in 1:DV$ncats){
                  use<-(result$iv==IV$cases[i] & result$dv==DV$cases[j])
                  obs[i,j]<-sum(use)
                }
              }
              expect<-rowSums(obs)%*%t(colSums(obs))
              outputText<-c(outputText,"deviance",format(sum(abs(obs-expect))/sum(obs),digits=report_precision),rep("",nc-2))
            }
            
          })
  
  
  nr=length(outputText)/nc
  list(outputText=outputText,nc=nc,nr=nr)
  

}
