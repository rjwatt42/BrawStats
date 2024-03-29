debugHere<-FALSE


wsd<-function(x,w=1,na.rm=TRUE) {
  if (length(w)==1) {
    sqrt(mean((x-mean(x,na.rm=na.rm))^2))
  } else {
    sqrt(sum((x-mean(x,na.rm=na.rm))^2 *w)/sum(w))
  }
}


p2r<-function(p,n,df1=1) {
  if (any(abs(n)<3)) {
    print("p2r n-exception")
    n[n<3]<-4
  }
  df2<-n-(df1+1)
  
  Fvals <- qf(1-p,df1,df2)
  r <- sqrt(Fvals*df1)/sqrt(Fvals*df1+df2)
  return(r)
  
  t_vals <- qt(p/2,n-2)
  r_vals <- t_vals/sqrt(t_vals^2+(n-2))
  r_vals
}


r2p<-function(r,n,df1=1){
  if (!is.numeric(r) || !is.numeric(n)) {return(1)}
  if (any(abs(r)>1)) {
    print(paste("r2p r-exception",format(max(abs(r)),digits=3)))
    r[r>1]<-1
    r[r < -1]<- -1
  }
  if (any(abs(n)<3)) {
    print("r2p n-exception")
    n[n<3]<-4
  }
  df2<-n-(df1+1)
  
  Fvals<-r^2/(1-r^2)*df2/df1
  p<-(1-pf(Fvals,df1,df2))
  return(p)
  
  if (any(df1>1)) {
    Fvals<-r^2/(1-r^2)*df2/df1
    (1-pf(Fvals,df1,df2))
  } else {
    t_vals<-r/r2se(r,n)
    (1-pt(abs(t_vals),df2))*2
  }
  
}

r2se<-function(r,n){
  if (any(abs(r)>1)) {
    print(paste("r2se r-exception",format(max(abs(r)),digits=3)))
    r[r>1]<-1
    r[r < -1]<- -1
  }
  if (any(abs(n)<3)) {
    print("r2se n-exception")
    n[n<3]<-4
  }
  sqrt((1-r^2)/(as.vector(n)-2))
}

r2ci<-function(r,n,s=0){
  if (any(abs(r)>1)) {
    print(paste("r2ci r-exception",format(max(abs(r)),digits=3)))
    r[r>1]<-1
    r[r < -1]<- -1
  }
  if (any(abs(n)<3)) {
    print("r2ci n-exception")
    n[n<3]<-4
  }
  z<-atanh(r)
  zci<-qnorm(1-0.05/2)*sqrt(1/(n-3))
  if (s==0){
    tanh(z+c(-1,1)*zci)
  } else {
    tanh(z+s*zci)
  }
}

res2llr<-function(result,method=STMethod) {
  r2llr(result$rIV,result$nval,result$df1,method,result$evidence$llr,result$evidence$prior)
}

r2llr<-function(r,n,df1,method=STMethod,llr=list(e1=c(),e2=0),world=NULL) {
  if (any(abs(r)>1)) {
    print(paste("r2llr r-exception",format(max(abs(r)),digits=3)))
    r[r>1]<-1
    r[r < -1]<- -1
  }
  if (any(abs(n)<3)) {
    print("r2llr n-exception")
    n[n<3]<-4
  }
  z<-atanh(r)
  if (method=="dLLR") {
    z<-abs(z)
    if (!is.matrix(z)) {
      z<-matrix(z,ncol=1)
    }
    if (!is.matrix(n)) {
      n<-matrix(n,ncol=1)
    }
    if (is.null(world) || is.null(world$worldOn) || !world$worldOn) {
      world$populationPDF<-"Single"
      world$populationNullp<-0.5
    }
    lk<-getLogLikelihood(z,n,df1,world$populationPDF,world$populationPDFk,worldDistNullP=c(0,1),remove_nonsig=FALSE,doSeparate=TRUE)
    lk1<-lk[,,1]+log(1-world$populationNullp)
    lk2<-lk[,,2]+log(world$populationNullp)
    llk<-lk1-lk2
  } else {
    if (isempty(llr$e1) || is.na(llr$e1)) { llr1=z }
    else {llr1=llr$e1}
    lk1<-dnorm(llr1,mean=z,sd=1/sqrt(n-3))
    lk2<-dnorm(llr$e2,mean=z,sd=1/sqrt(n-3))
    llk<-log(lk1/lk2)
  }
  llk
  # for llr1=0 this can be simplified to just this: z^2*(n-3)/2
}



model2directeffect<-function(mF){
  if (any(class(mF)[1]==c("lmerMod","glmerMod")))
  {
    mTerms<-attr(terms(mF),"term.labels")
    data<-model.frame(mF)
    expected<-fitted(mF)
    residuals<-residuals(mF)
  } else {
    mTerms<-attr(mF$terms,"term.labels")
    data<-mF$model
    expected<-mF$fitted.values
    residuals<-mF$residuals
  }
  dv<-data[,1]
  n<-length(dv)
  
  if (is.numeric(dv)) {
    DVgain<-wsd(dv,na.rm=TRUE)
  } else {
    DVgain<-wsd(expected+residuals)
  }
  
  # we find effect-size by 
  # looking at the change in prediction around the centre of an interval predictor
  # looking at the sd of predictions for all possible cases of categorical predictors

  if (any(class(mF)[1]==c("lmerMod","glmerMod"))) {
    p1<-data$participant
    data$participant<-p1[1]
  }

  directEffects<-c()
  for (iTerm in 1:length(mTerms)) {
    if (!grepl(":",mTerms[iTerm])) {
        v1<-data[[mTerms[iTerm]]]
        if (is.numeric(v1)){
          m1<-mean(v1,na.rm=TRUE)+c(-1,1)*wsd(v1,na.rm=TRUE)
        } else {
          m1<-levels(v1)
        }
        rawData<-data
        v<-c()
        for (i in 1:length(m1)) {
          rawData[[mTerms[iTerm]]]<-m1[i]
          if (any(class(mF)[1]==c("lmerMod","glmerMod"))) {
            v<-c(v,mean(predict(mF,rawData)))
          } else {
            v<-c(v,mean(predict.lm(mF,rawData)))
          }
        }
        ncoeff1<-wsd(v) * sign(sum(diff(v)))
    } else {
      terms<-strsplit(mTerms[iTerm],":")
      terms<-terms[[1]]
      v1<-data[[terms[1]]]
      if (is.numeric(v1)){
        h1<-c(1,1)
        m1<-mean(v1,na.rm=TRUE)+c(-1,1)*wsd(v1,na.rm=TRUE)
      } else {
        m1<-levels(v1)
      }
      v2<-data[[terms[2]]]
      if (is.numeric(v2)){
        m2<-mean(v2,na.rm=TRUE)+c(-1,1)*wsd(v2,na.rm=TRUE)
      } else {
        m2<-levels(v2)
      }
      rawData<-data
      v<-c()
      for (i1 in 1:length(m1)) {
        rawData[[terms[1]]]<-m1[i1]
        for (i2 in 1:length(m2)) {
          rawData[[terms[2]]]<-m2[i2]
          if (any(class(mF)[1]==c("lmerMod","glmerMod"))) {
            v<-c(v,mean(predict(mF,rawData)))
          } else {
            v<-c(v,mean(predict.lm(mF,rawData)))
          }
        }
      }
      dim(v)<-c(length(m2),length(m1))
      ie<-t(t(v)-colMeans(v))-rowMeans(v)
      ncoeff1<-wsd(ie,1)*sign(sum(diff(-ie[1,])))
    }
    directEffects<-c(directEffects,ncoeff1)
  }
  directEffects[is.na(directEffects)]=0
  return(directEffects/DVgain)
}

model2uniqueeffect<-function(anU){
  # get the unique effects
  if (grepl("Intercept",rownames(anU)[[1]])) {n1<-2} else {n1<-1}
  n2<-nrow(anU)
  uniqueEffects<-sqrt(anU$`Sum Sq`[n1:(n2-1)]/sum(anU$`Sum Sq`[n1:n2]))
  uniqueEffects<-uniqueEffects*sign(uniqueEffects)
  uniqueEffects[is.na(uniqueEffects)]=0

  return(uniqueEffects)
}

model2totaleffect<-function(mF){
  if (any(class(mF)[1]==c("lmerMod","glmerMod")))
  {
    mTerms<-attr(terms(mF),"term.labels")
    data<-model.frame(mF)
  } else {
    mTerms<-attr(mF$terms,"term.labels")
    data<-mF$model
  }
  dv<-data[,1]

  totalEffects<-c()
  for (iTerm in 1:length(mTerms)) {
    formula<-paste0("dv~",mTerms[iTerm])
    if (is.numeric(dv)) {
      # total effect sizes
      lm1total<-lm(formula=as.formula(formula),data=data)
    } else {
      lm1total<-glm(formula=as.formula(formula),data=data,family="binomial")
    }
    totalEffects<-c(totalEffects,model2directeffect(lm1total))
  }
  return(totalEffects)
}

model2fulleffect<-function(mF,anU) {
  if (any(class(mF)[1]==c("lmerMod","glmerMod"))) {
    # overall model effect-size
    if (class(mF)=="glmerMod") {
      r.full<-sqrt((sum(anU$Chisq)-anU$Chisq[1])/nlevels(mF$model$participant))
    } else {
      r.full<-r.squaredGLMM(mF)[[1]]
    }
  } else {
    if (grepl("Intercept",rownames(anU)[[1]])) {n1<-2} else {n1<-1}
    n2<-nrow(anU)-1
    r.full<-sqrt(sum(anU$`Sum Sq`[n1:n2])/sum(anU$`Sum Sq`))
  }
  return(r.full)
}


appendList <- function (x1, x2) 
{
  xnames <- names(x1)
  for (v in names(x2)) {
    x2[[v]]<-if (is.null(x2[[v]])) NA else x2[[v]]
    x1[[v]] <- if (v %in% xnames && is.list(x1[[v]]) && is.list(x2[[v]])) 
      appendList(x1[[v]], x2[[v]])
    else c(x1[[v]], x2[[v]])
  }
  x1
}


multipleAnalysis<-function(IV,IV2,DV,effect,design,evidence,n_sims=1,appendData=FALSE, earlierResult=c(),sigOnly=FALSE, showProgress=TRUE,progressPrefix=""){
  if (n_sims==1) {showProgress<-FALSE}
  
  rho<-effect$rIV
  rho2<-effect$rIV2
  
  pvals=c()
  rvals=c()
  nvals=c()
  df1vals<-c()
  if (length(rho)<n_sims) {rho<-rep(rho,n_sims)}
  if (!is.null(IV2)) {
    if (length(rho2)<n_sims) {rho2<-rep(rho2,n_sims)}
  }

  nterms<-0
  if (IV$type=="Categorical") {nterms<-nterms+IV$ncats-1} else {nterms<-nterms+1}
  if (!is.null(IV2)) {
    if (IV2$type=="Categorical") {nterms<-nterms+IV2$ncats-1} else {nterms<-nterms+1}
  }

    if (appendData) {
    mainResult<-earlierResult
  } else {
    mainResult<-list(rpIV=c(),roIV=c(),rIV=c(),pIV=c(),
                   rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),
                   nval=c(),df1=c(),
                   r=list(direct=c(),unique=c(),total=c(),coefficients=c()),
                   p=list(direct=c(),unique=c(),total=c()),
                   showType=design$showType)
  }
  
  for (i in 1:n_sims){
    if (showProgress && (n_sims<=50 || (n_sims>50 && i==round(i/25)*25))) {
      off<-length(mainResult$rIV)+1
      off<-0
      showNotification(paste(progressPrefix,format(i+off),"/",format(n_sims+off)),id="counting",duration=Inf,closeButton=FALSE,type="message")
    } 
    effect$rIV<-rho[i]
    if (!is.null(IV2)) {effect$rIV2<-rho2[i]}
    
    res<-runSimulation(IV,IV2,DV,effect,design,evidence,sigOnly)
      
    if (is.na(res$rIV)) {
      res$rIV<-0
      res$pIV<-1
      res$nval<-0
    }
    newResult<-list(rpIV=res$rpIV,roIV=res$roIV,rIV=res$rIV,pIV=res$pIV,poIV=res$poIV,
                    rIV2=res$rIV2,pIV2=res$pIV2,rIVIV2DV=res$rIVIV2DV,pIVIV2DV=res$pIVIV2DV,
                    nval=res$nval,df1=res$df1,
                    r=list(direct=res$r$direct,unique=res$r$unique,total=res$r$total,coefficients=res$r$coefficients),
                    p=list(direct=res$p$direct,unique=res$p$unique,total=res$p$total),
                    ResultHistory=res$ResultHistory,
                    showType=design$showType
                    )
    

   if (any(class(res$rawModel)[1]==c("lmerMod","glmerMod"))) {
      coeffs<-colMeans(coef(res$rawModel)$participant)
    } else {
      coeffs<-res$rawModel$coefficients
    }
    newResult$r$coefficients<-as.double(coeffs[2:length(coeffs)])
    # mainResult<-appendList(mainResult,newResult)
    
    mainResult$rpIV<-rbind(mainResult$rpIV,newResult$rpIV)
    mainResult$rIV<-rbind(mainResult$rIV,newResult$rIV)
    mainResult$roIV<-rbind(mainResult$roIV,newResult$roIV)
    mainResult$pIV<-rbind(mainResult$pIV,newResult$pIV)
    mainResult$poIV<-rbind(mainResult$poIV,newResult$poIV)
    mainResult$nval<-rbind(mainResult$nval,newResult$nval)
    mainResult$df1<-rbind(mainResult$df1,newResult$df1)
    if (!is.null(IV2)){
      mainResult$rIV2<-rbind(mainResult$rIV2,newResult$rIV2)
      mainResult$pIV2<-rbind(mainResult$pIV2,newResult$pIV2)
      mainResult$rIVIV2DV<-rbind(mainResult$rIVIV2DV,newResult$rIVIV2DV)
      mainResult$pIVIV2DV<-rbind(mainResult$pIVIV2DV,newResult$pIVIV2DV)

      mainResult$r$direct<-rbind(mainResult$r$direct,newResult$r$direct)
      mainResult$r$unique<-rbind(mainResult$r$unique,newResult$r$unique)
      mainResult$r$total<-rbind(mainResult$r$total,newResult$r$total)
      mainResult$r$coefficients<-rbind(mainResult$r$coefficients,newResult$r$coefficients)

      mainResult$p$direct<-rbind(mainResult$p$direct,newResult$p$direct)
      mainResult$p$unique<-rbind(mainResult$p$unique,newResult$p$unique)
      mainResult$p$total<-rbind(mainResult$p$total,newResult$p$total)

    } else {
      mainResult$rIV2<-rbind(mainResult$rIV2,NA)
      mainResult$pIV2<-rbind(mainResult$pIV2,NA)
      mainResult$rIVIV2DV<-rbind(mainResult$rIVIV2DV,NA)
      mainResult$pIVIV2DV<-rbind(mainResult$pIVIV2DV,NA)

      mainResult$r$direct<-rbind(mainResult$r$direct,newResult$rIV)
      mainResult$r$unique<-rbind(mainResult$r$unique,newResult$rIV)
      mainResult$r$total<-rbind(mainResult$r$total,newResult$rIV)
      mainResult$r$coefficients<-rbind(mainResult$r$coefficients,newResult$r$coefficients)

      mainResult$p$direct<-rbind(mainResult$p$direct,newResult$pIV)
      mainResult$p$unique<-rbind(mainResult$p$unique,newResult$pIV)
      mainResult$p$total<-rbind(mainResult$p$total,newResult$pIV)

    }
  }
  # if (showProgress) removeNotification(id="counting")
  mainResult$showType<-evidence$showType
  mainResult$effect<-effect
  mainResult$design<-design
  mainResult$evidence<-evidence
  
  mainResult
}

convert2Interval<-function(var) {
  var$type<-"Interval"
  var$mu<-var$median
  var$sd<-var$iqr*qnorm(0.75)
}

generalAnalysis<-function(allData,InteractionOn,withins,ssqType="Type3",caseOrder="Alphabetic") {
  
  if (ncol(allData)<3) {
    result$rIV<-NA
    result$pIV<-NA
    result$rpIV<-NA
    return(result)
  }
  
  no_ivs<-ncol(allData)-2
  n<-nrow(allData)
  
  catVars<-c()
  for (i in 2:ncol(allData)) {
    # get Categorical cases sorted
    if (is.factor(allData[,i])) {
      switch (caseOrder,
              "Alphabetic"={ref=sort(levels(allData[,i]))[1]},
              "AsFound"={ref=as.numeric(allData[1,i])},
              "Frequency"={ref=which.max(tabulate(match(allData[,i], levels(allData[,i]))))}
      )
      allData[,i]<-relevel(allData[,i],ref=ref)
      catVars<-c(catVars,TRUE)
    } else {
      catVars<-c(catVars,FALSE)
    }
  }
  
  # MAKE MAIN DATA STORAGE
  resultRawData<-data.frame(allData)
  names(resultRawData)<-c("participant","dv",paste0("iv",1:no_ivs))
  
  #MAKE NORM DATA STORAGE
  # centre variables on zero
  # this helps with the interaction term
  resultNormData<-resultRawData
  for (i in 0:no_ivs) {
    if (!is.factor(resultNormData[[i+2]]))  
      resultNormData[[i+2]]=(resultNormData[[i+2]]-mean(resultNormData[[i+2]],na.rm=TRUE))
  }
  
  # CREATE FORMULA
  formula<-"dv~iv1"
  if (no_ivs>1)
    for (i in 2:no_ivs) {
      formula<-paste(formula,"+iv",i,sep="")
      if (InteractionOn==1) formula<-paste(formula,"+iv1:iv",i,sep="")
    }
  if (any(withins)){
    doingWithin<-TRUE
    formula<-paste(formula,"+(1|participant)",sep="")
    if (all(withins)){
      formula<-paste(formula,"+(1|iv1:participant)+(1|iv2:participant)",sep="")
    }
  } else {
    doingWithin<-FALSE
  }
  
  # SET UP CONTRASTS
  # these are needed to make the anova type 3 work properly
  contrasts<-c()
  for (i in 1:no_ivs) {
    if (catVars[i+1])  {
      nm<-names(contrasts)
      contrasts<-c(contrasts,list(iv=contr.sum))
      names(contrasts)<-c(nm,paste0("iv",i))
    } 
  }
  
  # LINEAR MODELS  
  # get linear model and anova
  if (catVars[1]) {
    if (doingWithin) {
      lmRaw<-glmer(formula=as.formula(formula),data=resultRawData,family="binomial")
      lmRawC<-glmer(formula=as.formula(formula),data=resultRawData,family="binomial",contrasts=contrasts)
      # lmNorm to calculate effect sizes
      lmNorm<-glmer(formula=as.formula(formula),data=resultNormData,family="binomial")
      lmNormC<-glmer(formula=as.formula(formula),data=resultNormData,family="binomial",contrasts=contrasts)
      testMethod<-"Chisq"
    } else {
      lmRaw<-glm(formula=as.formula(formula),data=resultRawData,family="binomial")
      lmRawC<-glm(formula=as.formula(formula),data=resultRawData,family="binomial",contrasts=contrasts)
      lmNorm<-glm(formula=as.formula(formula),data=resultNormData,family="binomial")
      lmNormC<-glm(formula=as.formula(formula),data=resultNormData,family="binomial",contrasts=contrasts)
      testMethod<-"F"
    }
    pcol=3;prow=2
    
  } else { # Interval DV
    # lmRaw to report model
    if (doingWithin) {
      # print(cor(resultRawData[1:42,4],resultRawData[43:84,4]))
      lmRaw<-lmer(formula=as.formula(formula),data=resultRawData)
      lmRawC<-lmer(formula=as.formula(formula),data=resultRawData,contrasts=contrasts)
      # lmNorm to calculate effect sizes
      lmNorm<-lmer(formula=as.formula(formula),data=resultNormData)
      lmNormC<-lmer(formula=as.formula(formula),data=resultNormData,contrasts=contrasts)
    } else {
      lmRaw<-lm(formula=as.formula(formula),data=resultRawData)
      lmRawC<-lm(formula=as.formula(formula),data=resultRawData,contrasts=contrasts)
      # lmNorm to calculate effect sizes
      lmNorm<-lm(formula=as.formula(formula),data=resultNormData)
      lmNormC<-lm(formula=as.formula(formula),data=resultNormData,contrasts=contrasts)
    }
    testMethod<-"F"
    pcol=4;prow=2;
  }
  
  #ANOVAS
  switch (ssqType,
          "Type1"={
            anRaw<-Anova(lmRaw,test=testMethod)
            anRawC<-Anova(lmRawC,test=testMethod)
            anNorm<-Anova(lmNorm,test=testMethod)
            anNormC<-Anova(lmNormC,test=testMethod)
          },
          "Type2"={
            anRaw<-Anova(lmRaw,test=testMethod,type=2)
            anRawC<-Anova(lmRawC,test=testMethod,type=2)
            anNorm<-Anova(lmNorm,test=testMethod,type=2)
            anNormC<-Anova(lmNormC,test=testMethod,type=2)
          },
          "Type3"={
            anRaw<-Anova(lmRaw,test=testMethod,type=3,singular.ok=TRUE)
            anRawC<-Anova(lmRawC,test=testMethod,type=3,singular.ok=TRUE)
            anNorm<-Anova(lmNorm,test=testMethod,type=3,singular.ok=TRUE)
            anNormC<-Anova(lmNormC,test=testMethod,type=3,singular.ok=TRUE)
          }
  )
  
  if (grepl("Intercept",rownames(anRaw)[[1]])) {n1<-2} else {n1<-1}
  n2<-nrow(anRaw)
  if (grepl("Residuals",rownames(anRaw)[[n2]])) {n2<-n2-1}
  df<-anRaw$Df[n1:n2]

  # EFFECT SIZES  
  r.direct<-model2directeffect(lmNormC)
  if (doingWithin) {
    r.unique<-r.direct
    r.total<-r.direct
  } else {
    r.unique<-model2uniqueeffect(anNormC)*sign(r.direct)
    r.total<-model2totaleffect(lmNormC)
  }
  r.full<-model2fulleffect(lmNormC,anNormC)
  
  p.direct<-r2p(r.direct,n,df)
  p.unique<-r2p(r.unique,n,df)
  p.total<-r2p(r.total,n,df)
  
  return(list(r.direct=r.direct,
              r.unique=r.unique,
              r.total=r.total,
              r.full=r.full,
              
              p.direct=p.direct,
              p.unique=p.unique,
              p.total=p.total,
              
              lmRaw=lmRaw,
              lmNorm=lmNorm,
              lmRawC=lmRawC,
              lmNormC=lmNormC,
              
              df=df,
              
              anRaw=anRaw,
              anNorm=anNorm,
              anRawC=anRawC,
              anNormC=anNormC
  ))
}

analyseSample<-function(IV,IV2,DV,effect,design,evidence,result){

  switch (evidence$Transform,
          "Log"={allData<-data.frame(result$participant,log(result$dv))},
          "Exp"={allData<-data.frame(result$participant,exp(result$dv))},
          "None"={allData<-data.frame(result$participant,result$dv)}
  )
  if (!all(result$iv==result$iv[1])) 
    allData<-cbind(allData,result$iv)
  if (!is.null(IV2) && !all(result$iv2==result$iv2[1]))
    allData<-cbind(allData,result$iv2)
  no_ivs<-ncol(allData)-2
  n<-nrow(allData)

  withins<-c(design$sIV1Use=="Within",design$sIV2Use=="Within")
  
  anResult<-generalAnalysis(allData,evidence$rInteractionOn,withins,evidence$ssqType,evidence$evidenceCaseOrder)
  
# MOVE RESULTS OUT TO BRAWSTATS  
  r_use<-anResult$r.direct
  p_use<-anResult$p.direct
  result$rIV<-r_use[1]
  result$pIV<-p_use[1]
  result$rIVCI<-r2ci(result$rIV,n)
  result$pIVCI<-r2p(result$rIVCI,n,anResult$df[1])
  if (result$rIV>0 && result$rIVCI[1]<0 && result$rIVCI[2]>0) result$pIVCI[1]<-1
  if (result$rIV<0 && result$rIVCI[1]<0 && result$rIVCI[2]>0) result$pIVCI[2]<-1
  result$rpIV<-result$effectRho
  
  if (no_ivs==2) {
    result$rIV2<-r_use[2]
    result$pIV2<-p_use[2]
    result$rIV2CI<-r2ci(result$rIV2,n)
    result$pIV2CI<-r2p(result$rIV2CI,n,anResult$df[2])
    if (result$rIV2>0 && result$rIV2CI[1]<0 && result$rIV2CI[2]>0) result$pIV2CI[1]<-1
    if (result$rIV2<0 && result$rIV2CI[1]<0 && result$rIV2CI[2]>0) result$pIV2CI[2]<-1
    
    #  interaction term
    if (evidence$rInteractionOn==1) {
      result$rIVIV2DV<-r_use[3]
      result$pIVIV2DV<-p_use[3]
      result$rIVIV2CI<-r2ci(result$rIVIV2DV,n)
      result$pIVIV2CI<-r2p(result$rIVIV2CI,n,anResult$df[3])
      if (result$rIVIV2DV>0 && result$rIVIV2CI[1]<0 && result$rIVIV2CI[2]>0) result$pIVIV2CI[1]<-1
      if (result$rIVIV2DV<0 && result$rIVIV2CI[1]<0 && result$rIVIV2CI[2]>0) result$pIVIV2CI[2]<-1
    } else {
      result$rIVIV2DV<-NA
      result$pIVIV2DV<-NA
      result$rIVIV2CI<-NA
      result$pIVIV2CI<-NA
    }
  }
  result$rIVIV2<-0
  
  result$rFull<-anResult$r.full
  result$rFullse<-r2se(result$rFull,n)
  result$rFullCI<-r2ci(result$rFull,n)
  result$wFull<-rn2w(result$rFull,n)
  result$wFulln80<-rw2n(result$rFull,0.8)
  
  iv1<-result$iv
  iv2<-result$iv2
  dv<-result$dv
  # prepare the output to look like Jamovi
  if (any(class(anResult$lmRaw)[1]==c("lmerMod","glmerMod"))) {
    # we need to sort this to match Jamovi etc
    # we use anNormC as the starting point, but replace any Interval predictors
    anRaw<-anResult$anNormC
    if (!is.factor(iv1)) {
      anRaw["iv1",]<-anResult$anNormC["iv1",]
    }
    if (no_ivs>1 && !is.factor(iv1)) {
      anRaw["iv2",]<-anResult$anNormC["iv2",]
    }
    # now we move cells around
    newRow<-nrow(anRaw)+1
    anRaw[newRow,]<-c(NA,anRaw$Df.res[2],0,NA)
    rownames(anRaw)[newRow]<-"Error"
    anRaw["Df.res"]<-NA
    anRaw<-anRaw[,c(3,2,1,4)]
    colnames(anRaw)[1]<-""
    anRaw[,1]<-NA
    anRaw[1,2]<-0 # intercept df
    if ((IV$deploy=="Within") && (no_ivs>1 && IV2$deploy=="Within")) {
      z1<-summary(aov(dv~iv1*iv2+Error(participant/(iv1*iv2)),resultRawData))
      F<-c(0,
           z1$'Error: participant:iv1'[[1]]$'F value'[1],
           z1$'Error: participant:iv2'[[1]]$'F value'[1],
           z1$'Error: participant:iv1:iv2'[[1]]$'F value'[1],
           NA
      )
      p<-c(0,
           z1$'Error: participant:iv1'[[1]]$'Pr(>F)'[1],
           z1$'Error: participant:iv2'[[1]]$'Pr(>F)'[1],
           z1$'Error: participant:iv1:iv2'[[1]]$'Pr(>F)'[1],
           NA
      )
      anRaw[,3]<-F
      anRaw[,4]<-p
    }
    anResult$anRaw<-anRaw
  }
  
  lmRaw<-anResult$lmRaw
  lmNorm<-anResult$lmNorm
  lmRawC<-anResult$lmRawC
  lmNormC<-anResult$lmNormC
  
  anRaw<-anResult$anRaw
  anNorm<-anResult$anNorm
  anRawC<-anResult$anRawC
  anNormC<-anResult$anNormC
  
  # simulate the single IV analyses
  if (is.null(IV2)) {
    hypothesisType=paste(IV$type,DV$type,sep=" ")
    switch (hypothesisType,
            "Interval Interval"={
              an_name<-"Pearson Correlation"
              t_name<-"r"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              tval<-result$rIV
            },
            "Ordinal Interval"={
              an_name<-"Pearson Correlation"
              t_name<-"r"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              tval<-result$rIV
            },
            "Categorical Interval"={
              if (IV$ncats==2){
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"t-test: Paired Samples"
                  tv<-t.test(dv~iv1,paired=TRUE,var.equal=!evidence$Welch)
                  tval<-tv$statistic
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
                  result$pIV<-tv$p.value
                } else {
                  an_name<-"t-test: Independent Samples"
                  if (any(c(sum(iv1==levels(iv1)[1]),sum(iv1==levels(iv1)[2]))<3))
                  {             
                    tval<-0
                    result$pIV<-1
                    df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
                  } else {
                  tv<-t.test(dv~iv1,var.equal=!evidence$Welch)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                  df<-paste("(",format(tv$parameter),")",sep="")
                  }
                }
                t_name<-"t"
                # tval<-sqrt(anRaw$`F value`[2])*sign(result$rIV)
              } else {
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"One-Way ANOVA: Repeated Measures"
                } else {
                  an_name<-"One-Way ANOVA: Independent Measures"
                }
                t_name<-"F"
                if (evidence$Welch) {
                  tv<-oneway.test(dv~iv1, data = resultRawData, var.equal = FALSE)
                  tval<-tv$statistic
                  df<-paste("(",format(tv$parameter[1]),",",format(tv$parameter[2],digits=3),")",sep="")
                  result$pIV<-tv$p.value
                } else {
                  tval<-anRaw$`F value`[2]
                  if (is.null(tval)) {tval<-anRaw$F[2]}
                  df<-paste("(",format(anRaw$Df[2]),",",format(anRaw$Df[3]),")",sep="")
                  result$pIV<-anRaw$"Pr(>F)"[2]
                }
              }
            },
            "Interval Ordinal"={
              an_name<-"Spearman Correlation"
              t_name<-"rho"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              op <- options(warn = (-1))
              tv<-cor.test(iv1, dv, method="spearman")
              options(op)
              tval<-tv$estimate
              result$pIV<-tv$p.value
            },
            "Ordinal Ordinal"={
              an_name<-"Spearman Correlation"
              t_name<-"rho"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              op <- options(warn = (-1))
              tv<-cor.test(iv1, dv, method="spearman")
              options(op)
              tval<-tv$estimate
              result$pIV<-tv$p.value
            },
            "Categorical Ordinal"={
              if (IV$ncats==2){
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"Wilcoxon signed-rank Test: Paired Samples"
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")")
                  t_name<-"T"
                  op <- options(warn = (-1))
                  tv<-wilcox.test(dv~iv1,paired=TRUE,exact=FALSE)
                  options(op)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                } else {
                  an_name<-"Mann Whitney U test: Independent Samples"
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")")
                  t_name<-"U"
                  op <- options(warn = (-1))
                  tv<-wilcox.test(dv~iv1,exact=FALSE)
                  options(op)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                }
              } else {
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"Friedman Test: Repeated Measures"
                  op <- options(warn = (-1))
                  tv<-friedman(dv~iv1);
                  options(op)
                  t_name='chi2'
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                } else {
                  an_name<-"Kruskal Wallis Test: Independent Measures"
                  op <- options(warn = (-1))
                  tv<-kruskal.test(dv~iv1);
                  options(op)
                  t_name='chi2'
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                }
                df<-paste("(",format(anRaw$Df[2]),",n=",format(length(dv)),")",sep="")
              }
            },
            "Interval Categorical"={
              an_name<-"Logistic Regression"
              t_name<-"chi2"
              df<-paste("(",format(anRaw$Df[2]),",","n=",format(lmNormC$df.null+1),")",sep="")
              tval<-lmRaw$null.deviance-lmRaw$deviance
              result$pIV<-1-pchisq(tval,1) # noCases-1
              
            },
            "Ordinal Categorical"={
              an_name<-"Logistic Regression"
              t_name<-"chi2"
              df<-paste("(",format(anRaw$Df[2]),",","n=",format(lmNormC$df.null+1),")",sep="")
              tval<-lmRaw$null.deviance-lmRaw$deviance
              result$pIV<-1-pchisq(tval,1) # noCases-1
            },
            "Categorical Categorical"={
              an_name<-"Chi-square test of independence"
              t_name<-"chi2"
              print(c(lmRaw$null.deviance,lmRaw$deviance))
              
              chiResult<-chisq.test(iv1,dv,correct = FALSE)
              df<-paste("(",format(chiResult$parameter),",","n=",format(length(result$participant)),")",sep="")
              
              nhold<-c()
              for (ini in 1:DV$ncats) {
                nhold<-c(nhold,sum(as.numeric(result$dv)==ini))
              }
              ncorrection<-(max(nhold)/min(nhold))
              result$rIV<-sqrt(unname(chiResult$statistic/n/ncorrection))*sign(result$rIV)
              result$pIV<-chiResult$p.value
              result$rFull<-result$rIV
              result$rFullse<-r2se(result$rFull,n)
              tval<-chiResult$statistic
            }
    )
    if (is.na(result$pIV)) {result$pIV<-1}
  } else {
    switch (DV$type,
            "Interval"={
              an_name<-"General Linear Model"
              t_name<-"F"
              # df<-anResult$anRaw$Df
              tval<-anResult$anRaw$`F value`
            },
            "Ordinal"={
              an_name<-"General Linear Model"
              t_name<-"F"
              # df<-anResult$anRaw$Df
              tval<-anResult$anRaw$`F value`
            },
            "Categorical"={
              an_name<-"Generalized Linear Model"
              t_name<-"chi2"
              # df<-anResult$anRaw$Df
              tval<-anResult$anRaw$Deviance
            }
    )
    df<-anResult$df
    
    result$r=list(direct=anResult$r.direct,unique=anResult$r.unique,total=anResult$r.total)
    result$rse=list(direct=r2se(anResult$r.direct,n),unique=r2se(anResult$r.unique,n),total=r2se(anResult$r.total,n))
    result$p=list(direct=anResult$p.direct,unique=anResult$p.unique,total=anResult$p.total)
  }
  
  # adding fields to existing result
  result$rawModel<-anResult$lmRaw
  result$normModel<-anResult$lmNorm
  result$rawModelC<-anResult$lmRawC
  result$normModelC<-anResult$lmNormC
  
  result$rawAnova<-anResult$anRaw
  result$normAnova<-anResult$anNorm
  result$rawAnovaC<-anResult$anRawC
  result$normAnovaC<-anResult$anNormC
  result$nval<-n
  if (IV$type=="Categorical") {
    result$df1<-IV$ncats-1
  } else {
    result$df1<-1
  }
  if (no_ivs>1) {
    if (IV2$type=="Categorical") {
      result$df2<-IV2$ncats-1
    } else {
      result$df2<-1
    }
  } else {
    result$df2<-0
  }
  result$df12<-result$df1*result$df2
  
  result$model<-result$rawModel
  result$anova<-result$rawAnovaC
  
  result$an_name<-an_name
  result$test_name<-t_name
  result$df<-df
  result$test_val<-tval
  
  result$effect<-effect
  result$design<-design
  result$evidence<-evidence
  
  result$ResultHistory<-ResultHistory
  
  result$showType<-evidence$showType
  result$Heteroscedasticity<-0
  result
  
}

runSimulation<-function(IV,IV2,DV,effect,design,evidence,sig_only=FALSE,onlyAnalysis=FALSE,oldResult=NULL) {
  if (onlyAnalysis && !is.null(oldResult)) {
    res<-analyseSample(IV,IV2,DV,effect,design,evidence,oldResult)
    return(res)
  }
  
  ntrials<-0
  p_min<-1
  while (1==1) {
    if (!shortHand) {
      # sample<-makeSample(IV,IV2,DV,effect,design)
      # res<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
      res<-getSample(IV,IV2,DV,effect,design)
    } else {
      res<-sampleShortCut(IV,IV2,DV,effect,design,evidence,1,FALSE)
    }
    res1<-res
    if (design$sBudgetOn) {
      if (res$pIV<p_min) {
        p_min<-res$pIV
        res1<-res
      } else {
        res<-res1
      }
      ntrials<-ntrials+res$nval
      if (ntrials>=design$sNBudget) {
        break
      }
    } else {
      break
    }
  }
  
  # sig only
  while (sig_only && !isSignificant(STMethod,res$pIV,res$rIV,res$nval,res$df1,evidence)) {
    if (!shortHand) {
      # sample<-makeSample(IV,IV2,DV,effect,design)
      # res<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
      res<-getSample(IV,IV2,DV,effect,design)
    } else {
      res<-sampleShortCut(IV,IV2,DV,effect,design,evidence,1,FALSE)
    }
  }
  # # Cheating ?
  # res<-cheatSample(IV,IV2,DV,effect,design,evidence,sample,res)
  # Replication?
  res<-replicateSample(IV,IV2,DV,effect,design,evidence,sample,res)
  
  res
  
}

getSample<-function(IV,IV2,DV,effect,design) {
  if (!shortHand) {
    sample<-makeSample(IV,IV2,DV,effect,design)
    res<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
  } else {
    res<-sampleShortCut(IV,IV2,DV,effect,design,evidence,1,FALSE)
  }
  # Cheating ?
  res<-cheatSample(IV,IV2,DV,effect,design,evidence,sample,res)
  res
}
