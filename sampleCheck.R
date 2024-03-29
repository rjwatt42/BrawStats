changeAmount<-1

cheatSample<-function(IV,IV2,DV,effect,design,evidence,sample,result) {

  if (design$sCheating=="None") return(result)
  if (design$sCheatingAmount==0) return(result)
  if (isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence)) return(result)

  # fix the hypothesis
  effect$world$worldOn<-FALSE
  effect$rIV<-result$rpIV
  
  if (is.element(design$sCheating,c("Retry","Add"))) {
    ntrials<-0
    switch(design$sCheatingLimit,
           "Fixed"={limit<-design$sCheatingAmount},
           "Budget"={limit<-design$sCheatingBudget}
           )
    while (!isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence) && ntrials<limit) {
      sample<-makeSample(IV,IV2,DV,effect,design)
      result<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
      switch(design$sCheatingLimit,
             "Fixed"={ntrials<-ntrials+1},
             "Budget"={ntrials<-ntrials+result$nval}
      )
    }
    return(result)
  }
  
  if (is.element(design$sCheating,c("Grow","Replace"))) {
    design2<-design
    design2$sN<-design$sCheatingAmount*changeAmount
    design2$sNRand<-FALSE
    
    effect2<-effect

    sample2<-makeSample(IV,IV2,DV,effect2,design2)
  }

  if (is.element(design$sCheating,c("Prune","Replace"))) {
    ntrials<-0
    while (!isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence) && ntrials<design$sCheatingAmount) {
      ps<-c()
      for (i in 1:length(sample$iv)) {
        sample1<-sample
        use<-rep(TRUE,length(sample1$iv))
        use[i]<-FALSE
        sample1$participant<-sample1$participant[use]
        sample1$iv<-sample1$iv[use]
        sample1$dv<-sample1$dv[use]
        design$sN<-length(sample1$iv)
        result1<-analyseSample(IV,IV2,DV,effect,design,evidence,sample1)
        ps<-c(ps,result1$pIV)
      }
      switch(design$sCheating,
             "Prune"={
               keep<-ps>min(ps)
               sample$participant<-sample$participant[keep]
               sample$iv<-sample$iv[keep]
               sample$dv<-sample$dv[keep]
               sample$ivplot<-sample$ivplot[keep]
               sample$dvplot<-sample$dvplot[keep]},
             "Replace"={
               change<-which.min(ps)
               sample$iv[change]<-sample2$iv[ntrials+1]
               sample$dv[change]<-sample2$dv[ntrials+1]
               sample$ivplot[change]<-sample2$ivplot[ntrials+1]
               sample$dvplot[change]<-sample2$dvplot[ntrials+1]
             }
      )
      result<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
      ntrials<-ntrials+1
    }
    return(result)
  }
  
  if (design$sCheating=="Grow") {
    ntrials<-0
    while (!isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence) && ntrials<design$sCheatingAmount*changeAmount) {
      sample$participant<-c(sample$participant,length(sample$participant)+(1:changeAmount))
      sample$iv<-c(sample$iv,sample2$iv[ntrials+(1:changeAmount)])
      sample$dv<-c(sample$dv,sample2$dv[ntrials+(1:changeAmount)])
      sample$ivplot<-c(sample$ivplot,sample2$ivplot[ntrials+(1:changeAmount)])
      sample$dvplot<-c(sample$dvplot,sample2$dvplot[ntrials+(1:changeAmount)])
      design$sN<-design$sN+(1:changeAmount)
      
      result<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
      ntrials<-ntrials+changeAmount
    }
    return(result)
  }
  
  
  if (design$sCheating=="Replace") {
    ntrials<-0
    while (!isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$df1,evidence) && ntrials<design$sCheatingAmount) {
      ps<-c()
      for (i in 1:length(sample$iv)) {
        sample1<-sample
        use<-rep(TRUE,length(sample1$iv))
        use[i]<-FALSE
        sample1$participant<-sample1$participant[use]
        sample1$iv<-sample1$iv[use]
        sample1$dv<-sample1$dv[use]

        result1<-analyseSample(IV,IV2,DV,effect,design,evidence,sample1)
        ps<-c(ps,result1$pIV)
      }
      
      result<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
      ntrials<-ntrials+1
    }
    return(result)
  }
  
}

replicateSample<-function(IV,IV2,DV,effect,design,evidence,sample,res) {
  oldalpha<-alphaSig
  res1<-res
  ResultHistory<-list(n=res$nval,df1=res$df1,r=res$rIV,rp=res$rpIV,p=res$pIV)
  
  if (!isempty(design$sReplicationOn) && !is.na(design$sReplicationOn) && design$sReplicationOn) {
    if (design$sReplVarAlpha) alphaSig<<-oldalpha*design$sReplAlpha
    while (design$sReplSigOnly=="Yes" && !isSignificant(STMethod,res$pIV,res$rIV,res$nval,res$df1,evidence)) {
      if (!shortHand) {
        sample<-makeSample(IV,IV2,DV,effect,design)
        res<-analyseSample(IV,IV2,DV,effect,design,evidence,sample)
      } else {
        res<-sampleShortCut(IV,IV2,DV,effect,design,evidence,1,FALSE)
      }
      ResultHistory<-list(n=res$nval,df1=res$df1,r=res$rIV,rp=res$rpIV,p=res$pIV)
    }
    if (design$sReplVarAlpha) alphaSig<<-oldalpha/design$sReplAlpha
    
    res1<-res
    resHold<-res
    # now we freeze the population effect size
    effect$rIV<-res$rpIV
    effect$world$worldOn<-FALSE
    # if we are doing one-tailed we need the sign
    rSign<-sign(res$rIV)
    
    design1<-design
    design1$sNRand<-FALSE
    design1$sN<-res$nval
    if (design$sReplType=="Budget") {
      design$sReplRepeats<-1000
      budgetUse<-res$nval
    }
    if (design$sReplRepeats>0) {
    for (i in 1:design$sReplRepeats) {
      if (design$sReplKeep=="cautious" && !isSignificant(STMethod,res$pIV,res$rIV,res$nval,res$df1,evidence)) {
        break
      }
      # get the relevant sample effect size for the power calc
      if (design$sReplPowerOn && (design$sReplPower>0)) {
        switch(design$sReplCorrection,
               "None"={r<-res$rIV},
               "World"={r<-rSamp2Pop(res$rIV,design1$sN,effect$world)},
               "Prior"={r<-rSamp2Pop(res$rIV,design1$sN,evidence$prior)}
        ) 
        # get the new sample size
        design1$sN<-rw2n(r,design$sReplPower,design$sReplTails)
        design1$sNRand<-FALSE
        if (design$sReplType=="Budget") {
          design1$sN<-min(design1$sN,design$sReplBudget-budgetUse)
        }
      }

      if (!shortHand) {
        sample<-makeSample(IV,IV2,DV,effect,design1)
        res<-analyseSample(IV,IV2,DV,effect,design1,evidence,sample)
      } else {
        res<-sampleShortCut(IV,IV2,DV,effect,design1,evidence,1,FALSE)
      }
      if (design$sReplTails==1) {
        if (sign(res$rIV)!=sign(ResultHistory$r[1])) {
          res$pIV<-1
        }
      }
      
      if ((design$sReplKeep=="largeN" && res$nval>resHold$nval) || 
          (design$sReplKeep=="smallP" && res$pIV<resHold$pIV) || 
          design$sReplKeep=="last")
      { resHold<-res }
      ResultHistory$n<-c(ResultHistory$n,res$nval)
      ResultHistory$df1<-c(ResultHistory$df1,res$df1)
      ResultHistory$r<-c(ResultHistory$r,res$rIV)
      ResultHistory$rp<-c(ResultHistory$rp,res$rpIV)
      ResultHistory$p<-c(ResultHistory$p,res$pIV)
      
      if (design$sReplType=="Budget") {
        budgetUse<-budgetUse+res$nval
        if (budgetUse>=design$sReplBudget) break;
      }
    }
    }
    res<-resHold
    
    if (design$sReplKeep=="cautious") {
      use<-!isSignificant(STMethod,ResultHistory$p,ResultHistory$r,ResultHistory$n,ResultHistory$df1,evidence)
      use[1]<-FALSE
      if (any(use)) {
        use<-which(use)[1]
        res$rIV<-ResultHistory$r[use]
        res$nval<-ResultHistory$n[use]
        res$df1<-ResultHistory$df1[use]
        res$pIV<-ResultHistory$p[use]
      }
    }
    
    if (design$sReplKeep=="median" && design$sReplRepeats>0) {
      use<-which(ResultHistory$p==sort(ResultHistory$p)[ceil(length(ResultHistory$p)/2)])
      use<-use[1]
        res$rIV<-ResultHistory$r[use]
        res$nval<-ResultHistory$n[use]
        res$df1<-ResultHistory$df1[use]
        res$pIV<-ResultHistory$p[use]
    }
    
  }
  alphaSig<<-oldalpha
  
  res$ResultHistory<-ResultHistory
  res$roIV<-res1$rIV
  res$no<-res1$nval
  res$df1o<-res1$df1
  res$poIV<-res1$pIV
  
  res
}
