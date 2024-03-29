npoints=10
min_n=10
max_n=250
min_prop=0.2
absRange<-FALSE
quants=0.25

exploreSimulate <- function(IV,IV2,DV,effect,design,evidence,metaAnalysis,explore,exploreResult,nsim,doingNull=FALSE,showProgress=FALSE){
  
  oldAlpha<-alphaSig
  npoints<-explore$Explore_npoints
  quants<-explore$Explore_quants
  max_n<-explore$Explore_nRange
  effectSizeRange<-explore$Explore_esRange
  maxESrange<-explore$Explore_esRange
  anomaliesRange<-explore$Explore_anomRange
  kurtRange<-10^5
  
  # showNotification(paste(format(0),format(npoints),sep="/"),id="counting",duration=Inf,closeButton = FALSE,type = "message")
  
  if (absRange) {vals<-seq(0,1,length.out=npoints)}
  else          {vals<-seq(-1,1,length.out=npoints)}
  switch (explore$Explore_type,
          "IVType"={vals<-c("Interval","Ord7","Ord4","Cat2","Cat3")},
          "DVType"={vals<-c("Interval","Ord7","Ord4","Cat2")},
          "IVIV2Type"={vals<-c("IntInt","Cat2Int","Cat3Int","IntCat","Cat2Cat","Cat3Cat")},
          "IVDVType"={vals<-c("IntInt","Ord7Int","Cat2Int","Cat3Int","IntOrd","Ord7Ord","Cat2Ord","Cat3Ord","IntCat","Ord7Cat","Cat2Cat","Cat3Cat")},
          "IVcats"={vals<-2:7},
          "IVlevels"={vals<-2:10},
          "IVprop"={vals<-seq(min_prop,1,length.out=npoints)},
          "IVskew"={vals<-vals},
          "IVkurtosis"={vals<-seq(0,log10(kurtRange),length.out=npoints)},
          "DVcats"={vals<-2:7},
          "DVlevels"={vals<-2:10},
          "DVprop"={vals<-seq(min_prop,1,length.out=npoints)},
          "DVskew"={vals<-vals},
          "DVkurtosis"={vals<-seq(0,log10(kurtRange),length.out=npoints)},
          "EffectSize"={vals<-vals*effectSizeRange},
          "EffectSize1"={
            # fullES<-effect$rIV^2+effect$rIV2^2+2*effect$rIV*effect$rIV2*effect$rIVIV2+
            b<-2*effect$rIV2*effect$rIVIV2
            c<-effect$rIV2^2+effect$rIVIV2DV^2-maxESrange
            r1<- (-b-sqrt(b^2-4*c))/2
            r2<-(-b+sqrt(b^2-4*c))/2
            vals<-seq(r1,r2,length.out=npoints)
            },
          "EffectSize2"={
            b<-2*effect$rIV*effect$rIVIV2
            c<-effect$rIV^2+effect$rIVIV2DV^2-maxESrange
            r1<- (-b-sqrt(b^2-4*c))/2
            r2<-(-b+sqrt(b^2-4*c))/2
            vals<-seq(r1,r2,length.out=npoints)
          },
          "Covariation"={
            # fullES<-effect$rIV^2+effect$rIV2^2+2*effect$rIV*effect$rIV2*effect$rIVIV2+effect$rIVIV2DV^2
            maxCov<-abs((maxESrange-effect$rIV^2-effect$rIV2^2-effect$rIVIV2DV^2)/(2*effect$rIV*effect$rIV2))
            maxCov<-min(maxCov,maxESrange)
            vals<-seq(-maxCov,maxCov,length.out=npoints)
            },
          "Interaction"={
            vals<-vals*effectSizeRange
            },
          
          "PDF"={vals<-c("Single","Uniform","Gauss","Exp",">","<")},
          "k"={vals<-10^seq(-1,-0.1,length.out=npoints)},
          "pNull"={vals<-seq(0,1,length.out=npoints)},
          
          "SampleSize"={
            if (explore$Explore_xlog){
              vals<-round(10^seq(log10(min_n),log10(max_n),length.out=npoints))
            }else{
              vals<-round(seq(min_n,max_n,length.out=npoints))
            }
          },
          "Method"={vals<-c("Random","Stratified","Cluster","Convenience","Snowball")},
          "Usage"={vals<-c("Between","Within")},
          "SampleGamma"={vals<-seq(1,10,length.out=npoints)},
          "Alpha"={
            if (explore$Explore_xlog) {
            vals<-vals<-10^seq(log10(0.001),log10(0.5),length.out=npoints)
            } else {
              vals<-vals<-seq(0.001,0.1,length.out=npoints)
            }
            },
          "Dependence"={vals<-seq(0,anomaliesRange,length.out=npoints)},
          "Outliers"={vals<-seq(0,anomaliesRange,length.out=npoints)},
          "Heteroscedasticity"={vals<-seq(0,1,length.out=npoints)},
          "Transform"={vals<-c("None","Log","Exp")},
          "IVRange"={vals<-seq(3,0.5,length.out=npoints)},
          "DVRange"={vals<-seq(3,0.5,length.out=npoints)},
          "Cheating"={vals<-c("None","Grow","Prune","Replace","Retry","Add")},
          "CheatingAmount"={
            if (explore$Explore_xlog){
              vals<-round(10^seq(log10(1),log10(max_n),length.out=npoints))
            }else{
              if ((max_n+1)<npoints) vals<-0:max_n
              else vals<-round(seq(0,max_n,length.out=npoints))
            }
          },
          
          "SigOnly"={vals<-c("Yes","No")},
          "Power"={vals<-seq(0.1,0.9,length.out=npoints)},
          "Repeats" ={
            if (design$sReplKeep=="median") vals<-seq(0,explore$Explore_nrRange,by=2)
            else vals<-seq(0,explore$Explore_nrRange)
            },
          
          "NoStudies"={
            if (explore$Explore_Mxlog){
              vals<-round(10^seq(log10(min_n),log10(explore$Explore_metaRange),length.out=npoints))
            }else{
              vals<-round(seq(min_n,explore$Explore_metaRange,length.out=npoints))
            }
            },
          "sig_only"={vals<-c(FALSE,TRUE)}
  )

  n_sims<-nsim
  if (is.null(exploreResult$rIVs)) {nc<-0}
  else {nc<-exploreResult$count}
  
  for (ni in 1:n_sims){
    main_res<-list(rval=c(),pval=c(),nval=c(),
                   r1=list(direct=c(),unique=c(),total=c()),
                   r2=list(direct=c(),unique=c(),total=c()),
                   r3=list(direct=c(),unique=c(),total=c())
                   )
  
    if (showProgress) {
      if (doingNull) {
        label<-paste("Explore/Null(",explore$Explore_family,")",":  n=",format(nc+ni),"/",format(exploreResult$nsims),sep="")
        showNotification(label,id="counting",duration=Inf,closeButton = FALSE,type = "message")
      } else {
        label<-paste("Explore(",explore$Explore_family,")",":  n=",format(nc+ni),"/",format(exploreResult$nsims),sep="")
        showNotification(label,id="counting",duration=Inf,closeButton = FALSE,type = "message")
      }
    }
    for (i in 1:length(vals)){
      
    switch (explore$Explore_type,
            "IVType"={
              switch (vals[i],
                      "Cat2"={
                        IV$type<-"Categorical"
                        IV$ncats<-2
                        IV$cases<-c("C1","C2")
                        IV$proportions<-c(1,1)
                      },
                      "Cat3"={
                        IV$type<-"Categorical"
                        IV$ncats<-3
                        IV$cases<-c("C1","C2","C3")
                        IV$proportions<-c(1,1,1)
                      },
                      "Ord7"={
                        IV$type<-"Ordinal"
                        IV$nlevs<-7
                      },
                      "Ord4"={
                        IV$type<-"Ordinal"
                        IV$nlevs<-4
                      },
                      "Interval"={IV$type<-"Interval"}
              )
              },
            "DVType"={
              switch (vals[i],
                      "Cat2"={
                        DV$type<-"Categorical"
                        DV$ncats<-2
                        DV$cases<-c("E1","E2")
                        DV$proportions<-c(1,1)
                      },
                      # "Cat3"={
                      #   DV$type<-"Categorical"
                      #   DV$ncats<-3
                      #   DV$cases<-c("D1","D2","D3")
                      # },
                      "Ord7"={
                        DV$type<-"Ordinal"
                        DV$nlevs<-7
                      },
                      "Ord4"={
                        DV$type<-"Ordinal"
                        DV$nlevs<-4
                      },
                      "Interval"={DV$type<-"Interval"}
              )
            },
            "IVDVType"={
              switch (vals[i],
                      "IntInt"={
                        IV$type<-"Interval"
                        DV$type<-"Interval"
                      },
                      "Ord7Int"={
                        IV$type<-"Ordinal"
                        IV$nlevs<-7
                        DV$type<-"Interval"
                      },
                      "Ord4Int"={
                        IV$type<-"Ordinal"
                        IV$nlevs<-4
                        DV$type<-"Interval"
                      },
                      "Cat2Int"={
                        IV$type<-"Categorical"
                        IV$ncats<-2
                        IV$cases<-c("C1","C2")
                        IV$proportions<-c(1,1)
                        DV$type<-"Interval"
                      },
                      "Cat3Int"={
                        IV$type<-"Categorical"
                        IV$ncats<-3
                        IV$cases<-c("C1","C2","C3")
                        IV$proportions<-c(1,1,1)
                        DV$type<-"Interval"
                      },
                      "IntOrd"={
                        IV$type<-"Interval"
                        DV$type<-"Ordinal"
                        DV$nlevs<-7
                      },
                      "Ord7Ord"={
                        IV$type<-"Ordinal"
                        IV$nlevs<-7
                        DV$type<-"Ordinal"
                        DV$nlevs<-7
                      },
                      "Ord4Ord"={
                        IV$type<-"Ordinal"
                        IV$nlevs<-4
                        DV$type<-"Ordinal"
                        DV$nlevs<-7
                      },
                      "Cat2Ord"={
                        IV$type<-"Categorical"
                        IV$ncats<-2
                        IV$cases<-c("C1","C2")
                        IV$proportions<-c(1,1)
                        DV$type<-"Ordinal"
                        DV$nlevs<-7
                      },
                      "Cat3Ord"={
                        IV$type<-"Categorical"
                        IV$ncats<-3
                        IV$cases<-c("C1","C2","C3")
                        IV$proportions<-c(1,1,1)
                        DV$type<-"Ordinal"
                        DV$nlevs<-7
                      },
                      "IntCat"={
                        IV$type<-"Interval"
                        DV$type<-"Categorical"
                        DV$ncats<-2
                        DV$cases<-c("E1","E2")
                        DV$proportions<-c(1,1)
                      },
                      "Ord7Cat"={
                        IV$type<-"Ordinal"
                        IV$nlevs<-7
                        DV$type<-"Categorical"
                        DV$ncats<-2
                        DV$cases<-c("E1","E2")
                        DV$proportions<-c(1,1)
                      },
                      "Ord4Cat"={
                        IV$type<-"Ordinal"
                        IV$nlevs<-4
                        DV$type<-"Categorical"
                        DV$ncats<-2
                        DV$cases<-c("E1","E2")
                        DV$proportions<-c(1,1)
                      },
                      "Cat2Cat"={
                        IV$type<-"Categorical"
                        IV$ncats<-2
                        IV$cases<-c("C1","C2")
                        IV$proportions<-c(1,1)
                        DV$type<-"Categorical"
                        DV$ncats<-2
                        DV$cases<-c("E1","E2")
                        DV$proportions<-c(1,1)
                      },
                      "Cat3Cat"={
                        IV$type<-"Categorical"
                        IV$ncats<-3
                        IV$cases<-c("C1","C2","C3")
                        IV$proportions<-c(1,1,1)
                        DV$type<-"Categorical"
                        DV$ncats<-2
                        DV$cases<-c("E1","E2")
                        DV$proportions<-c(1,1)
                      }
              )
            },
            "IVIV2Type"={
              switch (vals[i],
                      "IntInt"={
                        IV$type<-"Interval"
                        IV2$type<-"Interval"
                      },
                      "Cat2Int"={
                        IV$type<-"Categorical"
                        IV$ncats<-2
                        IV$cases<-c("C1","C2")
                        IV2$type<-"Interval"
                      },
                      "Cat3Int"={
                        IV$type<-"Categorical"
                        IV$ncats<-3
                        IV$cases<-c("C1","C2","C3")
                        IV2$type<-"Interval"
                      },
                      "IntCat"={
                        IV$type<-"Interval"
                        IV2$type<-"Categorical"
                        IV2$ncats<-2
                        IV2$cases<-c("D1","D2")
                      },
                      "Cat2Cat"={
                        IV$type<-"Categorical"
                        IV$ncats<-2
                        IV$cases<-c("C1","C2")
                        IV2$type<-"Categorical"
                        IV2$ncats<-2
                        IV2$cases<-c("D1","D2")
                      },
                      "Cat3Cat"={
                        IV$type<-"Categorical"
                        IV$ncats<-3
                        IV$cases<-c("C1","C2","C3")
                        IV2$type<-"Categorical"
                        IV2$ncats<-2
                        IV2$cases<-c("D1","D2")
                      }
              )
            },
            "IVprop"={
              IV$type<-"Categorical"
              IV$proportions<-c(vals[i],1)
            },
            "IVskew"={
              IV$type<-"Interval"
              IV$skew<-vals[i]
            },
            "IVkurtosis"={
              IV$type<-"Interval"
              IV$kurtosis<-10^vals[i]
            },
            "IVcats"={
              IV$type<-"Categorical"
              IV$ncats<-vals[i]
              IV$cases<-format(1:IV$ncats)
            },
            "DVprop"={
              DV$type<-"Categorical"
              DV$proportions<-c(vals[i],1)
            },
            "DVlevels"={
              DV$type<-"Ordinal"
              DV$nlevs<-vals[i]
              DV$median<-(DV$nlevs+1)/2
              DV$iqr<-(DV$nlevs-1)/2
            },
            "DVcats"={
              DV$type<-"Categorical"
              DV$ncats<-vals[i]
            },
            "DVskew"={
              DV$type<-"Interval"
              DV$skew<-vals[i]
            },
            "DVkurtosis"={
              DV$type<-"Interval"
              DV$kurtosis<-10^vals[i]
            },
            "EffectSize"={effect$rIV<-vals[i]},
            "EffectSize1"={effect$rIV<-vals[i]},
            "EffectSize2"={effect$rIV2<-vals[i]},
            "Covariation"={effect$rIVIV2<-vals[i]},
            "Interaction"={effect$rIVIV2DV<-vals[i]},
            
            "PDF"={
              effect$world$worldOn<-TRUE
              effect$world$populationPDF<-vals[i]
              },
            "k"={
              effect$world$worldOn<-TRUE
              effect$world$populationPDFk<-vals[i]
              },
            "pNull"={
              effect$world$worldOn<-TRUE
              effect$world$populationNullp<-vals[i]
                     metaAnalysis$meta_nullAnal<-TRUE
                     },
            
            "Heteroscedasticity"={effect$Heteroscedasticity<-vals[i]},
            "Transform"={evidence$Transform<-vals[i]},
            "SampleSize"={design$sN<-round(vals[i])},
            "Method"={design$sMethod<-vals[i]},
            "Usage"={design$sIV1Use<-vals[i]},
            "SampleGamma"={
              design$sNRand<-TRUE
              design$sNRandK<-vals[i]
              },
            "Alpha"={
              alphaSig<<-vals[i]
              alphaLLR<<-0.5*qnorm(1-alphaSig/2)^2
            },
            "Dependence"={design$sDependence<-vals[i]},
            "Outliers"={design$sOutliers<-vals[i]},
            "IVRange"={
              design$sRangeOn<-TRUE
              design$sIVRange<-vals[i]*c(-1,1)
              },
            "DVRange"={
              design$sRangeOn<-TRUE
              design$sDVRange<-vals[i]*c(-1,1)
              },
            "Cheating"={
              design$sCheating<-vals[i]
            },
            "CheatingAmount"={
              design$sCheatingAmount<-vals[i]
            },
            
            "SigOnly"={
              design$sReplSigOnly<-vals[i]
            },
            "Power"={
            design$sReplPower<-vals[i]
            },
            "Repeats"={
              design$sReplRepeats<-vals[i]
            },
            
            "NoStudies"={
              metaAnalysis$nstudies<-vals[i]
            },
            "sig_only"={
              metaAnalysis$sig_only<-vals[i]
              metaAnalysis$meta_psigAnal<-vals[i]
            }
    )

    # if (explore$doNull) {
    #   effect$rIV<-0
    #   effect$rIV2<-0
    #   effect$rIVIV2DV<-0
    # }

      if (explore$Explore_show=="likelihood") 
      {
        exploreResult$rIVs<-matrix(0,nrow=nsim,ncol=1)
        exploreResult$likes<-rnorm(length(vals))
        exploreResult$vals<-vals
        exploreResult$Explore_type<-explore$Explore_type
        exploreResult$Explore_show<-explore$Explore_show
        exploreResult$Explore_typeShow<-explore$Explore_typeShow
        exploreResult$Explore_family<-explore$Explore_family
        return(exploreResult)
        
      } 
      
      if (explore$Explore_family=="MetaAnalysis") {
          result<-multipleAnalysis(IV,IV2,DV,effect,design,evidence,metaAnalysis$nstudies,FALSE,metaResult$result,sigOnly=metaAnalysis$sig_only,
                                   showProgress=FALSE,progressPrefix=paste0("MetaAnalysis: ",metaResult$count+1,":"))
        metaResult$result<-result
        metaResult<-runMetaAnalysis(metaAnalysis,metaResult)
        main_res$ks<-cbind(main_res$ks,metaResult$bestK)
        main_res$pnulls<-cbind(main_res$pnulls,metaResult$bestNull)
        main_res$Ss<-cbind(main_res$Ss,metaResult$bestS)
        main_res$dists<-cbind(main_res$dists,metaResult$bestDist)
        main_res$rval<-cbind(main_res$rval,metaResult$bestS)
      } else {
        res<-multipleAnalysis(IV,IV2,DV,effect,design,evidence,1,appendData=FALSE,sigOnly=FALSE,showProgress=FALSE)
        main_res$rval<-cbind(main_res$rval,res$rIV)
        main_res$rpval<-cbind(main_res$rpval,res$rpIV)
        main_res$pval<-cbind(main_res$pval,res$pIV)
        main_res$nval<-cbind(main_res$nval,res$nval)
        main_res$df1<-cbind(main_res$df1,res$df1)

        if (!is.null(IV2)){
          main_res$r1$direct<-cbind(main_res$r1$direct,res$r$direct[,1])
          main_res$r1$unique<-cbind(main_res$r1$unique,res$r$unique[,1])
          main_res$r1$total<-cbind(main_res$r1$total,res$r$total[,1])
          
          main_res$r2$direct<-cbind(main_res$r2$direct,res$r$direct[,2])
          main_res$r2$unique<-cbind(main_res$r2$unique,res$r$unique[,2])
          main_res$r2$total<-cbind(main_res$r2$total,res$r$total[,2])
          
          main_res$r3$direct<-cbind(main_res$r3$direct,res$r$direct[,3])
          main_res$r3$unique<-cbind(main_res$r3$unique,res$r$unique[,3])
          main_res$r3$total<-cbind(main_res$r3$total,res$r$total[,3])
          
          main_res$p1$direct<-cbind(main_res$p1$direct,res$p$direct[,1])
          main_res$p1$unique<-cbind(main_res$p1$unique,res$p$unique[,1])
          main_res$p1$total<-cbind(main_res$p1$total,res$p$total[,1])
          
          main_res$p2$direct<-cbind(main_res$p2$direct,res$p$direct[,2])
          main_res$p2$unique<-cbind(main_res$p2$unique,res$p$unique[,2])
          main_res$p2$total<-cbind(main_res$p2$total,res$p$total[,2])
          
          main_res$p3$direct<-cbind(main_res$p3$direct,res$p$direct[,3])
          main_res$p3$unique<-cbind(main_res$p3$unique,res$p$unique[,3])
          main_res$p3$total<-cbind(main_res$p3$total,res$p$total[,3])
        }
        # if (i>1) {removeNotification(id = "counting")}
      }
    }
    
    if (explore$Explore_family=="MetaAnalysis") {
      exploreResult$rIVs<-rbind(exploreResult$rIVs,main_res$rval)
      exploreResult$ks<-rbind(exploreResult$ks,main_res$ks)
      exploreResult$pnulls<-rbind(exploreResult$pnulls,main_res$pnulls)
      exploreResult$Ss<-rbind(exploreResult$Ss,main_res$Ss)
      exploreResult$dists<-rbind(exploreResult$dists,main_res$dists)
      
    } else {
      exploreResult$rIVs<-rbind(exploreResult$rIVs,main_res$rval)
      # if (!is.null(exploreResult$rpIVs))
      exploreResult$rpIVs<-rbind(exploreResult$rpIVs,main_res$rpval)
      exploreResult$pIVs<-rbind(exploreResult$pIVs,main_res$pval)
      exploreResult$nvals<-rbind(exploreResult$nvals,main_res$nval)
      exploreResult$df1vals<-rbind(exploreResult$df1vals,main_res$df1)
      
        exploreResult$r1$direct<-rbind(exploreResult$r1$direct,main_res$r1$direct)
        exploreResult$r1$unique<-rbind(exploreResult$r1$unique,main_res$r1$unique)
        exploreResult$r1$total<-rbind(exploreResult$r1$total,main_res$r1$total)
        
        exploreResult$r2$direct<-rbind(exploreResult$r2$direct,main_res$r2$direct)
        exploreResult$r2$unique<-rbind(exploreResult$r2$unique,main_res$r2$unique)
        exploreResult$r2$total<-rbind(exploreResult$r2$total,main_res$r2$total)
        
        exploreResult$r3$direct<-rbind(exploreResult$r3$direct,main_res$r3$direct)
        exploreResult$r3$unique<-rbind(exploreResult$r3$unique,main_res$r3$unique)
        exploreResult$r3$total<-rbind(exploreResult$r3$total,main_res$r3$total)
        
        exploreResult$p1$direct<-rbind(exploreResult$p1$direct,main_res$p1$direct)
        exploreResult$p1$unique<-rbind(exploreResult$p1$unique,main_res$p1$unique)
        exploreResult$p1$total<-rbind(exploreResult$p1$total,main_res$p1$total)
        
        exploreResult$p2$direct<-rbind(exploreResult$p2$direct,main_res$p2$direct)
        exploreResult$p2$unique<-rbind(exploreResult$p2$unique,main_res$p2$unique)
        exploreResult$p2$total<-rbind(exploreResult$p2$total,main_res$p2$total)
        
        exploreResult$p3$direct<-rbind(exploreResult$p3$direct,main_res$p3$direct)
        exploreResult$p3$unique<-rbind(exploreResult$p3$unique,main_res$p3$unique)
        exploreResult$p3$total<-rbind(exploreResult$p3$total,main_res$p3$total)
        
        exploreResult$wIVs<-rn2w(exploreResult$rIVs,exploreResult$nvals)
        # for (i in 1:length(vals)) {
        #   p<-mean(isSignificant(STMethod,exploreResult$pIVs[,i],exploreResult$rIVs[,i],exploreResult$nvals[,i],evidence))
        #   exploreResult$psig[i]<-p
        #   exploreResult$psig25[i]<-p-sqrt(p*(1-p)/length(exploreResult$pIVs[,i]))
        #   exploreResult$psig75[i]<-p+sqrt(p*(1-p)/length(exploreResult$pIVs[,i]))
        # }
      }
    }
  # removeNotification(id = "counting")
  
  alphaSig<<-oldAlpha
  alphaLLR<<-0.5*qnorm(1-alphaSig/2)^2

  exploreResult$vals<-vals
  exploreResult$Explore_type<-explore$Explore_type
  exploreResult$Explore_show<-explore$Explore_show
  exploreResult$Explore_typeShow<-explore$Explore_typeShow
  exploreResult$Explore_family<-explore$Explore_family
  exploreResult
}
