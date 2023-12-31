source("sampleMake.R")
source("sampleAnalyse.R")
source("myGlobal.R")

runBatchFiles<-function(IV,IV2,DV,effect,design,evidence,input){

  subDir<-"DataFiles"
  if (!file.exists(subDir)){
    dir.create(file.path(pwd(), subDir))
  }

rvals<-c()    
pvals<-c()
files<-c()

nfiles<-input$batchFile_length
for (i in 1:nfiles) {
  print(paste("OK0 : ",format(i)))
  IVtype<-ceil(runif(1)*4)
  switch (IVtype,
          {IV$type<-"Interval"},
          {IV$type<-"Ordinal"},
          {IV$type<-"Categorical"
          IV$ncats<-2
          IV$cases<-c("C1","C2")
          IV$proportions<-c(1,1)},
          {IV$type<-"Categorical"
          IV$ncats<-3
          IV$cases<-c("C1","C2","C3")
          IV$proportions<-c(1,1,1)},
  )

  DVtype<-ceil(runif(1)*3)
  switch (DVtype,
          {DV$type<-"Interval"},
          {DV$type<-"Ordinal"},
          {DV$type<-"Categorical"
          DV$ncats<-2
          DV$cases<-c("E1","E2")
          DV$proportions<-c(1,1)},
  )
  switch (input$batchFile_nVars,
          "2"={IV2<-c()},
          "3"={
            IV2<-IV
            IV2type<-ceil(runif(1)*4)
            IV2$name<-"IV2"
            switch (IV2type,
                    {IV2$type<-"Interval"},
                    {IV2$type<-"Ordinal"},
                    {IV2$type<-"Categorical"
                    IV2$ncats<-2
                    IV2$cases<-c("D1","D2")
                    IV2$proportions<-c(1,1)},
                    {IV2$type<-"Categorical"
                    IV2$ncats<-3
                    IV2$cases<-c("D1","D2","D3")
                    IV2$proportions<-c(1,1,1)},
            )
            
          },
          "both"={
            if (runif(1)<0.5) {
              IV2<-c()
            } else {
              IV2<-IV
              IV2type<-ceil(runif(1)*4)
              IV2$name<-"IV2"
              switch (IV2type,
                      {IV2$type<-"Interval"},
                      {IV2$type<-"Ordinal"},
                      {IV2$type<-"Categorical"
                      IV2$ncats<-2
                      IV2$cases<-c("D1","D2")
                      IV2$proportions<-c(1,1)},
                      {IV2$type<-"Categorical"
                      IV2$ncats<-3
                      IV2$cases<-c("D1","D2","D3")
                      IV2$proportions<-c(1,1,1)},
              )
            }
          }
  )

  exponent<-0.1
  effect$rIV<-tanh(rexp(1,1/exponent))
  if (!is.null(IV2)) {
    effect$rIV2<-tanh(rexp(1,1/exponent))
    effect$rIVIV2<-tanh(rexp(1,1/exponent))
    effect$rIVIV2DV<-tanh(rexp(1,1/exponent))
    while(effect$rIV^2+effect$rIV2^2+effect$rIVIV2DV^2>1) {
      effect$rIV2<-tanh(rexp(1,1/exponent))
      effect$rIVIV2<-tanh(rexp(1,1/exponent))
      effect$rIVIV2DV<-tanh(rexp(1,1/exponent))
    }
  }
  design$sN<-round(runif(1,50,200))
# print("OK1")

  sample<-makeSample(IV,IV2,DV,effect,design)
  if (is.null(sample)) return(NULL)
  result<-analyseSample(IV,IV2,DV,design,evidence,sample)
  if (is.null(result)) return(NULL)
# print("OK2")

  iv<-sample$iv
  dv<-sample$dv
  if (is.null(IV2)){
    data<-data.frame(participant=result$participant,iv=iv,dv=dv)
    colnames(data)<-c("Participant",IV$name,DV$name)
    rvals<-rbind(rvals,c(result$rIV,0,0,0,0,0,0,0,0))
    pvals<-rbind(pvals,c(result$pIV,0,0,0,0,0,0,0,0))
  } else {
    iv2<-sample$iv2
    data<-data.frame(participant=result$participant,iv=iv,iv=iv2,dv=dv)
    colnames(data)<-c("Participant",IV$name,IV2$name,DV$name)
    rvals<-rbind(rvals,c(result$rIV,result$rIV2,result$rIVIV2DV,result$r$unique,result$r$total))
    pvals<-rbind(pvals,c(result$pIV,result$pIV2,result$pIVIV2DV,result$p$unique,result$p$total))
  }
# print("OK3")

  if (!is.null(data)) 
  {filename<-paste0(subDir,"/","Data",format(i),".xlsx")
  write_xlsx(data, path = filename)
  files<-rbind(files,filename)
  }
# print("OK4")

}

  filename<-paste0(subDir,"/","Results.xlsx")
  data<-data.frame(file=files,r=rvals,p=pvals)
  colnames(data)<-c("files",
                    "rIV","rIV2","rIVxIV2","rUniqueIV","rUniqueIV2","rUniqueIVxIV2","rTotalIV","rTotalIV2","rTotalIVxIV2",
                    "pIV","pIV2","pIVxIV2","pUniqueIV","pUniqueIV2","pUniqueIVxIV2","pTotalIV","pTotalIV2","pTotalIVxIV2")
  
  write_xlsx(data, path = filename)


}


