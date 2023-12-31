
reportPlot<-function(outputText,nc,nr,rd=1){

  bg<-maincolours$graphC
  margin=0.5
  colSpace=2
  font_size=4
  top=12*rd
  font_size_extra=0
  characterWidth=1/14/sqrt(rd)
  
  oT<-matrix(outputText,ncol=nc,byrow=TRUE)
  nT<-nchar(oT)
  nrT<-rowSums(nT)
  # now break into blocks separated by empty rows
  blockEnds<-c(0,which(nrT==0),nrow(nT))
  colX<-c()
  if (length(blockEnds)>1) {
  for (i in 2:length(blockEnds)){
    block<-nT[(blockEnds[i-1]+1):blockEnds[i],]
    if (is.null(dim(block))){
      colSize<-block+colSpace
      colOffset<-cumsum(c(0,colSize))
      colX<-c(colX,rep(colOffset[1:length(block)],1))
    }
    else     {
      colSize<-apply(block,2,max)+colSpace
      colOffset<-cumsum(c(0,colSize))
      colX<-c(colX,rep(colOffset[1:ncol(block)],nrow(block)))
    }
  }
  }
  x_gap1<-colX*font_size*characterWidth
  
  # x_gap=apply(nchar(matrix(outputText,nrow=nr,ncol=nc,byrow=TRUE)),2,max)*0.25+2
  # x_gap=cumsum(c(0,x_gap[1:nc-1]))
  # x_gap1=rep(x_gap,nr)
  
  d<-expand.grid(x=1:nc,y=1:nr)

  boldlabels<-grepl("\b",outputText)
  outputText<-sub("\b","",outputText)
  redlabels<-grepl("\r",outputText)
  outputText<-sub("\r","",outputText)
  greenlabels<-grepl("!g",outputText)
  outputText<-sub("!g","",outputText)
  pts<-data.frame(x=x_gap1,y=d$y)
  g<-ggplot()
  # print(c(sum(boldlabels),sum(!boldlabels)))
  
  if (any(boldlabels)){
    pts1<-data.frame(x=x_gap1[boldlabels],y=d$y[boldlabels],labels=outputText[boldlabels])
    g<-g+geom_label(data=pts1,aes(x=x+1, y=top+1-y, label=labels), hjust=0, vjust=0, size=font_size+font_size_extra, fill=bg,fontface="bold",
                    label.size=NA,label.padding=unit(0,"lines"))
  }
  if (any(redlabels)) {
    pts1<-data.frame(x=x_gap1[redlabels],y=d$y[redlabels],labels=outputText[redlabels])
    g<-g+geom_label(data=pts1,aes(x=x+1, y=top+1-y, label=labels), hjust=0, vjust=0, size=font_size+font_size_extra, fontface="bold", color="black",fill="red",
                    label.size=NA,label.padding=unit(0,"lines"))
  }
  if (any(greenlabels)) {
    pts1<-data.frame(x=x_gap1[greenlabels],y=d$y[greenlabels],labels=outputText[greenlabels])
    g<-g+geom_label(data=pts1,aes(x=x+1, y=top+1-y, label=labels), hjust=0, vjust=0, size=font_size+font_size_extra, fontface="bold", color="black",fill="green",
                    label.size = NA,label.padding=unit(0,"lines"))
  }
  if (any(!boldlabels)) {
    x1<-x_gap1[!boldlabels & !redlabels & !greenlabels]
    y1<-d$y[!boldlabels & !redlabels & !greenlabels]
    t1<-outputText[!boldlabels & !redlabels & !greenlabels]
    mathlabels<-grepl("['^']{1}[0-9.{}]",t1)
    pts<-data.frame(x=x1[!mathlabels],y=y1[!mathlabels],labels=t1[!mathlabels])
    g<-g+geom_label(data=pts,aes(x=x+1, y=top+1-y, label=labels), hjust=0, vjust=0, size=font_size, fill=bg,
                    label.size=NA,label.padding=unit(0,"lines"))
    if (any(mathlabels)) {
    pts<-data.frame(x=x1[mathlabels],y=y1[mathlabels],labels=t1[mathlabels])
    g<-g+geom_label(data=pts,aes(x=x+1, y=top+1-y, label=labels), hjust=0, vjust=0, size=font_size, fill=bg,parse=TRUE,
                    label.size=NA,label.padding=unit(0,"lines"))
    }
  }

  g<-g+labs(x="  ",y="  ")+plotTheme+theme(legend.position = "none")
  g<-g+theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_rect(fill=maincolours$graphC, colour=maincolours$graphC)
    )
  g+coord_cartesian(xlim = c(1-margin,25+margin), ylim = c(1-margin,top+margin))
}
