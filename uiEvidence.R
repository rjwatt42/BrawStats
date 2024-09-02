source("uiPrior.R")


basicType<-list("r"="r","p"="p")
likeType<-list("log(lrs)"="log(lrs)","log(lrd)"="log(lrd)")
powerType<-list("w"="w","nw"="nw")
worldType<-list("n"="n","rp"="rp","wp"="wp")
replicationType<-list("r1"="r1","p1"="p1")


inferShowParams<-list("rs"="rs","p"="p"," "=" ",
                        "n"="n","rp"="rp","wp"="wp"," "=" ",
                        "ws"="ws","nw"="nw"
)

inferShowParamsExtra<-list("rs"="rs","p"="p",
                             " "=" ","n"="n","rp"="rp","wp"="wp",
                             " "=" ","ws"="ws","nw"="nw",
                             " "=" ","ro"="ro","po"="po","no"="no"
)

singleTypeChoices<-list("Basic" = "Basic","Custom"="Custom")
singleTypeChoicesExtra<-c(singleTypeChoices,list("Likelihood"=likeType))
if (switches$doLikelihoodInfer) singleTypeChoices<-singleTypeChoicesExtra

multipleTypeChoices<-list("Basic" = "Basic","Custom"="Custom","Power" = "Power","NHST" = "NHST","Hits"="Hits","Misses"="Misses")

whichEffectShow=c("Main 1" = "Main 1",
                  "Main 2" = "Main 2",
                  "Interaction" = "Interaction",
                  "Mains" = "Mains",
                  "All" = "All")


EvidenceTab <-
  
  wellPanel(id="EvidenceMain",
            style = paste("background: ",panelcolours$simulateC), 
            fluidRow(headerText("Make a simulated sample; run multiple samples")),
            # h5("Evidence"),
            tabsetPanel(id="Evidence", type="tabs",
                        tabPanel("Evidence:",value="Evidence"
                        ),
                        # single tab
                        tabPanel("Single",value="Single",id="uiSingle",
                                 style = paste("background: ",subpanelcolours$simulateC), 
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "10%", tags$div(style = localStyle, "show:")),
                                              tags$td(width = "35%", 
                                                      selectInput("EvidenceInfer_type",label=NULL,
                                                                  singleTypeChoices,
                                                                  selectize=FALSE)
                                              ),
                                              tags$td(width = "15%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.EvidenceInfer_type=='Custom'",
                                                                       selectInput("EvidenceInfer_par1", label=NULL, 
                                                                                   inferShowParams,
                                                                                   selected="rs", selectize=FALSE)
                                                      ),
                                              ),
                                              tags$td(width = "15%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.EvidenceInfer_type=='Custom'",
                                                                       selectInput("EvidenceInfer_par2", label=NULL, 
                                                                                   inferShowParams,
                                                                                   selected="p", selectize=FALSE)
                                                      ),
                                              ),
                                              tags$td(width = "25%",style = localPlainStyle,
                                                      selectInput("EvidenceSingleDim", label=NULL, 
                                                                  choices=c("1D"="1D","2D"="2D"),
                                                                  selected="1D", selectize=FALSE)
                                              ),
                                            )
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "25%", actionButton("EvidencenewSample", "New Sample")),
                                              tags$td(width = "75%", tags$div(style = localPlainStyle, "")),
                                            ),
                                 )
                        ),
                        # multiple tab
                        tabPanel("Multiple",value="Multiple",id="uiMultiple",
                                 style = paste("background: ",subpanelcolours$simulateC), 
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "10%", tags$div(style = localStyle, "show:")),
                                              tags$td(width = "35%", 
                                                      selectInput("EvidenceExpected_type",label=NULL,
                                                                  multipleTypeChoices,
                                                                  selected="Basic",
                                                                  selectize=FALSE)
                                              ),
                                              tags$td(width = "15%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.EvidenceExpected_type=='Custom'",
                                                      selectInput("EvidenceExpected_par1", label=NULL, 
                                                                  inferShowParams,
                                                                  selected="r", selectize=FALSE)
                                                      ),
                                              ),
                                              tags$td(width = "15%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.EvidenceExpected_type=='Custom'",
                                                      selectInput("EvidenceExpected_par2", label=NULL, 
                                                                  inferShowParams,
                                                                  selected="p", selectize=FALSE)
                                                      ),
                                              ),
                                              tags$td(width = "25%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.EvidenceExpected_type=='Basic' | input.EvidenceExpected_type=='Custom' | input.EvidenceExpected_type=='Power'",
                                                                       selectInput("EvidenceExpectedDim", label=NULL, 
                                                                                   choices=c("1D"="1D","2D"="2D"),
                                                                                   selected="1D", selectize=FALSE)
                                                      ),
                                              ),
                                            ),
                                            tags$tr(
                                              tags$td(width = "10%",style = localPlainStyle),
                                              tags$td(width = "35%", 
                                                      conditionalPanel(condition="input.IV2choice != 'none'",
                                                                       selectInput("whichEffectExpected", label=NULL,
                                                                                   whichEffectShow, selected="All",selectize = FALSE)
                                                      )),
                                            )
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "20%",actionButton("EvidenceExpectedRun", startLabel)),
                                              tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                              tags$td(width = "55%", 
                                                      numericInput("EvidenceExpected_length",label=NULL,
                                                                  value = 100)
                                              ),
                                              tags$td(width = "15%", tags$div(style = localStyle, " "))
                                            )
                                 )
                        ),
                        tabPanel("#",id="EvidenceOptions",
                                 style = paste("background: ",subpanelcolours$simulateC),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "25%", tags$div(style = paste(localStyle,"text-align: left"), "Display")),
                                            ),
                                            tags$tr(
                                              tags$td(width = "35%", tags$div(style = localPlainStyle, "case order:")),
                                              tags$td(width = "65%", selectInput("evidenceCaseOrder", choices = c("Alphabetic"="Alphabetic","As Found"="AsFound","Frequency"="Frequency"),selected=braw.def$evidence$evidenceCaseOrder, label=NULL, selectize=FALSE)),
                                              # tags$td(width = "25%", tags$div(style = localPlainStyle, "scatter plot:")),
                                              # tags$td(width = "25%", selectInput("allScatter", label=NULL, c("none"="none","all"="all","corr"="corr"), selected=braw.def$evidence$allScatter, selectize=FALSE)),
                                            )
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "35%", tags$div(style = localPlainStyle, "p-scale:")),
                                              tags$td(width = "65%", selectInput("pScale", label=NULL, c("linear"="linear","log10"="log10"), selected=braw.env$pPlotScale, selectize=FALSE)),
                                            ),
                                            tags$tr(
                                              tags$td(width = "35%", tags$div(style = localPlainStyle, "w-scale:")),
                                              tags$td(width = "65%", selectInput("wScale", label=NULL, c("linear"="linear","log10"="log10"), selected=braw.env$wPlotScale, selectize=FALSE)),
                                            ),
                                            tags$tr(
                                              tags$td(width = "35%", tags$div(style = localPlainStyle, "n-scale:")),
                                              tags$td(width = "65%", selectInput("nScale", label=NULL, c("linear"="linear","log10"="log10"), selected=braw.env$nPlotScale, selectize=FALSE)),
                                              # tags$td(width = "25%", tags$div(style = localPlainStyle, "Sig Only")),
                                              # tags$td(width = "25%", checkboxInput("evidenceSigOnly",label=NULL,value=braw.def$evidence$sigOnly))
                                            ),
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "35%", tags$div(style = localPlainStyle, "show theory:")),
                                              tags$td(width = "5%", checkboxInput("evidenceTheory",label=NULL,value=braw.def$evidence$showTheory)),
                                              tags$td(width = "60%", tags$div(style = localPlainStyle, "")),
                                            )
                                 ),
                        )
                        # help tab
                        ,tabPanel(helpChar,value="?",
                                  style = paste("background: ",subpanelcolours$simulateC),
                                  tags$table(width = "100%",class="myTable",
                                             tags$tr(
                                               tags$div(style = helpStyle, 
                                                        tags$br(HTML('<b>'),"Single simulation:",HTML('</b>')),
                                                        tags$br(HTML('&emsp;'), '1. press "New Sample", nothing else required'),
                                                        tags$br(HTML('&emsp;'), '2. results are found in:'),
                                                        tags$br(HTML('&emsp;'),HTML('&emsp;'), 'Sample: raw data'),
                                                        tags$br(HTML('&emsp;'),HTML('&emsp;'), 'Description: effects'),
                                                        tags$br(HTML('&emsp;'),HTML('&emsp;'), 'Inference: null hypothesis tests'),
                                                        tags$br(HTML('&emsp;'), '3. choose to see:'),
                                                        tags$br(HTML('&emsp;'),HTML('&emsp;'), 'Basic: effect-size & p-value'),
                                                        tags$br(HTML('&emsp;'),HTML('&emsp;'), 'Power: post-hoc power & n80'),
                                                        tags$br(HTML('<b>'),"Multiple simulations: ",HTML('</b>')),
                                                        tags$br(HTML('&emsp;'), paste0('1. choose desired output then press "', startLabel, '"')),
                                                        tags$br(HTML('&emsp;'),HTML('&emsp;'), '(use the append option to add further simulations)'),
                                                        tags$br(HTML('&emsp;'), '2. results are found in:'),
                                                        tags$br(HTML('&emsp;'),HTML('&emsp;'), 'Expected:')
                                               ),
                                             )
                                  )
                        )
            )
  )
