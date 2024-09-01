
if (switches$doWorlds) {
  source2<-c("null","world","prior")
  source1<-c("none","world","prior")
} else {
  source2<-c("null","hypothesis","prior")
  source1<-c("none","prior")
}

AnalysisTab <-

  wellPanel(ID="MainAnalysis",
    style = paste("background: ",panelcolours$analysisC), 
    # h5("Evidence"),
  fluidRow(headerText("Analysis options")),
  tabsetPanel(type="tabs",id="AnalysisPanel",
              # single tab
              tabPanel("Analysis:"
              ),
              tabPanel("Describe",
                       style = paste("background: ",subpanelcolours$analysisC), 
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "50%", tags$div(style = labelStyle, "Effect size type:")),
                                      tags$td(width = "40%", 
                                                               selectInput("EvidenceEffect_type",label=NULL,
                                                                           c("direct" = "direct",
                                                                             "unique" = "unique",
                                                                             "total" = "total",
                                                                             "all" = "all"
                                                                             ),
                                                                           selected="all",
                                                                           selectize=FALSE)
                                              ),
                                      tags$td(width = "10%", tags$div(style = localPlainStyle, "")),
                                    ),
                                    tags$tr(
                                      tags$td(width="50%",tags$div(style = labelStyle, "Display as:")),
                                      tags$td(width="40%",selectInput("RZ",label=NULL, c("r"="r","z"="z"), selected=braw.env$RZ, selectize=FALSE)),
                                      tags$td(width = "10%", tags$div(style = localPlainStyle, "")),
                                    )
                         )),
              tabPanel("Infer",
                       style = paste("background: ",subpanelcolours$analysisC), 
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "10%",tags$div(style = localStyle, "Use:")),
                                    tags$td(width = "5%",tags$div(style = localStyle, " ")),
                                    tags$td(width="35%", 
                                            selectInput("STMethod",label=NULL,
                                                        choices=c("NHST","sLLR","dLLR"),
                                                        selected="NHST",
                                                        selectize=FALSE
                                            )
                                    ),
                                    tags$td(width = "15%",tags$div(style = localPlainStyle, paste0(braw.env$alphaChar,":"))),
                                    tags$td(width = "35%",
                                            numericInput("alpha",label=NULL,value=braw.env$alphaSig,step=0.01)
                                    ),
                                  )
                                  ),
                       conditionalPanel(condition="input.IV2choice != 'none'",
                                        tags$table(width = "100%",class="myTable",
                                                   tags$tr(
                                                     tags$td(width = "30%", tags$div(style = localPlainStyle, "Interaction:")),
                                                     tags$td(width = "25%", tags$div(style = localPlainStyle, "analyse")),
                                                     tags$td(width = "10%",checkboxInput("rInteractionOn",label=NULL,value=braw.def$evidence$rInteractionOn))
                                                   )
                                        )
                       ),
              ),
              tabPanel("Likelihood",
                       style = paste("background: ",subpanelcolours$analysisC), 
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "25%", tags$div(style = paste(localStyle,"text-align: left"), "Analysis:")),
                                    tags$td(width = "22%",tags$div(style = localPlainStyle, "prior:")),
                                    tags$td(width = "25%",selectInput("likelihoodUsePrior",label=NULL,
                                                                      choices=source1,selected=braw.def$possible$UsePrior,
                                                                      selectize=FALSE)
                                    ),
                                    tags$td(width = "28%")
                                  )
                       ),
                       conditionalPanel(condition="input.likelihoodUsePrior=='prior'",
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "30%", tags$div(style = localStyle, "pdf:")),
                                    tags$td(width = "30%",
                                            selectInput("Prior_distr", label=NULL,
                                                        c("Single" = "Single",
                                                          "Uniform" = "Uniform",
                                                          "Gauss"="Gauss",
                                                          "Exp" = "Exp",
                                                          ">"=">",
                                                          "<"="<"),
                                                        selected="Uniform",
                                                        width="100%",selectize=FALSE)
                                    ),
                                    tags$td(width = "15%",
                                            selectInput("Prior_distr_rz", label=NULL,
                                                        c("r" = "r",
                                                          "z" = "z"),
                                                        selected="r",
                                                        width="100%",selectize=FALSE)
                                    ),
                                    tags$td(width = "15%",
                                            conditionalPanel(condition="input.Prior_distr!=='Uniform'",
                                                             numericInput( "Prior_distr_k",label=NULL,
                                                                          min = 0,
                                                                          max = 1,
                                                                          step = 0.05,
                                                                          value = 0.2)
                                            )
                                    ),
                                    tags$td(width="10%")
                                  ),
                                  tags$tr(
                                    tags$td(width = "30%", tags$div(style = localStyle, braw.env$pPlusLabel)),
                                    tags$td(width = "30%", numericInput("Prior_Nullp", label=NULL,min=0,max=1, step=0.025,value=0.5)),
                                    tags$td(width = "15%"),
                                    tags$td(width = "15%"),
                                    tags$td(width="10%")
                                  )
                       )
                       ),
              ),
              tabPanel("#",
                       style = paste("background: ",subpanelcolours$analysisC), 
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "20%", tags$div(style = paste(localStyle,"text-align: left"), "Describe:")),
                                  )
                       ),
                       conditionalPanel(condition="input.IV2choice != 'none'",
                                        tags$table(width = "100%",class="myTable",
                                                   tags$tr(
                                                     tags$td(width = "30%", tags$div(style = localPlainStyle, "SSQ Type:")),
                                                     tags$td(width = "25%", selectInput("ssqType", label=NULL, c("Type1"="Type1","Type2"="Type2","Type3"="Type3"), selected=braw.def$evidence$ssqType, selectize=FALSE)),
                                                     tags$td(width = "45%", tags$div(style = localPlainStyle, " ")),
                                                   ),
                                                   tags$tr(
                                                     tags$td(width = "30%", tags$div(style = localPlainStyle, "Report:")),
                                                     tags$td(width = "25%", selectInput("dataType", label=NULL, c("Raw"="Raw","Norm"="Norm","RawC"="RawC","NormC"="NormC"), selected=braw.def$evidence$dataType, selectize=FALSE)),
                                                     tags$td(width = "25%", selectInput("analysisType", label=NULL, c("Anova"="Anova","Model"="Model"), selected=braw.def$evidence$analysisType, selectize=FALSE)),
                                                     tags$td(width = "20%", tags$div(style = localPlainStyle, " ")),
                                                   )
                                        )
                       ),
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "20%", tags$div(style = paste(localStyle,"text-align: left"), "Infer:")),
                                  )
                       ),
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "40%", tags$div(style = localPlainStyle, "Eq variance?")),
                                    tags$td(width = "5%", 
                                            checkboxInput("Welch",label=NULL,value=!braw.def$evidence$Welch),
                                    ),
                                    tags$td(width = "20%", tags$div(style = localPlainStyle, "Transform")),
                                    tags$td(width = "35%", 
                                            selectInput("Transform",label=NULL,
                                                        choices=c("None","Log","Exp"),
                                                        selected=braw.def$evidence$Transform,
                                                        selectize=FALSE
                                            ),
                                    )
                                  )
                       ),
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "20%", tags$div(style = paste(localStyle,"text-align: left"), "Likelihood:")),
                                  )
                       ),
                       conditionalPanel(condition="input.LikelihoodDisplay=='samples' && input.possibleView=='3D'",
                                        tags$table(width = "100%",class="myTable",
                                                   tags$tr(
                                                     tags$td(width = "25%",tags$div(style = localPlainStyle, "cutaway")),
                                                     tags$td(width = "35%", checkboxInput("possible_cutaway",label=NULL,value=FALSE)),
                                                   )
                                        )
                       ),
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "15%"),
                                    # tags$td(width = "25%", tags$div(style = paste(localStyle,"text-align: left"), "Display:")),
                                    # tags$td(width = "22%", tags$div(style = localPlainStyle, "possible")),
                                    # tags$td(width = "33%",
                                    #         selectInput("LikelihoodDisplay", label=NULL,
                                    #                     c("Samples" = "Samples",
                                    #                       "Populations" = "Populations"),
                                    #                     selected="Populations",
                                    #                     width="100%",selectize=FALSE)),
                                    tags$td(width = "20%", 
                                            selectInput("possibleView", label=NULL,
                                                        c("3D" = "3D",
                                                          "2D" = "2D"),selected="3D",selectize=FALSE)
                                    ),
                                    tags$td(width = "65%"),
                                  )
                       ),
                       conditionalPanel(condition="input.possibleView=='3D'",
                                        tags$table(width = "100%",class="myTable",
                                                   tags$tr(
                                                     tags$td(width = "15%", tags$div(style = localPlainStyle, "az:")),
                                                     tags$td(width = "20%", 
                                                             numericInput("possibleAzimuth",label=NULL,
                                                                          min = -180,
                                                                          max = 180,
                                                                          step = 5,
                                                                          value = 60)
                                                     ),
                                                     tags$td(width = "15%", tags$div(style = localPlainStyle, "elev:")),
                                                     tags$td(width = "20%", 
                                                             numericInput("possibleElevation",label=NULL,
                                                                          min = 0,
                                                                          max = 90,
                                                                          step = 5,
                                                                          value = 5)
                                                     ),
                                                     tags$td(width = "15%", tags$div(style = localPlainStyle, "range:")),
                                                     tags$td(width = "15%", 
                                                             numericInput("possibleRange",label=NULL,
                                                                          min = 0,
                                                                          max = 100,
                                                                          step = 1,
                                                                          value = 2)
                                                     ),
                                                   )
                                        ),
                       )
              )
              # help tab
              ,tabPanel("?",
                        style = paste("background: ",subpanelcolours$analysisC),
                          tags$table(width = "100%",class="myTable",
                                     tags$tr(
                                       tags$div(style = helpStyle, 
                                                tags$br(HTML('<b>'),"Samples:",HTML('</b>')),
                                                tags$br("Visualize the samples produced by a given population"),
                                                tags$br(" "),
                                                tags$br(HTML('<b>'),"Populations:",HTML('</b>')),
                                                tags$br("Visualize the populations that produce a given sample"),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), '(slower process)'),
                                                tags$br(" "),
                                                tags$br(HTML('<b>'),"Steps:",HTML('</b>')),
                                                tags$br(HTML('&emsp;'), '1. choose the distribution of populations'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'uniform - the common assumption'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'),HTML('&emsp;'), '(extremely unlikely in practice)'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'exponential - much more likely'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'),HTML('&emsp;'), '(high effect sizes are rare)'),
                                                tags$br(HTML('&emsp;'), '2. choose whether to see theoretical distributions'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'),HTML('&emsp;'), '(these are idealized)'),
                                                tags$br(HTML('&emsp;'), paste0('3. press "',startLabel,'"')),
                                       ),
                                     )
                          ),

                                      )
  )
)
