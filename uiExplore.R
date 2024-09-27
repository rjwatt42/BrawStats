
hypothesisChoicesV3=list("Effects"=list("Effect Size1" = "rIV",
                                                                "Effect Size2" = "rIV2",
                                                                "Interaction" = "rIVIV2DV",
                                                                "Covariation" = "rIVIV2",
                                                                "Heteroscedasticity" = "Heteroscedasticity"
                                          ),
                          "Variables"=list("IVType" = "IVType",
                                          "IVskew" = "IVskew",
                                          "IVkurtosis" = "IVkurtosis",
                                          "IVlevels" = "IVlevels",
                                          "IVcats" = "IVcats",
                                          "IVprops" = "IVprops",
                                          
                                          "DVType" = "DVType",
                                          "DVskew" = "DVskew",
                                          "DVkurtosis" = "DVkurtosis",
                                          "DVlevels" = "DVlevels",
                                          "DVcats" = "DVcats",
                                          "DVprops" = "DVprops"
                                        )
)

hypothesisChoicesV2=list("Effects"=list("Effect Size" = "rIV",
                                        "Heteroscedasticity" = "Heteroscedasticity"
                                        ),
                         "Variables"=list("IVType" = "IVType",
                                          "IVskew" = "IVskew",
                                          "IVkurtosis" = "IVkurtosis",
                                          "IVlevels" = "IVlevels",
                                          "IVcats" = "IVcats",
                                          "IVprops" = "IVprops",
                                          
                                          "DVType" = "DVType",
                                          "DVskew" = "DVskew",
                                          "DVkurtosis" = "DVkurtosis",
                                          "DVlevels" = "DVlevels",
                                          "DVcats" = "DVcats",
                                          "DVprops" = "DVprops"
                         )
)

hypothesisChoicesV2Extra=c(hypothesisChoicesV2,list("Worlds"=list("worldLambda"="lambda","worldPNull"="pNull")
))
hypothesisChoicesV3Extra=c(hypothesisChoicesV3,list("Worlds"=list("worldLambda"="lambda","worldPNull"="pNull")
))

designChoices=list("Sampling"=list("Sample Size" = "n",
                                   "Method" = "Method",
                                   "Usage" = "Usage"),
                   "Anomalies"=list("Dependence" = "Dependence",
                                    "Outliers" = "Outliers",
                                    "IV RangeC" = "IVRangeC",
                                    "IV RangeE" = "IVRangeE")
)

designChoicesExtra=list("Sampling"=list("Sample Size" = "n",
                                   "Method" = "Method",
                                   "Usage" = "Usage",
                                   "Gamma" = "SampleGamma"
                                   ),
                   "Anomalies"=list("Dependence" = "Dependence",
                                    "Outliers" = "Outliers",
                                    "IV RangeC" = "IVRangeC",
                                    "IV RangeE" = "IVRangeE"),
                   "Cheating"=list("CheatMethod" = "Cheating",
                                   "CheatAmount" = "CheatingAmount"),
                   "Replications"=list("SigOnly"="SigOnly",
                                       "Repl Power"="Power",
                                       "Repl Repeats" = "Repeats")
)

analysisChoices=list( "Alpha" = "Alpha",
                      "Transform" = "Transform",
                      "EqualVar" = "EqualVar",
                      "InteractionOn" = "InteractionOn"
)

# names(designChoicesExtra$Sampling)[5]<-braw.env$alphaChar

exploreShowChoices<-list("Basic" = "Basic","Custom"="Custom","p(sig)"="p(sig)","Power" = "Power","NHST" = "NHST","Hits"="Hits","Misses"="Misses")

exploreShowParams<-list("rs"="rs","p"="p"," "=" ",
                        "n"="n","rp"="rp","wp"="wp"," "=" ",
                        "ws"="ws","nw"="nw"
)

exploreShowParamsExtra<-list("rs"="rs","p"="p",
                             " "=" ","n"="n","rp"="rp","wp"="wp",
                            " "=" ","ws"="ws","nw"="nw",
                            " "=" ","ro"="ro","po"="po","no"="no"
)

whichEffectShow=c("Main 1" = "Main 1",
                  "Main 2" = "Main 2",
                  "Interaction" = "Interaction",
                  "Mains" = "Mains",
                  "All" = "All")

ExploreTab <-
    wellPanel(id="uiExplore",
            style = paste("background: ",panelcolours$exploreC), 
            fluidRow(headerText("Explore design decisions")),
            tabsetPanel(type="tabs",id="Explore",
                        # sampling tab
                        tabPanel("Explore:",value="Explore"
                        ),
                        tabPanel("Hypothesis",id="ExH",
                                 style = paste("background: ",subpanelcolours$exploreC), 
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "10%", tags$div(style = localStyle, "explore:")),
                                              tags$td(width = "50%", 
                                                      selectInput("Explore_typeH",label=NULL,
                                                                  hypothesisChoicesV2,selectize=FALSE)
                                              ),
                                              tags$td(width = "40%", tags$div(style = localStyle, " "))
                                            )
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "20%",tags$div(style = localPlainStyle, "min:")),
                                              tags$td(width = "20%",numericInput("Explore_minValH", label=NULL,value=10)
                                              ),
                                              tags$td(width = "10%",tags$div(style = localPlainStyle, "max:")),
                                              tags$td(width = "20%",numericInput("Explore_maxValH", label=NULL,value=250)
                                              ),
                                              tags$td(width = "5%",tags$div(style = localPlainStyle, "n:")),
                                              tags$td(width = "15%",numericInput("Explore_NPointsH", label=NULL,value=13)
                                              ),
                                              tags$td(width = "5%",tags$div(style = localPlainStyle, "log")),
                                              tags$td(width = "5%",checkboxInput("Explore_xlogH",label="",value=FALSE)
                                              )
                                            )
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "10%", tags$div(style = localStyle, "show:")),
                                              tags$td(width = "35%", 
                                                      selectInput("Explore_showH",label=NULL,
                                                                  exploreShowChoices,
                                                                  selected="Basic",
                                                                  selectize=FALSE)
                                              ),
                                              tags$td(width = "15%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.Explore_showH=='Custom'",
                                                                       selectInput("Explore_par1H", label=NULL, 
                                                                                   exploreShowParams,
                                                                                   selected="rs", selectize=FALSE)
                                                      ),
                                              ),
                                              tags$td(width = "15%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.Explore_showH=='Custom'",
                                                                       selectInput("Explore_par2H", label=NULL, 
                                                                                   exploreShowParams,
                                                                                   selected="p", selectize=FALSE)
                                                      ),
                                              ),
                                              tags$td(width = "25%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.Explore_showH=='Basic' | input.Explore_showH=='Custom' | input.Explore_showH=='Power'",
                                                                       selectInput("Explore_dimH", label=NULL, 
                                                                                   choices=c("1D"="1D","2D"="2D"),
                                                                                   selected="1D", selectize=FALSE)
                                                      ),
                                              ),
                                            ),
                                            tags$tr(
                                              tags$td(width = "10%",style = localPlainStyle),
                                              tags$td(width = "35%", 
                                                      conditionalPanel(condition="input.IV2choice != 'none'",
                                                                       selectInput("whichEffectH", label=NULL,
                                                                                   whichEffectShow, selected="Mains",selectize = FALSE)
                                                      )),
                                            )
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "20%", actionButton("exploreRunH", startLabel)),
                                              tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                              tags$td(width = "55%", 
                                                      numericInput("Explore_lengthH", label=NULL,
                                                                   value=10)
                                              ),
                                            )
                                 )
                        ),
                        tabPanel("Design",id="ExD",
                                 style = paste("background: ",subpanelcolours$exploreC), 
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "10%", tags$div(style = localStyle, "explore:")),
                                              tags$td(width = "50%", 
                                                      selectInput("Explore_typeD",label=NULL,
                                                                  designChoicesExtra,selectize=FALSE)
                                              ),
                                              tags$td(width = "40%", tags$div(style = localStyle, " "))
                                            )
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "20%",tags$div(style = localPlainStyle, "min:")),
                                              tags$td(width = "20%",numericInput("Explore_minValD", label=NULL,value=10)
                                              ),
                                              tags$td(width = "10%",tags$div(style = localPlainStyle, "max:")),
                                              tags$td(width = "20%",numericInput("Explore_maxValD", label=NULL,value=250)
                                              ),
                                              tags$td(width = "5%",tags$div(style = localPlainStyle, "n:")),
                                              tags$td(width = "15%",numericInput("Explore_NPointsD", label=NULL,value=13)
                                              ),
                                              tags$td(width = "5%",tags$div(style = localPlainStyle, "log")),
                                              tags$td(width = "5%",checkboxInput("Explore_xlogD",label="",value=FALSE)
                                              )
                                            )
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "10%", tags$div(style = localStyle, "show:")),
                                              tags$td(width = "35%", 
                                                      selectInput("Explore_showD",label=NULL,
                                                                  exploreShowChoices,
                                                                  selected="Basic",
                                                                  selectize=FALSE)
                                              ),
                                              tags$td(width = "15%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.Explore_showD=='Custom'",
                                                                       selectInput("Explore_par1D", label=NULL, 
                                                                                   exploreShowParams,
                                                                                   selected="rs", selectize=FALSE)
                                                      ),
                                              ),
                                              tags$td(width = "15%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.Explore_showD=='Custom'",
                                                                       selectInput("Explore_par2D", label=NULL, 
                                                                                   exploreShowParams,
                                                                                   selected="p", selectize=FALSE)
                                                      ),
                                              ),
                                              tags$td(width = "25%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.Explore_showD=='Basic' | input.Explore_showD=='Custom' | input.Explore_showD=='Power'",
                                                                       selectInput("Explore_dimD", label=NULL, 
                                                                                   choices=c("1D"="1D","2D"="2D"),
                                                                                   selected="1D", selectize=FALSE)
                                                      ),
                                              ),
                                            ),
                                            tags$tr(
                                              tags$td(width = "10%",style = localPlainStyle),
                                              tags$td(width = "35%", 
                                                      conditionalPanel(condition="input.IV2choice != 'none'",
                                                                       selectInput("whichEffectD", label=NULL,
                                                                                   whichEffectShow, selected="Mains",selectize = FALSE)
                                                      )),
                                            )),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "20%", actionButton("exploreRunD", startLabel)),
                                              tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                              tags$td(width = "55%", 
                                                      numericInput("Explore_lengthD", label=NULL,
                                                                   value=10)
                                              ),
                                            )
                                 )
                        ),
                        tabPanel("Analysis",id="ExA",
                                 style = paste("background: ",subpanelcolours$exploreC), 
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "10%", tags$div(style = localStyle, "explore:")),
                                              tags$td(width = "50%", 
                                                      selectInput("Explore_typeA",label=NULL,
                                                                  analysisChoices,selectize=FALSE)
                                              ),
                                              tags$td(width = "40%", tags$div(style = localStyle, " "))
                                            )
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "20%",tags$div(style = localPlainStyle, "min:")),
                                              tags$td(width = "20%",numericInput("Explore_minValA", label=NULL,value=10)
                                              ),
                                              tags$td(width = "10%",tags$div(style = localPlainStyle, "max:")),
                                              tags$td(width = "20%",numericInput("Explore_maxValA", label=NULL,value=250)
                                              ),
                                              tags$td(width = "5%",tags$div(style = localPlainStyle, "n:")),
                                              tags$td(width = "15%",numericInput("Explore_NPointsA", label=NULL,value=13)
                                              ),
                                              tags$td(width = "5%",tags$div(style = localPlainStyle, "log")),
                                              tags$td(width = "5%",checkboxInput("Explore_xlogA",label="",value=FALSE)
                                              )
                                            )
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "10%", tags$div(style = localStyle, "show:")),
                                              tags$td(width = "35%", 
                                                      selectInput("Explore_showA",label=NULL,
                                                                  exploreShowChoices,
                                                                  selected="Basic",
                                                                  selectize=FALSE)
                                              ),
                                              tags$td(width = "15%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.Explore_showA=='Custom'",
                                                                       selectInput("Explore_par1A", label=NULL, 
                                                                                   exploreShowParams,
                                                                                   selected="rs", selectize=FALSE)
                                                      ),
                                              ),
                                              tags$td(width = "15%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.Explore_showA=='Custom'",
                                                                       selectInput("Explore_par2A", label=NULL, 
                                                                                   exploreShowParams,
                                                                                   selected="p", selectize=FALSE)
                                                      ),
                                              ),
                                              tags$td(width = "25%",style = localPlainStyle,
                                                      conditionalPanel(condition="input.Explore_showA=='Basic' | input.Explore_showA=='Custom' | input.Explore_showA=='Power'",
                                                                       selectInput("Explore_dimA", label=NULL, 
                                                                                   choices=c("1D"="1D","2D"="2D"),
                                                                                   selected="1D", selectize=FALSE)
                                                      ),
                                              ),
                                            ),
                                            tags$tr(
                                              tags$td(width = "10%",style = localPlainStyle),
                                              tags$td(width = "35%", 
                                                      conditionalPanel(condition="input.IV2choice != 'none'",
                                                                       selectInput("whichEffectA", label=NULL,
                                                                                   whichEffectShow, selected="Mains",selectize = FALSE)
                                                      )),
                                            )),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "20%", actionButton("exploreRunA", startLabel)),
                                              tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                              tags$td(width = "55%", 
                                                      numericInput("Explore_lengthA", label=NULL,
                                                                   value=10)
                                              ),
                                            )
                                 )
                        ),
                        tabPanel("#",
                                 style = paste("background: ",subpanelcolours$exploreC), 
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "25%", tags$div(style = paste(localStyle,"text-align: left"), "Display")),
                                                tags$td(width = "15%"),
                                                tags$td(width = "30%"),
                                                tags$td(width = "25%"),
                                                tags$td(width = "5%")
                                              ),
                                              tags$tr(
                                                tags$td(width = "25%", tags$div(style = localPlainStyle, "quantiles:")),
                                                tags$td(width = "15%", 
                                                        numericInput("Explore_quants", label=NULL,value=0.5, step = 0.01,min=0.01,max=0.99)
                                                ),
                                                tags$td(width = "60%")
                                              ),
                                   )
                        )
                        # help tab
                        ,tabPanel(helpChar,value="?",
                                  style = paste("background: ",subpanelcolours$exploreC),
                                    tags$table(width = "100%",class="myTable",
                                               tags$tr(
                                                 tags$div(style = helpStyle, 
                                                          tags$br(HTML('<b>'),"Before starting:",HTML('</b>')),
                                                          tags$br(HTML('&emsp;'), '1. set up a basic hypothesis with other panels'),
                                                          tags$br(HTML('<b>'),"Set up:",HTML('</b>')),
                                                          tags$br(HTML('&emsp;'), '2. choose the decision to explore'),
                                                          tags$br(HTML('&emsp;'),HTML('&emsp;'), '(these are split into separate groups)'),
                                                          tags$br(HTML('&emsp;'), '3. choose the outcome to examine'),
                                                          tags$br(HTML('<b>'),"Run: ",HTML('</b>')),
                                                          tags$br(HTML('&emsp;'), paste0('4. press the "',startLabel,'" button')),
                                                          tags$br(HTML('&emsp;'),HTML('&emsp;'), '(can be slow - it is working hard!)'),
                                                 ),
                                               )
                                    )
                                  )

            )
                                                      
)
