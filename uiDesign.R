source("uiReplication.R")

DesignTab <-
  wellPanel(
    style = paste("background: ",panelcolours$designC), 
    # h5("Design"),
    fluidRow(headerText("Design the sample: size & method")),
    tabsetPanel(id="Design", type="tabs",
                # sampling tab
                tabPanel("Design:",value="Design",
                         style = paste("background: ",subpanelcolours$designC)
                         ),
                tabPanel("Sampling",value="Sampling",
                         style = paste("background: ",subpanelcolours$designC), 
                         wellPanel(
                           style = paste("background: ",subpanelcolours$designC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$td(width = "40%", tags$div(style = localStyle, "Sample Size:")),
                                        tags$td(width = "60%", 
                                                numericInput("sN",label=NULL,value=design$sN)
                                        )
                                      ),
                                      tags$tr(id="DesignMethod",
                                        tags$td(width = "40%", tags$div(style = localStyle, "Method:")),
                                        tags$td(width = "60%", 
                                                selectInput("sMethod",label=NULL,c("Random","Stratified","Cluster","Convenience","Snowball"),
                                                            selected=design$sMethod,
                                                            selectize=FALSE)
                                        )
                                      ),
                                      tags$tr(
                                        tags$td(width = "40%", tags$div(style = localStyle, "Usage (IV):")),
                                        tags$td(width = "60%", 
                                                selectInput("sIV1Use",label=NULL,c("Between","Within"),
                                                            selected=design$sIV1Use,
                                                            selectize=FALSE)
                                        )
                                      ),
                                      tags$tr(id="IV2Design",
                                              tags$td(width = "40%", 
                                                      conditionalPanel(condition="input.IV2choice != 'none'",
                                                                       tags$div(style = localStyle, "Usage (IV2):"))
                                                      ),
                                              tags$td(width = "60%", 
                                                      conditionalPanel(condition="input.IV2choice != 'none'",
                                                                       selectInput("sIV2Use",label=NULL,c("Between","Within"),
                                                                  selected=design$sIV2Use,
                                                                  selectize=FALSE)
                                              )
                                      ))
                           ))
                ),
                tabPanel("Anomalies",
                         style = paste("background: ",subpanelcolours$designC), 
                         wellPanel(
                           style = paste("background: ",subpanelcolours$designC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$td(width = "40%", tags$div(style = localStyle, "Dependence:")),
                                        tags$td(width = "60%", 
                                                numericInput("sDependence",label=NULL,value=design$sDependence,min=0, max=1, step=0.1)
                                        )
                                      ),
                                      tags$tr(
                                        tags$td(width = "40%", tags$div(style = localStyle, "Outliers:")),
                                        tags$td(width = "60%", 
                                                numericInput("sOutliers",label=NULL,value=design$sOutliers,min=0, max=1, step=0.1)
                                        )
                                      ),
                                      tags$tr(
                                        tags$td(width = "40%", tags$div(style = localStyle, "Limited Ranges:")),
                                        tags$td(width = "60%", 
                                                checkboxInput("sRangeOn",label=NULL,value=design$sRangeOn)
                                        )
                                      )
                           )
                         ),
                conditionalPanel(condition  = "input.sRangeOn",
                           fluidRow(
                           column(width=6,offset=0,
                                  sliderInput("sDVRange",
                                              label="DV:",
                                              min = -fullRange,
                                              max = fullRange,
                                              step = 0.1,
                                              value = design$sDVRange
                                  )
                           ),
                           column(width=6,offset=0,
                                  sliderInput("sIVRange",
                                              label="IV:",
                                              min = -fullRange,
                                              max = fullRange,
                                              step = 0.1,
                                              value = design$sIVRange
                                  )
                           )
                         )
                )
                )
                
                # replication tab
                ,replicationTab
                
                # options tab
                ,tabPanel("#",id="DesignOptions",
                          style = paste("background: ",subpanelcolours$designC),
                          wellPanel(
                            style = paste("background: ",subpanelcolours$designC,";"),
                            conditionalPanel(condition="input.sMethod == 'Random'",
                                             tags$table(width = "100%",class="myTable",
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Random:")),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "100%", tags$div(style = localPlainStyle, "no options")),
                                                        ))),
                            conditionalPanel(condition="input.sMethod == 'Stratified'",
                                             tags$table(width = "100%",class="myTable",
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Stratified:")),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "number:")),
                                                          tags$td(width = "50%",numericInput("sN_Strata",label=NULL,value=design$sN_Strata)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "range:")),
                                                          tags$td(width = "50%",numericInput("sR_Strata",label=NULL,value=design$sR_Strata)),
                                                        ))),
                            conditionalPanel(condition="input.sMethod == 'Cluster'",
                                             tags$table(width = "100%",class="myTable",
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Cluster:")),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "clusters number:")),
                                                          tags$td(width = "50%",numericInput("sNClu_Cluster",label=NULL,value=design$sNClu_Cluster)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "cluster range:")),
                                                          tags$td(width = "50%",numericInput("sRClu_Cluster",label=NULL,value=design$sRClu_Cluster)),
                                                        ))),
                            conditionalPanel(condition="input.sMethod == 'Convenience'",
                                             tags$table(width = "100%",class="myTable",
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Convenience:")),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "clusters number:")),
                                                          tags$td(width = "50%",numericInput("sNClu_Convenience",label=NULL,value=design$sNClu_Convenience)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "cluster range:")),
                                                          tags$td(width = "50%",numericInput("sRClu_Convenience",label=NULL,value=design$sRClu_Convenience)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "contacts number:")),
                                                          tags$td(width = "50%",numericInput("sNCont_Convenience",label=NULL,value=design$sNCont_Convenience)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "contact range:")),
                                                          tags$td(width = "50%",numericInput("sRCont_Convenience",label=NULL,value=design$sRCont_Convenience)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "spread range:")),
                                                          tags$td(width = "50%",numericInput("sRSpread_Convenience",label=NULL,value=design$sRSpread_Convenience)),
                                                        ))),
                            conditionalPanel(condition="input.sMethod == 'Snowball'",
                                             tags$table(width = "100%",class="myTable",
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Snowball:")),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "clusters number:")),
                                                          tags$td(width = "50%",numericInput("sNClu_Snowball",label=NULL,value=design$sNClu_Snowball)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "cluster range:")),
                                                          tags$td(width = "50%",numericInput("sRClu_Snowball",label=NULL,value=design$sRClu_Snowball)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "contacts number:")),
                                                          tags$td(width = "50%",numericInput("sNCont_Snowball",label=NULL,value=design$sNCont_Snowball)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "contact range:")),
                                                          tags$td(width = "50%",numericInput("sRCont_Snowball",label=NULL,value=design$sRCont_Snowball)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "spread:")),
                                                          tags$td(width = "50%",numericInput("sRSpread_Snowball",label=NULL,value=design$sRSpread_Snowball)),
                                                        ))),
                          )
                )
                # help tab
                ,tabPanel(helpChar,value="?",
                          style = paste("background: ",subpanelcolours$designC,";"),
                          wellPanel(
                            style = paste("background: ",subpanelcolours$designC,";"),
                            tags$table(width = "100%",class="myTable",
                                       tags$tr(
                                         tags$div(style = helpStyle, 
                                                  tags$br(HTML("<b>"),"Sampling:",HTML("</b>")),
                                                  tags$br(HTML('&emsp;'), '1. choose the sampling method'),
                                                  tags$br(HTML('&emsp;'), '2. set the sample size'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), '(see the Prediction diagram change)'),
                                                  tags$br(HTML('&emsp;'), '3. choose a between/within design'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), '(Categorical IVs only)'),
                                                  tags$br(HTML("<b>"),"Anomalies: ",HTML("</b>")),
                                                  tags$br(HTML('&emsp;'), '1. add in outliers'),
                                                  tags$br(HTML('&emsp;'), '2. sample with non-independence'),
                                                  tags$br(HTML('&emsp;'), '3. apply heteroscedasticity (unequal variance)'),
                                                  tags$br(HTML('&emsp;'), '4. apply limited range to IV or DV'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), 'a. set the range of IV sampling'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), 'b. set the range of DV values retained'),
                                         ),
                                       )
                            )
                          )
                )
    )
  )
