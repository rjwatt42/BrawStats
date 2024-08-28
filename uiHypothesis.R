source("uiEffect.R")
source("uiWorld.R")
source("uiQuickHyp.R")

varTypes<- c("Interval" = "Interval",
             "Ordinal" = "Ordinal",
             "Categorical" = "Categorical"
)
varChoices<- names(braw.env$variables)

HypothesisTab <-
  
  wellPanel(id="HypothesisTabset",
            style = paste("background: ",panelcolours$hypothesisC), 
            # h5("Hypothesis"),
            fluidRow(headerText("Build a hypothesis: variables & effect-size")),
            tabsetPanel(id="Hypothesis",
                        # Hypothesis tab
                        tabPanel("Hypothesis:",value="Hypothesis"
                        ),
                        
                        # variables tab
                        tabPanel("Variables",value="Variables",
                                 style = paste("background: ",subpanelcolours$hypothesisC), 
                                 conditionalPanel(condition="input.Using!='Data'",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "40%", selectInput("Using", label = NULL,
                                                                                                  choices=c("Simulations"="Simulations","Data"="Data"),
                                                                                                  selected="Data",
                                                                                                  selectize=FALSE
                                                               )),
                                                               tags$td(width = "60%", tags$div(style = localStyle, " ")),
                                                             )
                                                  )
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "10%", tags$div(style = localStyle, "IV:")),
                                              tags$td(width = "60%", selectInput("IVchoice", label = NULL,
                                                                                 choices=varChoices,
                                                                                 selected=braw.def$IV$name,
                                                                                 selectize=FALSE
                                              )),
                                              tags$td(width = "20%", tags$div(style = localStyle, "edit")),
                                              tags$td(width = "5%", checkboxInput("editIV","")),
                                              tags$td(width = "5%", tags$div(style = localStyle, " ")),
                                              # tags$td(width = "10%", actionButton("inspectIV","i")),
                                            )),
                                 conditionalPanel(condition="input.editIV",
                                                  tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "30%", tags$div(style = localStyle, "Name:")),
                                              tags$td(width = "70%", textInput("IVname", value = braw.def$IV$name, label = NULL))
                                            ),
                                            tags$tr(
                                              tags$td(width = "30%", div(style = localStyle, "Type:")),
                                              tags$td(width = "70%", 
                                                      selectInput("IVtype", label= NULL,
                                                                  varTypes,selected=braw.def$IV$type,
                                                                  selectize=FALSE
                                                      )
                                              )
                                            ))),
                                 conditionalPanel(condition="input.editIV && input.IVtype == 'Interval'",
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                                    tags$td(width = "30%",  tags$div(style = localStyle, "Mean:")),
                                                    tags$td(width = "20%", numericInput("IVmu", value = braw.def$IV$mu, label = NULL)),
                                                    tags$td(width = "30%",  tags$div(style = localStyle, "Sd:")),
                                                    tags$td(width = "20%", numericInput("IVsd", value = braw.def$IV$sd, label = NULL))
                                            ),
                                            tags$tr(
                                                    tags$td(width = "30%",  tags$div(style = localStyle, "Skew:")),
                                                    tags$td(width = "20%",  numericInput("IVskew", value = braw.def$IV$skew, step=0.1, label = NULL)),
                                                    tags$td(width = "30%",  tags$div(style = localStyle, "Kurtosis:")),
                                                    tags$td(width = "20%",  numericInput("IVkurt", value = braw.def$IV$kurtosis, step=0.1, label = NULL))
                                            ),
                                 ),
                                 ),
                                 conditionalPanel(condition="input.editIV && input.IVtype == 'Ordinal'",
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                                    tags$td(width = "30%",  tags$div(style = localStyle, "No levels:")),
                                                    tags$td(width = "20%", numericInput("IVnlevs", value = braw.def$IV$nlevs, label = NULL,step=1,min=2)),
                                                    tags$td(width = "30%",  tags$div(style = localStyle, "IQR:")),
                                                    tags$td(width = "20%", numericInput("IViqr", value = braw.def$IV$iqr, label = NULL,step=0.5)),
                                            ),
                                 ),
                                 ),
                                 conditionalPanel(condition="input.editIV && input.IVtype == 'Categorical'",
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                                    tags$td(width = "30%",  tags$div(style = localStyle, "No cases:")),
                                                    tags$td(width = "20%", numericInput("IVncats", value = braw.def$IV$ncats, label = NULL,step=1,min=2)),
                                                    tags$td(width = "30%",  tags$div(style = localStyle, "Proportions:")),
                                                    tags$td(width = "20%", textInput("IVprop", value = braw.def$IV$proportions, label = NULL)),
                                                    # tags$td(width = "10%",  tags$div(style = localStyle, " "))
                                            ),
                                 ),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                                    tags$td(width = "30%",  tags$div(style = localStyle, "Cases:")),
                                                    tags$td(width = "60%", textInput("IVcases", value = braw.def$IV$cases, label = NULL)),
                                                    tags$td(width = "10%",  tags$div(style = localStyle, " "))
                                            ),
                                 )
                                 ),
                                 conditionalPanel(condition="input.editIV",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "90%", tags$div(style = localStyle, "")),
                                                               tags$td(width = "10%", actionButton("inspectIV","i")),
                                                             )
                                                  )),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "10%", tags$div(style = localStyle, "IV2:")),
                                              tags$td(width = "60%", selectInput("IV2choice", label = NULL,
                                                                                 choices=c("none",varChoices),
                                                                                 selected=braw.def$IV2$name,
                                                                                 selectize=FALSE
                                              )),
                                              conditionalPanel(condition = "input.IV2choice != 'none'",
                                                                            tags$td(width = "20%", tags$div(id="editIV2T",style = localStyle, "edit")),
                                                               tags$td(width = "5%", checkboxInput("editIV2","")),
                                                               tags$td(width = "5%", tags$div(style = localStyle, " ")),
                                              )
                                            ),
                                 ),
                                 conditionalPanel(condition="input.editIV2",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "30%", tags$div(style = localStyle, "Name:")),
                                                               tags$td(width = "70%", textInput("IV2name", value = braw.def$IV2$name, label = NULL))
                                                             ),
                                                             tags$tr(
                                                               tags$td(width = "30%", div(style = localStyle, "Type:")),
                                                               tags$td(width = "70%", 
                                                                       selectInput("IV2type", label= NULL,
                                                                                   varTypes,selected=braw.def$IV2$type,
                                                                                   selectize=FALSE
                                                                       )
                                                               )
                                                             ))),
                                 conditionalPanel(condition="input.editIV2 && input.IV2type == 'Interval'",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "30%",  tags$div(style = localStyle, "Mean:")),
                                                               tags$td(width = "20%", numericInput("IV2mu", value = braw.def$IV2$mu, label = NULL)),
                                                               tags$td(width = "30%",  tags$div(style = localStyle, "Sd:")),
                                                               tags$td(width = "20%", numericInput("IV2sd", value = braw.def$IV2$sd, label = NULL))
                                                             ),
                                                             tags$tr(
                                                               tags$td(width = "30%",  tags$div(style = localStyle, "Skew:")),
                                                               tags$td(width = "20%",  numericInput("IV2skew", value = braw.def$IV2$skew, step=0.1, label = NULL)),
                                                               tags$td(width = "30%",  tags$div(style = localStyle, "Kurtosis:")),
                                                               tags$td(width = "20%",  numericInput("IV2kurt", value = braw.def$IV2$kurtosis, step=0.1, label = NULL))
                                                             ),
                                                  ),
                                 ),
                                 conditionalPanel(condition="input.editIV2 && input.IV2type == 'Ordinal'",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "30%",  tags$div(style = localStyle, "No levels:")),
                                                               tags$td(width = "20%", numericInput("IV2nlevs", value = braw.def$IV2$nlevs, label = NULL,step=1,min=2)),
                                                               tags$td(width = "30%",  tags$div(style = localStyle, "IQR:")),
                                                               tags$td(width = "20%", numericInput("IV2iqr", value = braw.def$IV2$iqr, label = NULL,step=0.5)),
                                                             ),
                                                  ),
                                 ),
                                 conditionalPanel(condition="input.editIV2 && input.IV2type == 'Categorical'",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "30%",  tags$div(style = localStyle, "No cases:")),
                                                               tags$td(width = "20%", numericInput("IV2ncats", value = braw.def$IV2$ncats, label = NULL,step=1,min=2)),
                                                               tags$td(width = "30%",  tags$div(style = localStyle, "Proportions:")),
                                                               tags$td(width = "20%", textInput("IV2prop", value = braw.def$IV2$proportions, label = NULL)),
                                                               # tags$td(width = "10%",  tags$div(style = localStyle, " "))
                                                             ),
                                                  ),
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "30%",  tags$div(style = localStyle, "Cases:")),
                                                               tags$td(width = "60%", textInput("IV2cases", value = braw.def$IV2$cases, label = NULL)),
                                                               tags$td(width = "10%",  tags$div(style = localStyle, " "))
                                                             ),
                                                  )
                                 ),
                                 conditionalPanel(condition="input.editIV2",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "90%", tags$div(style = localStyle, "")),
                                                               tags$td(width = "10%", actionButton("inspectIV2","i")),
                                                             )
                                 )),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$td(width = "10%", tags$div(style = localStyle, "DV:")),
                                              tags$td(width = "60%", selectInput("DVchoice", label = NULL,
                                                                                 choices=varChoices,
                                                                                 selected=braw.def$DV$name,
                                                                                 selectize=FALSE
                                              )),
                                              tags$td(width = "20%", tags$div(style = localStyle, "edit")),
                                              tags$td(width = "5%", checkboxInput("editDV","")),
                                              tags$td(width = "5%", tags$div(style = localStyle, " ")),
                                            ),
                                 ),
                                 conditionalPanel(condition="input.editDV",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "30%", tags$div(style = localStyle, "Name:")),
                                                               tags$td(width = "70%", textInput("DVname", value = braw.def$DV$name, label = NULL))
                                                             ),
                                                             tags$tr(
                                                               tags$td(width = "30%", div(style = localStyle, "Type:")),
                                                               tags$td(width = "70%", 
                                                                       selectInput("DVtype", label= NULL,
                                                                                   varTypes,selected=braw.def$DV$type,
                                                                                   selectize=FALSE
                                                                       )
                                                               )
                                                             ))),
                                 conditionalPanel(condition="input.editDV && input.DVtype == 'Interval'",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                                     tags$td(width = "30%",  tags$div(style = localStyle, "Mean:")),
                                                                     tags$td(width = "20%", numericInput("DVmu", value = braw.def$DV$mu, label = NULL)),
                                                                     tags$td(width = "30%",  tags$div(style = localStyle, "Sd:")),
                                                                     tags$td(width = "20%", numericInput("DVsd", value = braw.def$DV$sd, label = NULL))
                                                             ),
                                                             tags$tr(
                                                                     tags$td(width = "30%",  tags$div(style = localStyle, "Skew:")),
                                                                     tags$td(width = "20%",  numericInput("DVskew", value = braw.def$DV$skew, step=0.1, label = NULL)),
                                                                     tags$td(width = "30%",  tags$div(style = localStyle, "Kurtosis:")),
                                                                     tags$td(width = "20%",  numericInput("DVkurt", value = braw.def$DV$kurtosis, step=0.1, label = NULL))
                                                             ),
                                                  ),
                                 ),
                                 conditionalPanel(condition="input.editDV && input.DVtype == 'Ordinal'",
                                                  tags$table(id="IVOrdVal",width = "100%",class="myTable",
                                                             tags$tr(
                                                                     tags$td(width = "30%",  tags$div(style = localStyle, "No levels:")),
                                                                     tags$td(width = "20%", numericInput("DVnlevs", value = braw.def$DV$nlevs, label = NULL,step=1,min=2)),
                                                                     tags$td(width = "30%",  tags$div(style = localStyle, "IQR:")),
                                                                     tags$td(width = "20%", numericInput("DViqr", value = braw.def$DV$iqr, label = NULL,step=0.5))
                                                             ),
                                                  ),
                                 ),
                                 conditionalPanel(condition="input.editDV && input.DVtype == 'Categorical'",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                                     tags$td(width = "30%",  tags$div(style = localStyle, "No cases:")),
                                                                     tags$td(width = "20%", numericInput("DVncats", value = braw.def$DV$ncats, label = NULL,step=1,min=2)),
                                                                     tags$td(width = "30%",  tags$div(style = localStyle, "Proportions:")),
                                                                     tags$td(width = "20%", textInput("DVprop", value = braw.def$DV$proportions, label = NULL)),
                                                                     # tags$td(width = "10%",  tags$div(style = localStyle, " "))
                                                             ),
                                                  ),
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                                     tags$td(width = "30%",  tags$div(style = localStyle, "Cases:")),
                                                                     tags$td(width = "60%", textInput("DVcases", value = braw.def$DV$cases, label = NULL)),
                                                                     tags$td(width = "10%",  tags$div(style = localStyle, " "))
                                                             ),
                                                  )
                                 ),
                                 conditionalPanel(condition="input.editDV",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "90%", tags$div(style = localStyle, "")),
                                                               tags$td(width = "10%", actionButton("inspectDV","i")),
                                                             )
                                                  )),
                        ),
                        
                        # prediction tab
                        tabPanel("Effects",id="Effects",
                                 style = paste("background: ",subpanelcolours$hypothesisC), 
                                 effectPanel(""),
                        ),
                        
                        # world tab
                        worldPanel(),
                        
                        # options tab
                        tabPanel("#",
                                 style = paste("background: ",subpanelcolours$hypothesisC),
                                 # conditionalPanel(condition="input.Using!='Data'",
                                                  tags$table(width = "100%",class="myTable",
                                                             tags$tr(
                                                               tags$td(width = "45%", tags$div(style = localPlainStyle, "Heteroscedasticity:")),
                                                               tags$td(width = "30%", 
                                                                       numericInput("Heteroscedasticity",label=NULL,value=braw.def$effect$Heteroscedasticity,min=-2, max=2, step=0.1),
                                                               ),
                                                               tags$td(width = "25%")
                                                             ),
                                                             tags$tr(
                                                               tags$td(width = "45%", tags$div(style = localPlainStyle, "Residuals:")),
                                                               tags$td(width = "30%", 
                                                                       selectInput("ResidDistr",label=NULL,
                                                                                   choices=list("normal"="normal","skewed","uniform"="uniform","Cauchy"="Cauchy","t(3)"="t(3)"),selected=braw.def$effect$ResidDistr,selectize=FALSE),
                                                               ),
                                                               tags$td(width = "25%")
                                                             )
                                                  )
                                 # )
                                 ,quickHypotheses
                        ),
                        # help tab
                        tabPanel(helpChar,value="?",
                                 style = paste("background: ",subpanelcolours$hypothesisC),
                                 tags$table(width = "100%",class="myTable",
                                            tags$tr(
                                              tags$div(style = helpStyle, 
                                                       tags$br(HTML("<b>"),"Variables:",HTML("</b>")),
                                                       tags$br(HTML('&emsp;'), '1. choose one or two IVs and a DV by name'),
                                                       tags$br(HTML('&emsp;'), '2. edit the variable name/type/details if needed'),
                                                       tags$br(HTML('&emsp;'),HTML('&emsp;'), 'eg. mean, sd , skew, kurtosis'),
                                                       tags$br(HTML('&emsp;'),HTML('&emsp;'), 'or no cases, case names, proportions'),
                                                       tags$br(HTML('&emsp;'),HTML('&emsp;'), '(watch the Hypothesis diagram)'),
                                                       tags$br(HTML("<b>"),"Effects: ",HTML("</b>")),
                                                       tags$br(HTML('&emsp;'), '3. select effect size or sizes (for 2 IVs)'),
                                                       tags$br(HTML('&emsp;'),HTML('&emsp;'), 'these are normalized and range from -1 to +1'),
                                                       tags$br(HTML('&emsp;'),HTML('&emsp;'), '(watch the Population or Prediction diagram)')
                                              ),
                                            )
                                 )
                        )
            )
  )
