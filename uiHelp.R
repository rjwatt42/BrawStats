HelpTab <-
  

wellPanel(id="HelpTabset",
  style = paste("background: ",maincolours$graphC,";","margin-left:0px"),
  # tags$table(width = "100%",class="myTable",
  #            tags$tr(
  #              tags$td(width = "5%",  tags$div(style = localStyle, "?")),
  #              tags$td(width = "5%",
  #         checkboxInput("showHelp","",value=FALSE) 
  #              ),
  fluidRow(helpHeaderText(HTML("Follow these 4 steps. The <strong>?</strong> tabs give more help."))),
  # tags$td(width = "90%",
          # conditionalPanel(condition="input.showHelp",            
                           tabsetPanel(id="Help",
                # Help tab
                                 tabPanel("Help:",
                         style = paste("background: ",maincolours$graphC,";"),
                         # wellPanel(
                         #   style = paste("background: ",maincolours$graphC,";"),
                         #   tags$table(width = "100%",class="myTable",
                         #              tags$tr(
                         #                tags$div(style = helpStyle,
                         #                         tags$br(HTML("<b>"),"Use the ", helpChar, " tabs below for more",HTML("</b>"))
                         #                )
                         #              )
                         #   )
                         # )
                ),

                # Step 1 tab
                tabPanel("Step 1",id="Step1",
                         style = paste("background: ",maincolours$graphC),
                         wellPanel(
                           style = paste("background: ",subpanelcolours$hypothesisC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$div(style = helpStyle,
                                                 tags$br(HTML("<b>"),"Make a Hypothesis:",HTML("</b>")),
                                                 tags$br(HTML('&emsp;'), '1. Variables: choose variables'),
                                                 tags$br(HTML('&emsp;'), '2. Effects: set expected effect-sizes'),
                                        )
                                      )
                           )
                         )
                ),

                # Step 2 tab
                tabPanel("Step 2",id="Step2",
                         style = paste("background: ",maincolours$graphC),
                         wellPanel(
                           style = paste("background: ",subpanelcolours$designC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$div(style = helpStyle,
                                                 tags$br(HTML("<b>"),"Set a Design:",HTML("</b>")),
                                                 tags$br(HTML('&emsp;'), '1. Sampling: choose sample size & method'),
                                                 tags$br(HTML('&emsp;'), '2. Anomalies: build in sampling anomalies'),
                                        )
                                      )
                           )
                         )
                ),

                # Step 3 tab
                tabPanel("Step 3",id="Step3",
                         style = paste("background: ",maincolours$graphC),
                         wellPanel(
                           style = paste("background: ",subpanelcolours$simulateC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                                tags$div(style = helpStyle,
                                                         tags$br(HTML("<b>"),"Evidence:",HTML("</b>")),
                                                         tags$br(HTML('&emsp;'), '1. Single: simulate a sample and analyse it'),
                                                         tags$br(HTML('&emsp;'), '2. Multiple: run many samples at once'),
                                                )
                                        )
                           )
                         )
                ),
                
                # Step 4 tab
                tabPanel("Step 4",id="Step4",
                         style = paste("background: ",maincolours$graphC),
                         wellPanel(
                           style = paste("background: ",subpanelcolours$exploreC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$div(style = helpStyle,
                                                 tags$br(HTML("<b>"),"Explore:",HTML("</b>")),
                                                 tags$br(HTML('&emsp;'), '1. Look at the consequences of your decisions'),
                                                 tags$br(HTML('&emsp;'), '2. Vary any of them and look at outputs'),
                                        )
                                      )
                           )
                         )
                ),
                
                # Files tab
                tabPanel("Data",id="Data",
                         style = paste("background: ",maincolours$graphC),
                         wellPanel(
                           style = paste("background: ",subpanelcolours$filesC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$div(style = helpStyle,
                                                 tags$br(HTML("<b>"),"Data:",HTML("</b>")),
                                                 tags$br(HTML('&emsp;'), '1. Export data to a file or clipboard'),
                                                 tags$br(HTML('&emsp;'), '2. Import data from a file or the clipboard'),
                                        )
                                      )
                           )
                         )
                )
                # ) ) )
 )
)
