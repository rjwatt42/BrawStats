source("uiPossibleGraph.R")


MainGraphs <- function() {
  wellPanel(
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Graphs",
                tabPanel("Sample",class="Graphs",
                         style="margin:0px;padding:0px;",
                         plotOutput("SamplePlot")
                ),
                tabPanel("Describe",class="Graphs",
                         style="margin:0px;padding:0px;",
                         plotOutput("DescriptivePlot")
                ),
                tabPanel("Infer",
                         style="margin:0px;padding:0px;",
                         plotOutput("InferentialPlot")
                ),
                tabPanel("Likelihood",
                         style="margin:0px;padding:0px;",
                         plotOutput("LikelihoodPlot")
                ),
                tabPanel("Expect",value="Expect", 
                         style="margin:0px;padding:0px;",
                         plotOutput("ExpectedPlot")
                )
                ,tabPanel("Explore",value="Explore",
                          style="margin:0px;padding:0px;",
                          plotOutput("ExplorePlot")
                )
    ),
  )
}

MainGraphs1 <-function() {
  wellPanel(
    # style=paste("min-width:", graphWidth, ";"),
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Graphs",
                tabPanel("Sample",class="Graphs",
                         style="margin:0px;padding:0px;",
                         plotOutput("SamplePlot1")
                ),
                tabPanel("Describe",class="Graphs",
                         style="margin:0px;padding:0px;",
                         plotOutput("DescriptivePlot1")
                ),
                tabPanel("Infer",
                         style="margin:0px;padding:0px;",
                         plotOutput("InferentialPlot1")
                ),
                tabPanel("Likelihood",
                         style="margin:0px;padding:0px;",
                         plotOutput("LikelihoodPlot1")
                ),
                tabPanel("Expect",value="Expect", 
                         style="margin:0px;padding:0px;",
                         plotOutput("ExpectedPlot1")
                )
                ,tabPanel("Explore",value="Explore",
                          style="margin:0px;padding:0px;",
                          plotOutput("ExplorePlot1")
                )
    ),
    # ,width=fullPanelWidth
  )
}
