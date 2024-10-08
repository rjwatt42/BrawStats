MainReports <- function() {
  wellPanel(id="MainReports",
            style = paste("background: ",maincolours$panelC), 
            tabsetPanel(type="tabs",
                        id="Reports",
                        tabPanel("Sample",     
                                 style="margin:0px;padding:0px;",
                                 htmlOutput("SampleReport")
                                 ),
                        tabPanel("Describe",   
                                 style="margin:0px;padding:0px;",
                                 htmlOutput("DescriptiveReport")
                                 ),
                        tabPanel("Infer",      
                                 style="margin:0px;padding:0px;",
                                 htmlOutput("InferentialReport")
                        ),
                        tabPanel("Likelihood",      
                                 style="margin:0px;padding:0px;",
                                 htmlOutput("LikelihoodReport")
                        )
                        ,tabPanel("Expect",value="Expect",   
                                  style="margin:0px;padding:0px;",
                                  htmlOutput("ExpectedReport"))
                        ,tabPanel("Explore",value="Explore",
                                  style="margin:0px;padding:0px;",
                                  htmlOutput("ExploreReport")
                                  )
            )
  )
}

MainReports1 <- function() {
  wellPanel(id="MainReports",
            style = paste("background: ",maincolours$panelC), 
            tabsetPanel(type="tabs",
                        id="Reports",
                        tabPanel("Sample",     
                                 style="margin:0px;padding:0px;",
                                 htmlOutput("SampleReport1")
                                 ),
                        tabPanel("Describe",   
                                 style="margin:0px;padding:0px;",
                                 plotOutput("DescriptiveReport1")
                                 ),
                        tabPanel("Infer",      
                                 style="margin:0px;padding:0px;",
                                 plotOutput("InferentialReport1")
                        ),
                        tabPanel("Likelihood",      
                                 style="margin:0px;padding:0px;",
                                 plotOutput("likelihoodReportPanel1")
                        )
                        ,tabPanel("Expect",value="Expect",   
                                  style="margin:0px;padding:0px;",
                                  plotOutput("ExpectedReport1")
                                  )
                        ,tabPanel("Explore",value="Explore",
                                  style="margin:0px;padding:0px;",
                                  plotOutput("ExploreReport1")
                                  )
            )
  )
}

