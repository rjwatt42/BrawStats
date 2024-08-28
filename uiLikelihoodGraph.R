
likelihoodGraphPanel<-function() {
  
    likelihoodGraphPanel<-
      tabPanel("Possible",class="Graphs",
               style="margin:0px;padding:0px;",
               plotOutput("PossiblePlot")
      )
}

likelihoodGraphPanel1<-function() {
  
    likelihoodGraphPanel<-
      tabPanel("Possible",class="Graphs",
               style="margin:0px;padding:0px;",
               plotOutput("PossiblePlot1")
      )
}

likelihoodReportPanel<-function() {
  
  likelihoodReportPanel<-
    tabPanel("Possible",class="Graphs",
             style="margin:0px;padding:0px;",
             plotOutput("PossibleReport")
    )
}

likelihoodReportPanel1<-function() {
  
  likelihoodReportPanel<-
    tabPanel("Possible",class="Graphs",
             style="margin:0px;padding:0px;",
             plotOutput("PossibleReport1")
    )
}
