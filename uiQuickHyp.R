if (is_local) {
quickHypotheses<-
  tags$table(width = "100%",class="myTable",
             tags$tr(
               tags$td(width = "45%",
                       tags$div(style = localPlainStyle, "Hypothesis:")
               ),
               tags$td(width = "30%",
                       selectInput("Hypchoice", label = NULL,
                                   choices=
                                     list("i~i","i~o","i~c2","i~c3",
                                          "o~i","o~o","o~c2","o~c3",
                                          "c~i","c~o","c~c2","c~c3"," ",
                                          "i~i+i","i~c+i","i~i+c","i~c+c","  ",
                                          "i~w+i","i~i+w","i~w+c","i~w+w","  ",
                                          "c~i+i","c~c+i","c~i+c","c~c+c"),
                                   selected="i~i",
                                   selectize=FALSE)
               ),
               tags$td(width = "25%")
             ),
             tags$tr(
               tags$td(width = "45%",
                       tags$div(style = localPlainStyle, "Presets:")
                       ),
               tags$td(width = "30%", 
                                        selectInput("Effectchoice", label = NULL,
                                                    choices=
                                                      list("zeroes"="e0","interaction"="e1","opposite"="e2"),
                                                    selected="none",
                                                    selectize=FALSE)
                       ),
               tags$td(width = "25%")
             )
  )
} else {
  quickHypotheses<-c()
}

