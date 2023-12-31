#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

list.of.packages<-c("shiny","shinyWidgets","shinyjs","shinyBS",
                    "ggplot2","tidyr","tools",
                    "mnormt","lme4","MuMIn",
                    "readxl","writexl","car","stringi","stringr","clipr","SuppDists","e1071","pracma"
                    )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

library(ggplot2)
library(tidyr)
library(tools)       # system

# library(gridExtra)   # not sure any longer
library(mnormt)      # pmnorm for logistic
library(lme4)        # lmer (mixed models)
library(MuMIn)       # r-squared for mixed models
library(readxl)      # excel
library(writexl)     # x excel
library(car)         # Anova type 3 correct
library(stringr)     # for str_* functions
library(clipr)       # for clipboard functions
library(SuppDists)   # for Johnson distributions
library(e1071)       # for skewness and kurtosis
library(pracma)      # for meshgrid


source("uiHeaderText.R") # headers to each tab

source("uiHypothesis.R")
source("uiHelp.R")
source("uiDesign.R")
source("uiEvidence.R")
source("uiExplore.R")
source("uiFiles.R")
source("uiLikelihood.R")


source("uiHypothesisDiagram.R")
source("uiPopulationDiagram.R")
source("uiMainGraph.R")
source("uiMainReport.R")

source("uiInspectDiagram.R")

shinyUI(fluidPage(
    useShinyjs(),
    
    tags$style(type="text/css",".recalculating {opacity: 1.0;}" ),   
               
    tags$script('
    pressedKeyCount = 0;
    $(document).on("keydown", function (e) {
       Shiny.onInputChange("pressedKey", pressedKeyCount++);
       Shiny.onInputChange("keypress", e.which);
    });
    $(document).on("keyup", function (e) {
       Shiny.onInputChange("releasededKey", pressedKeyCount++);
    });
    '),
    
    tags$head(
        tags$script(
            "$(document).on('shiny:inputchanged', function(event) {
          if (event.name != 'changed') {
            Shiny.setInputValue('changed', event.name);
          }
        });"
        )
    ),
    
    tags$head(
        tags$style(type = 'text/css', paste("#SampleReport         {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#DescriptiveReport    {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#InferentialReport    {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#ExpectedReport    {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#ExploreReport    {background-color: ", maincolours$graphC, ";}"))
    ),
    tags$head(
        tags$style(type = 'text/css', paste("#SampleGraph         {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#DescriptiveGraph    {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#InferentialGraph    {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#ExpectedGraph    {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#ExploreGraph    {background-color: ", maincolours$graphC, ";}"))
    ),
    
    tags$style(type="text/css", ".shiny-file-input-progress { display: none }"),
    
    tags$head(
        tags$style(
            HTML(".shiny-notification {position:fixed;top: calc(20%);left: calc(55%);}")
        )
    ),
    
    # 
    # tags$style("#DataTabset { display:none; }"), 
    # 
    tags$head(
        tags$style(paste0("label{font-size: ",format(8*fontScale) ,"pt;}")),
        # tags$style(HTML( # textInput
        #     ".form-control {font-size: 8pt; height:20px; padding:0px 0px; margin-top:-20px; margin-bottom:-5px; min-height:10px;}"
        # )),
        tags$style(HTML( # textInput
            paste0(".form-control {font-size: ",format(8*fontScale) ,"pt; height:20px; padding:0px 0px;}")
        )),
        tags$style(HTML( # selectInput
            paste0(".selectize-input {font-size: ",format(8*fontScale) ,"pt; height:12px; width:60px; padding:0px; margin-right:-10px; margin-top:-5px;margin-bottom:-5px; min-height:10px;}"),
            paste0(".selectize-dropdown { font-size: ",format(8*fontScale) ,"pt;line-height:10px}")
        )),
        tags$style(HTML( # helpText
            paste0(".help-block {font-size: ",format(8*fontScale) ,"pt; height:20px; padding:0px 0px; margin-top:25px; margin-bottom:-5px; min-height:10px;}")
            )),
        tags$style(HTML( # slider bar
            ".irs {margin:0px; margin-bottom:-15px; padding:0px;padding-bottom:-15px; height:30px;}",
            ".irs-with-grid {height:50px;}",
            ".irs-bar {height:6.5px; top:15.5px;}",
            ".irs-bar-edge {border-color: transparent; background-color: transparent;}",
            paste0(".irs-grid-text {font-size:",format(4*fontScale) ,"pt;}"),
            paste0(".irs-max {font-size:",format(5*fontScale) ,"pt;}"),
            paste0(".irs-min {font-size:",format(5*fontScale) ,"pt;}"),
            paste0(".irs-single {font-size:",format(5*fontScale) ,"pt;}"),
            ".irs-slider {width: 10px; height: 10px; top:14px;padding:0px;}",
            ".irs-line {top: 15px;}",
            ".irs-grid {height: 21px; top: 23px}"
        )),
        tags$style(HTML( # specific slider bar
            "#r .irs-bar {background-color: transparent; border-color: transparent; }"
        )),
        tags$style(HTML( # action button
           paste0(".col-sm-3 button {font-size:",format(8*fontScale) ,"pt;font-weight:Bold;color:white; background-color: #005886;height:20px;padding-top:0px;padding-bottom:0px;padding-left:4px;padding-right:4px;margin-bottom:4px;margin-right:12px;margin-top:4px;margin-left:0px}"),
           paste0( ".col-sm-2 button {font-size:",format(8*fontScale) ,"pt;font-weight:Bold;color:white; background-color: #005886;height:20px;padding-top:0px;padding-bottom:0px;padding-left:4px;padding-right:4px;margin-bottom:4px;margin-right:12px;margin-top:4px;margin-left:0px}"),
           paste0(".col-sm-1 button {font-size:",format(8*fontScale) ,"pt;font-weight:Bold;color:white; background-color: #005886;height:20px;padding-top:0px;padding-bottom:0px;padding-left:4px;padding-right:4px;margin-bottom:4px;margin-right:12px;margin-top:4px;margin-left:0px}")
        )),
        tags$style(HTML( # tab panels
            paste0(".tabbable > .nav > li > a {font-weight: normal; font-size: ",format(8*fontScale) ,"pt; padding:2px; margin:1px; color:#222222; background-color:#dddddd}"),
            ".tabbable > .nav > .active > a {font-weight: bold; color:black;  background-color:white; }",
            paste0(".nav-tabs {font-size: ",format(8*fontScale) ,"pt; padding:0px; margin-bottom:0px;} "),
        )),
        tags$style(HTML( # well panels
                ".well {padding:8px; margin:0px;margin-bottom:8px;margin-left:0px;margin-right:0px;} ",
        )),
        tags$style(HTML(paste(".help-block b {color:", maincolours$panelC,  "!important;margin:0px;padding:0px;margin-bottom:8px;font-size:",format(12*fontScale) ,"pt; font-weight:bold;}"))),
        tags$style(HTML(paste(".help-block a {color:", maincolours$panelC,  "!important;margin:0px;padding:0px;margin-bottom:8px;font-size:",format(8*fontScale) ,"pt; font-weight:normal;font-style: italic;}"))),
        tags$style(HTML(paste0(".btn-file {padding:0px; margin: 0px; font-size:",format(8*fontScale) ,"pt; font-weight:Bold; color:white; background-color: #005886;height:20px;padding-top:0px;padding-bottom:0px;padding-left:4px;padding-right:4px;margin-bottom:8px;margin-right:12px;margin-top:0px;margin-left:0px}")
        )),
        tags$style(HTML(
            ".checkbox {line-height: 10px;margin:0px;padding:0px;}"
        )),
    ),
    tags$head( # input tables
        tags$style(type="text/css",".table label{ display: table-cell; text-align: center;vertical-align: middle; } .myTable .form-group { display: table-row;}")
    ),

    # Application title
    # titlePanel("BrawStats"),
    
    # basic controls
    sidebarLayout(
                 sidebarPanel(
                     style = paste("background: ",maincolours$panelC), 
                     verticalLayout(
                         # Help panel                
                         HelpTab,
                         # Hypothesis panel                
                         HypothesisTab,
                         # Design panel
                         DesignTab,
                         # Evidence panel
                         EvidenceTab,
                         # Explore panel
                         ExploreTab,
                         # Likelihood panel
                         LikelihoodTab,
                         # Files panel
                         FilesTab
                     ),
                     width = 3
                 ),
                 # 
                 # results`
                 mainPanel(
                     style = paste("background: ",maincolours$windowC), 
                     column(width=4,
                            HypothesisDiagram,
                            PopulationDiagram
                     ),
                     column(width=8,
                            MainGraphs,
                            MainReports
                     ),
                     bsModal(id="testedOutput", title="tested Outputs", trigger="testedOutputButton", size = "large",plotOutput("plotPopUp")
                     ),
                     inspectDiagram,
                     width = 9
                 ),
        ),
        setBackgroundColor(maincolours$windowC)
    )
)
