library(shiny)
library(shinyjs)
library(multcompView)
library(mixOmics)
library(corrplot)
library(BH)
library(NMF)
library(doParallel)
library(gridBase)
library(irlba)
library(pkgmaker)
library(registry)
library(rngtools)

fluidPage(
  useShinyjs(),
  navbarPage("Statistics",id="stats",
             tabPanel("Welcome",
              mainPanel(
                p("This is a work in progress. There are currently many bugs..."),
                p("Statistical test outputs are currently unformatted and not likely usable just yet."),
                p("Feel free to use the example dataset below to explore the currently implemented features."),
                p("NOTE: Be sure to only select a column with defined 'groups' in the 'Data Import' tab."),
                p("Download example data: https://raw.githubusercontent.com/CoreyGiles/ShinyApp/master/tempCSV.csv"),
                hr()
              ),
              sidebarLayout(
               sidebarPanel(
                 p(strong("Choose the type of statistics you'd like to perform:")),
                 radioButtons(inputId="simpleStats",label="",choices=c("Simple Statistics"="simple","'Omics Statistics"="omics"),selected = "simple")
               ),
               mainPanel(
                 p(strong("Simple statistics offer commonly used statistics for simple experimental designs")),
                 p("It is the best option for univariate statistics (ANOVA, t-test, mann-whitney U) with defined groups (Treatment vs control)."),
                 br(),
                 p(strong("'Omics statistics provides advanced statistical procedures for analysis of 'omics datasets")),
                 p("This will allow PCA and regression analysis. More will come!"),
                 p("This is better for multivariate statistics with defined groups"),
                 br(),
                 br(),
                 p("Once you have selected the type of statistics, continue onto the 'Data Import' tab to upload your data.")
               ))
             ),
             tabPanel("Data Import",sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload csv file:",accept=c('text/csv','.csv')),
                 checkboxInput(inputId = "header","First row contains variable names?",value=T),
                 hr(),
                 selectInput(inputId = "groupSelect","Select the Variable that identifies groups:",choices = "")
               ),
               mainPanel(
                 uiOutput("dataImport")
               )
             )),
             tabPanel("Preprocessing",mainPanel(
               p(code("This area is under development.")),
               uiOutput("preProcess")
             )),
             tabPanel("Exploration",mainPanel(
               verbatimTextOutput(outputId = "summaryTable"),
               hr(),
               h3("Correlation Analysis"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("exploreCORview",label="View:",choices = c("Variables"="var","Samples"="samples")),
                   hr(),
                   selectInput("exploreCORcolour3",label="High Colour:",choices=c("Red"="#FF0000","Blue"="#0000FF","Green"="#00FF00","White"="#FFFFFF","Black"="#000000"),selected = "#FF0000"),
                   selectInput("exploreCORcolour2",label="Mid Colour:",choices=c("Red"="#FF0000","Blue"="#0000FF","Green"="#00FF00","White"="#FFFFFF","Black"="#000000"),selected = "#FFFFFF"),
                   selectInput("exploreCORcolour1",label="Low Colour:",choices=c("Red"="#FF0000","Blue"="#0000FF","Green"="#00FF00","White"="#FFFFFF","Black"="#000000"),selected = "#0000FF")
                   
                   
                   
                 ),
                   mainPanel(
                   plotOutput("exploreCOR"))
               ),
               hr(),
               h3("Principle Component Analysis"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("explorePCAview",label="View:",choices = c("Variables"="var","Samples"="samples")),
                   numericInput("explorePCAcomp","Number of components:",value=2, min=2,max=10,step=1),
                   checkboxInput("explorePCAcentre","Centre variables",value=TRUE),
                   checkboxInput("explorePCAscale","Scale variables",value=TRUE),
                   hr(),
                   textInput("explorePCAxAxis","X-axis component:",value=1),
                   textInput("explorePCAyAxis","X-axis component:",value=2)
                 ),
                 mainPanel(
                   plotOutput("explorePCA"))
               )
             )),
             tabPanel("Univariate",verticalLayout(
               verticalLayout(sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId="uniVarSelect",label="Variable to analyse:",choices=character(0)),
                   hr(),
                   radioButtons(inputId="uniTestRadio","Statistical test:",choices="temp"),
                   textInput(inputId="uniPlotVMin",label="Plot-Y min:",value=1),
                   textInput(inputId="uniPlotVMax",label="Plot-Y max:",value=50)
                 ),
                 mainPanel(
                   splitLayout(
                     tagList(
                       verbatimTextOutput("uniStatSummary")
                     ),
                     tagList(
                       plotOutput(outputId = "uniVarPlot")
                     )
                   )
                 )),hr(),
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(inputId="corVarSelectX",label="X-axis Variable:",choices=character(0)),
                     selectInput(inputId="corVarSelectY",label="Y-axis variable:",choices=character(0)),
                     hr(),
                     radioButtons(inputId="corTestRadio","Statistical test:",choices="temp"),
                     textInput(inputId="corPlotXMin",label="Plot-X min:",value=1),
                     textInput(inputId="corPlotXMax",label="Plot-X max:",value=50),
                     textInput(inputId="corPlotYMin",label="Plot-Y min:",value=1),
                     textInput(inputId="corPlotYMax",label="Plot-Y max:",value=50)
                   ),
                   mainPanel(
                     splitLayout(
                       tagList(
                         verbatimTextOutput("corStatSummary")
                       ),
                       tagList(
                         plotOutput(outputId = "corVarPlot")
                       )
                     )
                   ))
               )
             )),
             tabPanel("Regression",sidebarLayout(
              sidebarPanel(
               selectInput("regressionResponseVar","Response variable",choices="none"),
               tags$div(id = 'placeholder', tags$div(tagList(
                 selectInput("regressionIndep1",label = "Independent Variable 1",choices="none")),
                 id = 'regressionInput1'
               )),
               actionButton('insertBtn', 'Insert'), 
               actionButton('removeBtn', 'Remove'),
               textOutput("formula")
              ),
              mainPanel(
                
              )
             )),
             tabPanel("Multivariate",mainPanel(
               p(code("Work In Progress"))
             )),
             tabPanel("Predictive",mainPanel(
               p(code("test this out"))
             ))
  )
)
