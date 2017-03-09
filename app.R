library(shiny)
library(multcompView)

ui<-fluidPage(
  navbarPage("Statistics",
             tabPanel("Welcome",sidebarLayout(
               sidebarPanel(
                 p(strong("Choose the type of statistics you'd like to perform:")),
                 radioButtons(inputId="simpleStats",label="",choices=c("Simple Statistics"="simple","'Omics Statistics"="omics"),selected = "simple")
               ),
               mainPanel(
                 p(strong("Simple statistics offer commonly used statistics for simple experimental designs")),
                 p("It is the best option for univariate statistics (ANOVA, t-test, mann-whitney U) with defined groups (Treatment vs control)."),
                 br(),
                 p(strong("'Omics statistics provides advanced statistical procedures for analysis of 'omics datasets")),
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
               p(code("test this out")),
               textInput(inputId = "textIn","Enter some Text here:", value=""),
               p(code("test this out")),
               textOutput(outputId = "b")
             )),
             tabPanel("Exploration",mainPanel(
               verbatimTextOutput(outputId = "summaryTable")
             )),
             tabPanel("Univariate",verticalLayout(
               uiOutput("univariateMeans")                                   ####################
             )),
             tabPanel("Univariate2",verticalLayout(
               #selectInput(inputId="uniVarSelect",label="Variable to analyse:",choices=character(0))
               #uiOutput("univariate")                                   ####################
               verticalLayout(sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId="uniVarSelect",label="Variable to analyse:",choices=character(0)),
                   hr(),
                   textInput(inputId="uniPlotVMin",label="Plot-Y min:",value=1),
                   textInput(inputId="uniPlotVMax",label="Plot-Y max:",value=50)
                 ),
                 mainPanel(
                   #uiOutput(paste0("univariateMain",i))
                 )),hr())
             )),
             tabPanel("Multivariate",mainPanel(
               p(code("test this out"))
             )),
             tabPanel("Predictive",mainPanel(
               p(code("test this out"))
             ))
  )
)


server<-function(input,output,session) {
  #############################################  Welcome Page  ###########
  
  
  #############################################  Data Import   ###########
  dataInput<-reactive({
    fileSaved<-input$file
    if(is.null(fileSaved)) 
      return(NULL)
    data<-read.csv(fileSaved$datapath,header=input$header)
    updateSelectInput(session,"groupSelect",choices=colnames(data))
    return(data)
  })
  
  output$dataImport<-renderUI({
    if(!is.null(dataInput())) {
      tagList(h3("Uploaded Data Overview:"),
              br(),
              dataTableOutput("dataTable")
      )
    }
  })
  
  groups<-reactive({                                                       ## groups()    Currently selected group variable name
    input$groupSelect
  })
  groupList<-reactive({                                                    ## groupList()   List of groups that subjects belong to
    dataInput()[[input$groupSelect]]
  })
  groupUnique<-reactive({                                                  ## groupUnique()   Number of unique groups
    nlevels(dataInput()[[input$groupSelect]])
  })
  varList<-reactive({                                                      ## varList()   List of variable names
    colnames(dataInput())[-which(colnames(dataInput())==groups())]
  })
  varLength<-reactive({                                                    ## varLength()   Number of variables
    length(varList())
  })
  output$dataTable<-renderDataTable(dataInput(),options=list(pageLength=10))

  
  #############################################  Exploration  ###########
  
  output$summaryTable<-renderPrint(summary(dataInput()))
  
  
  #############################################  Univariate Statistics  ###########
  output$univariateMeans<-renderUI({
    verticalLayout(
      lapply(1:varLength(), function(i) {tagList(sidebarLayout(
        sidebarPanel(
          textOutput(outputId=paste0("univariateLabel",i)),
          hr(),
          if(groupUnique()>2) {
            radioButtons(inputId=paste0("univariateRadio",i),"Statistical test:",c("ANOVA + Tukey HSD"="anova", "T-Test"="ttest","Mann-Whitney U"="mwu"))
          }else {
            radioButtons(inputId=paste0("univariateRadio",i),"Statistical test:",c("T-Test"="ttest","Mann-Whitney U"="mwu"))
          },
          textInput(inputId=paste0("uniTextMin",i),label="Plot-Y min:",value=min(dataInput()[,which(colnames(dataInput())==varList()[i])])),
          textInput(inputId=paste0("uniTextMax",i),label="Plot-Y max:",value=max(dataInput()[,which(colnames(dataInput())==varList()[i])]))
          ),
        mainPanel(
          uiOutput(paste0("univariateMain",i))
        )),hr())
        }
    ))
  })
  
  univariateStats<-observe(lapply(1:varLength(),function(i) {
    variable<-which(colnames(dataInput())==varList()[i])
    stats<-numeric(groupUnique()*(groupUnique()+1)/2-groupUnique())
    sigLabels<-stats
    if(!is.null(dataInput())) {
      output[[paste0("univariateMain",i)]]<-renderUI({splitLayout({
        if(input[[paste0("univariateRadio",i)]]=="ttest") {
          stats<<-tTestStats(dataInput()[,variable],groupList())
          sigLabels<<-statLabels(stats)
        }
        if(input[[paste0("univariateRadio",i)]]=="anova") {
          model=lm(dataInput()[,variable]~groupList())
          ANOVA=aov(model)
          stats<<-TukeyHSD(x=ANOVA, conf.level=0.95)[[1]][,4]
          sigLabels<<-statLabels(stats)
        }
        p(input[[paste0("univariateRadio",i)]])},
        plotOutput(outputId = paste0("tempHackPlot",i))
      )})
      output[[paste0("tempHackPlot",i)]]<-renderPlot({
        #hist(dataInput()[,which(colnames(dataInput())==varList()[i])],main=varList()[i],xlab=varList()[i])
        #bPlot<-boxplot(dataInput()[,variable]~groupList(),ylim=c(min(dataInput()[,variable]),1.1*max(dataInput()[,variable])),col=c("red","green","blue"))
        bPlot<-boxplot(dataInput()[,variable]~groupList(),ylim=c(as.numeric(eval(parse(text=paste0("input$uniTextMin",i)))),1.1*as.numeric(eval(parse(text=paste0("input$uniTextMax",i))))),col=c("red","green","blue"))
        over=0.1*max(bPlot$stats[nrow(bPlot$stats),])
        text( c(1:groupUnique()) , bPlot$stats[nrow(bPlot$stats),]+over,as.character(sigLabels) )
      })
      output[[paste0("univariateLabel",i)]]<-renderText({
        paste(varList()[[i]],eval(parse(text=paste0("input$uniTextMin",i))))
      })
    }
  }))

#############################################  Univariate Statistics  ###########
  output$univariate<-renderUI({
    verticalLayout(sidebarLayout(
        sidebarPanel(
          i<-1,
          selectInput(inputId="uniVarSelect",label="Variable to analyse:",choices=varList()),
          p(paste0(input$uniVarSelect)),
          hr(),
          if(groupUnique()>2) {
            radioButtons(inputId=paste0("univariateRadio",i),"Statistical test:",c("ANOVA + Tukey HSD"="anova", "T-Test"="ttest","Mann-Whitney U"="mwu"))
          }else {
            radioButtons(inputId=paste0("univariateRadio",i),"Statistical test:",c("T-Test"="ttest","Mann-Whitney U"="mwu"))
          },
          textInput(inputId=paste0("uniTextMin",i),label="Plot-Y min:",value=min(dataInput()[,which(colnames(dataInput())==varList()[i])])),
          textInput(inputId=paste0("uniTextMax",i),label="Plot-Y max:",value=max(dataInput()[,which(colnames(dataInput())==varList()[i])]))
        ),
        mainPanel(
          uiOutput(paste0("univariateMain",i))
        )),hr())
  })
  
  observe({
    updateSelectInput(session,"uniVarSelect",choices=varList())
  })
}

shinyApp(ui=ui,server=server)


statLabels<-function(pVal) {
  labels<-data.frame(multcompLetters(pVal)['Letters'])
  labels$treatment=rownames(labels)
  labels<-labels[order(labels$treatment),1]
  return(labels)
}

tTestStats<-function(X,Y) {
  nGroups<-nlevels(Y)
  nComps<-nGroups*(nGroups+1)/2-nGroups
  groups<-levels(Y)
  pVal<-numeric(nComps)
  t<-1
  group1<-1
  group2<-2
  while(t<(nComps+1)) {
    selected<-c(which(Y==groups[group1]),which(Y==groups[group2]))
    pVal[t]<-t.test(X[selected]~Y[selected])$p.value
    names(pVal)[t]<-paste(groups[group2],"-",groups[group1],sep="")
    group2<-group2+1
    if(group2>nGroups) {
      group1<-group1+1
      group2<-group1+1
    }
    t<-t+1
  }
  return(pVal)
}

mwuStats<-function(X,Y) {
  nGroups<-nlevels(Y)
  nComps<-nGroups*(nGroups+1)/2-nGroups
  groups<-levels(Y)
  pVal<-numeric(nComps)
  t<-1
  group1<-1
  group2<-2
  while(t<(nComps+1)) {
    selected<-c(which(Y==groups[group1]),which(Y==groups[group2]))
    pVal[t]<-wilcox.test(X[selected]~Y[selected])$p.value
    names(pVal)[t]<-paste(groups[group2],"-",groups[group1],sep="")
    group2<-group2+1
    if(group2>nGroups) {
      group1<-group1+1
      group2<-group1+1
    }
    t<-t+1
  }
  return(pVal)
}





# A panel of colors to draw each group with the same color :
my_colors=c( rgb(143,199,74,maxColorValue = 255),rgb(242,104,34,maxColorValue = 255), rgb(111,145,202,maxColorValue = 255),rgb(254,188,18,maxColorValue = 255) , rgb(74,132,54,maxColorValue = 255),rgb(236,33,39,maxColorValue = 255),rgb(165,103,40,maxColorValue = 255))

