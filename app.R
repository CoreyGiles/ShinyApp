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
               p(code("This area is under development.")),
               uiOutput("preProcess")
             )),
             tabPanel("Exploration",mainPanel(
               verbatimTextOutput(outputId = "summaryTable")
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

  
  #############################################  Preprocess  ###########
  output$preProcess<-renderUI({
    lapply(1:varLength(),function(i) {
      fluidRow(
        column(width=4,tagList(
        h1(varList()[i])
      )),
      column(width=4,tagList(
        checkboxInput(paste0("ignore",i),label="Ignore")
      )),
      column(width=4,tagList(
        selectInput(paste0("transform",i),label="Transform",choices=c("None"="none","Logarithm"="log","Square root"="sqrt"))
      )))
    })
  })

  temp2<-reactive({
    ae<<-c()
    if(varLength()!=0) {
      lapply(1:varLength(),function(i) {
        eval(parse(text=paste0("temp<-input$ignore",i)))
        if(!is.null(temp) && temp) {
          ae<<-append(ae,which(colnames(dataInput())==varList()[i]))
        }
      })
    }
    return(ae)
  })
  
  dataOut<-reactive({
    if(!is.null(temp2())) {
      return(dataInput()[,-temp2()])
    } else {
      return(dataInput())
    }
  })
  #############################################  Exploration  ###########
  
  output$summaryTable<-renderPrint(summary(dataOut()))
  
  
  ####################################################   Univariate Statistics
  observe({
    updateSelectInput(session,"uniVarSelect",choices=varList())
  })

  observe({
    if(groupUnique()>2) {
      updateRadioButtons(session,"uniTestRadio",choices=c("ANOVA + Tukey HSD"="anova", "T-Test"="ttest","Mann-Whitney U"="mwu"))
    }else {
      updateRadioButtons(session,"uniTestRadio",choices=c("T-Test"="ttest","Mann-Whitney U"="mwu"))
    }
  })

  pVal<-reactive({
    test<-switch(input$uniTestRadio,
                 anova=runANOVA,
                 ttest=tTestStats,
                 mwu=mwuStats,
                 ttest)
    test(dataInput()[,input$uniVarSelect],groupList())
  })

  ####################################################   Univariate plots
  observe({
    updateTextInput(session,"uniPlotVMin",value=min(varYPlotMin()))
    updateTextInput(session,"uniPlotVMax",value=min(varYPlotMax()))
  })
  
  output$uniStatSummary<-renderPrint({
    selected<-input$uniVarSelect
    if(selected=="") {return(NULL)}
    print(pVal())
  })
  
  varYPlotMin<-reactive({
    selected<-input$uniVarSelect
    if(selected=="") {return(0)}
    return(min(isolate({dataInput()[,input$uniVarSelect]})))
  })
  
  varYPlotMax<-reactive({
    selected<-input$uniVarSelect
    if(selected=="") {return(10)}
    return(max(isolate({dataInput()[,input$uniVarSelect]})))
  })
  
  output$uniVarPlot<-renderPlot({
    selected<-input$uniVarSelect
    if(selected=="") {return(NULL)}
    bPlot<-boxplot(dataInput()[,selected]~groupList(),ylim=c(as.numeric(input$uniPlotVMin),1.1*as.numeric(input$uniPlotVMax)),col=c("red","green","blue"))
    over=0.1*max(bPlot$stats[nrow(bPlot$stats),])
    text( c(1:groupUnique()) , bPlot$stats[nrow(bPlot$stats),]+over,as.character(statLabels(pVal())) )
  })
  
  ####################################################   Correlation Statistics
  observe({
    updateSelectInput(session,"corVarSelectX",choices=varList())
    updateSelectInput(session,"corVarSelectY",choices=varList())
    updateRadioButtons(session,"corTestRadio",choices=c("Pearson's r"="pearson"))
  })

  ####################################################   Correlation plots
  observe({
    updateTextInput(session,"corPlotXMin",value=min(corXPlotMin()))
    updateTextInput(session,"corPlotXMax",value=min(corXPlotMax()))
    updateTextInput(session,"corPlotYMin",value=min(corYPlotMin()))
    updateTextInput(session,"corPlotYMax",value=min(corYPlotMax()))
  })
  
  output$corStatSummary<-renderPrint({
    selectedY<-input$corVarSelectY
    selectedX<-input$corVarSelectX
    if(selectedY==""||selectedX=="") {return(NULL)}
    print(cor(dataInput()[,input$corVarSelectX],dataInput()[,input$corVarSelectY]))
  })
  
  corXPlotMin<-reactive({
    selected<-input$corVarSelectX
    if(selected=="") {return(0)}
    return(min(isolate({dataInput()[,selected]})))
  })
  corXPlotMax<-reactive({
    selected<-input$corVarSelectX
    if(selected=="") {return(0)}
    return(max(isolate({dataInput()[,selected]})))
  })
  corYPlotMin<-reactive({
    selected<-input$corVarSelectY
    if(selected=="") {return(0)}
    return(min(isolate({dataInput()[,selected]})))
  })
  corYPlotMax<-reactive({
    selected<-input$corVarSelectY
    if(selected=="") {return(0)}
    return(max(isolate({dataInput()[,selected]})))
  })
  
  output$corVarPlot<-renderPlot({
    selectedY<-input$corVarSelectY
    selectedX<-input$corVarSelectX
    if(selectedY==""||selectedX=="") {return(NULL)}
    plot(dataInput()[,selectedX],dataInput()[,selectedY],ylim=c(as.numeric(input$corPlotYMin),as.numeric(input$corPlotYMax)),xlim=c(as.numeric(input$corPlotXMin),as.numeric(input$corPlotXMax)))
  })
}





shinyApp(ui=ui,server=server)







statLabels<-function(pVal) {
  labels<-data.frame(multcompLetters(pVal)['Letters'])
  labels$treatment=rownames(labels)
  labels<-labels[order(labels$treatment),1]
  return(labels)
}

runANOVA<-function(data,group) {
  model=lm(data~group)
  ANOVA=aov(model)
  TukeyHSD(x=ANOVA, conf.level=0.95)[[1]][,4]
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
