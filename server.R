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

function(input,output,session) {
  #############################################  Welcome Page  ###########

  observe({
    if(is.null(dataInput())) {
      shinyjs::hide(selector = "#stats li a[data-value=Preprocessing]")
      shinyjs::hide(selector = "#stats li a[data-value=Exploration]")
      shinyjs::hide(selector = "#stats li a[data-value=Univariate]")
      shinyjs::hide(selector = "#stats li a[data-value=Regression]")
      shinyjs::hide(selector = "#stats li a[data-value=Multivariate]")
      shinyjs::hide(selector = "#stats li a[data-value=Predictive]")
    } else {
      shinyjs::show(selector = "#stats li a[data-value=Preprocessing]")
      shinyjs::show(selector = "#stats li a[data-value=Exploration]")
      shinyjs::show(selector = "#stats li a[data-value=Univariate]")
      shinyjs::show(selector = "#stats li a[data-value=Regression]")
      if(input$simpleStats=="omics") {
        shinyjs::show(selector = "#stats li a[data-value=Multivariate]")
        shinyjs::show(selector = "#stats li a[data-value=Predictive]")
        shinyjs::hide(selector = "#stats li a[data-value=Regression]")
      } else if(input$simpleStats=="regression"){
        shinyjs::hide(selector = "#stats li a[data-value=Multivariate]")
        shinyjs::hide(selector = "#stats li a[data-value=Predictive]")
        shinyjs::show(selector = "#stats li a[data-value=Regression]")
      } else {
        shinyjs::hide(selector = "#stats li a[data-value=Multivariate]")
        shinyjs::hide(selector = "#stats li a[data-value=Predictive]")
        shinyjs::hide(selector = "#stats li a[data-value=Regression]")
        
      }
    }
  })
  
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
    if(is.null(dataInput())) {
      return(NULL)
    }
    input$groupSelect
  })
  groupList<-reactive({                                                    ## groupList()   List of groups that subjects belong to
    if(is.null(dataInput())) {
      return(NULL)
    }
    dataInput()[[input$groupSelect]]
  })
  groupUnique<-reactive({                                                  ## groupUnique()   Number of unique groups
    if(is.null(dataInput())) {
      return(NULL)
    }
    nlevels(dataInput()[[input$groupSelect]])
  })
  varList<-reactive({                                                      ## varList()   List of variable names
    if(is.null(dataInput())) {
      return(NULL)
    }
    colnames(dataInput())[-which(colnames(dataInput())==groups())]
  })
  varLength<-reactive({                                                    ## varLength()   Number of variables
    if(is.null(dataInput())) {
      return(NULL)
    }
    length(varList())
  })
  factorList<-reactive({
    if(is.null(dataInput())) {
      return(NULL)
    }
    names(Filter(is.factor,dataInput()))
  })
  contList<-reactive({
    if(is.null(dataInput())) {
      return(NULL)
    }
    colnames(dataInput())[-which(colnames(dataInput())==factorList())]
  })
  
  
  
  output$dataTable<-renderDataTable(dataInput(),options=list(pageLength=10))
  
  
  #############################################  Preprocess  ###########
  output$preProcess<-renderUI({
    if(!is.null(varLength())) {
      lapply(1:length(colnames(dataInput())),function(i) {
        fluidRow(
          column(width=4,tagList(
            h3(colnames(dataInput())[i])
          )),
          column(width=4,tagList(
            checkboxInput(paste0("ignore",i),label="Ignore")
          )),
          column(width=4,tagList(
            selectInput(paste0("transform",i),label="Transform",choices=c("None"="none","Logarithm"="log","Square root"="sqrt"))
          )))
      })
    }
  })
  
  variableIgnore<-reactive({
    ae<<-c()
    if(!is.null(dataInput()) && varLength()!=0) {
      lapply(1:length(colnames(dataInput())),function(i) {
        eval(parse(text=paste0("temp<-input$ignore",i)))
        if(!is.null(temp) && temp) {
          ae<<-append(ae,which(colnames(dataInput())==colnames(dataInput())[i]))
        }
      })
    }
    return(ae)
  })
  
  dataOut<-reactive({
    if(!is.null(variableIgnore())) {
      return(dataInput()[,-variableIgnore()])
    } else {
      return(dataInput())
    }
  })
  
  dataDetails<-reactiveValues()

  dataDetails$contList<-reactive({
    if(!is.null(dataOut()))
      colnames(dataOut())[which(colnames(dataOut())%in%contList())]
  })
  #############################################  Exploration  ###########
  
  output$summaryTable<-renderPrint(summary(dataOut()))
  
  corPlot<-reactive({
    if(input$exploreCORview=="var")
      cor(dataInput()[,dataDetails$contList()])
    else
      cor(t(dataInput()[,dataDetails$contList()]))
  })
  
  output$exploreCOR<-renderPlot({
    validate(
      need(dataInput(),"Upload data in the 'Data Import' tab.")
    )
    col<-colorRampPalette(c(input$exploreCORcolour1,input$exploreCORcolour2,input$exploreCORcolour3))
    corrplot(corPlot(),method = "color",col=col(50),outline=TRUE)
  })
  
  pcaPlot<-reactive({
    pca(dataInput()[,varList()],ncomp=input$explorePCAcomp,center=input$explorePCAcentre,scale=input$explorePCAscale)
  })
  
  output$explorePCA<-renderPlot({
    validate(
      need(dataInput(),"Upload data in the 'Data Import' tab.")
    )
#    if(!is.null(dataInput()) && input$simpleStats=="omics") {
    if(input$simpleStats=="omics") {
      if(input$explorePCAview=="samples") {
        plotIndiv(pcaPlot(),comp=c(as.numeric(input$explorePCAxAxis),as.numeric(input$explorePCAyAxis)))
      }
      if(input$explorePCAview=="var") {
        plotVar(pcaPlot(),comp=c(as.numeric(input$explorePCAxAxis),as.numeric(input$explorePCAyAxis)))
      }
    } else {
      return(NULL)
    }
  })
  
  observe({
    if(!is.null(groupUnique())) {
      updateNumericInput(session,"explorePCAcomp",max=groupUnique())
    }
  })
  
  ####################################################   Univariate Statistics
  observe({
    for(i in 1:length(varList())) {
      if(!is.factor(varList()))
        selected<-varList()[i]
    }
    updateSelectInput(session,"uniVarSelect",choices=varList(),selected = selected)
  })
  
  observe({
    if(!is.null(groupUnique())) {
      if(groupUnique()>2) {
        updateRadioButtons(session,"uniTestRadio",choices=c("ANOVA + Tukey HSD"="anova", "T-Test"="ttest","Mann-Whitney U"="mwu"))
      }else {
        updateRadioButtons(session,"uniTestRadio",choices=c("T-Test"="ttest","Mann-Whitney U"="mwu"))
      }
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
    for(i in 1:length(varList())) {
      if(!is.factor(varList()))
        selected<-varList()[i]
    }
    updateSelectInput(session,"corVarSelectX",choices=varList(),selected = selected)
    updateSelectInput(session,"corVarSelectY",choices=varList(),selected = selected)
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
    print(paste0("Pearson's r: ",cor(dataInput()[,input$corVarSelectX],dataInput()[,input$corVarSelectY])))
    print(summary(lm(dataInput()[,input$corVarSelectX]~dataInput()[,input$corVarSelectY])))    
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
    abline(lm(dataInput()[,input$corVarSelectY]~dataInput()[,input$corVarSelectX]))
  })
  
  
  ####################################################   Regression
  inserted<-reactiveValues()
  inserted$reg <- c("regressionInput1")
  
  observeEvent(input$insertBtn, {
    num <- length(inserted$reg)+1
    id <- paste0('regressionInput', num)
    insertUI(
      selector = '#placeholder',
      ## wrap element in a div with id for ease of removal
      ui = tags$div(tagList(
        selectInput(paste0("regressionIndep",num),label = paste0("Independent Variable ", num),choices = colnames(dataInput()))), 
        id = id
      )
    )
    inserted$reg <<- c(id, inserted$reg)
  })
  
  observeEvent(input$removeBtn, {
    if(length(inserted$reg)>1) {
      removeUI(
        selector = paste0('#', inserted$reg[1])
      )
      inserted$reg <<- inserted$reg[-1]
    }
  })
  
  output$formula<-renderText({
    len<-length(inserted$reg)
    vars<-c()
    for(i in 1:len) {
      vars<-c(vars,input[[paste0("regressionIndep",i)]])
    }
    print(paste0(input$regressionResponseVar,"~",paste(vars,sep="",collapse="+"),collapse=""))
  })
  
  observeEvent(varList(),{
    if(!is.null(varList()) && length(varList())>0) {
      updateSelectInput(session,"regressionResponseVar",choices=colnames(dataInput()))
      updateSelectInput(session,"regressionIndep1",choices=colnames(dataInput()))
    }
  })
  
  regression<-reactive({
    if(!is.null(varList()) && length(varList())>0) {
      if(is.factor(dataInput()[,input$regressionResponseVar]) && input$regressionType=="linear") {
        alert("Response variable must be continuous for linear regression")
        return(NULL)
      }
      if(!is.factor(dataInput()[,input$regressionResponseVar]) && input$regressionType=="logistic") {
        alert("Response variable must be a factor for logistic regression")
        return(NULL)
      }
      if(input$regressionType=="logistic" && nlevels(dataInput()[,input$regressionResponseVar])!=2) {
        alert("Response variable must only have two levels. Please remove groups on the 'Preprocessing' tab.")
      }
      len<-length(inserted$reg)
      vars<-c()
      for(i in 1:len) {
        vars<-c(vars,input[[paste0("regressionIndep",i)]])
      }
      print("here")
      regressionFormula<-as.formula(paste0(input$regressionResponseVar,"~",paste(vars,sep="",collapse="+"),collapse=""))
      if(input$regressionType=="linear") {
        return(lm(regressionFormula,data=dataInput()))
      }
      if(input$regressionType=="logistic") {
        return(glm(regressionFormula,family=binomial(link='logit'),data=dataInput()))
      }
    }
  })
  
  output$regStatSummary<-renderPrint({
    validate(
      need(regression(), "")
    )
    print(summary(regression()))
  })
  
  output$regPlot<-renderPlot({
    validate(
      need(regression(), "Ensure the appropriate regression type and response variable is selected")
    )
    X<-predict(regression())
    Y<-dataInput()[,input$regressionResponseVar]
    plot(X,Y)
    #if(input$regressionType=="logistic") {
    #  curve(predict(regression(),data.frame(   =x),type="resp),add=TRUE)
    #}
  })
}
