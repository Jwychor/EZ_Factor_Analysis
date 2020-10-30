EZ_FA<-function(){
  ####Options####
  #Remove Scientific Notation
  options(scipen=999)
  
  ####Packages####
  load.lib<-c('rlang','DT','psych','jmv','shiny','tidyverse','ggcorrplot','shinythemes')
  
  sapply(load.lib,library,character=TRUE)
  
  #Custom Function Used Later
  round_df <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    df[,nums] <- round(df[,nums], digits = digits)
    
    (df)
  }
  
  temp<-names(Filter(function(x) {is(x, "data.frame")}, mget(ls(rlang::global_env()),env=rlang::global_env())))
  
  ####Application#####
  #Server
  server <- function(input, output, session) {
    
    #Reactives
    alpha2OrLess<-reactive({as.data.frame(matrix(c('Scales Need to have >2 Items for Scale Statistics')))})
    dataSelected<-reactive({if(exists(input$dataname) && length(input$dataname) !=0) get(as.character(input$dataname),env=global_env())})
    dataSelectedNumeric<-reactive({select_if(dataSelected(),is.numeric)})
    
    dataSelectedDvExclude<-reactive({select_if(dataSelected()[,-1],is.numeric)})
    dataSelectedScaled<-reactive({dataSelectedNumeric() %>%
        mutate_all(funs(scale))})
    
    dataSelectedCorrelations<-reactive({round(cor(dataSelectedNumeric(),use='pairwise.complete.obs'),2)})
    dataSelectedPvalMatrix<-reactive({round(cor_pmat(dataSelectedNumeric()),2)})
    
    factorReactive<-reactive({input$factor})
    loadingReactive<-reactive({input$loadings})
    rotationReactive<-reactive({input$rotation})
    startReactive<-eventReactive(input$button,{input$factor})
    scaleReactive<-reactive({input$scale})
    dvReactive<-reactive({as.character(input$column1)})
    interactionsReactive<-reactive({as.character(input$reg.inter)})
    
    itemStatistics<-reactive({psych::alpha(if(dvReactive()=='No'){dataSelectedNumeric()} else {dataSelectedDvExclude()})})
    itemStatisticsScaled<-reactive({input$scale})
    
    PCAResults<-eventReactive(input$button,{jmv::pca(data=if(dvReactive()=='No'){dataSelectedNumeric()} else dataSelectedDvExclude(),nFactorMethod='fixed',nFactors = factorReactive(),hideLoadings=loadingReactive(),
                                             screePlot=F,sortLoadings = T,eigen = T,rotation=rotationReactive(),
                                             factorSummary = T)})
    PCAEigenResults<-reactive({jmv::pca(if(dvReactive()=='No'){dataSelectedNumeric()} else {dataSelectedDvExclude()},screePlot=T,nFactorMethod = 'eigen',eigen=T)})
    
    PCALoadings<-reactive({as.data.frame(PCAResults()$loadings)})
    
    PCALoadingColumns<-reactive({PCALoadings()[,c(1,(itemStatisticsScaled()+1))]})
    PCAColumnsMatch<-reactive({subset(PCALoadingColumns(),!is.na(PCALoadingColumns()[,2]))})
    PCAColumnMatchNames<-reactive({PCAColumnsMatch()[,1]})
    
    currentSubscaleItemStatistics<-reactive({
      psych::alpha(
        dataSelectedNumeric()[,colnames(dataSelectedNumeric()) %in% PCAColumnMatchNames()])
    })
    
    currentSubscaleItemNames<-reactive({paste("'",as.character(colnames(dataSelectedNumeric())[colnames(dataSelectedNumeric()) %in% PCAColumnMatchNames()]),"'",sep="",collapse=", ")})
    
    IVsSelected<-reactive({as.character(input$reg.IV)})
    DVSelected<-reactive({as.character(input$reg.DV)})
    regressionLMExpression<-eventReactive(input$reg.button,{
      IVs<-paste(IVsSelected(),collapse=if(interactionsReactive()=='No'){'+'} else{'*'},sep='')
      IVNames<-vector(mode='character',length=length(IVsSelected()))
      if(input$reg.type=='Standardized'){
        IVNames<-paste("lm(",DVSelected(),'~',IVs,",data=dataSelectedScaled())")
      }
      else{
        IVNames<-paste("lm(",DVSelected(),'~',IVs,",data=dataSelectedNumeric())")
      }
      evaluatedRegression<-eval(parse(text=IVNames))
      regressionSummary<-summary(evaluatedRegression)
    })
    
    #Outputs
    output$cor.plot<-renderPlot({ggcorrplot(dataSelectedCorrelations(),outline.color='black',type='lower',
                                            lab=T,p.mat=dataSelectedPvalMatrix(),ggtheme=ggplot2::theme_bw(),
                                            lab_col = 'black',sig.level = .05,insig='blank')})
    
    output$reg.table<-DT::renderDataTable(
      DT::datatable(round_df(as.data.frame(regressionLMExpression()$coefficients),digits = 3),
                    options = list(pageLength = 100)))
    
    output$reg.table2<-renderText({HTML(paste("R-Squared = <u>",round(regressionLMExpression()$r.squared,3),
                                              "</u><br>F(",round(regressionLMExpression()$fstatistic[2],3),',',round(regressionLMExpression()$fstatistic[3],3),') = <u>',round(regressionLMExpression()$fstatistic[1],3),'</u><br>',
                                              "<br><b>Note:</b> p-values coming soon!</br>",sep=''))})
    
    output$factor.table <- DT::renderDataTable({
      DT::datatable(round_df(as.data.frame(PCAResults()$loadings),digits=3),
                    options = list(pageLength = ncol(dataSelectedNumeric()),
                                   lengthMenu = c(5,10,15,round(1/2*ncol(dataSelectedNumeric()),0),ncol(dataSelectedNumeric()))),rownames=F)
    })
    
    output$eigen.table <- DT::renderDataTable({
      DT::datatable(round_df(as.data.frame(PCAResults()$factorStats$factorSummary),digits=2),
                    options = list(pageLength = factorReactive()))
    })
    
    output$init.eigen <- DT::renderDataTable({
      DT::datatable(round_df(as.data.frame(PCAEigenResults()$eigen$initEigen),digits=2),
                    options = list(pageLength = ncol(dataSelectedNumeric())))
    })
    
    output$al.table <- DT::renderDataTable({
      DT::datatable(round_df(as.data.frame(itemStatistics()$total),digits=3),
                    options = list(pageLength = 1))
    })
    
    output$al.items <- DT::renderDataTable({
      DT::datatable(round_df(as.data.frame(itemStatistics()$item.stats),digits=3),
                    options = list(pageLength = ncol(dataSelectedNumeric())))
    })
    
    
    output$alpha.table <- DT::renderDataTable({
      if(length(PCAColumnMatchNames())>2){
        DT::datatable(round_df(currentSubscaleItemStatistics()$total,digits = 3))
      }
      else{
        DT::datatable(alpha2OrLess())
      }
    })
    
    output$scale.table <- DT::renderDataTable({
      if(length(PCAColumnMatchNames())>2){
        DT::datatable(round_df(currentSubscaleItemStatistics()$alpha.drop,digits = 3),
                      options = list(pageLength = 100))
      }
      else{
        DT::datatable(alpha2OrLess())
      }
    })
    
    output$item.table <- DT::renderDataTable({
      if(length(PCAColumnMatchNames())>2){
        DT::datatable(round_df(currentSubscaleItemStatistics()$item.stats,digits = 3),
                      options = list(pageLength = 100))
      }
      else{
        DT::datatable(alpha2OrLess())
      }
    })
    
    output$itemStatistics.slider<-renderUI({
      tagList(
        sliderInput('scale',
                    label='Subscale Index',
                    value = 1, min = 1, max = input$factor,step = 1)
      )
      
    })
    
    output$datanameo<-renderUI({tagList(selectInput(inputId = 'dataname',
                                                    label = 'Select a Dataframe',
                                                    choices=c(temp)))})
    
    output$factoro<-renderUI({tagList(sliderInput(inputId='factor',
                                                  label = 'Number of Factors',
                                                  value = 2, min = 1, max = ncol(dataSelectedNumeric()), step = 1))})
    
    output$loadingso<-renderUI({tagList(sliderInput(inputId='loadings',
                                                    label = 'Hide Loadings Below',
                                                    value = 0.59, min = 0, max = 1, step=.01))})
    
    output$rotationo<-renderUI({tagList(selectInput(inputId='rotation',
                                                    label = 'Rotation',
                                                    choices=c('varimax','oblimin')))})
    
    output$buttono<-renderUI({tagList(actionButton(inputId='button',
                                                   label = 'Run'))})
    
    output$column1o<-renderUI({tagList(selectInput(inputId = 'column1',
                                                   label = 'First Column is DV?',
                                                   choices=c('No','Yes')))})
    
    output$reg.IVo<-renderUI({tagList(checkboxGroupInput('reg.IV',
                                                         label='IVs',
                                                         choices=as.list(colnames(dataSelectedNumeric())),
                                                         inline=F
    ))})
    
    output$reg.DVo<-renderUI({tagList(radioButtons('reg.DV',
                                                   label='DV',
                                                   choices=as.list(colnames(dataSelectedNumeric())),
                                                   inline=F
    ))})
    
    output$alpha.names<-renderPrint({
      cat(paste(currentSubscaleItemNames()))
    })
    
    output$scree <- renderPlot({
      PCAEigenResults()$eigen$screePlot
    })
    
    #Text
    
    output$factor.text<- renderText({
      HTML(paste('<h4><B>Factor Loadings on Principal Component Analysis of ',
                 startReactive(),' Factors</B></h4>',sep=''))
    })
    
    output$eigen.text<- renderText({
      HTML(paste('<h4><B>Eigen Value Information for Principal Component Analysis of ',
                 startReactive(),' Factors</h4></B>',sep=''))
    })
    
    output$alpha.text<- renderText({
      HTML(paste("<h4><B>Cronbach's Alpha for Subscale ",scaleReactive(),'</B></h4>',sep=''))
    })
    
    output$scale.text<- renderText({
      HTML(paste("<h4><B>Alpha if Item Dropped for Subscale ",scaleReactive(),'</B></h4>',sep=''))
    })
    
    output$items.text<- renderText({
      HTML(paste("<h4><B>Item Statistics for Subscale ",scaleReactive(),'</B></h4>',sep=''))
    })
  }
  
  
#####################################################################################################
  #UI
  if(length(temp) == 0){
    ui <- fluidPage(
      includeCSS(base::system.file("css","styles.css", package = "EZFA")),
      
      headerPanel(
                 list(HTML('<a href="https://github.com/jwychor"><img src="https://i.ibb.co/n3r8vLx/Logo.png" alt="Logo" border="0" style="height: 100px; width: 100px;" /></a>','EZ Factor Analysis'))
      ),
      
      wellPanel(
        tags$h2("No dataframes found. Please take the following steps:"),
        tags$h4("1.) Close the application"),
        tags$h4("2.) Create or import a dataframe to your R environment"),
        tags$h4("3.) Run 'EZ_FA()' to start the dashboard again")
      )
    )
  }
  else{
    ui <- fluidPage(theme = shinytheme("darkly"),
      includeCSS(base::system.file("css","styles.css", package = "EZFA")),
                    
      headerPanel(
        list(HTML('<a href="https://github.com/jwychor"><img src="https://i.ibb.co/n3r8vLx/Logo.png" alt="Logo" border="0" style="height: 100px; width: 100px;" /></a>','EZ Factor Analysis'))
      ),
      
      #Inputs
      tabsetPanel(
        
        tabPanel("Factor",
                 tags$p(tags$h3(tags$strong("Choose a Dataframe, PCA Settings, and Press 'Run'"))),
                 
                 sidebarLayout(
                   sidebarPanel(
                     uiOutput('datanameo'),
                     
                     uiOutput('factoro'),
                     
                     uiOutput('loadingso'),
                     
                     uiOutput('rotationo'),
                     
                     uiOutput('column1o'),
                     
                     uiOutput('buttono')),
                   
                   #Outputs
                   mainPanel(
                     wellPanel(
                     tags$p(tags$h3(tags$strong("Scree Plot"))),
                     plotOutput(outputId = 'scree',width = '100%')
                     )
                   )
                 ),
                     wellPanel(
                       htmlOutput('factor.text'),
                       
                       fluidRow(
                         column(12,
                                div(DT::dataTableOutput(outputId = 'factor.table'),
                                    style = "font-size: 100%; width: 95%")
                         ),
                         
                         
                         tags$p(tags$h4(tags$strong('Component Statistics', id = "component-statistics"))),
                         
                         column(8,
                                div(DT::dataTableOutput(outputId = 'eigen.table'),
                                    style = "font-size: 100%; width: 95%")
                         ),
                         width = 12
                      )
                   ),
                   wellPanel(
                     tags$p(tags$h4(tags$strong('Eigenvalue Statistics'))),
                     
                     div(DT::dataTableOutput(outputId = 'init.eigen'),
                         style = 'font-size: 100%; width: 95%'),
                     
                     width = 12
                   ),
                     wellPanel(
                     htmlOutput('al.text'),
                     
                     tags$p(tags$h4(tags$strong("Scale Cronbach's Alpha"))),
                     DT::dataTableOutput(outputId = 'al.table'),
                     
                     tags$hr(),
                     
                     tags$p(tags$h4(tags$strong("Scale Item Statistics"))),
                     DT::dataTableOutput(outputId = 'al.items'),
                     
                     width = 12
                   )
        ),
        tabPanel("Subscales",
                 tags$p(tags$h3(tags$strong("Press 'Run' in the 'Factor' Tab to Analyze Subscales"))),
                 sidebarLayout(
                   sidebarPanel(
                     
                     uiOutput('itemStatistics.slider')
                   ),
                   
                   mainPanel(
                     wellPanel(
                     htmlOutput('alpha.text'),
                     
                     div(DT::dataTableOutput(outputId = 'alpha.table'),
                         style = "font-size: 90%; width: 45%")
                     
                    )
                   )
                 ),
                 wellPanel(
                   tags$p(tags$h4(tags$strong("Subscale Items List"))),
                   tags$head(tags$style(HTML("pre { overflow: auto; word-wrap: normal; }"))),
                   
                   div(textOutput(outputId='alpha.names'),
                       style = "font-size: 100%; width: 90%"),
                   tags$hr(),
                   
                   htmlOutput('scale.text'),
                   
                   div(DT::dataTableOutput(outputId = 'scale.table'),
                       style = "font-size: 100%; width: 90%"),
                   ),
                 wellPanel(
                   htmlOutput('items.text'),
                   
                   div(DT::dataTableOutput(outputId = 'item.table'),
                       style = "font-size: 100%; width: 90%")
                 )
        ),
        tabPanel('Regression',
                 tags$p(tags$h3(tags$strong("Choose Linear Regression Settings and Press 'Run'"))),
                 
                 fluidRow(
                   column(6,
                     fluidRow(
                     column(4,
                            wellPanel(
                              uiOutput('reg.IVo'
                              )
                            )
                     ),
                     column(4,
                            wellPanel(
                              uiOutput('reg.DVo'
                              )
                            )
                     ),
                     column(4,
                            wellPanel(
                              id = "Regression-Run",
                              
                              class = "sticky-top",
                              
                              class = "position-sticky",
                              
                              selectInput('reg.type',
                                          label='Type',
                                          choices=c('Standardized','Unstandardized')),
                              
                              selectInput('reg.inter',
                                          label='Include all Interactions?',
                                          choice=c('No','Yes')),
                              
                              actionButton('reg.button',
                                           label='Run')
                            )
                     ),
                     column(12,
                            
                       )
                     )
                   ),
                   column(6,
                          wellPanel(
                            tags$p(tags$h4(tags$strong("Linear Regression Table"))),
                            
                            div(DT::DTOutput(outputId='reg.table'),
                                style = "font-size: 100%; width: 100%"
                            ),
                            
                            div(htmlOutput('reg.table2'),
                                style = "font-size: 1.3em")
                          ), 
                          wellPanel(
                            
                            tags$p(tags$h4(tags$strong("Correlation Matrix (Non-significant Values at p > .05 are Left Blank)"))),
                            
                            div(plotOutput('cor.plot',width = '103%', height = '600px'),
                                style = "margin-left: -20px; min-height: 650px")
                          )
                     )
          )
        )
      )
    )
  }
  
  shinyApp(ui, server)
}

EZ_FA()
