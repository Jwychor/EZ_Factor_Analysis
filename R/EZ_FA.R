####Options####
#Remove Scientific Notation
options(scipen=999)
  
####Packages####
load.lib<-c('rlang','dplyr','DT','psych','jmvcore','shiny','shinydashboard','ggplot2','ggcorrplot')

install.lib<-load.lib[!load.lib %in% installed.packages()];if(exists("install.lib")){for(lib in install.lib) install.packages(lib,dependencies=TRUE,verbose=F,quiet=T);install.lib;}

sapply(load.lib,library,character=TRUE)

#Custom Function Used Later
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}

temp<-names(Filter(function(x) is(x, "data.frame"), mget(ls(),envir=global_env())))

####Application#####
#Server
server <- function(input, output, session) {

  #Reactives
  b1<-reactive({as.data.frame(matrix(c('Scales Need to have >2 Items for Scale Statistics')))})
  dat<-reactive({if(exists(input$dataname) && length(input$dataname) !=0) get(as.character(input$dataname),env=global_env())})
  dats<-reactive({dat() %>%
      mutate_all(funs(scale))})

  datt<-reactive({dat()[,-1]})
  datn<-reactive({select_if(dat(),is.numeric)})
  datc<-reactive({round(cor(datn(),use='pairwise.complete.obs'),2)})
  pmat<-reactive({round(cor_pmat(datn()),2)})

  e1<-reactive({input$factor})
  e2<-reactive({input$loadings})
  e3<-reactive({input$rotation})
  e4<-eventReactive(input$button,{input$factor})
  e5<-reactive({input$scale})
  e6<-reactive({as.character(input$column1)})
  e7<-reactive({as.character(input$reg.inter)})

  a<-reactive({psych::alpha(if(e6()=='No'){dat()} else datt())})
  a1<-reactive({input$scale})

  r1<-eventReactive(input$button,{jmv::pca(data=if(e6()=='No'){dat()} else datt(),nFactorMethod='fixed',nFactors = e1(),hideLoadings=e2(),
                                           screePlot=F,sortLoadings = T,eigen = T,rotation=e3(),
                                           factorSummary = T)})
  r2<-reactive({jmv::pca(if(e6()=='No'){dat()} else datt(),screePlot=T,nFactorMethod = 'eigen',eigen=T)})

  a2<-reactive({as.data.frame(r1()$loadings)})

  m1<-reactive({a2()[,c(1,(a1()+1))]})
  n1<-reactive({subset(m1(),!is.na(m1()[,2]))})
  m2<-reactive({n1()[,1]})

  alp<-reactive({
    psych::alpha(
      dat()[,colnames(dat()) %in% m2()])
  })

  alpr<-reactive({paste("'",as.character(colnames(dat())[colnames(dat()) %in% m2()]),"'",sep="",collapse=",")})

  l1<-reactive({as.character(input$reg.IV)})
  l2<-reactive({as.character(input$reg.DV)})
  l3<-eventReactive(input$reg.button,{
    u<-paste(l1(),collapse=if(e7()=='No'){'+'} else{'*'},sep='')
    u1<-vector(mode='character',length=length(l1()))
    if(input$reg.type=='Standardized'){
      u1<-paste("lm(",l2(),'~',u,",data=dats())")
    }
    else{
      u1<-paste("lm(",l2(),'~',u,",data=dat())")
    }
    u2<-eval(parse(text=u1))
    u3<-summary(u2)
  })

  #Outputs
  output$cor.plot<-renderPlot({ggcorrplot(datc(),outline.color='black',type='lower',
                                          lab=T,p.mat=pmat(),ggtheme=ggplot2::theme_bw(),
                                          lab_col = 'black',sig.level = .05,insig='blank')})

  output$reg.table<-DT::renderDataTable(
    DT::datatable(round_df(as.data.frame(l3()$coefficients),digits = 3),
                  options = list(pageLength = 100)))

  output$reg.table2<-renderText({HTML(paste("R-Squared = <u>",round(l3()$r.squared,3),
                                            "</u><br>F(",round(l3()$fstatistic[2],3),',',round(l3()$fstatistic[3],3),') = <u>',round(l3()$fstatistic[1],3),'</u><br>',
                                            "<br><b>Note:</b> p-values coming soon!</br>",sep=''))})

  output$factor.table <- DT::renderDataTable({
    DT::datatable(round_df(as.data.frame(r1()$loadings),digits=3),
                  options = list(pageLength = ncol(dat()),
                                 lengthMenu = c(5,10,15,round(1/2*ncol(dat()),0),ncol(dat()))),rownames=F)
  })

  output$eigen.table <- DT::renderDataTable({
    DT::datatable(round_df(as.data.frame(r1()$factorStats$factorSummary),digits=2),
                  options = list(pageLength = e1()))
  })

  output$init.eigen <- DT::renderDataTable({
    DT::datatable(round_df(as.data.frame(r2()$eigen$initEigen),digits=2),
                  options = list(pageLength = ncol(dat())))
  })

  output$al.table <- DT::renderDataTable({
    DT::datatable(round_df(as.data.frame(a()$total),digits=3),
                  options = list(pageLength = 1))
  })

  output$al.items <- DT::renderDataTable({
    DT::datatable(round_df(as.data.frame(a()$item.stats),digits=3),
                  options = list(pageLength = ncol(dat())))
  })


  output$alpha.table <- DT::renderDataTable({
    if(length(m2())>2){
      DT::datatable(round_df(alp()$total,digits = 3))
    }
    else{
      DT::datatable(b1())
    }
  })

  output$scale.table <- DT::renderDataTable({
    if(length(m2())>2){
      DT::datatable(round_df(alp()$alpha.drop,digits = 3),
                    options = list(pageLength = 100))
    }
    else{
      DT::datatable(b1())
    }
  })

  output$item.table <- DT::renderDataTable({
    if(length(m2())>2){
      DT::datatable(round_df(alp()$item.stats,digits = 3),
                    options = list(pageLength = 100))
    }
    else{
      DT::datatable(b1())
    }
  })

  output$a.slider<-renderUI({
    tagList(
      sliderInput('scale',
                  label='Subscale Number',
                  value = 1, min = 1, max = input$factor,step = 1)
    )

  })

  output$datanameo<-renderUI({tagList(selectInput(inputId = 'dataname',
                                                  label = 'Select a Dataframe',
                                                  choices=c(temp)))})

  output$factoro<-renderUI({tagList(sliderInput(inputId='factor',
                                                label = 'Number of Factors',
                                                value = 2, min = 1, max = ncol(dat()), step = 1))})

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
                                                       choices=as.list(colnames(dat())),
                                                       inline=F
  ))})

  output$reg.DVo<-renderUI({tagList(radioButtons('reg.DV',
                                                 label='DV',
                                                 choices=as.list(colnames(dat())),
                                                 inline=F
  ))})

  output$alpha.names<-renderPrint({
    cat(paste(alpr()))
  })

  output$scree <- renderPlot({
    r2()$eigen$screePlot
  })

  #Text

  output$factor.text<- renderText({
    HTML(paste('<h4><B>Factor Loadings on Principal Component Analysis of ',
               e4(),' Factors</B></h4>',sep=''))
  })

  output$eigen.text<- renderText({
    HTML(paste('<h4><B>Eigen Value Information for Principal Component Analysis of ',
               e4(),' Factors</h4></B>',sep=''))
  })

  output$alpha.text<- renderText({
    HTML(paste("<h4><B>Cronbach's Alpha Criteria for Subscale ",e5(),'</B></h4>',sep=''))
  })

  output$scale.text<- renderText({
    HTML(paste("<h4><B>Alpha if Item Dropped for Subscale ",e5(),'</B></h4>',sep=''))
  })

  output$items.text<- renderText({
    HTML(paste("<h4><B>Item Statistics for Subscale ",e5(),'</B></h4>',sep=''))
  })
}
#UI
ui <- fluidPage(

  titlePanel('EZ Factor Analysis'),

  #Inputs
  tabsetPanel(

    tabPanel("Factor",
             tags$p(tags$h3(tags$strong("Choose a Dataframe, PCA Settings, and Press 'Run' to get Started"))),

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
                 tags$p(tags$h4(tags$strong("Scree Plot"))),

                 plotOutput(outputId = 'scree',width = '100%')
               )
             ),
             wellPanel(
               htmlOutput('factor.text'),

               div(DT::dataTableOutput(outputId = 'factor.table'),
                   style = "font-size: 100%; width: 45%")
             ),
             wellPanel(
               div(DT::dataTableOutput(outputId = 'eigen.table'),
                   style = "font-size: 100%; width: 95%")),

             sidebarLayout(
               sidebarPanel(
                 htmlOutput('eigen.text'),

                 tags$p(tags$h4(tags$strong('Initial Eigenvalue Table'))),

                 div(DT::dataTableOutput(outputId = 'init.eigen'),
                     style = 'font-size: 100%; width: 95%')
               ),

               mainPanel(
                 htmlOutput('al.text'),

                 tags$p(tags$h4(tags$strong("Scale Cronbach's Alpha"))),
                 DT::dataTableOutput(outputId = 'al.table'),

                 tags$p(tags$h4(tags$strong("Scale Item Statistics"))),
                 DT::dataTableOutput(outputId = 'al.items')

               )
             )
    ),
    tabPanel("Subscales",
             tags$p(tags$h3(tags$strong("Press 'Run' in the 'Factor' Tab to Analyze Subscales"))),
             sidebarLayout(
               sidebarPanel(

                 uiOutput('a.slider')
               ),

               mainPanel(
                 htmlOutput('alpha.text'),

                 div(DT::dataTableOutput(outputId = 'alpha.table'),
                     style = "font-size: 90%; width: 45%")

               )
             ),
             wellPanel(
               tags$p(tags$h4(tags$strong("Subscale Items List"))),
               tags$head(tags$style(HTML("pre { overflow: auto; word-wrap: normal; }"))),
               div(textOutput(outputId='alpha.names'),
                   style = "font-size: 100%; width: 90%"),

               htmlOutput('scale.text'),

               div(DT::dataTableOutput(outputId = 'scale.table'),
                   style = "font-size: 100%; width: 90%"),

               wellPanel(
                 htmlOutput('items.text'),

                 div(DT::dataTableOutput(outputId = 'item.table'),
                     style = "font-size: 100%; width: 90%")
               )
             )
    ),
    tabPanel('Regression',
             tags$p(tags$h3(tags$strong("Choose Linear Regression Settings and Press 'Run' to get Started"))),

             fluidRow(
               column(2,
                      wellPanel(
                        uiOutput('reg.IVo'
                        )
                      )
               ),
               column(2,
                      wellPanel(
                        uiOutput('reg.DVo'
                        )
                      )
               ),
               column(2,
                      wellPanel(
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
               column(6,
                      wellPanel(
                        tags$p(tags$h4(tags$strong("Linear Regression Table"))),

                        div(DT::DTOutput(outputId='reg.table'),
                            style = "font-size: 100%; width: 100%"
                        ),
                        htmlOutput('reg.table2'),

                        tags$p(tags$h4(tags$strong("Correlation Matrix (Non-significant Values at p > .05 are Left Blank)"))),

                        plotOutput('cor.plot',width='650px',height='800px')
                      )
               )
             )
    )
  )
)

EZ_FA<-function(){
  shinyApp(ui, server)
}
