####Data Entry#####
#replace 'name' with the name of a dataframe in your global environment
dat <- os #<---- Replace
#Then press cntrl + a, then cntrl + enter. The app should initialize.

####Packages####
#Use {install.packages('package_name')} on any required
#packages that have not yet been installed
require('dplyr')
require('DT')
require('jmv')
require('shiny')
require('shinydashboard')
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

####Application#####
r2<-pca(dat,screePlot=T,nFactorMethod = 'eigen')


ui <- fluidPage(
  
  titlePanel('EZ Factor Analysis'),
  
    #Inputs
  sidebarLayout(
    sidebarPanel(
    sliderInput(inputId='factor',
      label = 'Number of Factors',
      value = 2, min = 2, max = ncol(dat), step = 1),
    
    sliderInput(inputId='loadings',
      label = 'Hide Eigenvalues Below',
      value = 0.59, min = 0, max = 1, step=.01),
    
    selectInput(inputId='rotation',
      label = 'Rotation',
      choices=c('varimax','oblimin')),
    
    actionButton(inputId='button',
      label = 'Run')),
  
  #Outputs
  mainPanel(
    plotOutput(outputId = 'scree',width = '100%')
  )),
  div(DT::dataTableOutput(outputId = 'factor.table'),
      style = "font-size: 100%; width: 45%"),
  
  div(DT::dataTableOutput(outputId = 'eigen.table'),
      style = "font-size: 100%; width: 95%")
)
  

server <- function(input, output, session) {
  
  e1<-reactive({input$factor})
  e2<-reactive({input$loadings})
  e3<-reactive({input$rotation})
  
  r1<-eventReactive(input$button,{pca(dat,nFactorMethod='fixed',nFactors = e1(),hideLoadings=e2(),
                screePlot=F,sortLoadings = T,eigen = T,rotation=e3(),
                factorSummary = T)})
  
  output$factor.table <- DT::renderDataTable({
    DT::datatable(round_df(as.data.frame(r1()$loadings)[,-1],digits=3),
          options = list(pageLength = ncol(dat),
                         lengthMenu = c(5,10,15,round(1/2*ncol(dat),0),ncol(dat))))
  })
  
  output$eigen.table <- DT::renderDataTable({
    DT::datatable(round_df(as.data.frame(r1()$factorStats$factorSummary)[,-1],digits=2),
        options = list(pageLength = e1()))
  })
  
  output$scree <- renderPlot({
    r2$eigen
  })
}

shinyApp(ui, server)
