library(shiny)
library(ggplot2)
library(scatterplot3d)
library(rgl)
library(plotly)

ui <- navbarPage(title = "Iris dataset",
                 tabPanel(title = "Overview",
                          # The grey input part
                          wellPanel(
                            fluidRow(
                              column(3, sliderInput("num_samples",label="Data samples",value=30,min=0,max=150))
                            )
                          ),
                          dataTableOutput("overview_tab")
                 ),
                 
                 navbarMenu(title = "Plots",
                           
                             tabPanel(title = "2D Scatter plot",
                                     sidebarPanel(
                                       # X-axis selection
                                       selectInput("x2", label = "X-axis", 
                                                   choices = list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                                                   selected ="Sepal.Length"),
                                       # Y-axis selection
                                       selectInput("y2", label = "Y-axis", 
                                                   choices = list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                                                   selected ="Sepal.Width")
                                     ),
                                      # 2D scatter plot
                                     mainPanel(
                                       plotlyOutput("scatterplot2")
                                     )
                            ),
                            
                            tabPanel(title = "3D Scatter plot",
                                     sidebarPanel(
                                       # X-axis selection
                                       selectInput("x3", label = "X-axis", 
                                                   choices = list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                                                   selected ="Sepal.Length"),
                                       # Y-axis selection
                                       selectInput("y3", label = "Y-axis", 
                                                   choices = list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                                                   selected ="Sepal.Width"),
                                       # Z-axis selection
                                       selectInput("z3", label = "Z-axis", 
                                                   choices = list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                                                   selected ="Sepal.Width")
                                     ),
                                     # 3D scatter plot
                                     mainPanel(
                                       plotlyOutput("scatterplot3")
                                     )
                                     
                                  
                            ),
                            tabPanel(title = "K-means clustering",
                                     sidebarPanel(
                                       # X variable selection
                                       selectInput("x", label = "X-axis", 
                                                   choices = list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                                                   selected ="Sepal.Length"),
                                       # Y variable selection
                                       selectInput("y", label = "Y-axis", 
                                                   choices = list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                                                   selected ="Sepal.Width"),
                                       # Cluster count selection
                                       numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
                                     ),
                                     # k-means plot
                                     mainPanel(
                                       plotlyOutput("kmeans")
                                     ) 
                            ),
                            tabPanel(title = "Histograms",
                                     sidebarPanel(
                                       selectInput("h.variable", label = "Choose the variable to plot", 
                                                   choices = list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                                                   selected ="Sepal.Length")
                                     ),
                                     # histogram
                                     mainPanel(
                                       plotlyOutput("hist")
                                     ) 
                            ),
                            tabPanel(title = "Boxplot",
                                     
                                       plotOutput("boxplot")
                                     
                            ),
                            tabPanel(title = "Density plot",
                                     
                                     plotlyOutput("densityoutput")
                                     
                            )
                 )
                 
              )

server <- function(input, output){
  # Overview data table
  output$overview_tab <- renderDataTable({
    title <- "Iris data frame"
    head(iris,n=input$num_samples)
  })
  # 2D scatter plot
  output$scatterplot2 <- renderPlotly({
    fig <- plot_ly(iris,x=iris[,input$x2],y=iris[,input$y2],color=as.factor(iris$Species), type = 'scatter', mode='markers')
    fig <- fig %>% layout(xaxis = list(title = input$x2),
                                       yaxis = list(title = input$y2))
  })
  # 3D scatter plot
  output$scatterplot3 <- renderPlotly({
    fig <- plot_ly(iris,x=iris[,input$x3],y=iris[,input$y3],z=iris[,input$z3],color=as.factor(iris$Species),type = 'scatter3d', mode='markers')
    fig <- fig %>% layout(scene = list(xaxis = list(title = input$x3),
                                       yaxis = list(title = input$y3),
                                       zaxis = list(title = input$z3)))
  })
  # selected (x,y) variables for k means clustering
  selectedData <- reactive({
    iris[, c(input$x,input$y)]
  })
  clusters <- reactive({
    kmeans(selectedData(),input$clusters)
  })
  # k-means plot
  output$kmeans <- renderPlotly({
    fig <- plot_ly(iris,x=selectedData()[,1],y=selectedData()[,2],color=clusters()$cluster, type = 'scatter',mode='markers')
    fig <- fig %>% layout(xaxis = list(title = input$x),
                          yaxis = list(title = input$y))
    
  })
  # Histograms
  output$hist <- renderPlotly({
    #title <- paste("Histogram of",input$h.variable) 
    #hist(iris[,input$h.variable], main=title,xlab=paste("iris",input$h.variable),ylab="Data frequency")
    # fig <- plot_ly(iris,x=iris[,input$h.variable],type = 'histogram')
    # fig <- fig %>% layout(xaxis = list(title = input$h.variable),
    #                       yaxis = list(title = "Data frequency"))
    fig <- ggplot(iris,aes(x=iris[,input$h.variable],fill=Species)) + geom_histogram()
    fig <- ggplotly(fig)
  })
  
  #Boxplot
 #df <- iris[,-5]
 #dfmelt <- melt(df, measure.vars=1:4)
  output$boxplot <- renderPlot({
    boxplot(iris[,-5])
    # fig <- ggplot(dfmelt) + geom_boxplot(aes(x=x,fill=iris$Species))
    # facet_grid(iris$Species)
  })
  
  # density plot
  output$densityoutput <- renderPlotly({
    fig <- plot_ly(x=iris[,"Sepal.Length"],y=iris[,"Sepal.Width"]) %>% 
      add_histogram2dcontour(showscale=TRUE, ncontours=20, colorscale='Viridis', 
                             contours = list(coloring='heatmap')) %>%
      add_markers(x = iris[,"Sepal.Length"], y = iris[,"Sepal.Width"], marker=list(size=3))
   fig <- fig %>% layout(
     title = "Iris",
     xaxis = list(title = "Sepal.Length"),
     yaxis = list(title = "Sepal.Width")
   )
   fig
    
  })
  
}

shinyApp(ui = ui, server = server)
