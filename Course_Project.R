
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
#
# Created by Teo Chee Seong
# 21 July 2020
# Module 9: Developing Data Products
# Week 4 Course Project

library(shiny)

ui <- fluidPage(
   titlePanel("The Effect of Different Cylinders on MPG with Horsepower"),
   
   sidebarLayout(
      sidebarPanel(
            radioButtons("radio","Choose One",
                         choices= list("All" = "All",
                                       "4" = "4", 
                                       "6" = "6", 
                                       "8" = "8",
                                       "4 & 6" = "4 & 6",
                                       "4 & 8" = "4 & 8",
                                       "6 & 8" = "6 & 8"),
                         selected="All"),
                         
            checkboxInput('lines',"Include Regression Line",value=TRUE),
            checkboxInput('lines2',"Original Regression Line",value=TRUE)
      ),
      
      mainPanel(
         plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
      
      
      output$plot <- renderPlot({
            dist <- switch(input$radio,
                           "All" = c(4,6,8),
                           "4" = 4,
                           "6" = 6,
                           "8" = 8,
                           "4 & 6" = c(4,6),
                           "4 & 8" = c(4,8),
                           "6 & 8" = c(6,8))
            new_mtcars <- mtcars[mtcars$cyl %in% dist,]
            plot(new_mtcars$hp,new_mtcars$mpg, col = new_mtcars$cyl,
            xlab="Miles per Gallon",ylab="Horsepower",
            bty="n",pch=16,xlim=c(50,350),ylim=c(10,35))
      
      fit <- lm(mpg~hp,data=new_mtcars)
      fit2 <-lm(mpg~hp,data=mtcars)
      if(input$lines){abline(fit,col="blue",lwd=2)}
      if(input$lines2){abline(fit2,col="red",lwd=2)}
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

