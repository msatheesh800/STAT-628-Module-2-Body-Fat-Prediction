library(shiny)
library(ggplot2)
library(rsconnect)

name <- file.path("cleanedbodyfat.csv")
df<- read.csv(name)
df$group = ifelse(df$BODYFAT < 5, "0-5", ifelse(df$BODYFAT < 17, "6-17", ifelse(df$BODYFAT < 24, "18-24", ">25")))
ui = fluidPage(titlePanel("Bodyfat Prediction"), 
  fluidRow(column(6, wellPanel(sliderInput(inputId='weight',label='Weight (Lb):',value='0',min=10,max=400,step=2))),
  column(6, wellPanel(sliderInput(inputId='abdomen',label='Abdomen circumference (cm):',value='0',min=10,max=180,step=1)))),
  h6("Please write to msatheesh@wisc.edu for any further queries"), submitButton("Predict"), verbatimTextOutput("result"), 
  verbatimTextOutput("suggestion"), plotOutput("hist"))

#Model Calculation
fatfunction <- function(x,y){
  return (round(x-0.1*y-44,1))
}
histogram_func <- function(x) {
  ggplot(df, aes(BODYFAT, fill = group)) + geom_histogram(binwidth = 0.5) +
  scale_fill_manual(values = c("0-5" = "red", "6-17" = "green", "18-24"= "yellow", ">25" = "red")) +xlab("Bodyfat %") +
  ylab("Count") + geom_vline(xintercept =x, size=1.5)+ theme_minimal()
}

server = function(input, output,session) {
  output$result <- renderText({
  fat= fatfunction(input$abdomen, input$weight)
  if (input$weight==10 | input$abdomen==10)
    paste0("Welcome to the bodyfat prediction app!")
  else if (fat<0)
    paste0("Your body fat percentage is ",0)
  else
    paste0("Your body fat percentage is ",fat)
  }) 
  output$suggestion <- renderText({
  fat= fatfunction(input$abdomen, input$weight)
  if (input$weight==10 | input$abdomen==10)
    paste0("Please fill in your information.")
  else if(fat< -5)
    paste0("Please recheck your input!")
  else if(fat<5)
    paste0("You are underweight.")
  else if(fat<17)
    paste0("You are fit")
  else if(fat<24)
    paste0("You are healthy.")
  else if(fat<28)
    paste0("You are overweight.")
  else
    paste0("You are obese.")
  })
  output$hist <- renderPlot({
    fat= fatfunction(input$abdomen, input$weight)
    histogram_func(ifelse(fat<0, 0,fat))
  })
}
shinyApp(ui, server)