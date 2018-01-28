## Capstone: Coursera Data Science
## Final Project
## Seher F


suppressPackageStartupMessages(c(
    library(shinythemes),
    library(shiny),
    library(tm),
    library(stringr),
    library(markdown),
    library(stylo)))

# setting the working directory
#setwd("C:/Users/s.fazlioglu/Dropbox/Coursera/Course10_capstones/Shiny_apps")

source("./NextWordPredict.R")

bg <- readRDS("./bigram.RData"); tg <- readRDS("./trigram.RData"); qd <- readRDS("./quadgram.RData")

names(bg)[names(bg) == 'word1'] <- 'w1'; names(bg)[names(bg) == 'word2'] <- 'w2';
names(tg)[names(tg) == 'word1'] <- 'w1'; names(tg)[names(tg) == 'word2'] <- 'w2'; names(tg)[names(tg) == 'word3'] <- 'w3';
names(qd)[names(qd) == 'word1'] <- 'w1'; names(qd)[names(qd) == 'word2'] <- 'w2'; names(qd)[names(qd) == 'word3'] <- 'w3'
names(qd)[names(qd) == 'word4'] <- 'w4';
message <- "" ## cleaning message


ui<-navbarPage("Coursera Data Science Capstone", 
                   
                   theme = shinytheme("cerulean"),
                   
                   ############################### ~~~~~~~~1~~~~~~~~ ##############################  
                   ## Tab 1 - Prediction
                   
                   tabPanel("Next Word Prediction",
                            
                            
                            fluidRow(
                                
                                column(3),
                                column(6,
                                       tags$div(textInput("text", 
                                                          label = h3("Please enter the text here:"),
                                                          value = ),
                                                tags$span(style="color:grey",("Only English words are supported.")),
                                                br(),
                                                tags$hr(),
                                                h4("The predicted next word:"),
                                                tags$span(style="color:darkred",
                                                          tags$strong(tags$h3(textOutput("predictedWord")))),
                                                br(),
                                                tags$hr(),
                                                h4("What you have entered:"),
                                                tags$em(tags$h4(textOutput("enteredWords"))),
                                                align="center")
                                ),
                                column(3)
                            )
                   ))
                   
                  
                   
######  

server<-function(input, output) {
    
    wordPrediction <- reactive({
        text <- input$text
        textInput <- cleanInput(text)
        wordCount <- length(textInput)
        wordPrediction <- nextWordPrediction(wordCount,textInput)})
    
    output$predictedWord <- renderPrint(wordPrediction())
    output$enteredWords <- renderText({ input$text }, quoted = FALSE)
}


# Run the application 
shinyApp(ui = ui, server = server)

