# the ui file

library(shiny)
library(manipulate)
library(ggplot2)
library(ISLR)
library(repmis)

eddata <- source_DropboxData(file = "EducationDataforClass_F.csv", 
                             key = "v6vrgmmvj49iucb", sep = ",", header = TRUE)

eddata2 <- source_DropboxData(file = "EducationDataforClass_FF.csv", 
                              key = "1drvjmqeu3krt49", sep = ",", header = TRUE)

eddata2 <- eddata2[-1,]
cleanup <- c("V25","V26","V27","V28","V29","V30","V31","V32","V33","V34","V35","V36",
             "V37","V38","V39","V40","V41","V42","V43")
eddata2 <- eddata2[, !names(eddata2) %in% cleanup]

shinyUI(fluidPage(
  titlePanel("Different educational spending categories that impact and help to predict US high school graduation rates"),
  
  h5("Most education leaders agree that there is a stron relationship between instructional spend and graduation rates. But what about other spending categories? This application shows how four ancillary spending categories impact and predict 2013 high school graduation rates for different states.  By moving the sliders to see how a 5% to 20% increase in any of the four spending categories predicts the number of high school graduates for 2013. (Data was available only for 2010, 2011 and 2012.)"),
 
  img(src='us-map-with-states-hi.png', align = "center"),
  
  h4("How to use:"), 
  h5("1. Select the state that you would like to evaluate from the dropdown menu. The application is currently showing results for the default state of Alabama."),  
  h5("2. Move the sliders to adjust the four spending categories."),
  h5("3. Click the Submit button to see the impact of the changes on high school graduation rate."),
  h5("4. Compare the predicted graduation rate* with the projected graduation rate from the Western Interstate Commission for Higher Education (WICHE)."),
  h6("* Application does not consider key, non-spending factors that impact graduation rate like demographics, potential policy changes and changes in government funding."),
   hr(),

  selectInput("SelectState", 
              label = "Which state would you like to review?",
              choices = unique(eddata2$States), selected = "Alabama"
              ),
  
  fluidRow(
    column(3,
    
    h4('Average Spend on Instuction in $ Dollars:'),
    verbatimTextOutput("avg_inst"),
    h4('Average Spend on Student Support in $ Dollars:'),
    verbatimTextOutput("avg_studsrvcs"),
    h4('Average Spend on General Administration in $ Dollars:'),
    verbatimTextOutput("avg_genadmin"),
    h4('Average Spend on Food Services in $ Dollars:'),
    verbatimTextOutput("avg_foodsrvcs")
    ),
    
    column(4, offset = 1,
    
    plotOutput('StateHist')
    ),
    
    column(4,
           
    sliderInput('i_Instruction', 'What percentage increase would you like to apply to Instructional Spend?',
                value = 10, min = 5, max = 20, step = 5, post = "%"),
    sliderInput('i_Student_support', 'What percentage increase would you like to apply to Student Support Spend?',
                value = 10, min = 5, max = 20, step = 5, post = "%"),
    sliderInput('i_General_admin', 'What percentage increase would you like to apply to General Administration Spend?',
                value = 10, min = 5, max = 20, step = 5, post = "%"),
    sliderInput('i_Food_Srvcs', 'What percentage increase would you like to apply to Food Services Spend?',
                value = 10, min = 5, max = 20, step = 5, post = "%"),
    submitButton('Submit')
    
    ),
        
    h4('The number of high school graduates for 2013 would have been:'),
    verbatimTextOutput("text2"),
    h4("Compared to the 2013 number projected by Western Interstate Commission for Higher Education (WICHE):"),
    verbatimTextOutput("actualHS")
    )
))
