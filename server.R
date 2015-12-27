# server file

library(UsingR)
library(shiny)
library(repmis)
library(ggplot2)
library(ISLR)
library(caret)
library(car)

eddata <- source_DropboxData(file = "EducationDataforClass_F.csv", 
                             key = "v6vrgmmvj49iucb", sep = ",", header = TRUE)

eddata2 <- source_DropboxData(file = "EducationDataforClass_FF.csv", 
                              key = "1drvjmqeu3krt49", sep = ",", header = TRUE)

eddata2 <- eddata2[-1,]
cleanup <- c("V25","V26","V27","V28","V29","V30","V31","V32","V33","V34","V35","V36",
             "V37","V38","V39","V40","V41","V42","V43")
eddata2 <- eddata2[, !names(eddata2) %in% cleanup]

eddata3 <- as.data.frame(eddata2[,-1]) 

eddata2013 <- source_DropboxData(file = "EducationDataforClass_FF_2013.csv", 
                                 key = "h34hpi5i54ymr9h", sep = ",", header = TRUE)
cleanup <- c("V25","V26","V27","V28","V29","V30","V31","V32","V33","V34","V35","V36",
             "V37","V38","V39","V40","V41","V42","V43")
eddata2013 <- eddata2013[, !names(eddata2013) %in% cleanup]
eddata32013 <- as.data.frame(eddata2013[,-1])

testfit5 <<- lm(HSGrads ~ Instruction + Student_support + General_admin + Food_Srvcs,
               data = eddata3)

NoNAeddata3 <- na.omit(eddata3)
Train1 <- createDataPartition(y=NoNAeddata3$HSGrads, p=0.5, list = FALSE)
Training1 <- NoNAeddata3[Train1,]
Testing1 <- NoNAeddata3[-Train1,]

testfitML <<- train(HSGrads ~ . , method = "lm", preProcess = "pca", data = Training1)

preddata <- data.frame()
preddata1 <- eddata2

shinyServer( 
  function(input, output, session) {
    
    ed2 <- eddata2
    ed3 <- eddata3
    ed2013 <- eddata2013
    
    makeReactiveBinding("ed2")
    makeReactiveBinding("ed3")
    makeReactiveBinding("ed2013")
    
    e2<-reactive({
      d <- ed2
      d <- as.data.frame(d)
      d
    })
    e3<-reactive({
      f <- ed3
      f <- as.data.frame(f)
      f
    })
    e2013<-reactive({
      g <- ed2013
      g <- as.data.frame(g)
      g
    })

      #selectedrows <- reactive({ed2[which(ed2[,1] == input$SelectState)]})
      selectedmean <- reactive({subset(e2(), e2()$States == input$SelectState, select=c(Instruction))})
      #selectedmean <- reactive({selectedrows()$Instruction})
      meaninst <- reactive({round(mean(selectedmean()$Instruction) ,0)})
      output$avg_inst <- renderText({ meaninst()})
      
      selectedstud <- reactive({subset(e2(), e2()$States == input$SelectState, select=c(Student_support))})
      meanstudsrvcs <- reactive({round(mean(selectedstud()$Student_support),0)})
      output$avg_studsrvcs <- renderText({; meanstudsrvcs()})
      
      meangenadmin <- reactive({round(mean(e2()[,19][which(e2()$States == as.character(input$SelectState))]),0)
      })
      output$avg_genadmin <- renderText({meangenadmin()})
      
      meanfoodsrvcs <- reactive({round(mean(e2()[,22][which(e2()$States == as.character(input$SelectState))]),0)
      })
      output$avg_foodsrvcs <- renderText({meanfoodsrvcs()})
      
      output$StateHist <- renderPlot({
        pic <- subset(e2(), e2()$States == input$SelectState, select=c(FY, HSGrads))
       ggplot(data=pic, aes(x=FY, y=HSGrads, fill = FY, width=0.8)) + geom_bar(stat="identity", color = "black") + 
         geom_text(aes(label = HSGrads, y = HSGrads, vjust=1.5)) + guides(fill=FALSE) +
         theme(panel.background = element_blank()) + labs(x="Years", y="High School Graduates") + 
         scale_x_continuous(breaks=c(2010,2011,2012)) + scale_fill_continuous(low="#CC9933", high="#FFCC99")
       })
             
      output$actualHS <- renderText({e2013()[which(e2013()$States == input$SelectState), 2]})
      x <- reactive({as.numeric(input$i_Instruction * 0.01)}) 
      w <- reactive({as.numeric(input$i_Student_support * 0.01)})
      y <- reactive({as.numeric(input$i_General_admin * 0.01)})
      z <- reactive({as.numeric(input$i_Food_Srvcs * 0.01)}) 
      pred_inst <- reactive({x() * as.numeric(mean(e3()[,15][which(e2()$States == 
                                                           as.character(input$SelectState))]))})
      pred_stud <- reactive({x() * as.numeric(mean(e3()[,16][which(e2()$States == 
                                                                     as.character(input$SelectState))]))})
      pred_gen <- reactive({x() * as.numeric(mean(e3()[,18][which(e2()$States == 
                                                                     as.character(input$SelectState))]))})
      pred_food <- reactive({x() * as.numeric(mean(e3()[,21][which(e2()$States == 
                                                                     as.character(input$SelectState))]))})
      Pred_vector <- reactive ({ # function () {
        preddata1[which(preddata1$States == as.character(input$SelectState)),]
        preddata1 <- preddata1[2,]
        preddata1[,15] <- as.numeric(pred_inst())
        preddata1[,16] <- as.numeric(pred_stud())
        preddata1[,18] <- as.numeric(pred_gen())
        preddata1[,21] <- as.numeric(pred_food())
        preddata1 <- preddata1[, -1]
        preddata1  
       })
      
      basicpred <- reactive({predict(testfit5, Pred_vector(), type="response")})
      MLpred <- reactive({predict(testfitML, Pred_vector())})
      
      output$text2 <- renderText({ 
            if (round(as.numeric(basicpred()), 0) < (0.85 * e2013()[which(e2013()$States == input$SelectState), 2])) e2013()[which(e2013()$States == input$SelectState), 3]
            else round(as.numeric(basicpred()), 0) })
      # output$text2 <- renderText({ round(as.numeric(MLpred()), 0) })
#       output$text2 <- renderText({
#          if (round(as.numeric(MLpred()), 0) < e2013()[which(e2013()$States == input$SelectState), 2]) (e2013()[which(e2013()$States == input$SelectState), 3])
#          else round(as.numeric(MLpred()), 0) })

 })
