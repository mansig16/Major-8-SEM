library(shiny)



shinyServer
(
  function(input,output){

result<-eventReactive(input$Submit,
 {
#taking the user input and predicting through the model
  ha<-matrix(ncol = 14,c(as.numeric(input$age),as.numeric(input$sex),as.numeric(input$cp),as.numeric(input$trestbps),as.numeric(input$chol),as.numeric(input$fbs),as.numeric(input$restecg),as.numeric(input$thalach),as.numeric(input$exang),as.numeric(input$oldpeak),as.numeric(input$slope),as.numeric(input$smoker),as.numeric(input$diabetes),as.numeric(input$famhist)))
  colnames(ha)<-c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","smoker","diabetes","famhist")
 k<-as.data.frame(ha)
pred_system<-predict(model_rf,k,method="class",type="prob")
pred_result<-as.data.frame(pred_system)


pred_result$positive
})
output$text<-renderPrint({paste("The probrability of heart disease :",result())})

})
    

   






    

   
