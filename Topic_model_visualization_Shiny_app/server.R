# reset the file path to the same folder as the shiny project
# setwd("~/Box Sync/SYS 6018/Final Project/Topic model visualization")

library(stringr)
load("email_cluster.RData")
load("document_topic_probabilities.RData")
load("topic_word_matrix.RData")
load("email_cluster_1.RData")

library(shiny)

shinyServer(function(input, output) {
  
  output$email_in_cluster <- renderText({
    cluster_no <- as.numeric(input$clusterno)
    emails.in.cluster <- as.character(email.cluster.1[email.cluster.1$cluster==cluster_no,]$email_index)
    str_c(emails.in.cluster,collapse=", ")
    })
  
  output$cluster_topic_Plot <- renderPlot({
    cluster_no <- input$cluster
    first_second <- email.cluster[email.cluster$cluster==cluster_no,][,c("first","second")]
    par(mar=c(5.1,6.6,4.6,2.1))
    plot(first_second,xlab = "most likely topic",ylab="second most likely topic",
         main = paste0("Cluster ", as.character(cluster_no)," : 2nd most likely topic vs 1st most likely topic"),col="red",cex=1)
  })
  
  output$Bar_plot_1 <- renderPlot({
    email_no <- as.numeric(input$Email)
    indx <- order(document.topic.probabilities[email_no,],decreasing = T)[5:1]
    probs <- document.topic.probabilities[email_no,indx]
    names(probs) <- as.character(indx)
    par(mar=c(5.1,6.6,4.6,2.1))
    barplot(probs,horiz = T,xlab="topic probabilities",las=2,ylab="Topic",
            main=paste0("Bar Plot for Probabilities of 5 Most Likely Topics in Email ",as.character(email_no)),
            col="green",las=2)
  })
  
  output$Bar_plot_2 <- renderPlot({
    topic_no <- as.numeric(input$Topic)
    indx <- order(topic_word_matrix[topic_no,],decreasing = T)[20:1]
    probs <- topic_word_matrix[topic_no,indx]
    names(probs) <- as.character(colnames(topic_word_matrix)[indx])
    par(mar=c(5.1,6.6,4.6,2.1))
    barplot(probs,horiz = T,space =c(0.7,1.7),xlab="word probabilities",ylab=" ",
            main=paste0("Bar Plot for Probabilities of Top 20 Words in Topic ",as.character(topic_no)),
            col="blue",las=2)
  })
  
  output$cluster_for_email <- renderText({
    email_id <- as.numeric(input$emailid)
    row_indx <- which(email.cluster.1$email_index==email_id)
    cluster_id <- email.cluster.1$cluster[row_indx]
    paste("Cluster: ",as.character(cluster_id))
  })
  
})