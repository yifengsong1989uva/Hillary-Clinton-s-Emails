library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Visualization for the Topic Model (100 Topics)",align="center")),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      br(),
      br(),
      sliderInput("cluster",
                  "Cluster ID",
                  min = 1,
                  max = 50,
                  value = 1),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      textInput("Email","Email ID (1 - 7888)",value = 1),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      textInput("Topic","Topic ID (1 - 100)",value = 1),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      textInput("clusterno","Cluster ID (1 - 50)",value = 1),
      br(),
      br(),
      textInput("emailid","Search the cluster containing Email ID (1 - 7888)",value = 1),
      textOutput("cluster_for_email")     
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("cluster_topic_Plot"),
      plotOutput("Bar_plot_1"),
      plotOutput("Bar_plot_2"),
      h3("Emails contained in the cluster"),
      textOutput("email_in_cluster")
    )
  )
))