# loading required package
library("shiny")           # package for creating shiny app
library("visNetwork")      # network visualiation package
library("DT")              # Data table package for better table output

shinyUI(
  fluidPage(
    theme = "style.css",
    headerPanel("Ajinkya_Rane_Social_Network Analysis"),                   # header panel
    
    sidebarPanel(
      fileInput("File1","Please Select Links file to upload",              # First file input
                buttonLabel = "BROWSE", placeholder = "No File Selected"),
      tags$hr(),                                                           # line break
      fileInput("File2","Please Select Node file to upload",               # second file input
                buttonLabel = "BROWSE", placeholder = "No File Selected"),
      
      # tags$hr(),
      # selectInput("Options","Data Seperated By", c("Space"=" ", "Comma"=",","Dot"=".", 
      #                                              "Tab"="\t"), selected = "Space"),
      
      tags$hr(),                                                           # line break
      textInput("noOfConnections", "Please Enter Number of connections to display :"),  # Input for no of connections
      
      actionButton("Enter", "Show Me Connections"),                        # Button for output
      tags$hr(),
      fluidRow(column(3,tableOutput("fl")))                                # to show uploaded files
    ),
    
    mainPanel(                                                             # Main panel
      tabsetPanel(                                                         # set of tab panels
        tabPanel("Connections", visNetworkOutput("Connections")),          # Tab panel for showing input no of connections
        tabPanel("Sent", DT::dataTableOutput("sent")),                     # Tab panel for showing email sent by each employee
        tabPanel("Received", DT::dataTableOutput("Received")),             # Tab panel for showing email received by each employee
        tabPanel("2 Hop Neighbour", fluidRow(column(6, uiOutput("selectId"))),       # Tab panel for network showing 2 hop neighnours for top 10 connections
                 fluidRow(visNetworkOutput("Neighbour"))),
        tabPanel("Degree Centrality", fluidRow(column(6, uiOutput("selectId1"))),    # Tab panel for network showing top 10 degree centrality and their 2 hop neighbours
                 fluidRow(visNetworkOutput("dCentrality"))),
        tabPanel("Betweeness Centrality", fluidRow(column(6, uiOutput("selectId2"))),# Tab panel for network showing top 10 betweeness centrality and their 2 hop neighbours
                 fluidRow(visNetworkOutput("bCentrality"))),
        tabPanel("In Degree Centrality", fluidRow(column(6, uiOutput("selectId3"))), # Tab panel for network showing top 10 indegree centrality and their 2 hop neighbours
                 fluidRow(visNetworkOutput("inDCentrality"))),
        tabPanel("Emails at Department Level", DT::dataTableOutput("emailsDept")),   # Tab panel for showing no of email communication between different departments
        tabPanel("Department Communication", visNetworkOutput("DeptComm")),          # Tab panel for showing network graph between different departments.
        type = "pills"                                                               # changing tab panel type to pills
      )
    )
  )  
)