# loading required libraries
library("magrittr")         # package for pipe operator
library("dplyr")            # package for data manipulation
library("igraph")           # package for calculating network parameters

shinyServer(function(input, output){
# read data from first file
    fileData1 <- reactive({
    if(is.null(input$File1))
      return()
    read.csv(input$File1$datapath, header = F, sep = " ")
  })
    
# read data from second file    
  fileData2 <- reactive({
    if(is.null(input$File2))
      return()
    read.csv(input$File2$datapath, header = F, sep = " ")
  })

# showing uploaded files in tabel format   
  output$fl <- renderTable({                        # use of table functions from DT library
    if(is.null(input$File1) && is.null(input$File2))
      return()
    fpath1 <- input$File1
    fpath2 <- input$File2
    fpath <- data.frame(rbind(fpath1, fpath2))
    fpath[,1:3]
  })

# Computation for showing input no of connection and render network  
    output$Connections <- renderVisNetwork({
    if(is.null(input$File1) || is.null(input$File2) || input$Enter == 0)
        return()
    input$Enter
    e <- fileData1()
    f <- fileData2()
    colnames(e) <- c('from','to')
    colnames(f) <- c('id', 'group')
    no <- isolate({input$noOfConnections})        # isolating input from action

    no <- as.numeric(no)                          # converting to numeric
    link <- sample_n(e,size = no)                 # taking n samples from links
    n <- unique(unlist(link))
    node <- subset(f,id %in% n)
    node <- mutate(node, label = id)

    visNetwork(node, link, height = "500px", width = "500px")%>%     # Network output
      visEdges(smooth = FALSE) %>%
      visNodes(shape = "circle",size = 1)   
  })

# render table for outgoing connections      
  output$sent <- DT::renderDataTable({
    if (is.null(fileData1()))
      return()
    emailSent <- data.frame(table(fileData1()[,1]))
    colnames(emailSent) <- c("Employee","sentEmails")
    emailSent[order(-emailSent$sentEmails),]
    DT::datatable(emailSent[order(-emailSent$sentEmails),],rownames = FALSE,
                  class = 'cell-border stripe compact',
                  options = list(lengthMenu = c(15, 25, 50, 75, 100), pageLength = 15))
  })

# render table for incoming connections    
  output$Received <- DT::renderDataTable({
    if (is.null(fileData1()))
      return()
    emailReceived <- data.frame(table(fileData1()[,2]))
    colnames(emailReceived) <- c("Employee","receivedEmails")
    DT::datatable(emailReceived[order(-emailReceived$receivedEmails),],rownames = FALSE,
                  class = 'cell-border stripe compact',
                  options = list(lengthMenu = c(15, 25, 50, 75, 100), pageLength = 15))
  })

# computation for getting top nodes from sent and received tab    
  top_nodes <- reactive({
    if(is.null(fileData1()) || is.null(fileData2)) 
      return()
    link <- fileData1()
    emailSent <- data.frame(table(link[,1]))
    colnames(emailSent) <- c("Employee","sentEmails")
    emailSent <- emailSent[order(-emailSent$sentEmails),]
    
    emailReceived <- data.frame(table(link[,2]))
    colnames(emailReceived) <- c("Employee","receivedEmails")
    emailReceived <- emailReceived[order(-emailReceived$receivedEmails),]
    
    top_node <- data.frame(emailSent[1:10,1],emailReceived[1:10,1])
    sort(unique(unlist(top_node)))
  })

# select input for neighbour tab   
  output$selectId <- renderUI({
    if(is.null(fileData1()) || is.null(fileData2)) 
      return()
    topNodes <- top_nodes()
    selectInput("topNodes",label = "Please Select ID", choices = topNodes,
                width = "300px")
  })

# render network output for neighbout tab    
  output$Neighbour <- renderVisNetwork({
    if(is.null(fileData1()) || is.null(fileData2()))
      return()
    link <- fileData1()
    node <- fileData2()

    colnames(link) <- c('from','to')
    colnames(node) <- c('id','group')

    selectedNode <- input$topNodes                  #####################################################
    link1 <- subset(link, link[,1] == selectedNode) ##                                                 ##
    l <- unique(unlist(link1))                      ##    Manipulation of link and node data frame     ##
    link <- subset(link, link[,1] %in% l)           ##    for visNetwork function                      ##
    l <- unique(unlist(link))                       ##                                                 ##
    node <- subset(node, node[,1] %in% l)           #####################################################

    node <- mutate(node, label = id)
    visNetwork(node, link, height = "100%", width = "100%") %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(smooth = FALSE) %>%
      visNodes(shape = "circle",size = 1)

  })

# calculating alldegree,indegree and betweeness centrality    
  centrality <- reactive({
    link <- fileData1()
    node <- fileData2()
    colnames(link) <- c('from','to')
    colnames(node) <- c('id','group')
    g <- graph.data.frame(link, directed = T)
    
    V(g)$alldegree <- centr_degree(g,mode = 'all')$res                                    ###################
    deg_allcentrality <- data.frame(Node = node[,1], degAllCentrality = V(g)$alldegree)   ##  All Degree   ##
    deg_allcentrality <- deg_allcentrality[order(-deg_allcentrality$degAllCentrality),]   ###################
    
    V(g)$between <- centr_betw(g, directed = T, normalized = T)$res                       ###################
    betw_centrality <- data.frame(Node = node[,1], betwCentrality = V(g)$between)         ##    Between    ##
    betw_centrality <- betw_centrality[order(-betw_centrality$betwCentrality),]           ###################
    
    V(g)$indegree <- centr_degree(g,mode = 'in')$res                                      ###################
    deg_incentrality <- data.frame(Node = node[,1], degInCentrality = V(g)$indegree)      ##  In Degree    ##
    deg_incentrality <- deg_incentrality[order(-deg_incentrality$degInCentrality),]       ###################
    
    centrality_df <- cbind(deg_allcentrality, betw_centrality, deg_incentrality)
  })

# select input for Degree Centrality tab    
  output$selectId1 <- renderUI({
    if(is.null(fileData1()) || is.null(fileData2)) 
      return()
    centrality <- centrality()
    selectInput("topNodes1",label = "Please Select ID", choices = centrality[1:10,1],
                width = "300px")
  })

# render network output for Degree Centrality tab      
  output$dCentrality <- renderVisNetwork({
    if(is.null(fileData1()) || is.null(fileData2)) 
      return()
    link <- fileData1()
    node <- fileData2()
    colnames(link) <- c('from','to')
    colnames(node) <- c('id','group')

    selectedNode <- input$topNodes1                       #####################################################
    l1 <- subset(link, link[,1] == selectedNode)          ##                                                 ##
    twoHopNode <- unique(unlist(l1))                      ##    Manipulation of link and node data frame     ##
    link <- subset(link, link[,1] %in% twoHopNode)        ##    for visNetwork function                      ##
    n <- unique(unlist(link))                             ##                                                 ##
    node <- subset(node, node[,1] %in% n)                 #####################################################

    node <- mutate(node, label = id)
    visNetwork(node, link, height = "100%", width = "100%") %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(smooth = FALSE) %>%
      visNodes(shape = "circle",size = 1)
  })

# select input for Between Centrality tab    
  output$selectId2 <- renderUI({
    if(is.null(fileData1()) || is.null(fileData2)) 
      return()
    centrality <- centrality()
    selectInput("topNodes2",label = "Please Select ID", choices = centrality[1:10,3],
                width = "300px")
  })
 
# render network output for Betweeness Degree Centrality tab   
  output$bCentrality <- renderVisNetwork({
    if(is.null(fileData1()) || is.null(fileData2)) 
      return()
    link <- fileData1()
    node <- fileData2()
    colnames(link) <- c('from','to')
    colnames(node) <- c('id','group')
    
    selectedNode <- input$topNodes2                       #####################################################
    l1 <- subset(link, link[,1] == selectedNode)          ##                                                 ##
    twoHopNode <- unique(unlist(l1))                      ##    Manipulation of link and node data frame     ##
    link <- subset(link, link[,1] %in% twoHopNode)        ##    for visNetwork function                      ##
    n <- unique(unlist(link))                             ##                                                 ##
    node <- subset(node, node[,1] %in% n)                 #####################################################

    node <- mutate(node, label = id)
    visNetwork(node, link, height = "100%", width = "100%") %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(smooth = FALSE) %>%
      visNodes(shape = "circle",size = 1)
  })

# select input for In Degree Centrality tab    
  output$selectId3 <- renderUI({
    if(is.null(fileData1()) || is.null(fileData2)) 
      return()
    centrality <- centrality()
    selectInput("topNodes3",label = "Please Select ID", choices = centrality[1:10,5],
                width = "300px")
  })

# render network output for In Degree Centrality tab    
  output$inDCentrality <- renderVisNetwork({
    if(is.null(fileData1()) || is.null(fileData2)) 
      return()
    link <- fileData1()
    node <- fileData2()
    colnames(link) <- c('from','to')
    colnames(node) <- c('id','group')
    
    selectedNode <- input$topNodes3                        #####################################################
    l1 <- subset(link, link[,1] == selectedNode)           ##                                                 ##
    twoHopNode <- unique(unlist(l1))                       ##    Manipulation of link and node data frame     ##
    link <- subset(link, link[,1] %in% twoHopNode)         ##    for visNetwork function                      ##
    n <- unique(unlist(link))                              ##                                                 ##
    node <- subset(node, node[,1] %in% n)                  #####################################################
    
    node <- mutate(node, label = id)
    visNetwork(node, link, height = "100%", width = "100%") %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(smooth = FALSE) %>%
      visNodes(shape = "circle",size = 1)
  })

# creating table of frequency of communication between differnt departents 
  deptMails <- reactive({
    if(is.null(fileData1()) || is.null(fileData2)) 
      return()
    link <- fileData1()
    node <- fileData2()
    colnames(link) <- c('from','to')
    colnames(node) <- c('id','group')
    
    from_join <- inner_join(link, node, by = c("from" = "id"))
    to_join <- inner_join(link, node, by = c("to" = "id"))
    dept_comm <- data.frame("from_dept" = from_join[,3], "to_dept" = to_join[,3])
    dept_mail_count <- as.data.frame(table(dept_comm))
    colnames(dept_mail_count) <- c('From_Department', 'To_Department', 'Frequency')
    dept_mail_count
  })

# render table output of count of communication between different departments  
  output$emailsDept <- DT::renderDataTable({
    if(is.null(fileData1()) || is.null(fileData2)) 
      return()
    DT::datatable(deptMails(),rownames = FALSE,
                  class = 'cell-border stripe compact',
                  options = list(lengthMenu = c(15, 25, 50, 75, 100), pageLength = 15))
  })
# network output for department level connections  
  output$DeptComm <- renderVisNetwork({
    if(is.null(fileData1()) || is.null(fileData2)) 
      return()
    deptLink <- deptMails()
    deptLink <- subset(deptLink, Frequency > 0)               # removed connection where there is no communication
    deptNode <- data.frame(unique(unlist(deptLink[,c(1,2)])))
    colnames(deptLink) <- c('from','to', 'value')
    colnames(deptNode) <- c('id')
    deptNode <- mutate(deptNode, label = id)
    visNetwork(deptNode, deptLink, height = "100%", width = "100%") %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(smooth = FALSE, arrows = "to") %>%
      visNodes(shape = "circle",size = 1)
  })
  
}
)