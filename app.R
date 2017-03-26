load("shiny.RData")

library(magrittr)
library(dplyr)
library(shiny)
library(visNetwork)



server = function(input,output) {
  observe({
    visNetworkProxy("network_proxy_options") %>%
      visOptions(highlightNearest = list(enabled = T, hover = F,
                                         algorithm = c("hierarchical"), degree = 1))})
 
  myFilteredSystems <- reactive({head(mySystems, input$nSysSlider)})

  
  output$vi <- renderVisNetwork({
    
    nodes <- data.frame(id = myFilteredSystems()$SystemID,
                        color = ifelse(myFilteredSystems()$SystemID %in% myPopulatedSystems$SystemID, "blue", "cyan"),
                        label = myFilteredSystems()$Name,
                        shadow = ifelse(myFilteredSystems()$SystemID %in% myPopulatedSystems$SystemID, T, F),
                        title = myFilteredSystems()$label)
    
    edges <- data.frame(from = myJumps[,1], to = myJumps[,2],
                        label = ifelse(myJumps[,3],"GATE","JUMP"),
                        color = ifelse(myJumps[,3],"gold","black"))
    
    visNetwork(nodes, edges) %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(smooth = FALSE) 
})}

ui =  fluidPage(
  sliderInput("nSysSlider", "Systems", min = 1, max = nrow(mySystems), value = 6, step = 1),
  visNetworkOutput("vi",height = "1000px"))

shinyApp(ui,server)