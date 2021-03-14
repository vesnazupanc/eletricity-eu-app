#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

set.seed(2021)
library(shiny)
library(plotly)
library(eurostat)
library(dplyr)
library(shinyWidgets)


drzava <- c("Slovenija", "Hrvaska", "Italija", "Avstrija", "Francija", "Nemcija", "Velika Britanija")
leto <- c(2010:2019)
vir <- c("Hidro", "Vetrna", "Nuklearna", "Premog", "Soncna")
mesec <- c("januar", "februar","marec","april","maj","junij","julij","avgust","september","oktober","november","december")

podatki <- expand.grid(leto,mesec,vir,drzava)

colnames(podatki) <- c("leto","mesec","vir","drzava")


podatki$proizvodnja <- round(runif(nrow(podatki),min=1,max=10000))




# Define UI for application that draws a histogram
ui <- navbarPage("Aplikacija",
                 tabPanel("Proizvodnja",
                
                                sidebarPanel(
                                            selectInput("drzava", "Izberi drzavo:",drzava,multiple = F),
                                            selectInput(
                                                inputId =  "leto", 
                                                label = "Izberi leto:", 
                                                choices = 2010:2019)),
                              
                              # define content of the main part of the page ####   
                              mainPanel(
                                  plotlyOutput("data"),
                                  plotlyOutput("data1")
                              )
                       )
                         ,
                 
                 tabPanel("Nalozi podatke",
                          
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              # Input: Choose dataset ----
                              
                              pickerInput(
                                "drzava1",
                                "Izberi drzavo:",
                                choices = drzava,
                                selected = "Slovenija",
                                multiple = T,
                                options = list(`actions-box` = TRUE)
                              ),
                              
                              
                              
                              pickerInput(
                                "leto1",
                                "Izberi leto:",
                                choices = leto,
                                selected = "2019",
                                multiple = T,
                                options = list(`actions-box` = TRUE)
                              ),
                              pickerInput(
                                "vir",
                                "Izberi vir:",
                                choices = vir,
                                selected = "Hidro",
                                multiple = T,
                                options = list(`actions-box` = TRUE)
                              ),
                              # Button
                              downloadButton("downloadData", "Download")
                              
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              
                              dataTableOutput("table")
                              
                            )
                            
                          )
                 )
                          ,
                 tabPanel("Ostalo")
                 
                 )


                 


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    Data <- reactive({
        Data <- data.frame()
        Data <- podatki %>% 
            filter(leto== input$leto & drzava ==input$drzava)%>% group_by(vir)%>% summarise(proizvodnja = sum(proizvodnja))
        Data$proizvodnja <- Data$proizvodnja/1000
        return(Data)
    })
    
    Data1 <- reactive({
        Data <- data.frame()
        Data <- podatki %>% 
            filter(leto== input$leto & drzava ==input$drzava)
        return(Data)
    })
    
    datasetInput <- reactive({
      Data <- data.frame()
      Data <- podatki %>% 
        filter(leto%in% c(input$leto1) & drzava %in% c(input$drzava1) & vir %in% c(input$vir)) 
      return(Data)
    })
    
    output$data <- renderPlotly({
      
        fig <- plot_ly(Data(), labels = ~vir, values = ~proizvodnja, sort = F, 
                       marker = list(colors = c("#FF0000", "#CCFF00", "#00FF66", "#0066FF" ,"#CC00FF")),text = ~paste(vir)) %>%
            add_pie(hole = 0.5) %>%
            layout(
                yaxis = list(showgrid = FALSE, zeroline = F, showticklabels = FALSE))%>% 
            layout(title = 'Proizvodnja elektricne energije po viru za izbrano drzavo in leto',showlegend = FALSE)
        fig
    })
    
    output$data1 <- renderPlotly({
        fig <- plot_ly(Data1(), x = ~mesec, y = ~proizvodnja, type = 'bar',color=~vir,colors = c("#FF0000", "#CCFF00", "#00FF66", "#0066FF" ,"#CC00FF"))
        fig <- fig %>% layout(yaxis = list(title = 'Proizvodnja[GWh]'), barmode = 'stack',showlegend = FALSE)
        fig
    })
    
   
    
    # Table of selected dataset ----
    output$table <- renderDataTable({
      datasetInput()
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
    
}







# Run the application 
shinyApp(ui = ui, server = server)

