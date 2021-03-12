#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(plotly)
library(eurostat)
library(dplyr)

drzava <- c("Slovenija", "Hrva?ka", "Italija", "Avstrija", "Francija", "Nem?ija", "Velika Britanija")
leto <- c(2010:2019)
vir <- c("Hidro", "Vetrna", "Nuklearna", "Premog", "Soncna")
mesec <- c("januar", "februar","marec","april","maj","junij","julij","avgust","september","oktober","november","december")

podatki <- expand.grid(leto,mesec,vir,drzava)

colnames(podatki) <- c("leto","mesec","vir","drzava")


podatki$proizvodnja <- round(runif(nrow(podatki),min=1,max=10000))





# Define UI for application that draws a histogram
ui <- navbarPage("Aplikacija",
                 tabPanel("Proizvodnja",
                          fluidPage(
                              selectInput("drzava", "Izberi dr?avo:",drzava),
                              selectInput(
                                  inputId =  "leto", 
                                  label = "Izberi leto:", 
                                  choices = 2010:2019
                              ),
                              plotlyOutput("data"),
                              plotlyOutput("data1")
                          )
                          
                          
                 ),
                 tabPanel("Poraba"),
                 tabPanel("Ostalo")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    Data <- reactive({
        Data <- data.frame()
        Data <- podatki %>% 
            filter(leto== input$leto & drzava ==input$drzava)%>% group_by(vir)
        Data$proizvodnja <- Data$proizvodnja/1000
        return(Data)
    })
    
    Data1 <- reactive({
        Data <- data.frame()
        Data <- podatki %>% 
            filter(leto== input$leto & drzava ==input$drzava)
        return(Data)
    })
    
    
    output$data <- renderPlotly({
        fig <- plot_ly(Data(), labels = ~vir, values = ~proizvodnja, sort = T, 
                       marker = list(colors = c("1" = "#B76C9E", 
                                                "2" = "#4285F4",
                                                "3" = "#EA4335")),text = ~paste(vir,"",proizvodnja, ' THWh')) %>%
            add_pie(hole = 0.5) %>%
            layout(
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% 
            layout(title = 'Proizvodnja elektricne energije po viru za izbrano drzavo in leto')
        fig
    })
    
    output$data1 <- renderPlotly({
        fig <- plot_ly(Data1(), x = ~mesec, y = ~proizvodnja, type = 'bar',color=~vir)
        fig <- fig %>% layout(yaxis = list(title = 'Proizvodnja[GWh]'), barmode = 'stack')
        fig
    })
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
