library(shinydashboard)
library(eurostat)
library(stringr)
library(dplyr)
library(shiny)
library(plotly)

country_codes = c("Belgija"= "BE","Bolgarija"= "BG","Ceska Republika"= "CZ","Danska"= "DK","Nemcija"= "DE",
                  "Estonija"= "EE","Irska"= "IE","Grcija"= "EL","Španija"= "ES","Francija"= "FR","Hrvaška"= "HR",
                  "Italija"= "IT","Ciper"= "CY","Latvija"= "LV","Litva"= "LT","Luksemburg"= "LU",
                  "Madžarska"= "HU","Malta"= "MT","Nizozemska"= "NL","Avstrija"= "AT","Poljska"= "PL","Portugalska"= "AT",
                  "Romunija"= "RO","Slovenija"= "SI","Slovaška"= "SK","Finska"= "FI","Švedska"= "SE")

get_total_consumption <- function(country, year){
  country_code <- str_replace_all(country, country_codes)
  table_id = "nrg_cb_e"
  list_geo = c(country_code)
  list_time = c(year)
  list_datatypes =  'FC'
  data <- get_eurostat(table_id, filters = list(geo=list_geo, nrg_bal=list_datatypes, time=list_time), time_format = "num")
  
  return(data$values)
}

get_total_population <- function(country, year){
  country_code <- str_replace_all(country, country_codes)
  table_id = "demo_gind"
  list_geo = c(country_code)
  list_time = c(year)
  data <- get_eurostat(table_id, filters = list(geo=list_geo, time=list_time), time_format = "num")
  return(data$values[data$indic_de=="AVG"])
}



get_consumption_per_sector <- function(country){
  country_code <- str_replace_all(country, country_codes)
  table_id = "nrg_cb_e"
  list_geo = c(country_code)

  list_datatypes = c('NRG_E', 'FC_IND_E', 'FC_TRA_E', 'FC_OTH_CP_E', 'FC_OTH_HH_E', 'FC_OTH_AF_E', 'FC_OTH_FISH_E', 'FC_OTH_NSP_E')
  
  data <- get_eurostat(table_id, filters = list(geo=list_geo, nrg_bal=list_datatypes), time_format = "num")
  
  # SECTOR DATA TRANSLATION
  
  dict_sector_type = c('NRG_E'= 'Enetgetski sektor', 'FC_IND_E'= 'Industrijski sektor', 'FC_TRA_E'= 'Prometni sektor', 'FC_OTH_CP_E'= 'Komercialne in javne storitve', 'FC_OTH_HH_E'= 'Gospodinjstva', 'FC_OTH_AF_E'= 'Drugi sektorji', 'FC_OTH_FISH_E'= 'Drugi sektorji', 'FC_OTH_NSP_E'= 'Drugi sektorji')
  data$Sektor <- str_replace_all(data$nrg_bal, dict_sector_type)
  
  data <- data %>% group_by(geo, time, Sektor) %>% summarise(values = sum(values))
  
  return(data)
  
}



ui <- dashboardPage(
  dashboardHeader(title = "Električna energija v EU"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Poraba", tabName = "dashboard", icon = icon("plug")),
      menuItem("Proizvodnja", tabName = "widgets", icon = icon("industry"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(title = "Parametri pregleda", width = 4,
          sliderInput("year_slider", "Izberite leto:", 1990, as.integer(format(Sys.Date(), "%Y"))-1, 1990, sep=""),
          selectInput("country1", "Izberite državo 1:", names(country_codes), "Slovenija"),
          selectInput("country2", "Izberite državo 2:", names(country_codes), "Avstrija")),
    
    fluidRow(
      valueBoxOutput("choosen_year"),
      valueBoxOutput("total_consumption_percapita1"),
      valueBoxOutput("total_consumption_percapita2"),
      ),
    
    fluidRow(
      box(
        title = "Primerjava med drzavama:"
        ,status = "primary"
        ,solidHeader = TRUE
        ,collapsible = TRUE
        ,plotlyOutput("consumption_per_sector_comparison"),height = "500px"
        )
    )

    
    
    
  )
)
)




server <- function(input, output) {
  
  total_consumption_val_per_capita_country1 <- reactive({
    num1 <- 1000*get_total_consumption(input$country1, input$year_slider)
    num2 <- get_total_population(input$country1, input$year_slider)
    format(round(num1/num2,digits=3),nsmall = 3)
    
  })
  
  total_consumption_val_per_capita_country2 <- reactive({
    num1 <- 1000*get_total_consumption(input$country2, input$year_slider)
    num2 <- get_total_population(input$country2, input$year_slider)
    format(round(num1/num2,digits=3),nsmall = 3)
  })
  
  

  output$choosen_year <- renderValueBox({
    valueBox(
      formatC(input$year_slider, format="d", big.mark='')
      ,"Izbrano leto"
      ,icon = icon("calendar-alt")
      ,color = "purple")  
  })
  
  output$total_consumption_percapita1<- renderValueBox({ 
    valueBox(
      paste(input$country1,":",formatC(total_consumption_val_per_capita_country1(),
                    format="g", big.mark='.', decimal.mark = ","), "MWh/preb")
      ,'Skupna poraba električne energije na prebivalca'
      ,icon = icon("bolt")
      ,color = "red")  
  })
  
  output$total_consumption_percapita2<- renderValueBox({ 
    valueBox(
      paste(input$country2,":",formatC(total_consumption_val_per_capita_country2(),
                    format="g", big.mark='.', decimal.mark = ","), "MWh/preb")
      ,'Skupna poraba električne energije na prebivalca'
      ,icon = icon("bolt")
      ,color = "green")  
  })
  
  
  
  ####################
  # PLOTS AND TABLES #
  ####################
 
  
  output$consumption_per_sector_comparison <- renderPlotly({
    data <- get_consumption_per_sector(input$country1)
    data2 <- get_consumption_per_sector(input$country2)
    
    data <- data %>% filter(time == input$year_slider)
    data2 <- data2 %>% filter(time == input$year_slider)
    
    data$values <- 1000*data$values/get_total_population(input$country1,input$year_slider)
    data2$values <- 1000*data2$values/get_total_population(input$country2,input$year_slider)
    
    data$values2 <- data2$values
       
    fig <- plot_ly(data, x = ~Sektor, y = ~values, type = 'bar', name = paste(input$country1),colors = c("red","green"))
    fig <- fig %>% add_trace(y = ~ values2, name = paste(input$country2))
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
    
    fig
  })


  
  
  
}

shinyApp(ui, server)