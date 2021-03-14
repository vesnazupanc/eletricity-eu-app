library(shinydashboard)
library(eurostat)
library(stringr)
library(dplyr)


country_codes = c("Avstrija"= "AT", "Slovenija"= "SI", "Nemčija"= "DE")

get_total_consumption <- function(country, year){
  country_code <- str_replace_all(country, country_codes)
  table_id = "nrg_cb_e"
  list_geo = c(country_code)
  list_time = c(year)
  list_datatypes =  'FC'
  data <- get_eurostat(table_id, filters = list(geo=list_geo, nrg_bal=list_datatypes, time=list_time), time_format = "num")
  
  return(data$values)
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
          selectInput("country", "Izberite državo:", names(country_codes), "Slovenija"))
      
      ,box(title = "Poraba EE po letih", width = 8, height = 350,
           plotlyOutput("consumption_yearly"))
      ),
    
    fluidRow(
      valueBoxOutput("choosen_country"),
      valueBoxOutput("choosen_year"),
      valueBoxOutput("total_consumption")
      ),
    
    fluidRow(
      box(
        title = "Poraba EE po sektorjih - pie chart"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotlyOutput("consumption_per_sector") #,height = "500px"
        )
      ,box(
        title = "Poraba EE po sektorjih - tabela"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,tableOutput("consumption_per_sector_tbl")
      ) 
    )
  )

)


server <- function(input, output) {
  
  ###################
  # INFORMATION BOX #
  ###################
  
  data_consumption <- reactive({
    get_consumption_per_sector(input$country)
  })
  
  total_consumption_val <- reactive({
    get_total_consumption(input$country, input$year_slider)
  })
  
  output$choosen_country <- renderValueBox({
    valueBox(
      input$country
      ,"Izbrana država"
      ,icon = icon("globe")
      ,color = "blue")  
  })
  
  output$choosen_year <- renderValueBox({
    valueBox(
      formatC(input$year_slider, format="d", big.mark='')
      ,"Izbrano leto"
      ,icon = icon("calendar-alt")
      ,color = "purple")  
  })
  
  output$total_consumption <- renderValueBox({ 
    valueBox(
      paste(formatC(total_consumption_val(),
                    format="d", big.mark='.', decimal.mark = ","), "GWh")
      ,'Skupna poraba električne energije'
      ,icon = icon("bolt")
      ,color = "green")  
  })
  
  ####################
  # PLOTS AND TABLES #
  ####################
  
  output$consumption_yearly <- renderPlotly({
    data <- data_consumption()
    cons_yearly <- data %>% group_by(time) %>% summarise(values = sum(values))
    
    default_color = "rgba(150, 150, 150, 0.7)"
    chosen_color = "steelblue3"
    color_list <- rep(default_color, nrow(cons_yearly))
    
    if(input$year_slider <= cons_yearly$time %>% max()){
      k <- input$year_slider - cons_yearly$time %>% min() + 1
      color_list[k] <- chosen_color
    }
    
    
    fig_yearly <- plot_ly(type = "bar",
                  x = cons_yearly$time,
                  y = cons_yearly$values,
                  marker = list(color = color_list),
                  hovertemplate = "Leto: %{label} <br>Poraba: %{value} GWh<extra></extra>",
                  height = 250) %>% 
      layout(yaxis = list(title = "Poraba [GWh]"),
             xaxis = list(title = "Leto"),
             barmode = 'stack')
    
    fig_yearly
  })
  
  output$consumption_per_sector <- renderPlotly({
    data <- data_consumption()
    df <- data %>% filter(time == input$year_slider)
    fig_sectors <- plot_ly(type='pie', labels=df$Sektor, values=df$values, 
                           textinfo='label+percent',
                           insidetextorientation='radial',
                           hovertemplate = "%{label}: <br>Poraba: %{value} GWh<extra></extra>")
    
    fig_sectors <- fig_sectors %>% layout(showlegend = FALSE)
    fig_sectors
  })
  
  output$consumption_per_sector_tbl <- renderTable({
    data <- data_consumption()
    df <- data %>% filter(time == input$year_slider)
    df <- subset(df, select = c(Sektor, values))
    df <- df %>% mutate(Poraba=paste(round(values,2),"GWh"),
                        Procent=paste0(round(values/sum(values)*100,2),"%"))
    df <- df[order(df$values, decreasing = TRUE),]
    df <- subset(df, select = -c(values))
    df
  })
  
  
  
}

shinyApp(ui, server)