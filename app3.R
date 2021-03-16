library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(eurostat)
library(stringr)
library(dplyr)


################ IMPORT FUNCTION & PARAMETERS ################

# Country to country codes translator
country_codes = c('Belgija' = 'BE', 'Bulgarija' = 'BG', 'Češka' = 'CZ', 'Danska' = 'DK', 'Nemčija' = 'DE', 
                  'Estonija' = 'EE', 'Irska' = 'IE', 'Grčija' = 'EL', 'Španija' = 'ES', 'Francija' = 'FR', 
                  'Hrvaška' = 'HR', 'Italija' = 'IT', 'Ciper' = 'CY', 'Latvija' = 'LV', 'Litva' = 'LT', 
                  'Luksemburg' = 'LU', 'Madžarska' = 'HU', 'Malta' = 'MT', 'Nizozemska' = 'NL', 'Avstrija' = 'AT',
                  'Poljska' = 'PL', 'Portugalska' = 'PT', 'Romunija' = 'RO', 'Slovenija' = 'SI', 'Slovaška' = 'SK',
                  'Finska' = 'FI', 'Švedska' = 'SE', 'Islandija' = 'IS', 'Norveška' = 'NO', 'Združeno kraljestvo' = 'UK',
                  'Črna gora' = 'ME', 'Severna Makedonija' = 'MK', 'Albanija' = 'AL', 'Srbija' = 'RS', 'Turčija' = 'TR',
                  'Bosna in Hercegovina' = 'BA', 'Kosovo' = 'XK', 'Molavija' = 'MD', 'Ukrajina' = 'UA', 'Gruzija' = 'GE')

get_avg_population <- function(country, year){
  #' Get average population
  #' 
  #' @param country Country name in Slovene language
  #' @param year int, wanted year
  #' 
  #' @return int, average population for a chosen country and year
  
  country_code <- str_replace_all(country, country_codes)
  table_id = "demo_gind"
  list_geo = c(country_code)
  list_time = c(year)
  data <- get_eurostat(table_id, filters = list(geo=list_geo, time=list_time, indic_de=c("AVG")), time_format = "num")
  
  return(data$values)
}

get_total_production <- function(country, year){
  #' Get total electricity production
  #' 
  #' @param country Country name in Slovene language
  #' @param year int, wanted year
  #' 
  #' @return total net production of electricity for chosen a country and year
  
  country_code <- str_replace_all(country, country_codes)
  table_id = "nrg_bal_peh"
  list_geo = c(country_code)
  list_time = c(year)
  list_datatypes =  'TOTAL'
  data <- get_eurostat(table_id, filters = list(nrg_bal = c("GEP"), geo=list_geo,
                                                siec=list_datatypes, time = list_time,
                                                unit=c("GWH")
  ), time_format = "num")
  
  return(data$values)
}

get_import_export <- function(country, year){
  #' Get import and export of electricity
  #' 
  #' @param country Country name in Slovene language
  #' @param year int, wanted year
  #' 
  #' @return named vector with corresponding values of import and export of electricity for a chosen country and year
  
  country_code <- str_replace_all(country, country_codes)
  table_id = "nrg_cb_e"
  list_geo = c(country_code)
  list_time = c(year)
  list_datatypes =  c("IMP", "EXP")
  data <- get_eurostat(table_id, filters = list(geo=list_geo, nrg_bal=list_datatypes, time=list_time), time_format = "num")
  
  return(c("import" = data[data$nrg_bal=='IMP',]$values, "export" = data[data$nrg_bal=='EXP',]$values))
}


get_production_per_type <- function(country){
  #' Get electricity production per fuel type
  #' 
  #' @param country Country name in Slovene language
  #' 
  #' @return data frame with production per fuel type over all available years for a chosen country
  
  country_code <- str_replace_all(country, country_codes)
  table_id = "nrg_bal_peh"
  list_geo = c(country_code)
  
  list_datatypes = c('N900H', 'RA100', 'RA000', 'RA300', 'RA410', 'RA420', 'C0350-0370',
                     'P1000', 'S2000', 'O4000XBIO', 'W6100_6220', 'G3000', 'C0000X0350-0370')
  
  data <- get_eurostat(table_id, filters = list(nrg_bal = c("GEP"), geo=list_geo,
                                                siec=list_datatypes,
                                                unit=c("GWH")
  ), time_format = "num")
  
  decode_datatypes = c('N900H' = 'Nuklearna',
                       'RA100' = 'Hidro',
                       'RA000' = 'Obnovljivi',
                       'RA300' = 'Veter',
                       'RA410' = 'Sonce', 
                       'RA420' = 'Sonce',
                       'C0350-0370' = 'Ostali neobnovljivi viri',
                       'P1000' = 'Ostali neobnovljivi viri',
                       'S2000' = 'Ostali neobnovljivi viri',
                       'O4000XBIO' = 'Ostali neobnovljivi viri',
                       'W6100_6220' = 'Ostali neobnovljivi viri', 
                       'G3000' = 'Naravni plin', 
                       'C0000X0350-0370' = 'Fosilna goriva')
  
  # SECTOR DATA TRANSLATION
  
  data$Vir <- str_replace_all(data$siec, decode_datatypes)
  res_temp <- data %>% filter(Vir%in%c('Obnovljivi','Hidro', 'Veter', 'Sonce'))
  res_temp[res_temp$Vir%in%c('Hidro', 'Veter', 'Sonce'),c('values')] <- -1*res_temp[res_temp$Vir%in%c('Hidro', 'Veter', 'Sonce'),c('values')]
  res_temp <- res_temp %>% group_by(geo, time) %>% summarise(values = sum(values))
  res_temp$Vir <- "Ostali obnovljivi viri"
  
  df <- rbind(subset(data, Vir != 'Obnovljivi', select = c('geo', 'time', 'values', 'Vir')), res_temp)
  
  return(df)
  
}

get_total_consumption <- function(country, year){
  #' Get total electricity consumption
  #' 
  #' @param country Country name in Slovene language
  #' @param year int, wanted year
  #' 
  #' @return total consumption of electricity for chosen a country and year
  
  country_code <- str_replace_all(country, country_codes)
  table_id = "nrg_cb_e"
  list_geo = c(country_code)
  list_time = c(year)
  list_datatypes =  'FC'
  data <- get_eurostat(table_id, filters = list(geo=list_geo, nrg_bal=list_datatypes, time=list_time), time_format = "num")
  
  return(data$values)
}

get_consumption_per_sector <- function(country){
  #' Get electricity consumption per sector
  #' 
  #' @param country Country name in Slovene language
  #' 
  #' @return data frame with consumption per sector over all available years for a chosen country
  
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




library(shiny)
library(shinydashboard)
library(shinyWidgets)

################ TABS ######################


#----------- Home Tab ------------####

home_tab <- tabItem(
  tabName = 'home',
  h1("Naslov texta"),
  p("Navaden text ... tu bo opis kaj je ta aplikacija ipd :) ")
)


#----------- Consumption Tab -----####

consumption_tab <- tabItem(
  tabName = 'consumption',
  
  fluidRow(
    box(title = "Parametri pregleda", width = 4,
        sliderInput("year1", "Izberite leto:", 1990, as.integer(format(Sys.Date(), "%Y"))-1, 1990, sep=""),
        selectInput("country1", "Izberite državo:", names(country_codes), "Slovenija")),
    
    box(title = "Poraba po letih", width = 8, height = 350,
        plotlyOutput("consumption_yearly"))
  ),
  
  fluidRow(
    valueBoxOutput("chosen_country1"),
    valueBoxOutput("chosen_year1"),
    valueBoxOutput("total_cons1")
  ),
  
  fluidRow(
    tabBox(
      title = "Poraba po sektorjih",
      tabPanel(icon("chart-pie"),plotlyOutput("cons_per_sector_pie")),
      tabPanel(icon("table"), tableOutput("cons_per_sector_tbl"))
    )
  )
)


#----------- Production Tab ------####

production_tab <- tabItem(
  tabName = 'production',
  fluidRow(
    box(title = "Parametri pregleda", width = 4,
        sliderInput("year2", "Izberite leto:", 1990, as.integer(format(Sys.Date(), "%Y"))-1, 1990, sep=""),
        selectInput("country2", "Izberite državo:", names(country_codes), "Slovenija")),
    
    box(title = "Proizvodnja po letih", width = 8, height = 350,
        plotlyOutput("production_yearly"))
  ),
  
  fluidRow(
    valueBoxOutput("chosen_country2"),
    valueBoxOutput("chosen_year2"),
    valueBoxOutput("total_prod2")
  ),
  
  fluidRow(
    tabBox(
      title = "Proizvodnja glede na vir goriva"
      ,height = 700
      ,tabPanel(icon("chart-pie"), plotlyOutput("prod_per_type_pie"))
      ,tabPanel(icon("table"), tableOutput("prod_per_type_tbl"))
    )
    ,box(
      title = "Tukaj uvoz izvoz?"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE)
  )
)


#----------- Comparison Tab ------####

comparison_tab <- tabItem(
  tabName = "comparison"
  ,fluidRow(
    box(title = "Parametri pregleda", width = 4, height = 350
        ,sliderInput("year31", "Izberite leto:", 1990, as.integer(format(Sys.Date(), "%Y"))-1, 1990, sep="")
        ,selectInput("country31", "Izberite prvo državo:", names(country_codes), "Nemčija")
        ,selectInput("country32", "Izberite drugo državo:", names(country_codes), "Slovenija"))
    
    ,box(title = "Pregled po letih", width = 8, height = 350
         ,checkboxGroupInput("check_compare", label = h3("Izberite serije za prikaz na grafu"), 
                             choices = list("Poraba" = 1, "Proizvodnja" = 2))
         ,plotlyOutput("compare_over_years"))
  ),
  
  fluidRow(
    valueBoxOutput("country_1"),
    valueBoxOutput("import_pc_1"),
    valueBoxOutput("export_pc_1")
  ),
  
  fluidRow(
    valueBoxOutput("country_2"),
    valueBoxOutput("import_pc_2"),
    valueBoxOutput("export_pc_2")
  ),
  
  fluidRow(
    tabBox(height = 500,
           title = "Primerjava porabe po sektorjih za izbrano leto",
           tabPanel(icon("chart-bar"), plotlyOutput("compare_cons_bar")),
           tabPanel(icon("table"), tableOutput("compare_cons_tbl"))
    ),
    
    tabBox(height = 500,
           title = "Primerjava proizvodnje glede na vir za izbrano leto",
           tabPanel(icon("chart-bar"), plotlyOutput("compare_prod_bar")),
           tabPanel(icon("table"), tableOutput("compare_prod_tbl"))
    )
    
  )
  
)


#----------- Download Tab --------####

download_tab <- tabItem(
  tabName = 'download',
  h1("Tu bomo loadali podatke"),
)




################ DASHBOARD ##################

ui <- dashboardPage(
  
  dashboardHeader(title = "Elektrika v EU"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Domov", tabName = 'home', icon = icon("home")),
      menuItem("Poraba", tabName = "consumption", icon = icon("plug")),
      menuItem("Proizvodnja", tabName = "production", icon = icon("industry")),
      menuItem("Primerjalnik", tabName = "comparison", icon = icon("exchange-alt")),
      menuItem("Izvozi podatke", tabName = "download", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    # Background
    tags$img(src = "https://images.freeimages.com/images/large-previews/ad6/light-bulb-1416824.jpg",
             style = 'position: absolute; opacity: 0.2;'),
    # Tabs
    tabItems(
      home_tab,
      consumption_tab,
      production_tab,
      comparison_tab,
      download_tab
    )
  )
)



################ SERVER ################


server <- function(input, output) {
  
  
  #============= DATA AND VALUES ===============
  
  # Consumption tag
  data_cons_1 <- reactive({
    get_consumption_per_sector(input$country1)
  })
  
  val_total_cons_1 <- reactive({
    get_total_consumption(input$country1, input$year1)
  })  
  
  # Production tag
  data_prod_1 <- reactive({
    get_production_per_type(input$country2)
  })
  
  val_total_prod_1 <- reactive({
    get_total_production(input$country2, input$year2)
  })
  
  # Comparison tag
  data_cons_31 <- reactive({
    get_consumption_per_sector(input$country31)
  })
  
  #============= CONSUMPTION TAG ===============
  
  # Value boxes
  
  output$chosen_country1 <- renderValueBox({
    valueBox(
      input$country1,
      "Izbrana država", icon = icon("globe"),
      color = "blue")  
  })
  
  output$chosen_year1 <- renderValueBox({
    valueBox(
      formatC(input$year1, format="d", big.mark=''),
      "Izbrano leto",
      icon = icon("calendar-alt"),
      color = "purple")  
  })
  
  output$total_cons1 <- renderValueBox({ 
    valueBox(
      paste(formatC(val_total_cons_1(), format="d", big.mark='.', decimal.mark = ","), "GWh"),
      'Skupna poraba električne energije',
      icon = icon("bolt"),
      color = "green")  
  })
  
  # Plots and tables
  
  output$consumption_yearly <- renderPlotly({
    data <- data_cons_1()
    cons_yearly <- data %>% group_by(time) %>% summarise(values = sum(values))
    
    default_color = "grey"
    chosen_color = "green"
    color_list <- rep(default_color, nrow(cons_yearly))
    
    if(input$year1 <= cons_yearly$time %>% max()){
      k <- input$year1 - cons_yearly$time %>% min() + 1
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
  
  output$cons_per_sector_pie <- renderPlotly({
    data <- data_cons_1()
    df <- data %>% filter(time == input$year1)
    fig_sectors <- plot_ly(type='pie', labels=df$Sektor, values=df$values, 
                           textinfo='label+percent',
                           insidetextorientation='radial',
                           hovertemplate = "%{label}: <br>Poraba: %{value} GWh<extra></extra>")
    
    fig_sectors <- fig_sectors %>% layout(showlegend = FALSE)
    fig_sectors
  })
  
  output$consumption_per_sector_tbl <- renderTable({
    data <- data_cons_1()
    df <- data %>% filter(time == input$year1)
    df <- subset(df, select = c(Sektor, values))
    df <- df %>% mutate(Poraba=paste(round(values,2),"GWh"),
                        Procent=paste0(round(values/sum(values)*100,2),"%"))
    df <- df[order(df$values, decreasing = TRUE),]
    df <- subset(df, select = -c(values))
    df
  })
  
  #============= PRODUCTION TAG ================
  
  
  # Value boxes
  
  output$chosen_country2 <- renderValueBox({
    valueBox(
      input$country2,
      "Izbrana država", icon = icon("globe"),
      color = "blue")  
  })
  
  output$chosen_year2 <- renderValueBox({
    valueBox(
      formatC(input$year2, format="d", big.mark=''),
      "Izbrano leto", icon = icon("calendar-alt"),
      color = "purple")  
  })
  
  output$total_prod2 <- renderValueBox({ 
    valueBox(
      paste(formatC(val_total_prod_1(), format="d", big.mark='.', decimal.mark = ","), "GWh"),
      'Skupna proizvodnja električne energije', icon = icon("bolt"),
      color = "green")  
  })
  
  
  output$production_per_type_pie <- renderPlotly({
    data <- data_prod_1()
    df <- data %>% filter(time == input$year2)
    fig <- plot_ly(type='pie', labels=df$Vir, values=df$values, 
                   textinfo='label+percent',
                   insidetextorientation='radial',
                   hovertemplate = "%{label}: <br>Proizvodnja: %{value} GWh<extra></extra>")
    
    fig <- fig %>% layout(showlegend = FALSE)
    fig
  })
  
  output$prod_per_type_pie <- renderTable({
    data <- data_prod_1()
    df <- data %>% filter(time == input$year2)
    df <- subset(df, select = c(Vir, values))
    df <- df %>% mutate(Proizvodnja=paste(round(values,2),"GWh"),
                        Procent=paste0(round(values/sum(values)*100,2),"%"))
    df <- df[order(df$values, decreasing = TRUE),]
    df <- subset(df, select = -c(values))
    df
  })
  
  #============= COMPARISON TAG ================
  
  ##### information box #####
  
  output$country_1 <- renderValueBox({
    valueBox(
      input$country31
      ,"Prva država"
      ,icon = icon("globe")
      ,color = "yellow")  
  })
  
  output$country_2 <- renderValueBox({
    valueBox(
      input$country32
      ,"Druga država"
      ,icon = icon("globe")
      ,color = "olive")  
  })
  
  output$import_pc_1 <- renderValueBox({
    valueBox(
      input$country31
      ,paste(input$country31, "UVOZ")
      ,icon = icon("arrow-left")
      ,color = "yellow")  
  })
  
  output$import_pc_2 <- renderValueBox({
    valueBox(
      input$country32
      ,paste(input$country32, "UVOZ")
      ,icon = icon("arrow-left")
      ,color = "olive")  
  })
  
  output$export_pc_1 <- renderValueBox({
    valueBox(
      input$country31
      ,paste(input$country31, "IZVOZ")
      ,icon = icon("arrow-right")
      ,color = "yellow")  
  })
  
  output$export_pc_2 <- renderValueBox({
    valueBox(
      input$country32
      ,paste(input$country32, "IZVOZ")
      ,icon = icon("arrow-right")
      ,color = "olive")  
  })
}



shinyApp(ui, server)