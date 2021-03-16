library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(eurostat)
library(stringr)
library(dplyr)
library(DT)


country_codes = c('Belgija' = 'BE', 'Bulgarija' = 'BG', 'Češka' = 'CZ', 'Danska' = 'DK', 'Nemčija' = 'DE', 
                  'Estonija' = 'EE', 'Irska' = 'IE', 'Grčija' = 'EL', 'Španija' = 'ES', 'Francija' = 'FR', 
                  'Hrvaška' = 'HR', 'Italija' = 'IT', 'Ciper' = 'CY', 'Latvija' = 'LV', 'Litva' = 'LT', 
                  'Luksemburg' = 'LU', 'Madžarska' = 'HU', 'Malta' = 'MT', 'Nizozemska' = 'NL', 'Avstrija' = 'AT',
                  'Poljska' = 'PL', 'Portugalska' = 'PT', 'Romunija' = 'RO', 'Slovenija' = 'SI', 'Slovaška' = 'SK',
                  'Finska' = 'FI', 'Švedska' = 'SE', 'Islandija' = 'IS', 'Norveška' = 'NO', 'Združeno kraljestvo' = 'UK',
                  'Črna gora' = 'ME', 'Severna Makedonija' = 'MK', 'Albanija' = 'AL', 'Srbija' = 'RS', 'Turčija' = 'TR',
                  'Bosna in Hercegovina' = 'BA', 'Kosovo' = 'XK', 'Moldavija' = 'MD', 'Ukrajina' = 'UA', 'Gruzija' = 'GE')

##### IMPORT FUNCTIONS #####

get_production_per_type_tab <- function(country,year){
  country_code <- str_replace_all(country, country_codes)
  list_time = c(year)
  table_id = "nrg_bal_peh"
  list_geo = c(country_code)
  
  list_datatypes = c('N900H', 'RA100', 'RA000', 'RA300', 'RA410', 'RA420', 'C0350-0370',
                     'P1000', 'S2000', 'O4000XBIO', 'W6100_6220', 'G3000', 'C0000X0350-0370')
  
  data <- get_eurostat(table_id, filters = list(nrg_bal = c("GEP"), geo=list_geo,
                                                siec=list_datatypes,time=list_time,
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

get_consumption_per_sector_tab <- function(country,year){
  country_code <- str_replace_all(country, country_codes)
  table_id = "nrg_cb_e"
  list_geo = c(country_code)
  list_time = c(year)
  list_datatypes = c('NRG_E', 'FC_IND_E', 'FC_TRA_E', 'FC_OTH_CP_E', 'FC_OTH_HH_E', 'FC_OTH_AF_E', 'FC_OTH_FISH_E', 'FC_OTH_NSP_E')
  
  data <- get_eurostat(table_id, filters = list(geo=list_geo, nrg_bal=list_datatypes,time=list_time), time_format = "num")
  
  # SECTOR DATA TRANSLATION
  
  dict_sector_type = c('NRG_E'= 'Enetgetski sektor', 'FC_IND_E'= 'Industrijski sektor', 'FC_TRA_E'= 'Prometni sektor', 'FC_OTH_CP_E'= 'Komercialne in javne storitve', 'FC_OTH_HH_E'= 'Gospodinjstva', 'FC_OTH_AF_E'= 'Drugi sektorji', 'FC_OTH_FISH_E'= 'Drugi sektorji', 'FC_OTH_NSP_E'= 'Drugi sektorji')
  data$Sektor <- str_replace_all(data$nrg_bal, dict_sector_type)
  
  data <- data %>% group_by(geo, time, Sektor) %>% summarise(values = sum(values))
  
  return(data)
  
}



get_total_population <- function(country, year){
  country_code <- str_replace_all(country, country_codes)
  table_id = "demo_gind"
  list_geo = c(country_code)
  list_time = c(year)
  data <- get_eurostat(table_id, filters = list(geo=list_geo, time=list_time), time_format = "num")
  return(data$values[data$indic_de=="AVG"])
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
      box(title = "Izbira spremenljivk", width = 4,
          pickerInput(
            "spremenljivka",
            "Izberi spremenljivko:",
            choices = c("Proizvodnja","Poraba"),
            selected = "Proizvodnja",
            multiple = F,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            "drzava",
            "Izberi državo:",
            choices = names(country_codes),
            selected = "Slovenija",
            multiple = T,
            options = list(`actions-box` = TRUE,`deselect-all-text` = "Izbriši vse",
                           `select-all-text` = "Izberi vse",
                           `none-selected-text` = "Brez")
          ),
          pickerInput(
            "obdobje",
            "Izberi leto:",
            choices = 1990:2019,
            selected = 2019,
            multiple = T,
            options = list(`actions-box` = TRUE,`deselect-all-text` = "Izbriši vse",
                           `select-all-text` = "Izberi vse",
                           `none-selected-text` = "Brez")
          ),
          # Button
          downloadButton("Download","Naloži podatke")
          
      ),
    
    fluidRow(
      box(
        title = "Željeni podatki:"
        ,status = "primary"
        ,solidHeader = TRUE
        ,collapsible = TRUE
        ,dataTableOutput("tabela"),
        )
    )

    
    
    
  )
)
)

server <- function(input, output) {
  
  
  Data <- reactive({
    
    if(input$spremenljivka=="Poraba"){
    Data <- get_consumption_per_sector_tab(c(input$drzava),c(input$obdobje))
    colnames(Data) <- c("Drzava","Leto","Sektor","Poraba[GWh]")
    Data
    } else {
      Data <- get_production_per_type_tab(c(input$drzava),c(input$obdobje))
      Data <- Data[,c(1,2,4,3)]
      colnames(Data) <- c("Drzava","Leto","Vir","Proizvodnja[GWh]")
      Data
    }
  })
  
  output$tabela <- renderDataTable({
    datatable(Data(), options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20)
    ))

  })

  output$Download <- downloadHandler(
    filename = function() {
      paste("Podatki-",input$spremenljivka,"-",Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(Data(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)