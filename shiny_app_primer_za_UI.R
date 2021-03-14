library(shinydashboard)
library(eurostat)
library(stringr)
library(dplyr)


country_codes = c("Avstrija"= "AT", "Slovenija"= "SI", "Nemčija"= "DE")

get_consumption_data <- function(country, year){
  country_code <- str_replace_all(country, country_codes)
  table_id = "nrg_cb_e"
  list_geo = c(country_code)
  list_time = c(year)
  
  list_datatypes = c('NRG_E', 'NRG_CM_E', 'NRG_OIL_NG_E', 'NRG_PF_E', 'NRG_CO_E', 
                     'NRG_BKBPB_E', 'NRG_GW_E', 'NRG_BF_E', 'NRG_PR_E', 'NRG_NI_E', 'NRG_CL_E', 
                     'NRG_LNG_E', 'NRG_BIOG_E', 'NRG_GTL_E', 'NRG_CPP_E', 'NRG_NSP_E', 'FC', 
                     'FC_IND_E', 'FC_IND_IS_E', 'FC_IND_CPC_E', 'FC_IND_NFM_E', 'FC_IND_NMM_E', 
                     'FC_IND_TE_E', 'FC_IND_MAC_E', 'FC_IND_MQ_E', 'FC_IND_FBT_E', 'FC_IND_PPP_E', 
                     'FC_IND_WP_E', 'FC_IND_CON_E', 'FC_IND_TL_E', 'FC_IND_NSP_E', 'FC_TRA_E', 
                     'FC_TRA_RAIL_E', 'FC_TRA_ROAD_E', 'FC_TRA_PIPE_E', 'FC_TRA_NSP_E', 
                     'FC_OTH_CP_E', 'FC_OTH_HH_E', 'FC_OTH_AF_E', 'FC_OTH_FISH_E', 'FC_OTH_NSP_E')
  
  data <- get_eurostat(table_id, filters = list(geo=list_geo, nrg_bal=list_datatypes, time=list_time), time_format = "num")
  
  # SECTOR DATA TRANSLATION
  
  dict_sector_type = c('NRG_E'= 'Energetski sektor', 'NRG_CM_E'= 'Energetski sektor', 'NRG_OIL_NG_E'= 'Energetski sektor', 
                       'NRG_PF_E'= 'Energetski sektor', 'NRG_CO_E'= 'Energetski sektor', 'NRG_BKBPB_E'= 'Energetski sektor',
                       'NRG_GW_E'= 'Energetski sektor', 'NRG_BF_E'= 'Energetski sektor', 'NRG_PR_E'= 'Energetski sektor',
                       'NRG_NI_E'= 'Energetski sektor', 'NRG_CL_E'= 'Energetski sektor', 'NRG_LNG_E'= 'Energetski sektor', 
                       'NRG_BIOG_E'= 'Energetski sektor', 'NRG_GTL_E'= 'Energetski sektor', 'NRG_CPP_E'= 'Energetski sektor',
                       'NRG_NSP_E'= 'Energetski sektor', 'FC$'= 'Skupna poraba', 'FC_IND_E'= 'Industrijski sektor', 
                       'FC_IND_IS_E'= 'Industrijski sektor', 'FC_IND_CPC_E'= 'Industrijski sektor', 'FC_IND_NFM_E'= 'Industrijski sektor',
                       'FC_IND_NMM_E'= 'Industrijski sektor', 'FC_IND_TE_E'= 'Industrijski sektor', 'FC_IND_MAC_E'= 'Industrijski sektor',
                       'FC_IND_MQ_E'= 'Industrijski sektor', 'FC_IND_FBT_E'= 'Industrijski sektor', 'FC_IND_PPP_E'= 'Industrijski sektor',
                       'FC_IND_WP_E'= 'Industrijski sektor', 'FC_IND_CON_E'= 'Industrijski sektor', 'FC_IND_TL_E'= 'Industrijski sektor',
                       'FC_IND_NSP_E'= 'Industrijski sektor', 'FC_TRA_E'= 'Prometni sektor', 'FC_TRA_RAIL_E'= 'Prometni sektor',
                       'FC_TRA_ROAD_E'= 'Prometni sektor', 'FC_TRA_PIPE_E'= 'Prometni sektor', 'FC_TRA_NSP_E'= 'Prometni sektor', 
                       'FC_OTH_CP_E'= 'Komercialne in javne storitve', 'FC_OTH_HH_E'= 'Gospodinjstva', 'FC_OTH_AF_E'= 'Drugi sektorji',
                       'FC_OTH_FISH_E'= 'Drugi sektorji', 'FC_OTH_NSP_E'= 'Drugi sektorji')
  
  dict_sector_subtype = c('NRG_E'= 'Skupaj', 'NRG_CM_E'= 'Premogovniki', 'NRG_OIL_NG_E'= 'Naprave za črpanje nafte in zemeljskega plina', 
                          'NRG_PF_E'= 'Briketi črnega premoga?', 'NRG_CO_E'= 'Koksarne', 
                          'NRG_BKBPB_E'= 'Briketi rjavega premoga in obrati za prikete šote', 'NRG_GW_E'= 'Plinarne', 
                          'NRG_BF_E'= 'Plavži', 'NRG_PR_E'= 'Rafinereje nafte', 'NRG_NI_E'= 'Jedrska industrija', 
                          'NRG_CL_E'= 'Naprave za utekočinjenje premoga', 'NRG_LNG_E'= 'Naprave za utekočinjenje in uplinjanje (LNG)',
                          'NRG_BIOG_E'= 'Naprave za uplinjanje bioplina', 'NRG_GTL_E'= 'Obrati za plin in tekočine',
                          'NRG_CPP_E'= 'Proizvodnja oglja', 'NRG_NSP_E'= 'Drugo', 'FC$'= '-', 'FC_IND_E'= 'Skupaj', 
                          'FC_IND_IS_E'= 'Železo in jeklo', 'FC_IND_CPC_E'= 'Kemična in petrokemijska industrija',
                          'FC_IND_NFM_E'= 'Neželezna kovine', 'FC_IND_NMM_E'= 'Nekovisnki minerali',
                          'FC_IND_TE_E'= 'Transportna oprema', 'FC_IND_MAC_E'= 'Stroji', 'FC_IND_MQ_E'= 'Rudarstvo in kamnolomi',
                          'FC_IND_FBT_E'= 'Hrana, pjače, tobak', 'FC_IND_PPP_E'= 'Papir, celuloza, tisk', 
                          'FC_IND_WP_E'= 'Les in lesni proizvodi', 'FC_IND_CON_E'= 'Gradbeništvo', 'FC_IND_TL_E'= 'Tekstil in usnje',
                          'FC_IND_NSP_E'= 'Drugo', 'FC_TRA_E'= 'Skupaj', 'FC_TRA_RAIL_E'= 'Železnica', 'FC_TRA_ROAD_E'= 'Ceste',
                          'FC_TRA_PIPE_E'= 'Cevovodni promet', 'FC_TRA_NSP_E'= 'Drugo', 'FC_OTH_CP_E'= 'Skupaj', 'FC_OTH_HH_E'= 'Skupaj', 
                          'FC_OTH_AF_E'= 'Kmetijstvo in gozdarstvo', 'FC_OTH_FISH_E'= 'Ribištvo', 'FC_OTH_NSP_E'= 'Drugo')
  
  
  data$Sektor <- str_replace_all(data$nrg_bal, dict_sector_type)
  data$Podsektor <- str_replace_all(data$nrg_bal, dict_sector_subtype)
  
  others_to_sum = c("FC_OTH_AF_E", "FC_OTH_FISH_E", "FC_OTH_NSP_E")
  
  
  df <- rbind(subset(data, select = -c(nrg_bal, siec,unit)),
              subset(data, nrg_bal%in%others_to_sum) %>%
                group_by(geo, time) %>%
                summarise(values = sum(values), Sektor = 'Drugi sektorji', Podsektor = 'Skupaj')
  )
  
  return(df)
  
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
      box(title = "Parametri pregleda",
        column(7, sliderInput("slider", "Izberite leto:", 1999, 2019, 1999)),
        column(4, selectInput("country", "Izberite državo:", names(country_codes), "Slovenija"))
      ))
    ,
    fluidRow(
      valueBoxOutput("choosen_country"),
      valueBoxOutput("choosen_year"),
      valueBoxOutput("total_consumption")
    ),
    
    fluidRow(
      box(
        title = "Poraba EE po sektorjih"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotlyOutput("consumption_per_sector", height = "300px")
      )
      ,box(
        title = "Poraba po podsektorjih izbranega sektorja"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("consumption_per_subsector", height = "300px")
      ) 
    )
  )

)


server <- function(input, output) {
  
  ###################
  # INFORMATION BOX #
  ###################
  
  data_consumption <- reactive({
    data <- get_consumption_data(input$country, input$slider)
    return(data)
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
      formatC(input$slider, format="d", big.mark='')
      ,"Izbrano leto"
      ,icon = icon("calendar-alt")
      ,color = "purple")  
  })
  
  output$total_consumption <- renderValueBox({ 
    valueBox(
      paste(formatC(filter(data_consumption(), Sektor == 'Skupna poraba')$values,
                    format="d", big.mark='.', decimal.mark = ","), " GWh")
      ,'Skupna poraba električne energije'
      ,icon = icon("bolt")
      ,color = "green")  
  })
  
  ###################
  # PLOT #
  ###################
  
  output$consumption_per_sector <- renderPlotly({
    data <- data_consumption()
    df <- data %>% filter(Sektor!='Skupna Poraba')
    fig_sectors <- plot_ly(type='pie', labels=df$Sektor, values=df$values, 
                           textinfo='label+percent',
                           insidetextorientation='radial')
    
    fig_sectors
  })
  
  
}

shinyApp(ui, server)