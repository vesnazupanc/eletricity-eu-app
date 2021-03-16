library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(eurostat)
library(stringr)
library(dplyr)
library(DT)


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
  
  df <- bind_rows(subset(data, Vir != 'Obnovljivi', select = c('geo', 'time', 'values', 'Vir')), res_temp)
  df <- df %>% group_by(time, Vir) %>% summarise(values = sum(values))
  
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


get_production_per_type_multiple <- function(country,year){
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
  
  df <- bind_rows(subset(data, Vir != 'Obnovljivi', select = c('geo', 'time', 'values', 'Vir')), res_temp)
  df <- df %>% group_by(geo, time, Vir) %>% summarise(values = sum(values))
  
  return(df)
  
}

get_consumption_per_sector_multiple <- function(country,year){
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

library(shiny)
library(shinydashboard)
library(shinyWidgets)

################ TABS ######################


#----------- Home Tab ------------####

home_tab <- tabItem(
  tabName = 'home',
  h1(strong("Dobrodošli v spletni aplikaciji Elektrika v Evropi!")),

  p("Spletna aplikacija je namenjena pregledu porabe in proizvodnje električne energije po posameznih državah v Evropi.
    Pogledate si lahko količino porabe po različnih sektorjih in količino proizvodnje glede na vir goriva.
    Podatki so na voljo od leta 1990 dalje in se dopolnjujejo za leto nazaj."),
  p("Vir podatkov: Eurostat"),
  br(),
  
  h3(strong("Zavihki")),
  
  h4(strong("Poraba in proizvodnja")),
  p("Tu si lahko pogledate porabo in proizvodnjo za posamezno državo in leto. Poraba je prikazana glede na sektorje, proizvodnja pa glede na vir goriva.
    Tako pri porabi kot pri proizvodnji, sta na voljo grafičen prikaz in tabela. Za lažjo izbiro zanimivih let, pa vam je lahko v pomoč graf porabe in proizvodnje za izbrano državo,
    ki je prikazan v zgornjem desnem grafu. Poleg porabe in proizvodnje pa je prikazan tudi podatek, koliko električne energije je izbana država v izbranem letu uvozila in izvozila iz sosenjih držav." ),

  h4(strong("Primerjalnik")),
  p("V zavihku Primerjalnih lahko primerjate med seboj dve državi za izbrano leto.
    Za bolj smiselno primerjavo so tu količine prikazane glede na povprečno število prebivalcev v izbranem letu.
    Na voljo sta dva grafična prikaza, glede na strukturo porabe in proizvodnje ter izpisane so ključne vrednosti v izbranem letu."),
 
  h4(strong("Izvozi podatke")),
  p("V kolikor si želite še sami narediti analizo na podatkih, ki so prikazani v aplikaciji pa si lahko pomagate z zavihkom Izvozi podatke.
    V zavihku si lahko izberete poljubne parametre (države in leta) ter za proizvodnjo ali porabo izvozite podatke v obliki csv datoteke.")
)

#----------- Consumption & Production Tab -----####

consprod_tab <- tabItem(
  tabName = 'consprod',
  
  fluidRow(
    box(title = "Parametri pregleda", width = 4, height = 350,
        sliderInput("year1", "Izberite leto:", 1990, as.integer(format(Sys.Date(), "%Y"))-1, 1990, sep=""),
        selectInput("country1", "Izberite državo:", names(country_codes), "Slovenija")),
    
    box(title = "Poraba in proizvodnja po letih", width = 8, height = 350,
        plotlyOutput("cons_prod_yearly"))
  ),
  
  fluidRow(
    valueBoxOutput("total_cons1", width = 3),
    valueBoxOutput("total_prod1", width = 3),
    valueBoxOutput("import1", width = 3),
    valueBoxOutput("export1", width = 3)
  ),
  
  fluidRow(
    tabBox(
      title = "Poraba po sektorjih",
      tabPanel(icon("chart-pie"),plotlyOutput("cons_per_sector_pie")),
      tabPanel(icon("table"), tableOutput("cons_per_sector_tbl"))
    ),
    
    tabBox(
      title = "Proizvodnja glede na vir goriva"
      ,tabPanel(icon("chart-pie"), plotlyOutput("prod_per_type_pie"))
      ,tabPanel(icon("table"), tableOutput("prod_per_type_tbl"))
    )
  )
)

#----------- Comparison Tab ------####

comparison_tab <- tabItem(
  tabName = "comparison",
  box(title = "Parametri pregleda", width = 12,
      column(width = 6, 
             sliderInput("year2", "Izberite leto:", 1990, as.integer(format(Sys.Date(), "%Y"))-1, 1990, sep="")),
      column(width = 6, 
             selectInput("country21", "Izberite prvo državo:", names(country_codes), "Nemčija"),
             selectInput("country22", "Izberite drugo državo:", names(country_codes), "Slovenija"))),
  
  fluidRow(
    valueBoxOutput("total_cons2", width = 3),
    valueBoxOutput("total_prod2", width = 3),
    valueBoxOutput("import2", width = 3),
    valueBoxOutput("export2", width = 3)
  ),
  
  fluidRow(
    box(title = "Primerjava porabe po sektorjih za izbrano leto",
        plotlyOutput("compare_cons_bar")
    ),
    
    box(title = "Primerjava proizvodnje glede na vir za izbrano leto",
        plotlyOutput("compare_prod_bar"))
    
  )
)

  


#----------- Download Tab --------####

download_tab <- tabItem(
  tabName = 'download',
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
        downloadButton("Download","Izvozi v csv")
        
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




################ DASHBOARD ##################

ui <- dashboardPage(
  
  dashboardHeader(title = "Elektrika v Evropi"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Domov", tabName = 'home', icon = icon("home")),
      menuItem("Poraba in proizvodnja", tabName = "consprod", icon = icon("plug")),
      menuItem("Primerjalnik", tabName = "comparison", icon = icon("exchange-alt")),
      menuItem("Izvozi podatke", tabName = "download", icon = icon("download"))
    ),
    tags$footer(HTML(paste("Avtorja aplikacije:", br() ,"Vesna Zupanc in Janez Bijec")), align = "center", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:70px; 
              color: white;
              padding: 10px;
              background-color: #367fa9;
              z-index: 1000;")
  ),
  
  dashboardBody(
    # Background
    tags$img(src = "https://images.freeimages.com/images/large-previews/ad6/light-bulb-1416824.jpg",
             style = 'position: absolute; opacity: 0.2;'),
    # Tabs
    tabItems(
      home_tab,
      consprod_tab,
      comparison_tab,
      download_tab
    )
  )
)




################ SERVER ################


server <- function(input, output) {
  
  
  #============= DATA AND VALUES ===============
  
  # Consumption and production tag
  data_cons_1 <- reactive({
    get_consumption_per_sector(input$country1)
  })
  
  val_total_cons_1 <- reactive({
    get_total_consumption(input$country1, input$year1)
  })  
  
  data_prod_1 <- reactive({
    get_production_per_type(input$country1)
  })
  
  val_total_prod_1 <- reactive({
    get_total_production(input$country1, input$year1)
  })
  
  val_impexp_1 <- reactive({
    get_import_export(input$country1, input$year1)
  })
  
  # Comparison tag
  data_cons_21 <- reactive({
    get_consumption_per_sector(input$country21)
  })
  
  data_prod_21 <- reactive({
    get_production_per_type(input$country21)
  })
  
  data_cons_22 <- reactive({
    get_consumption_per_sector(input$country22)
  })
  
  data_prod_22 <- reactive({
    get_production_per_type(input$country22)
  })
  
  pop21 <- reactive({
    get_avg_population(input$country21, input$year2)
  })
  
  pop22 <- reactive({
    get_avg_population(input$country22, input$year2)
  })
  
  
  val_total_cons_21 <- reactive({
    # get_total_consumption(input$country21, input$year2)
    num1 <- 1000*get_total_consumption(input$country21, input$year2)
    num2 <- pop21()
    format(round(num1/num2,digits=3),nsmall = 3)
  })  
  
  val_total_prod_21 <- reactive({
    num1 <- 1000*get_total_production(input$country21, input$year2)
    num2 <- pop21()
    format(round(num1/num2,digits=3),nsmall = 3)
  })
  
  val_total_cons_22 <- reactive({
    num1 <- 1000*get_total_consumption(input$country22, input$year2)
    num2 <- pop22()
    format(round(num1/num2,digits=3),nsmall = 3)
  })  
  
  val_total_prod_22 <- reactive({
    num1 <- 1000*get_total_production(input$country22, input$year2)
    num2 <- pop22()
    format(round(num1/num2,digits=3),nsmall = 3)
  })
  
  val_impexp_21 <- reactive({
    num1 <- 1000*get_import_export(input$country22, input$year2)
    num2 <- pop21()
    format(round(num1/num2,digits=3),nsmall = 3)
  })
  
  val_impexp_22 <- reactive({
    num1 <- 1000*get_import_export(input$country22, input$year2)
    num2 <- pop22()
    format(round(num1/num2,digits=3),nsmall = 3)
  })
  
  #============= CONSUMPTION&PRODUCTION TAG ===============
  
  # Value boxes
  
  output$total_cons1 <- renderValueBox({ 
    valueBox(
      paste(formatC(val_total_cons_1(), format="d", big.mark='.', decimal.mark = ","), "GWh"),
      'Skupna poraba električne energije',
      icon = icon("bolt"),
      color = "red")  
  })
  
  output$total_prod1 <- renderValueBox({ 
    valueBox(
      paste(formatC(val_total_prod_1(), format="d", big.mark='.', decimal.mark = ","), "GWh"),
      'Skupna proizvodnja električne energije', icon = icon("bolt"),
      color = "light-blue")  
  })
  
  output$import1 <- renderValueBox({ 
    valueBox(
      paste(formatC(val_impexp_1()['import'], format="d", big.mark='.', decimal.mark = ","), "GWh"),
      'Uvoz električne energije', icon = icon("arrow-left"),
      color = "yellow")   
  })
  
  output$export1 <- renderValueBox({ 
    valueBox(
      paste(formatC(val_impexp_1()['export'], format="d", big.mark='.', decimal.mark = ","), "GWh"),
      'Izvoz električne energije', icon = icon("arrow-right"),
      color = "olive")  
  })
  
  # Plots and tables
  
  output$cons_prod_yearly <- renderPlotly({
    data_cons <- data_cons_1()
    data_prod <- data_prod_1()
    
    cons_yearly <- data_cons %>% group_by(time) %>% summarise(values = sum(values)) #%>% rename(Poraba = values)
    cons_yearly$Krivulja <- "Poraba"
    prod_yearly <- data_prod %>% group_by(time) %>% summarise(values = sum(values)) # %>% rename(Proizvodnja = values)
    prod_yearly$Krivulja <- "Proizvodnja"
    data <- bind_rows(x = cons_yearly, y = prod_yearly)
    
    p <- plot_ly(type = 'scatter', mode = 'lines', height = 250)
    for (i in c("Poraba", "Proizvodnja")) {
      barve = c("Poraba"='grey', "Proizvodnja"='#0091D5')
      p <- add_trace(p,
                     data = data[data$Krivulja == i,],
                     legendgroup = i,
                     x = ~time, 
                     y = ~values, 
                     type = 'scatter', 
                     mode = 'lines', 
                     line = list(color = barve[i]),
                     name = i
      )
      p <- add_trace(p, 
                     data = data[(data$Krivulja == i) & (data$time == input$year1),],
                     legendgroup = i,
                     showlegend = F,
                     mode = 'markers',
                     marker = list(color = barve[i], size = 12),
                     x = ~time, 
                     y = ~values,
                     hoverinfo = 'none' )
    }
    
    p <- p %>%
      layout(hovermode = "x unified",
             yaxis = list(title = "Količina [GWh]"),
             xaxis = list(title = "Leto"), autosize=F)
    p
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
  
  output$cons_per_sector_tbl <- renderTable({
    data <- data_cons_1()
    df <- data %>% filter(time == input$year1)
    df <- subset(df, select = c(Sektor, values))
    df <- df %>% mutate(Poraba=paste(round(values,2),"GWh"),
                        Procent=paste0(round(values/sum(values)*100,2),"%"))
    df <- df[order(df$values, decreasing = TRUE),]
    df <- subset(df, select = -c(values))
    df
  })
  

  output$prod_per_type_pie <- renderPlotly({
    data <- data_prod_1()
    df <- data %>% filter(time == input$year1)
    fig <- plot_ly(type='pie', labels=df$Vir, values=df$values, 
                   textinfo='label+percent',
                   insidetextorientation='radial',
                   hovertemplate = "%{label}: <br>Proizvodnja: %{value} GWh<extra></extra>",
                   automargin=T)
    
    fig <- fig %>% layout(showlegend = FALSE)
    fig
  })
  
  output$prod_per_type_tbl <- renderTable({
    data <- data_prod_1()
    df <- data %>% filter(time == input$year1)
    df <- subset(df, select = c(Vir, values))
    df <- df %>% mutate(Proizvodnja=paste(round(values,2),"GWh"),
                        Procent=paste0(round(values/sum(values)*100,2),"%"))
    df <- df[order(df$values, decreasing = TRUE),]
    df <- subset(df, select = -c(values))
    df
  })
  
  
  #============= COMPARISON TAG ================
  
  ##### information box #####
  
  output$total_cons2 <- renderValueBox({ 
    valueBox(
      "Poraba",
      HTML(paste0(input$country21, ": ", val_total_cons_21(), " MWh\\preb",
             br(),
             input$country22, ": ", val_total_cons_22(), "MWh\\preb")),
      icon = icon("bolt"),
      color = "red")  
  })
  
  output$total_prod2 <- renderValueBox({ 
    valueBox(
      "Proizvodnja",
      HTML(paste0(input$country21, ": ", val_total_prod_21(), " MWh\\preb",
             br(),
             input$country22, ": ", val_total_prod_22(), "MWh\\preb")),
      icon = icon("bolt"),
      color = "light-blue")  
  })
  
  
  output$import2 <- renderValueBox({ 
    valueBox(
      "Uvoz",
      HTML(paste0(input$country21, ": ", val_impexp_21()['import'], " MWh\\preb",
                  br(),
                  input$country22, ": ", val_impexp_22()['import'], " MWh\\preb")),
      icon = icon("arrow-left"),
      color = "yellow")   
  })
  
  output$export2 <- renderValueBox({ 
    valueBox(
      "Izvoz",
      HTML(paste0(input$country21, ": ", val_impexp_21()['export'], "MWh\\preb",
             br(),
             input$country22, ": ", val_impexp_22()['export'], "MWh\\preb")),
      icon = icon("arrow-left"),
      color = "olive")  
  })
  
  
  # Plots and tables 
  
  output$compare_cons_bar <- renderPlotly({
    data <- data_cons_21() %>% filter(time==input$year2)
    data2 <- data_cons_22() %>% filter(time==input$year2)

    data$values <- 1000*data$values/pop21()
    data2$values <- 1000*data2$values/pop22()
    
    data$values2 <- data2$values
    
    fig <- plot_ly(data, x = ~Sektor, y = ~values, type = 'bar',
                   name = paste(input$country21),colors = c("red","green"))
    fig <- fig %>% add_trace(y = ~ values2, name = paste(input$country22))
    fig <- fig %>% layout(yaxis = list(title = 'Količina [MWh/preb]'), barmode = 'group')
    
    fig
  })
  
  output$compare_prod_bar <- renderPlotly({
    data <- data_prod_21() %>% filter(time==input$year2)
    data2 <- data_prod_22() %>% filter(time==input$year2)
    
    data$values <- 1000*data$values/pop21()
    data2$values <- 1000*data2$values/pop22()
    
    data$values2 <- data2$values
    
    fig <- plot_ly(data, x = ~Vir, y = ~values, type = 'bar', name = paste(input$country21),colors = c("red","green"))
    fig <- fig %>% add_trace(y = ~ values2, name = paste(input$country22))
    fig <- fig %>% layout(yaxis = list(title = 'Količina [MWh/preb]'), barmode = 'group')
    
    fig
  })
  
  
  #============= DOWNLOAD TAG ================
  
  data_download <- reactive({
    
    if(input$spremenljivka=="Poraba"){
      Data <- get_consumption_per_sector_multiple(c(input$drzava),c(input$obdobje))
      colnames(Data) <- c("Drzava","Leto","Sektor","Poraba[GWh]")
      Data
    } else {
      Data <- get_production_per_type_multiple(c(input$drzava),c(input$obdobje))
      Data <- Data[,c(1,2,4,3)]
      colnames(Data) <- c("Drzava","Leto","Vir","Proizvodnja[GWh]")
      Data
    }
  })
  
  output$tabela <- renderDataTable({
    datatable(data_download(), options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20)
    ))
    
  })
  
  output$Download <- downloadHandler(
    filename = function() {
      paste("Podatki-",input$spremenljivka,"-",Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_download(), file, row.names = FALSE)
    }
  )
}



shinyApp(ui, server)