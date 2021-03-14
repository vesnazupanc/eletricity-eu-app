library(eurostat)
library(stringr)
library(dplyr)

get_consumption_data <- function(country, year){
  table_id = "nrg_cb_e"
  list_geo = c(country)
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
  
  dict_sector_subtype = c('NRG_E'= 'Skupaj', 'NRG_CM_E'= 'Premogovniki', 'NRG_OIL_NG_E'= 'naprave za črpanje nafte in zemeljskega plina', 
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


# PORABA PO SEKTORJIH

table_id = "nrg_cb_e"
list_geo = c('BE', 'BG', 'CZ', 'DK', 'DE', 'EL', 'ES', 'FR', 'HR', 'IT',
             'LU', 'HU', 'NL', 'AT', 'PL', 'PT', 'RO', 'SI', 'SK', 'SE',
             'NO', 'UK', 'ME', 'MK', 'AL', 'RS', 'BA', 'UA')

list_geo = c("DE")

list_import_export = c('IMP', 'EXP')

list_datatypes = c('NRG_E', 'NRG_CM_E', 'NRG_OIL_NG_E', 'NRG_PF_E', 'NRG_CO_E', 
                   'NRG_BKBPB_E', 'NRG_GW_E', 'NRG_BF_E', 'NRG_PR_E', 'NRG_NI_E', 'NRG_CL_E', 
                   'NRG_LNG_E', 'NRG_BIOG_E', 'NRG_GTL_E', 'NRG_CPP_E', 'NRG_NSP_E', 'FC', 
                   'FC_IND_E', 'FC_IND_IS_E', 'FC_IND_CPC_E', 'FC_IND_NFM_E', 'FC_IND_NMM_E', 
                   'FC_IND_TE_E', 'FC_IND_MAC_E', 'FC_IND_MQ_E', 'FC_IND_FBT_E', 'FC_IND_PPP_E', 
                   'FC_IND_WP_E', 'FC_IND_CON_E', 'FC_IND_TL_E', 'FC_IND_NSP_E', 'FC_TRA_E', 
                   'FC_TRA_RAIL_E', 'FC_TRA_ROAD_E', 'FC_TRA_PIPE_E', 'FC_TRA_NSP_E', 
                   'FC_OTH_CP_E', 'FC_OTH_HH_E', 'FC_OTH_AF_E', 'FC_OTH_FISH_E', 'FC_OTH_NSP_E')

data <- get_eurostat(table_id, filters = list(geo=c("DE"), nrg_bal=list_datatypes, time=c(2019)), time_format = "num")


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

dict_sector_subtype = c('NRG_E'= 'Skupaj', 'NRG_CM_E'= 'Premogovniki', 'NRG_OIL_NG_E'= 'naprave za črpanje nafte in zemeljskega plina', 
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



library(plotly)

df_i <- df %>% filter(time == 2018)
df_sectors <- df %>% filter(Sektor!='Skupna Poraba')
fig_sectors <- plot_ly(type='pie', labels=df_sectors$Sektor, values=df_sectors$values, 
                       textinfo='label+percent',
                       insidetextorientation='radial')

fig_sectors

# Dodamo skupno količino za druge sektorje: Kmetijstvo in gozdarstvo, Ribištvo, Drugo






# TRANSLATION
# > setNames(c("a","b"), c("A","B"))
# A   B 
# "a" "b" 
# > c("a" = "A", "b"="B")
# a   b 
# "A" "B" 
# > str_replace_all(c("a","b"), c("a" = "A", "b"="B"))
# [1] "A" "B"


