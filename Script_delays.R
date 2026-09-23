#------------------------------------------------------------------------------#
#                       CODIGO DEL ARTICULO DE RETRASOS                        #
#                                                                              #
#                                  Elvis Diaz                                  #
#                                                                              #
#------------------------------------------------------------------------------#

# Instalar librerias
  paquetes <- c("tidyverse", "vioplot", "ggridges", "patchwork", 
                "tidyr","dplyr", "readr")
  install.packages(paquetes)

# Cargar librerías
  library(tidyverse)
  library(vioplot)
  library(ggridges)
  library(patchwork)   
  library(tidyr)
  library(dplyr)
  library(readr)
  library(corrplot)
  library(purrr)
  library(FSA)
## Bases de datos  ----

# Llamando a la base de datos
  url_rabia <- "https://raw.githubusercontent.com/elvisd1089/Delays_Rabies/refs/heads/main/Data_Rabies_Delays.csv"
  
# Cargando base de datos 
  df <- read.csv(url_rabia, sep = ";") 

## Renombrando las base de datos ----
  
# Uniendo fechas
  df$Report<- paste(df$complaint_day, df$complaint_month,df$complaint_year, sep="/") # Reporte
  df$InitialSigns<- paste(df$day_sympoms_appeared, df$month_sympoms_appeared,df$year_sympoms_appeared, sep="/") # Inicio de signos
  df$Shipment<-paste(df$brain_sampling_shipping_day, df$brain_sampling_shipping_month,df$brain_sampling_shipping_year, sep="/") # envío de muestra
  df$LabDx<- paste(df$Confirmation_day, df$Confirmation_month,df$Confirmation_year, sep="/") # Diagnóstico de laboratorio
  df$FocusControl1<-paste(df$day_containent_started, df$month_containent_started,df$year_containent_started, sep="/") # Control de foco
  
# creando procesos
  df <- df %>%
    mutate(InitialSigns = as.Date(InitialSigns, format="%d/%m/%Y"),
           Report = as.Date(Report, format="%d/%m/%Y"),
           Shipment = as.Date(Shipment, format="%d/%m/%Y"),
           LabDx = as.Date(LabDx, format="%d/%m/%Y"),
           FocusControl1 = as.Date(FocusControl1, format="%d/%m/%Y")) %>%
    mutate(ReporteLag = Report - InitialSigns,
           SsaludLag = Shipment - Report, 
           LaboratorioLag = LabDx - Shipment,
           ControlfocoLag = FocusControl1 - LabDx)
  
# Cambiando valores de procesos de caracter a numéricos
  df$ReporteLag <- as.numeric(df$ReporteLag)
  df$SsaludLag <-  as.numeric(df$SsaludLag)
  df$LaboratorioLag <- as.numeric(df$LaboratorioLag)
  df$ControlfocoLag <- as.numeric(df$ControlfocoLag)
  
# Estandarizar datos atipicos
  df$ControlfocoLag[df$ControlfocoLag < 0] <- 0  
  df$SsaludLag[df$SsaludLag < 0] <- 0  
  df$LaboratorioLag[df$LaboratorioLag > 15] <- NA 
  
  df <- df %>%
    mutate(totalreport1 = rowSums(across(c(ReporteLag, SsaludLag, LaboratorioLag, ControlfocoLag)), na.rm = TRUE))

## Analisis de medidas de tendencia central y dispersión ----   
  tabla_descriptiva <- df %>%
    select(ReporteLag, SsaludLag, LaboratorioLag, ControlfocoLag) %>%
    summarise(
        across(
        everything(),
        list(
          n       = ~sum(!is.na(.)),
          media   = ~mean(., na.rm = TRUE),
          mediana = ~median(., na.rm = TRUE),
          DE      = ~sd(., na.rm = TRUE),
          Q1      = ~quantile(., 0.25, na.rm = TRUE),
          Q3      = ~quantile(., 0.75, na.rm = TRUE),
          IQR     = ~IQR(., na.rm = TRUE),
          minimo  = ~min(., na.rm = TRUE),
          maximo  = ~max(., na.rm = TRUE)))) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("Variable", "Medida"),
      names_sep = "_",
      values_to = "Valor") %>%
    pivot_wider(
      names_from = Medida,
      values_from = Valor)
  
  tabla_descriptiva
  

# Convertir a datos longitudinales para medir los procesos  
  datos_l <- df %>%
            pivot_longer(cols = c(ReporteLag, SsaludLag, LaboratorioLag, ControlfocoLag),
                 names_to = "tipo_demora", values_to = "tiempo_demora")
  
  datos_l$tipo_demora <- as.factor(datos_l$tipo_demora) 
  datos_l$tipo_demora <- factor(datos_l$tipo_demora , levels = c("ControlfocoLag", "LaboratorioLag",
                                                                 "SsaludLag", "ReporteLag"))
  datos_l$Microred <- as.factor(datos_l$Microred)
  
  excluir <- c("YANAHUARA", "TIABAYA", "CIUDAD BLANCA")
  datos_ll <- subset(datos_l, !Microred =="YANAHUARA"&!Microred =="TIABAYA"&!Microred =="CIUDAD BLANCA") 

## Elaboracion figura 3 ----   
  ggplot(datos_l, aes(x = tiempo_demora, y = tipo_demora, fill = tipo_demora)) +
    ggridges::geom_density_ridges(alpha = .5, color = "white", scale = 0.97, na.rm = TRUE, , from = -0.85, to = 10) +
    scale_fill_manual(values = c("ReporteLag" = "tomato", "SsaludLag" = "deepskyblue1", 
                                 "LaboratorioLag" = "aquamarine3", "ControlfocoLag" = "goldenrod1"),
                      breaks = c("ReporteLag", "SsaludLag" , "LaboratorioLag", "ControlfocoLag"),
                      labels = c("Reporte", "Envío", "Diagnóstico", "Control")) +
    stat_summary(fun = "mean", geom = "pointrange", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x),
                 shape = 21, size = 1.2, color = "black", position = position_nudge(y = 0)) +
    scale_x_continuous(limits = c(-1, 10), breaks = seq(0, 10, by = 1)) +
    theme_ridges()
  
## Analisis figura 4 ----
  correlacion_procesos <- cor(
    df[, c("ReporteLag", "SsaludLag", "LaboratorioLag", "ControlfocoLag")],
    use = "complete.obs" )
  
  print(correlacion_procesos)
  
  corrplot(correlacion_procesos, method = "color", addgrid.col = "grey30", type = "lower")
  
## Elaboración figura 5 ----
  media_por_año <- df %>%
    group_by(Year) %>%
    summarise(
        ReporteLag = mean(ReporteLag,na.rm = TRUE),
        SsaludLag = mean(SsaludLag,na.rm = TRUE),
        LaboratorioLag = mean(LaboratorioLag,na.rm = TRUE),
        ControlfocoLag = mean(ControlfocoLag,na.rm = TRUE)) %>%
    pivot_longer(cols = -Year, names_to = "Variable", values_to = "Media") %>%
    mutate(Variable = factor(Variable, levels = c("ReporteLag", "SsaludLag", "LaboratorioLag", "ControlfocoLag")))
  
  media_por_año <- media_por_año %>%
    group_by(Year) %>%
    mutate(start = cumsum(lag(Media, default = 0)), end = cumsum(Media)) %>%
    ungroup()
  
  ggplot(media_por_año) +
    geom_segment(aes(x = start, xend = end, y = Year, yend = Year, color = Variable),alpha = .6, size = 12) +
    geom_text(aes(x = (start + end) / 2, y = Year, label = round(Media, 2)), size = 4, vjust = 0.5, color = "black") +
    scale_color_manual(values = c("ReporteLag" = "tomato", "SsaludLag" = "deepskyblue1", 
                                  "LaboratorioLag" = "aquamarine3", "ControlfocoLag" = "goldenrod")) +
    theme_minimal() +
    theme(axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))
  
  
## Elaboración figura 6 ----
  media_por_microred <- df %>%
    filter(!Microred %in% c("YANAHUARA", "TIABAYA", "CIUDAD BLANCA")) %>%
    group_by(Microred) %>%
    summarise(
      ReporteLag = mean(ReporteLag, na.rm = TRUE),
      SsaludLag = mean(SsaludLag, na.rm = TRUE),
      LaboratorioLag = mean(LaboratorioLag, na.rm = TRUE),
      ControlfocoLag = mean(ControlfocoLag, na.rm = TRUE),
      Cantidad = n(),
      .groups = "drop") %>%
    mutate(Total = ReporteLag + SsaludLag + LaboratorioLag + ControlfocoLag) %>%
    mutate(Microred = reorder(Microred, Total)) %>%
    pivot_longer(
      cols = c(ReporteLag, SsaludLag, LaboratorioLag, ControlfocoLag),
      names_to = "Variable",
      values_to = "Media") %>%
    mutate(
      Variable = factor(Variable, levels = c("ReporteLag", "SsaludLag", "LaboratorioLag", "ControlfocoLag")))
  media_por_microred <- media_por_microred %>%
    group_by(Microred) %>%
    mutate(start = cumsum(lag(Media, default = 0)),
           end = cumsum(Media)) %>%
    ungroup()
  
  ggplot(media_por_microred) +
    geom_segment(aes(x = start, xend = end, y = Microred, yend = Microred, color = Variable),alpha = .6, size = 10) +
    geom_text(aes(x = (start + end) / 2, y = Microred, label = round(Media, 2)), size = 4, vjust = 0.5, color = "black") +
    scale_color_manual(values = c("ReporteLag" = "tomato", "SsaludLag" = "deepskyblue1", 
                                  "LaboratorioLag" = "aquamarine3", "ControlfocoLag" = "goldenrod")) +
    scale_x_continuous(breaks = seq(0, 10, by = 1)) +
    theme_minimal() +
    theme(axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))
  
## Resultados tabla 2 ----
# Kruskal-Wallis independiente para cada proceso   
  procesos <- c("ReporteLag", "SsaludLag", "LaboratorioLag", "ControlfocoLag")
  
  resultados_kw <- map_dfr(procesos, function(proceso) {
    
    datos <- df %>%
      filter(!is.na(.data[[proceso]]),!is.na(Year))
    
    resultado <- kruskal.test(datos[[proceso]] ~ datos$Year)
    
    data.frame(
      Proceso = proceso,
      Chi2 = as.numeric(resultado$statistic),
      gl = as.numeric(resultado$parameter),
      p = resultado$p.value)})
  
  resultados_kw
  
# Post-hoc de Dunn con corrección de Bonferroni 
  procesos <- c("ReporteLag", "SsaludLag", "LaboratorioLag", "ControlfocoLag")
  
  resultados_dunn <- map_dfr(procesos, function(proceso) {
    
    datos <- df %>%
      select(Year, all_of(proceso)) %>%
      filter(!is.na(Year), !is.na(.data[[proceso]]))
    
    resultado <- dunnTest(
      datos[[proceso]] ~ as.factor(datos$Year),
      data = datos, method = "bonferroni")
    
    resultado$res %>%
      mutate(Proceso = proceso) %>%
      select(Proceso, Comparison, Z, P.unadj, P.adj)})
  
  resultados_dunn
  
# Comparaciones significativas
  resultados_dunn %>%
    filter(P.adj < 0.05)
  
## Elaboración figura 1 ----
  install.packages("devtools") 
  devtools::install_github("ropensci/rnaturalearthhires")
  
  install.packages(c("patchwork", "ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", 
                    "rnaturalearthhires", "ggspatial", "dplyr", "tidyverse", "raster"))
  library(patchwork)
  library(sf)
  library(ggplot2)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(rnaturalearthhires)
  library(dplyr)
  library(ggspatial)
  library(tidyverse)
  library(raster)
  
# Obtenemos datos del mapa mundial
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
# Filtramos datos de Sudamerica
  sudamerica <- subset(world, continent == "South America")
  
# Mapa de Perú con sus departamentos
  peru_deptos <- ne_states(country = "Peru", returnclass = "sf")
  
# Filtrar Perú del mapa de Sudamérica
  peru_sf <- sudamerica %>% filter(admin == "Peru")
  
# Filtrar Arequipa
  arequipa <- peru_deptos %>% filter(name == "Arequipa")
  
# Generando mapa de sudamerica
  sud_amer <- ggplot() +
    geom_sf(data = sudamerica, fill = "gray90", color = "gray60", size = 0.2, alpha = 0.5) +
    geom_sf(data = peru_sf, fill = "pink", color = "gray25", size = 0.2, , alpha = 0.7) +
    geom_sf(data = arequipa, fill = "red", color = "gray25", size = 0.2,  alpha=0.5) + 
    annotate("text", x = -81, y = -15, label = "Perú", size = 6) +
    theme_void() +
    theme(legend.position = "none",
          panel.border = element_rect(colour = "gray25", fill = NA, size = 1)) +
    coord_sf(xlim = c(-83, -34), ylim = c(-53, 11)) +
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      style = ggspatial::north_arrow_fancy_orienteering)
  
  sud_amer
  
# Generando mapa de Arequipa  
  setwd("/Users/pmacs") # Abrir con su acceso local
  
  # Sudamérica
  south_america <- ne_countries(scale = "medium", continent = "South America", returnclass = "sf")
  # Peru  
  peru <- subset(south_america,admin=="Peru")
  # Arequipa 
  per <- ne_states(country = "Peru", returnclass = "sf")
  arequipa <- per[per$name == "Arequipa", ]
  # Distritos  
  peru_sf <- readRDS("Downloads/gadm36_PER_2_sf.rds")
  microredes <- read.csv("Downloads/microredes_AQP.csv",sep=";")
  provincias_sf <- st_read("Documents/GitHub/peru_spatial_data/data_original/02_boundaries/InstitutoGeograficoNacional/PROVINCIAS.shp")
  distritos_sf <- st_read("Downloads/Distrito/Distrito_INEI_2017.shp")
  
  arequipa_prov <- provincias_sf %>% filter(DEPARTAMEN == "AREQUIPA" & PROVINCIA == "AREQUIPA")
  caylloma_prov <- provincias_sf %>% filter(DEPARTAMEN == "AREQUIPA" & PROVINCIA == "CAYLLOMA") 
  
  arequipa_dis <- distritos_sf  %>% filter( nombprov == "AREQUIPA" )
  caylloma_dis <- distritos_sf %>% filter( nombprov == "CAYLLOMA") 
  
  distritos_esp_aqp <- c("MIRAFLORES", "SACHACA", "MOLLEBAYA", "SOCABAYA", "QUEQUEÑA", "JOSE LUIS BUSTAMANTE Y RIVERO", 
                         "CHARACATO", "JACOBO HUNTER", "YANAHUARA", "UCHUMAYO", "SABANDIA", "TIABAYA", "PAUCARPATA",
                         "AREQUIPA", "ALTO SELVA ALEGRE", "CERRO COLORADO", "CAYMA", "MARIANO MELGAR", "YURA")
  
  distritos_esp_cay <- c("MAJES")
  
  distritos_sel_aqp<- arequipa_dis %>% filter(nombdist %in% distritos_esp_aqp)
  distritos_sel_cay<- caylloma_dis %>% filter(nombdist %in% distritos_esp_cay)
  
 aqp_plot <-  ggplot() +
              geom_sf(data = arequipa, fill = "hotpink1", color = "gray", lwd = 0.3, alpha = 0.9) +
              geom_sf(data = arequipa_prov, fill = "hotpink1", color = "gray", lwd = 0.6, alpha = 0.9) +
              geom_sf(data = caylloma_prov, fill = "hotpink1", color = "gray", lwd = 0.6, alpha = 0.9) +
              geom_sf(data = distritos_sel_aqp, fill = "lightcyan2", color = "grey", lwd = 0.4) +
              geom_sf(data = distritos_sel_cay, fill = "burlywood1", color = "grey", lwd = 0.4) +
              theme_void() +
    annotation_scale(location = "bl", width_hint = 0.4, height = unit(0.3, "cm"),
                     pad_x = unit(10, "cm"),pad_y = unit(-0.1, "cm")) +
    annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering,
                           height = unit(1.4, "cm"), width = unit(1.4, "cm"), pad_x = unit(19.5, "cm"), pad_y = unit(19.3, "cm"))
 aqp_plot
 
 
 
# Uniendo mapas  
 aqp_plot + sud_amer
 