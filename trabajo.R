###Librerias 
library(tidyverse)
library(scales)
library(lubridate)

##Numero de victimas mortales por mes (2014-2023)
datos_muertos30<- "./Datos/Datos_muertos30.csv"
datos_muertos30<- rio::import(datos_muertos30)
datos_muertos30 <- datos_muertos30 %>%
  mutate(Periodo = factor(Periodo, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")))


p1<- ggplot(datos_muertos30, aes(x = Periodo, y = Victimas_mortales, group = Año, color = factor(Año))) +
  geom_line() +
  geom_point() +
  labs(title = "Número de víctimas mortales por mes (2014-2023)",
       x = "Mes",
       y = "Número de víctimas", color = "Año",  caption = "Fuente: EpData") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1


### Muertos en carretera durante el verano accidentes de tráfico 
muertes_verano<- "./Datos/muertes_verano.csv"
datos2<- rio::import(muertes_verano)
datos2<- drop_na(datos2)


p3<-ggplot(datos2, aes(x = factor(Año), y = Muertos, fill = Muertos)) +
  geom_bar(stat = "identity", color = "white", width = 0.8) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Número de muertos en carretera durante el verano en accidentes de tráfico",
       x = "Año",
       y = "Número de muertos", caption = "Fuente: DGT") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none") + scale_y_continuous(breaks = seq(0, max(datos2$Muertos), by = 200))  
p3



###Mapa

datos3<- "./Datos/46689 (2).csv"
datos3<- rio::import(datos3)



#######MAPA
library(sf)
library(tidyverse)
library(pjpv.curso.R.2022)



### En la columna Total queremos poner un punto en vez de una coma 

datos3 <- datos3%>% mutate(Total = stringr::str_replace(Total, "," , "." ))

#- Cambiamos el nombre de las columnas 

datos3<- datos3%>% rename(Tasa_Mortalidad = Total) %>% rename(CCAA = Comunidad_Autonomas)

###Separamos la columna de CCAA en dos Columnas, una que muestre el código y otra el nombre de la Comunidad Autonoma  

datos3<- datos3 %>%
  tidyr::separate(CCAA, sep = " ",
                  into = c("ine_ccaa", "ine_ccaa.n"), extra = "merge")
datos3<-  drop_na(datos3) #quitamos el total nacional, ya que no se van a utilizar

datos3<- datos3 %>% filter(Edades == "Todas las edades") %>% filter( Sexo =="Total") %>% filter(Periodo == "2021") ##cogemos Todas la Edades, del sexo queremos el total y elegimos el año más reciente que es 2021



###Cargamos las geometrías y elegimos las variables que nos interesan
df_geo_prov <- pjpv.curso.R.2022::LAU2_prov_2020_canarias
df_geo_prov <- df_geo_prov %>% select(ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n)

##Agregamos geometrías de las Comunidades Autonomas 

df_geo_ccaa <- df_geo_prov %>% group_by(ine_ccaa, ine_ccaa.n) %>% summarize()
df <- left_join(df_geo_ccaa, datos3, by = c("ine_ccaa" = "ine_ccaa"))
names(df)

##Calculamos los Centroides
library(sf)
df_geo_ccaa <- cbind(df_geo_ccaa, st_coordinates(st_centroid(df_geo_ccaa$geometry)))

##Juntamos los datos  con las geometrías 
df<- left_join(df_geo_ccaa, datos3, by = c("ine_ccaa" = "ine_ccaa"))
df$Tasa_Mortalidad <- as.numeric(df$Tasa_Mortalidad)

p4 <- ggplot(df) +
  geom_sf(aes(fill = Tasa_Mortalidad), color = "white", size = 0.2) +
  scale_fill_gradient(low = "white", high = "navyblue", name = "Tasa de Mortalidad") +  
  labs(title = "Tasa de Mortalidad debido a accidentes de tráfico por CCAA",
       subtitle = "2021", 
       caption = "Fuente: INE") +
  theme_minimal() +
  theme(legend.position = "bottom",  
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "italic"),  
        plot.caption = element_text(size = 10, hjust = 0.5))  
p4




# Crear el gráfico interactivo, muertes según el tipo de vía 

datos4<- "./Datos/muertes_tipo_via.csv"
datos4<- rio::import(datos4) 
datos4<- datos4 %>% select(!Periodo)
library(plotly)

p4 <- ggplot(datos4, aes(x = Año)) +
  geom_line(aes(y = Autopista, color = "Autopista"), size = 2) +
  geom_line(aes(y = Autovia, color = "Autovia"), size = 2, linetype = "dashed") +
  geom_line(aes(y = Carretera_convencional, color = "Carretera Convencional"), size = 2, linetype = "dotted") +
  labs(title = "Número de muertos en accidentes de tráfico\nsegún el tipo de vía", x = "Año", y = "Número de personas", caption = "Fuente: EpData") +
  scale_color_manual(name = "Tipo de Vía", values = c("Autopista" = "#4e79a7", "Autovia" = "#f28e2b", "Carretera Convencional" = "#59a14f")) +
  scale_y_continuous(breaks = seq(0, max(datos4$Carretera_convencional), 500), limits = c(0, max(datos4$Carretera_convencional) + 500)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 8),  
    axis.text.y = element_text(size = 8),  
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

grafico_interactivo <- ggplotly(p4)


grafico_interactivo

###Muertes según el tipo de veículo 
library(gganimate)
library(transformr)
library(tidyverse)

datos5<- "./Datos/Muertes_tipo_vehiculo.csv"
datos5<- rio::import(datos5)
datos5<- datos5 %>% select(!Periodo)
datos_longer<- pivot_longer(datos5, cols = -Año, names_to = "Tipo_Vehiculo", values_to = "Victimas")
datos_longer$Año <- as.factor(datos_longer$Año)

anim <- ggplot(datos_longer, aes(x = Año, y = Victimas, group = Tipo_Vehiculo, color = Tipo_Vehiculo)) +
  geom_line(size = 1, linetype = "solid") +
  geom_point(size = 2) +
  transition_reveal(as.integer(Año)) +  
  labs(title = "Muertes en accidentes de tráfico por tipo de vehículo",
       subtitle = "(Turismos, Motocicletas, Ciclomotores)",
       x = "Año",
       y = "Número de Víctimas", caption = "Fuente: EpData") +
  theme_minimal() +
  theme(legend.position = "top", 
        plot.margin = margin(1, 1, 1, 1, "cm")) + 
  scale_color_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  guides(color = guide_legend(title = "Tipo de vehiculo")) +
  ylim(0, 4000)
anim

####Ciclistas fallecidos en accidentes de tráfico 

library(viridis)

datos6 <- "./Datos/ciclistas_fallecidos.csv"
datos6 <- rio::import(datos6)

datos6$Año <- as.factor(datos6$Año)

p6 <- ggplot(datos6, aes(x = Año, y = Ciclistas_muertos)) +
  geom_bar(stat = "identity", color = "white", size = 0.5, fill = "skyblue") +  
  geom_text(aes(label = Ciclistas_muertos), vjust = -0.5, size = 3) +
  labs(title = "Ciclistas muertos en accidentes de tráfico",
       x = "Año",
       y = "Número de ciclistas",
       caption = "Fuente: DGT") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "aliceblue"),  
        plot.background = element_rect(fill = "aliceblue")) +  
  scale_fill_viridis_d()

p6 


### Evolución de las vivtimas de tráfico según ek tipo de sustancia detectada 
datos7<- "./Datos/Sustancias.csv"
datos7<- rio::import(datos7)
datos7<- datos7 %>% select(!Periodo)
datos_longer7<- pivot_longer( cols = 2:4, datos7, names_to = "Sustancia", values_to = "Porcentaje")
datos_longer7 <- datos_longer7 %>%
  mutate(Porcentaje = as.numeric(gsub(",", ".", Porcentaje)))


d_2010<- datos_longer7 %>%
  filter(Año == 2010)

p_2010 <- ggplot(d_2010, aes(x = "", y = Porcentaje, fill = Sustancia)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Porcentaje), y = Porcentaje), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3) +
  coord_polar(theta = "y") +
  scale_y_continuous(breaks = NULL) + 
  theme_void() +  
  labs(fill = "Sustancias", y = "Porcentaje")+ ggtitle("2010") + guides(fill = FALSE)



d_2015<- datos_longer7 %>%
  filter(Año == 2015)

p_2015<- ggplot(d_2015, aes(x = "", y = Porcentaje, fill = Sustancia)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Porcentaje), y = Porcentaje), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3) +
  coord_polar(theta = "y") +
  scale_y_continuous(breaks = NULL) + 
  theme_void() +  
  labs(fill = "Sustancias", y = "Porcentaje") + ggtitle("2015") + guides(fill = FALSE)


d_2019<- datos_longer7 %>%
  filter(Año == 2019)

p_2019 <- ggplot(d_2019, aes(x = "", y = Porcentaje, fill = Sustancia)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Porcentaje), y = Porcentaje), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3) +
  coord_polar(theta = "y") +
  scale_y_continuous(breaks = NULL) + 
  theme_void() +  
  labs(fill = "Sustancias", y = "Porcentaje") + ggtitle("2019") + guides(fill = FALSE)


d_2022<- datos_longer7 %>%
  filter(Año == 2022)

p_2022 <- ggplot(d_2022, aes(x = "", y = Porcentaje, fill = Sustancia)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Porcentaje), y = Porcentaje), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3) +
  coord_polar(theta = "y") +
  scale_y_continuous(breaks = NULL) + 
  theme_void() +  
  labs(fill = "Sustancias", y = "Porcentaje") + ggtitle("2022")


library(patchwork)
library(gridExtra)
library(cowplot)
wrap<- wrap_plots(p_2010, p_2015, p_2019, p_2022, 
                  ncol = 2, nrow = 2,
                  widths = c(1, 1), heights = c(1, 1)) +
  plot_annotation(title = "Evolución de las Víctimas de tráfico según el tipo de sustancia detectada", caption = "Fuente: Instituto Nacional de Toxicología y Ciencias Forenses")
wrap 


###Victimas por rango de edad y dia de la semana 
rm(list = ls())
datos8<- "./Datos/datos8.csv"
datos8<- rio::import(datos8)
datos8<- datos8 %>%
  select(!Periodo)

datos_long8<- pivot_longer(datos8, cols = 3:4, names_to = "Tipo_Dia", values_to = "Rango_Edad")

datos_long8$Parámetro <- factor(datos_long8$Parámetro, levels = unique(datos_long8$Parámetro))

p8 <- ggplot(datos_long8, aes(x = Parámetro, y = Rango_Edad, fill = Tipo_Dia)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Conductores víctimas mortales de tráfico\n por rango de edad y día de la semana ",
       subtitle = "Año 2022",
       x = "Rango de Edad",
       y = "%",
       fill = "Tipo de día", caption = "Fuente:EpData") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 
p8 
