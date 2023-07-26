
library(sf)
library(tidyverse) #dplyr
library(leaflet)
library(plotly)
library(jsonify)
library(dplyr)

#1)
#Levanto el geojson con la informacion geografica de los barrios de CABA
barrios <- st_read("barrios_Ciudad.geojson")

#Ploteo los barrios
barrios %>%  leaflet() %>% addTiles() %>% addPolygons()


#2)
#descargo la informacion de la API de Transporte y agregamos la columna de geometria
colectivos <- st_as_sf(from_json("https://apitransporte.buenosaires.gob.ar/colectivos/vehiclePositionsSimple?client_id=02bc7547f78543a9a217480ec619bb82&client_secret=A690E6d9390f406ea4f5246b2b0C62F1"), coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

#paso la velocidad a kilometros por hora ya que viene en metros por segundos
colectivos <- colectivos %>% mutate(speed = speed * 3.6)

#Ploteo los colectivos en el mapa
colectivos %>% leaflet() %>% addTiles() %>% addCircles(label = ~route_short_name)

#3)
#joineo los dos datasets de manera de asignarle a cada colectivo el barrio en el que se encontraba 
#en el momento en que consulté a la API (descarto los colectivos que no se encotraban en CABA)
colectivos_CABA <- colectivos %>% st_join(barrios) %>% st_as_sf() %>% drop_na()

#top de los barrios con más unidades operando
colectivos_CABA_by_barrios <- colectivos_CABA %>% group_by(BARRIO) %>% summarise(densidad_bondis = n()) %>% as.data.frame() %>% arrange(desc(densidad_bondis))

top_n(colectivos_CABA_by_barrios, 5, densidad_bondis)

#los barrios con mayor densidad de colectivos ajustada por área
#asumo que el area de los barrios esta en m2 y la paso a km2
colectivos_CABA_by_barrios <- colectivos_CABA %>% group_by(BARRIO) %>% summarise(densidad_bondis = n(), AREA = mean(AREA)/1000000) %>% as.data.frame()

colectivos_CABA_by_barrios$Bondis_x_Area = colectivos_CABA_by_barrios$densidad_bondis / colectivos_CABA_by_barrios$AREA

top_n(colectivos_CABA_by_barrios%>% as.data.frame() %>% arrange(desc(Bondis_x_Area)) %>% drop_na, 5, Bondis_x_Area)

#barrios que poseen promedio de velocidad más alta
colectivos_CABA_by_barrios_vel <- colectivos_CABA %>% group_by(BARRIO) %>% summarise(vel_prom = mean(speed)) %>% as.data.frame() %>% arrange(desc(vel_prom)) %>% drop_na()


top_n(colectivos_CABA_by_barrios_vel, 5, vel_prom)


#Es necesario que haya por lo menos un gráfico o mapa (de lo que quiera)

#Colectivos en CABA en función de su velocidad
ggplot() +
  geom_sf(data = barrios, fill = "lightblue") +
  geom_sf(data = colectivos[barrios, ], aes(color = (-speed)))

#a)
#¿En qué barrio hay más unidades funcionando de la línea 166?
colectivos_CABA_by_barrios_166 <- colectivos_CABA[grep("166", colectivos_CABA$route_short_name),] %>% group_by(BARRIO) %>% summarise(cant_166 = n()) %>% as.data.frame() %>% arrange(desc(cant_166)) %>% drop_na()
                                                                                                                     
colectivos_CABA_by_barrios_166

top_n(colectivos_CABA_by_barrios_166, 1, cant_166)$BARRIO


#b)
#¿Cuál es el Metrobús que más colectivos por metro posee en ese instante? 
#Para esto explicite el buffer que utilizará para intersectar geometrías.

#levantamos el geojason con la informacion geografica de metrobuses
metrobus <- st_read("recorrido-de-metrobus.geojson")

#buffer
metrobus_buff = st_buffer(metrobus, dist = 10)

#Grafico los Metrobuses
metrobus_buff %>%  leaflet() %>% addTiles() %>% addPolygons()

esquema_metrobus <- ggplot(metrobus_buff) +
  geom_sf(aes(color = METROBUS))
ggplotly(esquema_metrobus)

#joineo con conlectivos y agrupo por metrobus

colectivos_en_metro_by_metro <- colectivos %>% st_join(metrobus_buff) %>% group_by(METROBUS) %>% summarise(densidad_bondis = n()) %>% arrange(desc(densidad_bondis))  %>% as.data.frame() %>% drop_na()

colectivos_en_metro_by_metro 

#calculo longitud de metrobus

long_metro <- metrobus %>% filter(COD_SENT >= 0) %>%group_by(METROBUS) %>% summarise(long_metrobus = sum(LONG))  %>% as.data.frame()

colectivos_en_metro_by_metro_2 <- merge(x=colectivos_en_metro_by_metro, y=long_metro, by="METROBUS")

#Creo una nueva columna con los colectivos x metro x Metrobus
colectivos_en_metro_by_metro_2$Colectivosxmetro = colectivos_en_metro_by_metro_2$densidad_bondis / colectivos_en_metro_by_metro_2$long_metrobus

colectivos_en_metro_by_metro_2 <- colectivos_en_metro_by_metro_2 %>% arrange(desc(Colectivosxmetro))

dropList <- c("geometry.x", "geometry.y")
colectivos_en_metro_by_metro_2 <- colectivos_en_metro_by_metro_2[, !colnames(colectivos_en_metro_by_metro_2) %in% dropList]
colectivos_en_metro_by_metro_2



#c)
#¿Cuántos colectivos están operando en el entorno de la Línea A de Subterráneos?

#levantamos el geojason con la informacion geografica de las lineas de subte
subtes <- st_read("subte_lineas.geojson")

#buffer
subtes_buff = st_buffer(subtes, dist = 10)

#Grafico los Metrobuses
subtes_buff %>%  leaflet() %>% addTiles() %>% addPolygons()

esquema_subtes <- ggplot(subtes_buff) +
  geom_sf(aes(color = LINEASUB))
ggplotly(esquema_subtes)


#Me quedo solo con lo referente a la linea A de subte y joineo con los colectivos

colectivos_entorno_subte_A <- colectivos %>% st_join(subtes_buff) %>% filter(LINEASUB == "LINEA A") %>% group_by(LINEASUB) %>% summarise(densidad_bondis = n()) %>% arrange(desc(densidad_bondis))  %>% as.data.frame() %>% drop_na()

colectivos_entorno_subte_A




