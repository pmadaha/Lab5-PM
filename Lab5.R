install.packages ("leaflet")
install.packages("leaflet.extras")
install.packages("spdep")
library(tidyverse)
library(terra)
library(sf)
library(leaflet)
library(leaflet.extras)
library(spdep)
library(tmap)

#Task 1

bmp <- read.csv("./BMPreport2016_landbmps.csv")
Counties<- read_sf("./County_Boundaries.shp")

Counties <- Counties %>%sf::st_transform(., "EPSG:4326")

countybmp<- bmp %>% group_by (GeographyName) %>% summarise(Totalcost = sum(Cost, na.rm = T))
countybmp<- countybmp %>% mutate(GEOID10 = stringr:: str_sub(GeographyName, 1, 5))
countybmp<- Counties %>% left_join(countybmp, by = "GEOID10")

breaks<-  classInt(countybmp$Totalcost, n=5, style = "equal")

colorpal = colorBin("YlOrRd", domain = countybmp$Totalcost, n=5)

leaflet(data = countybmp) %>%
 addTiles(providers$CartoDB.Positron) %>% 
  addPolygons( fillColor = ~ colorpal(Totalcost), 
               label = ~paste0 ("$", Totalcost),
               fillOpacity = 0.7,
               color = "white", 
               weight = 0.7) %>%
  addLegend(pal = colorpal,
            values = ~Totalcost,
            title = "Total BMP Costs", 
            position = "bottomright",
            breaks = 5)

#Task 2

Task_join<- sf::read_sf("./Task_join.shp")
AOI.projected <- Task_join %>% sf::st_transform (., "ESRI: 102010")

neighbors<- poly2nb(AOI.projected) 

Weightlst<- nb2listw(neighbors, style = "W", zero.policy = TRUE)

sl<- lag.listw(Weightlst, AOI.projected$B01001e34, zero.policy = TRUE) 


StateBound<- Task_join %>% group_by (STATEFP) %>% summarise()

AOI.mean<- mean(AOI.projected$B01001e34, na.rm = TRUE)

Lag.mean<- mean(sl, na.rm = TRUE)

LMTest<- localmoran(AOI.projected$B01001e34,Weightlst, zero.policy = TRUE)

LMTdf<- as.data.frame(LMTest)


Bonus<- AOI.projected %>% mutate(Quadrant = case_when(B01001e34 >= AOI.mean & sl >= Lag.mean ~ "HH", B01001e34 >= AOI.mean & sl < Lag.mean ~ "HL", B01001e34 < AOI.mean & sl < Lag.mean ~ "LL", B01001e34 < AOI.mean & sl >= Lag.mean ~ "LH"))
Bonus<- Bonus %>% mutate (p_value = LMTdf$`Pr(z != E(Ii))`)
aoilag.lm<- lm(Bonus$B01001e34 ~ Bonus$sl)
coef(aoilag.lm)
inf<- influence.measures(aoilag.lm)
infmtrx<- as.data.frame(inf$is.inf)
infrw<- apply(infmtrx, 1, any)
Bonus<- Bonus %>% mutate (is.inf=infrw)
significant<- Bonus [Bonus$is.inf,]

Bonus <- Bonus %>%sf::st_transform(., "EPSG:4326")
StateBound <- StateBound %>%sf::st_transform(., "EPSG:4326")

colorpal2 = colorFactor(palette = "Reds", domain = (Bonus$Quadrant)
colorpal3 = colorFactor(palette = "Set1", domain = StateBound$STATEFP)
leaflet(data = Bonus) %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "WI" ) %>% 
  addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "WGC")%>%
  addLayersControl(baseGroups = c("WI","OSM","WGC"),
                   overlayGroups = c ("State Boundaries")) %>% 
  addPolygons(data = StateBound,
              fillColor = ~ colorpal3(STATEFP),
              fillOpacity = 0,
              color= "Black",
              weight = 5,
              group = "State Boundaries") %>%
  addPolygons(fillColor = ~ colorpal2(Quadrant), 
              popup = ~paste0 ("County:",NAME, "<br>pvalue:", p_value),
              fillOpacity = 0.7,
              color = "white",   
              weight = 0.5) %>%
  addLegend(pal = colorpal2,
            values = ~Quadrant,
            title = "Quadrant Values", position = "bottomright")
 
  
 #Task 3

oh_counties<- read_sf ("./oh_counties.shp")
oh_parks <- read_sf ("./oh_parks.shp")
oh_places <- read_sf ("./oh_places.shp")
oh_rivers <- read_sf ("./tl_2023_39153_linearwater.shp")
oh_rivers2<- read_sf ("./tl_2022_39133_linearwater.shp")
oh_census<- read_csv ("./oh_counties_DP2020.csv")
oh_elev = rast("./neoh_dem.tif")

Oh_county_census<-left_join(oh_counties, oh_census, by = c("GEOIDFQ" = "geoid")) 
Pop_dens <- Oh_county_census %>% st_transform(.,"EPSG:4326" ) %>% mutate(land_area = as.numeric(st_area(.)/1e6), pop_density =  poptotal / land_area)  
NEpop_dens<- Pop_dens %>% dplyr::filter ( NAME %in% c("Ashland","Ashtabula", "Carroll", "Columbiana", "Cuyahoga", "Erie", "Geauga", "Holmes", "Huron", "Lorain", "Lake", "Medina", "Mahoning", "Portage", "Richland", "Stark", "Summit", "Tuscarawas","Trumbull", "Wayne"))
NEpop_dens_centroids <- st_centroid(NEpop_dens)
oh_elev_projected<- terra::project(oh_elev, crs(NEpop_dens))
spat_pop_vect<- vect(NEpop_dens)
pop_dens_elev<- terra::crop (oh_elev_projected, spat_pop_vect)
oh_parks<- st_transform(oh_parks, crs= st_crs(Pop_dens))
NEparks<- st_intersection(oh_parks, NEpop_dens)
st_crs(NEparks) == st_crs(NEpop_dens)

colorpal4<- colorFactor(palette = "Greys", domain = NEpop_dens$NAME)
colorpal5<- colorFactor(palette = "Greens", domain = NEparks$FEATTYPE)
colorpal6<- colorNumeric(palette = "YlOrRd", domain = NEpop_dens_centroids$pop_density)
 
     
   
leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "WTM") %>%
  addLayersControl(baseGroups = c("WGC"),
                   overlayGroups = c ("State Boundaries", "Type of Parks", "Population Density", "NEOH Counties")) %>% 
   addPolygons(data = NEpop_dens,
              fillOpacity = 0.3,
              color= "grey",
              weight = 3,
              group = "NEOH Counties") %>%
  addPolygons(data = NEparks,
              fillColor = ~ colorpal5(FEATTYPE), 
              popup = ~ FEATTYPE,
              fillOpacity = 0.7,
              color = ~colorpal5(FEATTYPE),   
              weight = 0.5,
              group = "Type of Parks") %>% 
  addCircleMarkers(data = NEpop_dens_centroids,
              fillColor = ~colorpal6(pop_density),
              weight = 1,
              color =colorpal6(pop_density),
              fillOpacity = 0.9,
              group = "Population Density",
              popup = ~paste ("Pop Density:", pop_density)) %>%
   addLegend(pal = colorpal5,
            values = ~ FEATTYPE,
            title = "Type of Parks", 
            position = "bottomright") %>%
  addLegend(pal = colorpal6,
            values = ~ pop_density,
            title = "Population Density",
            position = "bottomleft")


