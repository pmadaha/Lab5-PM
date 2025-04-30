install.packages ("leaflet")
install.packages("leaflet.extras")
install.packages("spdep")
library(tidyverse)
library(terra)
library(sf)
library(leaflet)
library(leaflet.extras)
library(spdep)

#Task 1

bmp <- read.csv("./BMPreport2016_landbmps.csv")
Counties<- read_sf("./County_Boundaries.shp")

countybmp<- bmp %>% group_by (GeographyName) %>% summarise(Tcost = sum(Cost, na.rm = TRUE), Totalcost =round(Tcost, digits =2))
countybmp<- countybmp %>% mutate(GEOID10 = stringr:: str_sub(GeographyName, 1, 5))
countybmp<- Counties %>% left_join(countybmp, by = "GEOID10") %>%  sf::st_transform(., "EPSG:4326")

countybmp <- countybmp %>%sf::st_transform(., "EPSG:4326")
st_crs(countybmp)

min(countybmp$Totalcost, na.rm = TRUE)
max(countybmp$Totalcost, na.rm = TRUE)

breaks<- c(0,33000000,66000000,99000000,132000000, 165000000)

colorpal = colorBin("YlOrRd", domain = countybmp$Totalcost, bins=breaks)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = countybmp,
              fillColor = ~ colorpal(Totalcost), 
              label = ~paste0 ("$", Totalcost),
              popup = ~paste0 (NAME10),
              fillOpacity = 0.7,
              color = "black", 
              weight = 0.7) %>%
  addLegend(pal = colorpal,
            values = countybmp$Totalcost,
            title = "Total BMP Costs", 
            position = "bottomright")

#Task 2

Task_join<- sf::read_sf("./Task_join.shp")
AOI.projected <- Task_join %>% sf::st_transform (., "ESRI: 102010")
StateBound<- Task_join %>% group_by (STATEFP) %>% summarise()

neighbors<- poly2nb(AOI.projected) 
Weightlst<- nb2listw(neighbors, style = "W", zero.policy = TRUE)
sl<- lag.listw(Weightlst, AOI.projected$B01001e34, zero.policy = TRUE) 

AOI.mean<- mean(AOI.projected$B01001e34, na.rm = TRUE)
Lag.mean<- mean(sl, na.rm = TRUE)
LMTest<- localmoran(AOI.projected$B01001e34,Weightlst, zero.policy = TRUE)
LMTdf<- as.data.frame(LMTest)

Bonus<- AOI.projected %>% mutate(Quadrant = case_when(B01001e34 >= AOI.mean & sl >= Lag.mean ~ "HH", B01001e34 >= AOI.mean & sl < Lag.mean ~ "HL", B01001e34 < AOI.mean & sl < Lag.mean ~ "LL", B01001e34 < AOI.mean & sl >= Lag.mean ~ "LH"))
Bonus<- Bonus %>% mutate (p_value = LMTdf$`Pr(z != E(Ii))`)
aoilag.lm<- lm(Bonus$B01001e34 ~ sl)
coef(aoilag.lm)
inf<- influence.measures(aoilag.lm)
infmtrx<- as.data.frame(inf$is.inf)
infrw<- apply(infmtrx, 1, any)
Bonus<- Bonus %>% mutate (is.inf=infrw)
significant<- Bonus [Bonus$is.inf,]

Bonus <- Bonus %>%sf::st_transform(., "EPSG:4326")
StateBound <- StateBound %>%sf::st_transform(., "EPSG:4326")

colorpal2 = colorFactor(palette = "Reds", domain = (Bonus$Quadrant))
                        
colorpal3 = colorFactor(palette = "Set1", domain = (StateBound$STATEFP))
                        
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
              weight = 4,
              group = "State Boundaries") %>%
  addPolygons(fillColor = ~ colorpal2(Quadrant), 
              popup = ~paste0 ("County:",NAME, "<br>pvalue:", p_value),
              fillOpacity = 0.7,
              color = "white",   
              weight = 0.5) %>%
  addLegend(pal = colorpal2,
            values = ~Quadrant,
            title = "Quadrant Values", 
            position = "bottomright")
 
  
 #Task 3

#Looking at the relationship with park availability and population density 

oh_counties<- read_sf ("./oh_counties.shp")
oh_parks <- read_sf ("./oh_parks.shp")
oh_census<- read_csv ("./oh_counties_DP2020.csv")
oh_elev = rast("./neoh_dem.tif")

Oh_county_census<-left_join(oh_counties, oh_census, by = c("GEOIDFQ" = "geoid")) 
Pop_dens <- Oh_county_census %>% st_transform(.,"EPSG:4326" ) %>% mutate(land_area = as.numeric(st_area(.)/1e6), pdensity =  poptotal / land_area,  pop_density =round(pdensity, digits =2))  #projected based of leaflets CRS, mutate area field and find the pop density using area and total population
NEpop_dens<- Pop_dens %>% dplyr::filter ( NAME %in% c("Ashland", "Cuyahoga", "Eerie", "Geauga", "Lorain", "Lake", "Medina", "Portage", "Stark", "Summit", "Wayne")) #filter based on NE counties according to google. I also removed counties outside the DEM to make the map neater.
NEpop_dens_centroids <- st_centroid(NEpop_dens) #Created centroids because I wanted to visualize population density based of circles and not the whole polygon.

oh_elev_projected<- terra::project(oh_elev, crs(NEpop_dens))
spat_pop_vect<- vect(NEpop_dens)
pop_dens_elev<- terra::crop (oh_elev_projected, spat_pop_vect)  #crop the raster to the vector and discover elevation values
pop_dens_elev_simplified <- aggregate(pop_dens_elev, fact = 2, fun = mean) #Simplified the raster by reducing the resolution, because the data was too big to be represented in the leaflet


oh_parks<- st_transform(oh_parks, crs= st_crs(Pop_dens))
oh_parks<- oh_parks %>% mutate(parea = as.numeric(st_area(.)/1e6), park_area =round(parea, digits =2)) #Finding the total area of the parks
NEparks<- st_intersection(oh_parks, NEpop_dens)
st_crs(NEparks) == st_crs(NEpop_dens)
Cnt_parks<- NEparks %>% group_by(GEOID) %>% summarise (ParkArea = sum(park_area)) #summarize the total area of parks within each county 
NEpop_dens<-st_join (NEpop_dens, Cnt_parks) #Populating this information within each polygon to have them available for presenting it on the map 

#double down in transforming the projection to satify the leaflet's projection 

NEpop_dens<- NEpop_dens %>% st_transform(.,"EPSG:4326" )
NEpop_dens_centroids <- NEpop_dens_centroids %>% st_transform(.,"EPSG:4326" )
NEparks<- NEparks %>% st_transform(.,"EPSG:4326" )


colorpal4<- colorFactor(palette = "Greys", domain = NEpop_dens$NAME)
colorpal5<- colorFactor(palette = "Greens", domain = NEparks$FEATTYPE)
colorpal6<- colorNumeric(palette = "YlOrRd", domain = NEpop_dens_centroids$pop_density)
colorpal7<- colorNumeric(palette = "YlGnBu", domain = values (pop_dens_elev_simplified))
    
   
leaflet() %>%
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "CDBP") %>%
  addLayersControl(baseGroups = c("CDBP"),
                   overlayGroups = c ("NEOH Raster", "Type of Parks", "Population Density", "NEOH Counties")) %>%
  setView(lng = -81.77, lat = 41.7, zoom = 10) %>%
  addRasterImage(pop_dens_elev_simplified, 
                 opacity = 0.2,
                 colors = colorpal7,
                 group = "NEOH Raster") %>%
  addPolygons(data = NEpop_dens,
              fillOpacity = 0,
              color= "grey",
              weight = 3,
              popup = ~paste0 ("County:", NAME, "<br> Park Area(Km2):", ParkArea) ,
              group = "NEOH Counties") %>%  
  addPolygons(data = NEparks,
              fillColor = ~ colorpal5(FEATTYPE), 
              popup = ~ FEATTYPE,
              fillOpacity = 0.7,
              color = ~colorpal5(FEATTYPE),   
              weight = 0.5,
              group = "Type of Parks") %>% 
  addCircleMarkers(data = NEpop_dens_centroids,
              radius = ~ sqrt(pop_density) / 1,
              fillColor = ~colorpal6(pop_density),
              weight = 1,
              color = ~colorpal6(pop_density),
              fillOpacity = 0.9,
              group = "Population Density (Number of People per km2",
              popup = ~paste0 ("Pop Density(km2):",pop_density)) %>%
  addCircles( lng = -81.61, lat = 41.3,
              radius = 17000,
              color = "black",
              weight = 2,
              popup = "Highest Concentration of Park Area and Population Density") %>%
  addLegend(pal = colorpal5,
            values = NEparks$FEATTYPE,
            title = "Type of Parks", 
            position = "bottomright") %>%
  addLegend(pal = colorpal6,
            values = NEpop_dens_centroids$pop_density,
            title = "Population Density",
            position = "bottomleft") %>%
  addLegend (pal = colorpal7,
             values = values(pop_dens_elev_simplified),
             title = "Elevation",
             position = "topright")

#Q1 : The labs for this semester was very challenging, but i have learned that patience and attention to detail is very detrimental for this class. I learned how to manipulate data and generate maps, with the opportunity to manipulate every tiny detail through code. What I enjoyed the most is being able to ensure accuracy through R especially when working with large data sets, because I normally tend to go astray not knowing if what I am doing is accurate or not. Some of the labs were kind of confusing, I misinterpreted the wording of some of the instructions and questions a lot which resulted to me taking more time doing tasks, but that's a problem on my end and not being fully in tune of the vocabulary being used.
#Q2: One thing I added to my leaflet was a circle that indicates the part of NE Ohio with the highest concentration of Park Area and Population Density 

 

