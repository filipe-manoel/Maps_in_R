library(sf)
library(ggspatial)
library(raster)
library(tidyverse)
library(ggrepel)

coordenadas <- data.frame(Location = c("Altamira","Belterra","Birigui",'Campo_Mourao',
                           "Coimbra","Dourados","Goiania", "Janauba",
                           "Londrina","Lucas_do_Rio_Verde","Manaus",
                           "Nossa_Senhora_das_Dores","Palmital",
                           "Palotina","Paragominas","Patos_de_Minas",
                           "Planaltina","Primavera_do_Leste",
                           "Santo_Antonio_do_Aracangua",
                           "Sao_Raimundo_das_Mangabeiras","Sete_Lagoas",
                           "Sinop","Sobral","Sorriso","Teresina",
                           "Vicosa","Vilhena"),
                          Sigla = c('ALT','BEL','BIR','CM','COI','DOU','GOI','JAN',
                                    'LON','LCV','MAN','NSD','PMT','PLT','PAR',
                                    'PM','PLA','PRL','SAA','SRM','SLB','SIN',
                                    'SOB','SOR','TER','VIC','VIL'),
              lat = c(-3.1946,-2.69829,-21.2915055,-24.046,-20.856944,
                      -22.202369,-16.583047,-15.8037,-23.31028,-13.0634,
                      -3.048081,-10.4853,-22.7918,-24.2817,-2.96667,-18.582025,
                      -15.443390,-15.5597,-20.9338,-7.02209,-19.46583,
                      -11.86417,-3.68274,-12.5547,-5.081413,-20.7546,-12.7341),
              long = c(-52.2093,-54.8883,-50.3436312,-52.3838,-42.802778,
                       -54.798753,-49.291908,-43.3174,-51.16278,-55.921,
                       -59.888840,-37.2053,-50.205,-53.8404,-47.48333,-46.509526,
                       -47.625902,-54.2972,-50.4978,-45.4815,-44.24667,
                       -55.5025,-40.3512,-55.72578,-42.719806,-42.8825,-60.1446))


#setwd('G:\\Meu Drive\\Projetos\\Milho\\VCU')

br_est = shapefile("Mapas/BR_UF_2021.shp") ## Shapefile dos estados
br_bio = shapefile('Mapas/lm_bioma_250.shp')  ## Shapefile dos biomas

br_est = st_as_sf(br_est)

Amazonia = fortify(br_bio[br_bio$Bioma == "Amazônia",])
Caatinga = fortify(br_bio[br_bio$Bioma == "Caatinga",])
Cerrado = fortify(br_bio[br_bio$Bioma == "Cerrado",])
Mata_Atlantica = fortify(br_bio[br_bio$Bioma == "Mata Atlântica",])
Pampa = fortify(br_bio[br_bio$Bioma == "Pampa",])
Pantanal = fortify(br_bio[br_bio$Bioma == "Pantanal",])

br_bio = st_as_sf(br_bio)

ggplot(data = br_est)+
  geom_sf(fill = 'antiquewhite', show.legend = 'polygon')+
  annotation_scale(location = 'bl', width_hint = 0.3)+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.5,"cm"), pad_y = unit(0.7,"cm"),
                         style = north_arrow_nautical())+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = 'none')+
  geom_polygon(data = Amazonia, aes(x= long, y = lat, group = group),
               fill = "darkgreen", alpha = .3)+
  geom_polygon(data = Caatinga, aes(x= long, y = lat, group = group),
               fill = "gold1", alpha = .3)+
  geom_polygon(data = Cerrado, aes(x= long, y = lat, group = group),
               fill = "firebrick1", alpha = .3, show.legend = T)+
  geom_polygon(data = Mata_Atlantica, aes(x= long, y = lat, group = group),
               fill = "seagreen2", alpha = .3)+
  geom_polygon(data = Pampa, aes(x= long, y = lat, group = group),
               fill = "azure2", alpha = .3)+
  geom_polygon(data = Pantanal, aes(x= long, y = lat, group = group),
               fill = "yellowgreen", alpha = .3)+
  geom_point(data = coordenadas, aes(x = long, y = lat, color = Location))+
  labs(x = "Longitude", y = "Latitude", color = "")+
  geom_label_repel(data = coordenadas,aes(x = long, y = lat, label = Sigla), 
                   size = 3, box.padding = .5, max.overlaps = 13, fontface='bold')

