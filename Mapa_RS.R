#Mapas no R

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthhires)
install.packages("rnaturalearthhires")
install.packages("ggspatial")
library(ggspatial)

devtools::install_github("AndySouth/rnaturalearthhires")


# Baixar arquivos de mapa
BRA = ne_states(
  country = "Brazil",
  returnclass = "sf"  #sf = simple features
)
plot(BRA)


# Evidenciar o estado do Riogrande do Sul

RS = BRA[BRA$name_en == "Rio Grande do Sul", ]
plot(RS)


# Escolha as cidades

city = c("Dom Feliciano (DF)", "Lavras do Sul (LS)", "São Jerônimo (SJ)")
lat = c(-30.6139,  -30.8498, -30.31504)
lon = c(-52.19315, -54.0283, -51.92515)

dat = data.frame(city, lat, lon)


#Plot the graph

p1= ggplot()+
  geom_sf(data=RS, color= "black", fill="floralwhite") + #plotar os single features
  geom_point(data=dat,               
  aes(x=lon, y=lat, color = city)) +  #plotar as cidades
  labs(x= "Longitude",
       y= "Latitude") +
  labs(color='Environments') +
  cowplot::theme_cowplot()+
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 'dashed',
                                        size = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill=NA,color = 'black'),
        panel.ontop = F)
p1


# Add scale and North arrow
p1 = p1 +
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    pad_x = unit(3.05, "in"), pad_y = unit(4.2, "in"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(3.32, "in"), pad_y = unit(3.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )


pdf("C:/Users/LENOVO/OneDrive/PARCEIROS/Kaio/CMPC/CMPC_lite/figures/fig-coordinates-RS.pdf")
p1
dev.off()



