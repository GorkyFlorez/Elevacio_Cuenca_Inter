
library(tidyterra)
library(ggplot2)
library(dplyr)
library(scales)
library(terra)
library(sf)
temp_rast <- rast("RASTER/Cuenca.tif")

Cuenca_MDD  = st_read("SHP/Cuenca.geojson")  %>% st_as_sf()
Cuenca   <- st_transform(Cuenca_MDD ,
                         crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
# Mostrar una sola capa
names(temp_rast)

ggplot() +
  geom_spatraster(data = temp_rast, aes(fill = Cuenca)) +
  coord_sf(crs = 3857) +
  scale_fill_hypso_c()

r <- temp_rast %>%
  mutate(Cuenca = pmax(0, Cuenca ))

autoplot(r) +
  theme_minimal()

## Crear un efecto de sombra de colina

slope <- terrain(r, "slope", unit = "radians")
aspect <- terrain(r, "aspect", unit = "radians")
hill <- shade(slope, aspect, 30, 270)
# normalizar los nombres
names(hill) <- "shades"

# Hillshading, pero necesitamos una paleta
pal_greys <- hcl.colors(1000, "Grays")

ggplot() +
  geom_spatraster(data = hill) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA)

# Utilizar un vector de colores
index <- hill %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)

# Obtener cols
vector_cols <- pal_greys[index]

# Necesidad de evitar el remuestreo
# y no usar aes

# Gradiente regular
grad <- hypso.colors(10, "dem_poster")
autoplot(r) +
  scale_fill_gradientn(colours = grad_hypso, na.value = NA)

# Gradiente de Hypso
grad_hypso <- hypso.colors2(10, "dem_poster")
autoplot(r) + scale_fill_gradientn(colours = grad, na.value = NA)



hill_plot <- ggplot() +
  geom_spatraster(data = hill, fill = vector_cols, maxcell = Inf,alpha = 1)

hill_plot

# Prueba algunas opciones, pero tenemos que ser conscientes de los valores de nuestra trama

r_limits <- minmax(r) %>% as.vector()

# Redondeado a 500 inferior y superior
r_limits <- c(floor(r_limits[1] / 10), ceiling(r_limits[2] / 10)) * 10

# Y hacer que el valor mínimo sea 0.
r_limits <- pmax(r_limits, 157)

# Compara
minmax(r) %>% as.vector()
#> [1]    0 2481
r_limits
#> [1]    0 2500


# Ahora vamos a divertirnos con las balanzas de tidyterra

elevt_test <- ggplot() +
  geom_spatraster(data = r)

# Create a helper function

plot_pal_test <- function(pal) {
  elevt_test +
    scale_fill_hypso_tint_c(
      limits = r_limits,
      palette = pal
    ) +
    ggtitle(pal) +
    theme_minimal()
}

plot_pal_test("etopo1_hypso")
plot_pal_test("dem_poster")
plot_pal_test("spain")
plot_pal_test("pakistan")
plot_pal_test("utah_1")
plot_pal_test("wiki-2.0_hypso")


myload_fonts <- function(fontname, family,
                         fontdir = tempdir()) {
  fontname_url <- utils::URLencode(fontname)
  fontzip <- tempfile(fileext = ".zip")
  download.file(paste0("https://fonts.google.com/download?family=", fontname_url),
                fontzip,
                quiet = TRUE,
                mode = "wb"
  )
  unzip(fontzip,
        exdir = fontdir,
        junkpaths = TRUE
  )

  # Load fonts
  paths <- list(
    regular = "Regular.ttf",
    bold = "Bold.ttf",
    italic = "Italic.ttf",
    bolditalic = "BoldItalic.ttf"
  )


  namefile <- gsub(" ", "", fontname)
  paths_end <- file.path(
    fontdir,
    paste(namefile, paths, sep = "-")
  )


  names(paths_end) <- names(paths)

  sysfonts::font_add(family,
                     regular = paths_end["regular"],
                     bold = paths_end["bold"],
                     italic = paths_end["italic"],
                     bolditalic = paths_end["bolditalic"]
  )

  return(invisible())
}

# Adjust text size
base_text_size <- 30

Elevaba= ggplot() +
  geom_spatraster(data = hill, fill = vector_cols, maxcell = Inf,alpha = 1)+
  # Evitar el remuestreo con maxcell
  geom_spatraster(data = r, maxcell = Inf) +
  scale_fill_hypso_tint_c(
    limits = r_limits,
    palette = "utah_1",
    alpha = 0.4,
    labels = label_comma(),
    # Para la leyenda utilizo saltos personalizados
    breaks = c(seq(157, 304, 20)))
  # Guía para el cambio
  guides(fill = guide_legend(
    title = "   m.",
    direction = "horizontal",
    nrow = 1,
    keywidth = 1.75,
    keyheight = 0.5,
    label.position = "bottom",
    title.position = "right",
    override.aes = list(alpha = 1)
  )) +
  labs(
    title = "Elevación de la cuenca de Madre de Dios",
    subtitle = "Mezcla de tintes hipsométricos y de colinas",
    caption = paste0(
      "@gflorezc usando los paquetes R tidyterra, ggplot2, geodata.",
      " Datos: Misión Topográfica de Radar del Transbordador (SRTM)"
    )
  ) +
  theme_minimal(base_family = "notoserif") +
  theme(
    plot.background = element_rect("grey97", colour = NA),
    plot.margin = margin(20, 20, 20, 20),
    plot.caption = element_text(size = base_text_size * 0.5),
    plot.title = element_text(face = "bold", size = base_text_size * 1.4),
    plot.subtitle = element_text(
      margin = margin(b = 10),
      size = base_text_size
    ),
    axis.text = element_text(size = base_text_size * 0.7),
    legend.position = "bottom",
    legend.title = element_text(size = base_text_size * 0.8),
    legend.text = element_text(size = base_text_size * 0.8),
    legend.key = element_rect("grey50"),
    legend.spacing.x = unit(0, "pt")
  )



  ggplot() +
    geom_spatraster(data = hill, fill = vector_cols, maxcell = Inf,alpha = 1)+
    # Evitar el remuestreo con maxcell
    geom_spatraster(data = r, maxcell = Inf) +
    scale_fill_hypso_tint_c(
      limits = r_limits,
      palette = "utah_1",
      alpha = 0.4,
      labels = label_comma(),
      # Para la leyenda utilizo saltos personalizados
      breaks = c(seq(157, 304, 20)))
library(ggspatial)
  library(hrbrthemes)
  library(gcookbook)
  library(tidyverse)

  # current verison
  packageVersion("hrbrthemes")
  ## [1] '0.8.6'
  update_geom_font_defaults(font_rc_light)

  base_text_size <- 30


  library(ggspatial)
  library(raster)
  library(elevatr)
  elev = get_elev_raster(Cuenca, z=12)
  plot(Poligo_alt)
  Poligo_alt    <- crop(elev, Cuenca)                           #
  Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Cuenca)


  slopee    = terrain(Poligo_alt  , opt = "slope")
  aspecte    = terrain(Poligo_alt, opt = "aspect")
  hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

  hill.p        <-  rasterToPoints(hille)
  hill.pa_      <-  data.frame(hill.p)
  colores = c(
    "#8e9aaf",#celeste
    "#dda15e", # maroon
    "#faedcd")#amarillo pastel


  Geo_data       <-  rasterToPoints(  Poligo_alt)
  Geo_data_frame <-  data.frame(Geo_data)
  colnames(Geo_data_frame) <- c("x","y", "alt")


bivariate_color_scale <- tibble(
    "3 - 3" = "#92AE59", # high inequality, high income
    "2 - 3" = "#0F7435",
    "1 - 3" = "#006147", # low inequality, high income
    "3 - 2" = "#D4A554",
    "2 - 2" = "#A13E00", # medium inequality, medium income
    "1 - 2" = "#9E1000",
    "3 - 1" = "#FFFFFF", # high inequality, low income
    "2 - 1" = "#ACACAC",
    "1 - 1" = "#87564A" # low inequality, low income
  ) %>%
    gather("group", "fill")

  bivariate_color_scale %<>%
    separate(group, into = c("gini", "mean"), sep = " - ") %>%
    mutate(gini = as.integer(gini),
           mean = as.integer(mean))

  legend <-ggplot() +
    geom_tile(data = bivariate_color_scale, mapping = aes(
      x = gini, y = mean, fill = fill)) +
    scale_fill_identity() +
    labs(x = "mayor altura ⟶️",
         y = "menor altura ⟶️") +
    theme( axis.title = element_text(size = 6),
           axis.title.x=element_text(color="black"),
           axis.text = element_blank(),
           panel.background = element_rect(fill = "white"),
           axis.title.y=element_text(color="black")) +

    coord_fixed()
  legend
legend.grob <- ggplotGrob(legend)

  grad <- hypso.colors(10, "dem_poster")


  tbl = rasterToPoints(Poligo_alt, spatial = F)
  tbl = as_tibble(tbl)
  tbl = setNames(tbl, c("x", "y", "year"))
  tbl = filter(tbl, year > 0)

  summ = tbl %>%
    group_by(year)%>%
    summarise(count =n())%>%
    ungroup ()

  summ = mutate(summ, meters =count *20, has = meters /10000)
  summ = dplyr::select(summ, year,has)

  summ$has = summ$has + summ1$has
  summ$has =round(summ$has,2)
  # lets count proportions
  summ$fraction = summ$has/sum(summ$has)
  summ
  # Compute the cumulative proportions (top of each rectangle)
  summ$ymax = cumsum(summ$fraction)
  summ
  # Compute the bottom of each rectangle
  summ$ymin = c(0, head(summ$ymax, n=-1))
  summ
  #compute label position
  summ$labelPosition= (summ$ymax+summ$ymin)/2
  summ
  #get data label
  summ$label= paste0(summ$Categoria ,"\n Valor=", summ$has)
  summ
  library(ggrepel)
  Ha_total = sum(summ$has)

  summ$Porcentaje =  summ$has*100/Ha_total
  summ$Porcentaje =round(summ$Porcentaje,2)

  Pastel=ggplot(summ,aes(ymax=ymax,ymin=ymin,xmax=4, xmin=2, fill=year ))+
    geom_rect(alpha=0.8)+
    coord_polar(theta="y")+
    xlim(c(0,4))+
    theme_void()+
    theme(legend.position = "none")+
    scale_fill_gradientn(colours = grad)+
    annotate(geom = "text", x = 0.30, y = 0.75, hjust = 0, vjust = 1,
             label = "Elevacion",size = 2, family="serif", color = "black",  fontface="italic")

  Pastel
legend.Pastel <- ggplotGrob(Pastel)

Elevaba=ggplot()+
  geom_sf(data = Cuenca, fill=NA, color="black", size=0.8)+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt))+
  scale_fill_gradientn(colours = grad, na.value = NA,
                       breaks = c(seq(157, 304, 20)))+
  theme_classic()+
  theme_ipsum_rc(grid="X")+
  guides(fill = guide_legend(
    title = " msnm.",
    direction = "horizontal",
    nrow = 1,
    keywidth = 1.75,
    keyheight = 0.5,
    label.position = "bottom",
    title.position = "right",
    override.aes = list(alpha = 1)
  ))+
  theme(legend.position = "bottom",

        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),

        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11,  face = "italic", family="serif"),
        plot.caption = element_text(size = 9, family="serif", face = "italic"),

        plot.background = element_rect(fill = "white", color = NA),

        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotation_custom(grob= legend.grob, xmin = -69.2, xmax = -69.3, ymin =-12.85, ymax=-12.7)+
  annotation_custom(grob= legend.Pastel, xmin = -69.4, xmax = -69.3, ymin =-12.85, ymax=-12.7)+

  labs(x = 'Longitud', y = 'Latitud',
       title="Mapa de elevacion en la Region Hidrografica del Amazonas",
       subtitle="Cuenca Intercuenca Medio Alto Madre de Dios en el departamento  \nde Madre de Dios - Peru, Mezcla de tintes hipsométricos y de colinas\nresolución de 12.5m ",
       caption="Gorky Florez  'Datos: Misión Topográfica de Radar del Transbordador (SRTM)'")



ggsave(plot = Elevaba ,"MAPA/Elevacion3.png",  units = "cm", width = 29, #ancho
       height = 21, #Largo
       dpi = 1200)























