################################################################################
# Statische und interaktive Karten (Vorlage)
# Autor: Helge Emmler
# R Version: 4.0.3
# Letztes Update: 22.01.2021
################################################################################

# (1) Präambel----

# Pakete laden
pac <- "pacman" %in% rownames(installed.packages())
if(pac == FALSE) install.packages("pacman"); rm(pac)
library(pacman)
p_load("stringr", "haven", "sp", "rgdal", "leaflet", "styler", 
       "geosphere", "htmltools", "ggplot2", "ggmap", "rgeos", "maptools", 
       "openxlsx","extrafont", "htmlwidgets", "leaflet.extras", "dplyr",
       "scales", "widgetframe") # rgeos vor maptools!
loadfonts()

# (2) Definitionen----

# Ebene? 
ebene <- "KRS" # KRS, AMR, LAN
key <- "AGS" # analog zu ebene: AGS, AMR, GEN

# Dateinamen
# Name für statische Kreis-Karte
name_s <- paste0(Sys.Date(), "_wsi_statisch_", ebene)
# Name für interaktive Kreis-Karte
name_i <- paste0(Sys.Date(), "_wsi_statisch_", ebene)

# Farben für Regionen (zulässig: lila, pink, grau, tuerkis, blau)
col_s <- "pink"
col_i <- "pink"

# (3) Schritte----

# Funktionen einlesen

dir.create("./Output/", showWarnings=F)
dir.create("./Output/Karten", showWarnings=F)

source("./Funktionen/funktionen_karten.R", encoding="UTF-8")

# shapefile Download (nur wenn nicht vorhanden)
if(!("shapefiles" %in% dir("./Shapefiles"))) {
  download_shapefile(shape="amr") # krs_neu, krs_alt (2016), amr (Arbm-reg.)
  download_shapefile(shape="krs_neu") # krs_neu, krs_alt (2016), amr (Arbm-reg.)
}

# Automatische Ausführung:
n <- 1:length(dir("./Schritte"))
sapply(dir("./Schritte", full.names=T)[n], source, encoding="UTF-8")

# (4) Session Info----
writeLines(
  capture.output(sessionInfo()), 
  con=paste0("./Session_Info/sessionInfo_",format(Sys.time(), "%y%m%d"), ".txt")
)




