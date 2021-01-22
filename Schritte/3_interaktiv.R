
# Hintergrunddaten laden -----

# Annahme über Dateipfad siehe funktionen/funktionen_karten.R
geo <- load_shapefile(path=NULL, level=ebene, leaflet=T)
geo <- merge(geo, dat, by=key) # dat siehe 1_read.R

# Farbenpalette: Woran geknüpft?
colfunc <- colorFactor(farben(col_i), domain=geo$gruppe, ordered=T)
geo$color <- colfunc(geo$gruppe)

# Ländergrenzen (für später, um sie einzuzeichnen)
if(ebene %in% c("KRS", "AMR")) {
  geo_land <- load_shapefile(path=NULL, level="LAN", leaflet=T)
}

geo$hinweis <- NA # Platzhalter für weitere Hinweise aus Quelldaten

# Karte zeichnen-------

# Karte
fig <- leaflet_basis(geo) %>% 
  leaflet_text("Knapper Titel", size=20) %>%
  leaflet_text(lat=55.25, 
               "Untertitel, in dem etwas mehr Information steht", size=15) %>%
  leaflet_legend() %>%
  leaflet_logo() %>%
  leaflet_quelle(Text="Quelle: <br> Autor der Quelle") %>%
  leaflet_anmerkung(Text="Mehr Informationen erhalten Sie per Mausklick auf die Länder!") %>%
  leaflet_polygons(
    variablen=c("Gesamt", "Männer", "Frauen"),
    headers=c("Kategorie I", "Kategorie II", "Kategorie III"),
    k=3 # Nachkommastellen
  ) 

# Ländergrenzen (nur bei Kreisen, Bezirken, Arbeitsmarktregionen sinnvoll)
if(ebene %in% c("KRS", "AMR")) {
  fig <- fig %>%
    addPolygons(
      data = geo_land,
      color = "white", weight = 2, smoothFactor = 0.5, 
      opacity = 1.0, fillOpacity = 1.0,
      fill = F
    )
}

# Leaflet-Stil anwenden und in HTML überführen----
fig$dependencies <- list(
  htmlDependency(
    name = "leaflet_hbs"
    ,version = "1.0.0"
    # if local file use file instead of href below
    #  with an absolute path
    ,src = paste0(getwd(),"/CSS/Leaflet-Stil/")
    ,stylesheet = "rstudio_leaflet_helge.css"
  )
)

# abspeichern
saveWidget(
  frameableWidget(fig), 
  file = paste0(getwd(), "/Output/Karten/", name_i, ".html"),
  selfcontained=F
)

# mitliefern: WSI-Logo
file.copy(
  from="./CSS/WSI_Abbinder_RGB.jpg",
  to="./Output/Karten"
)



