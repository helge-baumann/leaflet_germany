# Farben (Böckler-CD) ----

farben <- function(x) {
  if (x == "tuerkis") {
    return(c("#e5f2f3", "#2b5f65"))
  }
  if (x == "grau") {
    return(c("#f9f9f4", "#64635c"))
  }
  if (x == "lila") {
    return(c("#dfd5e8", "#431d57"))
  }
  if (x == "blau") {
    return(c("#d3e3ee", "#00384f"))
  }
  if (x == "pink") {
    return(c("#f6dae8", "#740042"))
  }
}

# Shapefile Download----
dir.create("./Shapefiles", showWarnings=F)

download_shapefile <- function(shape="krs_neu") {
  
  if(shape == "krs_neu") {
    # Kreise und Länder aktuell
    url <- 
      paste0("https://daten.gdz.bkg.bund.de/produkte/vg/",
             "vg1000_ebenen_0101/aktuell/vg1000_01-01.utm32s.shape.ebenen.zip"
      )
    }

  if(shape == "krs_alt") {
    # Kreise und Länder alt (2016)
    url <- 
      paste0("https://daten.gdz.bkg.bund.de/produkte/vg/",
             "vg1000_ebenen_0101/2016/vg1000_01-01.utm32w.shape.ebenen.zip"
      )
  }
  
  if(shape == "amr") {
    # Arbeitsmarktregionen
    url <- 
      paste0("https://daten.gdz.bkg.bund.de/produkte/sonstige/",
             "ge1000/aktuell/ge1000.utm32s.shape.zip"
             )
  }
  
  download.file(url=url, destfile="./Shapefiles/Shapefiles.zip")
  unzip("./Shapefiles/Shapefiles.zip", exdir="./Shapefiles/shapefiles")
  file.remove("./Shapefiles/Shapefiles.zip")
  
}

# Shapefile laden----
load_shapefile <- function(path = NULL, level = "KRS", leaflet = F) {
  if (!level %in% c("KRS", "LAN", "AMR")) stop("zulässig: KRS, LAN, AMR")

  # Falls nötig, Download unter:
  # http://www.geodatenzentrum.de/geodaten/

  # best guess, wenn kein Pfad
  if (is.null(path)) {
    path <-
      ifelse(
        level %in% c("KRS", "LAN"),
        paste0("./Shapefiles/shapefiles/",
        "vg1000_01-01.utm32s.shape.ebenen/vg1000_ebenen_0101"
        ),
        paste0(
          "./Shapefiles/shapefiles/ge1000.utm32s.shape/ge1000/",
          "amr1000"
        )
      )
  }

  geo <- readOGR(
    path,
    ifelse(level == "AMR", "AMR1000", paste0("VG1000_", level)),
    use_iconv = TRUE,
    encoding = "UTF-8"
  )

  if (level == "LAN") {
    geo <- geo[geo$GF > 1, ]
  }
  
  if(level == "AMR") geo$AMR[geo$AMR == "Göttingen/Osterode"] <- "Göttingen"

  if (leaflet == T) geo <- spTransform(geo, CRS("+proj=longlat"))

  return(geo)
}

# gruppieren----
kat <- function(x, k = c(10, 20, 30, 50, 10000000), l = NULL) {
  
  add <- formatC(max(x), format = "e", digits = 2)
  if(str_detect(add, fixed("+"))) {
    add <- 1
  } else {
    if(substr(add, nchar(add), nchar(add)) == "1") add <- 0.01 
    if(substr(add, nchar(add), nchar(add)) == "2") add <- 0.001 
    if(substr(add, nchar(add), nchar(add)) == "3") add <- 0.0001 
  }

  ku <- k + add
  k <- str_remove_all(format(k, scientific = F), "\\s")
  ku <- str_remove_all(format(ku, scientific = F), "\\s")

  if (is.null(l)) {
    l[1] <- paste0("bis ", k[2])
    for (i in 2:(length(k) - 2)) {
      l[i] <-
        paste0("über ", ku[i], " bis ", k[i + 1])
    }
    l[length(k) - 1] <- paste0("über ", ku[length(k) - 1])
  }

  x <- cut(x, breaks = k, labels = l)
  return(x)
}

# statische Karte zeichnen----
static_map <- function(geo = geo, titel = "", untertitel = "", caption = "", leg = "") {
  karte <-
    ggplot(geo, aes(x = long, y = lat, group = group)) +
    labs(title = titel, subtitle = untertitel, caption = caption) +
    geom_polygon(data = geo, aes(fill = gruppe), color = gray(0.3), size = 0.1) +
    geom_polygon(
      data = geo[!geo$id %in% geo[geo$hole, ]$id, ],
      aes(fill = gruppe), color = gray(0.3), size = 0.1
    ) +
    scale_fill_manual(name = leg, values = colfunc(levels(geo$gruppe)), 
                      labels=levels(geo$gruppe)) +
    theme(
      text = element_text(family = "sans"),
      legend.text = element_text(size = 6),
      # legend.text = element_text(size=6),
      legend.key.size = unit(6, "points"),
      legend.position = "bottom",
      plot.caption = element_text(size = 9),
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill = "white")
    ) +
    guides(
      fill = guide_legend(ncol = 2, title = NULL, byrow = T)
    )

  return(karte)
}

# FormatDecimal----
FormatDecimal <- function(x, k) {
  return(format(round(as.numeric(x), k), nsmall = k))
}
# Labels leaflet----

leaflet_labels <-
  function(data = geo, vars = "Gesamt", labels = vars, suffix = "", head = "GEN") {
    labs <- lapply(seq(nrow(data)), function(i) {
      fix1 <- paste0("<p style='font-weight:bold;'>", "", data[[head]][i], "", "<p></p>")
      var <- NULL
      num <- 0
      for (b in vars) {
        num <- num + 1
        var[num] <- paste0(
          paste0(labels[num], ": "),
          str_replace(FormatDecimal(data[[b]][i], k = 1), fixed("."), ","),  suffix
        )
      }
      var <- paste(var, collapse = "; ")
      fix2 <- c("", "</p>")

      paste0(c(fix1, var, fix2), collapse = "")
    })

    return(labs)
  }

# Leaflet-Karte----
leaflet_basis <- function(data, width = 596, height = 596 * 1.25, zf = 6.2) {
  zf <- zf
  leaflet(
    data,
    width = width, height = height,
    options = leafletOptions(zoomControl = F, minZoom = zf, maxZoom = zf,
                             attributionControl=FALSE)
  ) %>%
    setView(lng = 9.5, lat = 51.5, zoom = zf) %>%
    suspendScroll(hoverToWake = T) %>%
    addMapPane("polygons", zIndex = 490) %>%
    addMapPane("text", zIndex = 1)
}

# Leaflet Text (Titel etc.)----

leaflet_text <- function(.data, text, lng = 3.9, lat = 55.5, size = 22) {
  x <- addLabelOnlyMarkers(.data,
    lng = lng, lat = lat,
    label = htmltools::HTML(
      paste(
        "<font face='Arial'>",
        text,
        "</font>",
        sep = ""
      )
    ),
    labelOptions = labelOptions(
      noHide = T,
      direction = "right",
      textOnly = T,
      textsize = paste0(size, "px"), ,
      style = list("font-family" = "Arial")
    )
  )

  return(x)
}

# Leaflet Legende----

leaflet_legend <-
  function(.data,
           lng = 3.9, lat = 54,
           colors = colfunc(levels(geo$gruppe)),
           lvl = levels(geo$gruppe),
           size = 13) {


    # Sortierung der Farben stimmt noch nicht!!

    addLabelOnlyMarkers(
      .data,
      lng, lat,
      label = htmltools::HTML(
        paste("<svg width='14.8' height='14.8'>",
          "<rect width='15' height='15' style='fill:",
          colors,
          ";stroke-width:3;",
          colors,
          " ' />",
          "</svg>",
          "</br>",
          collapse = ""
        )
      ),
      labelOptions = labelOptions(
        noHide = T, direction = "auto", textOnly = T,
        textsize = paste0(size, "px"), ,
        style = list(
          "font-family" = "Arial",
          "color" = gray(0.3)
        )
      )
    ) %>%
      # Legendenbeschriftung
      addLabelOnlyMarkers(
        lng + .5, lat,
        label = htmltools::HTML(
          paste(
            paste("<font face='Arial'>",
              lvl,
              "</br>",
              collapse = ""
            ),
            "</font>",
            sep = ""
          )
        ),
        labelOptions = labelOptions(
          noHide = T, direction = "auto", textOnly = T,
          textsize = paste0(size, "px"), ,
          style = list(
            "font-family" = "Arial",
            "color" = gray(0.3)
          )
        )
      )
  }

# Leaflet WSI-Logo----
leaflet_logo <-
  function(.data, path = "./CSS/WSI_Abbinder_RGB.jpg",
           size = c(72, 54), lng = 4.6, lat = 48.75) {
    addMarkers(.data, lng = lng, lat = lat, icon = list(iconUrl = path, iconSize = size))
  }

# Leaflet Quelle
# Quellenangabe:----
leaflet_quelle <- function(.data, lng = 3.9, lat = 47.5, Text = "Quelle", size = 12) {
  addLabelOnlyMarkers(
    .data,
    lng = lng, lat = lat,
    label = htmltools::HTML(
      paste("<font face='Arial'>",
        Text,
        sep = ""
      )
    ),
    labelOptions = labelOptions(
      noHide = T, direction = "right", textOnly = T,
      textsize = paste(size, "px"), ,
      style = list(
        "font-family" = "Arial",
        "color" = gray(0.2),
        "line-height" = "12px"
      )
    )
  )
}



# Leaflet Weitere Anmerkungen----
leaflet_anmerkung <- function(.data, lng = 3.9, lat = 47.1, size = 12, Text = "Anmerkung") {
  addLabelOnlyMarkers(
    .data,
    lng = lng, lat = lat,
    label = htmltools::HTML(
      paste("<font face='Arial'>",
        "<i>", Text,
        "</i> ",
        "</br>",
        "	",
        sep = ""
      )
    ),
    labelOptions = labelOptions(
      noHide = T, direction = "right", textOnly = T,
      textsize = paste(size, "px"), ,
      style = list(
        "font-family" = "Arial",
        "color" = gray(0.2),
        "line-height" = "12px"
      )
    )
  )
}

# Leaflet Polygone----
leaflet_polygons <-
  function(.data,
           colors = geo$color,
           bg = colfunc(levels(geo$gruppe)),
           variablen=c("Gesamt", "Männer", "Frauen", "Frauen_L"),
           headers=NULL,
           k=1, 
           suffix="") {

    if(is.null(headers)) headers <- variablen
    if("AMR" %in% names(geo))  geo$GEN <- geo$AMR
    
    # Labels
    labs <- lapply(seq(nrow(geo)), function(i) {
      paste0( "<p style='font-weight:bold;'>", "", geo[["GEN"]][i], "", '<p></p>', 
              headers[1], ": ",
              str_replace(FormatDecimal(geo[[variablen[1]]][i], k=k), fixed("."), ","),  suffix,
              '</p>' ) 
    })
    
    # Korrekturfaktor
    geo$pc1 <- NULL
    geo$pc2 <- NULL

    for (i in 1:length(geo@polygons)) {
      pc <- geo@polygons[[i]]@Polygons[[1]]@coords
      pc <- centroid(pc)
      geo$pc1[i] <- 9.5 - pc[1]
      geo$pc2[i] <- 51.5 - pc[2]
    }
    
    # popuptext 
    popup_text <- NULL
    for(i in 1:length(geo$GEN)) {
      
        
      popup_text[i] <- paste0(
        " <span style='vertical-align:0px''>",
        headers, ":",
        "</span>",
        paste0(
          "<div style=' background-color:white;color:black;padding-left:0px;",
          "padding-top: -20px; padding-right: 5px; width:195px;height:16px; '>"
        ),
        str_replace(FormatDecimal(geo@data[i,variablen], k = k), fixed("."), ","), " ", suffix, "<br>",
        "<div style=' background-color:",
        bg[3],
        paste0(";color:white;padding-left:0px; padding-top: 8px; ",
               "padding-right: 5px; width:195px;height:16px; '>"),
        collapse="")
    }

    # Polygone einzeichnen:----
    addPolygons(
      .data,
      color = gray(0.5), weight = 0.4, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 0.75,
      fillColor = ~color,
      highlightOptions = highlightOptions(
        color = gray(0.45), weight = 1.5, bringToFront = T, sendToBack = T,
      ),

      # Popups als Unteroption der Polygone:
      popup = paste(
        "<style> 
         .leaflet-popup-content-wrapper { 
         position: relative; top: ",
        geo$pc2*(-20),
        "px; right: ",
        geo$pc1*(-35),
        "px;
          } 
   .leaflet-container a.leaflet-popup-close-button {
  position: absolute; top: ", geo$pc2*(-20)+10, "px; right: ", geo$pc1*(-35)+25,
        "px;  }</style> " ,
        
        "<div style='background-color:",
          bg[3],
          " ;height: ", 80+length(variablen)*32, "px;'>",
          "<font face='Arial'>",
          "<div style='background-color:",
              bg[5],
              ";color:white;padding-left:10px;font-weight:bold;'>",
              "",
              paste(
                geo$GEN,
                sep = " "
              ),
              ":",
              "",
          "</div>",
          "<div style='background-color:",
            bg[3],
            ";color:white;padding-left:5px;height: 5px;'>", "",
          "</div>",
          "<div style='display: inline-block; background-color:",
            bg[3],
            paste0(
              ";color:white;padding-left:10px; padding-top: 8px; padding-right: ",
              "5px; width:65px;height:",
              50+length(variablen)*32,
              "px; margin:5; vertical-align:top;'>"
              ),
            "<img src='WSI_Abbinder_RGB.jpg' alt='WSI' width='60'>", "<br>", "",
          "</div>",
          "<div class='verticalLine' style='display: inline-block; background-color:",
            bg[3],
            paste0(
              ";color:white;padding-left:10px; padding-top: 0px; padding-right: ",
              "5px; width:65px;height:",
              40+length(variablen)*32,
              "px; margin:5px;'>"
              ),
        
        popup_text,
        "</div>",
        "</font>",
        "</div>",
        sep = ""
      ),
      label = lapply(labs, HTML),
      labelOptions = labelOptions(transparent = F, direction = "auto", textsize = "11px"),
      popupOptions = popupOptions(
        closeButton = T, closeOnClick = T, keepInView = F,
        autoPan = F, zIndex = 500, bringToFront = T
      )
    ) 
  }
