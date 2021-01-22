# read data

# Am besten einfach "dat" nennen. 

# Kreise (401)----

# key sollte AGS lauten!
if(ebene == "KRS") {
  dat <-
  read.xlsx(
    "./Input/Berechnungen Kurztexte Regionales 20 04 22.xlsx",
    sheet = 3, startRow = 10, colNames = T, cols = 1:7
  ) %>%
  as_tibble() %>%
  rename(
    ID = X1, Gesamt = `1`, Gesamt_L = `2`,
    Männer = `3`, Männer_L = `4`,
    Frauen = `5`, Frauen_L = `6`
  ) %>%
  mutate(Frauen_L = as.numeric(Frauen_L)) %>%
  filter(str_detect(ID, "[:digit:]{5} ") %in% T) %>%
  mutate(
    AGS = str_extract(ID, "[:digit:]{5}"),
    Kreis = str_remove(ID, "[:digit:]{5} ")
  ) %>%
  select(AGS, Kreis, everything(), -ID) %>%
  mutate(gruppe = 
           kat(Gesamt, 
               k = c(-1, 50000, 75000, 100000, 125000, max(Gesamt)),
               l = NULL)) # Label optional
}

# AMR (257)----

# key sollte AMR lauten!
if(ebene == "AMR") {
dat <-
  read.xlsx(
    "./Input/Helge Leiharbeiter Regression 19 09 12.xlsx",
    sheet = 1, startRow = 11, colNames = T, cols = 1:23
  ) %>%
  as_tibble() %>%
  rename(DEF = X22) %>%
  mutate(
    AGS = str_extract(Region, "[:digit:]{5}"),
    Kreis = str_remove(Region, "[:digit:]{5} "),
    AMR = amr_name
  ) %>%
  select(AGS, Kreis, amr, AMR, amr_styp3, DEF) %>%

   #bis hierhin nur Arbeitsmarktregionen gebildet, ab hier Kreisdaten anfügen:
  left_join(dat_kreise) %>%
  group_by(amr, AMR, amr_styp3, DEF) %>%
  summarise(
    Gesamt = sum(Gesamt_L) / sum(Gesamt),
    Männer = sum(Männer_L) / sum(Männer),
    Frauen = sum(Frauen_L) / sum(Frauen)
  ) %>%
  mutate(
    gruppe=cut(Gesamt, breaks = c(-1, 0.01, 0.02, 0.03, 0.04, max(Gesamt)),
               labels=c("bis 0,01", "0,011 bis 0,02", "0,021 bis 0,03", 
                        "0,031 bis 0,04", "0,041 und mehr"))
  )
}

# Länder (16)----

if(ebene == "LAN") {
dat <-
  read.xlsx(
    "./Input/Solo-Selbständige für 2019 Helge.xlsx",
    sheet = 1, startRow = 3, colNames = T, cols = 1:4
  ) %>%
  as_tibble() %>%
  filter(X1 != "Deutschland") %>%
  rename(GEN = X1, Gesamt = Insgesamt) %>%
  mutate(gruppe = cut(Gesamt, breaks=c(-1, 4, 4.5, 5, 5.5, max(Gesamt)), 
                      labels=c("bis 4", "4,1 bis 4,5", "4,6 bis 5", 
                               "5,1 bis 5,5", "5,6 und mehr")))
}
