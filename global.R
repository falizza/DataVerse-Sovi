#                        LOAD PACKAGE YANG DIGUNAKAN                          #

library(shiny)             # Framework dasar dashboard
library(shinydashboard)    # Layout dashboard
library(dplyr)             # Manipulasi data (select, filter, dll)
library(sf)                # Membaca data spasial (.geojson)
library(ggplot2)           # Visualisasi data statis
library(DT)                # Tabel interaktif
library(GGally)            # Untuk plot matriks korelasi (ggcorr)
library(car)               # Uji asumsi (leveneTest, vif)
library(lmtest)            # Uji asumsi (bptest)
library(DescTools)         # Statistik deskriptif (Skew, Kurt)
library(rmarkdown)         # Render laporan PDF
library(writexl)           # Menulis file Excel (.xlsx)
library(haven)             # Menulis file SPSS (.sav)
library(shinycssloaders)   # Animasi loading pada output
library(purrr)             # Fungsi iterasi elegan dengan map-style
library(spdep)             # Untuk Uji Autokorelasi Spasial (Moran's I)

#                   LOAD DATA YANG DIGUNAKAN                                   #

sovi_data <- read.csv2("data/sovi_data.csv")
sovi_data$DISTRICTCODE <- as.character(sovi_data$DISTRICTCODE)
distance_matrix <- read.csv("data/distance.csv")
indonesia_sf <- st_read("data/indonesia511.geojson")
indonesia_sf$kdkab <- as.character(indonesia_sf$kodeprkab)