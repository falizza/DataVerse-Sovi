library(shiny)             # Framework dasar dashboard
library(shinydashboard)    # Buat dashboard layout yang cakep
library(shinyWidgets)      # Tambahan widget interaktif yang lebih fleksibel
library(dplyr)             # Manipulasi data (filter, group, summarise, dll)
library(tidyr)             # Transformasi data (pivoting, unnesting, dll)
library(stringr)           # Manipulasi string (buat label, interpretasi, dll)
library(purrr)             # Iterasi dan mapping data
library(readr)             # Baca CSV
library(e1071)
library(sf)                # Baca data spasial (.geojson)
library(jsonlite)          # Untuk konversi data json
library(ggplot2)           # Visualisasi dasar
library(plotly)            # Visualisasi interaktif (zoom, hover, dll)
library(leaflet)           # Visualisasi peta interaktif
library(DT)                # Tabel interaktif
library(corrplot)          # Matriks korelasi
library(ggpubr)            # Buat plot yang cocok untuk statistik inferensia
library(psych)             # Statistik deskriptif yang komplit
library(car)               # Cek asumsi linear model (vif, dll)
library(broom)             # Buat hasil model jadi tabel rapi
library(nortest)           # Uji normalitas tambahan (Anderson-Darling, dll)
library(lmtest)            # Uji heteroskedastisitas, autokorelasi, dll
library(DescTools)         # Uji beda proporsi, varians, ANOVA lengkap
library(stats)             # Paket statistik bawaan (mean, sd, anova, aov, dll)
library(MASS)              # Uji transformasi dan model robust
library(multcomp)          # Post-hoc test ANOVA
library(rmarkdown)         # Buat render PDF dari RMarkdown
library(knitr)             # Knitting dokumen
library(gridExtra)         # Arrange output/plot di PDF
library(kableExtra)        # Bikin tabel di PDF makin cakep
library(scales)            # Formatting angka, persen, dll
library(forcats)           # Manipulasi faktor
library(lubridate)         # Jika ada kolom tanggal, biar bisa diatur
library(bs4Dash)
library(writexl)
library(haven)
library(shinycssloaders)

# Load Data
sovi_data <- read.csv("data/sovi_data.csv")
distance_matrix <- read.csv("data/distance.csv")
indonesia_sf <- st_read("data/indonesia511.geojson")

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Analisis Sosial"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("sliders-h")),
      menuItem("Eksplorasi Data", icon = icon("chart-bar"),
               menuSubItem("Grafik & Statistik", tabName = "eksplorasi", icon = icon("ruler-combined")),
               menuSubItem("Peta Interaktif", tabName = "peta", icon = icon("map"))
      ),
      menuItem("Uji Asumsi Data", tabName = "uji_asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", icon = icon("flask"),
               menuSubItem(" Uji Beda Rata-rata", tabName = "rata_rata", icon = icon("balance-scale")),
               menuSubItem(" Uji Proporsi & Varians", tabName = "prop_var", icon = icon("percent")),
               menuSubItem(" ANOVA", tabName = "anova", icon = icon("calculator"))
      ),
      menuItem("Analisis Regresi", tabName = "regresi", icon = icon("chart-line")),
      menuItem("Unduh Data", tabName = "unduh_data", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "beranda",
        fluidRow(
          box(
            width = 12,
            title = "\U0001F4C8 Informasi Umum Dataset",
            status = "primary",
            solidHeader = TRUE,
            HTML("<h2><b>DataVerse: Visualisasi dan Analisis SOVI Indonesia</b></h2>
      <p>Dashboard ini menyajikan eksplorasi dan analisis mendalam terhadap data <i>Social Vulnerability Index</i> (SOVI) di Indonesia, dilengkapi dengan <i>distance matrix</i> dan peta spasial. Tujuan utama dari pengembangan dashboard ini adalah untuk memahami kerentanan sosial masyarakat di berbagai wilayah Indonesia, serta mengevaluasi keterkaitannya dengan kondisi demografi, lingkungan, dan infrastruktur. Visualisasi dan analisis ini ditujukan untuk mendukung pengambilan keputusan berbasis data, khususnya dalam konteks manajemen risiko bencana dan pengembangan wilayah.</p>

      <h4><b>Spesifikasi Tabel</b></h4>
      <table class='table table-striped'>
        <tr><th>Kategori</th><th>Keterangan</th></tr>
        <tr><td>Subjek</td><td>Geografi</td></tr>
        <tr><td>Ruang lingkup khusus</td><td>Manajemen bencana dan pengurangan risiko, kerentanan sosial</td></tr>
        <tr><td>Tipe data</td><td>Tabel</td></tr>
        <tr><td>Sumber data</td><td>Survei Sosial Ekonomi Nasional (SUSENAS) 2017 dari BPS dan Peta Geospasial Indonesia tahun 2013</td></tr>
        <tr><td>Format data</td><td>Mentah, Telah dianalisis, Tersaring</td></tr>
        <tr><td>Parameter pengumpulan data</td><td>Data dikumpulkan pada level kabupaten/kota dan disesuaikan dengan batas administratif pada peta digital</td></tr>
        <tr><td>Deskripsi pengumpulan data</td><td>Data dikumpulkan dari SUSENAS 2017 kemudian diagregasi menggunakan aturan baku dan pembobotan sesuai metode sampling. Matriks jarak antar kabupaten dihitung dari data spasial peta wilayah administratif Indonesia.</td></tr>
        <tr><td>Lokasi sumber data</td><td>SUSENAS 2017 oleh BPS dan Peta Geospasial Indonesia tingkat kabupaten/kota tahun 2013</td></tr>
        <tr><td>Aksesibilitas data</td><td>Tersedia secara publik dan dapat diakses bersama artikel terkait</td></tr>
        <tr><td>Repositori</td><td>Paket GitHub <code>naspaclust</code></td></tr>
        <tr><td>Tautan langsung ke data</td><td>
          <ul>
            <li><a href='https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv' target='_blank'>Data SOVI</a></li>
            <li><a href='https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv' target='_blank'>Matriks Jarak Antar Kabupaten</a></li>
          </ul>
        </td></tr>
        <tr><td>Artikel Terkait</td><td>Nasution et al. (2020). <i>Revisiting social vulnerability analysis in Indonesia: An optimized spatial fuzzy clustering approach</i>. <a href='https://doi.org/10.1016/j.ijdrr.2020.101801' target='_blank'>DOI:10.1016/j.ijdrr.2020.101801</a></td></tr>
      </table>
      
      <br>
      <h4><b>Nilai dari Dataset (Value of the Data)</b></h4>
      <ul>
        <li>Dataset ini menyediakan indikator pembangunan dan kebencanaan dari 511 kabupaten/kota di Indonesia beserta matriks jarak antar wilayah.</li>
        <li>Data dapat digunakan untuk membandingkan dan mengevaluasi tingkat pembangunan antar wilayah di Indonesia, termasuk elaborasi konteks kerentanan sosial.</li>
        <li>Dataset ini dapat membantu pengambil kebijakan dalam merumuskan respons terhadap bencana dengan mempertimbangkan kondisi sosial dan wilayah.</li>
        <li>Data memungkinkan identifikasi lebih dalam terhadap ketahanan wilayah terhadap bencana, khususnya menggunakan pendekatan spasial.</li>
        <li>Dataset dapat dikombinasikan dengan data dari bidang studi lain, seperti kesehatan masyarakat dan transportasi, guna memperoleh pemahaman lintas sektor terhadap pembangunan wilayah.</li>
      </ul>

      <h4><b>Deskripsi Variabel</b></h4>
      <table class='table table-bordered'>
        <tr><th>Label</th><th>Variabel</th><th>Deskripsi</th></tr>
        <tr><td>DISTRICTCODE</td><td>Kode Kabupaten</td><td>Kode wilayah administratif kabupaten/kota</td></tr>
        <tr><td>CHILDREN</td><td>Anak-anak</td><td>Persentase penduduk berusia di bawah lima tahun</td></tr>
        <tr><td>FEMALE</td><td>Perempuan</td><td>Persentase penduduk perempuan</td></tr>
        <tr><td>ELDERLY</td><td>Lansia</td><td>Persentase penduduk berusia di atas 65 tahun</td></tr>
        <tr><td>FHEAD</td><td>Kepala keluarga perempuan</td><td>Persentase rumah tangga dengan kepala keluarga perempuan</td></tr>
        <tr><td>FAMILYSIZE</td><td>Ukuran rumah tangga</td><td>Rata-rata jumlah anggota rumah tangga dalam satu kabupaten</td></tr>
        <tr><td>NOELECTRIC</td><td>Rumah tanpa listrik</td><td>Persentase rumah tangga yang tidak menggunakan listrik sebagai sumber pencahayaan</td></tr>
        <tr><td>LOWEDU</td><td>Pendidikan rendah</td><td>Persentase penduduk usia 15 tahun ke atas dengan pendidikan rendah</td></tr>
        <tr><td>GROWTH</td><td>Pertumbuhan penduduk</td><td>Persentase perubahan jumlah penduduk</td></tr>
        <tr><td>POVERTY</td><td>Kemiskinan</td><td>Persentase penduduk miskin</td></tr>
        <tr><td>ILLITERATE</td><td>Buta huruf</td><td>Persentase penduduk yang tidak dapat membaca dan menulis</td></tr>
        <tr><td>NOTRAINING</td><td>Tanpa pelatihan bencana</td><td>Persentase rumah tangga yang tidak mengikuti pelatihan/simulasi kebencanaan</td></tr>
        <tr><td>DPRONE</td><td>Rawan bencana</td><td>Persentase rumah tangga yang tinggal di daerah rawan bencana</td></tr>
        <tr><td>RENTED</td><td>Sewa rumah</td><td>Persentase rumah tangga yang tinggal di rumah sewa</td></tr>
        <tr><td>NOSEWER</td><td>Tidak punya saluran limbah</td><td>Persentase rumah tangga tanpa sistem pembuangan limbah</td></tr>
        <tr><td>TAPWATER</td><td>Air bersih</td><td>Persentase rumah tangga yang menggunakan air ledeng</td></tr>
        <tr><td>POPULATION</td><td>Jumlah Penduduk</td><td>Total populasi di tiap kabupaten/kota</td></tr>
      </table>
      
      <br>
      <h4><b> 👩🏻‍💼 Disusun oleh</b></h4>
      <p><b>Faliza Maulidina Syarief</b><br>NIM: 222313077<br>Kelas: 2KS3</p>
      ")
          )
        )
      ),
    
      tabItem(
        tabName = "manajemen", 
        fluidRow(
          box(width = 4, title = "Pengaturan Kategorisasi", status = "primary", solidHeader = TRUE,
              selectInput("var_kontinyu", "Pilih Variabel Kontinyu", 
                          choices = names(sovi_data)[sapply(sovi_data, is.numeric)]),
              selectInput("metode_kat", "Metode Kategorisasi", 
                          choices = c("Equal Interval", "Quantile", "Manual Cut")),
              conditionalPanel(
                condition = "input.metode_kat == 'Manual Cut'",
                textInput("manual_breaks", "Titik Potong (pisahkan dengan koma)", "10,20,30")
              ),
              actionButton("proses_kat", "Proses", icon = icon("play"))
          ),
          box(width = 8, title = "Hasil Kategorisasi", status = "success", solidHeader = TRUE,
              DTOutput("tabel_kat"),
              br(),
              uiOutput("interpretasi_kat")
          )
        )
      ),
      
      tabItem(tabName = "eksplorasi",
              fluidRow(
                box(title = "Pilih Variabel", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput("vars_selected", "Variabel untuk Eksplorasi:", 
                                choices = names(sovi_data), multiple = TRUE, selectize = TRUE),
                    selectInput("var_plot", "Variabel untuk Plot (1 variabel):",
                                choices = NULL, selected = NULL[1])
                )
              ),
              
              fluidRow(
                box(title = "Statistik Deskriptif", width = 12, status = "info", solidHeader = TRUE,
                    DT::dataTableOutput("tabel_stat") %>% withSpinner()
                )
              ),
              
              fluidRow(
                box(title = "Boxplot", width = 4, status = "warning", solidHeader = TRUE,
                    plotOutput("boxplot") %>% withSpinner()
                ),
                box(title = "QQ Plot", width = 4, status = "warning", solidHeader = TRUE,
                    plotOutput("qqplot") %>% withSpinner()
                ),
                box(title = "Histogram", width = 4, status = "warning", solidHeader = TRUE,
                    plotOutput("histplot") %>% withSpinner()
                )
              ),
              
              fluidRow(
                box(title = "Matriks Korelasi", width = 12, status = "success", solidHeader = TRUE,
                    plotOutput("corrplot") %>% withSpinner()
                )
              ),
              
              fluidRow(
                box(title = "Interpretasi", width = 12, status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("interpretasi")
                )
              ),
              
              fluidRow(
                box(title = "Unduh Hasil PDF", width = 12, status = "danger", solidHeader = TRUE,
                    downloadButton("downloadPDF", "Unduh PDF")
                )
              )
      ),
      
      tabItem(tabName = "peta", h2("Peta Interaktif")),
      
      tabItem(tabName = "uji_asumsi",
              h2("Uji Asumsi Klasik"),
              fluidRow(
                box(title = "Pilih Variabel", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput("vars_selected", "Variabel untuk Eksplorasi:", 
                                choices = names(sovi_data), multiple = TRUE, selectize = TRUE),
                    selectInput("var_asumsi", "Variabel untuk Plot (1 variabel):",
                                choices = NULL, selected = NULL[1])
                )
              ),
              fluidRow(
                box(width = 6, title = "Uji Normalitas (Shapiro-Wilk)", status = "warning", solidHeader = TRUE,
                    verbatimTextOutput("shapiro_test")
                ),
                box(width = 6, title = "Uji Homogenitas Varians (Levene's Test)", status = "warning", solidHeader = TRUE,
                    verbatimTextOutput("homogeneity_test")
                )
              ),
              fluidRow(
                box(width = 6, title = "Deteksi Outlier (Boxplot)", status = "warning", solidHeader = TRUE,
                    plotOutput("outlier_plot")
                ),
                box(width = 6, title = "Ringkasan Outlier", status = "warning", solidHeader = TRUE,
                    verbatimTextOutput("outlier_summary")
                )
              ),
              fluidRow(
                box(width = 12, status = "info", solidHeader = TRUE, title = "Ringkasan & Rekomendasi Statistik", collapsible = TRUE,
                    verbatimTextOutput("interpretasiAsumsi")
                )
              ),
              fluidRow(
                box(width = 12, status = "danger", solidHeader = TRUE, title = "Unduh Laporan Uji Asumsi",
                    downloadButton("downloadAsumsiPDF", "Unduh PDF Hasil Uji Asumsi")
                )
              )
      ),
      
      tabItem(tabName = "rata_rata",
              h2("Uji Beda Rata-rata (t-test)"),
              tabBox(
                id = "ttest_tabs",
                width = 12,
                tabPanel(
                  "Uji 1 Kelompok",
                  fluidRow(
                    # Box untuk Input
                    box(
                      width = 4, status = "primary", solidHeader = TRUE, title = "Pengaturan Analisis",
                      selectInput("var_1samp", "Pilih Variabel Numerik:",
                                  choices = names(sovi_data)[sapply(sovi_data, is.numeric)]),
                      numericInput("mu_1samp", "Nilai Hipotesis (μ₀) untuk Dibandingkan:", value = 10),
                      radioButtons("alternative_1samp", "Jenis Uji (Hipotesis Alternatif):",
                                   choices = c("Dua Arah (two.sided)" = "two.sided",
                                               "Kurang Dari (less)" = "less",
                                               "Lebih Dari (greater)" = "greater"),
                                   inline = TRUE),
                      helpText("Contoh: Menguji apakah rata-rata persentase kemiskinan (POVERTY) secara signifikan berbeda dari 10%?"),
                      actionButton("run_1samp", "Jalankan Analisis", icon = icon("play"), class = "btn-success")
                    ),
                    box(
                      width = 8, status = "info", solidHeader = TRUE, title = "Hasil Analisis Uji 1 Kelompok",
                      h4("Hasil Statistik:"),
                      verbatimTextOutput("res_1samp") %>% withSpinner(),
                      hr(),
                      h4("Interpretasi Hasil:"),
                      uiOutput("int_1samp") %>% withSpinner()
                    )
                  )
                ),
                tabPanel(
                  "Uji 2 Kelompok Independen",
                  fluidRow(
                    # Box untuk Input
                    box(
                      width = 4, status = "primary", solidHeader = TRUE, title = "Pengaturan Analisis",
                      selectInput("var_2samp_num", "1. Pilih Variabel Numerik:",
                                  choices = names(sovi_data)[sapply(sovi_data, is.numeric)]),
                      selectInput("var_2samp_cat", "2. Pilih Variabel untuk Membuat Grup:",
                                  choices = names(sovi_data)[sapply(sovi_data, is.numeric)]),
                      numericInput("split_val_2samp", "3. Tentukan Titik Potong Grup:", value = 0),
                      radioButtons("alternative_2samp", "Jenis Uji (Hipotesis Alternatif):",
                                   choices = c("Dua Arah (two.sided)" = "two.sided",
                                               "Kurang Dari (less)" = "less",
                                               "Lebih Dari (greater)" = "greater"),
                                   inline = TRUE),
                      helpText("Aplikasi akan membuat 2 grup dari variabel di no. 2. Grup 1: <= titik potong. Grup 2: > titik potong."),
                      actionButton("run_2samp", "Jalankan Analisis", icon = icon("play"), class = "btn-success")
                    ),
                    box(
                      width = 8, status = "info", solidHeader = TRUE, title = "Hasil Analisis Uji 2 Kelompok",
                      h4("Hasil Statistik:"),
                      verbatimTextOutput("res_2samp") %>% withSpinner(),
                      hr(),
                      h4("Interpretasi Hasil:"),
                      uiOutput("int_2samp") %>% withSpinner()
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  width = 12, status = "danger", solidHeader = TRUE, title = "Unduh Laporan",
                  downloadButton("downloadRataPDF", "Unduh PDF Hasil Analisis")
                )
              )
      ),
      
      tabItem(tabName = "prop_var",
              h2("Uji Proporsi dan Uji Varians"),
              tabBox(
                id = "propvar_tabs", width = 12,
                tabPanel("Uji Proporsi 1 Kelompok", fluidRow(
                  box(width=4, status="primary", solidHeader=TRUE, title="Pengaturan",
                      selectInput("var_prop1_konteks", "1. Konteks Variabel:", choices=names(sovi_data)[sapply(sovi_data, is.numeric)]),
                      numericInput("x_prop1_manual", "2. Jumlah 'Sukses' (x):", value=50, min=0),
                      numericInput("n_prop1_manual", "3. Jumlah Total (n):", value=100, min=1),
                      numericInput("p_prop1", "4. Proporsi Hipotesis (p₀):", value=0.5, min=0, max=1, step=0.01),
                      radioButtons("alt_prop1", "Jenis Uji:", choices=c("Dua Arah"="two.sided", "Kurang Dari"="less", "Lebih Dari"="greater"), inline=TRUE),
                      actionButton("run_prop1", "Jalankan Analisis", icon=icon("play"))
                  ),
                  box(width=8, status="info", solidHeader=TRUE, title="Hasil Analisis",
                      verbatimTextOutput("res_prop1") %>% withSpinner(),
                      uiOutput("int_prop1") %>% withSpinner()
                  )
                )),
                tabPanel("Uji Proporsi 2 Kelompok", fluidRow(
                  box(width=4, status="primary", solidHeader=TRUE, title="Pengaturan",
                      h5("Definisi 'Sukses'"),
                      selectInput("var_prop2_cond", "1. Variabel Kondisi:", choices=names(sovi_data)[sapply(sovi_data, is.numeric)]),
                      textInput("cond_prop2_text", "2. Kondisi 'Sukses':", value="> 10"),
                      hr(),
                      h5("Definisi Grup"),
                      selectInput("var_prop2_group", "3. Variabel Grup:", choices=names(sovi_data)[sapply(sovi_data, is.numeric)]),
                      helpText("Grup dibagi otomatis berdasarkan median."),
                      hr(),
                      radioButtons("alt_prop2", "Jenis Uji:", choices=c("Dua Arah"="two.sided", "Kurang Dari"="less", "Lebih Dari"="greater"), inline=TRUE),
                      actionButton("run_prop2", "Jalankan Analisis", icon=icon("play"))
                  ),
                  box(width=8, status="info", solidHeader=TRUE, title="Hasil Analisis",
                      verbatimTextOutput("res_prop2") %>% withSpinner(),
                      uiOutput("int_prop2") %>% withSpinner()
                  )
                )),
                tabPanel("Uji Varians 2 Kelompok (F-test)", fluidRow(
                  box(width=4, status="primary", solidHeader=TRUE, title="Pengaturan",
                      selectInput("var1_ftest", "Variabel Kelompok 1:", choices=names(sovi_data)[sapply(sovi_data, is.numeric)]),
                      selectInput("var2_ftest", "Variabel Kelompok 2:", choices=names(sovi_data)[sapply(sovi_data, is.numeric)], selected=names(sovi_data)[sapply(sovi_data, is.numeric)][2]),
                      radioButtons("alt_ftest", "Jenis Uji:", choices=c("Dua Arah"="two.sided", "Kurang Dari"="less", "Lebih Dari"="greater"), inline=TRUE),
                      actionButton("run_ftest", "Jalankan Analisis", icon=icon("play"))
                  ),
                  box(width=8, status="info", solidHeader=TRUE, title="Hasil Analisis",
                      verbatimTextOutput("res_ftest") %>% withSpinner(),
                      uiOutput("int_ftest") %>% withSpinner()
                  )
                ))
              ),
              fluidRow(
                box(width = 12, status = "danger", solidHeader = TRUE, title = "Unduh Laporan",
                    downloadButton("downloadPropVarPDF", "Unduh Hasil Analisis")
                )
              )
      ),      
      
      tabItem(tabName = "anova",
              h2("Analisis Varians (ANOVA)"),
              p("Menu ini secara otomatis memeriksa asumsi sebelum menjalankan ANOVA."),
              tabBox(
                id = "anova_tabs", width = 12,
                tabPanel("ANOVA 1 Arah", fluidRow(
                  box(width=4, status="primary", solidHeader=TRUE, title="Pengaturan",
                      selectInput("var_anova1_dv", "1. Variabel Dependen (Numerik):", choices=names(sovi_data)[sapply(sovi_data, is.numeric)]),
                      selectInput("var_anova1_iv", "2. Variabel Independen (Grup):", choices=names(sovi_data)[sapply(sovi_data, is.numeric)]),
                      helpText("Variabel independen akan dibagi otomatis menjadi 3 grup."),
                      actionButton("run_anova1", "Jalankan Analisis", icon=icon("play"))
                  ),
                  box(width=8, status="warning", solidHeader=TRUE, title="Pemeriksaan Asumsi",
                      uiOutput("asumsi_anova1_status") %>% withSpinner()
                  )
                ), fluidRow(
                  box(width=12, status="info", solidHeader=TRUE, title="Hasil Analisis",
                      verbatimTextOutput("res_anova1") %>% withSpinner(),
                      h4("Uji Lanjutan (Post-Hoc Tukey HSD)"),
                      verbatimTextOutput("res_posthoc_anova1") %>% withSpinner(),
                      h4("Interpretasi"),
                      uiOutput("int_anova1") %>% withSpinner()
                  )
                )),
                tabPanel("ANOVA 2 Arah", fluidRow(
                  box(width=4, status="primary", solidHeader=TRUE, title="Pengaturan",
                      selectInput("var_anova2_dv", "1. Variabel Dependen (Numerik):", choices=names(sovi_data)[sapply(sovi_data, is.numeric)]),
                      selectInput("var_anova2_iv1", "2. Variabel Independen #1:", choices=names(sovi_data)[sapply(sovi_data, is.numeric)]),
                      selectInput("var_anova2_iv2", "3. Variabel Independen #2:", choices=names(sovi_data)[sapply(sovi_data, is.numeric)], selected=names(sovi_data)[sapply(sovi_data, is.numeric)][2]),
                      helpText("Kedua variabel independen akan dibagi otomatis menjadi grup Rendah/Tinggi."),
                      actionButton("run_anova2", "Jalankan Analisis", icon=icon("play"))
                  ),
                  box(width=8, status="warning", solidHeader=TRUE, title="Pemeriksaan Asumsi",
                      uiOutput("asumsi_anova2_status") %>% withSpinner()
                  )
                ), fluidRow(
                  box(width=12, status="info", solidHeader=TRUE, title="Hasil Analisis",
                      verbatimTextOutput("res_anova2") %>% withSpinner(),
                      h4("Uji Lanjutan (Post-Hoc Tukey HSD)"),
                      verbatimTextOutput("res_posthoc_anova2") %>% withSpinner(),
                      h4("Interpretasi"),
                      uiOutput("int_anova2") %>% withSpinner()
                  )
                ))
              ),
              fluidRow(
                box(width=12, status="danger", solidHeader=TRUE, title="Unduh Laporan",
                    downloadButton("downloadAnovaPDF", "Unduh PDF Hasil Analisis"))
              )
      ),
      
      tabItem(tabName = "regresi",
              h2("Analisis Regresi Linier"),
              fluidRow(
                box(
                  width = 4, status = "primary", solidHeader = TRUE, title = "Pengaturan Analisis",
                  selectInput("y_reg", "1. Pilih Variabel Dependen (Y):",
                              choices = names(sovi_data)[sapply(sovi_data, is.numeric)]),
                  
                  selectInput("x_reg", "2. Pilih Variabel Independen (X):",
                              choices = names(sovi_data)[sapply(sovi_data, is.numeric)], 
                              multiple = TRUE, selectize = TRUE),
                  
                  hr(),
                  h4("Transformasi Variabel (Opsional)"),
                  selectInput("var_transform", "Pilih variabel untuk ditransformasi:",
                              choices = NULL, multiple = TRUE, selectize = TRUE),
                  
                  selectInput("transform_method", "Pilih jenis transformasi:",
                              choices = c("Tidak Ada" = "None", "Logaritma (log(x+1))" = "Log", "Akar Kuadrat (sqrt)" = "Sqrt")),
                  
                  actionButton("run_analysis", "Jalankan Analisis", icon = icon("play"), class = "btn-success")
                ),
                
                box(
                  width = 8,
                  tabsetPanel(
                    tabPanel("Ringkasan Model", 
                             verbatimTextOutput("reg_summary") %>% withSpinner()
                    ),
                    tabPanel("Uji Asumsi", 
                             h4("1. Uji Multikolinearitas (VIF)"),
                             verbatimTextOutput("vif_result"),
                             hr(),
                             h4("2. Uji Normalitas Residual (Shapiro-Wilk)"),
                             plotOutput("qq_plot", height = "300px"),
                             verbatimTextOutput("shapiro_result"),
                             hr(),
                             h4("3. Uji Homoskedastisitas (Breusch-Pagan)"),
                             plotOutput("resid_plot", height = "300px"),
                             verbatimTextOutput("bp_result")
                    ),
                    tabPanel("Interpretasi Otomatis", 
                             uiOutput("interpretasi_regresi") %>% withSpinner()
                    )
                  )
                )
              ),
              fluidRow(
                box(width = 12, status = "danger", solidHeader = TRUE, title = "Unduh Laporan",
                    downloadButton("downloadRegresiPDF", "Unduh PDF Hasil Analisis Regresi")
                )
              )
      ),
      
      tabItem(tabName = "unduh_data",
              h2("Unduh Data Pilihan"),
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE, title = "Pengaturan Unduhan",
                  
                  selectInput(
                    inputId = "vars_unduh",
                    label = "Pilih variabel yang ingin diunduh:",
                    choices = names(sovi_data),
                    selected = names(sovi_data),
                    multiple = TRUE,
                    selectize = TRUE # Opsi ini membuat selectInput terlihat lebih modern
                  ),

                  radioButtons(
                    inputId = "format_unduh",
                    label = "Pilih Format File:",
                    choices = c("CSV" = "csv", "Excel (XLSX)" = "xlsx", "SPSS (SAV)" = "sav"),
                    selected = "csv",
                    inline = TRUE
                  ),
                  
                  downloadButton("download_data_button", "Unduh Data")
                )
              ),
              
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE, title = "Pratinjau Data yang Dipilih",
                  DTOutput("tabel_preview_unduh") %>% withSpinner()
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Server Menu Manajemen Data
  observeEvent(input$proses_kat, {
    req(input$var_kontinyu)
    var_data <- sovi_data[[input$var_kontinyu]]
    
    kategori <- NULL
    
    if (input$metode_kat == "Equal Interval") {
      breaks <- pretty(var_data, n = 4)
      kategori <- cut(var_data, breaks = breaks, include.lowest = TRUE)
      
    } else if (input$metode_kat == "Quantile") {
      breaks <- quantile(var_data, probs = seq(0, 1, length.out = 5), na.rm = TRUE)
      kategori <- cut(var_data, breaks = breaks, include.lowest = TRUE)
      
    } else if (input$metode_kat == "Manual Cut") {
      manual_breaks <- as.numeric(unlist(strsplit(input$manual_breaks, ",")))
      kategori <- cut(var_data, breaks = manual_breaks, include.lowest = TRUE)
    }
    
    hasil_df <- data.frame(ID = sovi_data$DISTRICTCODE,
                           Variabel = var_data,
                           Kategori = kategori)
    
    output$tabel_kat <- renderDT({
      datatable(hasil_df)
    })
    
    output$interpretasi_kat <- renderUI({
      jumlah_kat <- length(unique(kategori))
      tagList(
        h4("📌 Interpretasi:"),
        p(paste("Variabel", input$var_kontinyu, "dibagi menjadi", jumlah_kat, "kategori menggunakan metode", input$metode_kat)),
        if (input$metode_kat == "Manual Cut") p(paste("Titik potong manual yang digunakan:", input$manual_breaks))
      )
    })
  })
  
  # Seleksi Variabel
  data_filtered <- reactive({
    req(input$vars_selected)
    sovi_data %>% dplyr::select(all_of(input$vars_selected))
  })
  
  # Observer untuk memperbarui input lain ketika `vars_selected` berubah
  observe({
    variabel_terpilih <- input$vars_selected
    
    updateSelectInput(session, "var_plot", choices = variabel_terpilih, selected = variabel_terpilih[1])
    updateSelectInput(session, "var_asumsi", choices = variabel_terpilih, selected = variabel_terpilih[1])
  })
  
  # Eksplorasi Data
  output$tabel_stat <- DT::renderDataTable({
    df <- data_filtered()
    hasil <- data.frame(Variabel = character())
    
    for (v in names(df)) {
      x <- df[[v]]
      valid <- sum(!is.na(x))
      miss <- sum(is.na(x))
      total <- length(x)
      hasil <- rbind(hasil, data.frame(
        Variabel = v,
        N_Valid = valid,
        Percent_Valid = round(valid / total * 100, 1),
        N_Missing = miss,
        Percent_Missing = round(miss / total * 100, 1),
        Mean = round(mean(x, na.rm = TRUE), 2),
        Median = round(median(x, na.rm = TRUE), 2),
        Variance = round(var(x, na.rm = TRUE), 2),
        SD = round(sd(x, na.rm = TRUE), 2),
        Min = round(min(x, na.rm = TRUE), 2),
        Max = round(max(x, na.rm = TRUE), 2),
        Range = round(diff(range(x, na.rm = TRUE)), 2),
        Skewness = round(DescTools::Skew(x, na.rm = TRUE), 2),
        Kurtosis = round(DescTools::Kurt(x, na.rm = TRUE), 2)
      ))
    }
    DT::datatable(
      hasil,
      options = list(
        scrollX = TRUE,
        pageLength = 5
        
      )
    )
  })
  
  output$boxplot <- renderPlot({
    req(input$var_plot)
    ggplot(sovi_data, aes_string(y = input$var_plot)) +
      geom_boxplot(fill = "skyblue") +
      theme_minimal()
  })
  
  output$qqplot <- renderPlot({
    req(input$var_plot)
    x <- sovi_data[[input$var_plot]]
    qqnorm(x, main = paste("QQ Plot -", input$var_plot))
    qqline(x, col = "red")
  })
  
  output$histplot <- renderPlot({
    req(input$var_plot)
    ggplot(sovi_data, aes_string(x = input$var_plot)) +
      geom_histogram(fill = "#69b3a2", bins = 30) +
      theme_minimal()
  })
  
  output$corrplot <- renderPlot({
    df <- data_filtered()
    df_num <- df %>% dplyr::select(where(is.numeric))
    req(ncol(df_num) > 1)
    GGally::ggcorr(df_num, label = TRUE, label_round = 5, label_size = 5)
  })
  
  output$interpretasi <- renderPrint({
    df <- data_filtered()
    cat("Dashboard ini menyajikan statistik deskriptif untuk ", ncol(df), " variabel yang dipilih pengguna.\n")

    cat("📌 Interpretasi Distribusi:\n")
    for (v in names(df)) {
      x <- df[[v]]
      skew_x <- DescTools::Skew(x, na.rm = TRUE)
      kurt_x <- DescTools::Kurt(x, na.rm = TRUE)
      mean_x <- mean(x, na.rm = TRUE)
      median_x <- median(x, na.rm = TRUE)
      sd_x <- sd(x, na.rm = TRUE)
      
      cat(paste0("- Variabel ", v, ": "))
      
      # Skewness
      if (abs(skew_x) < 0.5) cat("\n   Berdistribusi simetris")
      else if (skew_x > 0) cat("\n   Berdistribusi menceng ke kanan")
      else cat("\n   Bberdistribusi menceng ke kiri")
      
      cat(", ")
      
      # Kurtosis
      if (kurt_x > 3) cat("dengan puncak tajam (leptokurtic)")
      else if (kurt_x < 3) cat("dengan puncak datar (platykurtic)")
      else cat("dengan distribusi menyerupai normal")
      
      # Median vs mean
      cat(". ")
      
      if (abs(mean_x - median_x) > 0.5 * sd_x) {
        cat("\n   Perbedaan mean dan median mengindikasikan kemungkinan adanya outlier atau distribusi tidak normal.\n")
      } else {
        cat("\n   Mean dan median relatif seimbang.\n")
      }
    }
    
    # Korelasi
    df_num <- df %>% dplyr::select(where(is.numeric))
    if (ncol(df_num) > 1) {
      cat("\n🔗 Interpretasi Korelasi:\n")
      cor_matrix <- cor(df_num, use = "pairwise.complete.obs")
      for (i in 1:(ncol(cor_matrix)-1)) {
        for (j in (i+1):ncol(cor_matrix)) {
          var1 <- colnames(cor_matrix)[i]
          var2 <- colnames(cor_matrix)[j]
          r <- cor_matrix[i, j]
          
          strength <- ifelse(abs(r) >= 0.8, "sangat kuat",
                             ifelse(abs(r) >= 0.6, "kuat",
                                    ifelse(abs(r) >= 0.4, "cukup", 
                                           ifelse(abs(r) >= 0.2, "lemah", "sangat lemah"))))
          direction <- ifelse(r > 0, "positif", ifelse(r < 0, "negatif", "tidak ada"))
          
          cat(paste0("- ", var1, " vs ", var2, ": hubungan ", direction, " dengan kekuatan ", strength, " (r = ", round(r, 2), ").\n"))
        }
      }
    }
  })
  
  
  
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste0("Hasil-Eksplorasi-Data-Sovi-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "Laporan-Eksplorasi.Rmd")
      file.copy("www/Laporan-Eksplorasi.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(data = data_filtered())
      
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # Uji Asumsi Data
  
  asumsi_results <- reactive({
    req(input$var_asumsi)
    dat <- sovi_data[[input$var_asumsi]]
    
    # Validasi awal
    if (!is.numeric(dat) || length(na.omit(dat)) < 3) {
      return(list(error = "Data tidak valid. Pilih variabel numerik dengan minimal 3 observasi."))
    }
    
    # Lakukan semua tes
    # Uji Normalitas (dilewati jika data > 5000)
    shapiro_res <- if(length(na.omit(dat)) <= 5000) shapiro.test(dat) else NULL
    
    # Uji Homogenitas (dilewati jika tidak bisa membentuk 2 grup)
    group <- as.factor(ifelse(dat <= median(dat, na.rm = TRUE), "Grup Bawah", "Grup Atas"))
    levene_res <- if(length(levels(na.omit(group))) > 1) car::leveneTest(dat ~ group) else NULL
    
    # Deteksi Outlier
    x_clean <- na.omit(dat)
    q1 <- quantile(x_clean, 0.25)
    q3 <- quantile(x_clean, 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    outliers <- x_clean[x_clean < lower_bound | x_clean > upper_bound]
    
    # Kembalikan semua hasil dalam satu list
    list(
      var_name = input$var_asumsi,
      shapiro = shapiro_res,
      levene = levene_res,
      outliers = outliers,
      data = dat,
      error = NULL
    )
  })
  
  output$shapiro_test <- renderPrint({
    req(input$var_asumsi)
    dat <- sovi_data[[input$var_asumsi]]
    
    if (!is.numeric(dat) || length(na.omit(dat)) < 3 || length(na.omit(dat)) > 5000) {
      cat("Analisis Dibatalkan: Variabel harus numerik dengan 3 hingga 5000 data valid (non-NA) untuk Uji Shapiro-Wilk.")
      return()
    }
    
    test_result <- shapiro.test(dat)
    test_output <- capture.output(print(test_result))
    interpretation <- if (test_result$p.value < 0.05) {
      "\n\nInterpretasi: \n   Karena p-value < 0.05, data TIDAK berdistribusi normal."
    } else {
      "\n\nInterpretasi: \n   Karena p-value >= 0.05, data cenderung berdistribusi normal."
    }
    cat(paste(test_output, collapse = "\n"))
    cat(interpretation)
  })
  
  output$homogeneity_test <- renderPrint({
    req(input$var_asumsi)
    dat <- sovi_data[[input$var_asumsi]]
    
    if (!is.numeric(dat) || length(na.omit(dat)) < 2) {
      cat("Analisis Dibatalkan: Variabel harus numerik dan memiliki setidaknya 2 data valid (non-NA).")
      return()
    }
    
    group <- as.factor(ifelse(dat <= median(dat, na.rm = TRUE), "Grup Bawah", "Grup Atas"))
    
    if (length(levels(na.omit(group))) < 2) {
      cat("Analisis Dibatalkan: Tidak dapat membentuk dua grup berbeda dari data untuk diuji.")
      return()
    }
    test_result <- car::leveneTest(dat ~ group)
    test_output <- capture.output(print(test_result))
    p_value <- test_result$`Pr(>F)`[1]
    interpretation <- if (p_value < 0.05) {
      "\n\nInterpretasi: \n   Karena p-value < 0.05, varians antar grup TIDAK homogen."
    } else {
      "\n\nInterpretasi: \n   Karena p-value >= 0.05, varians antar grup cenderung homogen."
    }
    cat(paste(test_output, collapse = "\n"))
    cat(interpretation)
  })
  
  
  output$outlier_plot <- renderPlot({
    req(input$var_asumsi)
    boxplot(sovi_data[[input$var_asumsi]], main = paste("Boxplot dari", input$var_asumsi), col = "tomato")
  })
  
  output$outlier_summary <- renderPrint({
    req(input$var_asumsi)
    x <- na.omit(sovi_data[[input$var_asumsi]])
    
    q1 <- quantile(x, 0.25)
    q3 <- quantile(x, 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    outliers <- x[x < lower_bound | x > upper_bound]
    
    cat("Ringkasan Deteksi Outlier (Metode IQR):\n")
    cat("----------------------------------------\n")
    cat("Batas Bawah (Q1 - 1.5*IQR) :", round(lower_bound, 2), "\n")
    cat("Batas Atas (Q3 + 1.5*IQR)  :", round(upper_bound, 2), "\n")
    cat("Jumlah Outlier Ditemukan   :", length(outliers), "\n\n")
    if(length(outliers) > 0){
      cat("Nilai Outlier:\n")
      print(sort(outliers))
    }
  })
  
  output$interpretasiAsumsi <- renderPrint({
    res <- asumsi_results()
    if (!is.null(res$error)) { cat(res$error); return() }
    is_normal <- !is.null(res$shapiro) && res$shapiro$p.value >= 0.05
    is_homogen <- !is.null(res$levene) && res$levene$`Pr(>F)`[1] >= 0.05
    has_outliers <- length(res$outliers) > 0
    cat("===== PENILAIAN ASUMSI KLASIK UNTUK VARIABEL:", res$var_name, "=====\n\n")
    # Normalitas
    if(is.null(res$shapiro)) {
      cat("1. Normalitas: Tidak dapat dinilai (data > 5000). Disarankan inspeksi visual via QQ-Plot.\n")
    } else {
      cat("1. Normalitas: Data", ifelse(is_normal, "memenuhi asumsi normalitas", "TIDAK memenuhi asumsi normalitas"), 
          "(p-value =", round(res$shapiro$p.value, 4), ").\n")
    }
    # Homogenitas
    if(is.null(res$levene)) {
      cat("2. Homogenitas Varians: Tidak dapat dinilai (grup tidak terbentuk).\n")
    } else {
      cat("2. Homogenitas Varians: Data", ifelse(is_homogen, "memenuhi asumsi homogenitas", "TIDAK memenuhi asumsi homogenitas"),
          "(p-value =", round(res$levene$`Pr(>F)`[1], 4), ").\n")
    }
    # Outlier
    cat("3. Outlier: Teridentifikasi", ifelse(has_outliers, "ADANYA", "TIDAK ADANYA"), "outlier signifikan.\n\n")
    # Rekomendasi Final
    cat("----- REKOMENDASI -----\n")
    if (is_normal && is_homogen && !has_outliers) {
      cat("Kesimpulan: Semua asumsi utama terpenuhi. Pengujian statistik PARAMETRIK (misal: Uji-t, ANOVA) sangat direkomendasikan untuk analisis lebih lanjut.\n")
    } else {
      cat("Kesimpulan: Terdapat satu atau lebih asumsi yang dilanggar. Disarankan untuk:\n")
      cat("  a) Melakukan transformasi data (misal: log, sqrt) untuk mencoba memenuhi asumsi.\n")
      cat("  b) Menggunakan metode statistik NON-PARAMETRIK (misal: Uji Mann-Whitney, Kruskal-Wallis) yang lebih robust terhadap pelanggaran asumsi.\n")
    }
  })
  
  output$downloadAsumsiPDF <- downloadHandler(
    filename = function() {
      paste0("Laporan-Uji-Asumsi-", input$var_asumsi, "-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "Laporan-Uji-Asumsi.Rmd")
      file.copy("www/Laporan-Uji-Asumsi.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        results = asumsi_results()
      )
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    })
  
  # Menu Uji Beda Rata-rata
  rv_rata <- reactiveValues(
    last_test_inputs = NULL, 
    all_results = NULL, 
    test_type = NULL
  )
  
  # untuk Uji 1 Kelompok
  observeEvent(input$run_1samp, {
    req(input$var_1samp, !is.na(input$mu_1samp))
    var_data <- na.omit(sovi_data[[input$var_1samp]])
    rv_rata$last_test_inputs <- list(var1 = input$var_1samp, mu = input$mu_1samp)
    rv_rata$test_type <- "Uji 1 Kelompok"
    
    results_list <- list(
      two.sided = t.test(var_data, mu = input$mu_1samp, alternative = "two.sided"),
      less = t.test(var_data, mu = input$mu_1samp, alternative = "less"),
      greater = t.test(var_data, mu = input$mu_1samp, alternative = "greater")
    )
    rv_rata$all_results <- results_list
    
    selected_test <- results_list[[input$alternative_1samp]]
    output$res_1samp <- renderPrint({ print(selected_test) })
    
    p_val <- selected_test$p.value
    conclusion_text <- switch(input$alternative_1samp,
                              "two.sided" = paste0("berbeda secara signifikan dari ", input$mu_1samp),
                              "less"      = paste0("secara signifikan lebih kecil dari ", input$mu_1samp),
                              "greater"   = paste0("secara signifikan lebih besar dari ", input$mu_1samp)
    )
    interpretation_text <- if(p_val < 0.05) {
      paste0("Kesimpulan (H0 Ditolak): Karena p-value (", round(p_val, 4), ") < 0.05, terdapat bukti statistik yang signifikan untuk menyatakan bahwa rata-rata variabel '", input$var_1samp, "' ", conclusion_text, ".")
    } else {
      paste0("Kesimpulan (H0 Gagal Ditolak): Karena p-value (", round(p_val, 4), ") >= 0.05, tidak ada bukti statistik yang cukup untuk menyatakan bahwa rata-rata variabel '", input$var_1samp, "' ", conclusion_text, ".")
    }
    output$int_1samp <- renderUI({ p(HTML(interpretation_text)) })
  })
  
  # untuk Uji 2 Kelompok
  observeEvent(input$run_2samp, {
    req(input$var_2samp_num, input$var_2samp_cat, !is.na(input$split_val_2samp))
    
    df <- na.omit(sovi_data[, c(input$var_2samp_num, input$var_2samp_cat)])
    group1 <- df[df[[input$var_2samp_cat]] <= input$split_val_2samp, ][[input$var_2samp_num]]
    group2 <- df[df[[input$var_2samp_cat]] > input$split_val_2samp, ][[input$var_2samp_num]]
    
    if (length(group1) < 2 || length(group2) < 2) {
      shiny::showNotification("Analisis Gagal: Pastikan setiap kelompok memiliki minimal 2 observasi.", type = "error")
      return(NULL)
    }
    
    rv_rata$last_test_inputs <- list(var_num = input$var_2samp_num, var_cat = input$var_2samp_cat, split = input$split_val_2samp)
    rv_rata$test_type <- "Uji 2 Kelompok"
    
    results_list <- list(
      two.sided = t.test(group1, group2, alternative = "two.sided"),
      less = t.test(group1, group2, alternative = "less"),
      greater = t.test(group1, group2, alternative = "greater")
    )
    rv_rata$all_results <- results_list
    
    selected_test <- results_list[[input$alternative_2samp]]
    output$res_2samp <- renderPrint({ print(selected_test) })
    
    p_val <- selected_test$p.value
    conclusion_text <- switch(input$alternative_2samp,
                              "two.sided" = "berbeda secara signifikan di antara kedua kelompok",
                              "less"      = "pada kelompok pertama secara signifikan lebih kecil daripada kelompok kedua",
                              "greater"   = "pada kelompok pertama secara signifikan lebih besar daripada kelompok kedua"
    )
    interpretation_text <- if(p_val < 0.05) {
      paste0("Kesimpulan (H0 Ditolak): Karena p-value (", round(p_val, 4), ") < 0.05, terdapat bukti statistik yang signifikan bahwa rata-rata variabel '", input$var_2samp_num, "' ", conclusion_text, ".")
    } else {
      paste0("Kesimpulan (H0 Gagal Ditolak): Karena p-value (", round(p_val, 4), ") >= 0.05, tidak ada bukti statistik yang cukup untuk menyatakan bahwa rata-rata variabel '", input$var_2samp_num, "' ", conclusion_text, ".")
    }
    output$int_2samp <- renderUI({ p(HTML(interpretation_text)) })
  })
  
  # untuk Tombol Unduh
  output$downloadRataPDF <- downloadHandler(
    filename = function() {
      paste0("Laporan-Analisis-Uji-Rata-rata-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "Laporan-Uji-Beda-Rata-rata.Rmd")
      file.copy("www/Laporan-Uji-Beda-Rata-rata.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        test_type = rv_rata$test_type,
        inputs = rv_rata$last_test_inputs,
        all_results = rv_rata$all_results
      )
      
      if (is.null(params$all_results)) {
        shiny::showNotification(
          "Gagal: Jalankan salah satu analisis terlebih dahulu!",
          type = "error",
          duration = 5
        )
        return(NULL)
      } else {
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    })
  
  # Menu Uji Proporsi dan Varians
  rv_propvar <- reactiveValues(test_type = NULL, inputs = NULL, all_results = NULL)
  
  # Uji Proporsi 1 kelompok
  observeEvent(input$run_prop1, {
    req(input$x_prop1_manual, input$n_prop1_manual, !is.na(input$p_prop1))
    x <- input$x_prop1_manual
    n <- input$n_prop1_manual
    if(x > n) { shiny::showNotification("Error: x tidak boleh > n.", type="error"); return() }
    rv_propvar$test_type <- "Uji Proporsi 1 Kelompok"
    rv_propvar$inputs <- list(x=x, n=n, p=input$p_prop1, konteks=input$var_prop1_konteks)
    results_list <- list(
      two.sided = prop.test(x=x,n=n,p=input$p_prop1,alternative="two.sided"),
      less=prop.test(x=x,n=n,p=input$p_prop1,alternative="less"),
      greater=prop.test(x=x,n=n,p=input$p_prop1,alternative="greater")
    )
    rv_propvar$all_results <- results_list
    selected_test <- results_list[[input$alt_prop1]]
    output$res_prop1 <- renderPrint({ print(selected_test) })
    p_val <- selected_test$p.value
    interpretation_text <- if(p_val < 0.05){"<b>Kesimpulan (H0 Ditolak):</b> Proporsi sebenarnya berbeda signifikan dari nilai hipotesis."}else{"<b>Kesimpulan (H0 Gagal Ditolak):</b> Tidak ada bukti proporsi berbeda dari nilai hipotesis."}
    output$int_prop1 <- renderUI({ p(HTML(interpretation_text)) })
  })
  
  # Uji Proporsi 2 Kelompok
  observeEvent(input$run_prop2, {
    req(input$var_prop2_cond, input$cond_prop2_text, input$var_prop2_group)
    
    tryCatch({
      df <- na.omit(sovi_data[, c(input$var_prop2_cond, input$var_prop2_group)])
      median_value <- median(df[[input$var_prop2_group]], na.rm = TRUE)
      grup1_df <- df[df[[input$var_prop2_group]] <= median_value, ]
      grup2_df <- df[df[[input$var_prop2_group]] > median_value, ]
      
      if(nrow(grup1_df) < 2 || nrow(grup2_df) < 2) {
        shiny::showNotification("Error: Tidak dapat membentuk dua grup valid dari median.", type="error")
        return()
      }
      
      n1 <- nrow(grup1_df); x1 <- sum(eval(parse(text=paste("grup1_df[[input$var_prop2_cond]]", input$cond_prop2_text))))
      n2 <- nrow(grup2_df); x2 <- sum(eval(parse(text=paste("grup2_df[[input$var_prop2_cond]]", input$cond_prop2_text))))
      
      rv_propvar$test_type <- "Uji Proporsi 2 Kelompok"
      rv_propvar$inputs <- list(var_cond=input$var_prop2_cond, cond=input$cond_prop2_text, var_group=input$var_prop2_group, split=median_value)
      
      results_list <- list(
        two.sided=prop.test(x=c(x1,x2),n=c(n1,n2),alternative="two.sided"),
        less=prop.test(x=c(x1,x2),n=c(n1,n2),alternative="less"),
        greater=prop.test(x=c(x1,x2),n=c(n1,n2),alternative="greater")
      )
      rv_propvar$all_results <- results_list
      
      selected_test <- results_list[[input$alt_prop2]]
      output$res_prop2 <- renderPrint({
        cat("Ringkasan Data Dihitung:\n")
        cat(paste0("Grup 1 (", input$var_prop2_group, " <= ", round(median_value, 2), "):\n"))
        cat(paste0("  x₁ (sukses) = ", x1, ", n₁ (total) = ", n1, "\n"))
        cat(paste0("Grup 2 (", input$var_prop2_group, " > ", round(median_value, 2), "):\n"))
        cat(paste0("  x₂ (sukses) = ", x2, ", n₂ (total) = ", n2, "\n\n"))
        print(selected_test)
      })
      
      p_val <- selected_test$p.value
      
      conclusion_text <- switch(input$alt_prop2,
                                "two.sided" = "terdapat perbedaan proporsi yang signifikan antara kedua kelompok",
                                "less"      = "proporsi pada kelompok pertama secara signifikan lebih kecil daripada kelompok kedua",
                                "greater"   = "proporsi pada kelompok pertama secara signifikan lebih besar daripada kelompok kedua"
      )
      
      if (!is.na(p_val) && p_val < 0.05) {
        interpretation_text <- paste0("<b>Kesimpulan (H₀ Ditolak):</b> Karena p-value (", round(p_val, 4), ") < 0.05, ", conclusion_text, ".")
      } else if (!is.na(p_val)) {
        interpretation_text <- paste0("<b>Kesimpulan (H₀ Gagal Ditolak):</b> Karena p-value (", round(p_val, 4), ") >= 0.05, tidak ada bukti yang cukup untuk menyatakan bahwa ", conclusion_text, ".")
      } else {
        interpretation_text <- "<b>Interpretasi Gagal:</b> Hasil p-value tidak dapat dihitung dari data yang diberikan."
      }
      
      rv_propvar$last_test_interpretation <- interpretation_text # Simpan interpretasi untuk diunduh
      output$int_prop2 <- renderUI({ p(HTML(interpretation_text)) })
      
    }, error = function(e) { 
      shiny::showNotification(paste("Error:", e$message), type = "error") 
    })
  })
  
  # Uji Varians 2 kelompok
  observeEvent(input$run_ftest, {
    req(input$var1_ftest, input$var2_ftest)
    var1_data <- na.omit(sovi_data[[input$var1_ftest]])
    var2_data <- na.omit(sovi_data[[input$var2_ftest]])
    rv_propvar$test_type <- "Uji Varians 2 Kelompok"
    rv_propvar$inputs <- list(var1 = input$var1_ftest, var2 = input$var2_ftest)
    results_list <- list(
      two.sided=var.test(var1_data, var2_data, alternative="two.sided"),
      less=var.test(var1_data, var2_data, alternative="less"),
      greater=var.test(var1_data, var2_data, alternative="greater")
    )
    rv_propvar$all_results <- results_list
    selected_test <- results_list[[input$alt_ftest]]
    output$res_ftest <- renderPrint({ print(selected_test) })
    p_val <- selected_test$p.value
    interpretation_text <- if(p_val < 0.05){"<b>Kesimpulan (H0 Ditolak):</b> Terdapat perbedaan varians signifikan."}else{"<b>Kesimpulan (H0 Gagal Ditolak):</b> Tidak ada perbedaan varians signifikan."}
    output$int_ftest <- renderUI({ p(HTML(interpretation_text)) })
  })
  
  output$downloadPropVarPDF <- downloadHandler(
    filename = function() { paste0("Laporan-Proporsi-Varians-", Sys.Date(), ".pdf") },
    content = function(file) {
      tempReport <- file.path(tempdir(), "Laporan-Uji-PropVar.Rmd")
      file.copy("www/Laporan-Uji-PropVar.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        test_type = rv_propvar$test_type,
        inputs = rv_propvar$inputs,
        all_results = rv_propvar$all_results
      )
      if (is.null(params$all_results)) {
        shiny::showNotification("Gagal: Jalankan analisis dulu!", type="error"); return(NULL)
      } else {
        rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
      }
    })
  
  # ANOVA
  rv_anova <- reactiveValues(
    test_type = NULL, 
    inputs = NULL,
    asumsi_status = NULL,
    anova_result = NULL,
    posthoc_result = NULL,
    interpretation = NULL
  )
  
  # untuk ANOVA 1 Arah
  observeEvent(input$run_anova1, {
    req(input$var_anova1_dv, input$var_anova1_iv)
    
    df <- na.omit(sovi_data[, c(input$var_anova1_dv, input$var_anova1_iv)])
    dv_data <- df[[input$var_anova1_dv]]
    iv_data <- df[[input$var_anova1_iv]]
    
    quantiles <- quantile(iv_data, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
    iv_factor <- cut(iv_data, breaks = unique(quantiles), labels = c("Rendah", "Sedang", "Tinggi"), include.lowest = TRUE)
    
    shapiro_test <- shapiro.test(dv_data)
    levene_test <- car::leveneTest(dv_data ~ iv_factor)
    p_shapiro <- shapiro_test$p.value
    p_levene <- levene_test$`Pr(>F)`[1]
    
    asumsi_lolos <- p_shapiro >= 0.05 && p_levene >= 0.05
    
    output$asumsi_anova1_status <- renderUI({
      if(asumsi_lolos) {
        tags$div(class = "alert alert-success", h4("LOLOS"),
                 p(paste0("Asumsi Normalitas (p = ", round(p_shapiro, 3), ") dan Homogenitas (p = ", round(p_levene, 3), ") terpenuhi.")))
      } else {
        tags$div(class = "alert alert-danger", h4("GAGAL"),
                 p(paste0("Asumsi tidak terpenuhi. Normalitas (p = ", round(p_shapiro, 3), "), Homogenitas (p = ", round(p_levene, 3), "). Disarankan menggunakan uji non-parametrik (Kruskal-Wallis).")))
      }
    })
    
    if (asumsi_lolos) {
      anova_model <- aov(dv_data ~ iv_factor)
      anova_summary <- summary(anova_model)
      anova_p_val <- anova_summary[[1]]$`Pr(>F)`[1]
      posthoc_res <- if (!is.na(anova_p_val) && anova_p_val < 0.05) TukeyHSD(anova_model) else NULL
      
      output$res_anova1 <- renderPrint({ print(anova_summary) })
      output$res_posthoc_anova1 <- renderPrint({ if(!is.null(posthoc_res)) print(posthoc_res) else cat("Tidak dilakukan (karena hasil ANOVA tidak signifikan).") })
      
      interpretation_text <- paste0("Hasil ANOVA menunjukkan bahwa terdapat ", 
                                    if(!is.na(anova_p_val) && anova_p_val < 0.05) "perbedaan rata-rata yang signifikan" else "TIDAK ada perbedaan rata-rata yang signifikan",
                                    " pada variabel '", input$var_anova1_dv, "' di antara kelompok '", input$var_anova1_iv, "' (p = ", round(anova_p_val, 4), ").")
      if (!is.null(posthoc_res)) {
        interpretation_text <- paste0(interpretation_text, " Uji lanjut Tukey HSD menunjukkan detail kelompok mana saja yang berbeda secara signifikan.")
      }
      output$int_anova1 <- renderUI({ p(HTML(interpretation_text)) })
      
      rv_anova$test_type <- "ANOVA 1 Arah"
      rv_anova$inputs <- list(dv = input$var_anova1_dv, iv = input$var_anova1_iv)
      rv_anova$asumsi_status <- "Lolos"
      rv_anova$anova_result <- anova_summary
      rv_anova$posthoc_result <- posthoc_res
      rv_anova$interpretation <- interpretation_text
      
    } else {
      output$res_anova1 <- renderPrint({ cat("Analisis ANOVA dibatalkan karena asumsi tidak terpenuhi.") })
      output$res_posthoc_anova1 <- renderPrint({ cat("") })
      output$int_anova1 <- renderUI({ p("") })
      rv_anova$test_type <- NULL
    }
  })
  
  # untuk ANOVA 2 Arah
  observeEvent(input$run_anova2, {
    req(input$var_anova2_dv, input$var_anova2_iv1, input$var_anova2_iv2)
    
    df <- na.omit(sovi_data[, c(input$var_anova2_dv, input$var_anova2_iv1, input$var_anova2_iv2)])
    names(df) <- c("dv", "iv1", "iv2")
    
    df$iv1_factor <- cut(df$iv1, breaks = 2, labels = c("Rendah", "Tinggi"))
    df$iv2_factor <- cut(df$iv2, breaks = 2, labels = c("Rendah", "Tinggi"))
    
    model_res <- aov(dv ~ iv1_factor * iv2_factor, data = df)
    shapiro_test <- shapiro.test(model_res$residuals)
    levene_test <- car::leveneTest(dv ~ iv1_factor * iv2_factor, data = df)
    p_shapiro <- shapiro_test$p.value
    p_levene <- levene_test$`Pr(>F)`[1]
    
    asumsi_lolos <- p_shapiro >= 0.05 && p_levene >= 0.05
    
    output$asumsi_anova2_status <- renderUI({
      if(asumsi_lolos) {
        tags$div(class = "alert alert-success", h4("LOLOS"), p("Asumsi Normalitas Residual dan Homogenitas Varians terpenuhi."))
      } else {
        tags$div(class = "alert alert-danger", h4("GAGAL"), p("Asumsi tidak terpenuhi."))
      }
    })
    
    if (asumsi_lolos) {
      anova_summary <- summary(model_res)
      p_values <- anova_summary[[1]]$`Pr(>F)`
      posthoc_res <- if (any(p_values < 0.05, na.rm = TRUE)) TukeyHSD(model_res) else NULL
      
      output$res_anova2 <- renderPrint({ print(anova_summary) })
      output$res_posthoc_anova2 <- renderPrint({ if(!is.null(posthoc_res)) print(posthoc_res) else cat("Tidak dilakukan (karena tidak ada efek signifikan).") })
      
      p_iv1 <- p_values[1]; p_iv2 <- p_values[2]; p_interaksi <- p_values[3]
      interpretation_text <- paste0(
        "<ul>",
        "<li><b>Efek utama '", input$var_anova2_iv1, "'</b>: ", if(!is.na(p_iv1) && p_iv1 < 0.05) "Signifikan" else "Tidak Signifikan", " (p = ", round(p_iv1, 4), ").</li>",
        "<li><b>Efek utama '", input$var_anova2_iv2, "'</b>: ", if(!is.na(p_iv2) && p_iv2 < 0.05) "Signifikan" else "Tidak Signifikan", " (p = ", round(p_iv2, 4), ").</li>",
        "<li><b>Efek Interaksi</b>: ", if(!is.na(p_interaksi) && p_interaksi < 0.05) "Signifikan" else "Tidak Signifikan", " (p = ", round(p_interaksi, 4), ").</li>",
        "</ul>"
      )
      output$int_anova2 <- renderUI({ HTML(interpretation_text) })
      
      rv_anova$test_type <- "ANOVA 2 Arah"
      rv_anova$inputs <- list(dv=input$var_anova2_dv, iv1=input$var_anova2_iv1, iv2=input$var_anova2_iv2)
      rv_anova$asumsi_status <- "Lolos"
      rv_anova$anova_result <- anova_summary
      rv_anova$posthoc_result <- posthoc_res
      rv_anova$interpretation <- interpretation_text
      
    } else {
      output$res_anova2 <- renderPrint({ cat("Analisis ANOVA dibatalkan karena asumsi tidak terpenuhi.") })
      output$res_posthoc_anova2 <- renderPrint({ cat("") })
      output$int_anova2 <- renderUI({ p("") })
      rv_anova$test_type <- NULL
    }
  })
  
  # untuk Tombol Unduh
  output$downloadAnovaPDF <- downloadHandler(
    filename = function() { paste0("Laporan-ANOVA-", Sys.Date(), ".pdf") },
    content = function(file) {
      tempReport <- file.path(tempdir(), "Laporan-ANOVA.Rmd")
      file.copy("www/Laporan-ANOVA.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        test_type = rv_anova$test_type,
        inputs = rv_anova$inputs,
        asumsi_status = rv_anova$asumsi_status,
        anova_result = rv_anova$anova_result,
        posthoc_result = rv_anova$posthoc_result,
        interpretation = rv_anova$interpretation
      )
      
      if (is.null(params$test_type)) {
        shiny::showNotification("Gagal: Jalankan analisis dulu!", type="error"); return(NULL)
      }
      
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    })
  
  # Analisis Regresi

  observe({
    req(input$y_reg, input$x_reg)
    pilihan_var <- c(input$y_reg, input$x_reg)
    updateSelectInput(session, "var_transform", choices = pilihan_var)
  })
  
  apply_transform <- function(x, method) {
    if (method == "Log") return(log1p(x))
    if (method == "Sqrt") return(sqrt(x))
    return(x)
  }
  
  data_transformed <- reactive({
    df <- sovi_data
    if (!is.null(input$var_transform) && input$transform_method != "None") {
      for (var in input$var_transform) {
        df[[var]] <- apply_transform(df[[var]], input$transform_method)
      }
    }
    return(df)
  })
  
  model_fit <- eventReactive(input$run_analysis, {
    req(input$y_reg, input$x_reg)
    data_use <- data_transformed()
    
    formula_text <- paste(input$y_reg, "~", paste(input$x_reg, collapse = " + "))
    
    lm(as.formula(formula_text), data = data_use)
  })
  
  output$reg_summary <- renderPrint({
    summary(model_fit())
  })
  
  output$vif_result <- renderPrint({
    # VIF hanya bisa dihitung jika ada > 1 variabel X
    if (length(input$x_reg) > 1) {
      print(car::vif(model_fit()))
    } else {
      cat("VIF tidak dihitung untuk regresi dengan satu variabel independen.")
    }
  })
  
  output$qq_plot <- renderPlot({
    plot(model_fit(), which = 2) # which = 2 adalah plot QQ-Plot
  })
  
  output$shapiro_result <- renderPrint({
    shapiro.test(resid(model_fit()))
  })
  
  output$resid_plot <- renderPlot({
    plot(model_fit(), which = 1) # which = 1 adalah plot Residuals vs Fitted
  })
  
  output$bp_result <- renderPrint({
    lmtest::bptest(model_fit())
  })
  
  output$interpretasi_regresi <- renderUI({
    model <- model_fit()
    model_summary <- summary(model)
    
    p_shapiro <- shapiro.test(resid(model))$p.value
    p_bp <- lmtest::bptest(model)$p.value
    vif_vals <- if(length(coef(model)) > 2) car::vif(model) else c(0)
    adj_r_sq <- model_summary$adj.r.squared
    f_stat <- model_summary$fstatistic
    p_f_stat <- if(!is.null(f_stat)) pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE) else 1
    coef_summary <- model_summary$coefficients
    
    html_asumsi <- paste(
      "<h4>Ringkasan Uji Asumsi</h4>",
      "<ul>",
      paste0("<li><b>Multikolinearitas (VIF):</b> ", if(any(vif_vals > 10)) "Asumsi Terlanggar (VIF > 10)." else "Asumsi terpenuhi (VIF < 10).", "</li>"),
      paste0("<li><b>Normalitas Residual (Shapiro-Wilk):</b> ", if(!is.na(p_shapiro) && p_shapiro < 0.05) "Asumsi terlanggar (p < 0.05)." else "Asumsi terpenuhi (p ≥ 0.05).", "</li>"),
      paste0("<li><b>Homoskedastisitas (Breusch-Pagan):</b> ", if(!is.na(p_bp) && p_bp < 0.05) "Asumsi terlanggar (terjadi heteroskedastisitas)." else "Asumsi terpenuhi (homoskedastisitas).", "</li>"),
      "</ul>"
    )
    
    html_model <- paste(
      "<h4>Interpretasi Kelayakan Model</h4>",
      "<ul>",
      paste0("<li><b>Kelayakan Model (Uji-F):</b> Berdasarkan Uji F, model secara keseluruhan ", if(!is.na(p_f_stat) && p_f_stat < 0.05) "<b>layak (signifikan)</b>" else "<b>tidak layak (tidak signifikan)</b>", " untuk digunakan dalam prediksi (p = ", round(p_f_stat, 4), ").</li>"),
      paste0("<li><b>Koefisien Determinasi (Adjusted R-squared):</b> Sebesar <b>", round(adj_r_sq * 100, 2), "%</b> keragaman pada variabel dependen ('", input$y_reg, "') mampu dijelaskan oleh variabel-variabel independen di dalam model. Sisanya dijelaskan oleh variabel lain di luar model.</li>"),
      "</ul>"
    )
    
    interpretasi_coef_list <- ""
    for (i in 2:nrow(coef_summary)) {
      var_name <- rownames(coef_summary)[i]
      estimate <- coef_summary[i, 1]
      p_val <- coef_summary[i, 4]
      
      if (!is.na(p_val) && p_val < 0.05) {
        arah <- if(estimate > 0) "positif (menaikkan)" else "negatif (menurunkan)"
        interpretasi_coef_list <- paste0(interpretasi_coef_list, "<li>Variabel <b>", var_name, "</b> berpengaruh secara signifikan dan ", arah, " terhadap <b>", input$y_reg, "</b>. Setiap kenaikan satu unit pada '", var_name, "', akan mengubah '", input$y_reg, "' sebesar ", round(estimate, 4), ", dengan asumsi variabel lain konstan.</li>")
      }
    }
    
    html_coef <- paste(
      "<h4>Interpretasi Koefisien (Variabel yang Signifikan)</h4>",
      "<ul>",
      if(interpretasi_coef_list == "") "<li>Tidak ada variabel independen yang berpengaruh signifikan secara statistik terhadap variabel dependen.</li>" else interpretasi_coef_list,
      "</ul>"
    )
    
    HTML(paste(html_asumsi, html_model, html_coef))
  })
  
  output$downloadRegresiPDF <- downloadHandler(
    filename = function() { paste0("Laporan-Regresi-", Sys.Date(), ".pdf") },
    content = function(file) {
      req(model_fit())
      
      tempReport <- file.path(tempdir(), "Laporan-Regresi.Rmd")
      file.copy("www/Laporan-Regresi.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(model = model_fit(), 
                     y_var = input$y_reg, 
                     x_vars = input$x_reg)
      
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent=globalenv()))
    })
  
  # Unduh Data

  data_to_download <- reactive({
    req(input$vars_unduh)
    dplyr::select(sovi_data, all_of(input$vars_unduh))
  })
  
  output$tabel_preview_unduh <- renderDT({
    datatable(
      data_to_download(),
      options = list(scrollX = TRUE, pageLength = 5),
      rownames = FALSE
    )
  })
  
  output$download_data_button <- downloadHandler(
    filename = function() {
      paste0("sovi_data_pilihan-", Sys.Date(), ".", input$format_unduh)
    },
    content = function(file) {
      data_dipilih <- data_to_download()
      
      if (input$format_unduh == "csv") {
        write.csv(data_dipilih, file, row.names = FALSE)
      } else if (input$format_unduh == "xlsx") {
        writexl::write_xlsx(data_dipilih, path = file)
      } else if (input$format_unduh == "sav") {
        haven::write_sav(data_dipilih, path = file)
      }
    })
}

shinyApp(ui, server)