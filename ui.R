#                         USER INTERFACE (UI)                                 #

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "DataVerse SOVI"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Beranda", tabName = "beranda", icon = icon("tachometer-alt")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("sliders-h")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar")),
      menuItem("Peta Interaktif", tabName = "peta", icon = icon("map-marked-alt")),
      menuItem("Uji Asumsi Data", tabName = "uji_asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", icon = icon("flask"),
               menuSubItem("Uji Beda Rata-rata", tabName = "rata_rata"),
               menuSubItem("Uji Proporsi & Varians", tabName = "prop_var"),
               menuSubItem("ANOVA", tabName = "anova")
      ),
      menuItem("Analisis Regresi", tabName = "regresi", icon = icon("chart-line")),
      menuItem("Unduh Data", tabName = "unduh_data", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .beranda-header { text-align: center; padding: 20px; margin-bottom: 20px; background-color: #f8f9fa; border: 1px solid #e3e3e3; border-radius: 8px; }
        .beranda-header h2 { font-weight: 600; color: #3c8dbc; }
        .guide-step { margin-bottom: 15px; display: flex; align-items: flex-start; }
        .guide-step .icon-container { font-size: 22px; color: #3c8dbc; margin-right: 15px; width: 40px; text-align: center; padding-top: 2px; }
        .guide-step .text-container { flex: 1; }
        .guide-step strong { display: block; font-size: 16px; margin-bottom: 3px; }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "beranda",
        fluidPage(
          div(class = "beranda-header",
              h2(icon("rocket"), " DataVerse: Visualisasi dan Analisis SOVI Indonesia"),
              p(class = "lead", "Platform interaktif untuk eksplorasi data kerentanan sosial di seluruh Indonesia.")
          ),
          fluidRow(
            valueBoxOutput("total_kabkota", width = 3),
            valueBoxOutput("avg_poverty", width = 3),
            valueBoxOutput("avg_noelectric", width = 3),
            valueBoxOutput("total_populasi", width = 3)
          ),
          fluidRow(
            column(width = 6,
                   box(title = tagList(icon("info-circle"), " Tentang Dashboard Ini"), status = "primary", solidHeader = TRUE, width = NULL,
                       p("Dashboard ini adalah alat bantu untuk memahami kerentanan sosial (SOVI) di Indonesia. Anda dapat melakukan berbagai analisis, mulai dari manajemen data mentah hingga analisis regresi yang kompleks, semuanya dalam satu tempat."),
                       h4("Fitur Utama:"),
                       tags$ul(
                         tags$li("Manajemen dan kategorisasi data secara interaktif."),
                         tags$li("Eksplorasi data melalui statistik deskriptif dan visualisasi."),
                         tags$li("Pengujian asumsi statistik klasik untuk validitas data."),
                         tags$li("Analisis inferensia (Uji-t, ANOVA, Uji Proporsi)."),
                         tags$li("Pemodelan regresi linier dengan pemeriksaan asumsi."),
                         tags$li("Unduh data dan laporan analisis dalam format PDF, CSV, Excel, atau SPSS.")
                       )
                   ),
                   box(title = tagList(icon("book-reader"), " Panduan Penggunaan"), status = "primary", solidHeader = TRUE, width = NULL, collapsible = TRUE,
                       div(class="guide-step", div(class="icon-container", icon("sliders-h")), div(class="text-container", strong("Manajemen Data:"), "Ubah variabel numerik menjadi kategorik dengan berbagai metode.")),
                       div(class="guide-step", div(class="icon-container", icon("chart-bar")), div(class="text-container", strong("Eksplorasi Data:"), "Pilih variabel untuk melihat statistik, plot distribusi, dan korelasi.")),
                       div(class="guide-step", div(class="icon-container", icon("check-circle")), div(class="text-container", strong("Uji Asumsi Data:"), "Periksa normalitas, homogenitas, dan outlier pada data Anda.")),
                       div(class="guide-step", div(class="icon-container", icon("flask")), div(class="text-container", strong("Statistik Inferensia:"), "Lakukan uji beda rata-rata, proporsi, dan ANOVA untuk menarik kesimpulan.")),
                       div(class="guide-step", div(class="icon-container", icon("chart-line")), div(class="text-container", strong("Analisis Regresi:"), "Bangun model regresi untuk melihat pengaruh antar variabel.")),
                       div(class="guide-step", div(class="icon-container", icon("download")), div(class="text-container", strong("Unduh Data & Laporan:"), "Simpan data atau hasil analisis dalam berbagai format."))
                   )
            ),
            column(width = 6,
                   box(title = tagList(icon("database"), 
                                       " Metadata Variabel"), 
                       status = "primary", 
                       solidHeader = TRUE, 
                       width = NULL, 
                       collapsible = TRUE,
                       div(style = "overflow-x: auto;",
                           HTML("<table class='table table-bordered table-hover'><thead style='background-color: #f4f4f4;'><tr><th>Variabel</th><th>Tipe Data</th><th>Deskripsi</th></tr></thead><tbody><tr><td><b>Province_Name</b></td><td>Teks (Character)</td><td>Nama provinsi di Indonesia</td></tr><tr><td><b>City_Name</b></td><td>Teks (Character)</td><td>Nama kabupaten/kota di Indonesia</td></tr><tr><td>DISTRICTCODE</td><td>Teks (Character)</td><td>Kode wilayah administratif kabupaten/kota</td></tr><tr><td>CHILDREN</td><td>Numerik (Persentase)</td><td>Persentase penduduk berusia di bawah lima tahun</td></tr><tr><td>FEMALE</td><td>Numerik (Persentase)</td><td>Persentase penduduk perempuan</td></tr><tr><td>ELDERLY</td><td>Numerik (Persentase)</td><td>Persentase penduduk berusia di atas 65 tahun</td></tr><tr><td>FHEAD</td><td>Numerik (Persentase)</td><td>Persentase rumah tangga dengan kepala keluarga perempuan</td></tr><tr><td>FAMILYSIZE</td><td>Numerik (Rata-rata)</td><td>Rata-rata jumlah anggota rumah tangga</td></tr><tr><td>NOELECTRIC</td><td>Numerik (Persentase)</td><td>Persentase rumah tangga tanpa listrik</td></tr><tr><td>LOWEDU</td><td>Numerik (Persentase)</td><td>Persentase penduduk usia 15+ dengan pendidikan rendah</td></tr><tr><td>GROWTH</td><td>Numerik (Persentase)</td><td>Persentase pertumbuhan penduduk</td></tr><tr><td>POVERTY</td><td>Numerik (Persentase)</td><td>Persentase penduduk miskin</td></tr><tr><td>ILLITERATE</td><td>Numerik (Persentase)</td><td>Persentase penduduk buta huruf</td></tr><tr><td>NOTRAINING</td><td>Numerik (Persentase)</td><td>Persentase rumah tangga tanpa pelatihan kebencanaan</td></tr><tr><td>DPRONE</td><td>Numerik (Persentase)</td><td>Persentase rumah tangga di daerah rawan bencana</td></tr><tr><td>RENTED</td><td>Numerik (Persentase)</td><td>Persentase rumah tangga yang menyewa rumah</td></tr><tr><td>NOSEWER</td><td>Numerik (Persentase)</td><td>Persentase rumah tangga tanpa sistem pembuangan limbah</td></tr><tr><td>TAPWATER</td><td>Numerik (Persentase)</td><td>Persentase rumah tangga pengguna air ledeng</td></tr><tr><td>POPULATION</td><td>Integer (Jumlah)</td><td>Total populasi di tiap kabupaten/kota</td></tr></tbody></table>")
                       )
                   )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "manajemen", 
        fluidRow(
          box(width = 4, 
              title = "Pengaturan Kategorisasi", 
              status = "primary", 
              solidHeader = TRUE,
              selectInput("var_kontinyu", 
                          "1. Pilih Variabel Kontinyu:", 
                          choices = names(sovi_data)[sapply(sovi_data, is.numeric)]),
              selectInput("metode_kat", 
                          "2. Pilih Metode Kategorisasi:",
                          choices = c("Kuantil (Jumlah Anggota Sama)" = "quantile", "Interval Sama (Rentang Nilai Sama)" = "equal")),
              numericInput("n_kat", 
                           "3. Tentukan Jumlah Kategori:", 
                           value = 3, 
                           min = 2, 
                           max = 10),
              hr(),
              helpText("4. Beri Nama untuk Setiap Kategori:"),
              uiOutput("kat_labels_ui"),
              br(),
              actionButton("proses_kat", 
                           "Proses Kategorisasi", 
                           icon = icon("play"), 
                           class = "btn-success")
          ),
          box(width = 8, 
              title = "Hasil Kategorisasi", 
              status = "info", 
              solidHeader = TRUE,
              DTOutput("tabel_kat"),
              br(),
              uiOutput("interpretasi_kat"),
              hr(),
              uiOutput("download_buttons_ui")
          )
        )
      ),
      
      tabItem(tabName = "eksplorasi",
              fluidRow(
                box(title = "Pilih Variabel (Filter Utama)", 
                    width = 12, 
                    status = "primary", 
                    solidHeader = TRUE,
                    uiOutput("vars_selected_ui"),
                    selectInput("var_plot", 
                                "Variabel untuk Plot Tunggal:", 
                                choices = NULL)
                )
              ),
              fluidRow(box(title = "Statistik Deskriptif", 
                           width = 12, 
                           status = "info", 
                           solidHeader = TRUE, 
                           DT::dataTableOutput("tabel_stat") %>% withSpinner())),
              fluidRow(
                box(title = "Boxplot", 
                    width = 4, 
                    status = "warning", 
                    solidHeader = TRUE, 
                    plotOutput("boxplot") %>% withSpinner()),
                box(title = "QQ Plot", 
                    width = 4, 
                    status = "warning", 
                    solidHeader = TRUE, 
                    plotOutput("qqplot") %>% withSpinner()),
                box(title = "Histogram", 
                    width = 4, 
                    status = "warning", 
                    solidHeader = TRUE, 
                    plotOutput("histplot") %>% withSpinner())
              ),
              fluidRow(box(title = "Matriks Korelasi", 
                           width = 12, 
                           status = "success", 
                           solidHeader = TRUE, 
                           plotOutput("corrplot") %>% withSpinner())),
              fluidRow(box(title = "Interpretasi", 
                           width = 12, 
                           status = "primary", 
                           solidHeader = TRUE, 
                           verbatimTextOutput("interpretasi"))),
              fluidRow(box(title = "Unduh Hasil PDF", 
                           width = 12, 
                           status = "danger", 
                           solidHeader = TRUE, 
                           downloadButton("downloadPDF", "Unduh PDF")))
      ),
      
      tabItem(tabName = "peta",
              h2("Peta Sebaran Data Kerentanan Sosial"),
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = "Pengaturan Peta",
                  # Pilihan variabel untuk peta, akan diupdate dari server
                  selectInput("var_peta", "Pilih Variabel untuk Ditampilkan:", choices = NULL)
                )
              ),
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Peta Interaktif",
                  # Output untuk peta Leaflet
                  leafletOutput("peta_leaflet", height = "600px") %>% withSpinner()
                ),
                box(
                  width = 4, status = "success", solidHeader = TRUE,
                  title = "Interpretasi Peta",
                  # Output untuk teks interpretasi
                  uiOutput("interpretasi_peta")
                )
              )
      ),
      
      tabItem(tabName = "uji_asumsi",
              h2("Uji Asumsi Klasik"),
              fluidRow(
                box(title = "Treatment Data (Transformasi)", 
                    status = "danger", 
                    solidHeader = TRUE, 
                    width = 12, 
                    collapsible = TRUE, 
                    collapsed = TRUE,
                    p("Gunakan fitur ini jika variabel tidak lolos uji asumsi. Variabel baru hasil transformasi akan dapat digunakan di semua menu analisis."),
                    fluidRow(
                      column(width = 4,
                             selectInput("var_to_transform", 
                                         "1. Pilih Variabel untuk Treatment:", 
                                         choices = NULL)
                      ),
                      column(width = 4,
                             selectInput("transform_method_asumsi", 
                                         "2. Pilih Metode Transformasi:",
                                         choices = c("Logaritma (log1p)" = "log",
                                                     "Akar Kuadrat (sqrt)" = "sqrt",
                                                     "Arcsin Akar Kuadrat (untuk %)" = "arcsin_sqrt"))
                      ),
                      column(width = 4,
                             textInput("new_var_name", 
                                       "3. Nama Variabel Baru:", 
                                       placeholder = "Contoh: POVERTY_log"),
                             actionButton("apply_transform_button", 
                                          "Terapkan Treatment", 
                                          icon = icon("magic"), 
                                          class = "btn-success")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Pilih Variabel untuk Uji", 
                    width = 12, 
                    status = "primary", 
                    solidHeader = TRUE,
                    selectInput("var_asumsi", 
                                "Variabel Uji:",
                                choices = NULL)
                )
              ),
              fluidRow(
                box(width = 6, 
                    title = "Uji Normalitas (Shapiro-Wilk)", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    verbatimTextOutput("shapiro_test")),
                box(width = 6, 
                    title = "Uji Homogenitas Varians (Levene's Test)", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    verbatimTextOutput("homogeneity_test"))
              ),
              fluidRow(
                box(width = 6, 
                    title = "Deteksi Outlier (Boxplot)", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    plotOutput("outlier_plot")),
                box(width = 6, 
                    title = "Ringkasan Outlier", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    verbatimTextOutput("outlier_summary"))
              ),
              fluidRow(box(width = 12, status = "info", 
                           solidHeader = TRUE, 
                           title = "Ringkasan & Rekomendasi Statistik", 
                           collapsible = TRUE, 
                           verbatimTextOutput("interpretasiAsumsi"))),
              fluidRow(box(width = 12, 
                           status = "danger", 
                           solidHeader = TRUE, 
                           title = "Unduh Laporan Uji Asumsi", 
                           downloadButton("downloadAsumsiPDF", "Unduh PDF Hasil Uji Asumsi")))
      ),
      
      tabItem(tabName = "rata_rata",
              h2("Uji Beda Rata-rata (t-test)"),
              tabBox(
                id = "ttest_tabs", width = 12,
                tabPanel("Uji 1 Kelompok", fluidRow(
                  box(width = 4, status = "primary", solidHeader = TRUE, title = "Pengaturan Analisis",
                      selectInput("var_1samp", "Pilih Variabel Numerik:", choices = NULL),
                      numericInput("mu_1samp", "Nilai Hipotesis (μ₀):", value = 10),
                      radioButtons("alternative_1samp", "Hipotesis Alternatif:", choices = c("Dua Arah" = "two.sided", "Kurang Dari" = "less", "Lebih Dari" = "greater"), inline = TRUE),
                      actionButton("run_1samp", "Jalankan", icon = icon("play"), class = "btn-success")
                  ),
                  box(width = 8, status = "info", solidHeader = TRUE, title = "Hasil Analisis", verbatimTextOutput("res_1samp") %>% withSpinner(), hr(), h4("Interpretasi:"), uiOutput("int_1samp") %>% withSpinner())
                )),
                tabPanel("Uji 2 Kelompok Independen", fluidRow(
                  box(width = 4, status = "primary", solidHeader = TRUE, title = "Pengaturan Analisis",
                      selectInput("var_2samp_num", "1. Variabel Numerik:", choices = NULL),
                      selectInput("var_2samp_cat", "2. Variabel Grup:", choices = NULL),
                      numericInput("split_val_2samp", "3. Titik Potong Grup:", value = 0),
                      radioButtons("alternative_2samp", "Hipotesis Alternatif:", choices = c("Dua Arah" = "two.sided", "Kurang Dari" = "less", "Lebih Dari" = "greater"), inline = TRUE),
                      actionButton("run_2samp", "Jalankan", icon = icon("play"), class = "btn-success")
                  ),
                  box(width = 8, status = "info", solidHeader = TRUE, title = "Hasil Analisis", verbatimTextOutput("res_2samp") %>% withSpinner(), hr(), h4("Interpretasi:"), uiOutput("int_2samp") %>% withSpinner())
                ))
              ),
              fluidRow(box(width = 12, status = "danger", solidHeader = TRUE, title = "Unduh Laporan", downloadButton("downloadRataPDF", "Unduh PDF")))
      ),
      
      tabItem(tabName = "prop_var",
              h2("Uji Proporsi dan Uji Varians"),
              tabBox(
                id = "propvar_tabs", width = 12,
                tabPanel("Uji Proporsi 1 Kelompok", fluidRow(
                  box(width=4, status="primary", solidHeader=TRUE, title="Pengaturan",
                      selectInput("var_prop1_konteks", "1. Konteks Variabel:", choices=NULL),
                      numericInput("x_prop1_manual", "2. Jumlah 'Sukses' (x):", value=50, min=0),
                      numericInput("n_prop1_manual", "3. Jumlah Total (n):", value=100, min=1),
                      numericInput("p_prop1", "4. Proporsi Hipotesis (p₀):", value=0.5, min=0, max=1, step=0.01),
                      radioButtons("alt_prop1", "Jenis Uji:", choices=c("Dua Arah"="two.sided", "Kurang Dari"="less", "Lebih Dari"="greater"), inline=TRUE),
                      actionButton("run_prop1", "Jalankan", icon=icon("play"))
                  ),
                  box(width=8, status="info", solidHeader=TRUE, title="Hasil Analisis", verbatimTextOutput("res_prop1") %>% withSpinner(), uiOutput("int_prop1") %>% withSpinner())
                )),
                tabPanel("Uji Proporsi 2 Kelompok", fluidRow(
                  box(width=4, status="primary", solidHeader=TRUE, title="Pengaturan",
                      h5("Definisi 'Sukses'"),
                      selectInput("var_prop2_cond", "1. Variabel Kondisi:", choices=NULL),
                      textInput("cond_prop2_text", "2. Kondisi 'Sukses':", value="> 10"), hr(), h5("Definisi Grup"),
                      selectInput("var_prop2_group", "3. Variabel Grup:", choices=NULL),
                      helpText("Grup dibagi otomatis berdasarkan median."), hr(),
                      radioButtons("alt_prop2", "Jenis Uji:", choices=c("Dua Arah"="two.sided", "Kurang Dari"="less", "Lebih Dari"="greater"), inline=TRUE),
                      actionButton("run_prop2", "Jalankan", icon=icon("play"))
                  ),
                  box(width=8, status="info", solidHeader=TRUE, title="Hasil Analisis", verbatimTextOutput("res_prop2") %>% withSpinner(), uiOutput("int_prop2") %>% withSpinner())
                )),
                tabPanel("Uji Varians 2 Kelompok (F-test)", fluidRow(
                  box(width=4, status="primary", solidHeader=TRUE, title="Pengaturan",
                      selectInput("var1_ftest", "Variabel Kelompok 1:", choices=NULL),
                      selectInput("var2_ftest", "Variabel Kelompok 2:", choices=NULL),
                      radioButtons("alt_ftest", "Jenis Uji:", choices=c("Dua Arah"="two.sided", "Kurang Dari"="less", "Lebih Dari"="greater"), inline=TRUE),
                      actionButton("run_ftest", "Jalankan", icon=icon("play"))
                  ),
                  box(width=8, status="info", solidHeader=TRUE, title="Hasil Analisis", verbatimTextOutput("res_ftest") %>% withSpinner(), uiOutput("int_ftest") %>% withSpinner())
                ))
              ),
              fluidRow(box(width = 12, status = "danger", solidHeader = TRUE, title = "Unduh Laporan", downloadButton("downloadPropVarPDF", "Unduh PDF")))
      ),      
      
      tabItem(tabName = "anova",
              h2("Analisis Varians (ANOVA)"),
              tabBox(
                id = "anova_tabs", width = 12,
                tabPanel("ANOVA 1 Arah", fluidRow(
                  box(width=4, status="primary", solidHeader=TRUE, title="Pengaturan",
                      selectInput("var_anova1_dv", "1. Variabel Dependen:", choices=NULL),
                      selectInput("var_anova1_iv", "2. Variabel Independen:", choices=NULL),
                      helpText("Variabel independen akan dibagi menjadi 3 grup."),
                      actionButton("run_anova1", "Jalankan", icon=icon("play"))
                  ),
                  box(width=8, status="warning", solidHeader=TRUE, title="Pemeriksaan Asumsi", uiOutput("asumsi_anova1_status") %>% withSpinner())
                ), fluidRow(
                  box(width=12, status="info", solidHeader=TRUE, title="Hasil Analisis", verbatimTextOutput("res_anova1") %>% withSpinner(),
                      h4("Uji Lanjutan (Tukey HSD)"), verbatimTextOutput("res_posthoc_anova1") %>% withSpinner(), h4("Interpretasi"), uiOutput("int_anova1") %>% withSpinner()
                  )
                )),
                tabPanel("ANOVA 2 Arah", fluidRow(
                  box(width=4, status="primary", solidHeader=TRUE, title="Pengaturan",
                      selectInput("var_anova2_dv", "1. Variabel Dependen:", choices=NULL),
                      selectInput("var_anova2_iv1", "2. Variabel Independen #1:", choices=NULL),
                      selectInput("var_anova2_iv2", "3. Variabel Independen #2:", choices=NULL),
                      helpText("Variabel independen akan dibagi menjadi grup Rendah/Tinggi."),
                      actionButton("run_anova2", "Jalankan", icon=icon("play"))
                  ),
                  box(width=8, status="warning", solidHeader=TRUE, title="Pemeriksaan Asumsi", uiOutput("asumsi_anova2_status") %>% withSpinner())
                ), fluidRow(
                  box(width=12, status="info", solidHeader=TRUE, title="Hasil Analisis", verbatimTextOutput("res_anova2") %>% withSpinner(),
                      h4("Uji Lanjutan (Tukey HSD)"), verbatimTextOutput("res_posthoc_anova2") %>% withSpinner(), h4("Interpretasi"), uiOutput("int_anova2") %>% withSpinner()
                  )
                ))
              ),
              fluidRow(box(width=12, status="danger", solidHeader=TRUE, title="Unduh Laporan", downloadButton("downloadAnovaPDF", "Unduh PDF")))
      ),
      
      tabItem(tabName = "regresi",
              h2("Analisis Regresi Linier"),
              fluidRow(
                box(width = 4, status = "primary", solidHeader = TRUE, title = "Pengaturan Analisis",
                    selectInput("y_reg", "1. Variabel Dependen (Y):", choices = NULL),
                    selectInput("x_reg", "2. Variabel Independen (X):", choices = NULL, multiple = TRUE),
                    hr(), h4("Transformasi Variabel (Opsional)"),
                    selectInput("var_transform", "Pilih variabel untuk ditransformasi:", choices = NULL, multiple = TRUE),
                    selectInput("transform_method", "Jenis transformasi:", choices = c("Tidak Ada" = "None", "Logaritma" = "Log", "Akar Kuadrat" = "Sqrt")),
                    actionButton("run_analysis", "Jalankan", icon = icon("play"), class = "btn-success")
                ),
                box(width = 8, tabsetPanel(
                  tabPanel("Ringkasan Model", verbatimTextOutput("reg_summary") %>% withSpinner()),
                  tabPanel("Uji Asumsi", 
                           h4("Multikolinearitas (VIF)"), verbatimTextOutput("vif_result"), hr(),
                           h4("Normalitas Residual"), plotOutput("qq_plot", height = "300px"), verbatimTextOutput("shapiro_result"), hr(),
                           h4("Homoskedastisitas"), plotOutput("resid_plot", height = "300px"), verbatimTextOutput("bp_result"), hr(),
                           h4("Autokorelasi Spasial (Moran's I)"),verbatimTextOutput("moran_result")
                  ),
                  tabPanel("Interpretasi Otomatis", uiOutput("interpretasi_regresi") %>% withSpinner())
                ))
              ),
              fluidRow(box(width = 12, status = "danger", solidHeader = TRUE, title = "Unduh Laporan", downloadButton("downloadRegresiPDF", "Unduh PDF")))
      ),
      
      tabItem(tabName = "unduh_data",
              h2("Unduh Data Pilihan"),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE, title = "Pengaturan Unduhan",
                    uiOutput("vars_unduh_ui"),
                    radioButtons(inputId = "format_unduh", label = "Format File:", choices = c("CSV" = "csv", "Excel (XLSX)" = "xlsx", "SPSS (SAV)" = "sav"), selected = "csv", inline = TRUE),
                    downloadButton("download_data_button", "Unduh Data")
                )
              ),
              fluidRow(box(width = 12, status = "info", solidHeader = TRUE, title = "Pratinjau Data", DTOutput("tabel_preview_unduh") %>% withSpinner()))
      )
    )
  )
)