#                              SERVER LOGIC                                   #

server <- function(input, output, session) {
  data_reaktif <- reactiveVal(sovi_data)
  data_kategori <- reactiveVal(NULL)
  
  # Membuat matriks pembobot spasial (hanya sekali saat aplikasi dimulai)
  neighbors <- poly2nb(indonesia_sf, queen = TRUE)
  weights_list <- nb2listw(neighbors, style = "W", zero.policy = TRUE)
  
  # Observer untuk sinkronisasi input
  observe({
    pilihan_semua <- input$vars_selected
    if (is.null(pilihan_semua)) return()
    
    pilihan_numerik <- pilihan_semua[sapply(data_reaktif()[, pilihan_semua, drop = FALSE], is.numeric)]
    
    # 1. Update Menu Eksplorasi Data
    updateSelectInput(session, "var_plot", choices = pilihan_semua, selected = pilihan_semua[1])
    
    updateSelectInput(session, "var_peta", choices = pilihan_numerik, selected = pilihan_numerik[1])
    
    # 2. Update Menu Uji Asumsi
    updateSelectInput(session, "var_asumsi", choices = pilihan_numerik, selected = pilihan_numerik[1])
    
    # Transformasi Data
    updateSelectInput(session, "var_to_transform", choices = pilihan_numerik, selected = pilihan_numerik[1])
    
    # 3. Update Menu Uji Beda Rata-rata
    updateSelectInput(session, "var_1samp", choices = pilihan_numerik, selected = pilihan_numerik[1])
    updateSelectInput(session, "var_2samp_num", choices = pilihan_numerik, selected = pilihan_numerik[1])
    updateSelectInput(session, "var_2samp_cat", choices = pilihan_numerik, selected = pilihan_numerik[2])
    
    # 4. Update Menu Uji Proporsi & Varians
    updateSelectInput(session, "var_prop1_konteks", choices = pilihan_numerik, selected = pilihan_numerik[1])
    updateSelectInput(session, "var_prop2_cond", choices = pilihan_numerik, selected = pilihan_numerik[1])
    updateSelectInput(session, "var_prop2_group", choices = pilihan_numerik, selected = pilihan_numerik[2])
    updateSelectInput(session, "var1_ftest", choices = pilihan_numerik, selected = pilihan_numerik[1])
    updateSelectInput(session, "var2_ftest", choices = pilihan_numerik, selected = pilihan_numerik[2])
    
    # 5. Update Menu ANOVA
    updateSelectInput(session, "var_anova1_dv", choices = pilihan_numerik, selected = pilihan_numerik[1])
    updateSelectInput(session, "var_anova1_iv", choices = pilihan_numerik, selected = pilihan_numerik[2])
    updateSelectInput(session, "var_anova2_dv", choices = pilihan_numerik, selected = pilihan_numerik[1])
    updateSelectInput(session, "var_anova2_iv1", choices = pilihan_numerik, selected = pilihan_numerik[2])
    updateSelectInput(session, "var_anova2_iv2", choices = pilihan_numerik, selected = pilihan_numerik[3])
    
    # 6. Update Menu Analisis Regresi
    updateSelectInput(session, "y_reg", choices = pilihan_numerik, selected = pilihan_numerik[1])
    updateSelectInput(session, "x_reg", choices = pilihan_numerik, selected = NULL)
    updateSelectInput(session, "var_transform", choices = pilihan_numerik, selected = NULL)
  })
  
  # Menu Beranda
  output$total_kabkota <- renderValueBox({ valueBox(nrow(data_reaktif()), "Total Kabupaten/Kota", icon = icon("map-marked-alt"), color = "primary") })
  output$avg_poverty <- renderValueBox({ valueBox(paste0(round(mean(data_reaktif()$POVERTY, na.rm = TRUE), 2), "%"), "Rata-rata Kemiskinan", icon = icon("users"), color = "danger") })
  output$avg_noelectric <- renderValueBox({ valueBox(paste0(round(mean(data_reaktif()$NOELECTRIC, na.rm = TRUE), 2), "%"), "RT Tanpa Listrik", icon = icon("bolt"), color = "warning") })
  output$total_populasi <- renderValueBox({ valueBox(format(sum(as.numeric(data_reaktif()$POPULATION), na.rm = TRUE), big.mark = ",", scientific = FALSE), "Total Populasi", icon = icon("user-friends"), color = "success") })
  
  
  # Menu Manajemen Data
  output$kat_labels_ui <- renderUI({
    req(input$n_kat > 0)
    map(1:input$n_kat, ~ textInput(inputId = paste0("kat_label_", .x), 
                                   label = paste("Nama Kategori", .x), 
                                   value = paste("Kategori", .x)))
  })
  
  observeEvent(input$proses_kat, {
    req(input$var_kontinyu, input$n_kat > 1, input$metode_kat)
    
    kat_labels <- map_chr(1:input$n_kat, ~ input[[paste0("kat_label_", .x)]])
    
    if (any(kat_labels == "" | is.na(kat_labels))) {
      showNotification("Harap isi semua nama kategori.", type = "error")
      return()
    }
    
    var_data <- data_reaktif()[[input$var_kontinyu]]
    breaks <- NULL
    
    if (input$metode_kat == "quantile") {
      breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$n_kat + 1), na.rm = TRUE)
    } else if (input$metode_kat == "equal") {
      breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), length.out = input$n_kat + 1)
    }
    
    kategori <- cut(var_data, 
                    breaks = breaks,
                    labels = kat_labels, 
                    include.lowest = TRUE)
    
    hasil_df <- data.frame(
      Provinsi = data_reaktif()$PROVINCE_NAME,
      Nama_Kota = data_reaktif()$CITY_NAME,
      ID = data_reaktif()$DISTRICTCODE,
      Nilai_Asli = var_data,
      Kategori_Hasil = kategori
    )
    
    data_kategori(hasil_df)
    
    output$tabel_kat <- renderDT({
      datatable(hasil_df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    
    output$interpretasi_kat <- renderUI({
      metode_terpilih <- ifelse(input$metode_kat == "quantile", "Kuantil", "Interval Sama")
      
      rentang_teks <- map_chr(1:length(kat_labels), function(i) {
        paste0(tags$b(kat_labels[i]), ": ", round(breaks[i], 2), " - ", round(breaks[i+1], 2))
      })
      
      tagList(
        h4("📌 Interpretasi Hasil Kategorisasi:"),
        p("Variabel", tags$b(input$var_kontinyu), "telah dikategorikan menjadi", 
          tags$b(input$n_kat), "kelompok menggunakan metode", tags$b(metode_terpilih), "."),
        hr(),
        h4("Rentang Nilai per Kategori:"),
        HTML(paste(rentang_teks, collapse = "<br>"))
      )
    })
  })
  
  output$download_buttons_ui <- renderUI({
    req(data_kategori())
    tagList(
      h4("Unduh Hasil Kategorisasi"),
      downloadButton("download_csv", "Unduh .csv", class = "btn-primary"),
      downloadButton("download_xlsx", "Unduh .xlsx", class = "btn-success"),
      downloadButton("download_sav", "Unduh .sav", class = "btn-warning")
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("hasil_kategorisasi_", input$var_kontinyu, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data_kategori(), file, row.names = FALSE)
    })
  
  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste0("hasil_kategorisasi_", input$var_kontinyu, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(data_kategori(), path = file)
    })
  
  output$download_sav <- downloadHandler(
    filename = function() {
      paste0("hasil_kategorisasi_", input$var_kontinyu, "_", Sys.Date(), ".sav")
    },
    content = function(file) {
      haven::write_sav(data_kategori(), path = file)
    })
  
  data_filtered <- reactive({
    req(input$vars_selected)
    data_reaktif() %>% dplyr::select(all_of(input$vars_selected))
  })
  
  # Menu Eksplorasi Data
  output$vars_selected_ui <- renderUI({
    selectInput("vars_selected", "Pilih variabel yang akan digunakan di semua menu analisis:", 
                choices = names(data_reaktif()), 
                multiple = TRUE, 
                selectize = TRUE,
                selected = isolate(input$vars_selected) %||% names(sovi_data)[sapply(sovi_data, is.numeric)])
  })
  
  output$vars_unduh_ui <- renderUI({
    selectInput("vars_unduh", "Pilih variabel yang ingin diunduh:",
                choices = names(data_reaktif()),
                selected = names(data_reaktif()),
                multiple = TRUE, selectize = TRUE)
  })
  
  output$tabel_stat <- DT::renderDataTable({
    df <- data_filtered()
    req(ncol(df) > 0)
    
    numeric_df <- df %>% select_if(is.numeric)
    
    if (ncol(numeric_df) == 0) return(NULL)
    
    hasil <- data.frame(
      Variabel = names(numeric_df)
    )
    
    desc_stats <- as.data.frame(t(sapply(numeric_df, function(x) {
      c(
        N_Valid = sum(!is.na(x)),
        Percent_Valid = round(sum(!is.na(x))/length(x)*100, 1),
        N_Missing = sum(is.na(x)),
        Percent_Missing = round(sum(is.na(x))/length(x)*100, 1),
        Mean = round(mean(x, na.rm = TRUE), 2),
        Median = round(median(x, na.rm = TRUE), 2),
        Variance = round(var(x, na.rm = TRUE), 2),
        SD = round(sd(x, na.rm = TRUE), 2),
        Min = round(min(x, na.rm = TRUE), 2),
        Max = round(max(x, na.rm = TRUE), 2),
        Range = round(diff(range(x, na.rm = TRUE)), 2),
        Skewness = round(DescTools::Skew(x, na.rm = TRUE), 2),
        Kurtosis = round(DescTools::Kurt(x, na.rm = TRUE), 2)
      )
    })))
    
    hasil_final <- cbind(hasil, desc_stats)
    
    DT::datatable(
      hasil_final,
      options = list(scrollX = TRUE, pageLength = 5),
      rownames = FALSE
    )
  })
  
  output$boxplot <- renderPlot({
    req(input$var_plot, is.numeric(data_reaktif()[[input$var_plot]]))
    ggplot(data_reaktif(), aes_string(y = input$var_plot)) +
      geom_boxplot(fill = "skyblue", color = "navy") +
      theme_minimal(base_size = 14) +
      labs(title = paste("Boxplot untuk", input$var_plot), y = "Nilai", x = "")
  })
  
  output$qqplot <- renderPlot({
    req(input$var_plot, is.numeric(data_reaktif()[[input$var_plot]]))
    x <- data_reaktif()[[input$var_plot]]
    req(length(na.omit(x)) > 0)
    
    ggplot(data.frame(val = x), aes(sample = val)) +
      stat_qq(color = "skyblue4") +
      stat_qq_line(color = "red", linetype = "dashed") +
      theme_minimal(base_size = 14) +
      labs(title = paste("Normal Q-Q Plot untuk", input$var_plot), x = "Theoretical Quantiles", y = "Sample Quantiles")
  })
  
  output$histplot <- renderPlot({
    req(input$var_plot, is.numeric(data_reaktif()[[input$var_plot]]))
    ggplot(data_reaktif(), aes_string(x = input$var_plot)) +
      geom_histogram(fill = "#69b3a2", color = "white", bins = 30) +
      theme_minimal(base_size = 14) +
      labs(title = paste("Histogram untuk", input$var_plot), x = "Nilai", y = "Frekuensi")
  })
  
  output$corrplot <- renderPlot({
    df <- data_filtered()
    
    df_num <- df %>% select_if(is.numeric)
    
    req(ncol(df_num) > 1)
    GGally::ggcorr(df_num, label = TRUE, label_round = 2, label_size = 3.5, 
                   hjust = 0.75, layout.exp = 1,
                   low = "steelblue", mid = "white", high = "darkred") +
      labs(title = "Matriks Korelasi Pearson")
  })
  
  output$interpretasi <- renderPrint({
    req(input$vars_selected)
    df <- data_filtered()
    cat("===== INTERPRETASI OTOMATIS =====\n")
    cat("Dashboard ini menyajikan statistik deskriptif untuk ", ncol(df), " variabel yang dipilih.\n\n")
    
    df_num <- df %>% select_if(is.numeric)
    
    if(ncol(df_num) > 0) {
      cat(" Interpretasi Distribusi:\n")
      for (v in names(df_num)) {
        x <- df_num[[v]]
        skew_x <- DescTools::Skew(x, na.rm = TRUE)
        kurt_x <- DescTools::Kurt(x, na.rm = TRUE)
        cat(paste0("- Variabel ", v, ":\n"))
        if (abs(skew_x) < 0.5) cat("   Distribusi cenderung simetris")
        else if (skew_x > 0) cat("   Distribusi menceng ke kanan (positive skew)")
        else cat("   Distribusi menceng ke kiri (negative skew)")
        
        if (kurt_x > 0) cat(", dengan puncak lebih tajam dari normal (leptokurtic).\n")
        else if (kurt_x < 0) cat(", dengan puncak lebih datar dari normal (platykurtic).\n")
        else cat(", dengan puncak mendekati distribusi normal (mesokurtic).\n")
      }
      
      if (ncol(df_num) > 1) {
        cat("\n Interpretasi Korelasi:\n")
        cor_matrix <- cor(df_num, use = "pairwise.complete.obs")
        for (i in 1:(ncol(cor_matrix)-1)) {
          for (j in (i+1):ncol(cor_matrix)) {
            var1 <- colnames(cor_matrix)[i]
            var2 <- colnames(cor_matrix)[j]
            r <- cor_matrix[i, j]
            
            if(!is.na(r)){
              strength <- ifelse(abs(r) >= 0.7, "sangat kuat",
                                 ifelse(abs(r) >= 0.5, "kuat",
                                        ifelse(abs(r) >= 0.3, "cukup", 
                                               ifelse(abs(r) >= 0.1, "lemah", "sangat lemah"))))
              direction <- ifelse(r > 0, "positif", ifelse(r < 0, "negatif", "tidak ada"))
              
              cat(paste0("- ", var1, " & ", var2, ": Hubungan ", direction, " dengan kekuatan ", strength, " (r = ", round(r, 2), ").\n"))
            }
          }
        }
      }
    } else {
      cat("Tidak ada variabel numerik yang dipilih untuk dianalisis.")
    }
  })
  
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste0("Hasil-Eksplorasi-Data-Sovi-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "Laporan-Eksplorasi.Rmd")
      file.copy("www/Laporan-Eksplorasi.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(data = data_filtered(), vars = input$vars_selected)
      
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # Menu Peta
  peta_data <- reactive({
    # Pilih hanya kolom identitas & geometri dari data spasial untuk menghindari duplikasi kolom
    indonesia_sf_geom <- dplyr::select(indonesia_sf, kodeprkab, nmkab, nmprov)
    
    # Gabungkan dengan data reaktif. Semua kolom data (POVERTY, RENTED, dll.) akan
    # berasal dari data_reaktif() tanpa ada akhiran .x atau .y
    left_join(indonesia_sf_geom, data_reaktif(), by = c("kodeprkab" = "DISTRICTCODE"))
  })
  
  # 2. Render Peta Leaflet
  output$peta_leaflet <- renderLeaflet({
    req(input$var_peta)
    map_data <- peta_data()
    
    # Pastikan variabel yang dipilih ada di data
    if (!input$var_peta %in% names(map_data)) return(NULL)
    
    # Filter baris dengan nilai NA pada variabel terpilih agar palet warna dan popup tidak error
    map_data_filtered <- map_data[!is.na(map_data[[input$var_peta]]), ]
    
    # Jika tidak ada data sama sekali, tampilkan peta kosong
    if (nrow(map_data_filtered) == 0) {
      return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = 118, lat = -2, zoom = 5))
    }
    
    # Membuat palet warna numerik dari kuning ke merah
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = map_data_filtered[[input$var_peta]]
    )
    
    # Membuat label untuk ditampilkan saat kursor diarahkan ke wilayah
    labels <- sprintf(
      "<strong>%s, %s</strong><br/>%s: %s",
      map_data_filtered$nmkab,
      map_data_filtered$nmprov,
      input$var_peta,
      format(round(map_data_filtered[[input$var_peta]], 2), nsmall = 2, big.mark = ",")
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data = map_data_filtered) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Peta Dasar") %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addPolygons(
        fillColor = ~pal(get(input$var_peta)),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~get(input$var_peta),
        opacity = 0.7,
        title = input$var_peta,
        position = "bottomright"
      )
  })
  
  # 3. Render Interpretasi Peta
  output$interpretasi_peta <- renderUI({
    req(input$var_peta)
    
    # Hapus geometri untuk manipulasi data frame biasa (lebih cepat)
    df <- as.data.frame(peta_data()) %>%
      dplyr::select(nmkab, nmprov, variable = !!sym(input$var_peta)) %>%
      dplyr::filter(!is.na(variable))
    
    if(nrow(df) < 5) return(p("Data tidak cukup untuk menampilkan interpretasi."))
    
    # Ambil 3 wilayah dengan nilai tertinggi dan terendah
    top3 <- df %>% arrange(desc(variable)) %>% head(3)
    bottom3 <- df %>% arrange(variable) %>% head(3)
    
    tagList(
      h4("Analisis Spasial Sederhana"),
      p("Peta ini menunjukkan sebaran spasial untuk variabel", tags$b(input$var_peta), 
        "di seluruh kabupaten/kota di Indonesia. Warna yang lebih gelap (merah) menunjukkan nilai yang lebih tinggi."),
      hr(),
      h5(tags$b(icon("arrow-up", class="text-danger"), " 3 Wilayah dengan Nilai Tertinggi:")),
      tags$ul(
        lapply(1:nrow(top3), function(i) {
          tags$li(paste0(top3$nmkab[i], " (", top3$nmprov[i], "): ", round(top3$variable[i], 2)))
        })
      ),
      hr(),
      h5(tags$b(icon("arrow-down", class="text-success"), " 3 Wilayah dengan Nilai Terendah:")),
      tags$ul(
        lapply(1:nrow(bottom3), function(i) {
          tags$li(paste0(bottom3$nmkab[i], " (", bottom3$nmprov[i], "): ", round(bottom3$variable[i], 2)))
        })
      ),
      hr(),
      p(tags$i("Arahkan kursor pada sebuah wilayah di peta untuk melihat nilai spesifiknya."))
    )
  })
  
  
  # Menu Uji Asumsi Data
  observeEvent(input$apply_transform_button, {
    req(input$var_to_transform, input$new_var_name)
    
    if (input$new_var_name %in% names(data_reaktif())) {
      showNotification("Nama variabel baru sudah ada. Harap gunakan nama lain.", type = "error")
      return()
    }
    if (!grepl("^[a-zA-Z0-9_.]+$", input$new_var_name)) {
      showNotification("Nama variabel baru hanya boleh berisi huruf, angka, titik, dan underscore.", type = "error")
      return()
    }
    
    current_data <- data_reaktif()
    var_to_transform <- current_data[[input$var_to_transform]]
    
    new_var <- tryCatch({
      switch(input$transform_method_asumsi,
             "log" = log1p(var_to_transform),
             "sqrt" = sqrt(var_to_transform),
             "arcsin_sqrt" = asin(sqrt(var_to_transform / 100))
      )
    }, error = function(e) {
      showNotification(paste("Transformasi gagal:", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(new_var)) {
      current_data[[input$new_var_name]] <- new_var
      data_reaktif(current_data)
      
      updateSelectInput(session, "vars_selected", 
                        selected = c(input$vars_selected, input$new_var_name))
      
      showNotification(paste("Variabel baru '", input$new_var_name, "' berhasil dibuat!"), type = "message", duration = 5)
    }
  })
  
  asumsi_results <- reactive({
    req(input$var_asumsi)
    dat <- data_reaktif()[[input$var_asumsi]]
    
    if (!is.numeric(dat) || length(na.omit(dat)) < 3) {
      return(list(error = "Data tidak valid. Pilih variabel numerik dengan minimal 3 observasi."))
    }
    
    shapiro_res <- if(length(na.omit(dat)) <= 5000) shapiro.test(dat) else NULL
    
    group <- tryCatch(
      as.factor(ifelse(dat <= median(dat, na.rm = TRUE), "Grup Bawah", "Grup Atas")),
      error = function(e) NULL
    )
    
    levene_res <- if(!is.null(group) && length(levels(na.omit(group))) > 1) car::leveneTest(dat ~ group) else NULL
    
    x_clean <- na.omit(dat)
    q1 <- quantile(x_clean, 0.25)
    q3 <- quantile(x_clean, 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    outliers <- x_clean[x_clean < lower_bound | x_clean > upper_bound]
    
    list(
      var_name = input$var_asumsi, shapiro = shapiro_res,
      levene = levene_res, outliers = outliers, data = dat, error = NULL
    )
  })
  
  output$shapiro_test <- renderPrint({
    req(input$var_asumsi)
    dat <- data_reaktif()[[input$var_asumsi]]
    
    if (!is.numeric(dat) || length(na.omit(dat)) < 3 || length(na.omit(dat)) > 5000) {
      cat("Analisis Dibatalkan: Variabel harus numerik dengan 3 hingga 5000 data valid untuk Uji Shapiro-Wilk.")
      return()
    }
    
    test_result <- shapiro.test(dat)
    interpretation <- if (test_result$p.value < 0.05) {
      "\n\nInterpretasi: Karena p-value < 0.05, data TIDAK berdistribusi normal."
    } else {
      "\n\nInterpretasi: Karena p-value >= 0.05, data cenderung berdistribusi normal."
    }
    print(test_result)
    cat(interpretation)
  })
  
  output$homogeneity_test <- renderPrint({
    req(input$var_asumsi)
    dat <- data_reaktif()[[input$var_asumsi]]
    
    if (!is.numeric(dat) || length(na.omit(dat)) < 2) {
      cat("Analisis Dibatalkan: Variabel harus numerik dan memiliki > 2 data valid.")
      return()
    }
    
    group <- as.factor(ifelse(dat <= median(dat, na.rm = TRUE), "Grup Bawah", "Grup Atas"))
    
    if (length(levels(na.omit(group))) < 2) {
      cat("Analisis Dibatalkan: Tidak dapat membentuk dua grup berbeda dari data untuk diuji.")
      return()
    }
    test_result <- car::leveneTest(dat ~ group)
    p_value <- test_result$`Pr(>F)`[1]
    interpretation <- if (p_value < 0.05) {
      "\n\nInterpretasi: Karena p-value < 0.05, varians antar grup TIDAK homogen."
    } else {
      "\n\nInterpretasi: Karena p-value >= 0.05, varians antar grup cenderung homogen."
    }
    print(test_result)
    cat(interpretation)
  })
  
  output$outlier_plot <- renderPlot({
    req(input$var_asumsi)
    boxplot(data_reaktif()[[input$var_asumsi]], main = paste("Boxplot dari", input$var_asumsi), col = "tomato", border = "darkred", pch = 16)
  })
  
  output$outlier_summary <- renderPrint({
    req(input$var_asumsi)
    x <- na.omit(data_reaktif()[[input$var_asumsi]])
    
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
    if(is.null(res$shapiro)) {
      cat("1. Normalitas: Tidak dapat dinilai (data > 5000). Disarankan inspeksi visual via QQ-Plot.\n")
    } else {
      cat("1. Normalitas: Data", ifelse(is_normal, "MEMENUHI", "TIDAK memenuhi"), "asumsi normalitas (p-value =", round(res$shapiro$p.value, 4), ").\n")
    }
    if(is.null(res$levene)) {
      cat("2. Homogenitas Varians: Tidak dapat dinilai (grup tidak terbentuk).\n")
    } else {
      cat("2. Homogenitas Varians: Data", ifelse(is_homogen, "MEMENUHI", "TIDAK memenuhi"), "asumsi homogenitas (p-value =", round(res$levene$`Pr(>F)`[1], 4), ").\n")
    }
    cat("3. Outlier: Teridentifikasi", ifelse(has_outliers, "ADANYA", "TIDAK ADANYA"), "outlier signifikan.\n\n")
    cat("----- REKOMENDASI -----\n")
    if (is_normal && is_homogen && !has_outliers) {
      cat("Kesimpulan: Semua asumsi utama terpenuhi. Pengujian statistik PARAMETRIK (misal: Uji-t, ANOVA) sangat direkomendasikan.\n")
    } else {
      cat("Kesimpulan: Terdapat pelanggaran asumsi. Disarankan untuk:\n")
      cat("  a) Melakukan transformasi data (misal: log, sqrt) untuk mencoba memenuhi asumsi, atau\n")
      cat("  b) Menggunakan metode statistik NON-PARAMETRIK (misal: Uji Mann-Whitney, Kruskal-Wallis) yang lebih robust.\n")
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
  
  # Submenu Uji Beda Rata-rata
  rv_rata <- reactiveValues(last_test_inputs = NULL, all_results = NULL, test_type = NULL)
  
  observe({
    grouping_var <- input$var_2samp_cat
    
    if (!is.null(grouping_var) && grouping_var %in% names(data_reaktif())) {
      median_value <- median(data_reaktif()[[grouping_var]], na.rm = TRUE)
      updateNumericInput(session, "split_val_2samp", value = round(median_value, 2))
    }
  })
  
  observeEvent(input$run_1samp, {
    req(input$var_1samp, !is.na(input$mu_1samp))
    var_data <- na.omit(data_reaktif()[[input$var_1samp]])
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
      paste0("<b>Kesimpulan (H0 Ditolak):</b> Karena p-value (", round(p_val, 4), ") < 0.05, terdapat bukti statistik yang signifikan untuk menyatakan bahwa rata-rata variabel '", input$var_1samp, "' ", conclusion_text, ".")
    } else {
      paste0("<b>Kesimpulan (H0 Gagal Ditolak):</b> Karena p-value (", round(p_val, 4), ") >= 0.05, tidak ada bukti statistik yang cukup untuk menyatakan bahwa rata-rata variabel '", input$var_1samp, "' ", conclusion_text, ".")
    }
    output$int_1samp <- renderUI({ p(HTML(interpretation_text)) })
  })
  
  observeEvent(input$run_2samp, {
    req(input$var_2samp_num, input$var_2samp_cat, !is.na(input$split_val_2samp))
    
    df <- na.omit(data_reaktif()[, c(input$var_2samp_num, input$var_2samp_cat)])
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
      paste0("<b>Kesimpulan (H0 Ditolak):</b> Karena p-value (", round(p_val, 4), ") < 0.05, terdapat bukti statistik yang signifikan bahwa rata-rata variabel '", input$var_2samp_num, "' ", conclusion_text, ".")
    } else {
      paste0("<b>Kesimpulan (H0 Gagal Ditolak):</b> Karena p-value (", round(p_val, 4), ") >= 0.05, tidak ada bukti statistik yang cukup untuk menyatakan bahwa rata-rata variabel '", input$var_2samp_num, "' ", conclusion_text, ".")
    }
    output$int_2samp <- renderUI({ p(HTML(interpretation_text)) })
  })
  
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
        shiny::showNotification("Gagal: Jalankan salah satu analisis terlebih dahulu!", type = "error")
        return(NULL)
      } else {
        rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
      }
    })
  
  # Submenu Uji Proporsi dan Varians
  rv_propvar <- reactiveValues(test_type = NULL, inputs = NULL, all_results = NULL)
  
  observeEvent(input$run_prop1, {
    req(input$x_prop1_manual, input$n_prop1_manual, !is.na(input$p_prop1))
    x <- input$x_prop1_manual; n <- input$n_prop1_manual
    if(x > n) { shiny::showNotification("Error: Jumlah sukses (x) tidak boleh > jumlah total (n).", type="error"); return() }
    
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
  
  observeEvent(input$run_prop2, {
    req(input$var_prop2_cond, input$cond_prop2_text, input$var_prop2_group)
    
    tryCatch({
      df <- na.omit(data_reaktif()[, c(input$var_prop2_cond, input$var_prop2_group)])
      median_value <- median(df[[input$var_prop2_group]], na.rm = TRUE)
      grup1_df <- df[df[[input$var_prop2_group]] <= median_value, ]
      grup2_df <- df[df[[input$var_prop2_group]] > median_value, ]
      
      if(nrow(grup1_df) < 2 || nrow(grup2_df) < 2) {
        shiny::showNotification("Error: Tidak dapat membentuk dua grup valid dari median.", type="error"); return()
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
        interpretation_text <- "<b>Interpretasi Gagal:</b> Hasil p-value tidak dapat dihitung."
      }
      
      output$int_prop2 <- renderUI({ p(HTML(interpretation_text)) })
      
    }, error = function(e) { shiny::showNotification(paste("Error:", e$message), type = "error") })
  })
  
  observeEvent(input$run_ftest, {
    req(input$var1_ftest, input$var2_ftest)
    var1_data <- na.omit(data_reaktif()[[input$var1_ftest]])
    var2_data <- na.omit(data_reaktif()[[input$var2_ftest]])
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
  
  # Submenu ANOVA
  rv_anova <- reactiveValues(test_type = NULL, inputs = NULL, asumsi_status = NULL, anova_result = NULL, posthoc_result = NULL, interpretation = NULL)
  
  # ANOVA 1 Arah
  observeEvent(input$run_anova1, {
    req(input$var_anova1_dv, input$var_anova1_iv)
    df <- na.omit(data_reaktif()[, c(input$var_anova1_dv, input$var_anova1_iv)])
    dv_data <- df[[input$var_anova1_dv]]
    iv_data <- df[[input$var_anova1_iv]]
    
    quantiles <- quantile(iv_data, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
    iv_factor <- cut(iv_data, breaks = unique(quantiles), labels = c("Rendah", "Sedang", "Tinggi"), include.lowest = TRUE)
    
    p_shapiro <- if (length(na.omit(dv_data)) > 5000) 0.06 else shapiro.test(dv_data)$p.value
    levene_test <- car::leveneTest(dv_data ~ iv_factor)
    p_levene <- levene_test$`Pr(>F)`[1]
    asumsi_lolos <- p_shapiro >= 0.05 && p_levene >= 0.05
    
    output$asumsi_anova1_status <- renderUI({
      if(asumsi_lolos) {
        tags$div(class = "alert alert-success", h4("ASUMSI TERPENUHI"))
      } else {
        tags$div(class = "alert alert-warning", h4("PERINGATAN: ASUMSI TIDAK TERPENUHI"))
      }
    })
    
    anova_model <- aov(dv_data ~ iv_factor)
    anova_summary <- summary(anova_model)
    
    f_value <- anova_summary[[1]]$`F value`[1]
    df1 <- anova_summary[[1]]$`Df`[1]
    df2 <- anova_summary[[1]]$`Df`[2]
    p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    posthoc_res <- if (!is.na(p_value) && p_value < 0.05) TukeyHSD(anova_model) else NULL
    
    output$res_anova1 <- renderPrint({ print(anova_summary) })
    output$res_posthoc_anova1 <- renderPrint({ if(!is.null(posthoc_res)) print(posthoc_res) else cat("Tidak dilakukan (hasil ANOVA tidak signifikan).") })
    
    interpretation_text <- if (p_value < 0.05) {
      paste0("Hasil analisis menunjukkan bahwa terdapat perbedaan yang signifikan secara statistik pada rata-rata variabel '", 
             input$var_anova1_dv, "' di antara kelompok variabel '", input$var_anova1_iv, 
             "' (F(", df1, ", ", df2, ") = ", round(f_value, 2), ", p < 0.05). ",
             "Hasil uji lanjut Tukey HSD dapat digunakan untuk melihat kelompok mana saja yang berbeda secara spesifik.")
    } else {
      paste0("Hasil analisis menunjukkan bahwa tidak terdapat cukup bukti statistik untuk menyatakan adanya perbedaan rata-rata variabel '", 
             input$var_anova1_dv, "' di antara kelompok variabel '", input$var_anova1_iv, 
             "' (F(", df1, ", ", df2, ") = ", round(f_value, 2), ", p = ", round(p_value, 3), ").")
    }
    
    output$int_anova1 <- renderUI({ 
      if (!asumsi_lolos) {
        tagList(
          p(HTML(interpretation_text)),
          tags$b("Catatan: Karena asumsi tidak terpenuhi, hasil ini mungkin tidak valid. Pertimbangkan menggunakan uji non-parametrik Kruskal-Wallis.")
        )
      } else {
        p(HTML(interpretation_text))
      }
    })
    
    rv_anova$test_type <- "ANOVA 1 Arah"
    rv_anova$inputs <- list(dv = input$var_anova1_dv, iv = input$var_anova1_iv)
    rv_anova$asumsi_status <- ifelse(asumsi_lolos, "Terpenuhi", "Tidak Terpenuhi")
    rv_anova$anova_result <- anova_summary
    rv_anova$posthoc_result <- posthoc_res
    rv_anova$interpretation <- interpretation_text
  })
  
  # ANOVA 2 Arah
  observeEvent(input$run_anova2, {
    req(input$var_anova2_dv, input$var_anova2_iv1, input$var_anova2_iv2)
    
    df <- na.omit(data_reaktif()[, c(input$var_anova2_dv, input$var_anova2_iv1, input$var_anova2_iv2)])
    names(df) <- c("dv", "iv1", "iv2")
    
    df$iv1_factor <- cut(df$iv1, breaks = 2, labels = c("Rendah", "Tinggi"))
    df$iv2_factor <- cut(df$iv2, breaks = 2, labels = c("Rendah", "Tinggi"))
    
    model_res <- aov(dv ~ iv1_factor * iv2_factor, data = df)
    
    p_shapiro <- if (length(resid(model_res)) > 5000) 0.06 else shapiro.test(resid(model_res))$p.value
    levene_test <- car::leveneTest(dv ~ iv1_factor * iv2_factor, data = df)
    p_levene <- levene_test$`Pr(>F)`[1]
    asumsi_lolos <- p_shapiro >= 0.05 && p_levene >= 0.05
    
    output$asumsi_anova2_status <- renderUI({
      if(asumsi_lolos) {
        tags$div(class = "alert alert-success", h4("ASUMSI TERPENUHI"))
      } else {
        tags$div(class = "alert alert-warning", h4("PERINGATAN: ASUMSI TIDAK TERPENUHI"))
      }
    })
    
    anova_summary <- summary(model_res)
    
    stats <- anova_summary[[1]]
    iv1_f <- stats$`F value`[1]; iv1_df1 <- stats$`Df`[1]; iv1_df2 <- stats$`Df`[3]; iv1_p <- stats$`Pr(>F)`[1]
    iv2_f <- stats$`F value`[2]; iv2_df1 <- stats$`Df`[2]; iv2_df2 <- stats$`Df`[3]; iv2_p <- stats$`Pr(>F)`[2]
    int_f <- stats$`F value`[3]; int_df1 <- stats$`Df`[3]; int_df2 <- stats$`Df`[3]; int_p <- stats$`Pr(>F)`[3]
    
    iv1_interp <- paste0("<li><b>Efek Utama ", input$var_anova2_iv1, ":</b> ", if(!is.na(iv1_p) && iv1_p < 0.05) "Signifikan." else "Tidak signifikan.", " F(", iv1_df1, ", ", iv1_df2, ") = ", round(iv1_f, 2), ", p = ", round(iv1_p, 3), ".</li>")
    iv2_interp <- paste0("<li><b>Efek Utama ", input$var_anova2_iv2, ":</b> ", if(!is.na(iv2_p) && iv2_p < 0.05) "Signifikan." else "Tidak signifikan.", " F(", iv2_df1, ", ", iv2_df2, ") = ", round(iv2_f, 2), ", p = ", round(iv2_p, 3), ".</li>")
    int_interp <- paste0("<li><b>Efek Interaksi:</b> ", if(!is.na(int_p) && int_p < 0.05) "Signifikan." else "Tidak signifikan.", " F(", int_df1, ", ", int_df2, ") = ", round(int_f, 2), ", p = ", round(int_p, 3), ".</li>")

    interpretation_text <- paste0("<ul>", iv1_interp, iv2_interp, int_interp, "</ul>")
    
    posthoc_res <- if (any(stats$`Pr(>F)` < 0.05, na.rm = TRUE)) TukeyHSD(model_res) else NULL
    
    output$res_anova2 <- renderPrint({ print(anova_summary) })
    output$res_posthoc_anova2 <- renderPrint({ if(!is.null(posthoc_res)) print(posthoc_res) else cat("Tidak dilakukan.") })
    
    output$int_anova2 <- renderUI({
      if(!asumsi_lolos){
        tagList(
          HTML(interpretation_text),
          tags$b("Catatan: Karena asumsi tidak terpenuhi, hasil ini mungkin tidak valid. Pertimbangkan transformasi variabel dependen.")
        )
      } else {
        HTML(interpretation_text)
      }
    })
    
    rv_anova$test_type <- "ANOVA 2 Arah"
    rv_anova$inputs <- list(dv=input$var_anova2_dv, iv1=input$var_anova2_iv1, iv2=input$var_anova2_iv2)
    rv_anova$asumsi_status <- ifelse(asumsi_lolos, "Terpenuhi", "Tidak Terpenuhi")
    rv_anova$anova_result <- anova_summary
    rv_anova$posthoc_result <- posthoc_res
    rv_anova$interpretation <- interpretation_text
  })
  
  output$downloadAnovaPDF <- downloadHandler(
    filename = function() { paste0("Laporan-ANOVA-", Sys.Date(), ".pdf") },
    content = function(file) {
      tempReport <- file.path(tempdir(), "Laporan-ANOVA.Rmd")
      file.copy("www/Laporan-ANOVA.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        test_type = rv_anova$test_type, inputs = rv_anova$inputs, asumsi_status = rv_anova$asumsi_status,
        anova_result = rv_anova$anova_result, posthoc_result = rv_anova$posthoc_result, interpretation = rv_anova$interpretation
      )
      if (is.null(params$test_type)) {
        shiny::showNotification("Gagal: Jalankan analisis dulu!", type="error"); return(NULL)
      }
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    })
  
  # Menu Analisis Regresi
  observe({
    req(input$y_reg, input$x_reg)
    updateSelectInput(session, "var_transform", choices = c(input$y_reg, input$x_reg))
  })
  
  apply_transform <- function(x, method) {
    if (method == "Log") return(log1p(x))
    if (method == "Sqrt") return(sqrt(x))
    return(x)
  }
  
  data_transformed <- reactive({
    df <- data_reaktif()
    if (!is.null(input$var_transform) && input$transform_method != "None") {
      for (var in input$var_transform) {
        if (var %in% names(df)) {
          df[[var]] <- apply_transform(df[[var]], input$transform_method)
        }
      }
    }
    return(df)
  })
  
  model_fit <- eventReactive(input$run_analysis, {
    req(input$y_reg, input$x_reg)
    data_use <- data_transformed()
    formula_text <- paste(input$y_reg, "~", paste(input$x_reg, collapse = " + "))
    lm(as.formula(formula_text), data = data_use, na.action = na.exclude)
  })
  
  output$reg_summary <- renderPrint({ summary(model_fit()) })
  
  output$vif_result <- renderPrint({
    if (length(input$x_reg) > 1) {
      vif_values <- car::vif(model_fit())
      print(vif_values)
      if (any(vif_values > 10)) {
        cat("\nInterpretasi: Ditemukan multikolinearitas signifikan (VIF > 10).")
      } else if (any(vif_values > 5)) {
        cat("\nInterpretasi: Terdapat potensi multikolinearitas (VIF > 5), perlu diwaspadai.")
      } else {
        cat("\nInterpretasi: Tidak ditemukan multikolinearitas signifikan (VIF < 5).")
      }
    } else {
      cat("VIF tidak dihitung untuk regresi dengan satu variabel independen.")
    }
  })
  
  output$qq_plot <- renderPlot({ plot(model_fit(), which = 2) })
  
  output$shapiro_result <- renderPrint({
    resids <- resid(model_fit())
    if (length(na.omit(resids)) > 5000) {
      cat("Uji Shapiro-Wilk tidak dijalankan (data > 5000). Gunakan QQ-Plot untuk inspeksi visual.")
    } else {
      print(shapiro.test(resids))
    }
  })
  
  output$resid_plot <- renderPlot({ plot(model_fit(), which = 1) })
  
  output$bp_result <- renderPrint({ lmtest::bptest(model_fit()) })
  
  output$moran_result <- renderPrint({
    req(model_fit())
    moran.test(resid(model_fit()), listw = weights_list, zero.policy = TRUE)
  })
  
  output$interpretasi_regresi <- renderUI({
    model <- model_fit(); model_summary <- summary(model)
    
    resids <- resid(model); p_shapiro <- if(length(na.omit(resids)) <= 5000) shapiro.test(resids)$p.value else 0.06
    p_bp <- lmtest::bptest(model)$p.value
    vif_vals <- if(length(coef(model)) > 2) car::vif(model) else c(0)
    
    moran_test <- moran.test(resids, listw = weights_list, zero.policy = TRUE)
    p_moran <- moran_test$p.value
    
    adj_r_sq <- model_summary$adj.r.squared
    f_stat <- model_summary$fstatistic
    p_f_stat <- if(!is.null(f_stat)) pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE) else 1
    coef_summary <- model_summary$coefficients
    
    html_asumsi <- paste(
      "<h4>Ringkasan Uji Asumsi</h4><ul>",
      paste0("<li><b>Multikolinearitas (VIF):</b> ", if(any(vif_vals > 10)) "Asumsi Terlanggar (VIF > 10)." else "Asumsi terpenuhi (VIF < 10).", "</li>"),
      paste0("<li><b>Normalitas Residual (Shapiro-Wilk):</b> ", if(!is.na(p_shapiro) && p_shapiro < 0.05) "Asumsi terlanggar (p < 0.05)." else "Asumsi terpenuhi (p ≥ 0.05).", "</li>"),
      paste0("<li><b>Homoskedastisitas (Breusch-Pagan):</b> ", if(!is.na(p_bp) && p_bp < 0.05) "Asumsi terlanggar (terjadi heteroskedastisitas)." else "Asumsi terpenuhi (homoskedastisitas).", "</li>"),
      paste0("<li><b>Autokorelasi Spasial (Moran's I):</b> ", if(!is.na(p_moran) && p_moran < 0.05) "Asumsi terlanggar (terdapat autokorelasi spasial)." else "Asumsi terpenuhi (tidak ada autokorelasi spasial).", "</li>"),
      "</ul>"
    )
    
    html_model <- paste(
      "<h4>Interpretasi Kelayakan Model</h4><ul>",
      paste0("<li><b>Kelayakan Model (Uji-F):</b> Berdasarkan Uji F, model secara keseluruhan ", if(!is.na(p_f_stat) && p_f_stat < 0.05) "<b>layak (signifikan)</b>" else "<b>tidak layak (tidak signifikan)</b>", " untuk digunakan (p = ", round(p_f_stat, 4), ").</li>"),
      paste0("<li><b>Koefisien Determinasi (Adjusted R-squared):</b> Sebesar <b>", round(adj_r_sq * 100, 2), "%</b> keragaman pada variabel '", input$y_reg, "' mampu dijelaskan oleh model.</li>"),
      "</ul>"
    )
    
    interpretasi_coef_list <- ""
    for (i in 2:nrow(coef_summary)) {
      var_name <- rownames(coef_summary)[i]; estimate <- coef_summary[i, 1]; p_val <- coef_summary[i, 4]
      
      if (!is.na(p_val) && p_val < 0.05) {
        arah <- if(estimate > 0) "positif (menaikkan)" else "negatif (menurunkan)"
        interpretasi_coef_list <- paste0(interpretasi_coef_list, "<li>Variabel <b>", var_name, "</b> berpengaruh secara signifikan dan ", arah, " terhadap <b>", input$y_reg, "</b>. Kenaikan satu unit pada '", var_name, "', akan mengubah '", input$y_reg, "' sebesar ", round(estimate, 4), ", ceteris paribus.</li>")
      }
    }
    
    html_coef <- paste(
      "<h4>Interpretasi Koefisien (Variabel Signifikan)</h4><ul>",
      if(interpretasi_coef_list == "") "<li>Tidak ada variabel independen yang berpengaruh signifikan secara statistik.</li>" else interpretasi_coef_list,
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
      
      params <- list(model = model_fit(), y_var = input$y_reg, x_vars = input$x_reg, weights_list = weights_list)
      
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent=globalenv()))
    })
  
  # Menu Unduh Data
  data_to_download <- reactive({
    req(input$vars_unduh)
    dplyr::select(data_reaktif(), all_of(input$vars_unduh))
  })
  
  output$tabel_preview_unduh <- renderDT({
    datatable(data_to_download(), options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE)
  })
  
  output$download_data_button <- downloadHandler(
    filename = function() {
      paste0("data_reaktif()_pilihan-", Sys.Date(), ".", input$format_unduh)
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