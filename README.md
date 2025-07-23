# DataVerse SOVI: Dashboard Analisis Kerentanan Sosial Indonesia

Platform interaktif berbasis R Shiny untuk visualisasi dan analisis data Social Vulnerability Index (SOVI) di seluruh Indonesia. Dashboard ini menyediakan tools komprehensif untuk eksplorasi data, analisis statistik, dan pemodelan regresi dalam satu interface yang user-friendly.

## 🎯 Fitur Utama

### 📊 **Manajemen Data**
- Kategorisasi variabel kontinyu menggunakan metode kuantil atau interval sama
- Transformasi data (logaritma, akar kuadrat, arcsin) untuk treatment asumsi statistik
- Export data dalam format CSV, Excel (.xlsx), dan SPSS (.sav)

### 🔍 **Eksplorasi Data**
- Statistik deskriptif komprehensif (mean, median, skewness, kurtosis, dll.)
- Visualisasi distribusi data (boxplot, histogram, Q-Q plot)
- Matriks korelasi
- Interpretasi
- Laporan hasil analisis yang dapat diunduh dalam format .pdf

### 🗺️ **Peta Interaktif**
- Visualisasi spasial menggunakan Leaflet dengan palet warna YlOrRd (kuning ke merah)
- Choropleth mapping berdasarkan nilai variabel per kabupaten/kota
- Tooltip interaktif untuk melihat detail nilai saat hover
- Legend otomatis untuk interpretasi gradasi warna

### ✅ **Uji Asumsi Klasik**
- Uji normalitas (Shapiro-Wilk)
- Uji homogenitas varians (Levene's Test)
- Deteksi outlier menggunakan metode IQR dan boxplot
- Interpretasi dan rekomendasi statistik berdasarkan hasil uji
- Laporan hasil analisis yang dapat diunduh dalam format .pdf

### 📈 **Statistik Inferensia**
- **Uji Beda Rata-rata**: One-sample t-test, Two-sample t-test
- **Uji Proporsi**: One-proportion test, Two-proportion test
- **Uji Varians**: F-test untuk perbandingan varians
- **ANOVA**: One-way dan Two-way ANOVA dengan uji post-hoc Tukey HSD
- Laporan hasil analisis yang dapat diunduh dalam format .pdf

### 📉 **Analisis Regresi**
- Regresi linier berganda
- Uji asumsi regresi (multikolinearitas, normalitas residual, homoskedastisitas)
- Uji autokorelasi spasial (Moran's I)
- Interpretasi otomatis koefisien dan kelayakan model
- Laporan hasil analisis yang dapat diunduh dalam format .pdf

## 🗂️ Struktur Data

Dashboard menggunakan dataset SOVI Indonesia dengan variabel-variabel berikut:

| Variabel | Tipe | Deskripsi |
|----------|------|-----------|
| **PROVINCE_NAME** | Character | Nama provinsi |
| **CITY_NAME** | Character | Nama kabupaten/kota |
| **DISTRICTCODE** | Character | Kode wilayah administratif |
| **CHILDREN** | Numeric (%) | Persentase penduduk < 5 tahun |
| **FEMALE** | Numeric (%) | Persentase penduduk perempuan |
| **ELDERLY** | Numeric (%) | Persentase penduduk > 65 tahun |
| **FHEAD** | Numeric (%) | Persentase RT dengan kepala keluarga perempuan |
| **FAMILYSIZE** | Numeric | Rata-rata jumlah anggota RT |
| **NOELECTRIC** | Numeric (%) | Persentase RT tanpa listrik |
| **LOWEDU** | Numeric (%) | Persentase penduduk berpendidikan rendah |
| **GROWTH** | Numeric (%) | Persentase pertumbuhan penduduk |
| **POVERTY** | Numeric (%) | Persentase penduduk miskin |
| **ILLITERATE** | Numeric (%) | Persentase penduduk buta huruf |
| **NOTRAINING** | Numeric (%) | Persentase RT tanpa pelatihan kebencanaan |
| **DPRONE** | Numeric (%) | Persentase RT di daerah rawan bencana |
| **RENTED** | Numeric (%) | Persentase RT yang menyewa rumah |
| **NOSEWER** | Numeric (%) | Persentase RT tanpa sistem pembuangan limbah |
| **TAPWATER** | Numeric (%) | Persentase RT pengguna air ledeng |
| **POPULATION** | Integer | Total populasi |

## 💻 Instalasi dan Setup

### Prerequisites
```r
# Install required packages
install.packages(c(
  "shiny", "shinydashboard", "dplyr", "sf", "leaflet", 
  "ggplot2", "DT", "GGally", "car", "lmtest", "DescTools",
  "rmarkdown", "writexl", "haven", "shinycssloaders", 
  "purrr", "spdep"
))
```

### Data Requirements
Pastikan file-file berikut tersedia dalam folder `data/`:
- `sovi_data.csv` - Dataset SOVI Indonesia
- `indonesia511.geojson` - File geometri peta Indonesia (kabupaten/kota)

### Struktur Folder
```
project/
├── global.R          # Load packages dan data
├── server.R          # Server logic
├── ui.R              # User interface
├── data/
│   ├── sovi_data.csv
│   └── indonesia511.geojson
└── www/              # Template laporan PDF
    ├── Laporan-Eksplorasi.Rmd
    ├── Laporan-Uji-Asumsi.Rmd
    ├── Laporan-Uji-Beda-Rata-rata.Rmd
    ├── Laporan-Uji-PropVar.Rmd
    ├── Laporan-ANOVA.Rmd
    └── Laporan-Regresi.Rmd
```

## 🚀 Panduan Penggunaan

### 1. **Manajemen Data**
- Pilih variabel kontinyu yang ingin dikategorikan
- Tentukan metode kategorisasi (kuantil atau interval sama)
- Atur jumlah kategori dan beri nama untuk setiap kategori
- Unduh hasil kategorisasi dalam format yang diinginkan

### 2. **Eksplorasi Data**
- Pilih variabel yang ingin dianalisis menggunakan filter utama
- Review statistik deskriptif pada tabel interaktif
- Periksa distribusi data melalui visualisasi
- Analisis korelasi antar variabel
- Unduh laporan eksplorasi dalam format PDF

### 3. **Peta Interaktif**
- Pilih variabel untuk ditampilkan pada peta choropleth
- Hover pada wilayah untuk melihat nama dan nilai spesifik
- Gunakan legend untuk memahami gradasi warna (kuning = rendah, merah = tinggi)
- Identifikasi wilayah dengan nilai tertinggi dan terendah melalui interpretasi otomatis
  
### 4. **Uji Asumsi**
- Pilih variabel untuk diuji asumsinya
- Review hasil uji normalitas, homogenitas, dan deteksi outlier
- Gunakan fitur transformasi jika asumsi tidak terpenuhi
- Buat variabel baru hasil transformasi
- Interpretasi dan unduh laporan analisis dalam format PDF

### 5. **Statistik Inferensia**
- **Uji t**: Bandingkan rata-rata dengan nilai tertentu atau antar kelompok
- **Uji Proporsi**: Analisis proporsi dalam satu atau dua kelompok
- **ANOVA**: Bandingkan rata-rata lebih dari dua kelompok
- Interpretasi dan unduh laporan analisis dalam format PDF 

### 6. **Analisis Regresi**
- Pilih variabel dependen dan independen
- Terapkan transformasi jika diperlukan
- Review hasil model dan uji asumsi
- Interpretasi koefisien secara otomatis
- Unduh laporan analisis dalam format PDF

## 📋 Output dan Laporan

Dashboard menghasilkan berbagai output:

### **Laporan Hasil Analisis PDF**
- Laporan Eksplorasi Data
- Laporan Uji Asumsi
- Laporan Uji Statistik (t-test, proporsi, ANOVA)
- Laporan Analisis Regresi

### **Unduh Data**
- CSV untuk analisis lanjutan
- Excel (.xlsx) untuk reporting
- SPSS (.sav) untuk analisis statistik lanjutan

## ⚠️ Catatan Penting

- **Ukuran Data**: Uji Shapiro-Wilk terbatas untuk data ≤ 5000 observasi
- **Missing Values**: Data missing akan diabaikan dalam analisis
- **Spatial Analysis**: Memerlukan file geometri yang kompatibel dengan data
- **Performance**: Dashboard optimal untuk dataset dengan <50 variabel

---

*Faliza Maulidina Syarief*

*222313077*

*2KS3*
