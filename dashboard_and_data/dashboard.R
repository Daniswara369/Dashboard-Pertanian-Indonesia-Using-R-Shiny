library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(shinydashboardPlus)
library(dashboardthemes)
library(leaflet)
library(leafpop)
library(mapview)
library(DT)
library(summarytools)

#data
dpangan <- read.csv("pangan.csv", sep = ";")
dkebun <- read.csv("kebun.csv", sep = ";")
dhortikultura <- read.csv("hortikultura.csv", sep = ";")
dbuah <- read.csv("buah.csv", sep = ";")
dTernak <- read.csv("ternak.csv", sep = ";")
dUnggas <- read.csv("unggas.csv", sep = ";")
dPertumbuhan <- read.csv("dataPertumbuhan.csv", sep = ";")
location <- read.csv("location.csv", sep = ";")

## Rename columns
dUnggas <- rename(dUnggas, "Ayam Kampung" = Ayam.Kampung, "Ayam Petelur" = Ayam.Petelur, "Ayam Pedaging" = Ayam.Pedaging)
dTernak <- rename(dTernak, "Sapi Perah" = Sapi.Perah, "Sapi Potong" = Sapi.Potong)
dbuah <- rename(dbuah, "Jeruk Siam" = Jeruk.Siam)
dhortikultura <- rename(dhortikultura, "Bawang Merah" = Bawang.Merah, "Cabai Besar" = Cabai.Besar, "Cabai Keriting" = Cabai.Keriting, "Bawang Putih" = Bawang.Putih)
dkebun <- rename(dkebun, "Kelapa Sawit" = Kelapa.Sawit)
dPertumbuhan <- rename(dPertumbuhan, "2022" = X2022, "2021" = X2021, "2020" = X2020, "2019" = X2019)


Header <- dashboardHeader(
  title = "...",
  dropdownMenu(
    type = "notifications",
    notificationItem(text = "Data Referensi", href = "https://www.bps.go.id/publication/2023/02/28/18018f9896f09f03580a614b/statistik-indonesia-2023.html")
    
  ),
  dropdownMenu(
    type = "tasks",
    taskItem(text = "Kesukaan Terhadap Makanan Lokal", value = 88)
  ),
  dropdownMenu(
    type = "messages",
    messageItem(from = "Penulis", message = "Yuk Dukung Peternak dan Petani Lokal")
  )
)

Sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),
             menuSubItem("Tumbuhan", tabName = "tumbuhan", icon = icon("leaf")),
             menuSubItem("Hewan", tabName = "hewan", icon = icon("paw"))
    ),
    menuItem("Description", tabName = "information", icon = icon("bullseye") ),
    menuItem("Dataset", tabName="dataset", icon = icon("database") ),
    menuItem("Author", tabName = "author", icon = icon("user")),
    menuItem("About more", icon = icon("send", lib = "glyphicon"), href = "https://www.pertanian.go.id/")
  ))

Body <- dashboardBody(
  shinyDashboardThemes(theme = "grey_dark"),
  tags$head(
    tags$style(HTML("
      .carousel-inner > .item > img {
        display: block;
        margin-left: auto;
        margin-right: auto;
        width: 100%;
        height: 300px; /* Adjust the height */
        object-fit: cover; /* Ensures the images maintain their aspect ratio */
      }
    .youtube-video {
      position: relative;
      padding-bottom: 71%; 
      height: auto;
      overflow: hidden;
    }
    .youtube-video iframe {
      position: absolute;
      top: 0;
      left: 0;
      width: 96.5%;
      height: 100%;
    }
    "))
  ),
  tags$style(HTML('
      .content-wrapper {
        background: linear-gradient(to bottom,#7BCA9B, #2B8832);
      }
    ')),
  tabItems(
    tabItem(tabName = "home",
            titlePanel(
              h2(strong("Sistem Informasi Pertanian Indonesia"),
                 style="text-align:center;")),
            br(),
            carousel(width = 12,
                     id = "mycarousel",
                     carouselItem(
                       caption = h4(strong("Jokowi dan Menteri Pertanian Mendapat Apresiasi Dari Petani Saat Pekan Nasional (PENAS) Petani Nelayan XVI"),style="text-align:left;"),
                       tags$img(src = "https://assets.indopos.co.id/2023/02/Penas-XVI-750x375.jpg")
                     ),
                     carouselItem(
                       caption = h4(strong("Tingkatkan Sektor Pertanian, Menteri Pertanian SYL Dorong Pemda Optimalkan Serapan KUR Pertanian")),
                       tags$img(src = "https://cdn1-production-images-kly.akamaized.net/uzjcOPPVHROQD9P49CwHuJ_FYP8=/640x360/smart/filters:quality(75):strip_icc():format(webp)/kly-media-production/medias/4460665/original/042492200_1686376004-Kementan_2.jpg")
                     ),
                     carouselItem(
                       caption = h4(strong("Mentan SYL Dorong Pengembangan Varietas Tanaman Unggul untuk Perkuat Pertanian Indonesia")),
                       tags$img(src = "https://cdn1-production-images-kly.akamaized.net/aNXfgP4f-A71pQRlJ5ZBI2OlByc=/0x0:1280x721/640x360/filters:quality(75):strip_icc():format(webp)/kly-media-production/medias/4449171/original/073765400_1685563015-image__35_.jpg")
                     ),
                     carouselItem(
                       caption = h4(strong("Hadapi Tantangan Global, Mentan Ajak Pemuda Pancasila Perkuat Sektor Pertanian")),
                       tags$img(src = "https://cdn1-production-images-kly.akamaized.net/5U-p5fjKhWjP2g2ND6Py4R6vatQ=/640x360/smart/filters:quality(75):strip_icc():format(webp)/kly-media-production/medias/4445734/original/079664100_1685362463-image__11_.jpg")
                     ),
                     carouselItem(
                       caption = h4(strong("Mentan SYL Lepas Ekspor Produk Pertanian ke 23 Negara, Targetnya 1000 Triliun")),
                       tags$img(src = "https://cdn1-production-images-kly.akamaized.net/rG5sR9eH2_nEOGki1S6ZZva_6Xs=/640x360/smart/filters:quality(75):strip_icc():format(webp)/kly-media-production/medias/4438204/original/044284900_1684847243-WhatsApp_Image_2023-05-23_at_7.21.37_PM.jpeg")
                     )
            ),
            sidebarPanel(width = 6,
                         p(strong("Selamat datang di Sistem Informasi Pertanian di Indonesia! Sistem informasi ini dibangun sebagai bagian dari tugas mata kuliah Sistem Informasi Manajemen Departemen Statistika Fakultas Sains dan Analitika Data ITS, dengan tujuan untuk memenuhi kebutuhan informasi dan pengelolaan data sektor pertanian di Indonesia."),
                           style= "text-align:justify;"),
                         p(strong("Sistem informasi ini didesain agar memberikan akses mudah dan terkini terhadap informasi penting seputar pertanian di Indonesia. Informasi yang disediakan meliputi data tanaman, hewan ternak, dan map komoditas setiap provinsi di Indonesia. Dengan demikian, sistem ini diharapkan dapat membantu para petani dalam pengambilan keputusan yang lebih baik, meningkatkan produktivitas dan efisiensi pertanian, serta memfasilitasi penelitian dan pengembangan di bidang pertanian."),
                           style= "text-align:justify;")),
            fluidRow(
              column(
                width = 6,br(),
                div(class = "youtube-video",
                    tags$iframe(src = "https://www.youtube.com/embed/IEqPm2PMoTA", frameborder = "0", allowfullscreen = "true")
                )
              )
            )),
    tabItem(
      tabName = "tumbuhan",
      fluidRow(
        tabsetPanel(
          tabPanel("Pangan", icon = icon("seedling"),
                   fluidRow(
                     valueBoxOutput("totalProduksiPangan"),
                     valueBoxOutput("jumlahPanganTerbanyak"),
                     valueBoxOutput("pertumbuhanPangan")
                   ),
                   fluidRow(
                     column(width = 9, selectInput("provinsiPangan", "Provinsi", choices = unique(dpangan$Provinsi))),
                     column(width = 9, selectInput("tahunPangan", "Tahun", choices = unique(dpangan$Tahun))),
                     plotOutput("plotPangan")
                   )),
          tabPanel("Hortikultura", icon = icon("pepper-hot"),
                   fluidRow(
                     valueBoxOutput("totalProduksiHortikultura"),
                     valueBoxOutput("jumlahHortikulturaTerbanyak"),
                     valueBoxOutput("pertumbuhanHortikultura")
                   ),
                   fluidRow(
                     column(width = 9, selectInput("provinsiHortikultura", "Provinsi", choices = unique(dhortikultura$Provinsi))),
                     column(width = 9, selectInput("tahunHortikultura", "Tahun", choices = unique(dhortikultura$Tahun))),
                     plotOutput("plotHortikultura")
                   )),
          tabPanel("Buah", icon = icon("apple"),
                   fluidRow(
                     valueBoxOutput("totalProduksiBuah"),
                     valueBoxOutput("jumlahBuahTerbanyak"),
                     valueBoxOutput("pertumbuhanBuah")
                   ),
                   fluidRow(
                     column(width = 9, selectInput("provinsiBuah", "Provinsi", choices = unique(dbuah$Provinsi))),
                     column(width = 9, selectInput("tahunBuah", "Tahun", choices = unique(dbuah$Tahun))),
                     plotOutput("plotBuah")
                   )),
          tabPanel("Perkebunan", icon = icon("tree"),
                   fluidRow(
                     valueBoxOutput("totalProduksiKebun"),
                     valueBoxOutput("jumlahKebunTerbanyak"),
                     valueBoxOutput("pertumbuhanKebun")
                   ),
                   fluidRow(
                     column(width = 9, selectInput("provinsiKebun", "Provinsi", choices = unique(dkebun$Provinsi))),
                     column(width = 9, selectInput("tahunKebun", "Tahun", choices = unique(dkebun$Tahun))),
                     plotOutput("plotKebun")
                   )),
          tabPanel("Pertumbuhan Produksi", icon = icon("chart-line"),
                   fluidRow(
                     column(width = 9, selectInput("kelompok", "Kelompok Tumbuhan",
                                                   choices = unique(dPertumbuhan$Kelompok))),
                     column(width = 9, selectInput("jenis", "Jenis Tumbuhan",
                                                   choices = NULL)),
                     valueBoxOutput("rataPertumbuhan"),
                     plotOutput("pertumbuhan")
                   )),
          tabPanel("Map Komoditas", icon = icon("map"),
                   fluidRow(
                     column(width = 3, align = "center", offset = 4, selectInput("komoditas1", "Komoditas", 
                                                                                 choices = c("Pangan","Hortikultura","Buah","Perkebunan"))),
                     leafletOutput("Map1")
                   ))
        )
      )
    ),
    tabItem(
      tabName = "hewan",
      fluidRow(
        tabsetPanel(
          tabPanel("Hewan Ternak", icon = icon("cow"),
                   fluidRow(
                     valueBoxOutput("totalPopulasiTernak"),
                     valueBoxOutput("jumlahTernakTerbanyak"),
                     valueBoxOutput("pertumbuhanTernak")
                   ),
                   fluidRow(
                     column(width = 9, selectInput("provinsiTernak", "Provinsi", choices = unique(dTernak$Provinsi))),
                     column(width = 9, selectInput("tahunTernak", "Tahun", choices = unique(dTernak$Tahun))),
                     plotOutput("plotTernak")
                   )),
          tabPanel("Unggas", icon = icon("feather"), 
                   fluidRow(
                     valueBoxOutput("totalPopulasiUnggas"),
                     valueBoxOutput("jumlahUnggasTerbanyak"),
                     valueBoxOutput("pertumbuhanUnggas")
                   ),
                   fluidRow(
                     column(width = 9, selectInput("provinsiUnggas", "Provinsi", choices = unique(dUnggas$Provinsi))),
                     column(width = 9, selectInput("tahunUnggas", "Tahun", choices = unique(dUnggas$Tahun))),
                     plotOutput("plotUnggas")
                   )),
          tabPanel("Map Komoditas", icon = icon("map"),
                   fluidRow(
                     column(width = 3, align = "center", offset = 4, selectInput("komoditas2", "Komoditas", 
                                                                                 choices = c("Ternak", "Unggas"))),
                     leafletOutput("Map2")
                   ))
        )
      )
    ),
    tabItem(
      tabName = "information",
      fluidRow(
        sidebarPanel(width=12,
                     h3(strong("KETERANGAN")),
                     h4("Tanaman Pangan:"),
                     p("Tanaman pangan adalah tanaman yang dikembangkan dan dihasilkan untuk tujuan konsumsi manusia sebagai sumber makanan utama atau bahan baku pangan. Contoh tanaman pangan meliputi padi, jagung, dan sebagainya."),
                     
                     h4("Tanaman Hortikultura:"),
                     p("Tanaman hortikultura adalah kelompok tanaman yang berkaitan dengan budidaya tanaman hias, tanaman sayuran, dan tanaman buah-buahan non-pangan. Hortikultura mencakup berbagai jenis tanaman yang biasanya ditanam untuk keindahan estetika, kegiatan rekreasi, atau sebagai tanaman hias dalam taman, kebun, atau ruang dalam ruangan. Selain itu, hortikultura juga mencakup budidaya tanaman sayuran untuk konsumsi manusia dan tanaman buah-buahan yang tidak termasuk dalam kategori tanaman pangan utama. Budidaya tanaman hortikultura meliputi pemilihan bibit yang berkualitas, perawatan tanaman, pemeliharaan kebersihan, pengendalian hama dan penyakit, serta pemangkasan untuk mencapai hasil yang diinginkan. Contoh tanaman hortikultura meliputi mawar, tomat, kubis, cabai, dan sebagainya."),
                     
                     h4("Tanaman Buah:"),
                     p("Tanaman buah adalah jenis tanaman yang dikembangkan dan dihasilkan untuk menghasilkan buah yang dapat dikonsumsi. Buah biasanya dikonsumsi segar atau diolah menjadi berbagai produk seperti jus, selai, kue, dan sebagainya. Contoh tanaman buah meliputi apel, pisang, jeruk, mangga, stroberi, dan sebagainya."),
                     
                     h4("Tanaman Perkebunan:"),
                     p("Tanaman perkebunan merujuk pada tanaman yang ditanam dalam skala besar untuk tujuan komersial. Tanaman perkebunan umumnya meliputi tanaman seperti kopi, teh, kelapa sawit, karet, teh, tembakau, cokelat, dan sejenisnya."),
                     
                     h4("Hewan Ternak:"),
                     p("Hewan ternak merujuk pada hewan yang dipelihara untuk tujuan produksi seperti penghasilan daging, susu, telur, dan produk-produk lainnya. Hewan ternak umumnya dibudidayakan dalam peternakan atau peternakan komersial. Contoh hewan ternak meliputi sapi, kambing, domba, babi, dan sebagainya."),
                     
                     h4("Hewan Unggas:"),
                     p("Hewan ternak unggas adalah hewan ternak yang termasuk dalam kelompok unggas atau burung. Hewan ternak unggas ini biasanya dipelihara untuk menghasilkan daging, telur, dan bulu. Contoh hewan ternak unggas meliputi ayam, bebek, kalkun, dan burung puyuh.")
        ),
      )
    ),
    tabItem(tabName = "dataset",
            tabsetPanel(
              tabPanel("DataFrame", icon = icon("table"),
                       selectInput("dataframe", label = "Data", 
                                   choices = c("Pangan", "Hortikultura", "Buah", "Perkebunan", "Ternak", "Unggas", "Pertumbuhan Tumbuhan")),
                       downloadButton("downloadData", "Unduh Data"),
                       DTOutput("table_data")
              ),
              tabPanel("Summary", icon = icon("pen"),
                       verbatimTextOutput("summary_stat")
              )
            )
    ),
    tabItem(tabName = "author",
            div(h2(strong("Sistem Informasi"), style = "text-align: center;")),
            div(h2(strong("Pertanian Indonesia"), style = "text-align: center;")),
            br(),
            br(),
            div(h4(strong("Penyusun"), style = "text-align: center;")),
            br(),
            fluidRow(
              column(
                width = 6,
                status = NULL,
                div(imageOutput("foto1"), style = "text-align:center;",
                    style = "margin-bottom:-180px;"),
                div(strong("Daniswara Aditya Putra"), style = "text-align:center;"),
                div(strong("5003211132"), style = "text-align: center;"))
              ,
              column(
                width = 6,
                status = NULL,
                div(imageOutput("foto2"), style = "text-align:center;",
                    style = "margin-bottom:-180px;"),
                div(strong("Wily Ferdian"), style = "text-align:center;"),
                div(strong("5003211153"), style = "text-align: center;")
              ),
              div(h1(".")),
              br(),
              div(h5("Sistem Informasi Manajemen"), style = "text-align: center;"),
              div(h6("Departemen Statistika"), style = "text-align: center;"),
              div(h6("Fakultas Sains dan Analitika Data"), style = "text-align: center;"),
              div(h6("Institut Teknologi Sepuluh Nopember"), style = "text-align: center;"),
              div(h6("2023"), style = "text-align: center;")
            )),
    tabItem(
      tabName = "about_more",
      fluidRow(
        box("About more tab content")
      )
    )
  )
)

server <- function(input, output, session){
  filteredDataPangan <- reactive({
    dpangan %>%
      filter(Provinsi == input$provinsiPangan) %>%
      filter(Tahun == input$tahunPangan) %>%
      pivot_longer(cols = colnames(dpangan)[3:length(colnames(dpangan))], names_to = "Komoditas", values_to = "Jumlah_Produksi") %>%
      mutate(Komoditas = reorder(Komoditas, -Jumlah_Produksi))
  })
  
  output$plotPangan <- renderPlot({
    filteredData <- filteredDataPangan()
    ggplot(filteredData, aes(x = Komoditas, y = Jumlah_Produksi, fill = Komoditas)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Jumlah_Produksi), vjust = -0.5, size = 3.5, color = "black") +
      labs(x = "Komoditas", y = "Jumlah Produksi", title = paste("Produksi Komoditas Pangan di", input$provinsiPangan, "Tahun", input$tahunPangan, "(Ton)")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
  })
  
  output$jumlahPanganTerbanyak <- renderValueBox({
    filteredData <- filteredDataPangan()
    jumlahTerbanyak <- filteredData %>%
      group_by(Komoditas) %>%
      summarise(Total_Jumlah = sum(Jumlah_Produksi)) %>%
      filter(Total_Jumlah == max(Total_Jumlah))
    
    valueBox(
      formatC(jumlahTerbanyak$Total_Jumlah, format = "d", big.mark = ","),
      paste("Komoditas Terbanyak:", jumlahTerbanyak$Komoditas),
      icon = icon("star", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$totalProduksiPangan <- renderValueBox({
    filteredData <- filteredDataPangan()
    totalProduksi <- filteredData %>%
      summarise(Total_Produksi = sum(Jumlah_Produksi))
    
    valueBox(
      formatC(totalProduksi$Total_Produksi, format = "d", big.mark = ","),
      "Total Produksi",
      icon = icon("stats", lib = "glyphicon"),
      color = "orange"
    )
  })
  
  output$pertumbuhanPangan <- renderValueBox({
    dataPertumbuhanPangan <- dpangan %>%
      filter(Provinsi == input$provinsiPangan) %>%
      pivot_longer(cols = colnames(dpangan)[3:length(colnames(dpangan))], names_to = "Komoditas", values_to = "Jumlah_Produksi") %>%
      mutate(Komoditas = reorder(Komoditas, -Jumlah_Produksi))
    
    komoditas2022 <- dataPertumbuhanPangan %>%
      filter(Tahun == 2022)
    totalProduksi2022 <- komoditas2022 %>%
      summarise(Total_Produksi = sum(Jumlah_Produksi))
    
    komoditas2021 <- dataPertumbuhanPangan %>%
      filter(Tahun == 2021)
    totalProduksi2021 <- komoditas2021 %>%
      summarise(Total_Produksi = sum(Jumlah_Produksi))
    
    pertumbuhanKomoditas <- ((totalProduksi2022$Total_Produksi - totalProduksi2021$Total_Produksi) / totalProduksi2021$Total_Produksi) * 100
    
    valueBox(
      paste0(formatC(pertumbuhanKomoditas, digits = 3), "%"),
      "Pertumbuhan Total Produksi",
      icon = icon("chart-line", lib = "font-awesome"),
      color = "red"
    )
  })
  
  filteredDataHor <- reactive({
    dhortikultura %>%
      filter(Provinsi == input$provinsiHortikultura) %>%
      filter(Tahun == input$tahunHortikultura) %>%
      pivot_longer(cols = colnames(dhortikultura)[3:length(colnames(dhortikultura))], names_to = "Tanaman", values_to = "Jumlah_Produksi") %>%
      mutate(Tanaman = reorder(Tanaman, -Jumlah_Produksi))
  })
  
  output$plotHortikultura <- renderPlot({
    filteredData <- filteredDataHor()
    ggplot(filteredData, aes(x = Tanaman, y = Jumlah_Produksi, fill = Tanaman)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Jumlah_Produksi), vjust = -0.5, size = 3.5, color = "black") +
      labs(x = "Tanaman", y = "Jumlah Produksi", title = paste("Produksi Tanaman Hortikultura di", input$provinsiHortikultura, "Tahun", input$tahunHortikultura, "(Ton)")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
  })
  
  output$jumlahHortikulturaTerbanyak <- renderValueBox({
    filteredData <- filteredDataHor()
    jumlahTerbanyak <- filteredData %>%
      group_by(Tanaman) %>%
      summarise(Total_Jumlah = sum(Jumlah_Produksi)) %>%
      filter(Total_Jumlah == max(Total_Jumlah))
    
    valueBox(
      formatC(jumlahTerbanyak$Total_Jumlah, format = "d", big.mark = ","),
      paste("Tanaman Terbanyak:", jumlahTerbanyak$Tanaman),
      icon = icon("star", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$totalProduksiHortikultura <- renderValueBox({
    filteredData <- filteredDataHor()
    totalProduksi <- filteredData %>%
      summarise(Total_Produksi = sum(Jumlah_Produksi))
    
    valueBox(
      formatC(totalProduksi$Total_Produksi, format = "d", big.mark = ","),
      "Total Produksi",
      icon = icon("stats", lib = "glyphicon"),
      color = "orange"
    )
  })
  
  output$pertumbuhanHortikultura <- renderValueBox({
    dataPertumbuhanHortikultura <- dhortikultura %>%
      filter(Provinsi == input$provinsiHortikultura) %>%
      pivot_longer(cols = colnames(dhortikultura)[3:length(colnames(dhortikultura))], names_to = "Tanaman", values_to = "Jumlah_Produksi") %>%
      mutate(Tanaman = reorder(Tanaman, -Jumlah_Produksi))
    
    tanaman2022 <- dataPertumbuhanHortikultura %>%
      filter(Tahun == 2022)
    totalProduksi2022 <- tanaman2022 %>%
      summarise(Total_Produksi = sum(Jumlah_Produksi))
    
    tanaman2021 <- dataPertumbuhanHortikultura %>%
      filter(Tahun == 2021)
    totalProduksi2021 <- tanaman2021 %>%
      summarise(Total_Produksi = sum(Jumlah_Produksi))
    
    pertumbuhanTanaman <- ((totalProduksi2022$Total_Produksi - totalProduksi2021$Total_Produksi) / totalProduksi2021$Total_Produksi) * 100
    
    valueBox(
      paste0(formatC(pertumbuhanTanaman, digits = 3), "%"),
      "Pertumbuhan Total Produksi",
      icon = icon("chart-line", lib = "font-awesome"),
      color = "red"
    )
  })
  
  filteredDataBuah <- reactive({
    dbuah %>%
      filter(Provinsi == input$provinsiBuah) %>%
      filter(Tahun == input$tahunBuah) %>%
      pivot_longer(cols = colnames(dbuah)[3:length(colnames(dbuah))], names_to = "Komoditas", values_to = "Jumlah_Produksi") %>%
      mutate(Komoditas = reorder(Komoditas, -Jumlah_Produksi))
  })
  
  output$plotBuah <- renderPlot({
    filteredData <- filteredDataBuah()
    ggplot(filteredData, aes(x = Komoditas, y = Jumlah_Produksi, fill = Komoditas)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Jumlah_Produksi), vjust = -0.5, size = 3.5, color = "black") +
      labs(x = "Komoditas", y = "Jumlah Produksi", title = paste("Produksi Komoditas Buah di", input$provinsiBuah, "Tahun", input$tahunBuah, "(Ton)")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 20, face="bold"))
  })
  
  output$jumlahBuahTerbanyak <- renderValueBox({
    filteredData <- filteredDataBuah()
    jumlahTerbanyak <- filteredData %>%
      group_by(Komoditas) %>%
      summarise(Total_Jumlah = sum(Jumlah_Produksi)) %>%
      filter(Total_Jumlah == max(Total_Jumlah))
    
    valueBox(
      formatC(jumlahTerbanyak$Total_Jumlah, format = "d", big.mark = ","),
      paste("Komoditas Terbanyak:", jumlahTerbanyak$Komoditas),
      icon = icon("star", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$totalProduksiBuah <- renderValueBox({
    filteredData <- filteredDataBuah()
    totalProduksi <- filteredData %>%
      summarise(Total_Produksi = sum(Jumlah_Produksi))
    
    valueBox(
      formatC(totalProduksi$Total_Produksi, format = "d", big.mark = ","),
      "Total Produksi",
      icon = icon("stats", lib = "glyphicon"),
      color = "orange"
    )
  })
  
  output$pertumbuhanBuah <- renderValueBox({
    dataPertumbuhanBuah <- dbuah %>%
      filter(Provinsi == input$provinsiBuah) %>%
      pivot_longer(cols = colnames(dbuah)[3:length(colnames(dbuah))], names_to = "Komoditas", values_to = "Jumlah_Produksi") %>%
      mutate(Komoditas = reorder(Komoditas, -Jumlah_Produksi))
    
    buah2022 <- dataPertumbuhanBuah %>%
      filter(Tahun == 2022)
    totalProduksi2022 <- buah2022 %>%
      summarise(Total_Produksi = sum(Jumlah_Produksi))
    
    buah2021 <- dataPertumbuhanBuah %>%
      filter(Tahun == 2021)
    totalProduksi2021 <- buah2021 %>%
      summarise(Total_Produksi = sum(Jumlah_Produksi))
    
    pertumbuhanbuah <- ((totalProduksi2022$Total_Produksi - totalProduksi2021$Total_Produksi) / totalProduksi2021$Total_Produksi) * 100
    
    valueBox(
      paste0(formatC(pertumbuhanbuah, digits = 3), "%"),
      "Pertumbuhan Total Produksi",
      icon = icon("chart-line", lib = "font-awesome"),
      color = "red"
    )
  })
  
  filteredDataKebun <- reactive({
    dkebun %>%
      filter(Provinsi == input$provinsiKebun) %>%
      filter(Tahun == input$tahunKebun) %>%
      pivot_longer(cols = colnames(dkebun)[3:length(colnames(dkebun))], names_to = "Kebun", values_to = "Jumlah_Produksi") %>%
      mutate(Kebun = reorder(Kebun, -Jumlah_Produksi))
  })
  
  output$plotKebun <- renderPlot({
    filteredData <- filteredDataKebun()
    ggplot(filteredData, aes(x = Kebun, y = Jumlah_Produksi, fill = Kebun)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Jumlah_Produksi), vjust = -0.5, size = 3.5, color = "black") +
      labs(x = "Kebun", y = "Jumlah Produksi", title = paste("Produksi Tanaman Perkebunan di", input$provinsiKebun, "Tahun", input$tahunKebun, "(Ton)")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
  })
  
  output$jumlahKebunTerbanyak <- renderValueBox({
    filteredData <- filteredDataKebun()
    jumlahTerbanyak <- filteredData %>%
      group_by(Kebun) %>%
      summarise(Total_Jumlah = sum(Jumlah_Produksi)) %>%
      filter(Total_Jumlah == max(Total_Jumlah))
    
    valueBox(
      formatC(jumlahTerbanyak$Total_Jumlah, format = "d", big.mark = ","),
      paste("Kebun Terbanyak:", jumlahTerbanyak$Kebun),
      icon = icon("star", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$totalProduksiKebun <- renderValueBox({
    filteredData <- filteredDataKebun()
    totalProduksi <- filteredData %>%
      summarise(Total_Produksi = sum(Jumlah_Produksi))
    
    valueBox(
      formatC(totalProduksi$Total_Produksi, format = "d", big.mark = ","),
      "Total Produksi",
      icon = icon("stats", lib = "glyphicon"),
      color = "orange"
    )
  })
  
  output$pertumbuhanKebun <- renderValueBox({
    dataPertumbuhanKebun <- dkebun %>%
      filter(Provinsi == input$provinsiKebun) %>%
      pivot_longer(cols = colnames(dkebun)[3:length(colnames(dkebun))], names_to = "Kebun", values_to = "Jumlah_Produksi") %>%
      mutate(Kebun = reorder(Kebun, -Jumlah_Produksi))
    
    kebun2022 <- dataPertumbuhanKebun %>%
      filter(Tahun == 2022)
    totalProduksi2022 <- kebun2022 %>%
      summarise(Total_Produksi = sum(Jumlah_Produksi))
    
    kebun2021 <- dataPertumbuhanKebun %>%
      filter(Tahun == 2021)
    totalProduksi2021 <- kebun2021 %>%
      summarise(Total_Produksi = sum(Jumlah_Produksi))
    
    pertumbuhanKebun <- ((totalProduksi2022$Total_Produksi - totalProduksi2021$Total_Produksi) / totalProduksi2021$Total_Produksi) * 100
    
    valueBox(
      paste0(formatC(pertumbuhanKebun, digits = 3), "%"),
      "Pertumbuhan Total Produksi",
      icon = icon("chart-line", lib = "font-awesome"),
      color = "red"
    )
  })
  
  filteredDataTernak <- reactive({
    dTernak %>%
      filter(Provinsi == input$provinsiTernak) %>%
      filter(Tahun == input$tahunTernak) %>%
      pivot_longer(cols = colnames(dTernak)[3:length(colnames(dTernak))], names_to = "JenisTernak", values_to = "Populasi") %>%
      mutate(JenisTernak = reorder(JenisTernak, -Populasi))
  })
  
  output$plotTernak <- renderPlot({
    filteredData <- filteredDataTernak()
    ggplot(filteredData, aes(x = JenisTernak, y = Populasi, fill = JenisTernak)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Populasi), vjust = -0.5, size = 3.5, color = "black") +
      labs(x = "Jenis Ternak", y = "Populasi", title = paste("Populasi Ternak di", input$provinsiTernak, "Tahun", input$tahunTernak, "(Ton)")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
  })
  
  output$jumlahTernakTerbanyak <- renderValueBox({
    filteredData <- filteredDataTernak()
    jumlahTerbanyak <- filteredData %>%
      group_by(JenisTernak) %>%
      summarise(TotalPopulasi = sum(Populasi)) %>%
      filter(TotalPopulasi == max(TotalPopulasi))
    
    valueBox(
      formatC(jumlahTerbanyak$TotalPopulasi, format = "d", big.mark = ","),
      paste("Ternak Terbanyak:", jumlahTerbanyak$JenisTernak),
      icon = icon("star", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$totalPopulasiTernak <- renderValueBox({
    filteredData <- filteredDataTernak()
    totalPopulasi <- filteredData %>%
      summarise(TotalPopulasi = sum(Populasi))
    
    valueBox(
      formatC(totalPopulasi$TotalPopulasi, format = "d", big.mark = ","),
      "Total Populasi Ternak",
      icon = icon("stats", lib = "glyphicon"),
      color = "orange"
    )
  })
  
  output$pertumbuhanTernak <- renderValueBox({
    dataPertumbuhanTernak <- dTernak %>%
      filter(Provinsi == input$provinsiTernak) %>%
      pivot_longer(cols = colnames(dTernak)[3:length(colnames(dTernak))], names_to = "Ternak", values_to = "Populasi") %>%
      mutate(Ternak = reorder(Ternak, -Populasi))
    
    ternak2022 <- dataPertumbuhanTernak %>%
      filter(Tahun == 2022)
    totalProduksi2022 <- ternak2022 %>%
      summarise(Total_Produksi = sum(Populasi))
    
    ternak2021 <- dataPertumbuhanTernak %>%
      filter(Tahun == 2021)
    totalProduksi2021 <- ternak2021 %>%
      summarise(Total_Produksi = sum(Populasi))
    
    pertumbuhanTernak <- ((totalProduksi2022$Total_Produksi - totalProduksi2021$Total_Produksi) / totalProduksi2021$Total_Produksi) * 100
    
    valueBox(
      paste0(formatC(pertumbuhanTernak, digits = 3), "%"),
      "Pertumbuhan Total Produksi",
      icon = icon("chart-line", lib = "font-awesome"),
      color = "red"
    )
  })
  
  filteredDataUnggas <- reactive({
    dUnggas %>%
      filter(Provinsi == input$provinsiUnggas) %>%
      filter(Tahun == input$tahunUnggas) %>%
      pivot_longer(cols = colnames(dUnggas)[3:length(colnames(dUnggas))], names_to = "JenisUnggas", values_to = "Populasi") %>%
      mutate(JenisUnggas = reorder(JenisUnggas, -Populasi))
  })
  
  output$plotUnggas <- renderPlot({
    filteredData <- filteredDataUnggas()
    ggplot(filteredData, aes(x = JenisUnggas, y = Populasi, fill = JenisUnggas)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Populasi), vjust = -0.5, size = 3.5, color = "black") +
      labs(x = "Jenis Unggas", y = "Populasi", title = paste("Populasi Unggas di", input$provinsiUnggas, "Tahun", input$tahunUnggas, "(Ton)")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
  })
  
  output$jumlahUnggasTerbanyak <- renderValueBox({
    filteredData <- filteredDataUnggas()
    jumlahTerbanyak <- filteredData %>%
      group_by(JenisUnggas) %>%
      summarise(TotalPopulasi = sum(Populasi)) %>%
      filter(TotalPopulasi == max(TotalPopulasi))
    
    valueBox(
      formatC(jumlahTerbanyak$TotalPopulasi, format = "d", big.mark = ","),
      paste("Unggas Terbanyak:", jumlahTerbanyak$JenisUnggas),
      icon = icon("star", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$totalPopulasiUnggas <- renderValueBox({
    filteredData <- filteredDataUnggas()
    totalPopulasi <- filteredData %>%
      summarise(TotalPopulasi = sum(Populasi))
    
    valueBox(
      formatC(totalPopulasi$TotalPopulasi, format = "d", big.mark = ","),
      "Total Populasi Unggas",
      icon = icon("stats", lib = "glyphicon"),
      color = "orange"
    )
  })
  
  output$pertumbuhanUnggas <- renderValueBox({
    dataPertumbuhanUnggas <- dUnggas %>%
      filter(Provinsi == input$provinsiUnggas) %>%
      pivot_longer(cols = colnames(dUnggas)[3:length(colnames(dUnggas))], names_to = "Unggas", values_to = "Populasi") %>%
      mutate(Unggas = reorder(Unggas, -Populasi))
    
    unggas2022 <- dataPertumbuhanUnggas %>%
      filter(Tahun == 2022)
    totalProduksi2022 <- unggas2022 %>%
      summarise(Total_Produksi = sum(Populasi))
    
    unggas2021 <- dataPertumbuhanUnggas %>%
      filter(Tahun == 2021)
    totalProduksi2021 <- unggas2021 %>%
      summarise(Total_Produksi = sum(Populasi))
    
    pertumbuhanUnggas <- ((totalProduksi2022$Total_Produksi - totalProduksi2021$Total_Produksi) / totalProduksi2021$Total_Produksi) * 100
    
    valueBox(
      paste0(formatC(pertumbuhanUnggas, digits = 3), "%"),
      "Pertumbuhan Total Produksi",
      icon = icon("chart-line", lib = "font-awesome"),
      color = "red"
    )
  })
  
  observeEvent(input$kelompok, {
    jenis_choices <- unique(dPertumbuhan$Jenis[dPertumbuhan$Kelompok == input$kelompok])
    updateSelectInput(session, "jenis", choices = jenis_choices)
  })
  
  output$pertumbuhan <- renderPlot({
    filtered_data <- subset(dPertumbuhan, Jenis == input$jenis)
    plot_data <- data.frame(
      Tahun = c("2019", "2020", "2021", "2022"),
      JumlahPertumbuhan = c(filtered_data$`2019`, filtered_data$`2020`, filtered_data$`2021`, filtered_data$`2022`)
    )
    
    ggplot(plot_data, aes(x = Tahun, y = JumlahPertumbuhan)) +
      geom_line(aes(group = "Jenis"),color="orange") +
      geom_point() +
      geom_text(aes(label = JumlahPertumbuhan), vjust = -0.5, size = 3.5, color = "black") +
      labs(x = "Tahun", y = "JumlahPertumbuhan", title = paste("Pertumbuhan Jumlah Tumbuhan", input$jenis)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
  })
  
  output$rataPertumbuhan <- renderValueBox({
    fdata <- subset(dPertumbuhan, Jenis == input$jenis)
    as.numeric(fdata[,3:6])
    p20 <- (fdata[,4]-fdata[,3])/fdata[,3]
    p21 <- (fdata[,5]-fdata[,4])/fdata[,4]
    p22 <- (fdata[,6]-fdata[,5])/fdata[,5]
    
    pertumbuhan <- mean(p20,p21,p22)*100
    
    valueBox(
      paste0(formatC(pertumbuhan, digits = 3), "%"),
      "Rata-rata Pertumbuhan",
      icon = icon("chart-line", lib = "font-awesome"),
      color = "aqua"
    )
    
    
  })
  
  df1 <- reactive({
    if (input$komoditas1 == "Pangan") {
      return(dpangan)
    } else if (input$komoditas1 == "Hortikultura") {
      return(dhortikultura)
    } else if (input$komoditas1 == "Buah") {
      return(dbuah)
    } else {
      return(dkebun)
    }
  })
  
  df2 <- reactive({
    if(input$komoditas2 == "Ternak"){
      return(dTernak)
    } else if (input$komoditas2 == "Unggas"){
      return(dUnggas)
    }
  })
  
  
  output$Map1 <- renderLeaflet({
    dff <- df1()
    dff <- dff %>% filter(Tahun==2022)
    jenis_columns <- colnames(dff)[3:length(colnames(dff))]
    provinsi <- dff$Provinsi
    terbanyak <- character(length(provinsi))
    
    for (i in 1:length(provinsi)) {
      jenis_counts <- dff[i, jenis_columns]
      max_count <- max(jenis_counts)
      max_index <- which.max(jenis_counts)
      terbanyak[i] <- jenis_columns[max_index]
    }
    
    location <- location %>%
      mutate(Lat = as.numeric(gsub(",", ".", Lat)),
             Lng = as.numeric(gsub(",", ".", Lng)),
             terbanyak = terbanyak[-35])
    
    a <- leaflet() %>%
      addTiles() %>%
      addMarkers(data = location, lng = ~Lng, lat = ~Lat, group = "pnt") %>%
      addPopups(data = location, lng = ~Lng, lat = ~Lat,
                popup = ~paste("<center><b>", Provinsi, "</b></center>",
                               "<center>", terbanyak, "</center>"))
    return(a)
  })
  
  output$Map2 <- renderLeaflet({
    dff <- df2()
    dff <- dff %>% filter(Tahun==2022) 
    jenis_columns <- colnames(dff)[3:length(colnames(dff))]
    provinsi <- dff$Provinsi
    terbanyak <- character(length(provinsi))
    
    for (i in 1:length(provinsi)) {
      jenis_counts <- dff[i, jenis_columns]
      max_count <- max(jenis_counts)
      max_index <- which.max(jenis_counts)
      terbanyak[i] <- jenis_columns[max_index]
    }
    
    location <- location %>%
      mutate(Lat = as.numeric(gsub(",", ".", Lat)),
             Lng = as.numeric(gsub(",", ".", Lng)),
             terbanyak = terbanyak[-35])
    
    a <- leaflet() %>%
      addTiles() %>%
      addMarkers(data = location, lng = ~Lng, lat = ~Lat, group = "pnt") %>%
      addPopups(data = location, lng = ~Lng, lat = ~Lat,
                popup = ~paste("<center><b>", Provinsi, "</b></center>",
                               "<center>", terbanyak, "</center>"))
    return(a)
  })
  
  pick_df <- reactive({
    if (input$dataframe == "Pangan") {
      return(dpangan)
    } else if (input$dataframe == "Hortikultura") {
      return(dhortikultura)
    } else if (input$dataframe == "Buah") {
      return(dbuah)
    } else if (input$dataframe == "Perkebunan") {
      return(dkebun)
    } else if (input$dataframe == "Ternak"){
      return(dTernak)
    } else if (input$dataframe == "Unggas"){
      return(dUnggas)
    } else {
      return(dPertumbuhan)
    }
  })
  
  output$table_data <- renderDT({ pick_df() })
  output$summary_stat <- renderPrint({ dfSummary(pick_df()) })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("data",input$dataframe,".csv")  # Nama file yang akan diunduh
    },
    content = function(file) {
      # Logika untuk memilih atau memanipulasi dataframe yang akan diunduh
      data <- pick_df()  # Memanggil fungsi pick_df() untuk mendapatkan dataframe yang akan diunduh
      write.csv(data, file, row.names = FALSE)  # Menulis dataframe ke file CSV
    }
  )
  
  output[["foto1"]]<-renderImage({
    list(src = "danis.png", height = 200, width = 180)
  }, deleteFile = FALSE)
  
  output[["foto2"]]<-renderImage({
    list(src = "wily.png", height = 200, width = 180)
  }, deleteFile = FALSE)
  
}

ui <- dashboardPage(Header, Sidebar, Body)

shinyApp(ui, server)
