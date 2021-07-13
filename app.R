library(shinydashboard)
library(shiny)
library(shinydashboard)
library(summarytools)
library(readxl)
library(DT)
library(plotly)
library(ggplot2)
library(rhandsontable)
library(datasets)
library(rgdal)
library(maptools)
library(rgeos)
library(leaflet)
library(ggplot2)
library(gganimate)
library(gifski)
library(RColorBrewer)
library(dplyr)
library(shinyWidgets)
library(png)
df <- read.csv("df_33 (1).csv")
str(df)
df = na.omit(df)
df = within(df,{
    prop_starrating= as.factor(prop_starrating)
    date_time= as.POSIXct(date_time, format = "%d/%m/%Y")
    prop_brand_bool=as.factor(prop_brand_bool)
    srch_length_of_stay=as.factor(srch_length_of_stay)
    promotion_flag=as.factor(promotion_flag)
    srch_saturday_night_bool=as.factor(srch_saturday_night_bool)
    srch_adults_count=as.factor(srch_adults_count)
    srch_children_count=as.factor(srch_children_count)
    srch_room_count=as.factor(srch_room_count)
    random_bool=as.factor(random_bool)
    click_bool=as.factor(click_bool)
    booking_bool=as.factor(booking_bool)
    
})


ui <- dashboardPage(
    dashboardHeader(title = "Elia Site"),
    dashboardSidebar(sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("home", lib = "glyphicon")),
        menuItem("Get To Know ", tabName = "pendahuluan", icon = icon("asterisk", lib = "glyphicon")),
        menuItem("Dataset", tabName = "dataset", icon = icon("eye-open", lib = "glyphicon")),
        menuItem("Fasilitas Hotel", tabName = "fasilitas", icon = icon("home", lib = "glyphicon")),
        menuItem("Maps", icon = icon("eye-open", lib = "glyphicon"), tabName = "widgets",
                 badgeLabel = "Exclusive", badgeColor = "green"),
        menuItem("Tentang Penulis", tabName = "penulis", icon = icon("education", lib = "glyphicon"))
    )),
    dashboardBody(
        tabItems(tabItem(
            tabName = "dashboard",
            fluidRow(
                infoBox(
                    "Temukan ", paste0(length(df$X1), " Hotel"), icon = icon("home", lib = "glyphicon"),
                    color = "purple"
                ),
                infoBox(
                    "Tersebar di ", paste0(length(unique(df$province))," Provinsi"), icon = icon("road", lib = "glyphicon"),
                    color = "yellow"
                ),
                infoBox(
                    "Rantang Harga ", paste0(min(df$price_usd), " hingga ", max(df$price_usd)," USD"), icon = icon("usd", lib = "glyphicon"),
                    color = "red"
                )
            ),
            fluidRow(
                tabBox(width = 12,
                       tabPanel("Histogram", p(style="text-align: justify;",'Grafik Histogram berguna untuk mengetahui persebaran data antara variabel numerik berdasarkan kategori kategori yang tersedia')),
                       tabPanel("Bubble Chart", p(style="text-align: justify;",'Grafik Bubble Chart berguna untuk mengetahui perbedaan antara hotel dengan lokasi strategis berdasarkan rating dan harganya')),
                       tabPanel("Line Chart", p(style="text-align: justify;",'Grafik Line Chart berguna untuk mengetahui perkembangan harga hotel pada tahun 2012-2013 berdasarkan rating')),
                       tabPanel("Bar Plot", p(style="text-align: justify;",'Grafik Bar Plot berguna untuk mengetahui kota mana yang paling sering dikunjungi'))
                       
                       
                )),
            fluidRow(
                box(
                    width =6, title = "Histogram", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotlyOutput("graph_3", height = 400)
                ),
                box(title = "Kustomisasi Histogram-mu", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, height = 400,width = 6,
                    selectInput("x1", "Variabel",
                                choices = c("Skor Posisi Strategis"="prop_location_score1",
                                            "Harga Historical" ="prop_log_historical_price",
                                            "Posisi "= "position",
                                            "Harga" = "price_usd"
                                )),
                    radioButtons('urutan','Sorting',
                                 choices = c('Tertinggi',"Terendah"),
                                 selected = 'Tertinggi'),
                    sliderInput('slider3','Jumlah Observasi yang Ditampilkan',
                                min = 5, max = 900, value = 10, step = 10),
                    selectInput('x2',"Warna",
                                choices = c("Hotel memiliki cabang"="prop_brand_bool",
                                            "Lama Menginap"="srch_length_of_stay",
                                            "Promosi"="promotion_flag",
                                            "Hiburan Malam"="srch_saturday_night_bool",
                                            "Jumlah dewasa"="srch_adults_count",
                                            "Jumlah anak-anak"="srch_children_count",
                                            "Jumlah kamar"="srch_room_count",
                                            "Booking/Tidak"="booking_bool "
                                ))
                    
                ))
            ,
            fluidRow(
                box(
                    width = 7, title = "Bubble Chart", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    imageOutput("plot1", height = 500)
                ),
                box(
                    title = "Untuk apa bubble chart ini?", width = 5, height = 500, background = "light-blue",
                    p(style="text-align: justify;",br("Bubble chart ini menggambarkan hubungan antara 3 variabel yaitu, letak lokasi strategis, rating, dan harga."),tags$ul(
                        tags$li("+ Semakin strategis letaknya, maka bulatan akan semakin membesar."),
                        tags$li("+ Semakin tinggi ratingnya, maka bulatan akan memiliki warna yang berbeda.")),
                      
                      br("Kesimpulan yang dapat kita ambil dari plot ini yaitu :"),tags$ol(
                          tags$li(style="text-align: justify;","Semakin strategis lokasi hotel tersebut, dan rating yang semakin tinggi maka harganya akan semakin naik."),
                          tags$li(style="text-align: justify;","Lihat bulatan warna biru yaitu Rating 4 yang pergerakannya akan menjadi lebih tinggi dibanding bulatan berwarna hijau yaitu Rating 3."),
                          tags$li(style="text-align: justify;","Selain itu bulatan warna pink yaitu Rating 5 yang cenderung berada di sisi kanan yang artinya harganya paling mahal.")))
                    
                    
                    
                )),
            fluidRow(
                box(
                    title = "Untuk apa line chart ini?", width = 5, height = 500, background = "light-blue",
                    p(style="text-align: justify;","Line chart ini menggambarkan hubungan antara 3 variabel, yaitu harga, rating, dan perkembangannya dari tahun ke tahun.",
                      br("Variabel x menggambarkan tahun pencarian hotel dimana dibagi menjadi 5 axis yaitu :"),tags$ol(
                          tags$li("2012-2012.25 yaitu 2012 kuartal pertama"),
                          tags$li("2012.25-2012.50 yaitu 2012 kuartal kedua"),
                          tags$li("2012.5-2012.75 yaitu 2012 kuartal ketiga"),
                          tags$li("2012.75-2013 yaitu 2012 kuartal keempat")),
                      
                      br("Berdasarkan line chart ini, kesimpulan yang dapat kita ambil adalah :"),
                      p(style="text-align: justify;",br("1. Dari bulan ke bulan, harga hotel cenderung naik dan harga paling tinggi didominasi warna pink yaitu (Rating 5) dan paling
rendah didominasi warna kuning yaitu (Rating 2) yang menandakan bahwa terdapat hubungan antara harga, rating, dan tahun.")))  
                ),
                box(
                    width = 7, title = "Line Chart", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    imageOutput("plot2", height = 500))
                
            ),
            fluidRow(
                box(
                    width = 12, title = "Bar Chart", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    sliderInput('slider4','Top City to Visit',
                                min = 1, max = 9, value = 9, step = 1),
                    plotlyOutput("graph_4", height = 300)))
            
            
            
            
        ),
        tabItem(tabName = "fasilitas",
                fluidRow(box(
                    selectInput("PROVINCE", "Provinsi",
                                choices = unique(df$province)
                    )),
                    box(
                        selectInput("CITY", "Kota",
                                    choices = NULL
                        )
                    ),
                    box(width = 12,
                        selectInput("NAME", "Nama Hotel",
                                    choices = NULL
                        )
                    )
                ),
                fluidRow(box(
                    title = "Hotel Pilihanmu", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput(outputId = "pnghotel")
                ),
                box(
                    title = "Fasilitas Utama", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput(outputId = "pngfasil")
                )),
                fluidRow(box(
                    title = "Kamar Mewah", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput(outputId = "pngkamar")
                ),
                box(
                    title = "Kamar Mandi Nyaman", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput(outputId = "pngkm")
                )
                )
                
        ),
        tabItem(tabName = "widgets",
                fluidRow(
                    box(sliderInput("range", "Price", min(df$price_usd), max(df$price_usd),
                                    value = range(df$price_usd), step = 10
                    )),
                    box(selectInput("var_g2_numerik", "Variabel Metric",
                                    choices = c("Skor Posisi Strategis"="prop_location_score1",
                                                "Harga Historical" ="prop_log_historical_price",
                                                "Posisi "= "position",
                                                "Harga" = "price_usd"
                                    )))),
                fluidRow(
                    box(pickerInput("rating", "Rating",
                                    choices = c(0,1,2,3,4,5),
                                    selected = 3,multiple = TRUE
                    )),
                    
                    box(pickerInput("anak", "Jumlah Anak",
                                    choices = c(0,1,2),
                                    selected = 1,multiple = TRUE
                    ))),
                fluidRow(
                    box(pickerInput("dewasa", "Jumlah Dewasa",
                                    choices = c(1,2,4,6),
                                    selected = 1,multiple = TRUE
                    )),
                    infoBox(
                        "Silahkan Zoom Peta ", "Lihat Lebih Dekat", icon = icon("road", lib = "glyphicon"),
                        color = "yellow"
                    )
                    
                    
                ),
                fluidRow(
                    leafletOutput("mymap", height = 800)
                )
        ),
        tabItem(tabName = "penulis",
                fluidRow(box(
                    title = "Penulis 1", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput(outputId = "png")
                ),
                box(
                    title = "Penulis 2", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput(outputId = "png2")
                )),
                fluidRow(box(
                    width = 6, background = "light-blue",
                    p("Aulia Kharis Rakhmasari",br("06211940000073"))
                ),
                box(
                    width = 6, background = "light-blue",
                    p("Eva Marella R",br("06211840000046"))
                )
                ))
        ,
        tabItem(tabName = "dataset",
                tabsetPanel(
                    tabPanel("DataFrame",
                             fluidRow(
                                 box(
                                     sliderInput("sliderrr", "Harga", min(df$price_usd), max(df$price_usd),
                                                 value = range(df$price_usd), step = 10
                                     )),
                                 box(
                                     pickerInput("ratingtab", "Rating",
                                                 choices = c(1,2,3,4,5), selected = 3,multiple = TRUE
                                     ))),
                             dataTableOutput("data_table")),
                    tabPanel("Summary1",verbatimTextOutput("summary_stat")),
                    tabPanel("Summary2",verbatimTextOutput("summary_stat1"))
                )),
        tabItem(tabName = "pendahuluan",
                fluidRow(
                    tabBox(width = 12,
                           selected = "Tab3",
                           tabPanel("Latar Belakang", p(style="text-align: justify;",
                                                        br("Pariwisata sebagai fenomena mendunia , telah menjadi suatu kebutuhan dasar yang melibatkan ratusan juta manusia. Akomodasi wisata merupakan hal penting dalam memenuhi kebutuhan wisatawan yang sedang berwisata. Para wisatawan cenderung membutuhkan akomodasi yang memiliki beragam varian harga maupun macamnya.Salah satu hal krusial yang harus dipersiapkan para wisatawan adalah tempat dimana nantinya wisatawan akan beristirahat. Salah satu dari pilihan wisatawan yang lumrah digunakan adalah Hotel."), 
                                                        
                                                        br("Hotel merupakan suatu jenis akomodasi yang mempergunakan sebagian atau seluruh bangunan untuk menyediakan jasa penginapan, makanan, dan minum, serta jasa penunjang lainnya bagi umum yang dikelola secara komersial. Di Indonesia sendiri, terdapat banyak sekali ragam hotel yang memiliki variasi yang berbeda-beda mulai dari rating, fasilitas, pelayanan, dan lain lain. Wisatawan kerap mengalami permasalahan dalam pemesanan hotel, diantaranya seperti harga yang cenderung fluktuatif disaat saat tertentu, fasilitas yang tidak sepadan dengan harga yang telah dibayarkan, hingga pelayanan yang kurang memuaskan."), 
                                                        
                                                        br("Berdasarkan permasalahan tersebut, Penulis berinovasi melalui dashboard berjudul 'Sistem Informasi Pencarian Hotel' yang akan memudahkan pengguna dalam menentukan hotel yang sesuai dengan keinginan mereka. Dashboard ini akan menyajikan visualisasi berupa fasilitas yang terdapat dalam hotel tersebut, rating yang diterima, banyaknya kamar yang tersedia, lokasi, hingga seberapa baik pelayanan dalam hotel tersebut. Sehingga melalui adanya dashboard ini, diharapkan para wisatawan nantinya akan lebih menikmati kegiatan pariwisata yang berlangsung."
                                                        ))),
                           tabPanel("Tujuan", p(style="text-align: justify;",br("1. Merancang dashboard dengan tujuan mengetahui profil hotel sebelum melakukan pemesanan, seperti rating, review, biaya, serta fasilitas."),
                                                br("2. Serta Memonitor kondisi dan mengetahui pembaharuan masing-masing hotel."))),
                           tabPanel("Manfaat", p(style="text-align: justify;",br("1. Sebagai dasar pengambilan keputusan bagi calon penyewa Hotel."),
                                                 br("2. Sebagai media informasi yang dapat menyajikan informasi secara efisien kepada masyarakat untuk mengetahui hotel yang layak dihuni."), 
                                                 br("3. Sebagai media monitoring untuk dapat memantau progress atau perkembangan dari suatu hotel"),
                                                 br("4. Sebagai media untuk menganalisis kesuksesan suatu Hotel dengan adanya profil masing-masing Hotel."),   
                                                 br("5. Sebagai dasar untuk pembaharuan informasi terkini Hotel.")
                                                 
                           ))
                    )
                )
        )
        ))
    
)
server <- function(input, output) {
    output$data_table=renderDataTable({
        df3 = df[,c(31,27,8,13,19,18,20,16)]
        df3 = df3 %>%
            rename(
                Nama = name,
                Kota = city,
                Rating = prop_starrating,
                harga = price_usd,
                Anak = srch_children_count,
                Dewasa = srch_adults_count,
                Jumlah.Kamar = srch_room_count, 
                Lama.Menginap = srch_length_of_stay
            )
        df4 = df3 %>%
            filter(harga >= input$sliderrr[1] & harga <= input$sliderrr[2] & Rating %in% input$ratingtab)
        df4
    },options= list(scrollx=TRUE)
    )
    
    output$summary_stat=renderPrint(descr({
        df3 = df[,c(31,27,8,13,19,18,20,16)]
        df3 = df3 %>%
            rename(
                Nama = name,
                Kota = city,
                Rating = prop_starrating,
                harga = price_usd,
                Anak = srch_children_count,
                Dewasa = srch_adults_count,
                Jumlah.Kamar = srch_room_count, 
                Lama.Menginap = srch_length_of_stay
            )
        df4 = df3 %>%
            filter(harga >= input$sliderrr[1] & harga <= input$sliderrr[2] & Rating %in% input$ratingtab)
        df4
    },stats = "common"))
    
    output$summary_stat1=renderPrint(dfSummary({
        df3 = df[,c(31,27,8,13,19,18,20,16)]
        df3 = df3 %>%
            rename(
                Nama = name,
                Kota = city,
                Rating = prop_starrating,
                harga = price_usd,
                Anak = srch_children_count,
                Dewasa = srch_adults_count,
                Jumlah.Kamar = srch_room_count, 
                Lama.Menginap = srch_length_of_stay
            )
        df4 = df3 %>%
            filter(harga >= input$sliderrr[1] & harga <= input$sliderrr[2] & Rating %in% input$ratingtab)
        df4}))
    
    output$graph_3=renderPlotly({
        obs.num=input$slider3
        varx1=df[,input$x1]
        if (input$urutan=='Tertinggi') {
            sortdata=df[order(-varx1),]
        } else  {
            sortdata=df[order(varx1),]
        }
        sortdata=sortdata[1:obs.num,]
        varx1=sortdata[,input$x1]
        varx2=sortdata[,input$x2]
        if (input$urutan=='Tertinggi') {
            graph3=ggplot(data=sortdata, aes(x=varx1, 
                                             fill=varx2))+
                geom_histogram(bins = input$bin)+
                labs(y='',x=input$x1)+theme_minimal()+scale_fill_brewer(palette="Blues")
        } else  {
            graph3=ggplot(data=sortdata, aes(x=varx1, 
                                             fill=varx2))+
                geom_histogram(bins = input$bin)+
                labs(y='',x=input$x1)+theme_minimal()+scale_fill_brewer(palette="Blues")
        }
        graph3= ggplotly(graph3) %>% layout(legend = list(orientation = "h", x = 0.4, y = 1.1, 
                                                          title=list(text=input$x2)))
        graph3
    })
    
    output$plot1 <- renderImage({
        df$date_time= as.POSIXct(df$date_time, format = "%d/%m/%Y")
        df = na.omit(df)
        df$date_time= as.POSIXct(df$date_time, format = "%d/%m/%Y")
        df$Year = format(df$date_time, format="%Y")
        df$Year = as.numeric(df$Year)
        
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext='.gif')
        
        # now make the animation
        p = ggplot(df, aes(price_usd, position, size =prop_location_score1, color =prop_starrating)) +
            geom_point() +
            scale_x_log10() +
            theme_bw() +
            # gganimate specific bits:
            labs(title = 'Year: {frame_time}', x = 'Price', y = 'Location',size = "Lokasi",color="Rating") +
            transition_time(Year) +
            ease_aes('linear') # New
        
        anim_save("outfile.gif", animate(p)) # New
        
        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )},deleteFile = TRUE)
    output$plot2 <- renderImage({
        df$date_time= as.POSIXct(df$date_time, format = "%d/%m/%Y")
        df = na.omit(df)
        df$date_time= as.POSIXct(df$date_time, format = "%d/%m/%Y")
        df$Year = format(df$date_time, format="%Y")
        df$Year = as.numeric(df$Year)
        
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext='.gif')
        
        # now make the animation
        p =   ggplot(df, aes(x=Year, y=price_usd, group=prop_starrating, color=prop_starrating)) +
            geom_line() +
            geom_point() +
            ggtitle("Popularity of Hotels") +
            ylab("Price") +
            theme_bw() +
            labs(color = "Rating")+
            transition_reveal(Year)
        
        anim_save("outfile.gif", animate(p)) # New
        
        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )},deleteFile = TRUE)
    output$mymap <- renderLeaflet({
        df = df %>%
            filter(price_usd >= input$range[1] & price_usd <= input$range[2] & prop_starrating %in% input$rating & srch_adults_count%in%input$dewasa & srch_children_count %in% input$anak)
        var=df[,input$var_g2_numerik]
        str(df)
        mypalette <- colorBin( palette="viridis", domain=var, bins=10)
        
        mytext <- paste(
            "Price: ", df$price_usd, "<br/>", 
            "Star : ", df$prop_starrating, "<br/>", 
            "Name : ",df$name, sep="") %>%
            lapply(htmltools::HTML)
        m <- leaflet(df) %>% 
            addTiles()  %>% 
            setView( lat=38.13, lng=-87.32 , zoom=4) %>%
            addProviderTiles("OpenStreetMap.Mapnik") %>%
            addCircleMarkers(~lon, ~lat, 
                             fillColor = ~mypalette(var), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                             label = mytext,clusterOptions = markerClusterOptions(),
                             labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
            ) %>%
            addLegend( pal=mypalette, values=~var, opacity=0.9, title = input$var_g2_numerik, position = "bottomright" )
        m
    })
    output$graph_4 = renderPlotly({
        obs.num=input$slider4
        df2 = as.data.frame(xtabs(~df$city+df$site_id))
        varx1 = df2[,"Freq"]
        df2=df2[order(-varx1),]
        df2 = df2[1:obs.num,]
        varx1 = df2[,"Freq"]
        blues <- colorRampPalette(c('dark blue', 'light blue'))
        graph4=ggplot(data=df2, aes(x=reorder(df.city,varx1),y=reorder(df.city,varx1), fill = df.city))+
            geom_bar(stat="identity")+
            labs(x='city',y="Kota Paling Sering Dikunjungi")+theme_minimal()+
            scale_fill_brewer(palette="Blues")+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank())+
            geom_point( size=1, color="blue") +
            coord_flip()+
            theme(legend.position = "none")
        graph4
    })
    output$png <- renderPlot({
        pic = readPNG('Aulia.png')
        plot.new()
        grid::grid.raster(pic)
        
    })
    output$png2 <- renderPlot({
        pic = readPNG('eva.PNG')
        plot.new()
        grid::grid.raster(pic)
        
    })
    
    prov <- reactive({
        filter(df, province == input$PROVINCE)
    })
    observeEvent(prov(), {
        choices <- unique(prov()$city)
        updateSelectInput(inputId = "CITY", choices = choices) 
    })
    
    cit <- reactive({
        req(input$CITY)
        filter(prov(), city == input$CITY)
    })
    observeEvent(cit(), {
        choices <- unique(cit()$name)
        updateSelectInput(inputId = "NAME", choices = choices)
    })
    
    output$pnghotel <- renderPlot({
        req(input$NAME)
        pic = readPNG(df[df$name == input$NAME,35])
        plot.new()
        grid::grid.raster(pic)
        
    })
    
    output$pngfasil <- renderPlot({
        req(input$NAME)
        pic = readPNG(df[df$name == input$NAME,36])
        plot.new()
        grid::grid.raster(pic)
        
    })
    
    output$pngkamar <- renderPlot({
        req(input$NAME)
        pic = readPNG(df[df$name == input$NAME,37])
        plot.new()
        grid::grid.raster(pic)
        
    })
    
    output$pngkm <- renderPlot({
        req(input$NAME)
        pic = readPNG(df[df$name == input$NAME,38])
        plot.new()
        grid::grid.raster(pic)
        
    })
    
    
    
    
    
    
    
    
}



shinyApp(ui, server)