library(shiny)           # Etkilesimli web uygulamalari gelistirmek icin kullanilir
library(bslib)           # Shiny uygulamalari icin tema ozellestirme saglar
library(ggplot2)         # Grafik ve veri gorsellestirme icin kullanilir
library(dplyr)           # Veri manip??lasyonu ve filtreleme islemleri icin kullanilir
library(tidytext)        # Metin madenciligi islemleri icin kullanilir
library(httr)            # HTTP istekleri yaparak API baglantisi kurmak icin kullanilir
library(jsonlite)        # JSON veri formatini okumak ve yazmak icin kullanilir
library(stringr)         # Metin verileri uzerinde string islemleri icin kullanilir
library(DT)              # Etkilesimli veri tablolarini gorsellemek icin kullanilir
library(readr)           # Veri dosyalarini (CSV vb.) okumak icin kullanilir
library(wordcloud2)      # Kelime bulutu olusturmak icin kullanilir
library(igraph)          # Ag (network) yapilarini modellemek ve analiz etmek icin kullanilir
library(ggraph)          # Ag verilerini gorsellestirmek icin kullanilir
library(shinycssloaders) # Y??kleniyor animasyonu eklemek icin kullanilir
library(tidyr)           # Verileri yeniden sekillendirmek ve temizlemek icin kullanilir
library(leaflet)         # Harita bazli gorsellestirme yapmak icin kullanilir
library(ggwordcloud)     # Gelismis kelime bulutlari olusturmak icin kullanilir
library(visNetwork)      # Etkilesimli ag grafikleri olusturmak icin kullanilir

Sys.setlocale("LC_ALL", "Turkish")

# Turkce Stopwords
turkce_stopwords <- c("acaba","ama","aslinda","az","bazi","belki","ben","biri","birkac","bircok","bu","cunku","da","mal","a","icin","ilk","hic","buna","geliyor","olarak",
                      "daha","zaman","olan","ediyorum","de","defa","diye","en","gibi","hem","hep","hepsi","her","hic","icin","ile","ise","ay","yerine","sizin","nin","butun","buyuk","vs","asla","anda",
                      "kez","ki","kim","bile","yapan","demek","bunlara","kendileri","mi","mu","mu","mi","ne","neden","nerde","nerede","nereye","gidiyor","geliyor","etkisi","bununla","kalkan","etmek","evet","gerekiyor",
                      "nicin","niye","o","sanki","sey","siz","kesinlikle","hala","geldi","yapar","su","tum","ve","veya","ya","yani","yine","yeni","hatta","dan","asla","olamaz","yer","fazla","baya","yerde",
                      "yok","zaten","cok","nasil","simdi","bana","bende","beni","boyle","boyu","sana","sizi","benim","biz","bizim","bize","bizde","tekrar","gerek","veriyor","net","olursa","gelecek",
                      "bizi","bunun","bunu","diger","cok","oldu","ah","a","olsun","tey","be","kat","eger","elbette","et","fakat","falan","biraz","kendini","burada","malesef","ahhh","ahh",
                      "filan","haliyle","halen","hangi","hani","ver","olsa","olmaz","an","edin","herkes","illa","iste","ic","kadar","karsin","herkese","ediyor","bak","bulur","hale","gitti",
                      "kendi","kimse","onu","nasil","olabilir","madem","meger","nedenle","oluyor","yerini","diyor","ona","sadece","sayet","geliyor","gecmis",
                      "v.s.","vs.","tek","olur","ye","veren","varsa","birde","yapma","yapmak","yoksa","artik","uzere","digeri","kendisi","verilmeli","gerek",
                      "kimseye","gelsin","ederim","dedi","onlar","bizler","sizler","bunlar","sunlar","birisi","birileri","herhangi","sekilde","gereken",
                      "tarafta","bence","hemen","tarafindan","bir","iki","uc","dort","bes","alti","yedi","sekiz","dokuz","on","once","yapanlar","degil",
                      "sonra","diliyorum","size","bi","ancak","arada","etrafinda","beri","yakininda","ustunde","altinda","icinde","yada","etsin","istiyoruz",
                      "disinda","karsi","karsisinda","var","ah","sen","seni","den","gelince","onunde","arkasinda","benzer","yaklasik","devam","dilerim",
                      "uzerine","uzerinde","dolayi","birlikte","belli","olacak","olarak","gerek","onun","etsin","istiyorum","olmayan","bulsun","senin"
)

yorumlar_deprem <- read_csv("depremyorum.csv", locale = locale(encoding = "UTF-8"))
yorumlar_asgari_ucret <- read_csv("asgeriucret.csv", locale = locale(encoding = "UTF-8"))
yorumlar_ahmet_minguzzi <- read_csv("ahmetminguzziyorumlari.csv", locale = locale(encoding = "UTF-8"))
yorumlar_iklim_degisikligi <- read_csv("iklimdegisikligi.csv", locale = locale(encoding = "UTF-8"))

duygu_verisi_ahmet <- read_csv("yorumlar_duygu.csv", locale = locale(encoding = "UTF-8"))
duygu_verisi_deprem <- read_csv("yorumlardeprem_duygu.csv", locale = locale(encoding = "UTF-8"))
duygu_verisi_asgari <- read_csv("yorumlarasgari_duygu.csv", locale = locale(encoding = "UTF-8"))
duygu_verisi_iklim <- read_csv("yorumlariklim_duygu.csv", locale = locale(encoding = "UTF-8"))



duygu_sozlugu <- tibble(
  kelime = c("harika", "mukemmel", "seviyorum", "guzel", "begendim", 
             "katil", "berbat", "nefret", "rezalet", "cirkin", 
             "asla", "tiksindim", "sevdim", "super", "bayildim", "hic",
             "caniligi", "acimasizlik", "canavarca", "vahsice", 
             "seytan", "vicdansiz","katil","asagilik","pislik","katil","kati","vahsice","yikim", "ihmal", "enkaz", "????kt??", 
             "felaket","olum", "korkunc", "unutuldu", "caresiz", "sessizlik","korku","deprem","adaletsiz", "d??????k", "gecim", "kira", "enflasyon"),
  duygu = c("olumlu", "olumlu", "olumlu", "olumlu", "olumlu", 
            "olumsuz", "olumsuz", "olumsuz", "olumsuz", "olumsuz", 
            "olumsuz", "olumsuz", "olumlu", "olumlu", "olumlu", "olumsuz",
            "olumsuz", "olumsuz", "olumsuz", "olumsuz", "olumsuz", "olumsuz","olumsuz","olumsuz","olumsuz","olumsuz","olumsuz","olumsuz","olumsuz", 
            "olumsuz", "olumsuz", "olumsuz", "olumsuz","olumsuz", "olumsuz", "olumsuz", "olumsuz", "olumsuz","olumsuz","olumsuz","olumsuz", "olumsuz", "olumsuz","olumsuz","olumsuz")
)

deprem_haberleri <- data.frame(
  sehir = c("Istanbul"),
  tarih = c("2025-04-23"),
  baslik = c("Istanbul'da deprem"),
  icerik = c("Istanbul'da hissedilen deprem kisa s??rd??...")
)

# Sehir koordinatlar?? 
sehir_koord <- data.frame(
  sehir = c("Istanbul"),
  lat = c(40.8327),
  lng = c(28.2267)
)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,#bootstrap5 temasi
    bootswatch = "minty",
    primary = "#2C3E50",
    secondary = "#18BC9C",
    success = "#18BC9C",
    font_scale = 0.9 #yazi olcegi
  ),
  
  titlePanel( #sectigim ikon resmi ve baslik
    div(
      img(src = "https://cdn-icons-png.flaticon.com/512/1534/1534959.png", height = 50),
      span("Sosyal Analiz ", style = "color: #2C3E50; margin-left: 15px"),
      style = "display: flex; align-items: center;"
    )
  ),
  
  tags$style(HTML(" #sayfa maks 1400px siniri
    .container-fluid {
      max-width: 1400px;
      margin: auto;
    }
  ")),
  
  fluidRow( #sol kisimda olacaklar
    column(
      width = 3,
      # Sidebar icerigi
      selectInput(
        "haber_sec",
        label = div(icon("newspaper"), " Haber Secimi"),
        choices = c("Ahmet Minguzzi", "Deprem", "Asgari Ucret", "Iklim Degisikligi"),
        selected = "Ahmet Minguzzi"
      ),
      hr(),
      card( #hava durum karti
        card_header(div(icon("cloud-sun"), " Hava Durumu", style = "color: #2C3E50;")),
        selectInput("sehir_sec", "Sehir Secin", 
                    choices = c("Bilecik", "Ankara", "Izmir", "Bursa", "Istanbul"),
                    selected = "Bilecik"),
        uiOutput("hava_durumu_bilgisi"),
        card_footer(textOutput("hava_guncelleme"))
      ),
      hr(),
      div( #analiz bilgi basligi
        icon("info-circle"), 
        strong("Analiz Bilgisi:"),
        textOutput("haberOzet", inline = TRUE),
        style = "margin-top:10px; color: #18BC9C;"
      ),
      hr(),
      textAreaInput("kullaniciYorum", "Analiz Hakkindaki Dusunceleriniz",
                    placeholder = "Gorusleriniz bizim icin degerli...")
    ),
    
    column(
      width = 9,
      # Ana icerik
      conditionalPanel(
        condition = "input.haber_sec == 'Deprem'",
        h4("Harita Tabanli Deprem Haber Takibi"),
        leafletOutput("depremHarita", height = "500px"),
        br(),
        uiOutput("secilenSehir"),
        dataTableOutput("haberlerTablosu"),
        hr()
      ),
      
      navset_card_tab(
        nav_panel(
          title = div(icon("cloud"), "Kelime Bulutlari"),
          card(
            card_header("Emoji Kelime Bulutu"),
            card_body(
              plotOutput("emojiKelimeBulutu", height = "300px"),
              style = "padding: 10px;"  # ??ste??e ba??l??
            )
          ),
          card(
            full_screen = TRUE,
            card_header("Ozel Sekilli Bulut"),
            wordcloud2Output("kelimeBulutuDiamond", height = "400px")
          )
        ),
        nav_panel(
          title = div(icon("smile"), "Emoji Analiz"),
          card(
            full_screen = TRUE,
            plotOutput("emojiGrafik", height = "600px") %>%
              shinycssloaders::withSpinner(type = 6, color = "#18BC9C")
          )
        ),
        nav_panel(
          title = div(icon("heart"), "Duygu Analizi"),
          card(
            full_screen = TRUE,
            card_header("Bert ile Duygu Dagilimi"),
            plotOutput("duyguGrafik", height = "500px") %>% 
              shinycssloaders::withSpinner(type = 6, color = "#18BC9C")
          ),
          card(
            full_screen = TRUE,
            card_header("Kelimelerle Duygu Analizi"),
            plotOutput("trigramDuyguGrafik", height = "500px") %>% 
              shinycssloaders::withSpinner(type = 6, color = "#18BC9C")
          )
        ),
        nav_panel(
          title = div(icon("project-diagram"), "Kelime Iliskileri"),
          card(
            full_screen = TRUE,
            card_header("Bigram Dagilimi 2'li"),
            plotOutput("bigramPlot", height = "500px")
          ),
          card(
            full_screen = TRUE,
            card_header("Bigram Dagilimi 3'lu"),
            plotOutput("bigramPlott", height = "600px"),
            style = "background-color: #f5f5f5;"
          )
        ),
        nav_panel(
          title = div(icon("project-diagram"), "Trigram Agi"),
          card(
            full_screen = TRUE,
            plotOutput("agGrafik", height = "600px") %>%
              shinycssloaders::withSpinner(type = 6, color = "#18BC9C")
          )
        ),
        nav_panel(
          title = div(icon("file-signature"), "Analiz Sonuclari"),
          card(
            full_screen = TRUE,
            h3("Analiz Sonuclari"),
            verbatimTextOutput("haber_analiz_sonucu")
          )
        ),
        
        
        
        tabPanel("Duygu Anketi",
                 h5("Bu haber size nasil hissettirdi?"),
                 actionButton("mutlu", "Mutlu"),
                 actionButton("uzgun", "Uzgun"),
                 actionButton("kizgin", "Kizgin"),
                 actionButton("notr", "Notr"),
                 br(), br(),
                 plotOutput("duyguPlot", height = "400px")
        ),
        nav_panel(
          title = div(icon("info-circle"), "Bilgilendirme"),
          card(
            card_header(" Proje Hakkinda"),
            card_body(
              HTML("
      <h4> Proje Hakkinda</h4>
      <p>Bu uygulama sosyal medya yorum analizi icin gelistirilmistir.</p>
      <ul>
        <li>Gercek zamanli veri gorsellestirme</li>
        <li>Dinamik filtreleme</li>
        <li>Coklu gorsellestirme teknikleri</li>
      </ul>
      <p><strong>Veri Kaynaklari:</strong> Youtube API</p>
    ")
            ),
            card_footer("Guncelleme: 2025-05-20")
          )
        )
      )
    )
  )
)

# Server

server <- function(input, output) {
  veri_sec <- reactive({
    switch(input$haber_sec,
           "Deprem" = yorumlar_deprem,
           "Asgari Ucret" = yorumlar_asgari_ucret,
           "Ahmet Minguzzi" = yorumlar_ahmet_minguzzi,
           "Iklim Degisikligi" = yorumlar_iklim_degisikligi)
  })
  output$haberOzet <- renderText({
    switch(input$haber_sec,
           "Ahmet Minguzzi" = "Ahmet Minguzzi hakkinda sosyal medyada yapilan yorumlar analiz edilmistir.",
           "Deprem" = "Turkiye'de yasanan depremler sonrasi sosyal medya uzerinden yapilan yorumlar analiz edilmistir.",
           "Asgari Ucret" = "Asgari ucret ile ilgili sosyal medya yorumlari analiz edilmistir.",
           "Iklim Degisikligi" = "Iklim degisikligi ve kanunu ile ilgili sosyal medya yorumlari analiz edilmistir.")
  })
  
  yorum_df <- reactive({
    req(veri_sec())  # veri_sec() fonksiyonunun ciktisi burada gerekli
    if ("Yorumlar" %in% names(veri_sec())) {
      tibble(yorum = veri_sec()$Yorumlar)  # Yorumlar sutununu cikartiyoruz
    } else {
      stop("Yorum sutunu bulunamadi!")
    }
  })
  
  yorumlar_tidy <- reactive({
    yorum_df() %>%
      unnest_tokens(kelime, yorum) %>%
      filter(!kelime %in% turkce_stopwords,
             str_detect(kelime, "^[a-z????????????]+$")) %>%
      mutate(kelime = str_to_lower(kelime))
  })
  
  kelime_frekans <- reactive({
    yorumlar_tidy() %>% count(kelime, sort = TRUE)
  })
  
  
  output$emojiKelimeBulutu <- renderPlot({
    req(veri_sec()) #secilen veri bekleniyor
    yorumlar <- veri_sec()$Yorumlar #yorumlar aliniyor
    emojiler <- unlist(str_extract_all(yorumlar, "[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U00002700-\U000027BF]")) # Emojiler regex ile secilir
    emojiler <- emojiler[!is.na(emojiler)] #NA degerleri temizlenir
    if(length(emojiler) == 0){ 
      return(NULL) #emoji yoksa cizilmez
    }

    emoji_sayim <- as.data.frame(table(emojiler)) # Emoji frekansi hesaplanir
    emoji_sayim <- emoji_sayim %>% arrange(desc(Freq)) %>% slice_max(Freq, n = 50) # En cok gecen 50 emoji alinir
    
    #gorsellestirme yapilir
    ggplot(emoji_sayim, aes(label = emojiler, size = Freq, color = Freq)) +
      geom_text_wordcloud_area(family = "Segoe UI Emoji") + #emoji fontu
      scale_size_area(max_size = 20) +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal() +
      labs(title = "Emoji Kelime Bulutu", size = "Frekans", color = "Frekans")
  })
  
  output$kelimeBulutuDiamond <- renderWordcloud2({
    top_words <- kelime_frekans() %>% top_n(100, n) #en cok gecen 50 kelime
    wordcloud2(top_words, shape = ifelse(input$haber_sec == "Asgari Ucret", "star", "diamond"))
  })
  
  output$emojiGrafik <- renderPlot({
    #veri cekme ve emoji cikarma
    emojiler <- unlist(str_extract_all(veri_sec()$Yorumlar, "[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U00002700-\U000027BF]"))
    #emoji frekansi hesaplama
    emoji_sayim <- as.data.frame(table(emojiler)) %>% 
      arrange(desc(Freq)) %>% 
      slice_max(Freq, n = 10) #ilk 10 emoji
    #graif olusturma
    ggplot(emoji_sayim, aes(x = reorder(emojiler, Freq), y = Freq, fill = Freq)) +
      geom_col() + #sutun grafigi
      geom_text(aes(label = emojiler), size = 10, family = "Segoe UI Emoji", hjust = -0.5) +
      coord_flip() + #yatay eksen
      labs(title = "En cok Kullanilan 10 Emoji", x = NULL, y = "Kullanim") + #grafik basligi
      theme_minimal(base_size = 16)
  })
  
  
  
  output$duyguGrafik <- renderPlot({
    veri <- switch(input$haber_sec,
                   "Ahmet Minguzzi" = duygu_verisi_ahmet,
                   "Deprem" = duygu_verisi_deprem,
                   "Asgari ??cret" = duygu_verisi_asgari,
                   "Iklim Degisikligi" = duygu_verisi_iklim)
    # Duygu etiketlerini sayisal polariteye cevir
    veri <- veri %>%
      mutate(sentiment = case_when(
        duygu == "positive" ~ 1,
        duygu == "neutral"  ~ 0,
        duygu == "negative" ~ -1
      ))
    duygu_sayilari <- veri %>% # Duygu sayilari (grafik icin)
      count(duygu, sort = TRUE)
    # ??statistikleri hesapla
    ortalama_sentiment <- mean(veri$sentiment, na.rm = TRUE)
    varyans_sentiment <- var(veri$sentiment, na.rm = TRUE)
    std_sapma_sentiment <- sd(veri$sentiment, na.rm = TRUE)
    # ??statistik metni
    istatistik_metni <- paste0(
      "Ortalama: ", round(ortalama_sentiment, 3), "\n",
      "Varyans: ", round(varyans_sentiment, 3), "\n",
      "Standart Sapma: ", round(std_sapma_sentiment, 3)
    )
    max_y <- max(duygu_sayilari$n)
    ggplot(duygu_sayilari, aes(x = reorder(duygu, n), y = n, fill = duygu)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = n), hjust = -0.1, size = 5, color = "black", fontface = "bold") +
      coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Yorumlara Gore Temel Duygu Dagilimi",
           x = "Duygu Turu",
           y = "Yorum Sayisi",
           caption = istatistik_metni) +
      theme_minimal(base_size = 16) +
      theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))
  })
  
  # Yeni trigram duygu analizi
  output$trigramDuyguGrafik <- renderPlot({ #kullanici veri seti sectiginden emin olma
    req(yorum_df()) #burasi bos ise grafik olusmaz
    
    trigramlar <- yorum_df() %>%
      unnest_tokens(trigram, yorum, token = "ngrams", n = 3) %>% #yorumlardan 3'lu kelime grubu
      separate(trigram, into = c("kelime1", "kelime2", "kelime3"), sep = " ") %>% #trigram icindekiler ayri gruba
      filter(
        !kelime1 %in% turkce_stopwords, #alakasiz kelimeleri ve rakamlari cikarma
        !kelime2 %in% turkce_stopwords,
        !kelime3 %in% turkce_stopwords,
        !str_detect(kelime1, "\\d"),
        !str_detect(kelime2, "\\d"),
        !str_detect(kelime3, "\\d")
      )
    
    trigram_duygu <- trigramlar %>%
      pivot_longer( #kelimeler uzun formata cevrilir
        cols = c(kelime1, kelime2, kelime3), 
        names_to = "pozisyon", 
        values_to = "kelime"
      ) %>%
      left_join(duygu_sozlugu, by = "kelime") %>% #Kelime bazli duygu sozlugu ile eslestirme yapilir.
      filter(!is.na(duygu)) %>%
      count(duygu, sort = TRUE)
    #Gorsellestirme
    ggplot(trigram_duygu, aes(x = reorder(duygu, n), y = n, fill = duygu)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = n), vjust = -0.5, size = 6, fontface = "bold") +
      scale_fill_manual(values = c("olumlu" = "#4caf50", "olumsuz" = "#f44336")) +
      labs(
        title = "Duygu Analizi 2",
        subtitle = "Olumlu ve Olumsuz Kelimelerle Duygu Analizi",
        x = "Duygu", 
        y = "Kullanim Sayisi"
      ) +
      theme_minimal(base_size = 14)
  })
  
  output$bigramPlot <- renderPlot({
    bigram <- yorum_df() %>%
      unnest_tokens(bigram, yorum, token = "ngrams", n = 2) %>%
      separate(bigram, into = c("kelime1", "kelime2"), sep = " ") %>%
      filter(!kelime1 %in% turkce_stopwords,
             !kelime2 %in% turkce_stopwords,
             !is.na(kelime1), !is.na(kelime2)) %>%   # NA de??erleri filtrele
      unite(bigram, kelime1, kelime2, sep = " ") %>%
      count(bigram, sort = TRUE) %>%
      slice_max(n, n = 15)
    
    ggplot(bigram, aes(x = reorder(bigram, n), y = n, fill = n)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "En S??k Bigramlar", x = NULL, y = "Frekans") +
      theme_minimal(base_size = 14)
  })
  
  output$bigramPlott <- renderPlot({
    bigramm <- yorum_df() %>%
      unnest_tokens(bigramm, yorum, token = "ngrams", n = 3) %>%
      separate(bigramm, into = c("kelime1", "kelime2", "kelime3"), sep = " ") %>%
      filter(!kelime1 %in% turkce_stopwords,
             !kelime2 %in% turkce_stopwords,
             !kelime3 %in% turkce_stopwords,
             !is.na(kelime1), !is.na(kelime2), !is.na(kelime3)) %>%
      unite(bigramm, kelime1, kelime2, kelime3, sep = " ") %>%
      count(bigramm, sort = TRUE) %>%
      slice_max(n, n = 15) %>%
      mutate(bigramm_label = str_wrap(bigramm, width = 30))
    fill = factor(bigramm_label)  # farkl?? renkler i??in fill'i faktor yap
    ggplot(bigramm, aes(x = reorder(bigramm_label, n), y = n, fill = factor(bigramm_label))) +
      geom_col(show.legend = FALSE, color = "black") +
      scale_fill_brewer(palette = "Paired") + 
      coord_flip() +
      labs(title = " En S??k Trigramlar", x = NULL, y = "Frekans") +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.y = element_text(size = 11, face = "bold", color = "#c21313"),
        axis.text.x = element_text(color = "#4a0707"),
        plot.title = element_text(face = "bold", size = 16, color = "#1A1A1A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  output$bigramPlott <- renderPlot({
    bigramm <- yorum_df() %>%
      unnest_tokens(bigramm, yorum, token = "ngrams", n = 3) %>%
      separate(bigramm, into = c("kelime1", "kelime2", "kelime3"), sep = " ") %>%
      filter(!kelime1 %in% turkce_stopwords,
             !kelime2 %in% turkce_stopwords,
             !kelime3 %in% turkce_stopwords,
             !is.na(kelime1), !is.na(kelime2), !is.na(kelime3)) %>%
      unite(bigramm, kelime1, kelime2, kelime3, sep = " ") %>%
      count(bigramm, sort = TRUE) %>%
      slice_max(n, n = 15) %>%
      mutate(bigramm_label = str_wrap(bigramm, width = 30))  # burada sarma
    
    ggplot(bigramm, aes(x = reorder(bigramm_label, n), y = n, fill = n)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "En Cok Trigramlar", x = NULL, y = "Frekans") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 14)
      )
  })
  
  
  output$hava_durumu_bilgisi <- renderUI({
    req(input$sehir_sec)
    hava_data <- list(
      durum = "Gunesli",
      sicaklik = "25 C",
      nem = "%46",
      icon = "sun"
    )
    tagList(
      h3(hava_data$sicaklik, style = "color: #18BC9C; text-align: center;"),
      div(icon("temperature-three-quarters"), " Hissedilen: 26??C", 
          style = "margin: 5px 0;"),
      div(icon("droplet"), " Nem: ", hava_data$nem,
          style = "margin: 5px 0;"),
      div(icon("wind"), " R??zgar: 14 km/s",
          style = "margin: 5px 0;"),
      div(icon(hava_data$icon), hava_data$durum,
          style = "margin-top: 10px; color: #FFA500;")
    )
  })
  
  
  output$agGrafik <- renderPlot({
    trigram_df <- yorum_df() %>%
      unnest_tokens(trigram, yorum, token = "ngrams", n = 3) %>%
      separate(trigram, into = c("kelime1", "kelime2", "kelime3"), sep = " ") %>%
      filter(!kelime1 %in% turkce_stopwords,
             !kelime2 %in% turkce_stopwords,
             !kelime3 %in% turkce_stopwords,
             str_detect(kelime1, "^[a-z????????????]+$"),
             str_detect(kelime2, "^[a-z????????????]+$"),
             str_detect(kelime3, "^[a-z????????????]+$")) %>%
      count(kelime1, kelime2, kelime3, sort = TRUE) %>%
      slice_max(n, n = 30)
    
    edges <- trigram_df %>%
      transmute(from = kelime1, to = kelime2)
    
    graph <- graph_from_data_frame(edges, directed = TRUE)
    
    ggraph(graph, layout = "fr") +
      geom_edge_link(arrow = arrow(length = unit(4, 'mm')), end_cap = circle(3, 'mm'),
                     edge_alpha = 0.5, edge_colour = "#18BC9C") +
      geom_node_point(color = "#2C3E50", size = 4) +
      geom_node_text(aes(label = name), vjust = 1.5, hjust = 1, repel = TRUE, size = 5) +
      theme_void() +
      labs(title = "Trigram Kelime Agi") +
      theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
  })
  
  
  duyguSayaci <- reactiveValues(mutlu=0, uzgun=0, kizgin=0, notr=0)
  
  dosya_adi <- reactive({
    # Haber adindaki bosluklari ve Turkce karakterleri temizleyip dosya ismi olusturabilirsin
    paste0("duygu_sonuclari_", gsub(" ", "_", tolower(input$haber_sec)), ".csv")
  })
  observeEvent(input$haber_sec, {
    dosya <- dosya_adi()
    if (file.exists(dosya)) {
      df <- read.csv(dosya, stringsAsFactors = FALSE)
      duyguSayaci$mutlu <- df$mutlu
      duyguSayaci$uzgun <- df$uzgun
      duyguSayaci$kizgin <- df$kizgin
      duyguSayaci$notr <- df$notr
    } else {
      duyguSayaci$mutlu <- 0
      duyguSayaci$uzgun <- 0
      duyguSayaci$kizgin <- 0
      duyguSayaci$notr <- 0
    }
  }, ignoreNULL = FALSE)
  kaydet <- function() {
    df <- data.frame(
      mutlu = duyguSayaci$mutlu,
      uzgun = duyguSayaci$uzgun,
      kizgin = duyguSayaci$kizgin,
      notr = duyguSayaci$notr
    )
    write.csv(df, dosya_adi(), row.names = FALSE)
  }
  observeEvent(input$mutlu, {
    duyguSayaci$mutlu <- duyguSayaci$mutlu + 1
    kaydet()
  })
  observeEvent(input$uzgun, {
    duyguSayaci$uzgun <- duyguSayaci$uzgun + 1
    kaydet()
  })
  observeEvent(input$kizgin, {
    duyguSayaci$kizgin <- duyguSayaci$kizgin + 1
    kaydet()
  })
  observeEvent(input$notr, {
    duyguSayaci$notr <- duyguSayaci$notr + 1
    kaydet()
  })
  
  output$duyguPlot <- renderPlot({
    duygu_df <- data.frame(
      Duygu = c("Mutlu", "Uzgun", "Kizgin", "Notr"),
      Sayi = c(duyguSayaci$mutlu, duyguSayaci$uzgun, duyguSayaci$kizgin, duyguSayaci$notr),
      stringsAsFactors = FALSE
    )
    
    barplot(
      height = duygu_df$Sayi,
      names.arg = duygu_df$Duygu,
      col = c("green", "blue", "red", "gray"),
      ylim = c(0, max(duygu_df$Sayi, 5)),
      main = paste("Canli Duygu Anket Sonuclari -", input$haber_sec),
      ylab = "Oy Sayisi"
    )
  })
  output$depremHarita <- renderLeaflet({
    leaflet(data = sehir_koord) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lng, ~lat,
        label = ~sehir,
        layerId = ~sehir,
        radius = 8,
        color = "red",
        fillOpacity = 0.7
      )
  })
  
  secilen <- reactiveVal(NULL)
  
  observeEvent(input$depremHarita_marker_click, {
    secilen(input$depremHarita_marker_click$id)
  })
  
  output$secilenSehir <- renderUI({
    req(secilen())
    h4(paste("Secilen Sehir:", secilen()))
  })
  
  output$haberlerTablosu <- renderDataTable({
    req(secilen())
    deprem_haberleri %>%
      filter(sehir == secilen()) %>%
      select(tarih, baslik, icerik)
  })
  
  geri_bildirimler <- reactiveVal(data.frame(geri_bildirim = character(0)))
  # Geri bildirim g??nderildi??inde, yeni veri ekleme
  observeEvent(input$gonder, {
    yeni_geri_bildirim <- input$geri_bildirim
    geri_bildirimler(rbind(geri_bildirimler(), data.frame(geri_bildirim = yeni_geri_bildirim)))
  })# Geri bildirimlerin gorsellestirilmesi: Bar chart
  output$geriBildirimPlot <- renderPlot({# Veriyi al
    geri_bildirim_data <- geri_bildirimler()# Veri var mi kontrol et
    if (nrow(geri_bildirim_data) > 0) {# Geri bildirimlerin frekansini hesapla
      geri_bildirim_freq <- table(geri_bildirim_data$geri_bildirim)# Gorselletirme
      ggplot(as.data.frame(geri_bildirim_freq), aes(x = Var1, y = Freq)) +
        geom_bar(stat = "identity", fill = "#18BC9C") +
        labs(x = "Geri Bildirim", y = "Frekans", title = "Geri Bildirimlerin Da????l??m??") +
        theme_minimal()
    }
  })# Kullanici yorumlari
  observeEvent(input$gonder, {# Kullanici yorumlarini (textArea) saklama
    if(input$kullaniciYorum != "") {
      cat("Kullanici Yorum: ", input$kullaniciYorum, "\n")
    }
  })
  observeEvent(input$gonder, {
    if (input$kullaniciYorum != "") {
      veri <- data.frame(
        zaman = Sys.time(),
        secim = input$geri_bildirim,
        yorum = input$kullaniciYorum,
        stringsAsFactors = FALSE
      )
      if (!file.exists("kullaniciyorumlar.csv")) {
        write.csv(veri, "kullaniciyorumlar.csv", row.names = FALSE)
      } else {
        write.table(veri, "kullaniciyorumlar.csv", append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
      }
    }
  })
  
  # Secilen habere g??re analiz sonucu dondur
  output$haber_analiz_sonucu <- renderText({
    switch(input$haber_sec,
           "Ahmet Minguzzi" = "Ahmet Minguzzi hakkinda yapilan yorumlar agirlikli olarak olumsuz yonde. Ancak tamamen tek tarafli degil; hatiri sayilir sayida pozitif yorum da mevcut. Bu durum, izleyici kitlesinin bolundugunu veya elestirel bir yapida oldugunu gosterebilir.",
           "Deprem" = "Deprem hakkindaki yorumlarin genelinde olumsuz duygular hakim, ancak duygu dagilimi heterojen. Bu, felaketin yarattigi travma, endise ve uzuntunun yani sira, bazi olumlu mesajlarin (ornegin destek ifadeleri) da bulundugunu gosteriyor.",
           "Asgari Ucret" = "Asgari ucretle ilgili YouTube yorumlari buyuk oranda olumsuz ve halk arasinda memnuniyetsizlik baskin. Bu grafik, sosyal medyada asgari ucret politikalariyla ilgili yapilan uygulamalarin ciddi sekilde elestirildigini ve olumlu goruslerin cok sinirli kaldigini ortaya koyuyor.",
           "Iklim Degisikligi" = "Yorumlarda net bir kutuplasma yok, toplumun bu konuda karmasik, cift yonlu dusunceler tasidigi anlasiliyor. Konu, bazilari icin umut verici, bazilari icinse endise verici olabilir. Bu da sosyal politikalarin iletisiminde bu cesitliligi goz onunde bulundurmayi gerektirir.")
  })
}


# Uygulama Baslat
shinyApp(ui = ui, server = server)

