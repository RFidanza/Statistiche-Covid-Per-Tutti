
library("shiny")
library("shinythemes")
library("ggiraph")
library("plotly")
library("leaflet")
library("shinydashboard")
library("shinyWidgets")
library("purrr")
library("stringr")
library("lubridate")
library("shinybusy")



reg_choices <- list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta", "Emilia-Romagna", "Friuli Venezia Giulia", "Veneto",
                    "Trentino-Alto Adige","Lazio", "Marche", "Toscana", "Umbria",
                    "Abruzzo", "Basilicata", "Calabria", "Campania", "Molise", "Puglia","Sardegna", "Sicilia")
reg_vac<-list("Moderna","Pfizer/BioNTech","Janssen","Vaxzevria (AstraZeneca)", "Pfizer Pediatrico","Novavax")

reg_prov<-list("L'Aquila","Teramo","Pescara","Potenza","Cosenza","Catanzaro","Reggio di Calabria","Crotone","Vibo Valentia","Caserta","Benevento",
               "Napoli","Avellino","Salerno","Piacenza","Parma","Reggio nell'Emilia","Modena","Bologna","Ferrara","Ravenna","Forlì-Cesena","Rimini",
               "Udine","Gorizia","Trieste","Pordenone","Viterbo","Rieti","Roma","Latina","Frosinone","Imperia","Savona","Genova","La Spezia","Varese",
               "Como","Sondrio","Milano","Bergamo","Brescia","Pavia","Cremona","Mantova","Lecco","Lodi","Monza e della Brianza","Pesaro e Urbino",
               "Ancona","Macerata","Ascoli Piceno","Fermo","Campobasso","Isernia","Bolzano","Trento","Torino","Vercelli","Novara","Cuneo","Asti",
               "Alessandria","Biella","Verbano-Cusio-Ossola","Foggia","Bari","Taranto","Brindisi","Lecce","Barletta-Andria-Trani","Sassari","Nuoro",
               "Cagliari","Oristano","Sud Sardegna", "Trapani", "Palermo", "Messina","Agrigento","Caltanissetta", "Enna","Catania", "Ragusa", "Siracusa","Pistoia",
               "Firenze","Livorno","Pisa", "Arezzo", "Siena", "Grosseto", "Prato" , "Perugia","Terni","Aosta","Vicenza","Belluno","Treviso","Venezia","Padova", "Rovigo")  

reg_fasc<-list("40-49","90+","20-29","30-39","50-59","60-69","70-79","80-89","12-19","05-11")


ui<-navbarPage(
  
  
  
  theme = shinytheme("slate"),
  title = "Le statistiche Covid a portata di tutti",
  
  tabPanel(
    "Home Page",
    busy_start_up(
      loader = spin_epic("orbit", color = "#FFF"),
      text = "Loading...
      Il tempo di caricamento può dipendere dalla connessione",
      mode= "auto",
      color = "#FFF",
      background = "#112446"
    ),
    div(
      style="margin-top:-3.5em",
      fluidRow(
        HTML('<meta name="viewport" content="width=1024">'),tags$hr(),
        useShinydashboard(),
        column(width = 12,
               htmlOutput(outputId = "ultima_situazione"),
               h3("Dati giornalieri relativi all'ultimo aggiornamento",align="center"),
               valueBoxOutput("NuoviPositivi"), valueBoxOutput("Isolamento"), valueBoxOutput("Ospedalizzati"),
               valueBoxOutput("TerapiaIntensiva"), valueBoxOutput("DecedutiGiornalieri"), valueBoxOutput("OspedalizzatiGiornalieri"), 
               valueBoxOutput("TamponiTot"),valueBoxOutput("TamponiMolecolari"), valueBoxOutput("TamponiAntigenici"),
               h3("Dati totali relativi all'ultimo aggiornamento giornaliero",align="center"),
               valueBoxOutput("PositiviTot"), valueBoxOutput("MortiTot"), valueBoxOutput("TerapieTot"),
               h3("Vaccinazioni Giornaliere",align="center"),
               valueBoxOutput("vaccini1"),valueBoxOutput("vaccini2"),valueBoxOutput("vaccini3"),
               valueBoxOutput("vaccini4")
               
        )
      )
    )
    
    
  ),
  tabPanel(
    "Andamento Nazionale",
    fluidPage(
      h1("Nazionale"),
      h4("Analisi graficata, in base all'andamento nazionale del numero totale di positivi, il numero di terapie intensive, i deceduti e i tamponi. "),
      h4("Attraverso la tabella si possono visualizzare i dati utilizzati per l'analisi sottostante. "),
      dataTableOutput("dynamic"),
      h3("Grafico andamento totale positivi"),
      fluidRow(plotlyOutput("grafico")),
      h3("Grafico andamento terapie intensive"),
      fluidRow(plotlyOutput("grafico2")),
      h3("Grafico dei deceduti"),
      fluidRow(plotlyOutput("grafico3")),
      h3("Tamponi"),
      fluidRow(plotlyOutput("grafico4"))
      
      
    )
  ),
  tabPanel("Andamento Regionale",
           mainPanel(
             h1("Regionale"),
             h4("Studio suddiviso in dati cumulati e dati giornalieri, l'utente ha la possibilità di decidere sia quale dei due studi vuole visualizzare e in aggiunta anche la regione e la variabile da lui desiderata."),
             tabsetPanel(
               tabPanel("Cumulati",
                        fluidPage(
                          selectizeInput(inputId = "regions",
                                         label="Selezionare una o più regioni",
                                         choices= reg_choices,
                                         selected="Lazio",
                                         multiple=TRUE
                                         
                                         
                          ),
                          radioButtons("rb","Selezionare l'indicatore da visualizzare",
                                       choiceNames = list("Totale dei positivi",
                                                          "Pazienti deceduti",
                                                          "Pazienti dimessi o guariti",
                                                          "Tamponi effettuati",
                                                          "Pazienti ricoverati con sintomi",
                                                          "Terapie intensive"
                                       ),
                                       choiceValues = list("Totale dei positivi","Pazienti deceduti",
                                                           "Pazienti dimessi o guariti","Tamponi effettuati","Pazienti ricoverati con sintomi",
                                                           "Terapie intensive"
                                       ),
                                       selected = "Totale dei positivi"
                                       
                                       
                          ),
                          
                        ),
                        h5("Grafico a scelta multipla per le regioni e i vari indicatori"),
                        fluidRow(plotlyOutput("scatterRegionale"))
                        
               ),
               tabPanel("Giornalieri",
                        fluidPage(
                          selectizeInput(inputId = "Not_cumulate",
                                         label="Selezionare una o più regioni",
                                         choices=reg_choices,
                                         selected="Lazio",
                                         multiple=TRUE
                                         
                          ),
                          radioButtons("rbnc","Selezionare l'indicatore da visualizzare",
                                       choiceNames = list("Positivi giornalieri",
                                                          "Pazienti in isolamento domiciliare",
                                                          "Terapia intensiva",
                                                          "Pazienti ricoverati con sintomi"
                                                          
                                       ),
                                       choiceValues = list("Positivi giornalieri","Pazienti in isolamento domiciliare",
                                                           "Terapia intensiva","Pazienti ricoverati con sintomi"
                                                           
                                       ),
                                       selected = "Positivi giornalieri"
                                       
                          )
                        ),
                        h5("Grafico a scelta multipla per le regioni e i vari indicatori"),
                        fluidRow(plotlyOutput("scatterRegionaleGiorn"))
                        
               )
             )
           )
  ),
  tabPanel("Andamento delle province",
           fluidPage(
             h1("Province"),
             h4("Studio con possibilità di visualizzare le provincie italiane desiderate e in aggiunta è possibile attraverso una mappa interattiva vedere la provincia selezionata e il numero di positivi al suo interno."),
             selectizeInput(inputId="provincia",
                            label="Selezionare una provincia",
                            choices= reg_prov,
                            selected="Roma",
                            multiple=FALSE
                            
             )
           ),
           h5("Grafico dei positivi per le province con relativa mappa di riferimento"),
           fluidRow(splitLayout(plotlyOutput("graficoprovince"),leafletOutput("mappa")))
           
  ),
  tabPanel("Vaccinazioni",icon = icon("syringe"),
           fluidPage(
             h1("Vaccinazioni"),
             h4("Studio completo delle vaccinazioni in italia, suddiviso in prima dose, seconda dose, dose addizionale booster e seconda dose booster(4 dose)."),
             h4("Attraverso la tabella si possono visualizzare i dati utilizzati per l'analisi e in aggiunta attraverso i box di scelta si possono decidere la fascia d'eta desiderata e il fornitore del vaccino."),
             dataTableOutput("dynamicvac"),
             selectizeInput(inputId = "fascia",
                            label="Selezionare la fascia anagrafica desiderata",
                            choices=reg_fasc,
                            selected="20-29",
                            multiple=FALSE),
             selectizeInput(inputId = "vaccino",
                            label="Selezionare uno o più fornitori di vaccino",
                            choices=reg_vac,
                            selected="Pfizer/BioNTech",
                            multiple=TRUE
             ),
             h5("Grafico della somministrazione vaccinale (Prima dose) per fascia anagrafica e fornitore"),
             fluidRow(plotlyOutput("graficovaccino")),
             h5("Grafico della somministrazione vaccinale (Seconda dose) per fascia anagrafica e fornitore"),
             fluidRow(plotlyOutput("graficovaccino2")),
             h5("Grafico della somministrazione vaccinale (Dose Booster) per fascia anagrafica e fornitore"),
             fluidRow(plotlyOutput("graficovaccino3")),
             h5("Grafico della somministrazione vaccinale (Dose Booster 2) per fascia anagrafica e fornitore"),
             fluidRow(plotlyOutput("graficovaccino4"))
             
           )
           
           
  ),
  tabPanel("Riguardante questa pagina",
           tags$div(
             h1("Funzionalità"),
             h4("Questo sito viene aggiornato una volta al giorno. Sono disponibili eccellenti strumenti di mappatura COVID."),
             h4("Il nostro obiettivo è integrare queste risorse con diverse funzionalità per riuscire a capire come la situazione in italia sia cambiata con l'avanzare del tempo."),
             tags$br(),
             tags$hr(),
             h1("Contesto analizzato"),
             h4("A dicembre 2019, nella città di Wuhan, in Cina, sono iniziati a essere segnalati casi di gravi malattie respiratorie. Questi sono stati causati da un nuovo tipo di coronavirus e la malattia è ora comunemente chiamata COVID-19."),
             h4(" Il numero di casi di COVID-19 ha iniziato a crescere più rapidamente a metà gennaio e il virus si è presto diffuso oltre i confini della Cina. Da allora questa storia si è evoluta rapidamente e ogni giorno ci troviamo di fronte a titoli preoccupanti sullo stato attuale dell'epidemia."),
             h4("Questi titoli possono essere difficili da interpretare. Quanto velocemente si sta diffondendo il virus? Gli sforzi per controllare la malattia funzionano? Questo sito viene aggiornato quotidianamente. Guardando oltre i titoli, speriamo sia possibile ottenere una comprensione più profonda di questa pandemia in corso."),
             tags$br(),
             tags$br(),
             HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/AF1Ai4INiMs" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
             h1("Autori"),
             h4("Riccardo Fidanza Massaro Leonardo"),
             h2("Contatti"),
             h4("l.massro4@lumsastud.it, r.fidanza1@lumsastud.it"),
           ))
)





server<-function(input,output){
  covid_data<-reactive({
    covid <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv",encoding = 'UTF-8')
    
  })
  
  ieri_data<-reactive({
    today<-covid_data() %>%
      filter(data==Sys.Date()-1)
  })
  altro_data<-reactive({
    yesterday<-covid_data() %>%
      filter(data==Sys.Date()-2)
  }) 
  
  regioni_data<-reactive({
    regioni <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",encoding = 'UTF-8')
    regioni$data<-gsub('T17:00:00','',regioni$data)
    regioni$data<-gsub('T18:00:00','',regioni$data)
    return(as.data.frame(regioni))
  })
  province_data<-reactive({
    province <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv",encoding = 'UTF-8')
    province$data<-gsub('T17:00:00','',province$data)
    province$data<-gsub('T18:00:00','',province$data)
    return(as.data.frame(province))
  })
  vac_data<-reactive({
    vaccini <- read.csv("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-latest.csv",encoding = 'UTF-8')
  })
  ieri_vac<-reactive({
    today_vac<-vac_data()%>%
      filter(data==Sys.Date()-1)
  })
  subs_prov<-reactive({
    prov_sub<-subset(province_data(),province_data()$denominazione_provincia !="In fase di definizione/aggiornamento")
    prov_sub<-subset(prov_sub,prov_sub$denominazione_provincia !="Fuori Regione / Provincia Autonoma")
  })
  
  remove_data<-reactive({
    cov<-covid_data()
    cov$data<-gsub('T18:00:00','',cov$data)
    cov$data<-gsub('T17:00:00','',cov$data)
    return(as.data.frame(cov))
  })
  
  
  decumulate_dead<-reactive({
    decum_deceduti<- remove_data() %>% group_by(stato) %>% mutate(new_dead = c(deceduti[1], diff(deceduti)))
    return(as.data.frame(decum_deceduti))
  })
  
  decumulate_tamp<-reactive({
    decum_tamponi<- remove_data() %>% group_by(stato) %>% mutate(new_tamp = c(tamponi[1], diff(tamponi)))
    return(as.data.frame(decum_tamponi))
    
  })
  
  
  
  output$ultima_situazione <- renderUI({
    HTML(paste0(tags$h2(tags$strong("Dati riguardanti la situazione covid in Italia"), align = "left"), h5("ultimo aggiornamento: ", Sys.Date()-1, align = "left")))
  })
  output$NuoviPositivi <- renderValueBox({
    valueBox(
      value = HTML(paste0("Nuovi Positivi <br>",ieri_data()$nuovi_positivi)),
      subtitle= (""),
      icon = icon("fa-solid fa-virus",verify_fa=FALSE),
      color = "yellow"
    )
    
  })
  output$DecedutiGiornalieri <- renderValueBox({
    valueBox(
      value = HTML(paste0("Deceduti <br>",ieri_data()$deceduti-altro_data()$deceduti)),
      subtitle= (""),
      icon = icon("glyphicon glyphicon-tint",verify_fa=FALSE),
      color = "orange"
    )
    
  })
  output$OspedalizzatiGiornalieri <- renderValueBox({
    valueBox(
      value = HTML(paste0("Ricoveri con sintomi <br>",ieri_data()$ricoverati_con_sintomi-altro_data()$ricoverati_con_sintomi)),
      subtitle= (""),
      icon = icon("fa-solid fa-bed",verify_fa=FALSE),
      color = "red"
    )
    
  })
  output$TerapiaIntensiva <- renderValueBox({
    valueBox(
      value = HTML(paste0("Terapia Intensiva <br>",ieri_data()$ingressi_terapia_intensiva)),
      subtitle= (""),
      icon = icon("stethoscope"),
      color = "yellow"
    )
    
  })
  output$Isolamento <- renderValueBox({
    valueBox(
      value = HTML(paste0("Isolamento Domiciliare <br>",ieri_data()$isolamento_domiciliare-altro_data()$isolamento_domiciliare)),
      subtitle= (""),
      icon = icon("fa-regular fa-house-user",verify_fa=FALSE),
      color = "orange"
    )
    
  })
  output$Ospedalizzati <- renderValueBox({
    valueBox(
      value = HTML(paste0("Ospedalizzazioni <br>",ieri_data()$totale_ospedalizzati-altro_data()$totale_ospedalizzati)),
      subtitle= (""),
      icon = icon("fa-solid fa-hospital",verify_fa=FALSE),
      color = "red"
    )
    
  })
  output$TamponiTot <- renderValueBox({
    valueBox(
      value = HTML(paste0("Tamponi <br>",ieri_data()$tamponi-altro_data()$tamponi)),
      subtitle= (""),
      icon = icon("head-side-cough"),
      color = "yellow"
    )
    
  })
  output$TamponiMolecolari <- renderValueBox({
    valueBox(
      value = HTML(paste0("Tamponi molecolari <br>",ieri_data()$tamponi_test_molecolare-altro_data()$tamponi_test_molecolare)),
      subtitle= (""),
      icon = icon("head-side-cough"),
      color = "orange"
    )
    
  })
  output$TamponiAntigenici <- renderValueBox({
    valueBox(
      value = HTML(paste0("Tamponi antigenici <br>",ieri_data()$tamponi_test_antigenico_rapido-altro_data()$tamponi_test_antigenico_rapido)),
      subtitle= (""),
      icon = icon("head-side-cough"),
      color = "red"
    )
    
  })
  output$PositiviTot <- renderValueBox({
    valueBox(
      value = HTML(paste0("Totale Positivi <br>",ieri_data()$totale_positivi)),
      subtitle= (""),
      icon = icon("fa-solid fa-virus",verify_fa=FALSE),
      color = "navy"
    )
    
  })
  output$MortiTot <- renderValueBox({
    valueBox(
      value = HTML(paste0("Deceduti totali <br>",ieri_data()$deceduti)),
      subtitle= (""),
      icon = icon("glyphicon glyphicon-tint",verify_fa=FALSE),
      color = "blue"
    )
    
  })
  output$TerapieTot <- renderValueBox({
    valueBox(
      value = HTML(paste0("Terapie intensive <br>",ieri_data()$terapia_intensiva)),
      subtitle= (""),
      icon = icon("stethoscope"),
      color = "light-blue"
    )
    
  })
  output$vaccini1 <- renderValueBox({
    valueBox(
      value = HTML(paste0("Prime Dosi <br>",sum(ieri_vac()$d1))),
      subtitle= (""),
      icon = icon("syringe"),
      color = "green"
    )
    
  })
  output$vaccini2 <- renderValueBox({
    valueBox(
      value = HTML(paste0("Seconde Dosi <br>",sum(ieri_vac()$d2))),
      subtitle= (""),
      icon = icon("syringe"),
      color = "green"
    )
    
  })
  output$vaccini3 <- renderValueBox({
    valueBox(
      value = HTML(paste0("Terze Dosi <br>",sum(ieri_vac()$db1))),
      subtitle= (""),
      icon = icon("syringe"),
      color = "olive"
    )
    
  })
  output$vaccini4 <- renderValueBox({
    valueBox(
      value = HTML(paste0("Quarte Dosi <br>",sum(ieri_vac()$db2))),
      subtitle= (""),
      icon = icon("syringe"),
      color = "olive"
    )
    
  })
  
  output$grafico<-renderPlotly({
    plot_ly(remove_data(), x = ~data, y = ~totale_positivi,colors = "blue") %>%
      add_lines(color="")%>%
      layout(yaxis=list(title="Positivi"))})
  
  output$grafico2<-renderPlotly({
    plot_ly(remove_data(), x = ~data, y = ~terapia_intensiva,colors = "red")%>%
      add_lines(color="")%>%
      layout(yaxis=list(title="Terapie intensive"))})
  
  output$grafico3<-renderPlotly({
    plot_ly(decumulate_dead(), x = ~data, y = ~new_dead,colors=~"black") %>%
      add_lines(color="")%>%
      layout(yaxis=list(title="Deceduti"))})
  
  output$grafico4<-renderPlotly({
    plot_ly(decumulate_tamp(), x = ~data, y = ~new_tamp,colors=~"#7FFFD4") %>%
      add_lines(color="")%>%
      layout(yaxis=list(title="Tamponi"))})
  output$dynamic <- renderDataTable(covid_data()[-c(2,12,13,14,16,17,18,19,20,21,22,23,24)], options = list(pageLength = 10))
  
  week_reg <-reactive({ regioni_data() %>% 
      group_by(Y = year(data), W = epiweek(data), denominazione_regione) %>% 
      arrange(data) %>% 
      group_by(denominazione_regione) %>% 
      mutate(cum_cases = (totale_casi),cum_dead=(deceduti),cum_ricoveri=cumsum(ricoverati_con_sintomi),
             cum_terapia=cumsum(terapia_intensiva), cum_ospedalizzati=(totale_ospedalizzati),cum_dimessi=(dimessi_guariti),cum_tamponi=(tamponi)) %>% 
      ungroup()})
  
  output$scatterRegionale <- renderPlotly({
    myDataRegion <- subset(week_reg(), denominazione_regione %in% input$regions)
    
    
    
    if(input$rb == "Totale dei positivi") plotVar <- "cum_cases"
    if(input$rb == "Pazienti deceduti") plotVar <- "cum_dead"
    if(input$rb == "Pazienti dimessi o guariti") plotVar <- "cum_dimessi"
    if(input$rb == "Tamponi effettuati") plotVar <- "cum_tamponi"
    if(input$rb == "Pazienti ricoverati con sintomi") plotVar <- "cum_ricoveri"
    if(input$rb == "Terapie intensive") plotVar <- "cum_terapia"
    
    
    
    myDataRegionVariable <- myDataRegion[, c("data", "denominazione_regione", plotVar)]
    colnames(myDataRegionVariable) <- c("data", "Region", "plotVar")
    
    
    gg<-plot_ly(myDataRegionVariable, x = ~data, y =~plotVar ,color =~Region) %>%
      filter(Region %in% input$regions)%>%
      group_by(Region)%>%
      add_lines(color=~Region)%>%
      layout(font = list(size=12),autosize=F,width=1700,height=400,yaxis=list(title=input$rb))
    
  })
  output$scatterRegionaleGiorn <- renderPlotly({
    myDataRegionGiorn <- subset(week_reg(), denominazione_regione %in% input$Not_cumulate)
    
    if(input$rbnc == "Positivi giornalieri") plotVarGiorn <- "nuovi_positivi"
    if(input$rbnc == "Pazienti in isolamento domiciliare") plotVarGiorn <- "isolamento_domiciliare"
    if(input$rbnc == "Terapia intensiva") plotVarGiorn <- "terapia_intensiva"
    if(input$rbnc == "Pazienti ricoverati con sintomi") plotVarGiorn <- "ricoverati_con_sintomi"
    
    
    
    myDataRegionVariableGiorn <- myDataRegionGiorn[, c("data", "denominazione_regione", plotVarGiorn)]
    colnames(myDataRegionVariableGiorn) <- c("data", "Region", "plotVarGiorn")
    
    gg<-plot_ly(myDataRegionVariableGiorn, x = ~data, y = ~plotVarGiorn,color =~Region) %>%
      filter(Region %in% input$Not_cumulate)%>%
      group_by(Region)%>%
      add_lines(color=~Region)%>%
      layout(font = list(size=12),autosize=F,width=1700,height=400,yaxis=list(title=input$rbnc))
    
  })
  output$graficoprovince<-renderPlotly({ 
    plot_ly(province_data(), x = ~data, y = ~totale_casi,color = ~denominazione_provincia) %>%
      filter(denominazione_provincia %in% input$provincia)%>%
      group_by(denominazione_provincia)%>%
      add_markers(alpha = 0.5) %>%
      highlight("plotly_selected")%>%
      layout(yaxis=list(title="Positivi"))})
  
  dati_mappa<-eventReactive(input$provincia,{
    province_data() %>% filter(data==Sys.Date()-1)%>%
      mutate(casi=totale_casi)%>%
      mutate(popup=str_c("<strong>",denominazione_provincia,"<strong> <br>",
                         "Totale casi: ",totale_casi
                         
      )%>% map(htmltools::HTML),
      provsel=ifelse(denominazione_provincia==input$provincia,1,0)%>% factor)
  })
  dati_mappa<-eventReactive(input$provincia,{
    province_data() %>% filter(data==Sys.Date()-1)%>%
      mutate(casi=totale_casi)%>%
      mutate(popup=str_c("<strong>",denominazione_provincia,"<strong> <br>",
                         "Totale casi: ",totale_casi
                         
      )%>% map(htmltools::HTML),
      provsel=ifelse(denominazione_provincia==input$provincia,1,0)%>% factor)
  })
  
  
  output$mappa<-renderLeaflet({
    pal<-colorFactor(c("blue","red"),domain = c(1,0))
    leaflet() %>%
      addProviderTiles("OpenStreetMap.HOT")%>%
      setView(lng=12.483667,lat=	41.89277,zoom = 7)%>%
      addCircles(data = dati_mappa(),lat = ~lat,lng= ~long,radius = ~15000,
                 stroke = F, label = ~popup,color = ~pal(provsel),
                 fillOpacity = 0.5,group = "Casi totali",
                 labelOptions = labelOptions(style = list("font-weight"="normal",padding="3px 8px"),
                                             textsize = "18px",direction = "auto"))
    
  })
  observeEvent(input$provincia, {
    curr_view <- dati_mappa() %>% filter(denominazione_provincia == input$provincia)
    
    leafletProxy(mapId = "mappa") %>% 
      setView(lng = curr_view$long, lat = curr_view$lat, zoom = 7)# %>% 
    
  })
  output$graficovaccino<-renderPlotly({
    plot_ly(vac_data(),x=~data,y=~d1,color = ~forn)%>%
      filter(eta %in% input$fascia)%>%
      filter(forn %in% input$vaccino)%>%
      add_lines()%>%
      layout(yaxis=list(title="Prima dose"))
    
  })
  output$graficovaccino2<-renderPlotly({
    plot_ly(vac_data(),x=~data,y=~d2,color = ~forn)%>%
      filter(eta %in% input$fascia)%>%
      filter(forn %in% input$vaccino)%>%
      add_lines()%>%
      layout(yaxis=list(title="Seconda dose"))
    
  })
  output$graficovaccino3<-renderPlotly({
    plot_ly(vac_data(),x=~data,y=~db1,color = ~forn)%>%
      filter(eta %in% input$fascia)%>%
      filter(forn %in% input$vaccino)%>%
      add_lines()%>%
      layout(yaxis=list(title="Terza dose"))
  })
  output$graficovaccino4<-renderPlotly({
    plot_ly(vac_data(),x=~data,y=~db2,color = ~forn)%>%
      filter(eta %in% input$fascia)%>%
      filter(forn %in% input$vaccino)%>%
      add_lines()%>%
      layout(yaxis=list(title="Quarta dose"))
  })
  
  
  output$dynamicvac<-renderDataTable(vac_data()[-c(3,13,14,15,16)], options = list(pageLength = 10))
  
  
}

shinyApp(ui, server)
