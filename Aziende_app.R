#Library dependencies
library(shiny)
library(shinyWidgets)
library(ggthemes)
library(tidyverse)
library(ggplot2)
library(shinycssloaders)
library(stringr)
library(htmltools)
library(maps)
library(writexl)
library(philentropy)

#Import dataset
ER <- read_csv("https://raw.githubusercontent.com/rubenmartinez9/Search-for-comanies-in-Emilia-Romagna/master/ER_V3.csv")

cols.num <- c("ricavi_vendite", "dipendenti", "utile_netto", "longitudine", "latitudine")
cols.fact <- c("provincia", "regione", "forma_giuridica", "attivita_principale",
               "startup_innovativa", "settore_codice", "settore_liv3", "settore_liv2", 
               "settore_liv1", "region")
cols.string <- c("ragione_sociale", "CF", "indirizzo", "CAP", "telefono", "website", 
                 "descrizione_attivita", "principali_prodotti_servizi", "comune")

ER[cols.num] <- sapply(ER[cols.num],as.numeric)
ER[cols.fact] <- sapply(ER[cols.fact],as.factor)
ER[cols.string] <- sapply(ER[cols.string],as.character)

italy <- map_data("italy", region = c("Bologna","Parma", "Modena", "Forli'","Reggio Emilia",
                                      "Ravenna","Ferrara", "Piacenza"))
choicesprovince <- c("Piacenza", "Parma", "Modena", "Reggio nell'Emilia", "Bologna", "Ferrara", "Ravenna", 
                     "Forlì-Cesena", "Rimini", "Estero", "Unknown")
choicessetor <- c("AGRICOLTURA, SILVICOLTURA E PESCA","ESTRAZIONE DI MINERALI DA CAVE E MINIERE","ATTIVITÀ MANIFATTURIERE",
                  "FORNITURA DI ENERGIA ELETTRICA, GAS, VAPORE E ARIA CONDIZIONATA","COSTRUZIONI",
                  "COMMERCIO ALL'INGROSSO E AL DETTAGLIO; RIPARAZIONE DI AUTOVEICOLI E MOTOCICLI",
                  "TRASPORTO E MAGAZZINAGGIO","ATTIVITÀ DEI SERVIZI DI ALLOGGIO E DI RISTORAZIONE",
                  "SERVIZI DI INFORMAZIONE E COMUNICAZIONE","ATTIVITÀ FINANZIARIE E ASSICURATIVE",
                  "ATTIVITA' IMMOBILIARI", "ATTIVITÀ PROFESSIONALI, SCIENTIFICHE E TECNICHE",
                  "NOLEGGIO, AGENZIE DI VIAGGIO, SERVIZI DI SUPPORTO ALLE IMPRESE",
                  "AMMINISTRAZIONE PUBBLICA E DIFESA; ASSICURAZIONE SOCIALE OBBLIGATORIA","ISTRUZIONE",
                  "SANITA' E ASSISTENZA SOCIALE","ATTIVITÀ ARTISTICHE, SPORTIVE, DI INTRATTENIMENTO E DIVERTIMENTO",
                  "ALTRE ATTIVITÀ DI SERVIZI",
                  "ATTIVITÀ DI FAMIGLIE E CONVIVENZE COME DATORI DI LAVORO PER PERSONALE DOMESTICO; PRODUZIONE DI BENI E SERVIZI INDIFFERENZIATI PER USO PROPRIO DA PARTE DI FAMIGLIE E CONVIVENZE",
                  "ORGANIZZAZIONI ED ORGANISMI EXTRATERRITORIALI")
choicessubsetor <- list(
    "AGRICOLTURA, SILVICOLTURA E PESCA" = c("COLTIVAZIONI AGRICOLE E PRODUZIONE DI PRODOTTI ANIMALI, CACCIA E SERVIZI CONNESSI",
                                            "SILVICOLTURA ED UTILIZZO DI AREE FORESTALI",
                                            "PESCA E ACQUACOLTURA"),
    "ESTRAZIONE DI MINERALI DA CAVE E MINIERE" = c("ESTRAZIONE DI CARBONE (ESCLUSA TORBA)", 
                                                   "ESTRAZIONE DI PETROLIO GREGGIO E DI GAS NATURALE",
                                                   "ESTRAZIONE DI MINERALI METALLIFERI",
                                                   "ALTRE ATTIVITÀ DI ESTRAZIONE DI MINERALI DA CAVE E MINIERE",
                                                   "ATTIVITÀ DEI SERVIZI DI SUPPORTO ALL'ESTRAZIONE"), 
    "ATTIVITÀ MANIFATTURIERE" = c("INDUSTRIE ALIMENTARI",
                                  "INDUSTRIA DELLE BEVANDE",
                                  "INDUSTRIA DEL TABACCO",
                                  "INDUSTRIE TESSILI",
                                  "CONFEZIONE DI ARTICOLI DI ABBIGLIAMENTO; CONFEZIONE DI ARTICOLI IN PELLE E PELLICCIA",
                                  "FABBRICAZIONE DI ARTICOLI IN PELLE E SIMILI",
                                  "INDUSTRIA DEL LEGNO E DEI PRODOTTI IN LEGNO E SUGHERO (ESCLUSI I MOBILI); FABBRICAZIONE DI ARTICOLI IN PAGLIA E MATERIALI DA INTRECCIO",
                                  "FABBRICAZIONE DI CARTA E DI PRODOTTI DI CARTA",
                                  "STAMPA E RIPRODUZIONE DI SUPPORTI REGISTRATI",
                                  "FABBRICAZIONE DI COKE E PRODOTTI DERIVANTI DALLA RAFFINAZIONE DEL PETROLIO",
                                  "FABBRICAZIONE DI PRODOTTI CHIMICI",
                                  "FABBRICAZIONE DI PRODOTTI FARMACEUTICI DI BASE E DI PREPARATI FARMACEUTICI",
                                  "FABBRICAZIONE DI ARTICOLI IN GOMMA E MATERIE PLASTICHE",
                                  "FABBRICAZIONE DI ALTRI PRODOTTI DELLA LAVORAZIONE DI MINERALI NON METALLIFERI",
                                  "METALLURGIA",
                                  "FABBRICAZIONE DI PRODOTTI IN METALLO (ESCLUSI MACCHINARI E ATTREZZATURE)",
                                  "FABBRICAZIONE DI COMPUTER E PRODOTTI DI ELETTRONICA E OTTICA; APPARECCHI ELETTROMEDICALI, APPARECCHI DI MISURAZIONE E DI OROLOGI",
                                  "FABBRICAZIONE DI APPARECCHIATURE ELETTRICHE ED APPARECCHIATURE PER USO DOMESTICO NON ELETTRICHE",
                                  "FABBRICAZIONE DI MACCHINARI ED APPARECCHIATURE NCA",
                                  "FABBRICAZIONE DI AUTOVEICOLI, RIMORCHI E SEMIRIMORCHI",
                                  "FABBRICAZIONE DI ALTRI MEZZI DI TRASPORTO",
                                  "FABBRICAZIONE DI MOBILI",
                                  "ALTRE INDUSTRIE MANIFATTURIERE",
                                  "RIPARAZIONE, MANUTENZIONE ED INSTALLAZIONE DI MACCHINE ED APPARECCHIATURE"),
    "FORNITURA DI ENERGIA ELETTRICA, GAS, VAPORE E ARIA CONDIZIONATA" = c("FORNITURA DI ENERGIA ELETTRICA, GAS, VAPORE E ARIA CONDIZIONATA",
                                                                          "RACCOLTA, TRATTAMENTO E FORNITURA DI ACQUA",
                                                                          "GESTIONE DELLE RETI FOGNARIE",
                                                                          "ATTIVITÀ DI RACCOLTA, TRATTAMENTO E SMALTIMENTO DEI RIFIUTI; RECUPERO DEI MATERIALI",
                                                                          "ATTIVITÀ DI RISANAMENTO E ALTRI SERVIZI DI GESTIONE DEI RIFIUTI"),
    "COSTRUZIONI" = c("COSTRUZIONE DI EDIFICI",
                      "INGEGNERIA CIVILE",
                      "LAVORI DI COSTRUZIONE SPECIALIZZATI"),
    "COMMERCIO ALL'INGROSSO E AL DETTAGLIO; RIPARAZIONE DI AUTOVEICOLI E MOTOCICLI" = c("COMMERCIO ALL'INGROSSO E AL DETTAGLIO E RIPARAZIONE DI AUTOVEICOLI E MOTOCICLI",
                                                                                        "COMMERCIO ALL'INGROSSO (ESCLUSO QUELLO DI AUTOVEICOLI E DI MOTOCICLI)",
                                                                                        "COMMERCIO AL DETTAGLIO (ESCLUSO QUELLO DI AUTOVEICOLI E DI MOTOCICLI)"),
    "TRASPORTO E MAGAZZINAGGIO" = c("TRASPORTO TERRESTRE E TRASPORTO MEDIANTE CONDOTTE",
                                    "TRASPORTO MARITTIMO E PER VIE D'ACQUA",
                                    "TRASPORTO AEREO",
                                    "MAGAZZINAGGIO E ATTIVITÀ DI SUPPORTO AI TRASPORTI",
                                    "SERVIZI POSTALI E ATTIVITÀ DI CORRIERE"),
    "ATTIVITÀ DEI SERVIZI DI ALLOGGIO E DI RISTORAZIONE" = c("ALLOGGIO",
                                                             "ATTIVITÀ DEI SERVIZI DI RISTORAZIONE"),
    "SERVIZI DI INFORMAZIONE E COMUNICAZIONE" = c("ATTIVITÀ EDITORIALI",
                                                  "ATTIVITÀ DI PRODUZIONE CINEMATOGRAFICA, DI VIDEO E DI PROGRAMMI TELEVISIVI, DI REGISTRAZIONI MUSICALI E SONORE",
                                                  "ATTIVITÀ DI PROGRAMMAZIONE E TRASMISSIONE",
                                                  "TELECOMUNICAZIONI",
                                                  "PRODUZIONE DI SOFTWARE, CONSULENZA INFORMATICA E ATTIVITÀ CONNESSE",
                                                  "ATTIVITÀ DEI SERVIZI D'INFORMAZIONE E ALTRI SERVIZI INFORMATICI"),
    "ATTIVITÀ FINANZIARIE E ASSICURATIVE" = c("ATTIVITÀ DI SERVIZI FINANZIARI (ESCLUSE LE ASSICURAZIONI E I FONDI PENSIONE)",
                                              "ASSICURAZIONI, RIASSICURAZIONI E FONDI PENSIONE (ESCLUSE LE ASSICURAZIONI SOCIALI OBBLIGATORIE)",
                                              "ATTIVITÀ AUSILIARIE DEI SERVIZI FINANZIARI E DELLE ATTIVITÀ ASSICURATIVE"),
    "ATTIVITA' IMMOBILIARI" = c("ATTIVITA' IMMOBILIARI"),
    "ATTIVITÀ PROFESSIONALI, SCIENTIFICHE E TECNICHE" = c("ATTIVITÀ LEGALI E CONTABILITÀ",
                                                          "ATTIVITÀ DI DIREZIONE AZIENDALE E DI CONSULENZA GESTIONALE",
                                                          "ATTIVITÀ DEGLI STUDI DI ARCHITETTURA E D'INGEGNERIA; COLLAUDI ED ANALISI TECNICHE",
                                                          "RICERCA SCIENTIFICA E SVILUPPO",
                                                          "PUBBLICITÀ E RICERCHE DI MERCATO",
                                                          "ALTRE ATTIVITÀ PROFESSIONALI, SCIENTIFICHE E TECNICHE",
                                                          "SERVIZI VETERINARI"),
    "NOLEGGIO, AGENZIE DI VIAGGIO, SERVIZI DI SUPPORTO ALLE IMPRESE" = c("ATTIVITÀ DI NOLEGGIO E LEASING OPERATIVO",
                                                                         "ATTIVITÀ DI RICERCA, SELEZIONE, FORNITURA DI PERSONALE",
                                                                         "ATTIVITÀ DEI SERVIZI DELLE AGENZIE DI VIAGGIO, DEI TOUR OPERATOR E SERVIZI DI PRENOTAZIONE E ATTIVITÀ CONNESSE",
                                                                         "SERVIZI DI VIGILANZA E INVESTIGAZIONE",
                                                                         "ATTIVITÀ DI SERVIZI PER EDIFICI E PAESAGGIO",
                                                                         "ATTIVITÀ DI SUPPORTO PER LE FUNZIONI D'UFFICIO E ALTRI SERVIZI DI SUPPORTO ALLE IMPRESE"),
    "AMMINISTRAZIONE PUBBLICA E DIFESA; ASSICURAZIONE SOCIALE OBBLIGATORIA" = c("AMMINISTRAZIONE PUBBLICA E DIFESA; ASSICURAZIONE SOCIALE OBBLIGATORIA"),
    "ISTRUZIONE" = c("ISTRUZIONE"),
    "SANITA' E ASSISTENZA SOCIALE" = c("ASSISTENZA SANITARIA",
                                       "SERVIZI DI ASSISTENZA SOCIALE RESIDENZIALE",
                                       "ASSISTENZA SOCIALE NON RESIDENZIALE"),
    "ATTIVITÀ ARTISTICHE, SPORTIVE, DI INTRATTENIMENTO E DIVERTIMENTO" = c("ATTIVITÀ CREATIVE, ARTISTICHE E DI INTRATTENIMENTO",
                                                                           "ATTIVITÀ DI BIBLIOTECHE, ARCHIVI, MUSEI ED ALTRE ATTIVITÀ CULTURALI",
                                                                           "ATTIVITÀ RIGUARDANTI LE LOTTERIE, LE SCOMMESSE, LE CASE DA GIOCO",
                                                                           "ATTIVITÀ SPORTIVE, DI INTRATTENIMENTO E DI DIVERTIMENTO"),
    "ALTRE ATTIVITÀ DI SERVIZI" = c("ATTIVITÀ DI ORGANIZZAZIONI ASSOCIATIVE",
                                    "RIPARAZIONE DI COMPUTER E DI BENI PER USO PERSONALE E PER LA CASA",
                                    "ALTRE ATTIVITÀ DI SERVIZI PER LA PERSONA"),
    "ATTIVITÀ DI FAMIGLIE E CONVIVENZE COME DATORI DI LAVORO PER PERSONALE DOMESTICO; PRODUZIONE DI BENI E SERVIZI INDIFFERENZIATI PER USO PROPRIO DA PARTE DI FAMIGLIE E CONVIVENZE" = c("ATTIVITÀ DI FAMIGLIE E CONVIVENZE COME DATORI DI LAVORO PER PERSONALE DOMESTICO",
                                                                                                                                                                                          "PRODUZIONE DI BENI E SERVIZI INDIFFERENZIATI PER USO PROPRIO DA PARTE DI FAMIGLIE E CONVIVENZE"),
    "ORGANIZZAZIONI ED ORGANISMI EXTRATERRITORIALI" = c("ORGANIZZAZIONI ED ORGANISMI EXTRATERRITORIALI"))

#App
ui <- fluidPage(
    navbarPage(h4("Aziende di Emilia-Romagna"),
               tabPanel(h5("Sezione filtro")%>%em(),
                        uiOutput('page1'),
                        uiOutput('areasp')),
               tabPanel(h5("Sezione cluster")%>%em(), uiOutput('page2'))),
    theme = shinythemes::shinytheme("flatly")
)

server <- function(input, output) {
    #INITIAL MESSAGE
    showModal(modalDialog(
        title = h3("Messaggio importante"),
        HTML("Il tempo per eseguire questa applicazione gratuitamente è limitato, quindi per favore chiudi la 
            finestra una volta che hai finito la tua ricerca.
             <br><br>"),
        easyClose = TRUE,
        icon("user-clock", "fa-6x", lib = "font-awesome")
    ))
    
    #PAGES
    #PAGE1
    output$areasp <- renderUI({
        sidebarLayout(
            sidebarPanel(
                p("(Non è obbligatorio modificarlo)"),
                pickerInput("settore_liv2", h4("Area specifica del settore"), 
                            #choices = choicessubsetor,
                            choices = choicessubsetor[which(choicessetor %in% input$settore_liv1)],
                            multiple = T,
                            options = list(
                                `actions-box` = TRUE,
                                size = 10,
                                `selected-text-format` = "count > 1"
                            ))),
            mainPanel(),
        )
    })
    output$page1 <- renderUI({
        sidebarLayout(
            sidebarPanel(
                h2("Criteri di filtro", align ="center"),
                chooseSliderSkin("Round"),
                setSliderColor("#0170B4", 1),
                sliderInput("employees", h4("Seleziona l'intervallo del numero di dipendenti"),
                            value = c(10, 20000), min = 10, max = 20000,
                            ticks = F),
                pickerInput("forma_giuridica", h4("Forma giuridica"),
                            choices = unique(ER$forma_giuridica), multiple = T,
                            selected = unique(ER$forma_giuridica),
                            options = list(
                                `actions-box` = TRUE,
                                size = 10,
                                `selected-text-format` = "count > 1"
                            )),
                pickerInput("province", h4("Provincia"),
                            choices = choicesprovince, multiple = T,
                            selected = choicesprovince,
                            options = list(
                                `actions-box` = TRUE,
                                size = 10,
                                `selected-text-format` = "count > 1"
                            )),
                materialSwitch("web", strong("Deve avere un sito web"), value = F, status = "primary"),
                materialSwitch("telephone", strong("Deve avere un telefono"), value = F, status = "primary"),
                pickerInput("settore_liv1", h4("Settore industriale"),
                            choices = choicessetor, multiple = T,
                            options = list(
                                `actions-box` = TRUE,
                                size = 10,
                                `selected-text-format` = "count > 1"),
                            choicesOpt = list(
                                style = rep_len("font-size: 90%; line-height: 1.6;", length(choicessetor))))),
            mainPanel(
                actionButton("calculate", "Go!",
                             icon("paper-plane"), 
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                downloadButton('downloadDataFilt', 'Scaricare i dati',
                               style="color: #fff; background-color: #48A003; border-color: #3B8D03"),
                actionButton("help", "Help",
                             icon("question", lib = "font-awesome"), 
                             style="color: #fff; background-color: #EBDA05; border-color: #A49804"),
                p(),
                tabsetPanel(
                    tabPanel("Mappa",
                             plotly::plotlyOutput("map") %>% withSpinner(color="#0dc5c1"),
                             p("Visualizzazione limitata a 1000 aziende. La posizione è la loro sede
                               legale, quindi ci possono essere alcuni punti di dati al di fuori dei confini previsti")),
                    tabPanel("Grafico",
                             plotly::plotlyOutput("filt_data") %>% withSpinner(color="#0dc5c1")),
                    tabPanel("Dataset",
                             DT::DTOutput("filt") %>% withSpinner(color="#0dc5c1"))
                )
            )
        )
    })
    #PAGE2
    output$page2 <- renderUI({
        sidebarLayout(
            sidebarPanel(
                h2("Trova aziende simili", align = "center"),
                textInput("codf", h4("Scrivi il CF dell'azienda che ti interessa:")),
                pickerInput("province2", h4("Provincia"),
                            choices = choicesprovince, multiple = T,
                            selected = choicesprovince,
                            options = list(
                                `actions-box` = TRUE,
                                size = 10,
                                `selected-text-format` = "count > 1"
                            )),
                sliderTextInput(
                    inputId = "sector_match",
                    label = h4("Volete che la corrispondenza sul settore sia ampia o ristretta?"),
                    force_edges = TRUE,
                    choices = c("Ampia","Intermedia","Ristretta"),
                    selected = "Intermedia"
                ),
                p(br()),
                h4("Numero massimo di aziende simili che vuoi"),
                div(style = "margin: auto; width: 60%",
                    knobInput("ncomp","",
                              value = 15, min = 1, max = 50, displayPrevious = TRUE,
                              lineCap = "round", fgColor = "#428BCA", inputColor = "#428BCA"))
            ),
            mainPanel(
                actionButton("calculate2", "Go!",
                             icon("paper-plane"), 
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                downloadButton('downloadDataclust', 'Scaricare i dati',
                               style="color: #fff; background-color: #48A003; border-color: #3B8D03"),
                actionButton("help2", "Help",
                             icon("question", lib = "font-awesome"), 
                             style="color: #fff; background-color: #EBDA05; border-color: #A49804"),
                p(),
                tabsetPanel(
                    tabPanel("Mappa",
                             plotly::plotlyOutput("map2")),
                    tabPanel("Grafico",
                             plotly::plotlyOutput("filt_data2")),
                    tabPanel("Dataset",
                             DT::DTOutput("filt2") %>% withSpinner(color="#0dc5c1")
                    )
                )
            ) 
        )
    })
    #COMPUTATIONS
    #FOR PAGE 1
    #general computations
    filtered_data <- eventReactive(input$calculate, {
        ER %>% filter(dipendenti>=input$employees[1],#employees
                      dipendenti<=input$employees[2],
                      if(input$web==T){           #has website  
                          is.na(website)==F
                      } else {
                          ragione_sociale != "no_filter_condition"
                      },
                      if(input$telephone==T){           #has phone  
                          is.na(telefono)==F
                      } else {
                          ragione_sociale != "no_filter_condition"
                      },
                      forma_giuridica %in% input$forma_giuridica,
                      provincia %in% input$province,
                      if(is.null(input$settore_liv1)){
                          ragione_sociale != "no_filter_condition"}
                      else {
                          settore_liv1 %in% input$settore_liv1
                      },
                      if(is.null(input$settore_liv2)){
                          ragione_sociale != "no_filter_condition"}
                      else {
                          settore_liv2 %in% input$settore_liv2
                      }
        )
    })
    dimensions <- reactive({
        dim(filtered_data())[1]
    })
    perfected_data1 <- reactive({
        seldat <- filtered_data() %>% 
            select(ragione_sociale, CF, provincia, dipendenti, ricavi_vendite, utile_netto, 
                   settore_liv3, forma_giuridica, website, telefono, indirizzo, CAP, comune)
        names(seldat) <- c("Ragione sociale", "Codice fiscale", "Sede operativa - Provincia", "Dipendenti\nUltimo anno disp.",
                           "Ricavi delle vendite\nmil EUR\nUltimo anno disp.", "Utile Netto\nmigl EUR\nUltimo anno disp.",
                           "Settore", "Forma giuridica", "Website", "Numero di telefono","Sede operativa - Indirizzo",
                           "Codice postale", "Sede operativa - Comune")
        seldat
    })
    
    #plot turnover
    output$filt_data <- plotly::renderPlotly({
        data <- filtered_data() %>% head(1000) #maximum to plot in case there are too many
        ggplot(data) +
            geom_point(aes(dipendenti, ricavi_vendite, color = dipendenti*ricavi_vendite)) +
            scale_color_gradient(low = "#F0CCC7", high = "#FC2103", na.value = NA)+
            theme_economist() +
            labs(title= "Dimensione delle aziende selezionate",
                 y="Ricavi vendite (millione EUR)", x = "Dipendenti")+
            theme(legend.position = "none")
    })
    #table of filtered data
    output$filt <- DT::renderDT({
        DT::datatable(perfected_data1())
    })
    #plot italy
    italymerge <- reactive({
        left_join(italy, filtered_data()%>%count(region), by = "region")})
    
    output$map <- plotly::renderPlotly({
        data <- filtered_data()
        ggplot() + 
            geom_polygon(data = italymerge(), aes(x=long, y = lat, group = group, fill = n), 
                         color = "black") + 
            coord_fixed(1.3) +
            theme_map(base_size = 10, base_family = "") +
            theme(legend.position = c(0.75,0.57)) +
            scale_fill_gradient(low = "#E2F0FA", high = "#3A5161", na.value = NA) +
            labs(title= "Ubicazione delle aziende selezionate in Emilia-Romagna") +
            theme(plot.title = element_text(size=20, family = "sans", face = "bold")) +
            geom_point(data = data%>%head(1000), aes(longitudine, latitudine, color = dipendenti)) +
            scale_color_gradient(low = "#F0CCC7", high = "#FC2103", na.value = NA)
    })
    #download dataset
    output$downloadDataFilt <- downloadHandler(
        filename = function() { 
            paste("filter", dimensions(), "_", format(Sys.Date(), format="%d-%m-%Y"), ".xlsx", sep="")
        },
        content = function(file) {
            write_xlsx(perfected_data1(), file)
        }
    )
    #pop up message
    observeEvent(input$calculate, {
        showModal(modalDialog(
            HTML(paste("Ci sono", em(strong(dimensions())), em(strong("aziende")) ,"con queste caratteristiche.","<br><br>",
                       "I suoi filtri sono:", "<br>","- ",code("Dipendenti") ,"tra", input$employees[1], "e",
                       input$employees[2], "<br>", "- Deve avere un sito", code("web"), "=", input$web, "<br>",
                       "- Deve avere un", code("telefono"), "=", input$telephone, "<br>","-", length(input$forma_giuridica),
                       "selezioni di", code("forma giuridica"), "<br>" ,"-", length(input$province), 
                       "selezioni di", code("provincia"), "<br>","-",
                       ifelse(is.null(input$settore_liv1), length(choicessetor),length(input$settore_liv1)),
                       "selezioni di", code("industria")))))
    })
    #help button
    observeEvent(input$help, {
        showModal(modalDialog(
            HTML(paste("Questa sezione è utile per filtrare il set di dati iniziale e ottenere la selezione desiderata. 
                       Clicca su \"Go!\" prima di tentare di scaricare il database.",
                       "<br><br>",
                       " Il database grezzo ha 16422 aziende, che sono tutte le aziende attive con più di 10 dipendenti 
                       che sono salvate nel database", '<a href="https://www.bvdinfo.com/en-gb/our-products/data/national/aida">AIDA</a>',".",
                       "<br><br>",
                       "Il set di dati è stato recuperato il 20/03/2021"))))
    })
    
    #FOR PAGE 2
    #general computations
    clustered_data <- eventReactive(input$calculate2, {
        company_index <- ER$CF == input$codf
        ERS <- ER[-company_index,]
        
        if(input$sector_match == "Ampia"){
            company_sector <- ER$settore_liv1[company_index]
            come_company <- ERS[ERS$settore_liv1 == company_sector,]
        } else if(input$sector_match == "Intermedia"){
            company_sector <- ER$settore_liv2[company_index]
            come_company <- ERS[ERS$settore_liv2 == company_sector,]
        } else{
            company_sector <- ER$settore_liv3[company_index]
            come_company <- ERS[ERS$settore_liv3 == company_sector,]
        }
        
        filtered <- come_company %>% filter(provincia %in% input$province2)
        ready <- rbind(filtered, ER[company_index,])
        
        scaled_ERF <- ready %>% 
            transmute(
                scale(dipendenti),
                scale(ricavi_vendite))
        rownames(scaled_ERF) <- seq(1:nrow(scaled_ERF))
        
        simComp <- rep(NA, nrow(scaled_ERF))
        for (i in 1:nrow(scaled_ERF)){
            dista <- euclidean(t(scaled_ERF[i,]), t(scaled_ERF[nrow(scaled_ERF),]), testNA = F)
            simComp[i] <- dista
        }
        names(simComp) <- 1:length(simComp)
        
        orderComp <- order(simComp)
        comptop <- simComp[orderComp] %>% head(input$ncomp + 1) %>% names()
        return(ready[as.numeric(comptop),])

    })
    name_company <-reactive({
        ER[ER$CF == input$codf,"ragione_sociale"]
    })
    dim_cluster <- reactive({
        nrow(clustered_data())-1
    })
    perfected_data2 <- reactive({
        seldat2 <- clustered_data() %>% 
            select(ragione_sociale, CF, provincia, dipendenti, ricavi_vendite, utile_netto, 
                   settore_liv3, forma_giuridica, website, telefono, indirizzo, CAP, comune)
        names(seldat2) <- c("Ragione sociale", "Codice fiscale", "Sede operativa - Provincia", "Dipendenti\nUltimo anno disp.",
                            "Ricavi delle vendite\nmil EUR\nUltimo anno disp.", "Utile Netto\nmigl EUR\nUltimo anno disp.",
                            "Settore", "Forma giuridica", "Website", "Numero di telefono","Sede operativa - Indirizzo",
                            "Codice postale", "Sede operativa - Comune")
        unique(seldat2)
    })
    #Map of italy
    italymerge2 <- reactive({
        left_join(italy, clustered_data()%>%count(region), by = "region")})
    
    output$map2 <- plotly::renderPlotly({
        data3 <- clustered_data() %>% 
            select(ragione_sociale, longitudine, latitudine,dipendenti,CF) 
        ggplot(data3) + 
            geom_polygon(data = italymerge2(), aes(x=long, y = lat, group = group, fill = n), 
                         color = "black") + 
            coord_fixed(1.3) +
            theme_map(base_size = 10, base_family = "") +
            theme(legend.position = c(0.75,0.57)) +
            scale_fill_gradient(low = "#E2F0FA", high = "#3A5161", na.value = NA) +
            labs(title= "Ubicazione dei cluster di aziende in Emilia-Romagna") +
            theme(plot.title = element_text(size=20, family = "sans", face = "bold")) +
            geom_point(aes(longitudine, latitudine, color = dipendenti)) +
            scale_color_gradient(low = "#F0CCC7", high = "#FC2103", na.value = NA) +
            geom_text(aes(longitudine, latitudine,label=word(ragione_sociale,1)), size=4, 
                      position = position_nudge(y = +0.01))+
            geom_point(data= data3%>%filter(CF==input$codf), aes(longitudine, latitudine), shape=23,
                       color = "black", fill="blue", size=3)
        
    })
    #plot turnover
    output$filt_data2 <- plotly::renderPlotly({
        data <- clustered_data() #maximum to plot in case there are too many
        ggplot(data) +
            geom_point(aes(dipendenti, ricavi_vendite, color = dipendenti*ricavi_vendite)) +
            scale_color_gradient(low = "#F0CCC7", high = "#FC2103", na.value = NA)+
            theme_economist() +
            labs(title= "Dimensione delle aziende clusterizzate",
                 y="Ricavi vendite (millione EUR)", x = "Dipendenti")+
            theme(legend.position = "none")+
            geom_text(aes(dipendenti, ricavi_vendite,label=word(ragione_sociale,1)), size=4)+
            geom_point(data= data%>%filter(CF==input$codf), aes(dipendenti, ricavi_vendite), shape=23,
                       color = "black", fill="blue", size=3)
    })
    #datatable
    output$filt2 <- DT::renderDT({
        DT::datatable(perfected_data2())
    })
    #download dataset
    output$downloadDataclust <- downloadHandler(
        filename = function() { 
            paste("cluster", input$codf, "_", format(Sys.Date(), format="%d-%m-%Y"), ".xlsx", sep="")
        },
        content = function(file) {
            write_xlsx(perfected_data2(), file)
        }
    )
    #pop up message
    observeEvent(input$calculate2, {
        showModal(modalDialog(
            HTML(paste("L'azienda che avete selezionato è", code(name_company()), ". Abbiamo trovato le", strong(em(dim_cluster())),
                       em("aziende"),"più simili in termini di fatturato e numero di dipendenti, dati i vincoli di settore e di ubicazione.",
                       "<br><br>",
                       "I suoi filtri sono:", "<br>","- ", length(input$province2),
                       "selezioni di", code("provincia"), "<br>","- Corrispondenza =", code(input$sector_match)))))
    })
    #help button
    observeEvent(input$help2, {
        showModal(modalDialog(
            HTML("Questa sezione ti aiuta a trovare aziende simili a quella che definisci. Filtra il set di dati secondo 
            le province selezionate e fa una corrispondenza su uno dei 3 livelli di industria definiti da ATECO 2007, 
            che si seleziona con il cursore \"corrispondenza\". In seguito, calcola la somiglianza con le aziende filtrate in 
            termini di fatturato e numero di dipendenti con la distanza euclidea.
            <br><br>
            Se non conosci il CF (codice fiscale) dell'azienda a cui 
            sei interessato, cerca la tua azienda nella tabella della sezione filtri.
            <br><br>
            Il set di dati è stato recuperato il 20/03/2021.")))
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
