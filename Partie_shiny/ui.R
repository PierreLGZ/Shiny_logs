library(shiny)
library(shinythemes)
library(ggplot2)
library(highcharter)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(stringi)
library(tidytext)
library(DT)
library(shinyjs)
library(magrittr)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  #shinythemes::themeSelector(),
  theme = shinytheme("united"),
  useShinydashboard(),
  
  # Pour choisir soi même une couleur de fond
  setBackgroundColor(
    color = "#FBF1EF",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE),
  
  HTML("<h1>Analyse des données de sécurité</h1>"),
  
  # Menu de gauche
  navbarPage(title = "", 
             tags$style(HTML("
        .navbar-default .navbar-nav > li > a[data-value='Données'] {color: white;} 
        .navbar-default .navbar-nav > li > a[data-value='ACCUEIL'] {color: white;} 
        .navbar-default .navbar-nav > li > a[data-value='Analyse Exploratoire'] {color: white;}
        .navbar-default .navbar-nav > li > a[data-value='Clustering'] {color: white;}
                  ")), 
             
             # 1er volet
             tabPanel("Accueil", icon = icon("desktop"),
                      sidebarLayout(
                        sidebarPanel(
                          style = "position:relative; background:#FBF1EF",
                          h1(strong("Context"), style = "text-align:left;font-size:30px;"),
                          p("Ce dashboard a été crée dans le cadre du challenge de visualisation et analyse de données de sécurité fait par les étudiants des masters SISE (Data Science) 
                              et OPSIE (Sécurité Informatique).Le but final de ce challenge est la création d'une application permettant de visualiser 
                              des données provenants des logs d'un firewall et de permettre une prise de décision rapide et efficace.",style="text-align:justify"),
                          p("Ce challenge a été relevé en un jour et demi."),
                          br(),
                          div(img(src = "securite-informatique-pme.png"),style="text-align: center;")
                        ),
                        mainPanel(style = "position:relative; background:#FBF1EF",
                                  wellPanel(
                                    style="background:#FBF1EF",
                                    h1(strong("Précisions"), style = "text-align:left;font-size:30px;"),
                                    p("Le projet de 2022 aborde donc des problématiques classiques et surtout connues reposant généralement sur
                           la fouille de données et la détection des intrusions."),
                                    p("Ce dernier s'est déroulé comme suit : ", br()," • Le rôle des étudiants d'OPSIE était de générée la base de données au format csv et de réaliser une revue d’utilisation
                           des règles d’un Firewall en prenant en compte une migration de ces règles vers un autre Firewall de même technologie. En résumé
                           une analyse de règles a été faite à partir du log de Firewall IPTABLES de type on cloud et une migration de ces dernières fut réaliser sur un Firewall de type IPTABLES stand alone."),
                                    p("• Les étudiants de SISE quant à eux se sont occupés de l'application dynamique sous R Shiny et de l'affichage d'information pertinentes. On retrouvera dans dedans :"),
                                    p("- Un classement des règles les plus utilisées.",style="margin-left:2em"),
                                    p("- Une analyse descriptive des flux rejetés et autorisés par protocoles (TCP, UDP).",style="margin-left:2em"),
                                    p("- Une visualisation interactive des données.",style="margin-left:2em"),
                                    p("- Un rapprochement des règles par rapport aux ports de destination et aux action (uniquement sur le protocole TCP).",style="margin-left:2em"),
                                  )
                        )
                      ), br(),br(),br(),br(), br(),br(),br(),br(), br(),
                      p(strong("Equipe SISE :"), span("Pierre LE GALEZE - Hamza FASSI - Sami AIT TILAT /"),
                        strong("Equipe OPSIE :"), span("Bachir OUFAQUIR - Badereddine TOUATI /"),
                        strong("Professeurs référents :"), span("David PIERROT - Ricco RAKOTOMALALA"),style="text-align: center;")
             ),           
             # 2ème volet : affichage des données                     
             tabPanel("Données", icon = icon("table"),
                      tabsetPanel(fluidRow(column(7, offset = 1, dataTableOutput("df2", width = "150%", height = "400px")
                                 )
                        ),
                      )
             ),         
             # 3ème volet : analyse exploratoire
             tabPanel("Analyse exploratoire", icon=icon("chart-pie"),
                      tabsetPanel(
                        tabPanel("Utilisation des règles",
                                 
                                 sidebarPanel(
    
                                   sliderInput(
                                     inputId = "reg_pro",
                                     label = "Affiche toute regle ayant une utilisation supérieur à ",
                                     min = 1,
                                     max = n_rules,
                                     value = 500,
                                     step = 50
                                   ),
                                   
                                   selectInput(inputId = "Resultat", label = "Resultat selon", choices = c("proto","action"),selected="action"),
                                   
                                  ),
                                   
                                 mainPanel(tabsetPanel(
                                     tabPanel("Graphique",plotlyOutput("reg_desc_1")),
                                     tabPanel("Tableau",dataTableOutput('table_reg_desc_1'))
                                   ))
                        ),
                        tabPanel("Solicitation des protocoles en fonction des ports",
                                 sidebarPanel(
                                   sliderInput(
                                     inputId = "top_ports",
                                     label = "Affiche tout port ayant une utilisation supérieur à",
                                     min = 1,
                                     max = n_port,
                                     value = 300,
                                     step = 5
                                   ),
                                   checkboxGroupInput(
                                     inputId="action_choice_p",
                                     label="Choix de l'action",
                                     choices = c("permit","deny"),
                                     selected = c("permit")
                                     
                                   ),
                                   sliderInput("range_ports", label = "Plage de ports", min = 0, 
                                               max = 65535, value = c(0, 1024)
                                   )
                                   
                                   
                                 ),
                                 mainPanel(tabsetPanel(
                                   tabPanel("Graphiques",plotlyOutput("port_desc"),box(solidHeader = T,
                                                            width = 20,
                                                            fluidRow(
                                                              column(width=6, h4("TCP", align = 'center'), plotlyOutput("act_tcp")),
                                                              column(width=6, h4("UDP", align = 'center'), plotlyOutput("act_udp"))
                                                            ))),tabPanel("Tableau",dataTableOutput('table_port_desc'))
                                   
                                 )
                                 )
                        ),
                        
                        
                        
                        tabPanel("Top IP",
                                 sidebarLayout(sidebarPanel(
                                   sliderInput(
                                     'top_ip',
                                     "Nombre d'IP",
                                     min = 1,
                                     max = n_ip,
                                     value = 5,
                                     step = 1)
                                   
                                 ),
                                 mainPanel(tabsetPanel(tabPanel("Graphique",plotlyOutput("top_src")), 
                                           tabPanel("Tableau",dataTableOutput("table_ip"))))
                                 )
                        ),
                        tabPanel("Rapprochement des règles",
                                   sidebarPanel(
                                     
                                     checkboxGroupInput(
                                       inputId="action_choice_p2",
                                       label="Selection de l'action",
                                       choices = c("permit","deny"),
                                       selected = c("permit")
                                       
                                     ),
                                     pickerInput(
                                       inputId = "regles_selec",
                                       label = "Regles a selectionner",
                                       choices = unique(brutes$policyid),
                                       selected = c('999', '1', '3', '6','17'),
                                       multiple = TRUE, options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2",
                                                                       `count-selected-text` = "{0}/{1} regles")
                                       
                                     )
                                     
                                     
                                   ),
                                   mainPanel(
                                     plotlyOutput("rapp_regles")
                                   )), 
                        tabPanel('Adresses non inclues dans le plan d’adressage de l’Université', sidebarPanel(pickerInput(
                                     inputId = "id_type",
                                     label = "IP type :",
                                     choices = unique(brutes$ipsrc),
                                     selected = brutes[grep("^192.168.56.", brutes$ipsrc),]$ipsrc,
                                     multiple = TRUE, options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2",
                                                                     `count-selected-text` = "{0}/{1} IP")
                                   )),
                                   mainPanel(
                                     dataTableOutput("table_hp")
                                   )
                                   ), 
                        tabPanel("Heures de connexion",
                                 
                                 
                                 # selection du jour
                                 
                                 hr(),
                                 
                                 fluidRow(
                                   column(width=12, h4("Nombre d'acces autorises et refuses par heure", align = 'center'), plotlyOutput("temp_data_heure"))
                                 )  
                        ),
                        )
                        ),
             tabPanel("Data Mining", icon=icon("gears"),
                      tabsetPanel(
                        tabPanel("Clustering",sidebarLayout(sidebarPanel(
                          sliderInput(
                            inputId = "nb_clust",
                            label = "Nombre de clusters",
                            min = 2,
                            max = 10,
                            value = 5,
                            step = 1
                          ),
                          textInput(
                            inputId = "choose_clust",
                            label = "Analyse d'un cluster",
                            value = "1")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Dendrograme",plotOutput("dendogram",height=720))
                            ,tabPanel("Analyse d'un cluster",dataTableOutput('clusters_ana')),
                            tabPanel("Kmeans",plotOutput("km",height=650))
                
                          )
                          
                        )
                        )
                        
                        ), tabPanel("Graph des relations entre adresses IP",
                                    sidebarLayout(sidebarPanel(
                                      sliderInput(
                                        inputId = "sup_net",
                                        label = "Affiche tout nodes ayant un nombre de connexion supérieur à",
                                        min = 1,
                                        max = n_network,
                                        value = 1,
                                        step = 5
                                      )
                                    ),
                                    mainPanel(
                                      tabPanel("",visNetworkOutput("net",height=650))
                                      )))
             )
                      
               
             ),
             
                      )
             )
  )



