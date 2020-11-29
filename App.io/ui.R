# Packages ====
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(plotly)
options(scipen = 9999999, encoding = 'UTF-8')


# Ajouter le plancher et plafond pour l'abattement sur les frais pros
# Prévoir graphique avec courbes 2019 et 2020 sur la part d'impôt sur le revenu en fonction du revenu à situation identique


# UI conditionnelle frais pro P1 ====
parametres_fr1 <- tabsetPanel(
  id = "parametres_fr1",
  type = "hidden",
  tabPanel("Auto"
  ),
  tabPanel("Custom", 
           numericInput(inputId = "frais_custom_p1", label = "Frais pro", value = 500, min = 1, max = 50000000, step = 1)
  )
)

# UI conditionnelle P2 ====
parametres_couple <- tabsetPanel(
  id = "parametres_couple",
  type = "hidden",
  tabPanel("Célibataire"
  ),
  tabPanel("Couple", 
           numericInput(inputId = "rev_p2", label = "Revenus 2", value = 22000, min = 1, max = 50000000, step = 1),
           radioButtons(inputId = "type_frais_p2", label = "Frais profesionnel", choices = c("Forfait (10%)" = "Auto", "Frais réels" = "Custom"), inline = TRUE)
  )
)

# UI conditionnelle frais pro P2 ====
parametres_fr2 <- tabsetPanel(
  id = "parametres_fr2",
  type = "hidden",
  tabPanel("Auto"
  ),
  tabPanel("Custom", 
           numericInput(inputId = "frais_custom_p2", label = "Frais pro", value = 500, min = 1, max = 50000000, step = 1)
  )
)


# UI ====
ui <- dashboardPage(
  dashboardHeader(title = "Simulateur impôt sur le revenu"),
  
  dashboardSidebar(
    width = 220,
    sidebarMenu(tags$script(HTML("$('body').addClass('fixed');")),# Barre latérale fixée
                menuItem(text = "Dashboard", tabName = "synth_impot", icon = icon("dashboard"), selected = TRUE, startExpanded = TRUE),
                
                # Paramètres ====
                radioButtons(inputId = "sit_fam", label = "Situation", selected = "Célibataire", choices = c("Célibataire" = "Célibataire", "Couple" = "Couple"), inline = TRUE),
                numericInput(inputId = "nb_enf", label = "Enfant(s) à charge", value = 0, min = 0, max = 10, step = 1),
                numericInput(inputId = "rev_p1", label = "Revenus 1", value = 21000, min = 1, max = 50000000, step = 1),
                radioButtons(inputId = "type_frais_p1", label = "Frais profesionnel", choices = c("Forfait (10%)" = "Auto", "Frais réels" = "Custom"), inline = TRUE),
                # UI si frais réels sur P1
                parametres_fr1,
                # UI si couple
                parametres_couple,
                # UI si couple et frais réels sur P2
                parametres_fr2,
                
                
                menuItem("Progressivité de l'impôt", tabName = "progressivite"))),
  
  dashboardBody(useShinyjs(),
                tabItems(
                  tabItem(tabName = "synth_impot", useShinyjs(),
                          box(title = "Impôt 2021 sur les revenus 2020", id = "P1", status = "warning", width = 12,
                              box(title = "Personne 1", width = 12,
                                  box(valueBoxOutput("Taux_solo_p1_2020", width = 4),
                                      valueBoxOutput("Mt_solo_p1_2020", width = 4),
                                      valueBoxOutput("Gain_p1_2020", width = 4),
                                      valueBoxOutput("Taux_duo_p1_2020", width = 3),
                                      valueBoxOutput("Mt_duo_com_p1_2020", width = 3),
                                      valueBoxOutput("Taux_duo_ind_p1_2020", width = 3),
                                      valueBoxOutput("Mt_duo_ind_p1_2020", width = 3), width = 8, title = "Taux d'imposition et montant payé"),
                                  box(plotlyOutput("graph_decompos_p1", height = 246), width = 4, title = "Décomposition de l'impôt pour la 1e personne")),
                              box(title = "Personne 2", id = "P2", width = 12,
                                  box(valueBoxOutput("Taux_solo_p2_2020", width = 4),
                                      valueBoxOutput("Mt_solo_p2_2020", width = 4),
                                      valueBoxOutput("Gain_p2_2020", width = 4),
                                      valueBoxOutput("Taux_duo_p2_2020", width = 3),
                                      valueBoxOutput("Mt_duo_com_p2_2020", width = 3),
                                      valueBoxOutput("Taux_duo_ind_p2_2020", width = 3),
                                      valueBoxOutput("Mt_duo_ind_p2_2020", width = 3), width = 8, title = "Taux d'imposition et montant payé"),
                                  box(plotlyOutput("graph_decompos_p2", height = 246), width = 4, title = "Décomposition de l'impôt pour la 2e personne")),
                              box(title = "Famille", id = "famille", width = 12,
                                  box(valueBoxOutput("Taux_com_2020", width = 4),
                                      valueBoxOutput("Mt_com_2020", width = 4),
                                      valueBoxOutput("Gain_com_2020", width = 4), width = 8, title = "Taux d'imposition et montant payé"),
                                  box(plotlyOutput("graph_decompos_duo", height = 246), width = 4, title = "Décomposition de l'impôt pour les 2 personnes")))),
                  tabItem(tabName = "progressivite",
                          h2("graphique interactif"),
                          plotlyOutput("progressivite_2020",height = 600))
                )
  )
)


