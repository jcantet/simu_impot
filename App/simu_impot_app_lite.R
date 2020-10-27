# Packages ====
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(ggplot2)
options(scipen = 9999999)


# Ajouter le plancher et plafond pour l'abattement sur les frais pros
# Prévoir tableau de sortie, avec 2019 et 2020.
# Prévoir graphique avec courbes 2019 et 2020 sur la part d'impôt sur le revenu en fonction du revenu à situation identique
# Bug sur les montants quand enfants
# Tri à faire sur les outputs


# Fonctions génériques ====
source("./easy_format.R", local = TRUE) # Fonction pour mettre en forme les résultats
source("./calcul_impot.R", local = TRUE) # Fonction pour calculer les impôts bruts 2019

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




## app.R ##====

# UI ====
ui <- dashboardPage(
  dashboardHeader(title = "Simulateur impôt sur le revenu"),
  
  dashboardSidebar(
    width = 220,
    sidebarMenu(tags$script(HTML("$('body').addClass('fixed');"))), # Barre latérale fixée
    
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
    parametres_fr2
    ),
  
  dashboardBody(
    box(title = "2019", status = "primary", width = 12,
    fluidRow(useShinyjs(),
      valueBoxOutput("Taux_solo_p1_2019", width = 2),
      valueBoxOutput("Mt_solo_p1_2019", width = 2),
      
      valueBoxOutput("Taux_duo_p1_2019", width = 2),
      valueBoxOutput("Mt_duo_com_p1_2019", width = 2),
      
      valueBoxOutput("Taux_duo_ind_p1_2019", width = 2),
      valueBoxOutput("Mt_duo_ind_p1_2019", width = 2)
    ),
    fluidRow(useShinyjs(),
      valueBoxOutput("Taux_solo_p2_2019", width = 2),
      valueBoxOutput("Mt_solo_p2_2019", width = 2),             

      valueBoxOutput("Taux_duo_p2_2019", width = 2),
      valueBoxOutput("Mt_duo_com_p2_2019", width = 2),

      valueBoxOutput("Taux_duo_ind_p2_2019", width = 2),
      valueBoxOutput("Mt_duo_ind_p2_2019", width = 2)
    )),
    box(title = "2020", status = "warning", width =12,
    fluidRow(useShinyjs(),
             valueBoxOutput("Taux_solo_p1_2020", width = 2),
             valueBoxOutput("Mt_solo_p1_2020", width = 2),
             
             valueBoxOutput("Taux_duo_p1_2020", width = 2),
             valueBoxOutput("Mt_duo_com_p1_2020", width = 2),
             
             valueBoxOutput("Taux_duo_ind_p1_2020", width = 2),
             valueBoxOutput("Mt_duo_ind_p1_2020", width = 2)
    ),
    fluidRow(useShinyjs(),
             valueBoxOutput("Taux_solo_p2_2020", width = 2),
             valueBoxOutput("Mt_solo_p2_2020", width = 2),             
             
             valueBoxOutput("Taux_duo_p2_2020", width = 2),
             valueBoxOutput("Mt_duo_com_p2_2020", width = 2),
             
             valueBoxOutput("Taux_duo_ind_p2_2020", width = 2),
             valueBoxOutput("Mt_duo_ind_p2_2020", width = 2)
    ))
  )
)




# Server ====
server <- function(input, output, session) {

  # Infos institutionnelles 2019 ====
    # Barême utilisé en 2020 sur les revenus 2019
    tx <- c(0,0.14,0.3,0.41,0.45)
    min_tr <- c(0,10064,27794,74517,157806)
    max_tr <- c(10064,27794,74517,157806,100000000)
    bareme_2019 <- as_tibble(cbind(tx,min_tr,max_tr))
    # Plafond de l'avantage fiscal procuré par le QF pour chaque demie-part
    plaf_qf_2019 <- 1567
    # Décote 2019
    statut <- c("Célibataire","Couple")
    plafond <- c(1611, 2653)
    decote_2019 <- as_tibble(cbind.data.frame(statut, plafond)) # cbind.data.frame pour éviter la conversion en character
    coeff_decote_2019 <- 0.75
    # Réduction de 20% (valable uniquement pour 2019)
    statut_reduc <- c("Célibataire","Couple","Demie-part")
    plafond_reduc <- c(21249,42498,3836)
    reduc_2019 <- as_tibble(cbind.data.frame(statut_reduc, plafond_reduc))
    
    
  # Infos institutionnelles 2020 ====
    # Barême utilisé en 2021 sur les revenus 2020
    tx <- c(0, 0.11, 0.3, 0.41, 0.45)
    min_tr <- c(0, 10064, 25659, 73369,157806)
    max_tr <- c(10064, 25659, 73369, 157806, 100000000)
    bareme_2020 <- as_tibble(cbind(tx,min_tr,max_tr))
    # Plafond de l'avantage fiscal procuré par le QF pour chaque demie-part
    plaf_qf_2020 <- 1567
    # Décote 2019
    statut <- c("Célibataire","Couple")
    plafond <- c(1717, 2842)
    decote_2020 <- as_tibble(cbind.data.frame(statut, plafond)) # cbind.data.frame pour éviter la conversion en character
    coeff_decote_2020 <- 0.4525
    
    
    
    nb_parts <- reactive({
      ifelse(input$sit_fam == "Célibataire", 1, 2) +
        min(input$nb_enf, 2) * 0.5 +
        max(input$nb_enf - 2, 0) * 1
    })
    
    nb_payeurs <- reactive(ifelse(input$sit_fam == "Célibataire", 1, 2))
    
    # rfr_2019
    rfr_2019_p1 <- reactive(input$rev_p1 - ifelse(input$type_frais_p1 == "Auto", input$rev_p1 * 0.1, input$frais_custom_p1))
    rfr_2019_p2 <- reactive(input$rev_p2 - ifelse(input$type_frais_p2 == "Auto", input$rev_p2 * 0.1, input$frais_custom_p2))
  
    # rfr_2020
    rfr_2020_p1 <- reactive(input$rev_p1 - ifelse(input$type_frais_p1 == "Auto", input$rev_p1 * 0.1, input$frais_custom_p1))
    rfr_2020_p2 <- reactive(input$rev_p2 - ifelse(input$type_frais_p2 == "Auto", input$rev_p2 * 0.1, input$frais_custom_p2))
    
    
  # Impôt 2019 pour P1 (celib, 0 enf) ====
  synth_impot_p1_solo_2019 <- reactive({
    calcul_impot(annee_rev = 2019, sit = "Célibataire", rev1 = input$rev_p1, rev2 = 0, rfr1 = rfr_2019_p1(), rfr2 = 0,
                 bareme = bareme_2019, plaf_qf = plaf_qf_2019, nb_parts = 1, nb_payeurs = 1, decote = decote_2019,
                 coeff_decote = coeff_decote_2019, reduc = reduc_2019)
    
  })

  mt_impot_net_2019_p1 <- reactive(as.numeric(synth_impot_p1_solo_2019() %>% filter(Indicateurs == "impot_net") %>% select(Valeurs)))
  mt_impot_net_2019_p1_det <- reactive(synth_impot_p1_solo_2019())

  
  # Impôt 2019 pour P2 (celib, 0 enf) ====
  synth_impot_p2_solo_2019 <- reactive({
    calcul_impot(annee_rev = 2019, sit = "Célibataire", rev1 = input$rev_p2, rev2 = 0, rfr1 = rfr_2019_p2(), rfr2 = 0,
                 bareme = bareme_2019, plaf_qf = plaf_qf_2019, nb_parts = 1, nb_payeurs = 1, decote = decote_2019,
                 coeff_decote = coeff_decote_2019, reduc = reduc_2019)
    
  })
  
  mt_impot_net_2019_p2 <- reactive(as.numeric(synth_impot_p2_solo_2019() %>% filter(Indicateurs == "impot_net") %>% select(Valeurs)))
  mt_impot_net_2019_p2_det <- reactive(synth_impot_p2_solo_2019())
  
  # Impôt 2019 pour couple (couple) ====
  synth_impot_duo_2019 <- reactive({
    calcul_impot(annee_rev = 2019, sit = input$sit_fam, rev1 = input$rev_p1, rev2 = input$rev_p2, rfr1 = rfr_2019_p1(), rfr2 = rfr_2019_p2(),
                 bareme = bareme_2019, plaf_qf = plaf_qf_2019, nb_parts = nb_parts(), nb_payeurs = nb_payeurs(), decote = decote_2019,
                 coeff_decote = coeff_decote_2019, reduc = reduc_2019)
    
  })
  
  mt_impot_net_2019_duo <- reactive(as.numeric(synth_impot_duo_2019() %>% filter(Indicateurs == "impot_net") %>% select(Valeurs)))
  mt_impot_net_2019_duo_det <- reactive(synth_impot_duo_2019())
  
  
  
  # Impôt 2020 pour P1 (celib, 0 enf) ====
  synth_impot_p1_solo_2020 <- reactive({
    calcul_impot(annee_rev = 2020, sit = "Célibataire", rev1 = input$rev_p1, rev2 = 0, rfr1 = rfr_2020_p1(), rfr2 = 0,
                 bareme = bareme_2020, plaf_qf = plaf_qf_2020, nb_parts = 1, nb_payeurs = 1, decote = decote_2020,
                 coeff_decote = coeff_decote_2020, reduc = NA)
    
  })
  
  mt_impot_net_2020_p1 <- reactive(as.numeric(synth_impot_p1_solo_2020() %>% filter(Indicateurs == "impot_net") %>% select(Valeurs)))
  mt_impot_net_2020_p1_det <- reactive(synth_impot_p1_solo_2020())
  
  
  # Impôt 2020 pour P2 (celib, 0 enf) ====
  synth_impot_p2_solo_2020 <- reactive({
    calcul_impot(annee_rev = 2020, sit = "Célibataire", rev1 = input$rev_p2, rev2 = 0, rfr1 = rfr_2020_p2(), rfr2 = 0,
                 bareme = bareme_2020, plaf_qf = plaf_qf_2020, nb_parts = 1, nb_payeurs = 1, decote = decote_2020,
                 coeff_decote = coeff_decote_2020, reduc = NA)
    
  })
  
  mt_impot_net_2020_p2 <- reactive(as.numeric(synth_impot_p2_solo_2020() %>% filter(Indicateurs == "impot_net") %>% select(Valeurs)))
  mt_impot_net_2020_p2_det <- reactive(synth_impot_p2_solo_2020())
  
  # Impôt 2020 pour couple (couple) ====
  synth_impot_duo_2020 <- reactive({
    calcul_impot(annee_rev = 2020, sit = input$sit_fam, rev1 = input$rev_p1, rev2 = input$rev_p2, rfr1 = rfr_2020_p1(), rfr2 = rfr_2020_p2(),
                 bareme = bareme_2020, plaf_qf = plaf_qf_2020, nb_parts = nb_parts(), nb_payeurs = nb_payeurs(), decote = decote_2020,
                 coeff_decote = coeff_decote_2020, reduc = NA)
    
  })
  
  mt_impot_net_2020_duo <- reactive(as.numeric(synth_impot_duo_2020() %>% filter(Indicateurs == "impot_net") %>% select(Valeurs)))
  mt_impot_net_2020_duo_det <- reactive(synth_impot_duo_2020())
  
  
  
  
  
  # Taux individualisés ====
  # Préparer une borne min à 0%
  tx_impot_indivualise_p1_2019 <- reactive({
    ifelse(input$rev_p1 < input$rev_p2,
           mt_impot_net_2019_p1() / input$rev_p1,
           (mt_impot_net_2019_duo() - mt_impot_net_2019_p2()) / input$rev_p1)
    })
  
  tx_impot_indivualise_p2_2019 <- reactive({
    ifelse(input$rev_p2 <= input$rev_p1,
           mt_impot_net_2019_p2() / input$rev_p2,
           (mt_impot_net_2019_duo() - mt_impot_net_2019_p1()) / input$rev_p2)
  })
  
  
  tx_impot_indivualise_p1_2020 <- reactive({
    ifelse(input$rev_p1 < input$rev_p2,
           mt_impot_net_2020_p1() / input$rev_p1,
           (mt_impot_net_2020_duo() - mt_impot_net_2020_p2()) / input$rev_p1)
  })
  
  tx_impot_indivualise_p2_2020 <- reactive({
    ifelse(input$rev_p2 <= input$rev_p1,
           mt_impot_net_2020_p2() / input$rev_p2,
           (mt_impot_net_2020_duo() - mt_impot_net_2020_p1()) / input$rev_p2)
  })
  
  
  
  # Tableaux de synthèses ====
  lib <- c("Taux solo","Impôt annuel", "Impôt mensuel",
           "Impôt couple", "Tx marginal", 
           "Taux commun","Impôt annuel","Impôt mensuel",
           "Taux individualisé","Impôt annuel","Impôt mensuel")
  
  p1_synth_val_2019 <- reactive({
    c(easy_format(mt_impot_net_2019_p1() / input$rev_p1, type_out = "pourcent", decimal = 1),
      easy_format(mt_impot_net_2019_p1(), type_out = "milliers", suffix = "€"),
      easy_format(mt_impot_net_2019_p1() / 12, type_out = "milliers", suffix = "€"),
      easy_format(mt_impot_net_2019_duo(), type_out = "milliers", suffix = "€"),
      "tx marginal",
      easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1),
      easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p1, type_out = "milliers", suffix = "€"),
      easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p1 / 12, type_out = "milliers", suffix = "€"),
      easy_format(tx_impot_indivualise_p1_2019(), type_out = "pourcent", decimal = 1),
      easy_format(tx_impot_indivualise_p1_2019() * input$rev_p1, type_out = "milliers", suffix = "€"),
      easy_format(tx_impot_indivualise_p1_2019() * input$rev_p1 / 12, type_out = "milliers", suffix = "€"))
    })


  p2_synth_val_2019 <- reactive({
    c(easy_format(mt_impot_net_2019_p2() / input$rev_p2, type_out = "pourcent", decimal = 1),
      easy_format(mt_impot_net_2019_p2(), type_out = "milliers", suffix = "€"),
      easy_format(mt_impot_net_2019_p2() / 12, type_out = "milliers", suffix = "€"),
      easy_format(mt_impot_net_2019_duo(), type_out = "milliers", suffix = "€"),
      "tx marginal",
      easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1),
      easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p2, type_out = "milliers", suffix = "€"),
      easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p2 / 12, type_out = "milliers", suffix = "€"),
      easy_format(tx_impot_indivualise_p2_2019(), type_out = "pourcent", decimal = 1),
      easy_format(tx_impot_indivualise_p2_2019() * input$rev_p2, type_out = "milliers", suffix = "€"),
      easy_format(tx_impot_indivualise_p2_2019() * input$rev_p2 / 12, type_out = "milliers", suffix = "€"))
  })
  
  
  p12_synth_2019 <- reactive({
    cbind.data.frame(lib, p1_synth_val_2019(), p2_synth_val_2019())
  })
  


  
  # Output à afficher ====

  # Personne 1 2019
  output$Taux_solo_p1_2019 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "blue",
             value = easy_format(mt_impot_net_2019_p1() / input$rev_p1, type_out = "pourcent", decimal = 1))
  })
  
  output$Mt_solo_p1_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2019_p1() / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "blue",
             value = easy_format(mt_impot_net_2019_p1(), type_out = "milliers", suffix = "€"))
  })
  
  output$Taux_duo_p1_2019 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "green",
             value = easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1))
  })
  
  output$Mt_duo_com_p1_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p1 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "green",
             value = easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p1, type_out = "milliers", suffix = "€"))
  })
  
  output$Taux_duo_ind_p1_2019 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "aqua",
             value = easy_format(tx_impot_indivualise_p1_2019(), type_out = "pourcent", decimal = 1))
  })
  
  output$Mt_duo_ind_p1_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(tx_impot_indivualise_p1_2019() * input$rev_p1 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "aqua",
             value = easy_format(tx_impot_indivualise_p1_2019() * input$rev_p1, type_out = "milliers", suffix = "€"))
  })
  
  
  # Personne 1 2020
  output$Taux_solo_p1_2020 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "blue",
             value = easy_format(mt_impot_net_2020_p1() / input$rev_p1, type_out = "pourcent", decimal = 1))
  })
  
  output$Mt_solo_p1_2020 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2020_p1() / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "blue",
             value = easy_format(mt_impot_net_2020_p1(), type_out = "milliers", suffix = "€"))
  })
  
  output$Taux_duo_p1_2020 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "green",
             value = easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1))
  })
  
  output$Mt_duo_com_p1_2020 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p1 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "green",
             value = easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p1, type_out = "milliers", suffix = "€"))
  })
  
  output$Taux_duo_ind_p1_2020 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "aqua",
             value = easy_format(tx_impot_indivualise_p1_2020(), type_out = "pourcent", decimal = 1))
  })
  
  output$Mt_duo_ind_p1_2020 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(tx_impot_indivualise_p1_2020() * input$rev_p1 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "aqua",
             value = easy_format(tx_impot_indivualise_p1_2020() * input$rev_p1, type_out = "milliers", suffix = "€"))
  })
  
  
  
  # Personne 2

  # Afichage conditionnel des infos pour la P2
  observeEvent(input$sit_fam, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("Taux_duo_p1_2019")
    toggle("Mt_duo_com_p1_2019")
    toggle("Taux_duo_ind_p1_2019")
    toggle("Mt_duo_ind_p1_2019")
    toggle("Taux_solo_p2_2019")
    toggle("Mt_solo_p2_2019")
    toggle("Taux_duo_p2_2019")
    toggle("Mt_duo_com_p2_2019")
    toggle("Taux_duo_ind_p2_2019")
    toggle("Mt_duo_ind_p2_2019")
  })

    
  output$Taux_solo_p2_2019 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "maroon",
             value = easy_format(mt_impot_net_2019_p2() / input$rev_p2, type_out = "pourcent", decimal = 1))
  })
  
  output$Mt_solo_p2_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2019_p2() / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "maroon",
             value = easy_format(mt_impot_net_2019_p2(), type_out = "milliers", suffix = "€"))
  })
  
  output$Taux_duo_p2_2019 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "green",
             value = easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1))
  })
  
  output$Mt_duo_com_p2_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p2 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "green",
             value = easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p2, type_out = "milliers", suffix = "€"))
  })
  
  output$Taux_duo_ind_p2_2019 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "red",
             value = easy_format(tx_impot_indivualise_p2_2019(), type_out = "pourcent", decimal = 1))
  })
  
  output$Mt_duo_ind_p2_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(tx_impot_indivualise_p2_2019() * input$rev_p2 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "red",
             value = easy_format(tx_impot_indivualise_p2_2019() * input$rev_p2, type_out = "milliers", suffix = "€"))
  })
  
  # Afichage conditionnel des infos pour la P2
  observeEvent(input$sit_fam, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("Taux_duo_p1_2020")
    toggle("Mt_duo_com_p1_2020")
    toggle("Taux_duo_ind_p1_2020")
    toggle("Mt_duo_ind_p1_2020")
    toggle("Taux_solo_p2_2020")
    toggle("Mt_solo_p2_2020")
    toggle("Taux_duo_p2_2020")
    toggle("Mt_duo_com_p2_2020")
    toggle("Taux_duo_ind_p2_2020")
    toggle("Mt_duo_ind_p2_2020")
  })
  
  
  output$Taux_solo_p2_2020 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "maroon",
             value = easy_format(mt_impot_net_2020_p2() / input$rev_p2, type_out = "pourcent", decimal = 1))
  })
  
  output$Mt_solo_p2_2020 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2020_p2() / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "maroon",
             value = easy_format(mt_impot_net_2020_p2(), type_out = "milliers", suffix = "€"))
  })
  
  output$Taux_duo_p2_2020 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "green",
             value = easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1))
  })
  
  output$Mt_duo_com_p2_2020 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p2 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "green",
             value = easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p2, type_out = "milliers", suffix = "€"))
  })
  
  output$Taux_duo_ind_p2_2020 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "red",
             value = easy_format(tx_impot_indivualise_p2_2020(), type_out = "pourcent", decimal = 1))
  })
  
  output$Mt_duo_ind_p2_2020 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(tx_impot_indivualise_p2_2020() * input$rev_p2 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "red",
             value = easy_format(tx_impot_indivualise_p2_2020() * input$rev_p2, type_out = "milliers", suffix = "€"))
  })
  
  
  
  
  # Graphique impôt
  output$tab_p1 <- renderTable({
    mt_impot_net_2019_p1_det()})
  
  output$tab_p2 <- renderTable({
    mt_impot_net_2019_p2_det()})
  
  output$tab_duo <- renderTable({
    mt_impot_net_2019_duo_det()})
  
  
  
  # Interface dynamique =====
  # Si la personne 1 choisit les frais réels, apparition le champ dédié
  observeEvent(input$type_frais_p1, {
    updateTabsetPanel(session, "parametres_fr1", selected = input$type_frais_p1)
  }) 
  
  # Si la personne 1 est en couple, apparition des champs revenu et type de frais pro pour la personne 2
  observeEvent(input$sit_fam, {
    updateTabsetPanel(session, "parametres_couple", selected = input$sit_fam)
  }) 
  
  # Si la personne 1 est en couple et que la personne 2 choisit les frais réels, apparition du champ dédié
  observeEvent(input$type_frais_p2, {
    updateTabsetPanel(session, "parametres_fr2", selected = input$type_frais_p2)
  }) 
}

shinyApp(ui, server)