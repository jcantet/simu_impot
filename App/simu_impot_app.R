# Packages ====
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(ggplot2)
options(scipen = 9999999)

# Construire des modules
# Ajouter le plancher et plafond pour l'abattement sur les frais pros
# Prévoir tableau de sortie, avec 2019 et 2020.
# Prévoir graphique avec courbes 2019 et 2020 sur la part d'impôt sur le revenu en fonction du revenu à situation identique
# Pour cela il faut créer une fonction de calcul des impots


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
    sidebarMenu(tags$script(HTML("$('body').addClass('fixed');"))),# Barre latérale fixée
    
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

    )
  )
)




# Server ====
server <- function(input, output, session) {

  # Infos insitutionnelles ====
  # Barême utilisé en 2020 sur les revenus 2019
  tx_2019 <- c(0,0.14,0.3,0.41,0.45)
  min_tr_2019 <- c(0,10064,27794,74517,157806)
  max_tr_2019 <- c(10064,27794,74517,157806,100000000)
  bareme_2019 <- as_tibble(cbind(tx_2019,min_tr_2019,max_tr_2019))
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
  
  nb_parts_2019 <- reactive({
    ifelse(input$sit_fam == "Célibataire", 1, 2) +
      min(input$nb_enf, 2) * 0.5 +
      max(input$nb_enf - 2, 0) * 1
  })
  
  nb_payeurs_2019 <- reactive({
    ifelse(input$sit_fam == "Célibataire", 1, 2)
  })
  
  # rfr_2019_p1
  rfr_2019_p1 <- reactive({
    input$rev_p1 - ifelse(input$type_frais_p1 == "Auto", input$rev_p1 * 0.1, input$frais_custom_p1)
  })
  
  rfr_2019_p2 <- reactive({
    input$rev_p2 - ifelse(input$type_frais_p2 == "Auto", input$rev_p2 * 0.1, input$frais_custom_p2)
  })
  
  
  # Fonctions génériques ====
  
  # Fonction pour mettre en forme les résultats
  easy_format <- function(variable, type_out, decimal = 0, suffix = NULL){
    
    # Format pourcent
    if (type_out == "pourcent"){
      variable = paste0(format(x = round(variable*100, decimal)),"%")
      
      # Format milliers
    } else if (type_out == "milliers"){
      variable = paste0(format(x = round(variable, decimal), big.mark = " "), suffix)
    }
    
    # Gestion des erreurs : si l'utilisateur rentre un paramètre qui n'est pas prévu
    if ((type_out %in% c("pourcent","milliers")) == FALSE){
      print(paste0("type_out ", type_out," n'existe pas"))
    } else {
      return(variable)
    }
    
  }
  
  # Fonction pour calculer les impôts bruts 2019
  impot_2019 <- function(rfr){
    
    data <- 
      bareme_2019 %>%
      mutate(max_rev_tr = cumsum(max_tr_2019),
             rev_impose = ifelse(rfr > max_rev_tr,
                                 max_tr_2019 - min_tr_2019,
                                 pmax(pmin(rfr - min_tr_2019, max_tr_2019 - min_tr_2019),0)), # pmax : avoir le max par ligne (pas possible avec la fonction max)
             impot_tr = tx_2019 * rev_impose)
    
    sum(data$impot_tr)
    
  } 
  
  

  
  # Impôt 2019 pour P1 (celib, 0 enf) ====
  # Impot brut sans application du QF
  mt_impot_brut_2019_sans_QF_p1 <- reactive({
    impot_2019(rfr_2019_p1())
  })
  
  # Impot semi net après plafond QF (pas de différence quand solo)
  mt_impot_semi1_2019_p1 <- reactive({
    mt_impot_brut_2019_sans_QF_p1()
  })
  
  # Montant de la décote
  avantage_fin_decote_2019_p1 <- reactive({
    min(
    if (mt_impot_semi1_2019_p1() < decote_2019[which(decote_2019$statut == "Célibataire"),2]){
      # Si décôte active compte tenu des revenus
      as.numeric(decote_2019[which(decote_2019$statut == "Célibataire"),2]*coeff_decote_2019 - coeff_decote_2019 * mt_impot_semi1_2019_p1())
    } else {
      # Si revenu trop élevé pour bénéficier de la décote
      0
    }
    ,as.numeric(mt_impot_semi1_2019_p1()))
  })
  
  # Impôt corrigé du montant de la décote éventuelle
  mt_impot_semi2_2019_p1 <- reactive({
    mt_impot_semi1_2019_p1() - avantage_fin_decote_2019_p1()
  })
  
  
  # Réduction de 20%
    # Calcul du plafond de revenu pour bénéficier de la réduction
    plaf_reduc_2019_p1 <- reactive({
      as.numeric(
      reduc_2019[which(reduc_2019$statut_reduc == "Célibataire"),2] + # Selon si célibataire ou couple
        reduc_2019[3,2] * (nb_parts_2019() - nb_payeurs_2019())/0.5) # selon le nombre de demi part fiscal supplémentaire
    })
  
    # Montant de la réduction
    avantage_fin_reduc_2019_p1 <- reactive({
      if(input$rev_p1 < plaf_reduc_2019_p1()){
        0.2 * mt_impot_semi2_2019_p1()
        } else {
        0
      }
    })

  # Impot final (celib, 0 enf)
  mt_impot_net_2019_p1 <- reactive({
    mt_impot_semi2_2019_p1() - avantage_fin_reduc_2019_p1()
  })
  
  
  # Impôt 2019 pour P1 (celib/couple, X enfs) ====
  
  # Impot brut sans application du QF (rev P1)
  mt_impot_brut_2019_sans_QF_p1b <- reactive({
    impot_2019(rfr_2019_p1())
  })
  
  # Impot brut avec application du QF (rev duo)
  mt_impot_brut_2019_avec_QF_p1b <- reactive({
    impot_2019((rfr_2019_p1() + rfr_2019_p2())/nb_parts_2019()) * nb_parts_2019() / nb_payeurs_2019()
  })
  
  # Avantage fiscal max lié au QF en fonction du nombre de part
  avantage_max_QF_2019_p1b <- reactive({
    as_tibble(max(nb_parts_2019() - nb_payeurs_2019(), 0) / 0.5 * plaf_qf_2019)
  })
  
  # Avantage fiscal lié au QF à retenir : min entre le plafond autorisé et l'avantage réel
  avantage_fin_QF_2019_p1b <- reactive({
    min(avantage_max_QF_2019_p1b(), mt_impot_brut_2019_sans_QF_p1b() - mt_impot_brut_2019_avec_QF_p1b())
  })
  
  
  # Impot semi net après plafond QF
  mt_impot_semi1_2019_p1b <- reactive({
    mt_impot_brut_2019_sans_QF_p1b() - avantage_fin_QF_2019_p1b()
  })
  
  # Montant de la décôte
  avantage_fin_decote_2019_p1b <- reactive({
    min(
      if (mt_impot_semi1_2019_p1b() < decote_2019[which(decote_2019$statut == "Célibataire"),2]){
        # Si décôte active
        as.numeric(decote_2019[which(decote_2019$statut == "Célibataire"),2]*coeff_decote_2019 - coeff_decote_2019 * mt_impot_semi1_2019_p1b())
      } else {
        # Si revenu trop élevé pour bénéficier de la décôte
        0
      }
      ,as.numeric(mt_impot_semi1_2019_p1b()))
  })
  
  # Impôt corrigé du montant de la décote éventuelle
  mt_impot_semi2_2019_p1b <- reactive({
    mt_impot_semi1_2019_p1b() - avantage_fin_decote_2019_p1b()
  })
  
  
  # Calcul du plafond de revenu pour bénéficier de la réduction
    plaf_reduc_2019_p1b <- reactive({
      as.numeric(
        reduc_2019[which(reduc_2019$statut_reduc == "Célibataire"),2] +
          reduc_2019[3,2] * (nb_parts_2019() - nb_payeurs_2019())/0.5)
    })
  
    # Montant de la réduction
    avantage_fin_reduc_2019_p1b <- reactive({
      if(input$rev_p1 < plaf_reduc_2019_p1()){
        0.2 * mt_impot_semi2_2019_p1b()
      } else {
        0
      }
    })

  # Impot final (celib/couple, X enfs)
  mt_impot_net_2019_p1b <- reactive({
    mt_impot_semi2_2019_p1b() - avantage_fin_reduc_2019_p1b()
  })
    
  
  
  # Impôt 2019 solo pour P2 ====
  # Impot brut sans application du QF
  mt_impot_brut_2019_sans_QF_p2 <- reactive({
    impot_2019(rfr_2019_p2())
  })
  
  # Impot semi net après plafond QF
  mt_impot_semi1_2019_p2 <- reactive({
    mt_impot_brut_2019_sans_QF_p2()
  })
  
  # Décôte
  avantage_fin_decote_2019_p2 <- reactive({
    min(
      if (mt_impot_semi1_2019_p2() < decote_2019[which(decote_2019$statut == "Célibataire"),2]){
        # Si décôte active
        as.numeric(decote_2019[which(decote_2019$statut == "Célibataire"),2]*coeff_decote_2019 - coeff_decote_2019 * mt_impot_semi1_2019_p2())
      } else {
        # Si revenu trop élevé pour bénéficier de la décôte
        0
      }
      ,as.numeric(mt_impot_semi1_2019_p2()))
  })
  
  
  mt_impot_semi2_2019_p2 <- reactive({
    mt_impot_semi1_2019_p2() - avantage_fin_decote_2019_p2()
  })
  
  
  # Abattement de 20%
  plaf_reduc_2019_p2 <- reactive({
    as.numeric(
      reduc_2019[which(reduc_2019$statut_reduc == "Célibataire"),2] +
        reduc_2019[3,2] * (nb_parts_2019() - nb_payeurs_2019())/0.5)
  })
  
  
  avantage_fin_reduc_2019_p2 <- reactive({
    if(input$rev_p2 < plaf_reduc_2019_p2()){
      0.2 * mt_impot_semi2_2019_p2()
    } else {
      0
    }
  })
  
  # Impot final solo
  mt_impot_net_2019_p2 <- reactive({
    mt_impot_semi2_2019_p2() - avantage_fin_reduc_2019_p2()
  })
  
  
  # Impôt 2019 pour P2 avec QF ====
  
  # Impot brut sans application du QF
  mt_impot_brut_2019_sans_QF_p2b <- reactive({
    impot_2019((rfr_2019_p2()))
  })
  
  # Impot brut avec application du QF
  mt_impot_brut_2019_avec_QF_p2b <- reactive({
    impot_2019((rfr_2019_p1() + rfr_2019_p2())/nb_parts_2019()) * nb_parts_2019() / nb_payeurs_2019()
  })
  
  # Avantage fiscal max lié au QF en fonction du nombre de part
  avantage_max_QF_2019_p2b <- reactive({
    as_tibble(max(nb_parts_2019() - nb_payeurs_2019(), 0) / 0.5 * plaf_qf_2019)
  })
  
  # Avantage fiscal lié au QF à retenir : min entre le plafond autorisé et l'avantage réel
  avantage_fin_QF_2019_p2b <- reactive({
    min(avantage_max_QF_2019_p2b(), mt_impot_brut_2019_sans_QF_p2b() - mt_impot_brut_2019_avec_QF_p2b())
  })
  
  
  # Impot semi net après plafond QF
  mt_impot_semi1_2019_p2b <- reactive({
    mt_impot_brut_2019_sans_QF_p2b() - avantage_fin_QF_2019_p2b()
  })
  
  # Décôte
  avantage_fin_decote_2019_p2b <- reactive({
    min(
      if (mt_impot_semi1_2019_p2b() < decote_2019[which(decote_2019$statut == "Célibataire"),2]){
        # Si décôte active
        as.numeric(decote_2019[which(decote_2019$statut == "Célibataire"),2]*coeff_decote_2019 - coeff_decote_2019 * mt_impot_semi1_2019_p2b())
      } else {
        # Si revenu trop élevé pour bénéficier de la décôte
        0
      }
      ,as.numeric(mt_impot_semi1_2019_p2b()))
  })
  
  
  mt_impot_semi2_2019_p2b <- reactive({
    mt_impot_semi1_2019_p2b() - avantage_fin_decote_2019_p2b()
  })
  
  
  # Abattement de 20%
  plaf_reduc_2019_p2b <- reactive({
    as.numeric(
      reduc_2019[which(reduc_2019$statut_reduc == "Célibataire"),2] +
        reduc_2019[3,2] * (nb_parts_2019() - nb_payeurs_2019())/0.5)
  })
  
  
  avantage_fin_reduc_2019_p2b <- reactive({
    if(input$rev_p2 < plaf_reduc_2019_p2()){
      0.2 * mt_impot_semi2_2019_p2b()
    } else {
      0
    }
  })
  
  # Impot final solo
  mt_impot_net_2019_p2b <- reactive({
    mt_impot_semi2_2019_p2b() - avantage_fin_reduc_2019_p2b()
  })
  
  
  # Impôt duo ====
  
  
  mt_impot_brut_2019_sans_QF_duo <- reactive({
    impot_2019((rfr_2019_p1() + rfr_2019_p2()) / nb_parts_2019()) * nb_payeurs_2019()
  })
  
  # Impot brut avec application du QF
  mt_impot_brut_2019_avec_QF_duo <- reactive({
    impot_2019((rfr_2019_p1() + rfr_2019_p2())/nb_parts_2019()) * nb_parts_2019()
  })
  
  # Avantage fiscal max lié au QF en fonction du nombre de part
  avantage_max_QF_2019_duo <- reactive({
    as_tibble(max(nb_parts_2019() - nb_payeurs_2019(), 0) / 0.5 * plaf_qf_2019)
  })
  
  # Avantage fiscal lié au QF à retenir : min entre le plafond autorisé et l'avantage réel
  avantage_fin_QF_2019_duo <- reactive({
    min(avantage_max_QF_2019_duo(), mt_impot_brut_2019_sans_QF_duo() - mt_impot_brut_2019_avec_QF_duo())
  })
  
  # Impot semi net après plafond QF
  mt_impot_semi1_2019_duo <- reactive({
    mt_impot_brut_2019_sans_QF_duo() - avantage_fin_QF_2019_duo()
  })
  
  # Décôte
  avantage_fin_decote_2019_duo <- reactive({
    min(
    if (mt_impot_semi1_2019_duo() < decote_2019[which(decote_2019$statut == input$sit_fam),2]){
      # Si décôte active
      as.numeric(decote_2019[which(decote_2019$statut == input$sit_fam),2]*coeff_decote_2019 - coeff_decote_2019 * mt_impot_semi1_2019_duo())
    } else {
      # Si revenu trop élevé pour bénéficier de la décôte
      0
    }
    ,as.numeric(mt_impot_semi1_2019_duo()))
  })
  
  
  mt_impot_semi2_2019_duo <- reactive({
    mt_impot_semi1_2019_duo() - avantage_fin_decote_2019_duo()
  })
  
  
  # Abattement de 20%
  # Calcul du plafond de revenu pour bénéficier de la réduction
  plaf_reduc_2019_duo <- reactive({
    as.numeric(
      reduc_2019[which(reduc_2019$statut_reduc == input$sit_fam),2] + # Selon si célibataire ou couple
        reduc_2019[3,2] * (nb_parts_2019() - nb_payeurs_2019())/0.5) # selon le nombre de demi part fiscal supplémentaire
  })
  
  

  avantage_fin_reduc_2019_duo <- reactive({
    if ((input$rev_p1 + input$rev_p2) < plaf_reduc_2019_duo()){
      0.2 * mt_impot_semi2_2019_duo()
    } else {
      0
    }
  })
  
  
  # Impot final
  mt_impot_net_2019_duo <- reactive({
    mt_impot_semi2_2019_duo() - avantage_fin_reduc_2019_duo()
  })
  
  
  
  # Taux individualisés ====
  tx_impot_indivualise_p1 <- reactive({
    ifelse(input$rev_p1 < input$rev_p2,
           mt_impot_net_2019_p1() / input$rev_p1,
           (mt_impot_net_2019_duo() - mt_impot_net_2019_p2()) / input$rev_p1)
    })
  
  tx_impot_indivualise_p2 <- reactive({
    ifelse(input$rev_p2 <= input$rev_p1,
           mt_impot_net_2019_p2() / input$rev_p2,
           (mt_impot_net_2019_duo() - mt_impot_net_2019_p1()) / input$rev_p2)
  })
  
  # Tableaux de synthèses ====
  lib <- c("Taux solo","Impôt annuel", "Impôt mensuel",
           "Impôt couple", "Tx marginal", 
           "Taux commun","Impôt annuel","Impôt mensuel",
           "Taux individualisé","Impôt annuel","Impôt mensuel")
  
  p1_synth_val <- reactive({
    c(easy_format(mt_impot_net_2019_p1() / input$rev_p1, type_out = "pourcent", decimal = 1),
      easy_format(mt_impot_net_2019_p1(), type_out = "milliers", suffix = "€"),
      easy_format(mt_impot_net_2019_p1() / 12, type_out = "milliers", suffix = "€"),
      easy_format(mt_impot_net_2019_duo(), type_out = "milliers", suffix = "€"),
      "tx marginal",
      easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1),
      easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p1, type_out = "milliers", suffix = "€"),
      easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p1 / 12, type_out = "milliers", suffix = "€"),
      easy_format(tx_impot_indivualise_p1(), type_out = "pourcent", decimal = 1),
      easy_format(tx_impot_indivualise_p1() * input$rev_p1, type_out = "milliers", suffix = "€"),
      easy_format(tx_impot_indivualise_p1() * input$rev_p1 / 12, type_out = "milliers", suffix = "€"))
    })
  

  p2_synth_val <- reactive({
    c(easy_format(mt_impot_net_2019_p2() / input$rev_p2, type_out = "pourcent", decimal = 1),
      easy_format(mt_impot_net_2019_p2(), type_out = "milliers", suffix = "€"),
      easy_format(mt_impot_net_2019_p2() / 12, type_out = "milliers", suffix = "€"),
      easy_format(mt_impot_net_2019_duo(), type_out = "milliers", suffix = "€"),
      "tx marginal",
      easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1),
      easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p2, type_out = "milliers", suffix = "€"),
      easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p2 / 12, type_out = "milliers", suffix = "€"),
      easy_format(tx_impot_indivualise_p2(), type_out = "pourcent", decimal = 1),
      easy_format(tx_impot_indivualise_p2() * input$rev_p2, type_out = "milliers", suffix = "€"),
      easy_format(tx_impot_indivualise_p2() * input$rev_p2 / 12, type_out = "milliers", suffix = "€"))
  })
  
  
  p12_synth <- reactive({
    cbind.data.frame(lib, p1_synth_val(), p2_synth_val())
  })
  


  
  # Output à afficher ====

  # Personne 1
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
             value = easy_format(tx_impot_indivualise_p1(), type_out = "pourcent", decimal = 1))
  })
  
  
  output$Mt_duo_ind_p1_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(tx_impot_indivualise_p1() * input$rev_p1 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "aqua",
             value = easy_format(tx_impot_indivualise_p1() * input$rev_p1, type_out = "milliers", suffix = "€"))
  })
  
  # Personne 2

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
             value = easy_format(tx_impot_indivualise_p2(), type_out = "pourcent", decimal = 1))
  })
  
  
  output$Mt_duo_ind_p2_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(tx_impot_indivualise_p2() * input$rev_p2 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "red",
             value = easy_format(tx_impot_indivualise_p2() * input$rev_p2, type_out = "milliers", suffix = "€"))
  })
  

  # Graphique impôt

  
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