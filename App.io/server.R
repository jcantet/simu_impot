## app.R ##====



# Fonctions génériques ====
source("./data/easy_format.R", local = TRUE) # Fonction pour mettre en forme les résultats
source("./data/calcul_impot.R", local = TRUE) # Fonction pour calculer les impôts bruts 2019
base_exemple_impots_2020 <- readRDS("./data/base_exemple_impots_2020.rds")




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
  min_tr <- c(0, 10085, 25711, 73517, 158123)
  max_tr <- c(10084, 25710, 73516, 158122, 100000000)
  bareme_2020 <- as_tibble(cbind(tx,min_tr,max_tr))
  # Plafond de l'avantage fiscal procuré par le QF pour chaque demie-part
  plaf_qf_2020 <- 1570
  # Décote 2019
  statut <- c("Célibataire","Couple")
  plafond <- c(1722, 2849)
  decote_2020 <- as_tibble(cbind.data.frame(statut, plafond)) # cbind.data.frame pour éviter la conversion en character
  coeff_decote_2020 <- 0.4525
  
  
  # Infos individus ====
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
  
  mt_impot_net_2019_p1 <- reactive(as.numeric(synth_impot_p1_solo_2019() %>% filter(Indicateurs == "Impot_net") %>% select(Valeurs)))
  mt_impot_net_2019_p1_det <- reactive(synth_impot_p1_solo_2019())
  
  # Impôt 2019 pour P2 (celib, 0 enf) ====
  synth_impot_p2_solo_2019 <- reactive({
    calcul_impot(annee_rev = 2019, sit = "Célibataire", rev1 = input$rev_p2, rev2 = 0, rfr1 = rfr_2019_p2(), rfr2 = 0,
                 bareme = bareme_2019, plaf_qf = plaf_qf_2019, nb_parts = 1, nb_payeurs = 1, decote = decote_2019,
                 coeff_decote = coeff_decote_2019, reduc = reduc_2019)
    
  })
  
  mt_impot_net_2019_p2 <- reactive(as.numeric(synth_impot_p2_solo_2019() %>% filter(Indicateurs == "Impot_net") %>% select(Valeurs)))
  mt_impot_net_2019_p2_det <- reactive(synth_impot_p2_solo_2019())
  
  # Impôt 2019 pour couple (couple) ====
  synth_impot_duo_2019 <- reactive({
    calcul_impot(annee_rev = 2019, sit = input$sit_fam, rev1 = input$rev_p1, rev2 = input$rev_p2, rfr1 = rfr_2019_p1(), rfr2 = rfr_2019_p2(),
                 bareme = bareme_2019, plaf_qf = plaf_qf_2019, nb_parts = nb_parts(), nb_payeurs = nb_payeurs(), decote = decote_2019,
                 coeff_decote = coeff_decote_2019, reduc = reduc_2019)
    
  })
  
  mt_impot_net_2019_duo <- reactive(as.numeric(synth_impot_duo_2019() %>% filter(Indicateurs == "Impot_net") %>% select(Valeurs)))
  mt_impot_net_2019_duo_det <- reactive(synth_impot_duo_2019())
  
  
  
  # Impôt 2020 pour P1 (celib, 0 enf) ====
  synth_impot_p1_solo_2020 <- reactive({
    calcul_impot(annee_rev = 2020, sit = "Célibataire", rev1 = input$rev_p1, rev2 = 0, rfr1 = rfr_2020_p1(), rfr2 = 0,
                 bareme = bareme_2020, plaf_qf = plaf_qf_2020, nb_parts = 1, nb_payeurs = 1, decote = decote_2020,
                 coeff_decote = coeff_decote_2020, reduc = NA)
    
  })
  
  mt_impot_net_2020_p1 <- reactive(as.numeric(synth_impot_p1_solo_2020() %>% filter(Indicateurs == "Impot_net") %>% select(Valeurs)))
  mt_impot_net_2020_p1_det <- reactive(synth_impot_p1_solo_2020())
  
  # Data Graphique décomposition impôt 2021 type waterfall pour P1 célibataire
  data_graph_decompos_p1 <- reactive({
    data <- mt_impot_net_2020_p1_det() %>% filter(Indicateurs %in% c("Impot sans QF", "Avantage decote", "Impot_net")) %>% mutate(Indicateurs = factor(Indicateurs, levels = Indicateurs))
    data <- cbind.data.frame(data,"measure" = c("relative", "relative", "total"))
    
  })
  
  # Impôt 2020 pour P2 (celib, 0 enf) ====
  synth_impot_p2_solo_2020 <- reactive({
    calcul_impot(annee_rev = 2020, sit = "Célibataire", rev1 = input$rev_p2, rev2 = 0, rfr1 = rfr_2020_p2(), rfr2 = 0,
                 bareme = bareme_2020, plaf_qf = plaf_qf_2020, nb_parts = 1, nb_payeurs = 1, decote = decote_2020,
                 coeff_decote = coeff_decote_2020, reduc = NA)
    
  })
  
  mt_impot_net_2020_p2 <- reactive(as.numeric(synth_impot_p2_solo_2020() %>% filter(Indicateurs == "Impot_net") %>% select(Valeurs)))
  mt_impot_net_2020_p2_det <- reactive(synth_impot_p2_solo_2020())
  
  # Data Graphique décomposition impôt 2021 type waterfall pour P2 célibataire
  data_graph_decompos_p2 <- reactive({
    data <- mt_impot_net_2020_p2_det() %>% filter(Indicateurs %in% c("Impot sans QF", "Avantage decote", "Impot_net")) %>% mutate(Indicateurs = factor(Indicateurs, levels = Indicateurs))
    data <- cbind.data.frame(data,"measure" = c("relative", "relative", "total"))
    
  })
  
  # Impôt 2020 pour couple (couple) ====
  synth_impot_duo_2020 <- reactive({
    calcul_impot(annee_rev = 2020, sit = input$sit_fam, rev1 = input$rev_p1, rev2 = input$rev_p2, rfr1 = rfr_2020_p1(), rfr2 = rfr_2020_p2(),
                 bareme = bareme_2020, plaf_qf = plaf_qf_2020, nb_parts = nb_parts(), nb_payeurs = nb_payeurs(), decote = decote_2020,
                 coeff_decote = coeff_decote_2020, reduc = NA)
    
  })
  
  mt_impot_net_2020_duo <- reactive(as.numeric(synth_impot_duo_2020() %>% filter(Indicateurs == "Impot_net") %>% select(Valeurs)))
  mt_impot_net_2020_duo_det <- reactive(synth_impot_duo_2020())
  
  
  # Data Graphique décomposition impôt 2021 type waterfall pour P1 et P2 famille
  data_graph_decompos_duo <- reactive({
    data <- mt_impot_net_2020_duo_det() %>% filter(Indicateurs %in% c("Impot sans QF", "Avantage QF", "Avantage decote", "Impot_net")) %>% mutate(Indicateurs = factor(Indicateurs, levels = Indicateurs))
    data <- cbind.data.frame(data,"measure" = c("relative","relative", "relative", "total"))
    
  })  
  
  
  
  # Taux individualisés ====
  # Taux brut 2019
  tx_impot_indivualise_p1_2019_brut <- reactive({
    ifelse(input$rev_p1 < input$rev_p2,
           mt_impot_net_2019_p1() / input$rev_p1,
           (mt_impot_net_2019_duo() - mt_impot_net_2019_p2()) / input$rev_p1)
  })
  
  tx_impot_indivualise_p2_2019_brut <- reactive({
    ifelse(input$rev_p2 <= input$rev_p1,
           mt_impot_net_2019_p2() / input$rev_p2,
           (mt_impot_net_2019_duo() - mt_impot_net_2019_p1()) / input$rev_p2)
  })
  
  
  # Taux à retenir : on traite les cas où la formule donne un taux individualisé inférieur à 0%
  tx_impot_indivualise_p1_2019 <- reactive({
    ifelse(tx_impot_indivualise_p2_2019_brut() < 0 & input$rev_p1 <= input$rev_p2,
           0,
           mt_impot_net_2019_duo()/input$rev_p1)
  })
  
  tx_impot_indivualise_p2_2019 <- reactive({
    ifelse(tx_impot_indivualise_p1_2019_brut() < 0 & input$rev_p2 <= input$rev_p1,
           0,
           mt_impot_net_2019_duo()/input$rev_p2)
  })
  
  
  # Taux brut 2020
  tx_impot_indivualise_p1_2020_brut <- reactive({
    ifelse(input$rev_p1 < input$rev_p2,
           mt_impot_net_2020_p1() / input$rev_p1,
           (mt_impot_net_2020_duo() - mt_impot_net_2020_p2()) / input$rev_p1)
  })
  
  tx_impot_indivualise_p2_2020_brut <- reactive({
    ifelse(input$rev_p2 <= input$rev_p1,
           mt_impot_net_2020_p2() / input$rev_p2,
           (mt_impot_net_2020_duo() - mt_impot_net_2020_p1()) / input$rev_p2)
  })
  
  # Taux à retenir : on traite les cas où la formule donne un taux individualisé inférieur à 0%
  tx_impot_indivualise_p1_2020 <- reactive({
    ifelse(tx_impot_indivualise_p1_2020_brut() >=0 & tx_impot_indivualise_p1_2020_brut() >= 0,
           tx_impot_indivualise_p1_2020_brut(),
           ifelse(tx_impot_indivualise_p2_2020_brut() < 0 & input$rev_p1 <= input$rev_p2,
                  0,
                  mt_impot_net_2020_duo()/input$rev_p1))
  })
  
  tx_impot_indivualise_p2_2020 <- reactive({
    ifelse(tx_impot_indivualise_p1_2020_brut() >=0 & tx_impot_indivualise_p1_2020_brut() >= 0,
           tx_impot_indivualise_p2_2020_brut(),
           ifelse(tx_impot_indivualise_p1_2020_brut() < 0 & input$rev_p2 <= input$rev_p1,
                  0,
                  mt_impot_net_2020_duo()/input$rev_p2))
  })
  
  
  # Output à afficher ====
  
  # Infobox Personne 1 2019
  output$Taux_solo_p1_2019 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "blue",
             value = easy_format(mt_impot_net_2019_p1() / input$rev_p1, type_out = "pourcent", decimal = 1))
  }) # Taux d'imposition 2020 pour P1 en tant que célibataire
  
  output$Mt_solo_p1_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2019_p1() / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "blue",
             value = easy_format(mt_impot_net_2019_p1(), type_out = "milliers", suffix = "€"))
  }) # Montant d'impot 2020 pour P1 en tant que célibataire
  
  output$Taux_duo_p1_2019 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "green",
             value = easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1))
  }) # Taux d'impôt 2020 pour P1 avec prise en compte de la famille
  
  output$Mt_duo_com_p1_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p1 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "green",
             value = easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p1, type_out = "milliers", suffix = "€"))
  }) # Montant d'impôt 2020 pour P1 avec prise en compte de la famille
  
  output$Taux_duo_ind_p1_2019 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "aqua",
             value = easy_format(tx_impot_indivualise_p1_2019(), type_out = "pourcent", decimal = 1))
  }) # Taux d'impôt 2020 individualisé pour P1 avec prise en compte de la famille
  
  output$Mt_duo_ind_p1_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(tx_impot_indivualise_p1_2019() * input$rev_p1 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "aqua",
             value = easy_format(tx_impot_indivualise_p1_2019() * input$rev_p1, type_out = "milliers", suffix = "€"))
  })  # Montant d'impôt 2020 individualisé pour P1 avec prise en compte de la famille
  
  
  # Infobox Personne 1 2020
  output$Taux_solo_p1_2020 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition (célib)", color = "blue", icon = icon("male"),
             value = easy_format(mt_impot_net_2020_p1() / input$rev_p1, type_out = "pourcent", decimal = 1))
  }) # Taux d'imposition 2021 pour P1 en tant que célibataire
  
  output$Mt_solo_p1_2020 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2020_p1() / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "blue",icon = icon("male"),
             value = easy_format(mt_impot_net_2020_p1(), type_out = "milliers", suffix = "€"))
  }) # Montant d'impot 2021 pour P1 en tant que célibataire
  
  
  output$Gain_p1_2020 <- renderValueBox({
    valueBox(value = easy_format(mt_impot_net_2020_p1() - mt_impot_net_2019_p1(), type_out = "milliers", suffix = "€"),
             subtitle = paste("Par rapport à 2020, soit ",
                              easy_format((mt_impot_net_2020_p1() - mt_impot_net_2019_p1()) / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "blue", icon = icon("male"))
  })
  
  output$Taux_duo_p1_2020 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition (commun)", color = "green",
             value = easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1))
  }) # Taux d'impôt 2021 pour P1 avec prise en compte de la famille
  
  output$Mt_duo_com_p1_2020 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p1 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "green",
             value = easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p1, type_out = "milliers", suffix = "€"))
  }) # Montant d'impôt 2021 pour P1 avec prise en compte de la famille
  
  output$Taux_duo_ind_p1_2020 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition (indiv)", color = "aqua",
             value = easy_format(tx_impot_indivualise_p1_2020(), type_out = "pourcent", decimal = 1))
  }) # Taux d'impôt 2021 individualisé pour P1 avec prise en compte de la famille
  
  output$Mt_duo_ind_p1_2020 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(tx_impot_indivualise_p1_2020() * input$rev_p1 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "aqua",
             value = easy_format(tx_impot_indivualise_p1_2020() * input$rev_p1, type_out = "milliers", suffix = "€"))
  }) # Montant d'impôt 2021 individualisé pour P1 avec prise en compte de la famille
  
  
  
  
  
  
  
  # Afichage conditionnel des infos 2019 et 2020 pour la P2
  observeEvent(input$sit_fam, {
    
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
    
    toggle("graph_decompos_p2")
    toggle("graph_decompos_duo")
    
    toggle("P2")
    toggle("famille")
    
  })

  
  # Infobox Personne 2 2019 
  output$Taux_solo_p2_2019 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "maroon",
             value = easy_format(mt_impot_net_2019_p2() / input$rev_p2, type_out = "pourcent", decimal = 1))
  }) # Taux d'imposition 2021 pour P2 en tant que célibataire
  
  output$Mt_solo_p2_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2019_p2() / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "maroon",
             value = easy_format(mt_impot_net_2019_p2(), type_out = "milliers", suffix = "€"))
  }) # Montant d'impot 2021 pour P2 en tant que célibataire
  
  output$Gain_p2_2020 <- renderValueBox({
    valueBox(value = easy_format(mt_impot_net_2020_p2() - mt_impot_net_2019_p2(), type_out = "milliers", suffix = "€"),
             subtitle = paste("Par rapport à 2020, soit ",
                              easy_format((mt_impot_net_2020_p2() - mt_impot_net_2019_p2()) / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "maroon",icon = icon("female"))
  })
  
  output$Taux_duo_p2_2019 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "green",
             value = easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1))
  }) # Taux d'impôt 2020 pour P2 avec prise en compte de la famille
  
  output$Mt_duo_com_p2_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p2 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "green",
             value = easy_format(mt_impot_net_2019_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p2, type_out = "milliers", suffix = "€"))
  }) # Montant d'impôt 2020 pour P2 avec prise en compte de la famille
  
  output$Taux_duo_ind_p2_2019 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition", color = "red",
             value = easy_format(tx_impot_indivualise_p2_2019(), type_out = "pourcent", decimal = 1))
  }) # Taux d'impôt 2020 individualisé pour P2 avec prise en compte de la famille
  
  output$Mt_duo_ind_p2_2019 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(tx_impot_indivualise_p2_2019() * input$rev_p2 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "red",
             value = easy_format(tx_impot_indivualise_p2_2019() * input$rev_p2, type_out = "milliers", suffix = "€"))
  }) # Montant d'impôt 2020 individualisé pour P2 avec prise en compte de la famille
  
  # Infobox Personne 2 2020
  output$Taux_solo_p2_2020 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition (célib)", color = "maroon",icon = icon("female"),
             value = easy_format(mt_impot_net_2020_p2() / input$rev_p2, type_out = "pourcent", decimal = 1))
  }) # Taux d'imposition 2021 pour P2 en tant que célibataire
  
  output$Mt_solo_p2_2020 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2020_p2() / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "maroon", icon = icon("female"),
             value = easy_format(mt_impot_net_2020_p2(), type_out = "milliers", suffix = "€"))
  }) # Montant d'impot 2021 pour P2 en tant que célibataire
  
  output$Taux_duo_p2_2020 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition (commun)", color = "green",
             value = easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1))
  }) # Taux d'impôt 2021 pour P2 avec prise en compte de la famille
  
  output$Mt_duo_com_p2_2020 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p2 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "green",
             value = easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2) * input$rev_p2, type_out = "milliers", suffix = "€"))
  }) # Montant d'impôt 2021 pour P2 avec prise en compte de la famille
  
  output$Taux_duo_ind_p2_2020 <- renderValueBox({
    valueBox(subtitle = "Taux d'imposition (indiv)", color = "red",
             value = easy_format(tx_impot_indivualise_p2_2020(), type_out = "pourcent", decimal = 1))
  }) # Taux d'impôt 2021 individualisé pour P2 avec prise en compte de la famille
  
  output$Mt_duo_ind_p2_2020 <- renderValueBox({
    valueBox(subtitle = paste("Soit ",
                              easy_format(tx_impot_indivualise_p2_2020() * input$rev_p2 / 12, type_out = "milliers", suffix = "€"),
                              " par mois"), color = "red",
             value = easy_format(tx_impot_indivualise_p2_2020() * input$rev_p2, type_out = "milliers", suffix = "€"))
  }) # Montant d'impôt 2021 individualisé pour P2 avec prise en compte de la famille
  
  
  # InfoBox Famille 2020
  output$Taux_com_2020 <- renderValueBox({
    valueBox(value = easy_format(mt_impot_net_2020_duo() / (input$rev_p1 + input$rev_p2), type_out = "pourcent", decimal = 1),
             subtitle = "Taux d'imposition (commun)",
             color = "green")
  })
  
  output$Mt_com_2020 <- renderValueBox({
    valueBox(value = easy_format(mt_impot_net_2020_duo(), type_out = "milliers", suffix = "€"),
             subtitle = paste("Soit ",
                              easy_format(mt_impot_net_2020_duo(), type_out = "milliers", suffix = "€"),
                              " par mois"),
             color = "green")
  })
  
  output$Gain_com_2020 <- renderValueBox({
    valueBox(value = easy_format(mt_impot_net_2020_duo() - mt_impot_net_2019_duo(), type_out = "milliers", suffix = "€"),
             subtitle = paste("Par rapport à 2020, soit ",
                              easy_format(mt_impot_net_2020_duo() - mt_impot_net_2019_duo(), type_out = "milliers", suffix = "€"),
                              " par mois"),
             color = "green")
  })
  
  # Graphiques décomposition 
  output$graph_decompos_p1 <- renderPlotly({
    plot_ly(data = data_graph_decompos_p1(), type = "waterfall", measure = ~measure, x = ~Indicateurs, y = ~Valeurs,
            decreasing = list(marker = list(color = "#94F0A9")),
            increasing = list(marker = list(color = "#228CDB")),
            totals = list(marker = list(color = "#228CDB")))
  }) # Graphique de décomposition du montant final de l'impôt 2021 pour P1 célib
  
  output$graph_decompos_p2 <- renderPlotly({
    plot_ly(data = data_graph_decompos_p2(), type = "waterfall", measure = ~measure, x = ~Indicateurs, y = ~Valeurs,
            decreasing = list(marker = list(color = "#94F0A9")),
            increasing = list(marker = list(color = "#228CDB")),
            totals = list(marker = list(color = "#228CDB")))
  }) # Graphique de décomposition du montant final de l'impôt 2021 pour P2 célib
  
  output$graph_decompos_duo <- renderPlotly({
    plot_ly(data = data_graph_decompos_duo(), type = "waterfall", measure = ~measure, x = ~Indicateurs, y = ~Valeurs,
            decreasing = list(marker = list(color = "#94F0A9")),
            increasing = list(marker = list(color = "#228CDB")),
            totals = list(marker = list(color = "#228CDB")))
  }) # Graphique de décomposition du montant final de l'impôt 2021 pour P1 et P2 famille
  
  # Tableaux détaillés impôts 
  output$tab_p1_2020 <- renderTable(mt_impot_net_2020_p1_det()) # Tableau détaillé des impots 2021 de P1 célibataire
  
  output$tab_p2_2020 <- renderTable(mt_impot_net_2020_p2_det()) # Tableau détaillé des impots 2021 de P2 célibataire
  
  output$tab_duo_2020 <- renderTable(mt_impot_net_2020_duo_det()) # Tableau détaillé des impots 2021 pour P1 et P2 famille
  
  
  # Onglet 2 ====
  output$progressivite_2020 <- renderPlotly({
    # Palette de couleurs 
    pal_name <- c("1 adulte","1 adulte et 1 enfant","1 adulte et 2 enfants","1 adulte et 3 enfants", "2 adultes", "2 adultes et 1 enfant", "2 adultes et 2 enfants", "2 adultes et 3 enfants")
    pal_col <- c("#F6D09D", "#F2B96E","#FBA739","#F28500","#C7B3CE","#B194B9","#A579B4","#9255A5")
    pal <- setNames(pal_col, pal_name)
    
    
    # Graphique des revenus
    plot_ly(data = base_exemple_impots_2020,hoverinfo = 'text',
            text = ~paste('</br> <b> Situation : </b>', situation,
                          '</br> <b> Revenus :  </b>', easy_format(rev,"milliers",suffix = "€"),
                          '</br> <b> Impôt avant QF :  </b>', easy_format(`Impot sans QF`,"milliers",suffix = "€"),
                          '</br> <b> Avantage QF :  </b>', easy_format(`Avantage QF`,"milliers",suffix = "€"),
                          '</br> <b> Décôte :  </b>', easy_format(`Avantage decote`,"milliers",suffix = "€"),
                          '</br> <b> Impôt net :  </b>', easy_format(Impot_net,"milliers",suffix = "€"))) %>%
      add_trace(data = base_exemple_impots_2020, name = ~situation, x = ~rev, y = ~Impot_net, color = ~situation, colors = pal, type = 'scatter', mode = 'lines') %>% 
      layout(title = "Impôt à payer en fonction du revenu et de la situation",
             xaxis = list(title = "Revenus"),
             yaxis = list(title = "Impôt net à payer", tickformat = ".0f"),
             legend = list(x = 0.1, y = 0.9))
  })
  
  # Interface dynamique =====
  observeEvent(input$type_frais_p1, {
    updateTabsetPanel(session, "parametres_fr1", selected = input$type_frais_p1)
  }) # Si la personne 1 choisit les frais réels, apparition le champ dédié
  
  observeEvent(input$sit_fam, {
    updateTabsetPanel(session, "parametres_couple", selected = input$sit_fam)
  }) # Si la personne 1 est en couple, apparition des champs revenu et type de frais pro pour la personne 2
  
  observeEvent(input$type_frais_p2, {
    updateTabsetPanel(session, "parametres_fr2", selected = input$type_frais_p2)
  }) # Si la personne 1 est en couple et que la personne 2 choisit les frais réels, apparition du champ dédié
}
