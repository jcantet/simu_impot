# Fonction pour calculer les impôts bruts 2019
# Fonction pour calculer les impôts bruts 2019
calcul_impot <- function(annee_rev, sit, rev1, rev2, rfr1, rfr2, bareme, plaf_qf, nb_parts, nb_payeurs, decote, coeff_decote, reduc){
  
  rev <- rev1 + rev2
  
  
  rev_retenu_fam <- (rfr1 + rfr2) / nb_parts # Base calcul impot en prenant en compte toute la famille
  rev_retenu_duo <- (rfr1 + rfr2) / nb_payeurs # Base calcul impot en prenant en compte seul le couple
  
  
  data <- 
    bareme %>%
    mutate(max_rev_tr = cumsum(max_tr),
           rev_impose = ifelse(rev_retenu_fam > max_rev_tr,
                               max_tr - min_tr,
                               pmax(pmin(rev_retenu_fam - min_tr, max_tr - min_tr),0)), # pmax : avoir le max par ligne (pas possible avec la fonction max)
           impot_tr = tx * rev_impose)
  
  # uniquement utile pour calculer l'avantage lié au QF
  data_duo <- 
    bareme %>%
    mutate(max_rev_tr = cumsum(max_tr),
           rev_impose = ifelse(rev_retenu_duo > max_rev_tr,
                               max_tr - min_tr,
                               pmax(pmin(rev_retenu_duo - min_tr, max_tr - min_tr),0)), # pmax : avoir le max par ligne (pas possible avec la fonction max)
           impot_tr = tx * rev_impose)
  
  mt_impot_brut_duo <- sum(data_duo$impot_tr)
  
  
  
  # Impôt brut solo par part de QF
  mt_impot_brut <- sum(data$impot_tr)
  
  # Impôt brut pour le foyer fiscal si pas de QF (pas de prise en compte des enfants)
  mt_impot_brut_sans_qf <- mt_impot_brut_duo * nb_payeurs
  
  # Impôt brut pour le foyer fiscal avec le QF (prise en compte des enfants)
  mt_impot_brut_avec_qf <- mt_impot_brut * nb_parts
  
  # Avantage fiscal max lié au QF en fonction du nombre de part
  avantage_QF <- max(min(
    max(nb_parts - nb_payeurs, 0) / 0.5 * plaf_qf,
    mt_impot_brut_sans_qf - mt_impot_brut_avec_qf),0)
  
  
  # Impôt brut pour le foyer fiscal après QF plafonné
  mt_impot_brut_ff <- mt_impot_brut_sans_qf - avantage_QF
  
  # Montant de la décote
  avantage_fin_decote <-
    min(
      if (mt_impot_brut_ff < decote[which(decote$statut == sit),2]){
        # Si décôte active compte tenu des revenus
        as.numeric(decote[which(decote$statut == sit),2] * coeff_decote - coeff_decote * mt_impot_brut_ff)
      } else {
        # Si revenu trop élevé pour bénéficier de la décote
        0
      }
      ,as.numeric(mt_impot_brut_ff))
  
  # Impôt corrigé du montant de la décote éventuelle
  mt_impot_brut_ff_dec <- mt_impot_brut_ff - avantage_fin_decote
  
  if (annee_rev == 2019){
    # Réduction de 20%, uniquement sur 2019( et peut être avant...)
    # Calcul du plafond de revenu pour bénéficier de la réduction
    plaf_reduc <-
      as.numeric(
        reduc[which(reduc$statut_reduc == sit), 2] + # Selon si célibataire ou couple
          reduc[3,2] * (nb_parts - nb_payeurs)/0.5) # selon le nombre de demi part fiscal supplémentaire
    
    
    rfr <- rfr1 + rfr2
    # Montant de la réduction
    avantage_fin_reduc <- 
      if(rfr < (plaf_reduc - 2072 * nb_payeurs)){
        0.2 * mt_impot_brut_ff_dec
      } else if ((rfr > (plaf_reduc - 2072 * nb_payeurs)) & (rfr < plaf_reduc)){
        0.2 * (plaf_reduc - rfr) / (2072 * nb_payeurs) * mt_impot_brut_ff_dec
      } else {
        0
      }
    
    # Impôt final après réduction
    impot_net <- mt_impot_brut_ff_dec - avantage_fin_reduc
    
  } else {
    # Impôt final
    impot_net <- mt_impot_brut_ff_dec
    
  }
  
  
  Indicateurs <- c("Impot sans QF", "Avantage QF", "Impot après QF", "Avantage decote", "Avantage reduc", "Impot_net")
  Valeurs <- c(round(mt_impot_brut_sans_qf,0), -round(avantage_QF,0), round(mt_impot_brut_ff,0), -round(avantage_fin_decote,0), ifelse(annee_rev == 2019, -round(avantage_fin_reduc,0), 0), round(impot_net,0))
  cbind.data.frame(Indicateurs,Valeurs)
  
} 

library(dplyr)
library(tidyr)
library(plotly)

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





# Pour générer tous les résultats selon les revenus pour un célibataire
result_A1_E0 <- data.frame()
for (rev in seq(0,200000,500)){
  result_temp <- calcul_impot(2020, "Célibataire",rev,0,rev*0.9,0,bareme_2020, plaf_qf_2020, 1,1,decote_2020, coeff_decote_2020,NA)
  result_A1_E0 <- rbind.data.frame(result_A1_E0, cbind("situation" = rep("1 adulte",6),"rev"= rep(rev,6), result_temp))
}
result_A1_E0 <- pivot_wider(data = result_A1_E0, names_from = "Indicateurs", values_from = "Valeurs")


# Pour générer tous les résultats selon les revenus pour 1 adulte avec 1 enfant
result_A1_E1 <- data.frame()
for (rev in seq(0,200000,500)){
  result_temp <- calcul_impot(2020, "Célibataire",rev,rev,rev*0.9,0,bareme_2020, plaf_qf_2020, 1.5,1,decote_2020, coeff_decote_2020,NA)
  result_A1_E1 <- rbind.data.frame(result_A1_E1, cbind("situation" = rep("1 adulte et 1 enfant",6), "rev"= rep(rev,6), result_temp))
}
result_A1_E1 <- pivot_wider(data = result_A1_E1, names_from = "Indicateurs", values_from = "Valeurs")

# Pour générer tous les résultats selon les revenus pour 1 adulte avec 2 enfant
result_A1_E2 <- data.frame()
for (rev in seq(0,200000,500)){
  result_temp <- calcul_impot(2020, "Célibataire",rev,rev,rev*0.9,0,bareme_2020, plaf_qf_2020, 2,1,decote_2020, coeff_decote_2020,NA)
  result_A1_E2 <- rbind.data.frame(result_A1_E2, cbind("situation" = rep("1 adulte et 2 enfants",6), "rev"= rep(rev,6), result_temp))
}
result_A1_E2 <- pivot_wider(data = result_A1_E2, names_from = "Indicateurs", values_from = "Valeurs")

# Pour générer tous les résultats selon les revenus pour 1 adulte avec 3 enfant
result_A1_E3 <- data.frame()
for (rev in seq(0,200000,500)){
  result_temp <- calcul_impot(2020, "Célibataire",rev,rev,rev*0.9,0,bareme_2020, plaf_qf_2020, 3,1,decote_2020, coeff_decote_2020,NA)
  result_A1_E3 <- rbind.data.frame(result_A1_E3, cbind("situation" = rep("1 adulte et 3 enfants",6), "rev"= rep(rev,6), result_temp))
}
result_A1_E3 <- pivot_wider(data = result_A1_E3, names_from = "Indicateurs", values_from = "Valeurs")

# Pour générer tous les résultats selon les revenus pour un couple sans enfants
result_A2_E0 <- data.frame()
for (rev in seq(0,200000,500)){
  result_temp <- calcul_impot(2020, "Couple",rev,0,rev*0.9,0,bareme_2020, plaf_qf_2020, 2,2,decote_2020, coeff_decote_2020,NA)
  result_A2_E0 <- rbind.data.frame(result_A2_E0, cbind("situation" = rep("2 adultes",6), "rev"= rep(rev,6), result_temp))
}
result_A2_E0 <- pivot_wider(data = result_A2_E0, names_from = "Indicateurs", values_from = "Valeurs")

# Pour générer tous les résultats selon les revenus pour un couple sans enfants
result_A2_E1 <- data.frame()
for (rev in seq(0,200000,500)){
  result_temp <- calcul_impot(2020, "Couple",rev,0,rev*0.9,0,bareme_2020, plaf_qf_2020, 2.5,2,decote_2020, coeff_decote_2020,NA)
  result_A2_E1 <- rbind.data.frame(result_A2_E1, cbind("situation" = rep("2 adultes et 1 enfant",6), "rev"= rep(rev,6), result_temp))
}
result_A2_E1 <- pivot_wider(data = result_A2_E1, names_from = "Indicateurs", values_from = "Valeurs")

# Pour générer tous les résultats selon les revenus pour un couple sans enfants
result_A2_E2 <- data.frame()
for (rev in seq(0,200000,500)){
  result_temp <- calcul_impot(2020, "Couple",rev,0,rev*0.9,0,bareme_2020, plaf_qf_2020, 3,2,decote_2020, coeff_decote_2020,NA)
  result_A2_E2 <- rbind.data.frame(result_A2_E2, cbind("situation" = rep("2 adultes et 2 enfants",6), "rev"= rep(rev,6), result_temp))
}
result_A2_E2 <- pivot_wider(data = result_A2_E2, names_from = "Indicateurs", values_from = "Valeurs")

# Pour générer tous les résultats selon les revenus pour un couple sans enfants
result_A2_E3 <- data.frame()
for (rev in seq(0,200000,500)){
  result_temp <- calcul_impot(2020, "Couple",rev,0,rev*0.9,0,bareme_2020, plaf_qf_2020, 4,2,decote_2020, coeff_decote_2020,NA)
  result_A2_E3 <- rbind.data.frame(result_A2_E3, cbind("situation" = rep("2 adultes et 3 enfants",6), "rev"= rep(rev,6), result_temp))
}
result_A2_E3 <- pivot_wider(data = result_A2_E3, names_from = "Indicateurs", values_from = "Valeurs")


global_result <- rbind.data.frame(result_A1_E0,result_A1_E1,result_A1_E2,result_A1_E3,result_A2_E0,result_A2_E1,result_A2_E2,result_A2_E3)

save(global_result,file = "./App/base_exemple_impots_2020.Rdata")


# Palette de couleurs 
pal_name <- c("1 adulte","1 adulte et 1 enfant","1 adulte et 2 enfants","1 adulte et 3 enfants", "2 adultes", "2 adultes et 1 enfant", "2 adultes et 2 enfants", "2 adultes et 3 enfants")
pal_col <- c("#F6D09D", "#F2B96E","#FBA739","#F28500","#C7B3CE","#B194B9","#A579B4","#9255A5")
pal <- setNames(pal_col, pal_name)


# Graphique des revenus
plot_ly(data = global_result,hoverinfo = 'text',
        text = ~paste('</br> <b> Situation : </b>', situation,
                      '</br> <b> Revenus :  </b>', easy_format(rev,"milliers",suffix = "€"),
                      '</br> <b> Impôt avant QF :  </b>', easy_format(`Impot sans QF`,"milliers",suffix = "€"),
                      '</br> <b> Avantage QF :  </b>', easy_format(`Avantage QF`,"milliers",suffix = "€"),
                      '</br> <b> Décôte :  </b>', easy_format(`Avantage decote`,"milliers",suffix = "€"),
                      '</br> <b> Impôt net :  </b>', easy_format(Impot_net,"milliers",suffix = "€"))) %>%
  add_trace(data = global_result, name = ~situation, x = ~rev, y = ~Impot_net, color = ~situation, colors = pal, type = 'scatter', mode = 'lines') %>% 
  layout(title = "Impôt à payer en fonction du revenu et de la situation",
         xaxis = list(title = "Revenus"),
         yaxis = list(title = "Impôt net à payer", tickformat = ".0f"),
         legend = list(x = 0.1, y = 0.9))



            