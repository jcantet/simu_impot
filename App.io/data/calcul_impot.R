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
