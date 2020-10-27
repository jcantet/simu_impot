# Script pour tester les calculs d'impôts avant de réaliser l'application
options(scipen = 999999)

# Packages ====
library(dplyr)


# Il faut prévoir un calcul des impôts version famille (uniquement solo pour l'instant) pour chaque payeur, 
# et un calcul au niveau du couple


# Infos 2019
nb_payeurs_2019 <- 2
situation <- ifelse(nb_payeurs_2019 == 1, "Célibataire","Couple")
nb_parts_2019 <- 2.5
tx_2019 <- c(0,0.14,0.3,0.41,0.45)
min_tr_2019 <- c(0,10064,27794,74517,157806)
max_tr_2019 <- c(10064,27794,74517,157806,100000000)
bareme_2019 <- as_tibble(cbind(tx_2019,min_tr_2019,max_tr_2019))

plaf_qf_2019 <- 1567



# Decote
statut <- c("Célibataire","Couple")
plafond <- c(1611, 2653)
decote_2019 <- as_tibble(cbind.data.frame(statut, plafond)) # cbind.data.frame pour éviter la conversion en character


# Réduction de 20%
statut_reduc <- c("Célibataire","Couple","Demie-part")
plafond_reduc <- c(21249,42498,3836)
reduc_2019 <- as_tibble(cbind.data.frame(statut_reduc, plafond_reduc))

rev_2019_p1 <- 21000
rev_2019_p2 <- 22000

rfr_2019_p1 <- rev_2019_p1 * 0.9 # Par défaut, c'est l'abattement de 10% qui est retenu, sinon on prendra la valeur renseignée par l'utilisateur
rfr_2019_p2 <- rev_2019_p2 * 0.9
rfr_2019_duo <- rfr_2019_p1 + rfr_2019_p2

plaf_reduc_2019_p1 <- (reduc_2019[which(reduc_2019$statut_reduc == "Célibataire"),2] + reduc_2019[3,2] * (nb_parts_2019 - nb_payeurs_2019)/0.5)
plaf_reduc_2019_p2 <- (reduc_2019[which(reduc_2019$statut_reduc == "Célibataire"),2] + reduc_2019[3,2] * (nb_parts_2019 - nb_payeurs_2019)/0.5)
plaf_reduc_2019_duo <- reduc_2019[which(reduc_2019$statut_reduc == situation),2] + reduc_2019[3,2] * (nb_parts_2019 - nb_payeurs_2019)/0.5

# Calcul de l'impot brut en 2019 selon le revenu défini en input
impot_2019 <- function(rfr){

  data <- 
    bareme_2019 %>%
    mutate(max_rev_tr = cumsum(max_tr_2019),
           rev_impose = if_else(rfr > max_rev_tr,
                                max_tr_2019 - min_tr_2019,
                                pmax(pmin(rfr - min_tr_2019, max_tr_2019 - min_tr_2019),0)), # pmax : avoir le max par ligne (pas possible avec la fonction max)
           impot_tr = tx_2019 * rev_impose) %>%
    summarise(impot = sum(impot_tr))
  
} 


# Couple ============================================================================
mt_impot_brut_2019_sans_QF_duo <- impot_2019(rfr_2019_duo / nb_parts_2019) * nb_payeurs_2019


# Impot brut avec application du QF
mt_impot_brut_2019_avec_QF_duo <- impot_2019(rfr_2019_duo/nb_parts_2019) * nb_parts_2019


# Avantage fiscal max lié au QF en fonction du nombre de part
avantage_max_QF_2019_duo <- as_tibble(max(nb_parts_2019 - nb_payeurs_2019, 0) / 0.5 * plaf_qf_2019)

# Avantage fiscal lié au QF à retenir : min entre le plafond autorisé et l'avantage réel
avantage_fin_QF_2019_duo <- min(avantage_max_QF_2019_duo, mt_impot_brut_2019_sans_QF_duo - mt_impot_brut_2019_avec_QF_duo)

# Impot semi net après plafond QF
mt_impot_semi1_2019_duo <- mt_impot_brut_2019_sans_QF_duo - avantage_fin_QF_2019_duo

# Décôte
avantage_fin_decote_2019_duo <- min(
  if (mt_impot_semi1_2019_duo < decote_2019[which(decote_2019$statut == situation),2]){
    # Si décôte active
    as.numeric(decote_2019[which(decote_2019$statut == situation),2]*0.75 - 0.75 * mt_impot_semi1_2019_duo)
  } else {
    # Si revenu trop élevé pour bénéficier de la décôte
    0
  }
  ,as.numeric(mt_impot_semi1_2019_duo))

mt_impot_semi2_2019_duo <- mt_impot_semi1_2019_duo - avantage_fin_decote_2019_duo



# Abattement de 20%
avantage_fin_reduc_2019_duo <- if((rev_2019_p1 + rev_2019_p2) < plaf_reduc_2019_duo){
  0.2 * mt_impot_semi2_2019_duo
} else {
  0
}

# Impot final
(mt_impot_net_2019_duo <- mt_impot_semi2_2019_duo - avantage_fin_reduc_2019_duo)






# Impot Personne 1 solo ==================================================================
# Impot brut sans application du QF
mt_impot_brut_2019_sans_QF_p1 <- impot_2019(rfr_2019_p1)

# Impot semi net après plafond QF
mt_impot_semi1_2019_p1 <- mt_impot_brut_2019_sans_QF_p1

# Décôte
avantage_fin_decote_2019_p1 <- min(
    if (mt_impot_semi1_2019_p1 < decote_2019[which(decote_2019$statut == "Célibataire"),2]){
    # Si décôte active
    as.numeric(decote_2019[which(decote_2019$statut == "Célibataire"),2]*0.75 - 0.75 * mt_impot_semi1_2019_p1)
  } else {
    # Si revenu trop élevé pour bénéficier de la décôte
    0
  }
  ,as.numeric(mt_impot_semi1_2019_p1))

mt_impot_semi2_2019_p1 <- mt_impot_semi1_2019_p1 - avantage_fin_decote_2019_p1



# Abattement de 20%
avantage_fin_reduc_2019_p1 <- if(rev_2019_p1 < plaf_reduc_2019_p1){
  0.2 * mt_impot_semi2_2019_p1
} else {
  0
}

# Impot final
(mt_impot_net_2019_p1 <- mt_impot_semi2_2019_p1 - avantage_fin_reduc_2019_p1)



# Impot Personne 1 famille ==================================================================
# Impot brut sans application du QF
mt_impot_brut_2019_sans_QF_p1b <- impot_2019(rfr_2019_duo/nb_parts_2019)


# Impot brut avec application du QF
mt_impot_brut_2019_avec_QF_p1b <- impot_2019(rfr_2019_duo/nb_parts_2019) * nb_parts_2019 / nb_payeurs_2019


# Avantage fiscal max lié au QF en fonction du nombre de part
avantage_max_QF_2019_p1b <- as_tibble(max(nb_parts_2019 - nb_payeurs_2019, 0) / 0.5 * plaf_qf_2019)

# Avantage fiscal lié au QF à retenir : min entre le plafond autorisé et l'avantage réel
avantage_fin_QF_2019_p1b <- min(avantage_max_QF_2019_p1b, mt_impot_brut_2019_sans_QF_p1b - mt_impot_brut_2019_avec_QF_p1b)

# Impot semi net après plafond QF
mt_impot_semi1_2019_p1b <- mt_impot_brut_2019_sans_QF_p1b - avantage_fin_QF_2019_p1b


# Décôte
avantage_fin_decote_2019_p1b <- min(
  if (mt_impot_semi1_2019_p1b < decote_2019[which(decote_2019$statut == "Célibataire"),2]){
    # Si décôte active
    as.numeric(decote_2019[which(decote_2019$statut == "Célibataire"),2]*0.75 - 0.75 * mt_impot_semi1_2019_p1b)
  } else {
    # Si revenu trop élevé pour bénéficier de la décôte
    0
  }
  ,as.numeric(mt_impot_semi1_2019_p1b))

mt_impot_semi2_2019_p1b <- mt_impot_semi1_2019_p1b - avantage_fin_decote_2019_p1b



# Abattement de 20%
avantage_fin_reduc_2019_p1b <- if(rev_2019_p1 < plaf_reduc_2019_p1){
  0.2 * mt_impot_semi2_2019_p1b
} else {
  0
}

# Impot final
(mt_impot_net_2019_p1b <- mt_impot_semi2_2019_p1b - avantage_fin_reduc_2019_p1b)






# Impot Personne 2 solo ==================================================================
# Impot brut sans application du QF
mt_impot_brut_2019_sans_QF_p2 <- impot_2019(rfr_2019_p2)

# Impot semi net après plafond QF
mt_impot_semi1_2019_p2 <- mt_impot_brut_2019_sans_QF_p2

# Décôte
avantage_fin_decote_2019_p2 <- min(
  if (mt_impot_semi1_2019_p2 < decote_2019[which(decote_2019$statut == "Célibataire"),2]){
    # Si décôte active
    as.numeric(decote_2019[which(decote_2019$statut == "Célibataire"),2]*0.75 - 0.75 * mt_impot_semi1_2019_p2)
  } else {
    # Si revenu trop élevé pour bénéficier de la décôte
    0
  }
  ,as.numeric(mt_impot_semi1_2019_p2))

mt_impot_semi2_2019_p2 <- mt_impot_semi1_2019_p2 - avantage_fin_decote_2019_p2



# Abattement de 20%
avantage_fin_reduc_2019_p2 <- if(rev_2019_p2 < plaf_reduc_2019_p2){
  0.2 * mt_impot_semi2_2019_p2
} else {
  0
}

# Impot final
(mt_impot_net_2019_p2 <- mt_impot_semi2_2019_p2 - avantage_fin_reduc_2019_p2)



# Impot Personne 2 famille ==================================================================
# Impot brut sans application du QF
mt_impot_brut_2019_sans_QF_p2b <- impot_2019(rfr_2019_duo/nb_parts_2019)


# Impot brut avec application du QF
mt_impot_brut_2019_avec_QF_p2b <- impot_2019(rfr_2019_duo/nb_parts_2019) * nb_parts_2019 / nb_payeurs_2019


# Avantage fiscal max lié au QF en fonction du nombre de part
avantage_max_QF_2019_p2b <- as_tibble(max(nb_parts_2019 - nb_payeurs_2019, 0) / 0.5 * plaf_qf_2019)

# Avantage fiscal lié au QF à retenir : min entre le plafond autorisé et l'avantage réel
avantage_fin_QF_2019_p2b <- min(avantage_max_QF_2019_p2b, mt_impot_brut_2019_sans_QF_p2b - mt_impot_brut_2019_avec_QF_p2b)

# Impot semi net après plafond QF
mt_impot_semi1_2019_p2b <- mt_impot_brut_2019_sans_QF_p2b - avantage_fin_QF_2019_p2b


# Décôte
avantage_fin_decote_2019_p2b <- min(
  if (mt_impot_semi1_2019_p2b < decote_2019[which(decote_2019$statut == "Célibataire"),2]){
    # Si décôte active
    as.numeric(decote_2019[which(decote_2019$statut == "Célibataire"),2]*0.75 - 0.75 * mt_impot_semi1_2019_p2b)
  } else {
    # Si revenu trop élevé pour bénéficier de la décôte
    0
  }
  ,as.numeric(mt_impot_semi1_2019_p2b))

mt_impot_semi2_2019_p2b <- mt_impot_semi1_2019_p2b - avantage_fin_decote_2019_p2b



# Abattement de 20%
avantage_fin_reduc_2019_p2b <- if(rev_2019_p2 < plaf_reduc_2019_p2){
  0.2 * mt_impot_semi2_2019_p2b
} else {
  0
}

# Impot final
(mt_impot_net_2019_p2b <- mt_impot_semi2_2019_p2b - avantage_fin_reduc_2019_p2b)


mt_impot_net_2019_p1
mt_impot_net_2019_p1b

mt_impot_net_2019_p2
mt_impot_net_2019_p2b

mt_impot_net_2019_duo

