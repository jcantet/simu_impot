# Script pour tester les calculs d'impôts avant de réaliser l'application
options(scipen = 999999)

# Packages ====
library(dplyr)



# Infos 2019


nb_payeurs_2019 <- 1
situation <- ifelse(nb_payeurs_2019 == 1, "Célibataire","Couple")
nb_parts_2019 <- 1
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

rev_2019 <- 21000

rfr_2019 <- rev_2019 * 0.9 # Par défaut, c'est l'abattement de 10% qui est retenu, sinon on prendra la valeur renseignée par l'utilisateur

qf_2019 <- rfr_2019 / nb_payeurs_2019


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


# Impot brut sans application du QF
mt_impot_brut_2019_sans_QF <- impot_2019(rfr_2019) * nb_payeurs_2019


# Impot brut avec application du QF
mt_impot_brut_2019_avec_QF <- impot_2019(rfr_2019/nb_parts_2019) * nb_parts_2019


# Avantage fiscal max lié au QF en fonction du nombre de part
avantage_max_QF_2019 <- as_tibble(max(nb_parts_2019 - nb_payeurs_2019, 0) / 0.5 * plaf_qf_2019)

# Avantage fiscal lié au QF à retenir : min entre le plafond autorisé et l'avantage réel
avantage_fin_QF_2019 <- min(avantage_max_QF_2019, mt_impot_brut_2019_sans_QF - mt_impot_brut_2019_avec_QF)

# Impot semi net après plafond QF
mt_impot_semi1_2019 <- mt_impot_brut_2019_sans_QF - avantage_fin_QF_2019

# Décôte
avantage_fin_decote_2019 <- min(
    if (mt_impot_semi1_2019 < decote_2019[which(decote_2019$statut == situation),2]){
    # Si décôte active
    decote_2019[which(decote_2019$statut == situation),2]*0.75 - 0.75 * mt_impot_semi1_2019
  } else {
    # Si revenu trop élevé pour bénéficier de la décôte
    0
  }
  ,mt_impot_semi1_2019)

mt_impot_semi2_2019 <- mt_impot_semi1_2019 - avantage_fin_decote_2019



# Abattement de 20%
avantage_fin_reduc_2019 <- if(rev_2019 < (reduc_2019[which(reduc_2019$statut_reduc == situation),2] + reduc_2019[3,2] * (nb_parts_2019 - nb_payeurs_2019)/0.5)){
  0.2 * mt_impot_semi2_2019
} else {
  0
}

# Impot final
(mt_impot_net_2019 <- mt_impot_semi2_2019 - avantage_fin_reduc_2019)

