

calcul_impot_solo <- function(rfr){
  
  # Talbeau à compléter
  col <- c("Impôt brut", "Impôt brut ajusté QF", "Avantage décôte", "Impôt après décôte","Avantage réduc","Impôt net")
  
  # Calcul de l'impot brut en 2019 selon le revenu défini en input
    data <- 
      bareme_2019 %>%
      mutate(max_rev_tr = cumsum(max_tr_2019),
             rev_impose = if_else(rfr > max_rev_tr,
                                  max_tr_2019 - min_tr_2019,
                                  pmax(pmin(rfr - min_tr_2019, max_tr_2019 - min_tr_2019),0)), # pmax : avoir le max par ligne (pas possible avec la fonction max)
             impot_tr = tx_2019 * rev_impose)
    # Impot brut sans application du QF
    mt_impot_brut_2019_sans_QF_p1 <- sum(data$impot_tr)
    # Impot semi net après plafond QF
    mt_impot_semi1_2019_p1 <- mt_impot_brut_2019_sans_QF_p1
    
    
    # Décôte
    avantage_fin_decote_2019_p1 <- min(
      if (mt_impot_semi1_2019_p1 < decote_2019[which(decote_2019$statut == "Célibataire"),2]){
        # Si décôte active
        decote_2019[which(decote_2019$statut == "Célibataire"),2]*0.75 - 0.75 * mt_impot_semi1_2019_p1
      } else {
        # Si revenu trop élevé pour bénéficier de la décôte
        0
      }
      ,mt_impot_semi1_2019_p1)
    
    # Après décôte
    mt_impot_semi2_2019_p1 <- mt_impot_semi1_2019_p1 - avantage_fin_decote_2019_p1
    
    # Abattement de 20%
    avantage_fin_reduc_2019_p1 <- if(rev_2019_p1 < plaf_reduc_2019_p1){
      0.2 * mt_impot_semi2_2019_p1
    } else {
      0
    }
    
    # Impôt net
    mt_impot_net_2019_p1 <- mt_impot_semi2_2019_p1 - avantage_fin_reduc_2019_p1
    
    val <- c(mt_impot_brut_2019_sans_QF_p1,mt_impot_semi1_2019_p1,avantage_fin_decote_2019_p1,mt_impot_semi2_2019_p1,avantage_fin_reduc_2019_p1,mt_impot_net_2019_p1)
    
    synthese <- cbind.data.frame(col,val)
    
}

e <- calcul_impot_solo(12000)


