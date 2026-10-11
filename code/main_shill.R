# ============================================================
# main_shill.R
#
# Script principal : charge les fonctions, prépare les données
# shill, lance l'expérience complète (variation de n, plusieurs
# répétitions), et sauvegarde le résultat sur disque.
# ============================================================

#  1. Chargement des fonctions et packages 
source("C:/Users/TOSSE D.C.Christian/Desktop/MemoireMaster/Empirique/cleandata.R")
source("C:/Users/TOSSE D.C.Christian/Desktop/MemoireMaster/Empirique/arbres.R")
source("C:/Users/TOSSE D.C.Christian/Desktop/MemoireMaster/Empirique/foret.R")
library(randomForest)
library(pROC)

# 2. Chargement et préparation des données 
shill <- read.csv("C:/Users/TOSSE D.C.Christian/Desktop/MemoireMaster/Empirique/donnees/shill+bidding+dataset/Shill Bidding Dataset.csv",
                  header = TRUE, sep = ",")

donnees_shill <- preparer_donnees(
  df        = shill,
  label_col = "Class",
  drop_cols = c("Record_ID", "Auction_ID", "Bidder_ID")
)

#  3. Split test fixe / pool d'apprentissage 
set.seed(2025)

n_total <- nrow(donnees_shill$X)
idx_test_fixe  <- sample(seq_len(n_total), size = floor(0.2 * n_total))
idx_pool_train <- setdiff(seq_len(n_total), idx_test_fixe)

X_test_fixe <- donnees_shill$X[idx_test_fixe, ]
y_test_fixe <- donnees_shill$y[idx_test_fixe]

X_pool <- donnees_shill$X[idx_pool_train, ]
y_pool <- donnees_shill$y[idx_pool_train]

#  4. Paramètres de l'expérience 

choisir_k <- function(n) {
  max(20, round(sqrt(n)))
}

valeurs_n <- c(100, 250, 500, 1000, 2000, 4000)
n_repet   <- 10
m_arbres  <- 50

#  5. Boucle principale : erreurs de test 
resultats_complet_shill <- data.frame()

for (n_val in valeurs_n) {
  k_val <- choisir_k(n_val)
  cat("=== n =", n_val, "(k =", k_val, ") ===\n")
  
  for (r in seq_len(n_repet)) {
    idx_ech <- sample(seq_len(nrow(X_pool)), size = n_val)
    X_tr <- X_pool[idx_ech, ]
    y_tr <- y_pool[idx_ech]
    
    f_prf   <- construire_foret_purement_aleatoire(X_tr, y_tr, k = k_val, m = m_arbres, mode = "aleatoire")
    f_prfsm <- construire_foret_purement_aleatoire(X_tr, y_tr, k = k_val, m = m_arbres, mode = "mediane")
    f_srf   <- construire_foret_SRF(X_tr, y_tr, k = k_val, m = m_arbres)
    
    ligne <- data.frame(
      n          = n_val,
      repetition = r,
      PRF        = erreur_foret(f_prf,   X_test_fixe, y_test_fixe),
      PRFsM      = erreur_foret(f_prfsm, X_test_fixe, y_test_fixe),
      SRF        = erreur_foret(f_srf,   X_test_fixe, y_test_fixe),
      RF_reelle  = erreur_rf_reel(X_tr, y_tr, X_test_fixe, y_test_fixe, ntree = m_arbres)
    )
    resultats_complet_shill <- rbind(resultats_complet_shill, ligne)
    cat("  répétition", r, "OK\n")
  }
}

saveRDS(resultats_complet_shill, "resultats_shill.rds")

#  6. Boucle AUC + exactitude équilibrée 

#' Calcule l'AUC et l'exactitude équilibrée
calcul_auc_balanced <- function(y_true, scores) {
  roc_obj <- pROC::roc(y_true, scores, quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  pred <- ifelse(scores >= 0.5, 1, 0)
  tp <- sum(pred == 1 & y_true == 1)
  tn <- sum(pred == 0 & y_true == 0)
  fp <- sum(pred == 1 & y_true == 0)
  fn <- sum(pred == 0 & y_true == 1)
  
  tpr <- ifelse((tp + fn) > 0, tp / (tp + fn), NA)
  tnr <- ifelse((tn + fp) > 0, tn / (tn + fp), NA)
  balanced_acc <- (tpr + tnr) / 2
  
  c(auc = auc_val, balanced_acc = balanced_acc)
}

resultats_auc_shill <- data.frame()

for (n_val in valeurs_n) {
  k_val <- choisir_k(n_val)
  cat("=== AUC n =", n_val, "===\n")
  
  auc_rep <- matrix(NA, n_repet, 4)
  bal_rep <- matrix(NA, n_repet, 4)
  
  for (r in seq_len(n_repet)) {
    idx_ech <- sample(seq_len(nrow(X_pool)), size = n_val)
    X_tr <- X_pool[idx_ech, ]
    y_tr <- y_pool[idx_ech]
    
    f_prf   <- construire_foret_purement_aleatoire(X_tr, y_tr, k = k_val, m = m_arbres, mode = "aleatoire")
    f_prfsm <- construire_foret_purement_aleatoire(X_tr, y_tr, k = k_val, m = m_arbres, mode = "mediane")
    f_srf   <- construire_foret_SRF(X_tr, y_tr, k = k_val, m = m_arbres)
    
    s_prf   <- scores_foret(f_prf,   X_test_fixe)
    s_prfsm <- scores_foret(f_prfsm, X_test_fixe)
    s_srf   <- scores_foret(f_srf,   X_test_fixe)
    s_rf    <- score_rf_reel(X_tr, y_tr, X_test_fixe, ntree = m_arbres)
    
    res_prf   <- calcul_auc_balanced(y_test_fixe, s_prf)
    res_prfsm <- calcul_auc_balanced(y_test_fixe, s_prfsm)
    res_srf   <- calcul_auc_balanced(y_test_fixe, s_srf)
    res_rf    <- calcul_auc_balanced(y_test_fixe, s_rf)
    
    auc_rep[r, ] <- c(res_prf["auc"], res_prfsm["auc"], res_srf["auc"], res_rf["auc"])
    bal_rep[r, ] <- c(res_prf["balanced_acc"], res_prfsm["balanced_acc"],
                      res_srf["balanced_acc"], res_rf["balanced_acc"])
    
    cat("  répétition", r, "OK\n")
  }
  
  for (j in 1:4) {
    resultats_auc_shill <- rbind(resultats_auc_shill, data.frame(
      n = n_val,
      methode = c("PRF", "PRFsM", "SRF", "RF_reelle")[j],
      auc_moy = mean(auc_rep[, j], na.rm = TRUE),
      auc_sd  = sd(auc_rep[, j], na.rm = TRUE),
      bal_moy = mean(bal_rep[, j], na.rm = TRUE),
      bal_sd  = sd(bal_rep[, j], na.rm = TRUE)
    ))
  }
}

saveRDS(resultats_auc_shill, "resultats_auc_shill.rds")