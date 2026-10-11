# ============================================================
# main_eggeye.R
# ============================================================

source("C:/Users/TOSSE D.C.Christian/Desktop/MemoireMaster/Empirique/cleandata.R")
source("C:/Users/TOSSE D.C.Christian/Desktop/MemoireMaster/Empirique/arbres.R")
source("C:/Users/TOSSE D.C.Christian/Desktop/MemoireMaster/Empirique/foret.R")
library(randomForest)
library(foreign)

# --- Données ---
eggeye <- read.arff("C:/Users/TOSSE D.C.Christian/Desktop/MemoireMaster/Empirique/donnees/eeg+eye+state/EEG Eye State.arff")

donnees_eggeye <- preparer_donnees(
  df        = eggeye,
  label_col = "eyeDetection",
  drop_cols = character(0),
  ecreter   = TRUE
)

stopifnot(
  all(donnees_eggeye$X >= 0 & donnees_eggeye$X <= 1),
  all(donnees_eggeye$y %in% c(0, 1))
)

# --- Split test fixe / pool ---
set.seed(2025)
n_total <- nrow(donnees_eggeye$X)
idx_test_fixe  <- sample(seq_len(n_total), size = floor(0.2 * n_total))
idx_pool_train <- setdiff(seq_len(n_total), idx_test_fixe)

X_test_fixe <- donnees_eggeye$X[idx_test_fixe, ]
y_test_fixe <- donnees_eggeye$y[idx_test_fixe]
X_pool <- donnees_eggeye$X[idx_pool_train, ]
y_pool <- donnees_eggeye$y[idx_pool_train]

# --- Paramètres ---
choisir_k <- function(n) max(20, round(sqrt(n)))
valeurs_n <- c(100, 250, 500, 1000, 2000, 4000)
n_repet   <- 10
m_arbres  <- 50

# --- Boucle ---
resultats_complet_eggeye <- data.frame()

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
      n = n_val, repetition = r,
      PRF       = erreur_foret(f_prf,   X_test_fixe, y_test_fixe),
      PRFsM     = erreur_foret(f_prfsm, X_test_fixe, y_test_fixe),
      SRF       = erreur_foret(f_srf,   X_test_fixe, y_test_fixe),
      RF_reelle = erreur_rf_reel(X_tr, y_tr, X_test_fixe, y_test_fixe, ntree = m_arbres)
    )
    resultats_complet_eggeye <- rbind(resultats_complet_eggeye, ligne)
    cat("  répétition", r, "OK\n")
  }
}

saveRDS(resultats_complet_eggeye, "resultats_eggeye.rds")
cat("Terminé.\n")
