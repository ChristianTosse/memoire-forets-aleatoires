# ============================================================
# foret.R
#
# Contient la prédiction d'un arbre unique, l'agrégation en
# forêt par vote majoritaire (commune aux 3 méthodes maison),
# et les fonctions d'évaluation de l'erreur de test.
#
# Dépend de : arbres.R (doit être chargé avant celui-ci via
# source(), car predire_arbre() est utilisée sur les objets
# produits par construire_purement_aleatoire() / construire_SRF()).
# ============================================================

#' Prédit l'étiquette d'une nouvelle observation à partir d'un arbre
#' (représenté comme une liste de feuilles, cf. arbres.R)
#'
#' @param arbre liste de feuilles, chacune avec $bounds (matrice d x 2) et $label
#' @param x     vecteur de longueur d, l'observation à classer (déjà normalisée dans [0,1]^d)
#' @return 0 ou 1
predire_arbre <- function(arbre, x) {
  for (feuille in arbre) {
    b <- feuille$bounds
    if (all(x >= b[, 1] & x <= b[, 2])) {
      return(feuille$label)
    }
  }
  stop("Aucune feuille ne contient ce point x — vérifier les bornes.")
}

#' Construit une forêt de m arbres purement aléatoires (PRF ou PRFsM)
#'
#' @param X, y  données d'apprentissage
#' @param k     nombre de feuilles par arbre
#' @param m     nombre d'arbres dans la forêt
#' @param mode  "aleatoire" (PRF) ou "mediane" (PRFsM)
#' @return liste de m arbres (chacun = liste de feuilles)
construire_foret_purement_aleatoire <- function(X, y, k, m, mode = c("aleatoire", "mediane")) {
  mode <- match.arg(mode)
  replicate(m, construire_purement_aleatoire(X, y, k, mode), simplify = FALSE)
}

#' Construit une forêt de m arbres selon l'Algorithme 1 (SRF)
#'
#' @param X, y  données d'apprentissage
#' @param k     plafond maximal de feuilles par arbre
#' @param m     nombre d'arbres
#' @return liste de m arbres (chacun = liste de feuilles)
construire_foret_SRF <- function(X, y, k, m) {
  replicate(m, construire_SRF(X, y, k), simplify = FALSE)
}

#' Prédiction de la forêt par vote majoritaire (équation (2.11) du mémoire)
#'
#' @param foret liste de m arbres
#' @param x     observation à classer
#' @return 0 ou 1
predire_foret <- function(foret, x) {
  votes <- sapply(foret, function(arbre) predire_arbre(arbre, x))
  n1 <- sum(votes == 1)
  n0 <- length(votes) - n1
  if (n1 > n0) return(1L)
  if (n0 > n1) return(0L)
  sample(c(0L, 1L), 1)
}

#' Calcule l'erreur de classification d'une forêt (PRF, PRFsM ou SRF) sur un jeu de test
#'
#' @param foret       liste d'arbres (construite par une des fonctions ci-dessus)
#' @param X_test, y_test  données de test, X_test déjà normalisée dans [0,1]^d
#' @return proportion d'erreurs (un nombre entre 0 et 1)
erreur_foret <- function(foret, X_test, y_test) {
  predictions <- sapply(seq_len(nrow(X_test)), function(i) predire_foret(foret, X_test[i, ]))
  mean(predictions != y_test)
}

#' Erreur de test d'une vraie forêt de Breiman, pour comparaison
#'
#' Nécessite le package randomForest (library(randomForest) doit être
#' appelé dans le script principal, pas ici).
#'
#' @param X_train, y_train  données d'apprentissage
#' @param X_test, y_test    données de test
#' @param ntree             nombre d'arbres de la forêt
#' @return proportion d'erreurs (un nombre entre 0 et 1)
erreur_rf_reel <- function(X_train, y_train, X_test, y_test, ntree = 100) {
  modele <- randomForest(x = X_train, y = as.factor(y_train), ntree = ntree)
  predictions <- predict(modele, X_test)
  mean(as.integer(as.character(predictions)) != y_test)
}

# ============================================================
# AJOUT : scores continus pour le calcul de l'AUC
# ============================================================

#' Proportion d'arbres votant pour la classe 1 (score continu)
#'
#' @param foret liste de m arbres
#' @param x     observation à classer
#' @return proportion entre 0 et 1
score_foret <- function(foret, x) {
  votes <- sapply(foret, function(arbre) predire_arbre(arbre, x))
  mean(votes == 1)
}

#' Vecteur des scores d'une forêt sur un jeu de test
#'
#' @param foret       liste d'arbres
#' @param X_test      données de test
#' @return vecteur de proportions (une par observation)
scores_foret <- function(foret, X_test) {
  sapply(seq_len(nrow(X_test)), function(i) score_foret(foret, X_test[i, ]))
}

#' Score d'une vraie forêt de Breiman (proportion d'arbres votant pour la classe 1)
#'
#' @param X_train, y_train  données d'apprentissage
#' @param X_test            données de test
#' @param ntree             nombre d'arbres
#' @return vecteur de proportions
score_rf_reel <- function(X_train, y_train, X_test, ntree = 100) {
  modele <- randomForest(x = X_train, y = as.factor(y_train), ntree = ntree)
  predict(modele, X_test, type = "prob")[, 2]
}

