# ============================================================
# fonctions_arbres.R
#
# Contient la construction d'un arbre UNIQUE, pour les trois
# mécanismes du mémoire :
#   - PRF   (Chapitre 3, §3.1) : coupe aléatoire
#   - PRFsM (Chapitre 3, §3.3) : coupe au milieu
#   - SRF   (Chapitre 4, §4.1) : Algorithme 1 (dimension max,
#            coupe au milieu, arrêt sur pureté)
#
# Représentation commune aux trois : un arbre est une LISTE DE
# FEUILLES, chacune définie par :
#   $bounds  : matrice d x 2 (bornes min/max sur chaque dimension)
#   $indices : indices des observations d'apprentissage tombées ici
#   $label   : étiquette majoritaire de la feuille (ajoutée à la fin)
#
# Ce fichier ne dépend d'aucun autre fichier du projet.
# ============================================================

#' Étiquette majoritaire d'un ensemble d'observations (vote à l'intérieur d'une feuille)
#'
#' @param indices indices des observations d'apprentissage tombées dans la feuille
#' @param y       vecteur complet des étiquettes (0/1)
#' @return 0 ou 1 ; égalité départagée au hasard (comme "ties broken arbitrarily" dans l'article)
label_majorite <- function(indices, y) {
  if (length(indices) == 0) {
    # cellule vide (rare, mais possible si une coupe tombe mal) : on tire à pile ou face
    return(sample(c(0L, 1L), 1))
  }
  n1 <- sum(y[indices] == 1)
  n0 <- length(indices) - n1
  if (n1 > n0) return(1L)
  if (n0 > n1) return(0L)
  sample(c(0L, 1L), 1)   # égalité stricte : on départage arbitrairement
}

#' Vérifie si une cellule est "pure" au sens de l'Algorithme 1 (§4.1) :
#' toutes les observations qu'elle contient partagent la même étiquette
#' (une cellule avec 0 ou 1 observation est considérée pure par convention).
est_pure <- function(indices, y) {
  length(unique(y[indices])) <= 1
}

#' Construit un arbre purement aléatoire (PRF) ou à coupe médiane (PRFsM)
#'
#' Reprend exactement la construction du §3.1 (PRF) / §3.3 (PRFsM) du mémoire :
#' k-1 coupes successives, chacune sur une feuille tirée au hasard, une dimension
#' tirée au hasard, et une position soit aléatoire (PRF) soit au milieu (PRFsM).
#'
#' @param X    matrice des variables explicatives, déjà normalisée dans [0,1]^d
#' @param y    vecteur des étiquettes (0/1)
#' @param k    nombre de feuilles souhaité (k >= 2)
#' @param mode "aleatoire" pour PRF, "mediane" pour PRFsM
#' @return une liste de feuilles, chacune = list(bounds, indices, label)
construire_purement_aleatoire <- function(X, y, k, mode = c("aleatoire", "mediane")) {
  mode <- match.arg(mode)
  d <- ncol(X)
  n <- nrow(X)
  
  # Cellule initiale : tout l'espace [0,1]^d, toutes les observations
  bounds_init <- matrix(c(0, 1), nrow = d, ncol = 2, byrow = TRUE)
  feuilles <- list(list(bounds = bounds_init, indices = seq_len(n)))
  
  # On répète k-1 fois la procédure de coupe (Définition 3.1 du mémoire)
  for (iter in seq_len(k - 1)) {
    
    # 1. Sélection d'une feuille au hasard, uniformément parmi les feuilles actuelles
    i_feuille <- sample(seq_along(feuilles), 1)
    feuille   <- feuilles[[i_feuille]]
    
    j <- sample(seq_len(d), 1)   # 2. dimension tirée au hasard parmi les d
    
    borne_inf <- feuille$bounds[j, 1]
    borne_sup <- feuille$bounds[j, 2]
    
    # 3. Position de coupe : aléatoire uniforme (PRF) ou au milieu (PRFsM)
    position <- if (mode == "aleatoire") {
      runif(1, borne_inf, borne_sup)
    } else {
      (borne_inf + borne_sup) / 2
    }
    
    # Construction des deux sous-cellules (gauche : dim j < position ; droite : >=)
    bounds_gauche <- feuille$bounds; bounds_gauche[j, 2] <- position
    bounds_droite <- feuille$bounds; bounds_droite[j, 1] <- position
    
    idx <- feuille$indices
    idx_gauche <- idx[X[idx, j] <  position]
    idx_droite <- idx[X[idx, j] >= position]
    
    # On remplace la feuille coupée par sa moitié gauche, et on ajoute la moitié droite
    feuilles[[i_feuille]]       <- list(bounds = bounds_gauche, indices = idx_gauche)
    feuilles[[length(feuilles) + 1]] <- list(bounds = bounds_droite, indices = idx_droite)
  }
  
  # Une fois l'arbre construit, on calcule l'étiquette de chaque feuille
  for (i in seq_along(feuilles)) {
    feuilles[[i]]$label <- label_majorite(feuilles[[i]]$indices, y)
  }
  
  feuilles
}

#' Construit un arbre selon l'Algorithme 1 (variante simplifiée de Breiman, §4.1)
#'
#' @param X matrice des variables explicatives, normalisée dans [0,1]^d
#' @param y vecteur des étiquettes (0/1)
#' @param k nombre maximal de feuilles (plafond de sécurité, k >= 2)
#' @return une liste de feuilles, chacune = list(bounds, indices, label)
construire_SRF <- function(X, y, k) {
  d <- ncol(X)
  n <- nrow(X)
  
  bounds_init <- matrix(c(0, 1), nrow = d, ncol = 2, byrow = TRUE)
  cellule_init <- list(bounds = bounds_init, indices = seq_len(n))
  
  # P : file d'attente des cellules encore à examiner (pas encore classées "finales")
  P <- list(cellule_init)
  feuilles_finales <- list()   # cellules devenues pures : ne seront plus coupées
  n_feuilles <- 1L             # compte la cellule initiale
  
  while (n_feuilles < k && length(P) > 0) {
    
    # 1. On retire la première cellule de la file (FIFO, comme le pseudocode)
    C <- P[[1]]
    P <- P[-1]
    
    if (est_pure(C$indices, y)) {
      # 2. Cellule pure : on ne la coupe plus, elle devient une feuille finale
      feuilles_finales[[length(feuilles_finales) + 1]] <- C
      
    } else {
      # 3. Cellule impure : on sélectionne la (ou les) dimension(s) de plus
      #    grande étendue actuelle, et on tire au hasard parmi elles en cas d'égalité
      etendues <- C$bounds[, 2] - C$bounds[, 1]
      dims_max <- which(etendues == max(etendues))
      j <- if (length(dims_max) == 1) dims_max else sample(dims_max, 1)
      
      # 4. Coupe au milieu de cette dimension (jamais au hasard, contrairement au PRF)
      position <- (C$bounds[j, 1] + C$bounds[j, 2]) / 2
      
      bounds_gauche <- C$bounds; bounds_gauche[j, 2] <- position
      bounds_droite <- C$bounds; bounds_droite[j, 1] <- position
      
      idx <- C$indices
      idx_gauche <- idx[X[idx, j] <  position]
      idx_droite <- idx[X[idx, j] >= position]
      
      # 5. Les deux nouvelles cellules rejoignent la file d'attente P
      P[[length(P) + 1]] <- list(bounds = bounds_gauche, indices = idx_gauche)
      P[[length(P) + 1]] <- list(bounds = bounds_droite, indices = idx_droite)
      
      n_feuilles <- n_feuilles + 1L   # une coupe = une feuille de plus au total
    }
  }
  
  # Les cellules encore dans P (non pures, mais k atteint) sont aussi des feuilles
  toutes_feuilles <- c(feuilles_finales, P)
  
  # Calcul de l'étiquette de chaque feuille (vote majoritaire à l'intérieur)
  for (i in seq_along(toutes_feuilles)) {
    toutes_feuilles[[i]]$label <- label_majorite(toutes_feuilles[[i]]$indices, y)
  }
  
  toutes_feuilles
}
