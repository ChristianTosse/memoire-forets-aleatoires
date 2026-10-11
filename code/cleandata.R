#' Prépare un jeu de données pour les arbres PRF / PRFsM / SRF
#'
#' @param df        le data.frame brut importé
#' @param label_col nom (chaîne de caractères) de la colonne d'étiquette
#' @param drop_cols vecteur des noms de colonnes à retirer (ex: identifiants)
#' @param ecreter   si TRUE, écrête chaque variable numérique à ses quantiles
#'                  1% et 99% avant normalisation (utile en présence de valeurs
#'                  aberrantes extrêmes, comme sur eggeye).
#'
#' @return une liste avec X (normalisée dans [0,1]) et y (recodé en 0/1)
preparer_donnees <- function(df, label_col, drop_cols = character(0), ecreter = FALSE) {
  
  if (length(drop_cols) > 0) {
    df <- df[, !(names(df) %in% drop_cols)]
  }
  
  y_brut <- df[[label_col]]
  X_brut <- df[, !(names(df) %in% label_col)]
  
  y <- as.integer(as.factor(y_brut)) - 1L
  
  if (length(unique(y)) != 2) {
    stop("La colonne d'étiquette ne contient pas exactement 2 classes.")
  }
  
  X_brut <- data.frame(lapply(X_brut, function(col) as.numeric(as.character(col))))
  
  # écrêtage optionnel, avant la normalisation.
  # On ramène toute valeur en dessous du 1er centile à ce centile,
  # et toute valeur au-dessus du 99e centile à ce centile.
  # Cela neutralise l'effet des valeurs extrêmes sans supprimer
  # d'observations.
  if (ecreter) {
    ecreter_colonne <- function(col) {
      bornes <- quantile(col, probs = c(0.01, 0.99), na.rm = TRUE)
      col[col < bornes[1]] <- bornes[1]
      col[col > bornes[2]] <- bornes[2]
      col
    }
    X_brut <- as.data.frame(lapply(X_brut, ecreter_colonne))
  }
  
  normaliser_colonne <- function(col) {
    (col - min(col, na.rm = TRUE)) / (max(col, na.rm = TRUE) - min(col, na.rm = TRUE))
  }
  X <- as.data.frame(lapply(X_brut, normaliser_colonne))
  
  lignes_completes <- complete.cases(X)
  X <- X[lignes_completes, ]
  y <- y[lignes_completes]
  
  list(X = as.matrix(X), y = y)
}

