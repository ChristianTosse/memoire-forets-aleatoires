# memoire-forets-aleatoires
# Mémoire de Master - Analyse des vitesses de convergence des forêts aléatoires pour la classification

**Auteur :** Christian D. C. TOSSE  
**Encadreur :** Dr Freedath Djibril MOUSSA  
**Co-Superviseurs :** Drs Amour GBAGUIDI AMOUSSOU & Raymond AFFOSSOGBE  
**Institution :** Institut de Mathématiques et de Sciences Physiques (IMSP), Université d'Abomey-Calavi  
**Année :** 2025-2026  

---

## Objet

Ce dépôt contient le code R, les résultats et les figures associés au Chapitre 5 du mémoire, consacré à l'illustration expérimentale des vitesses de convergence des forêts aléatoires pour la classification.

Le mémoire étudie l'article de référence :

> Wei Gao, Fan Xu, Zhi-Hua Zhou. *Towards convergence rate analysis of random forests for classification*. Artificial Intelligence, 313:103788, 2022.

Trois variantes de forêts aléatoires sont analysées :
- **PRFs** — forêts purement aléatoires
- **PRFsM** — forêts purement aléatoires à coupe médiane
- **SRFs** — variante simplifiée de l'algorithme de Breiman

Une quatrième méthode, la **forêt de Breiman réelle** (package `randomForest` de R), est incluse à titre de comparaison empirique.

---

## Structure du dépôt

### Détail des fichiers

| Dossier | Fichier | Rôle |
|---------|---------|------|
| `code/` | `cleandata.R` | Importation, nettoyage et normalisation des données |
| `code/` | `arbres.R` | Construction d'un arbre individuel (PRFs, PRFsM, SRFs) |
| `code/` | `foret.R` | Agrégation en forêt, prédiction, calcul de l'erreur |
| `code/` | `main_shill.R` | Expérience complète sur le jeu `shill` |
| `code/` | `main_eggeye.R` | Expérience complète sur le jeu `eggeye` |
| `code/` | `rapportShill.Rmd` | Rapport d'analyse pour `shill` |
| `code/` | `rapportEggeye.Rmd` | Rapport d'analyse pour `eggeye` |
| `resultats/` | `resultats_shill.rds` | Erreurs de test sur `shill` |
| `resultats/` | `resultats_eggeye.rds` | Erreurs de test sur `eggeye` |
| `resultats/` | `resultats_auc_shill.rds` | AUC et exactitude équilibrée sur `shill` |
| `resultats/` | `resultats_auc_eggeye.rds` | AUC et exactitude équilibrée sur `eggeye` |
| `figures/` | `convshill_ic.png` | Courbe de convergence sur `shill` |
| `figures/` | `conveggeye_ic.png` | Courbe de convergence sur `eggeye` |

---

## Protocole expérimental

Pour chaque jeu de données :

- Séparation 80 % apprentissage / 20 % test (test fixe pour toute l'expérience).
- Tailles d'échantillon : \(n \in \{100, 250, 500, 1000, 2000, 4000\}\).
- 10 répétitions par taille d'échantillon.
- Nombre d'arbres par forêt : \(m = 50\).
- Nombre de feuilles par arbre : \(k = \max(20, \lceil\sqrt{n}\rceil)\).

Pour chaque méthode et chaque configuration, sont calculés :
- l'erreur de test moyenne, l'écart-type et l'intervalle de confiance à 95 % ;
- l'AUC (aire sous la courbe ROC) ;
- l'exactitude équilibrée (balanced accuracy).

---

## Données

Les deux jeux de données utilisés dans ce mémoire sont issus du UCI Machine Learning Repository.

### Shill Bidding Dataset

- **Lien** : https://archive.ics.uci.edu/dataset/562/shill+bidding+dataset
- **Taille** : 6 321 observations, 9 variables
- **Citation** : Shill Bidding Dataset (2020). UCI Machine Learning Repository. https://doi.org/10.24432/C5Z611

### EEG Eye State

- **Lien** : https://archive.ics.uci.edu/dataset/264/eeg+eye+state
- **Taille** : 14 980 observations, 14 variables
- **Citation** : Roesler, O. (2013). EEG Eye State. UCI Machine Learning Repository. https://doi.org/10.24432/C57G7J

---

## Reproduction des résultats

Pour reproduire les résultats :

1. Ouvrir R ou RStudio.
2. Installer les packages nécessaires :
   install.packages(c("randomForest", "pROC", "ggplot2", "foreign", "xtable"))
3. Exécuter les scripts dans l'ordre :
      source("code/main_shill.R")
      source("code/main_eggeye.R")
4. Générer les rapports :
   rmarkdown::render("code/rapportShill.Rmd")
   rmarkdown::render("code/rapportEggeye.Rmd")


