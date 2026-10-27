# Profils financiers des emprunteurs : analyse par ACP

Projet d'**Analyse de Données** réalisé en Master 1 (Économétrie et Statistique appliquée), en binôme avec Ly Néné Gallé.

## Objectif
Explorer et caractériser les profils financiers d'un échantillon de 256 emprunteurs (revenu, dépenses, épargne, endettement, type de prêt) par une approche descriptive puis multidimensionnelle.

## Données
`data/DATA.csv` : 256 individus, 11 variables quantitatives (âge, revenu mensuel, dépenses, épargne, montant/durée/mensualité/taux du prêt, ratio dette/revenu, score de crédit, ratio épargne/revenu) et 3 qualitatives (sexe, région, type de prêt). Source : Kaggle. Le jeu de données semble synthétique : les résultats sont **illustratifs**.

## Méthodes
1. **Analyse univariée** : histogrammes, boîtes à moustaches, détection des valeurs atypiques (test ESD de Rosner).
2. **Analyse bivariée** : corrélations de Pearson, nuages de points, tableaux de contingence, tests du χ².
3. **ACP** : tests KMO et de Bartlett, retrait des variables à faible MSA, choix du nombre d'axes (coude, critère de Kaiser), interprétation des individus et des variables.

## Résultats clés
- Revenu et dépenses sont fortement corrélés (r ≈ 0,81) ; montant du prêt et mensualité aussi.
- Les variables qualitatives (sexe, région, type de prêt) ne sont pas associées entre elles (χ² non significatifs).
- 2 axes retenus, environ 69 % de l'inertie : l'axe 1 reflète le niveau financier global (revenu, dépenses, épargne, prêt, mensualité), l'axe 2 la contrainte d'endettement (ratio dette/revenu). Le score de crédit est relativement indépendant du plan principal.

## Contenu
| Fichier | Description |
|---|---|
| `analyse_acp.qmd` | Rapport Quarto (code + texte) |
| `analyse_acp.R` | Script R de l'analyse |
| `rapport.pdf` | Rapport final rendu |
| `data/DATA.csv` | Données |

## Reproduire
Ouvrir le dossier comme projet RStudio (ou s'y placer) puis rendre `analyse_acp.qmd` avec Quarto. Packages : `tidyverse`, `psych`, `corrplot`, `PerformanceAnalytics`, `EnvStats`, `lsr`, `vcd`, `reshape2`, `mice`, `FactoMineR`, `missMDA`, `factoextra`.

## Outils
R, Quarto, FactoMineR.
