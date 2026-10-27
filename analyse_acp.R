# title: "Profils financiers des emprunteurs : Approche par Analyse en Composantes Principales"
# author: "DAGBO Elisée & Ly Néné Gallé"

# Library
library(psych)
library(corrplot)
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(PerformanceAnalytics)
library(EnvStats)
library(lsr)
library(vcd)
library(reshape2)
library(mice)
library(FactoMineR)
library(missMDA)
library(factoextra)

# Analyse Univariée

## Analyse Univariée des variables quantitatives

library(readr)
DATA <- read_csv("data/DATA.csv")

### Conversion des types de variables

DATA$Type_Pret <- as.factor(DATA$Type_Pret)
DATA$Region <- as.factor(DATA$Region)
DATA$Sexe <- as.factor(DATA$Sexe)

### Vérification des types

str(DATA)
summary(DATA)

### Histogrammes des variables quantitatives

#| fig-width: 10
#| fig-height: 10
par(mfrow = c(3, 2), mar = c(3, 3, 2, 1))

# Liste des variables quantitatives
quanti_vars <- c("Age", "Revenu_Mensuel", "Depenses_Mensuelles", "Epargne",
                 "Montant_Pret", "Duree_Pret_Mois", "Mensualite_Pret",
                 "Taux_Interet_Pret", "Ratio_Dette_Revenu", "Score_Credit",
                 "Ratio_Epargne_Revenu")



# Histogrammes 
for (v in quanti_vars) {
  hist(DATA[[v]],
       main = paste("Histogramme de", v),
       xlab = v,
       ylab = "Fréquence",
       col = "blue",
       border = "white")
}

### Box-plots des variables quantitatives

#| fig-width: 10
#| fig-height: 10
# Liste des variables quantitatives
quanti_vars <- c("Age", "Revenu_Mensuel", "Depenses_Mensuelles", "Epargne",
                 "Montant_Pret", "Duree_Pret_Mois", "Mensualite_Pret",
                 "Taux_Interet_Pret", "Ratio_Dette_Revenu", "Score_Credit",
                 "Ratio_Epargne_Revenu")

# Mise en page : 4 lignes x 3 colonnes
par(mfrow = c(3, 2), mar = c(3, 3, 2, 1))

# Boucle pour tracer les boxplots
for (v in quanti_vars) {
  boxplot(DATA[[v]],
          main = paste("Boxplot de", v),
          
          col = "lightblue",
          border = "blue")
  
}

### Gestion des outliers : TEST ESD
# FONCTION POUR CALCULER R_i

rval <- function(y) {
  ares <- abs(y - mean(y)) / sd(y)
  df <- data.frame(y = y, ares = ares)
  r <- max(df$ares)
  list(r = r, df = df)
}

alpha <- 0.05
k <- 20   # nombre maximum d’outliers testés


# 1) VARIABLE : EPARGNE

y <- DATA$Epargne
n <- length(y)

lam <- numeric(k)
R <- numeric(k)

for (i in 1:k) {
  if (i == 1) {
    rt <- rval(y)
    R[i] <- rt$r
    newdf <- rt$df[rt$df$ares != max(rt$df$ares), ]
  } else {
    rt <- rval(newdf$y)
    R[i] <- rt$r
    newdf <- rt$df[rt$df$ares != max(rt$df$ares), ]
  }
  p <- 1 - alpha / (2 * (n - i + 1))
  t <- qt(p, n - i - 1)
  lam[i] <- t * (n - i) / sqrt((n - i - 1 + t^2) * (n - i + 1))
}

esd_epargne <- data.frame(
  No_Outliers = 1:k,
  Test_Stat = R,
  Critical_Value = lam
)

print("===== ESD Test - Variable: Epargne =====")
print(esd_epargne)


# 2) VARIABLE : MONTANT_PRET

y <- DATA$Montant_Pret
n <- length(y)

lam <- numeric(k)
R <- numeric(k)

for (i in 1:k) {
  if (i == 1) {
    rt <- rval(y)
    R[i] <- rt$r
    newdf <- rt$df[rt$df$ares != max(rt$df$ares), ]
  } else {
    rt <- rval(newdf$y)
    R[i] <- rt$r
    newdf <- rt$df[rt$df$ares != max(rt$df$ares), ]
  }
  p <- 1 - alpha / (2 * (n - i + 1))
  t <- qt(p, n - i - 1)
  lam[i] <- t * (n - i) / sqrt((n - i - 1 + t^2) * (n - i + 1))
}

esd_montant <- data.frame(
  No_Outliers = 1:k,
  Test_Stat = R,
  Critical_Value = lam
)

print("===== ESD Test - Variable: Montant_Pret =====")
print(esd_montant)


# 3) VARIABLE : MENSUALITE_PRET

y <- DATA$Mensualite_Pret
n <- length(y)

lam <- numeric(k)
R <- numeric(k)

for (i in 1:k) {
  if (i == 1) {
    rt <- rval(y)
    R[i] <- rt$r
    newdf <- rt$df[rt$df$ares != max(rt$df$ares), ]
  } else {
    rt <- rval(newdf$y)
    R[i] <- rt$r
    newdf <- rt$df[rt$df$ares != max(rt$df$ares), ]
  }
  p <- 1 - alpha / (2 * (n - i + 1))
  t <- qt(p, n - i - 1)
  lam[i] <- t * (n - i) / sqrt((n - i - 1 + t^2) * (n - i + 1))
}

esd_mensualite <- data.frame(
  No_Outliers = 1:k,
  Test_Stat = R,
  Critical_Value = lam
)

print("===== ESD Test - Variable: Mensualite_Pret =====")
print(esd_mensualite)

# Supression de ces 4 outliers pour Montant_Pret


# 1) Identifier les indices des 4 valeurs les plus extrêmes
indices_outliers <- order(abs(DATA$Montant_Pret - mean(DATA$Montant_Pret)),
                          decreasing = TRUE)[1:4]

indices_outliers

# 2) Supprimer uniquement ces observations
DATA <- DATA[-indices_outliers, ]

# 3) Vérification
# dim avant = 256 x 14
dim(DATA)

## Analyse univariée des variables qualitatives

### Diagramme Circulaire

couleurs <- c(
  "Immobilier" = "firebrick3",
  "Business" = "sienna2",
  "Education" = "mediumpurple3",
  "Automobile" = "lightblue",
  "Amérique_Nord" = "mistyrose2",
  "Europe" = "skyblue4",
  "Asie" = "darksalmon",
  "Afrique" = "gray40")


# diagrammes circulaires

par(mfrow = c(2, 2), mar = c(2, 2, 3, 1))

# 1) Type de prêt

freq_type <- table(DATA$Type_Pret)
pct_type <- round(prop.table(freq_type) * 100, 1)

pie(freq_type,
    labels = paste(names(freq_type), "\n", pct_type, "%"),
    col = couleurs[names(freq_type)],
    main = "Type de prêt")


# 2) Région

freq_region <- table(DATA$Region)
pct_region <- round(prop.table(freq_region) * 100, 1)

pie(freq_region,
    labels = paste(names(freq_region), "\n", pct_region, "%"),
    col = couleurs[names(freq_region)],
    main = "Région")


# 3) Sexe

freq_sexe <- table(DATA$Sexe)
pct_sexe <- round(prop.table(freq_sexe) * 100, 1)

pie(freq_sexe,
    labels = paste(names(freq_sexe), "\n", pct_sexe, "%"),
    main = "Sexe",
    col = rainbow(length(freq_sexe)))

# Analyse Bivariée

## Relation entre 2 variables quantitatives :

### Matrice de corrélation de Pearson

data_quant <- DATA[quanti_vars]
#| fig-width: 8
#| fig-height: 8
# Matrice de corrélation de Pearson
cor_matrix <- cor(data_quant, use = "complete.obs", method = "pearson")

corrplot(round(cor_matrix, 2), method = "color", type = "upper", 
         addCoef.col = "black",tl.cex = 0.8, tl.col = "black", number.cex = 0.5)

### Nuages de points (scatter plots)

## 1. Montant_Pret × Mensualite_Pret
ggplot(DATA, aes(x = Montant_Pret, y = Mensualite_Pret)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Montant du prêt",
       y = "Mensualité du prêt",
       title = "Nuage de points : Montant du prêt vs Mensualité")

## 2. Mensualité_Pret × Ratio_Dette_Revenu
ggplot(DATA, aes(x = Mensualite_Pret, y = Ratio_Dette_Revenu)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Mensualite du prêt",
       y = "Ratio_Dette_Revenu",
       title = "Nuage de points : Mensualité du prêt vs Ratio_Dette_Revenu")

## 3. Revenu_Mensuel × Depenses_Mensuelles
ggplot(DATA, aes(x = Revenu_Mensuel, y = Depenses_Mensuelles)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Revenu mensuel",
       y = "Dépenses mensuelles",
       title = "Nuage de points : Revenu vs Dépenses")

## 4. Montant_Pret × Duree_Pret_Mois
ggplot(DATA, aes(x = Montant_Pret, y = Duree_Pret_Mois)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Montant du prêt",
       y = "Duree_Pret_Mois",
       title = "Nuage de points : Montant du pret vs Duree du Pret en Mois")

## 5. Score_Credit × Ratio_Dette_Revenu
ggplot(DATA, aes(x = Ratio_Dette_Revenu, y = Score_Credit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Ratio dette / revenu",
       y = "Score de crédit",
       title = "Nuage de points : Score de crédit vs Ratio dette/revenu")

## Relation entre 2 variables qualitatives :

vars_quali <- c("Type_Pret", "Region", "Sexe")

DATA_quali <- DATA[vars_quali]

### Tableaux de contingence


# Créer une liste vide pour stocker les tableaux
tabs_qual <- list()

# Boucle sur toutes les paires de variables qualitatives
for (i in 1:(length(vars_quali) - 1)) {
  for (j in (i + 1):length(vars_quali)) {
    var1 <- vars_quali[i]
    var2 <- vars_quali[j]
    
    tab_ij <- table(DATA[[var1]], DATA[[var2]])
    
    # Ajouter les totaux (lignes, colonnes, total général)
    tab_ij_tot <- addmargins(tab_ij)
    
    # Sauvegarde dans la liste (avec totaux)
    nom_tab <- paste(var1, var2, sep = "_x_")
    tabs_qual[[nom_tab]] <- tab_ij_tot
    
    # Affichage
    cat("\n=============================\n")
    cat("Tableau de contingence :", var1, "x", var2, "\n")
    print(tab_ij_tot)
    
  }
}

### Test du X² 

res_chi2 <- data.frame(
  Var1    = character(),
  Var2    = character(),
  Khi2    = numeric(),
  ddl     = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:(length(vars_quali) - 1)) {
  for (j in (i + 1):length(vars_quali)) {
    var1 <- vars_quali[i]
    var2 <- vars_quali[j]
    
    tab_ij  <- table(DATA[[var1]], DATA[[var2]])
    test_ij <- chisq.test(tab_ij)
    
    res_chi2 <- rbind(
      res_chi2,
      data.frame(
        Var1    = var1,
        Var2    = var2,
        Khi2    = unname(test_ij$statistic),
        ddl     = unname(test_ij$parameter),
        p_value = unname(test_ij$p.value)
      )
    )
  }
}

res_chi2

## Relation entre la varible Type de pret et toutes les variables quantitatives

#| fig-width: 10
#| fig-height: 10
DATA |>
  select(
    Type_Pret,
    Age,
    Revenu_Mensuel,
    Depenses_Mensuelles,
    Epargne,
    Montant_Pret,
    Duree_Pret_Mois,
    Mensualite_Pret,
    Taux_Interet_Pret,
    Score_Credit,
    Ratio_Dette_Revenu,
    Ratio_Epargne_Revenu
  ) |>
  pivot_longer(
    cols = -Type_Pret,
    names_to = "variable",
    values_to = "valeur"
  ) |>
  ggplot(aes(x = Type_Pret, y = valeur, fill = Type_Pret)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Boxplots des variables quantitatives selon le type de prêt",
    x = "Type de prêt",
    y = "Valeur"
  )

# Analyse Factorielle (ACP)
## Préparation des données :

### Test KMO

X <- DATA[, quanti_vars]

R <- cor(X, use = "pairwise.complete.obs")
KMO(R)

vars_quant_rest <- c("Revenu_Mensuel",
                     "Depenses_Mensuelles",
                     "Epargne",
                     "Mensualite_Pret",
                     "Montant_Pret","Ratio_Dette_Revenu","Score_Credit","Age")
X_ <- DATA[, vars_quant_rest]
C <- cor(X_, use = "pairwise.complete.obs")
KMO(C)

### Test de Bartlett

cortest.bartlett(C, n = nrow(X_))

## Réalisation de l'ACP

data_acp<-DATA |>
  select(
    Montant_Pret,Revenu_Mensuel,Depenses_Mensuelles,Epargne,
    Mensualite_Pret,Score_Credit,Ratio_Dette_Revenu)

Res_ACP <- PCA(data_acp,scale.unit = TRUE)

## Choix du nombre de dimensions :

### Éboulis des valeurs propres (scree plot): Critère de Coude

# Préparation des données
df_eig <- data.frame(
  Dim = factor(1:nrow(Res_ACP$eig)),
  Eigenvalue = Res_ACP$eig[,1]
)

# Graphique ggplot
ggplot(df_eig, aes(x = Dim, y = Eigenvalue)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 1, color = "red") +
  labs(title = "Éboulis des valeurs propres",
       x = "Dimension",
       y = "Valeur propre") 

### Règle de Kaiser: Pourcentage d’inertie expliquée

round(Res_ACP$eig,digit=2)

## Interprétation des axes factoriels :

Res_ACP$var

## Analyse des individus :


#| fig-width: 10
#| fig-height: 10
fviz_pca_ind(Res_ACP,
             habillage = DATA$Type_Pret,
             legend.title = "Type_Pret",
             labelsize = 3,
             title = "Individus par Type de pret")


#| fig-width: 10
#| fig-height: 10
plot(Res_ACP,axes=c(1,2),choix="ind", habillage = "cos2")
plot(Res_ACP,axes=c(1,2),choix="ind", habillage = "contrib")
plot(Res_ACP,axes=c(1,2),choix="ind", habillage = "Score_Credit")
plot(Res_ACP,axes=c(1,2),choix="ind", habillage = "Revenu_Mensuel")

## Analyse des variables:

plot(Res_ACP,axes=c(1,2),choix="var", habillage = "cos2")
plot(Res_ACP,axes=c(1,2),choix="var", habillage = "contrib")


