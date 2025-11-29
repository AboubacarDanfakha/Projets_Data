#---------------------------------------------------

# Chargement des données sur un site internet
 data = read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"),
                     header = FALSE)
# Pour importer  un jeu de données csv sur un site internet il suffit d'ecrire
 # le read.csv(url()) et copier le lien du fichier et  le mettre dans l'url
 # On met header = FALSE pour dire que la premiere ligne c'est le nom des colonne

 # Modification du noms des colonne
colnames(data)<-c("age", "sex", "cp","tresrbps", "chol","fbs", "restecg",
                  "thalach", "exang", "oldpeak","sloppe", "ca", "thal","target")

# Tranformation de la collone target en ne gardant que des 0 et 1
data$target[data$target==2] = 1
data$target[data$target==3] = 1
data$target[data$target==4] = 1

# Comment supprimer les ligne et colonne
data = data[-1,] # suppression de la premeire ligne
#data = data[,-1]# suppression de la premeire colonne

# Dans le code suivant nous cherchons a voir quels sont les lignes qui ont 
# de valeurs manquantes (?) dans la colonnes  ca et thal
valeurs_manquante_ca <- which(data$ca %in% '?')
valeurs_manquante_thal <- which(data$thal %in% '?') 
valeur_manqantes <- c(valeurs_manquante_ca, valeurs_manquante_thal) 
data <- data[-valeur_manqantes,]

# Modification des types des variables
# la fonction str permet de coannaitre le type de variable
str(data)
### Transformation des variables qualitatives en facteur,car
### elles sont reconnues comme des int avec R dans notre cas

data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$sloppe <- as.factor(data$sloppe)
data$ca <- as.factor(data$ca)
data$thal <- as.factor(data$thal)
data$target <- as.factor(data$target)

## Transformation des variables qui ne possedent pas de virgule en entier
## C'est-à-dire les variables quantitatives
## quant une variable est de type num, cela veut dire que c'est un float
data$age <- as.integer(data$age)
data$tresrbps <- as.integer(data$tresrbps)
data$chol <- as.integer(data$chol)
data$thalach <- as.integer(data$thalach)
# Recodage des des modalités des variables qualitatives

levels(data$sex) # cela nous dit que nous avons deux modalités 0 et 1
levels(data$sex) <- c("Femme", "Homme")
levels(data$cp) <- c("engine stable", "engine instable","Autre douleur", "Asymptomatique")
levels(data$fbs) <- c("Non", "Oui")
levels(data$restecg) <- c("Normal", "Anomalie", "Hypertrophie")
levels(data$exang) <- c("Non", "Oui")
levels(data$sloppe) <- c("En hausse", "Stable", "En baisse")
levels(data$ca)<- c("Absence d'anomalie", "Faible", "Moyen", "Elevé")
levels(data$thal) <- c("Non", "Thalasemie sous control", "Thalasemie instable")
levels(data$target)<- c("Non", "Oui")

# Derniere verification des types de nos variables
str(data)
# Verification s'il y a des valeurs manquantes
apply(data, 2, anyNA) # Ce code permet d'utiliser la fonction apply
# sur notre jeu de données data et le 2 indique que la verification
# se fait sur les colonne, si on avait fait 1 ça sera sur les lignes
#---------------------------------------------------

# Calcul des indicateurs clés (Variables qualitatives)

## Variable sex

table(data$sex) # c'est comme la fonction values_count de Python
prop.table(table(data$sex))# cela permet de nous donner  les frequences
round(prop.table(table(data$sex)), 4)*100 # arrondire 4 chiffre après la virgule

# Variable  cp
 table(data$cp)
 prop.table(table(data$cp))*100 
 round(prop.table(table(data$cp))*100,4)
 
 # Variable  fbs
 table(data$fbs)
 prop.table(table(data$fbs))*100 
 round(prop.table(table(data$fbs))*100,4)
 
 # Variable  fbs
 table(data$fbs)
 prop.table(table(data$fbs))*100 
 round(prop.table(table(data$fbs))*100,4)
 # on doit faire de meme pour tous les autres variables qualitatives
 
 # Calcul des indicateurs clés (Variables quantitatives)
 ## Moyenne, mediane, quartile, minimum, maximum
 summary(data$age)
 summary(data$tresrbps)
 summary(data$chol)
 summary(data$oldpeak)
 summary(data$thalach)
 
 ## Variance et ecart-type
 var(data$age)
 sd(data$age) 
### On voit que nos ecart-type est de 9.04 qui est  vraiment éloigné de 0
### cela permet de dire que nos données sont éloignées de la moyenne, autrement dit les
### les individus ont des ages vraiment inferieur et superieur de la maoyenne, car un ecart-type proche de 0
### permet de dire que le données sont proche de la moyenne
 
#---------------------------------------------------- 
 
 # Diagramme en barre variable qualitatives
 
graph1 <- plot(data$sex, xlab = "Sex",
      ylab = "Effectif",
      main = "Repartion des patients par sex",
      col = c("blue", "red"),
      sub = "Donnees prevenant de UCI machine Learning",
      las = 1, # Pour ecrire les  valeurs des effectif en orizontale
      #horiz = TRUE,
      #border = "blue",pour changer la couleur de la bordure en bleu
      #yaxt = 'n'pour supprimé la colonne y du graphe
      cex.main =1.8,# pour agrandir le titre
      #cex.axe = 1,
      cex.lab = 1.2,# Pour gerer la taille des labels (sex et effectif)
      ylim = c(0, 200) # pour definir la valeur min et la limite
      
      ) 
 
text(x = graph1, y = table(data$sex) +10, labels = as.character(table(data$sex)), cex = 1.1, font = 3)
 
 # le +10 permet de de mettre un peu plus haut le 96 et 201
 # font = 3 permet de mettre ces valeurs en italique
 
# Deuxieme diagramme en barre pour la variable 'cp'
 
 graph2 <- plot(data$cp,
                xlab = "Douleur thoracique",
                ylab = "Effectif",
                main = "Repartion des patients selon la douleur thoraciques",
                sub = "Jeu de données provenant de UCI Machine Learning",
                col = c("blue","red", "green", "pink"),
                ylim = c(0,160),
                #cex.axis = 1
                space = 0.4 # Tres important pour que toutes les modalités soit lisible en abcisse
                )
 
text(x = graph2, y = table(data$cp) + 10, labels = as.character(table(data$cp)), cex = 1.1, font = 3)

# Diagramme circulaire ou camenbert
# Mais je pense que les diagramme en barre sont beaucoups lisibles que les camenbert
# Dans le cas d'une variable qualitative on peut utiliser l'un ou l'autre
pie(table(data$target), 
    main = "Repartition des patients selon l'apparition de la maladie cardio vasculaire",
    col = c("blue", "red"),
    clockwise = TRUE # permet que la barre commence par 12h
    )

# Boite a moustache (Variables quantitatives)
boxplot(data$age,
        ylab = "Age",
        main = "Repartition des patients selon l'age",
        col = 'red',
        notch = TRUE, # permet de mettre en valeur la médiane
        las = 1,
        ylim = c(20,80)
        )
### Nous remarquons que le médiane est environs 56 selon le graphique
### Le premier trait que nous voyons en bas c'est l'age minimal 
### le premier trait du rectangle est la premier quartile, le trai en
### noir au milieu correspond a la médiane, le dernier trait en haut du rectangle
### correspond au 3 eme quartile et enfin le dernier trai apres les
### les pointillés en haut correspond à l'age maximal des patients

# Histogramme (variable quantitatives)
graph3 <- hist(data$tresrbps, 
     xlab = 'Tension arterielle au repos',
     ylab = "Effectif",
     main = 'Repartion des patients selon la tension arterielle',
     xlim = c(80,200),
     ylim = c(0,80),
     col = "blue"
     )
text(x= graph3$mids,y = graph3$counts, 
     labels= graph3$counts,
     adj = c(0.5, -0.5)
     )
###  ce code adj = c(0.5, -0.5) peret de centrer les valeurs sur les bars
### $mids permet de dire où centrer les données, c'est-à-dire au milieu des bars
### Pour analyser on peut dire que 6 patient ont une tension arterielle entre
### 90 et 100

#---------------------------------------------------

# Graphiques croisés (Analyse bivarié)

## Diagramme à barres croisés (deux variable qualitatives)

### Dans ce cas on utilise la fonction barplot() au lieu de plot()

Graph4 <- barplot(table(data$target, data$sex),
        beside = TRUE,
        xlab = "Sexe",
        ylab = "Patients",
        main = "Répartition des patients selon la présence d'une maladie \n et le sexe",
        col = c("blue", "black"),
        las = 1,
        
        ylim = c(0, 150)
)

legend("top", # Pour mettre la legende a haut centré
       legend = levels(data$target),
       fill = c("blue", "black"), 
       #title = "Maladie cardiovasculaire", 
       horiz = TRUE, #pour mettre les legendes de façon horizontale et non vertcal
       
       cex = 0.8 # Pour gérer la taille des ecritures dans la legende
       )


text(x = Graph4, y = table(data$target, data$sex) +10, 
     labels = as.character(table(data$target, data$sex)), font = 3 )

### Juste en regardant ces graphique on peut dire que le sex a un effet
### significatif la variable target

## Boite a moustache croisé (Variables quantitatives)
boxplot(data$age ~ data$target,
        main = "Repartition des patients selon l'age et la présence \n de la maladie ou non",
       xlab = "Target",
       ylab = "Age",
       col = "red",
       notch = TRUE,
       ylim = c(20,80))
### Pour l'analyse de ces boxplot nous constatons que pour les patients
### qui ne sont pas malade l'age médian est 52 environs et pour les patients qui sont malades
### l'age median est de 58 environs, juste en se basant sur ça on sait que 
### l'age a un impact significatif sur la présence ou non de la maladie car 
### les deux mediane ne sont pas les memes
### A travers la boite a moustache aussi on voit que le patient le plus
### jeune n'a pas la maladie et d'ailleur meme le plus agé n'a pas la maladie

#-----------------------------------------------------





# Tests Statistiques

## Calcul des pourcentage

round(prop.table(table(data$sex,data$target), margin = 1),4)*100
### La fonction margin=1 permet de donner les pourcentages de femmes
### en fonction dans le cas malade ou non e fonction du nombre total de femme dans l'etude
### On voit que les Hommes ont 55.72% d'avoir la maladie cardiavasculaire par rapport
### aux femmes qui n'ont que 26.04%
round(prop.table(table(data$cp,data$target), margin = 1),4)*100
round(prop.table(table(data$fbs,data$target), margin = 1),4)*100
round(prop.table(table(data$restecg,data$target), margin = 1),4)*100
round(prop.table(table(data$exang,data$target), margin = 1),4)*100

## Test de khi-2 (Variable qualitatives)

### H_0 : Les deux variables sont indépendantes
### H_1 : Les deux variables sont dépendantes
### C'est un test qui se fait uniquement pour deux variables qualitatives

chisq.test(data$sex, data$target)

### Les résultats donne une p-value de 2.946e-06 donc les deux variables sont dependantes
### autrement dit le sex a une influence sur la presence de la maladie

## Calcul de la moyenne par modalité

tapply(data$age, data$target, mean) #tapply permet de faire des operations

## Test de shapiro wilk
library(dplyr) # pour pouvoir utiliser la fonction filter

shapiro.test(filter(data, target =='Oui')$age)
### On voit une p-value de 0.00167 donc les données ne suivent pas la loi normale
shapiro.test(filter(data, target =='Oui')$thalach)
### Dans ce cas on a une pvalue de 0.40 donc les données suivent la distribution normale
 
## Test de Mann -Whitney (Test non parametrique en cas de non normalité)
### H_0 : Il n'y a pas de difference significative entre  la moyenne des variables
### H_1 : Il y a  une difference significative entre  la moyenne des variables

wilcox.test(data$age ~ data$target) # C'est vraiment le test de Mann -Whitney a ne pas
                                   # confondre avec le test de wilcoxon

## Test de student (Quand les données suivent la loi normale)

### H_0 : Il n'y a pas de difference significative entre  la moyenne des variables
### H_1 : Il y a  une difference significative entre  la moyenne des variables

t.test(data$thalach ~ data$target)

#------------------------------------------------------

# Regresion logistique (Machine Learning)

## Division du jeu de données en train et test
set.seed(99) # Pour fixer les lignes qui ont été prise
library(caTools) # Ce qui permet 
split = sample.split(data$target, SplitRatio = 0.8)
Train = subset(data, split == TRUE)
Test = subset(data, split == FALSE)

## Creation de notre modèle regression logistique

Regression_logistique = glm(target ~., data = Train, family = "binomial")
summary(Regression_logistique)
### target ~. cela veut dire qu'on prends toutes les covariables

## Optimisation du modèle de regression logistique (Nous allons enlévé toutes les variables avec des pvalues superieur a 5% de plus grand au plus petit)
### Il est trés important de supprimé  les variables non significatif avec des pvalue plus grandes
### au pvalues plus petite car les variables restante a chaque fois apres suppression  peuvent changer de  pvalue
Regression_logistique = update(Regression_logistique, .~.-restecg)
summary(Regression_logistique)
Regression_logistique = update(Regression_logistique, .~.-sloppe)
Regression_logistique = update(Regression_logistique, .~.-age)
Regression_logistique = update(Regression_logistique, .~.-fbs)
Regression_logistique = update(Regression_logistique, .~.-chol)
Regression_logistique = update(Regression_logistique, .~.-thalach)
# Regression_logistique = update(Regression_logistique, .~.-cp)
Regression_logistique = update(Regression_logistique, .~.-tresrbps)
summary(Regression_logistique)
### Nous constatons que la variable cp a une modalité qui est significatif est une des modalités non significatif
### Maintenant pour savoir si on va garder la variable cp ou pas
### On regarde le model l'AIC le plus petit (modèle efficace)

### Critere AIC sans la variable cp : 202,2
### Critere AIC avec la variable cp : 177,71
### Donc on prendre le modèle avec la variable cp

## Prediction
prediction = predict(Regression_logistique, Test, type = "response")
prediction
Tableau_prediction = as.data.frame(prediction)
fonction  <- function(x){
      
      return(ifelse(x>0.5, 1,0))
}

Tableau_prediction <- apply(Tableau_prediction, 2, fonction)
### le 2 signifie qu'on le fait sur la colonne

## Mesure de performance du modèle

### La matrice de confusion ne travail que avec des valeur numerique donc nous allons convertir reconvertir target en 0 et 1
levels(data$target) <- c(0,1)

library(caret)
#confusionMatrix(as.factor(Test$target), as.factor(Tableau_prediction))

## Tabeau de comparaison
### On va utiliser la fonction cbind pour fusionner deux tableaux

Tableau_comparaison <- cbind(Test, Tableau_prediction)

### Maintenant on va remettre nos 'oui' et 'non' dans notre tableau
str(Tableau_comparaison)
levels(Tableau_comparaison$target) <- c('Non', 'Oui')
Tableau_comparaison$prediction<- as.factor(Tableau_comparaison$prediction) 
levels(Tableau_comparaison$prediction)<- c('Non', 'Oui')

## Test de Hosmer et Lemeshow (test la performance du modele)
### C'est un test qui permet de nous dire si notre s'ajuste bien aux données
library(performance)
### H_0: L'ajustement du modèle aux données est bon
### H_1: L'ajustement du modèle aux données n'est pas bon
performance_hosmer(Regression_logistique)

### On voit qu'on a une pvalue de 0.537 donc on accepte H_0


## Creation de la Courbe ROC

### C'est un bon moyen visuel pour voir la performance du modèle
library(pROC)
 par(pty = "s") # permet de commencer la courbe et la droite au vrai diagonal(0,0)
roc(Train$target, Regression_logistique$fitted.values,
    plot = TRUE,
    main = "Courbe ROC du modèle de regression logistique",
    lwd = 4, #Pour agrandir la courbe
    xlab = "Taux de faux positifs",
    ylab = "Taux de vrais positif",
    col = "blue",
    cex.main = 1
    )

### On voit l'aire sous la courbe est "Area under the curve: 0.9348" ce qui indique notre modèle est bon
### c'est plus proche de 1 c'est ce qui u bon score


# Information
## Pour telecharger le fichier sous forma Word, HTML, ou PDF,
## On regarde tout en haut a gauche, on met le souris sur la partie
### file et clique sur compile Report et on aura toute les options