#---------------------------------------------------------------#
## Lire les data                                               ##

data <- read.csv("datatest1.csv", 
               sep=";", 
               dec=",", 
               na.strings=c(".", "NA", "", "?","#DIV/0!"), 
               strip.white=TRUE, 
               encoding="UTF-8")

data$classe=as.factor(data$classe) # il est bien de transformer
                                   # les valeurs discretes 
                                   # non ordonnees
                                   # en factors


#---------------------------------------------------------------#
## informations sur des data                                   ##
str(data)
dim(data)     # nb ligne, nb colonnes ... ou :
nrow(data)    # nb ligne
ncol(data)    # nb de colonne 
names(data)   # nom des colonnes


typeof(data$v1)     # reel codes en double mots
                    # typeof : typeof codage interne, 
                    # pas leur nature R !
is.double(data$v1)  # confirmation !
is.numeric(data$v1) # vrai aussi c'est du numerique
class(data$v1)      # nature de la variable
str(data$v1)        # regardons ces 100 numeriques

typeof(data$classe)      # entier (codage interne)
is.integer(data$classe)  # infirmation ! (pas de nature entier)
is.numeric(data$classe)  # infirmation !
is.character(data$classe)# pas des char non plus !
is.factor(data$classe)   # oui ce sont des factors!
class(data$classe)       # nature de la variable
str(data$classe)         # regardons ces 100 factors

anyNA(data)              # verifier si les data ne comporte 
                         # pas de valeurs inconnues
table(is.na(data))       # table permet de compte les FALSE 
                         # et donc le nb de NA


#---------------------------------------------------------------#
## manipulations de bases                                      ##
#---------------------------------------------------------------#

df <- data.frame(data)   # creation data frame
#---------------------------------------------------------------#
## les lignes                                                  ##
df[1,]                   # premiere ligne
d <- df[1,]    
is.data.frame(d)         # un data frame d'une ligne

df[10:20,]               # lignes 10 a 20 inclues

#---------------------------------------------------------------#
## les colonnes                                                ##

df[,1]            # colonne 1
c <-  df[,1]   
is.vector(c)      # c'est un vector qui represente la colonne

c <- data.frame(c)# transformation du vecteur en data frame
is.data.frame(c)  # ça marche !

c <-  df[,1:5]
is.data.frame(c) # c'est de suite un data frame de 5 colonnes !


d <- df[4:6,3:5]
dim(d)
is.data.frame(d) # c'est un data frame de 3 lignes sur 3 col


df$v1             # colonne 1 via son nom :v1
c <- df["v1"]     # c'est de suite un data frame
is.data.frame(c)  # ça marche !

                  # pour obtenir 4 des colonnes
                  # fabrique un vector avec les noms 
v <-c("v1","v2","v3","classe")  
df_ <- df[,v]              # data frame des 4 cols extraites
names(df_)

#---------------------------------------------------------------#
summary(df_)               # statistiques de base sur df_

#---------------------------------------------------------------#
## filtrer les colonnes avec subset                            ##

df__ <- subset(df_, select = -2)       # enlever colonne 2
names(df__)

df__ <- subset(df_, select = -c(1,2))  # enlever colonnes 1 et2
names(df__)

df__ <- subset(df_, select = -v1)      # enlever colonne v1
names(df__)

df__ <- subset(df_, select = -c(v1))   # idem enlever colonne v1
names(df__)

df__ <- subset(df_, select = -c(v1,v2))# enlever col v1 et v2
names(df__)

df__ <- subset(df_, select = c(1,2))   # garder colonnes 1 et 2
names(df__)

df__ <- subset(df_, select = c(v1,v3)) # garder colonne v1 et v3
names(df__)

df__ <- subset(df_, select = v1:v3)    # garder colonne v1 a v3
names(df__)


#---------------------------------------------------------------#
## travaux sur une colonne de factors                          ##
levels(df_$classe)         # tableau des valeurs, bizarre !
str(levels(df_$classe))    # un tableau de chr (factors)

unique(df$classe)          # tableau des factors
str(unique(df$classe))     # on a obtenu un tableau de factors

prop.table(table(df_$classe)) # proportion par factor

#---------------------------------------------------------------#
## quelques calculs                                            ##

v <-c("v1","v2","v3")
df_ <- df[,v] 

# des fonctions tres rapides, ne pas utiliser si il y a des NA
# car l'implementation est differente suivant les systemes
colSums(df_)               # somme par colonne (3 valeurs)
colMeans(df_)              # moyenne par colonne (3 valeurs)
rowSums(df_)               # somme par ligne   (100 valeurs)
rowMeans(df_)              # moyenne par ligne (100 valeurs)


#---------------------------------------------------------------#
## aggregate ... group by                                      ##

v <-c("v1","v2","v3","classe")  
df_ <- df[,v]

if(require("stats")==FALSE) install.packages("stats")
require("stats")                            # stats de base
library(help = "stats")                     # de l'aide

a <- aggregate(.~ classe, data = df_, mean) # data frame
                                            # moyenne par classe
                                            # par colonne num
a
dim(a)

#---------------------------------------------------------------#
## combinaison de lignes                                       ##
a <- df_[1:5, ]      # extraction 5 premieres lignes
b <- df_[96:100, ]   # extraction 5 dernieres lignes
c <- rbind(a,b)      # combinaisons des 2 paquets de lignes 
c
nrow(c)
names(c)
c[99,]
c["99",]

row.names(c) <- NULL

#---------------------------------------------------------------#
## filtrer des lignes avec subset                              ##

c <- subset(df_, classe == "1")  # sur la valeur d'un factor
c
nrow(c)

c <- subset(df_, v1 <= 0)        # sur le contenu d'une var num
c
nrow(c)

c <- subset(df_, v1 <= 0 &
                 v2 <= 0 &
                 classe != 2)    # ou plus complexe ...
c
nrow(c)

                                 # comparaison avec pop totale
c <- subset(df_, v1 <= 0)
p_v1_neg      <- prop.table(table(c$classe))
p_ensemble    <- prop.table(table(df_$classe))
m <- as.table(rbind(p_v1_neg,p_ensemble))
m

                                # test du chi-2
c_neg      <- subset(df_, v1 <= 0)
c_pos      <- subset(df_, v1 > 0)
v1_negatif <- table(c_neg$classe)
v1_negatif
v1_positif <- table(c_pos$classe)
v1_positif

m <- as.table(rbind(v1_negatif,
                    v1_positif)
              )
m

(Xsq <- chisq.test(m))

Xsq$expected

#---------------------------------------------------------------#
## transformation  ou ajout de colonnes par calcul  via within ##
## flexible  et puissant                                       ##


c <- within(df_,                    # pour chaque ligne faire :
                {
                 v4 <- v1+v2+v3
         
                }
            )
c
names(c) 

#---------------------------------------------------------------#
##  demonstration de puissance de within()                     ##

                                    # pour l'exemple
f <- function(x,y,z){round(log(x+y+10)/(abs(z)+1))}

c <- within(df_,                    # pour chaque ligne faire :
                {
                vd <- abs(v2-v1)   
                vr <- round((v1+v2)/vd)# super on fait appel a vd
                                       # de  la meme  boucle
                var_globale <<- v1+v2+v3   # evite creer une col
                vp <- f(v1,v2,var_globale) # pas de closure ici!  
                vd <- round(vd,2)
                }
            )
c
names(c)                            # on a ajoute 2 colonnes

#---------------------------------------------------------------#
## transformation ou ajout de col par calcul  via transform    ##

c <- transform(df_, v1 = v1 + 10)         # transforme une col
c

c <- transform(df_, vs = v1 + log(v2+10), # 2 colonnes
                    vd = v1 + v2 +10) 
c
 
                                          # sapply anonyme
c <- transform(df_, vs = sapply(v1,function(x){x+100}))     
c


                                         # test et choix
f <- function(x){round(x) %% 3}          # pour l'exemple

c <- transform(df_, vs = ifelse(f(v1)>f(v3),v1-v2,v3-v2))    
c

#---------------------------------------------------------------#

                                           # compter fiable
                                           # fonction auxiliaire
test <- function(classe,v1,v2){(classe ==1)&(v1>v2)}

c <- transform(df_,t = ifelse(test(classe,v1,v2),1,0))
c
sum(c$t)                                   # comptage

#---------------------------------------------------------------#
                                    
a <- aggregate(.~ t, data =c, mean)
a[,-5]                              # drop col 5

                                    # par classe et vs
a <- aggregate(.~ t+classe, data =c, mean)
a <- a[,-5]                        
a

#---------------------------------------------------------------#
# tri data.frame                                                #

order(c("a","b","d","c"))
order(c("a","b","d","c"), decreasing = TRUE)
order(c("a","b",NA,"d",NA,"c"), na.last = FALSE)

                                    # tri suivant col
b <- a[order(a$t, a$classe),]
b

                                    # par nom de col
d <- b[,order(names(b),decreasing = TRUE)]
d

d <- d[,c(2,1,3,4)]                 # manuellement
d


#---------------------------------------------------------------#
## travaux de base sur une variables numerique                 ##

#---------------------------------------------------------------#
## aspect de l'echantillon                                     ##

n_col <- ncol(df)-1              # la derniere colonne n'est pas
                                 # numerique, c'est la classe

op <- par(mfrow = c(2,4))        # chaque graphique 
                                 # contient  2 lignes
                                 # de        3 graphique
for (i in 1:n_col){
  df_i <- df[,i]
  nom <- names(df)[i]
  plot(density(df_i),main = toupper(nom))
  hist(df_i, main = "Fréquence par valeur",
             xlab = paste("valeurs de",nom))
  cdplot(df_i,df$classe,main = "Densité conditionnelle",
                        xlab = paste("valeurs de",nom),
                        ylab = "valeurs de classe"
         ) 
  plot(df$classe,df_i, xlab = "valeurs de classe",
                       ylab = paste("valeurs de",nom), 
                       main ="Répartition")
  } 
rm(df_i)   # nettoyage data.frame temporaire
par(op)                          # IMPORTANT restore les
                                 # parametres de presentation

#---------------------------------------------------------------#
## aspect de l'echantillon scatterplot v X v                   ##

n_col <- ncol(df)-1
op <- par(mfrow = c(2,2))      

classe_ <- df$classe
for (i in 1:n_col){
  for (j in 1:n_col) {
    if (i<j ){   
      df_i <- df[,i]
      df_j <- df[,j]
      df_ijc <- data.frame(df_i,df_j,classe_)  # data.frame temporaire
      names(df_ijc)<-c("axe_1","axe_2","classe")
      with(df_ijc,                        # pour ce data.frame faire                          
         plot(axe_1,
              axe_2,            
              col=classe,               # couleur depend de classe
              pch=as.numeric(classe),   # symbole depend de classe
              xlab = names(df)[i],      # les vrais noms des axes
              ylab = names(df)[j] 
              )) 
      }
    }
  }
rm(df_ijc) # nettoyage data.frame temporaire
rm(df_i)   # nettoyage data.frame temporaire
rm(df_j)   # nettoyage data.frame temporaire
rm(classe_)# nettoyage data.frame temporaire
par(op)                         

## SI VOUS OBTENEZ L'ERREUR :
## Error in plot.new() : figure margins too large
## il vous faut augmenter grandement avec votre souris
## la taille de la fenetre de visualisation des plots !!
## ou utilisez l'astuce suivante avant le code :
par("mar")            # verifie option
par(mar=c(1,1,1,1))   # adapte l'option

#---------------------------------------------------------------#
## creation legende naive - visualisation des symboles et coul ##

lib <- c("classe 0",
         "classe 1",
         "classe 2",
         "classe 3")
classe <- as.factor(0:3)
v      <- c(0,0,0,0)
w      <- 0:3
plot(v,
     w, 
     main = "Signification des symboles",
     xlab  ="",  # pas de libelle axe x
     ylab  ="",  # pas de libelle axe y
     xaxp = c(0, 1,1), # axe x de 0 à 1 tick 1
     xaxt = "n", # pas d'axe x
     yaxt = "n", # pas d'axe y
     bty = "n",  # pas de cadre
     col=classe,               # couleur depend de classe
     pch=as.numeric(classe))   # symbole depend de classe
# ajouts lib (avec position et taille)
text(v-0.43,w+0.04,lib,cex = 0.8)  

#---------------------------------------------------------------#
## correlations entre variables numeriques                     ##

if(require("corrplot")==FALSE) install.packages("corrplot")
require(corrplot)

num_all <- c(names(df)[1:12]) # les 12 premieres variables


cor <-cor(df[num_all])
corrplot(cor, 
         title = "Correlation entre var numeriques",
         order = "AOE",
         method = "ellipse", 
         type = "lower", 
         tl.cex = 0.6 ) 


#---------------------------------------------------------------#
## une variable par rapport a 2 autres                         ##

## plot les classes en fonction de v1 et v2                    ##

with(df,                           
     plot(v1, v2,                   # axes v1 et v2
          col=classe,               # couleur depend de classe
          pch=as.numeric(classe)))  # forme depend de classe

## separation des classes en fonction de v5 et v6
if(require("ggplot2")==FALSE) install.packages("ggplot2")
require("ggplot2")            # librairie graphique moderne
qplot(v5, v6, data=df, facets= classe ~.) 

## plot en 3D, manipulable a la souris                         ##
if(require("rgl")==FALSE) install.packages("rgl")
require("rgl")            # pour la 3D
open3d()                  # ouvre le canvas
plot3d(df$v3,df$v5,df$classe) # points (V3,v5,classe) en 3D
plot3d(df$v3,df$v5,df$v7)     # points (V1,v3,v7) en 3D

## graphiques par paires
v <-c("v3","v5","v6","v7","v11","v12","classe")                ##
df_ <- df[,v]
require(fBasics, quietly=TRUE) # pour appeler basicStats
                               # Statistiques utiles
lapply( subset(df_, select = v3:v7), basicStats) 
# plot avec une couleur par classe d'origine 
plot(df_, col = df_$classe, 
     pch = as.numeric(df_$classe))



## BONUS regression lineaire  v7 en fonction de v3 et v5       ##
model <- lm(v7 ~ v3+v5, data = df) # regression v1=a.v3+b.v7+c
summary(model)            # resultats
m<-model$coefficients     # stocker les coefficients de reg
m
plot(model)               # analyse visuelle de la regression

#---------------------------------------------------------------#

v <-c("v3","v5","v6","v7","v11","v12","classe")  
df_ <- df[,v]

require(fBasics, quietly=TRUE) # pour appeler basicStats
                                            # Statistique utiles
lapply( subset(df_, select = v3:v7), basicStats) 

                    # plot avec une couleur par classe d'origine 
plot(df_, col = df_$classe, 
          pch = as.numeric(df_$classe))


#---------------------------------------------------------------#
## boites a moustaches                                         ##

op <- par(mfrow=c(1,4))  # pour mette les 4 boites sur une meme page
boxplot(df$v5 , main="v5" , collapse=" ") # boites a moustaches
boxplot(df$v5 , main="v6" , collapse=" ") # 
boxplot(df$v11, main="v11", collapse=" ") # 
boxplot(df$v12, main="v12", collapse=" ") # 

length(boxplot.stats(df$v5 )$out) # nb de points etranges
length(boxplot.stats(df$v6 )$out)
length(boxplot.stats(df$v11)$out)
length(boxplot.stats(df$v12)$out)
par(op)

#---------------------------------------------------------------#
## elimination des points etranges                             ##

library(dplyr)      # librairie de manipulation de data
m <- min(boxplot.stats(df$v12)$out) 
df_bis <- filter(df, v12 <=m)       # filter de dplyr

op <- par(mfrow=c(1,2))  # pour mette les 2 boites sur une meme page
boxplot(df$v12,     main="v12 origine", collapse=" ") #
boxplot(df_bis$v12, main="v12 sans point etrange", collapse=" ") 
par(op)


#---------------------------------------------------------------#
# nouveau diagr par pair sans points étranges                   #
v <-c("v3","v5","v6","v7","v11","v12","classe")  
df_ <- df_bis[,v]

require(fBasics, quietly=TRUE) # pour appeler basicStats
# Statistique utiles
lapply( subset(df_, select = v3:v7), basicStats) 

# plot avec une couleur par classe d'origine 
plot(df_, col = df_$classe, 
     pch = as.numeric(df_$classe))

#---------------------------------------------------------------#
## code intermediaire avant reprise de la suite du chapitre    ##
#---------------------------------------------------------------#
set.seed(1)           # initialisation generateur de nb aleatoire
library(caret)        # nous apporte un partitioning elegant
p          <- createDataPartition(y=df_$classe,
                                  p= 60/100,
                                  list=FALSE)
training   <- df_[p,]  
inconnu    <- df_[-p,]
q          <- createDataPartition(y=inconnu$classe,
                                  p= 50/100,
                                  list=FALSE)
test       <- inconnu[q,]
validation <- inconnu[-q,]


# types de variables et liste

categorie <- NULL
risk      <- NULL
id        <- NULL
ignore    <- NULL
weights   <- NULL
# 

#---------------------------------------------------------------#
# choix des variables du data set 
X      <- c("v3", "v5", "v6", "v7")

y     <- "classe"                   # notre cible


#---------------------------------------------------------------#
##  Analyse de base des data                                   ##
require(Hmisc, quietly=TRUE) # chargement des fonctions utiles

contents(training[c(X, y)]) # types de variable
summary (training[c(X, y)]) # statistiques de base
str(training[c(X, y)])      # quelques valeurs
head(training[c(X, y)])     # premieres lignes
tail(training[c(X, y)])     # dernieres lignes
describe(training[c(X, y)]) # d'autres stats

#---------------------------------------------------------------#
## suite des exemples du chapitre                              ##
#---------------------------------------------------------------#
## cette partie est plus detaillee que l'ouvrage               ##
## on y trouve deux types d'arbres de decision                 ##
#---------------------------------------------------------------#
##  analyse via arbres de decision                             ##
library(rpart)  # arbre de decision
library(rattle) # pour utiliser fancyRpartPlot
library(RColorBrewer)  # gestionnaire de palette de couleur
library(caret)         # nous apporte un partitioning elegant
p          <- createDataPartition(y=df_$classe,
                                  p= 60/100,
                                  list=FALSE)
training   <- df_[p,]  
test       <- df_[-p,]

# choix des variables du data set 
X      <- c("v3", "v5", "v6", "v7")
y     <- "classe"                   # notre cible

## constitution arbre de decision                              ##
d <- training[c(X, y)] # le data frame d'entrainement

## un essai avec les parametres par defaut de rpart            ##
set.seed(1)           # initialisation generateur de nb aleatoire
model1 <- rpart(classe  ~ .  , 
               data = d)  # fitting
fancyRpartPlot(model1)     # dessine l'arbre de decision
                           # tres simple 

predictions <- predict(model1,newdata = test)

## un essai avec des param pour avoir un arbre plus verbeux    ##
model2 <- rpart(classe  ~ .  , 
               data = d,
               control=rpart.control(minsplit=1,cp=0)
               ) # fitting
fancyRpartPlot(model2)  # dessine l'arbre de decision

#---------------------------------------------------------------#
## on peut faire varier cp, et trouver un optimum, que l'on    ##
## utilisera pour elaguer l'arbre avec la fonction "prune"     ##
## model_optimal <- prune(model_max,cp=cp_optimal)             ##

library(rpart)         # arbre de decision
library(caret)         # nous apporte un partitioning elegant

                       # creation partition training/test
p          <- createDataPartition(y=df_$classe,
                                  p= 60/100,
                                  list=FALSE)
training   <- df_[p,]  
test       <- df_[-p,]

# choix des variables du data set 
X     <- c("v3", "v5", "v6", "v7")
y     <- "classe"                   # notre cible

## constitution arbre de decision                              ##
d <- training[c(X, y)] # le data frame d'entrainement
model <-  rpart(classe  ~ .  , 
                data = d,
                control=rpart.control(minsplit=1,cp=0)
                ) # fitting

library(rpart.plot)       # pour tracer l'arbre
prp(model,extra=1)        # tracer l'arbre

                          #  predire sur jeu de test
predictions <-predict(model, test, type="class")

print(confusionMatrix(predictions,test$classe)) # verifier 
                                                # via matrice
                                                # de confusion


#---------------------------------------------------------------#
