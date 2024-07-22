w#   OBJECTIF 1 : CONNAITRE LES DETERMINANTS DES PERFORMANCES SPORTIVES DES EQUIPES DE BASKETBALL

#IMPORTATION DE LA BASE DE DONNEE NUMERO 1

library(readxl)
stat_per_game <- read_excel("C:/Users/Admin/Desktop/mémoire/Travail finalisé/stat_per_game.xls")
View(stat_per_game)

#STATISTIQUES DESCRIPTIVES

attach(stat_per_game)

table(Performances_équipe)
summary(Performances_équipe)
Catégorie_perfomance <- c('Nombre_équipe_plus_performantes','Nombre_équipes_moins_performantes')
Nombre_équipe_performantes <-sum(Performances_équipe >= 0.5)
Nombre_équipes_non_performantes <-sum(Performances_équipe < 0.5)
effectif_performance <- c(16,14)
Pourcentage_performance <- round(effectif_performance/sum(effectif_performance)*100)
Catégorie_performance_2 <- paste(Catégorie_perfomance,"\n",Pourcentage_performance,"%",sep=" ")
library(plotrix)
pie3D (effectif_performance , labels = Catégorie_performance_2 , main = 'Répartition des équipes par niveau de performance en %',
      explode = 0.3 , radius = 1.2 , theta = 1.2 , border = "Brown" , labelcex = 1 , labelcol = "blue",
      col = heat.colors(length(Catégorie_perfomance)))

table(Age)
summary(Age)
Catégorie_Age <- c('équipes_plus_jeunes', 'équipes_moins_jeunes')
équipes_jeunes <- sum(Age < 26)
équipes_vieilles <- sum(Age >= 26)
effectifs_age <- c(15,15)
Pourcentage_age <- round(effectifs_age/sum(effectifs_age)*100)
Catégorie_age_2 <- paste(Catégorie_Age,"\n",Pourcentage_age,"%",sep=" ")
library(plotrix)
pie3D (effectifs_age , labels = Catégorie_age_2 , main = 'Répartition des équipes par âge moyen en %',
       explode = 0.3 , radius = 1.2 , theta = 1.2 , border = "Brown" , labelcex = 1 , labelcol = "blue",
       col = heat.colors(length(Catégorie_Age)))

table(Performance_à_domicile)
summary(Performance_à_domicile)
Catégorie_pad <- c('Performante_à_domicile', 'le_domicile_importe_peu')
Performante_à_domicile <- sum(Performance_à_domicile >= 0.5427)
le_domicile_importe_peu <- sum(Performance_à_domicile < 0.5427)
effectifs_pad <- c(16,14)
Pourcentage_pad <- round(effectifs_pad/sum(effectifs_pad)*100)
Catégorie_pad_2 <- paste(Catégorie_pad,"\n",Pourcentage_pad,"%",sep=" ")
library(plotrix)
pie3D (effectifs_pad , labels = Catégorie_pad_2 , main = 'Performante à domicile',
       explode = 0.3 , radius = 1.2 , theta = 1.2 , border = "Brown" , labelcex = 1 , labelcol = "blue",
       col = heat.colors(length(Catégorie_pad)))

table( `Performance du coach`)
summary( `Performance du coach`)
Catégorie_pts <- c('Coach_plus_performants', 'Coach_moins_performants')
Coach_plus_performants <- sum(`Performance du coach` >= 0.5219)
Coach_moins_performants <- sum(`Performance du coach`< 0.5219)
effectifs_pts <- c(18,12)
Pourcentage_pts <- round(effectifs_pts/sum(effectifs_pts)*100)
Catégorie_pts_2 <- paste(Catégorie_pts,"\n",Pourcentage_pts,"%",sep=" ")
library(plotrix)
pie3D (effectifs_pts , labels = Catégorie_pts_2 , main = 'Performance du coach',
       explode = 0.3 , radius = 1.2 , theta = 1.2 , border = "Brown" , labelcex = 1 , labelcol = "blue",
       col = heat.colors(length(Catégorie_pts)))


Matrice_cor <- data.frame(Performances_équipe , `Performance à domicile`, `Points Moyens du franchise Player` ,
                          Age, `Performance du coach`, `FG%` , `Salaire en dollars`, FG
                          ,DRB ,FGA,`3P%`, `3P`, `3P`, FG, `2P` , `2P%`,  `FT%` , ORB , TRB , STL ,AST , TOV , PF , PTS)
cor(Matrice_cor)
library(Hmisc)
rcorr(as.matrix(Matrice_cor))
Corrélation <- cor (stat_per_game$Performances_équipe , Matrice_cor)
summary(Corrélation)

#PARTIE : DETERMINANTS DES PERFORMANCES D'UNE EQUIPE DE BASKETBALL-------------------------------------------

#MODEL DE REGRESSION LINEAIRE MULTIPLE

Model_pg <- lm (Performances_équipe ~ ., data=stat_per_game)
summary(Model_pg)
step(Model_pg, direction = "both", k=2)


Model_pg1 <- lm(formula = Performances_équipe ~  
                   Age + `Performance du coach` + `FG%` + `3P%` +`FT%` 
                  +DRB+ STL , data = stat_per_game)
step(Model_pg1 , direction = "both", k=2)

#COLINEARITE ENTRE LES VARIABLES ET CORRECTION DU MODELE

attach(stat_per_game)
Data_model_pg <- data.frame( Performances_équipe,
                             Age, `Performance du coach`, `FG%`
                             ,`3P%` , `FT%` , ORB  , STL, DRB)
cor(Data_model_pg)

#NOUVEAU MODELE

Model_pg1 <- lm(formula = Performances_équipe ~ 
                  `Performance à domicile` +
                   `Performance du coach` + `FG%`
                + `3P%` + `FT%` + ORB  + STL + TOV + PF + PTS , data = stat_per_game)
Model_pg1$aic
summary(Model_pg1)
broom::glance(Model_pg1)
par(mfrow = c(2:2))
plot(Model_pg1)
return(Model_pg1)
#spreadLevelPlot(Model_pg1 , smooth=FALSE)

#FACTEUR DE L'INFLATION DE LA VARIANCE POUR MULTICOLINEARITE 

library(carData)
library(car)
vif(Model_pg1)

#SIGNIFICATIVITE GLOBALE DU MODELE ET HETEROSCEDASTICITE

library(gvlma)
gvlma_mod_pg <- gvlma(Model_pg1)
summary(gvlma_mod_pg)

#NORMALITE DES RESIDUS

res_pg1 <- Model_pg1$residuals
par(mfrow = c(2:2))
plot(res_pg1)
shapiro.test(res_pg1) 

#AUTOCORRELATION DES ERREURS 

library(zoo)
library(lmtest)
dwtest(Model_pg1)

#HETEROSCEDASTICITE 

library(carData)
library(car)
ncvTest(Model_pg1)

#CONCLUSION : LE MODELE EST BON , LES FACTEURS DU MODELES INFLUENCENT LES PERFORMANCES SPORTIVES DES 
#EQUIPES DE BASKETBALL



#PARTIE 2 : PREVISION DE L'ISSUE DES MATCHS ------------------------------------------------------------------------

#DETERMINER LES FACTEURS QUI JUSTIFIENT L'ISSUE D'UN MATCH

library(readxl)
Echantillon_match_327 <- read_excel("C:/Users/Admin/Desktop/mémoire/Travail finalisé/Echantillon_match_327.xls")
View(Echantillon_match_327)

#REGRESSION LINEAIRE MULTIPLE ET VALIDATION

forecast_logit <- glm(formula =  ISSUE ~ ., data = Echantillon_match_327 , family = binomial (logit))
summary(forecast_logit)
step(forecast_logit , direction = "forward" , K=2)
step(forecast_logit , direction = "backward" , K=2)
step(forecast_logit_1 , direction = "both" , K=2)
forecast_logit_1 <- glm(formula =  ISSUE ~ `DFGA` + `D3P%` + DFTM + DDREB + DBLK
                        , data = Echantillon_match_327 , family = binomial (logit))
summary(forecast_logit_1)
library (questionr)
odds.ratio(forecast_logit_1)
step(forecast_logit_1 , direction = 'both' , K = 2)
attach(Echantillon_match_327)

forecasting_dtf <- data.frame( DFGA , `DFG%` , D3PM , D3PA , `D3P%` , 
                                 DFTA , `DFT%` , DOREB , DDREB , DAST , DSTL ,
                                 DBLK , DPF)

cor(forecasting_dtf)


#LOGISTIC REGRESSION VS PROBIT REGRESSION

forecast_logit <- glm(formula =ISSUE ~ DFGA + `D3P%` + DFTM  + DBLK + DDREB + DSTL, data = Echantillon_match_327 , family = binomial (logit))
forecast_logit_1$aic

forecast_probit <- glm(formula = ISSUE ~ `DFGA` + `D3P%` + DFTM + DDREB + DBLK, data = Echantillon_match_327 , family = binomial (probit))
forecast_probit$aic
summary (forecast_probit)
summary (forecast_logit_1)

#library(questionr) 
#odds.ratio(forecast_logit_1) 

attach(Echantillon_match_327)
library(pROC)
library(ROCR)
prediction <- predict(forecast_logit_1, date = predictions ,type="response")
roc_object <- roc(Echantillon_match_327$ISSUE , prediction) 
plot(roc_object)
auc(roc_object) #0.9738  0.9621

#PERTINENCE DU MODEL, TEST DE HOSMER-LEMESHOW

library(performance)
performance_hosmer (forecast_logit_1) #p_value=0.823 0.976

#REGLE DU POUCE

library(stats)
deviance(forecast_logit_1)/df.residual(forecast_logit_1) #0.4287509  0.5032021

#RESIDUS DE PEARSON 

s2=sum(residuals(forecast_logit_1,type="pearson")^2)
dd1=df.residual(forecast_logit_1)
pvaleur=1-pchisq(s2,dd1)
pvaleur #0.9200561 0.7167697

#RESIDUS DE DEVIANCE

pvaleur1=1-pchisq(deviance(forecast_logit_1),df.residual(forecast_logit_1))
pvaleur1 #1

#PREVISION - 1
library(dplyr)
test_ <- slice(Echantillon_match_327, 1:163)
train_ <- slice (Echantillon_match_327, 163:326)
#data_training_ <- read_excel("C:/Users/Admin/Desktop/mémoire/Travail finalisé/data_training_.xls")
#view(data_training_)
#mm3 <- read_excel("C:/Users/Admin/Desktop/mémoire/Travail finalisé/mm3.xls")
#View(mm3)

#load necessary packages

library(caret)
library(InformationValue)
library(ISLR)


#fit logistic regression model
attach(data_training_)
training <- na.omit(data_training_)
rm(Echantillon_match_327,train_, test_)

library(fpp3)

#use model to predict probability of default

predicted <- predict(forecast_logit_1, test_ , type="response")

#convert defaults from "Yes" and "No" to 1's and 0's
#mm3$ISSUE_3 <- ifelse(mm3$ISSUE_3=="Yes", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test_$ISSUE, predicted)[1]

#create confusion matrix
confusionMatrix(test_$ISSUE, predicted)


#calculate sensitivity
sensitivity(test_$ISSUE, predicted)

#calculate specificity
specificity(test_$ISSUE, predicted)


#calculate total misclassification error rate
misClassError(test_$ISSUE, predicted, threshold=optimal)


#PREVISION 2 

table(train_$ISSUE)
prop.table(table(train_$ISSUE))*100
summary(train_$ISSUE)

attach(train_)
library(randomForest)
rfBase.train <- randomForest(ISSUE ~ DFGA + `D3P%` + DFTM  + DBLK + DDREB, data = train_) 
attach(test_)

confusionMatrix(test_$ISSUE, rfBase.train)

#PREVISION 3 


# Loading package
library(e1071)
library(caTools)
library(class)

# Loading data
attach(Echantillon_match_327)
data(Echantillon_match_327)
head(Echantillon_match_327)



#-----------------mm2

# Feature Scaling

train_scale <- scale(train_[, 1:4])
test_scale <- scale(test_ [, 1:4])

# Fitting KNN Model
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_$ISSUE,
                      k = 1)
classifier_knn

# Confusiin Matrix
#length.POSIXlt(mm2$ISSUE_2)
cm <- table(test_$ISSUE , classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_$ISSUE)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn_3 <- knn(train = train_scale,
                        test = test_scale,
                        cl = train_$ISSUE,
                        k = 3)
misClassError <- mean(classifier_knn_3 != test_$ISSUE)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn_5 <- knn(train = train_scale,
                        test = test_scale,
                        cl = train_$ISSUE,
                        k = 5)
misClassError <- mean(classifier_knn_5 != test_$ISSUE)
print(paste('Accuracy =', 1-misClassError))

# K = 7
classifier_knn_7 <- knn(train = train_scale,
                        test = test_scale,
                        cl = train_$ISSUE,
                        k = 7)
misClassError <- mean(classifier_knn_7!= test_$ISSUE)
print(paste('Accuracy =', 1-misClassError))

# K = 15
classifier_knn_15 <- knn(train = train_scale,
                         test = test_scale,
                         cl = train_$ISSUE,
                         k = 15)
misClassError <- mean(classifier_knn_15 != test_$ISSUE)
print(paste('Accuracy =', 1-misClassError))

