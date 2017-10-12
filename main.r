
rm(list=objects());
graphics.off()
library(corrplot)
library(gplots)
library(ROCR)
library(MASS)
setwd("~/Spam_detector-master")

df=read.table("spam.data")
head(df)
dim(df)
names(df)[58]<-"Y"
head(df)
cov(df)
summary(df)

#variables correlees
corrplot(cor(df))
#Ce quie st au milieu est un peu correle, le reste l'est tres peu
n=dim(df)[1]
sum(df$Y)/n
#39,4% de spams

plot(df$V1,df$Y)
plot(df$V5,df$Y)
plot(df$V24,df$Y)
plot(df$V41,df$Y)


#  Separation echantillons test et apprentissage
set.seed(103)
ind <- sample(2, nrow(df), replace=T, prob=c(2/3,1/3))
tdata<- df[ind==1,] # training = 2/3
vdata<- df[ind==2,] # validation = 1/3


Ntotv=dim(vdata)[1] #Nombre d'individus dans l'échantillon de test
Nposv=sum(vdata$Y) #Nombre de mail spam dans l'échantillon de test
Nnegv=Ntotv-Nposv #Nombre de mail non spam dans l'échantillon de test
Ntott=dim(tdata)[1] #Nombre d'individus dans l'échantillon d'apprentissage
Npost=sum(tdata$Y) #Nombre de mail spam dans l'échantillon d'apprentissage
Nnegt=Ntott-Npost #Nombre de mail non spam dans l'échantillon d'apprentissage


# Regression logistique sur le modèle complet 
results <-glm(Y~.,family="binomial",data=tdata)
summary(results)


# Determiner le meilleur cutoff pour l'échantillon d'apprentissage

pred_train <- predict(results,tdata,type='response')
pred <- prediction(pred_train,tdata$Y)
perf <- performance(pred,"acc")
plot(perf,main='Accuracy en fonction du cutoff pour apprentissage')
max1 <- which.max(slot(perf,"y.values")[[1]])
cut1 <- slot(perf,"x.values")[[1]][max1]
abline(v=cut1,h=slot(perf,"y.values")[[1]][max1],col="blue")

# Performance sur l'échantillon de test
pred_valiation <- predict(results,vdata,type='response')
model_pred_admitv <- rep("0",Ntotv)
model_pred_admitv[pred_valiation>0.5]<-"1"
confus_validation<-table(model_pred_admitv,vdata$Y)
confus_validation
# pred_valiation   0   1
#       0         882  74
#       1         46  542
erclassv=(confus_validation[2]+confus_validation[3])/Ntotv
erclassv
# erreur de classification sur l'echantillon de test est de 7,8%

# Performance sur l'échantillon d'apprentissage
model_pred_admitt <- rep("0",Ntott)
model_pred_admitt[pred_train>cut1]<-"1"
convus_train <- table(model_pred_admitt,tdata$Y)
convus_train
# pred_train       0      1
#       0         1765    92
#       1         95    1105
erclasst=(convus_train[2]+convus_train[3])/Ntott
erclasst
# erreur de classification sur l'echantillon d'aprentissage est de 6,1%

#ROC

predv <- predict(results,vdata)
predv <- prediction(predv,vdata$Y)
rocv <- performance(predv,"tpr","fpr")
plot(rocv)
title("ROC sur échantillon test")
abline(h=1,col='red')
abline(v=0,col='red')
abline(0,1,col='blue')

predt <- predict(results,tdata)
predt <- prediction(predt,tdata$Y)
roct <- performance(predt,"tpr","fpr")
plot(roct)
title("ROC sur échantillon d'apprentissage")
abline(h=1,col='red')
abline(v=0,col='red')
abline(0,1,col='blue')

#Area under curve ROC

auct <- performance(predt,"auc")
auct <- round(unlist(slot(auct,"y.values")),3)
auct
# auc sur l'echantillon d'apprentissage est de 0.978

aucv <- performance(predv,"auc")
aucv <- round(unlist(slot(aucv,"y.values")),3)
aucv
# auc sur l'echantillon de test est de 0.971

#lift

liftt <- performance(predt,"tpr","rpp")
plot(liftt, main="lift curve sur l'échantillon d'apprentissage")
abline(h=1,col='red')
abline(0,1/(Npost/Ntott),col='red')
abline(0,1,col='blue')

liftv <- performance(predv,"tpr","rpp")
plot(liftv, main="lift curve sur l'échantillon de test")
abline(h=1,col='red')
abline(0,1/(Nposv/Ntotv),col='red')
abline(0,1,col='blue')

# Question 7 sous modèle avec les varibles tres significtives
summary(results)
# On ne garde que les variables qui ont une Pr(>|t|) inférieur à 10^-5 :

result <- glm(Y~V5+V6+V7+V16+V21+V23+V25+V27+V45+V46+V52+V53,family="binomial",data=tdata)
summary(result)

pred_valiation <- predict(result,vdata,type="response")


# Determiner le meilleur cutoff pour l'échantillon d'apprentissage

pred_train <- predict(result,tdata,type="response")
pred <- prediction(pred_train,tdata$Y)
perf <- performance(pred,"acc")
plot(perf,main='Accuracy en fonction du cutoff pour apprentissage')
max2t <- which.max(slot(perf,"y.values")[[1]])
cut2t <- slot(perf,"x.values")[[1]][max2t]
abline(v=cut2t,h=slot(perf,"y.values")[[1]][max2t],col="blue")

model_pred_admitv <- rep("0",Ntotv)
model_pred_admitv[pred_valiation>0.5]<-"1"
confus_validation<-table(model_pred_admitv,vdata$Y)
confus_validation
# pred_valiation   0    1
#       0         871  100
#       1         57   516
erclassv=(confus_validation[2]+confus_validation[3])/Ntotv
erclassv
# erreur de classification sur l'chantillon de test est de 10,2%

pred_train <- predict(result,tdata)
model_pred_admitt <- rep("0",Ntott)
model_pred_admitt[pred_train>cut2t]<-"1"
convus_train <- table(model_pred_admitt,tdata$Y)
convus_train
# pred_train       0     1
#       0         1794  258
#       1         66    939
erclasst=(convus_train[2]+convus_train[3])/Ntott
erclasst
# erreur de classification sur l'chantillon de test est de 10,6%

#ROC
pred2 <- predict(result,vdata)
pred2 <- prediction(pred2,vdata$Y)
roc2 <- performance(pred2,"tpr","fpr")
plot(roc2)
par(new=TRUE)
plot(rocv,col="green")
title("ROC")
abline(h=1,col='red')
abline(v=0,col='red')
abline(0,1,col='blue')


aucv2 <- performance(pred2,"auc")
aucv2 <- round(unlist(slot(aucv2,"y.values")),3)
aucv2 # = 0.956

# question 8 selection de variable

#aic <- stepAIC(results)
res <- glm(Y ~ V2 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V15 + V16 + V17 + 
             V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
             V28 + V29 + V33 + V35 + V38 + V39 + V41 + V42 + V43 + V44 + 
             V45 + V46 + V48 + V49 + V52 + V53 + V54 + V56 + V57,family="binomial"
                , data = tdata)

pred_valiation <- predict(res,vdata,type="response")
model_pred_admitv <- rep("0",Ntotv)
model_pred_admitv[pred_valiation>0.5]<-"1"
confus_validation <- table(model_pred_admitv,vdata$Y)
confus_validation
# pred_valiation   0    1
#       0         875  74
#       1         53   542

erclassv=(confus_validation[2]+confus_validation[3])/Ntotv
erclassv
# erreur de classification sur l'echantillon de test est de 8,2%

#ROC
pred3 <- predict(res,vdata)
pred3 <- prediction(pred3,vdata$Y)
roc3 <- performance(pred3,"tpr","fpr")
plot(roc3)
par(new=TRUE)
plot(rocv,col="green")
title("ROC")
abline(h=1,col='red')
abline(v=0,col='red')
abline(0,1,col='blue')

aucv <- performance(pred3,"auc")
aucv <- round(unlist(slot(aucv,"y.values")),3)
aucv # = 0.972

# question 9 

pred_valiation <- predict(res,vdata,type="response")
model_pred_admitv <- rep("0",Ntotv)
model_pred_admitv[pred_valiation>0.225]<-"1" # On prend un seuil de 0.225 pour minimiser l'erreur tout en ayant 95% de spam bloqué
confus_validation<-table(model_pred_admitv,vdata$Y)
confus_validation
# pred_valiation   0   1
#       0        802   30
#       1        126  586
taux_block=confus_validation[4]/Nposv
taux_block #  0.9512987
erclassv=(confus_validation[2]+confus_validation[3])/Ntotv
erclassv # 0.1010363
taux_non_spam_block=confus_validation[2]/Nnegv
taux_non_spam_block # 0.1357759


#Partie 2

#La regression ridge consiste à rajouter une constante au XX' de l'estimateur : 
#Un estimateur ls est de la forme (X'X)*X'Y
#Dans cette regression on abondonne la contrainte de l'estimateur sans biais et on rajoute
#Une constante c pour obtenir (X'X + c Id)*X'Y

#Dans cette modélisation on utilisera ce nouvel estimateur pour minimiser la somme des carrés

#Concretement sur cette somme de carrés, ce nouveau terme 'c' sera multiplié par la norme
#au carre de l'estimateur : si l'estimateur prend des valeurs trop grosses, la
#fonction d'optimisation sera pénalisée

library(glmnet)

mat_tdata=as.matrix(tdata)
dim(mat_tdata)
mat_tdata=mat_tdata[,-58]
dim(mat_tdata)
model=glmnet(mat_tdata,tdata$Y, alpha = 0, lambda=c(1e-2, 1e-1, 1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10),family = "binomial") 

#lambda est le coefficient de penalisation. 
#avec un lambda de 10-2, quasi nul, on a quasimment un modele LS, pas de penalisation
#en fonction de l'estimateur
#Avec un lambda tres grand, la penalisation est enorme, on interdit a l'estimateur de prendre
#des grandes valeurs

dim(predict(model,mat_tdata,s=c(0.01,1,100,10^4, 10^6, 10^8, 10^10)))
#predict est une matrice de largeur 7 comportant pour chaque ligne i les predictions 
#du spam x[i], avec tous les lambdas

# cas lambda = 2
model2=glmnet(mat_tdata,tdata$Y,  lambda=2,alpha = 0)
mat_vdata=as.matrix(vdata)
mat_vdata=mat_vdata[,-58]
proba_pred=predict(model2,mat_vdata,type="response")

dim(proba_pred)
model_pred_admitv <- rep("0",Ntotv)
model_pred_admitv[proba_pred>0.4]<-"1"

confus_validation<-table(model_pred_admitv,vdata$Y)
confus_validation
#model_pred_admitv   0   1
#                0 848  160
#                1  38  456
taux_block=confus_validation[4]/Nposv
taux_block # 0.8587662
erclassv=(confus_validation[2]+confus_validation[3])/Ntotv
erclassv # 0.1081606
taux_non_spam_block=confus_validation[2]/Nnegv
taux_non_spam_block # 0.0862069

pred4 <- predict(model2,mat_vdata)
pred4 <- prediction(pred4,vdata$Y)
roc4 <- performance(pred4,"acc")
plot(roc4,main='Accuracy en fonction du cutoff pour le test')
max4 <- which.max(slot(roc4,"y.values")[[1]])
cut4 <- slot(roc4,"x.values")[[1]][max4]
abline(v=cut4,h=slot(roc4,"y.values")[[1]][max4],col="blue")



plot(model)
#toutes les courbes en couleur correspondent aux coefficients des variables du modèle,
#contre la valeur de la norme du vecteur penalisee par lambda

model=cv.glmnet(mat_tdata,tdata$Y)#nfolds est à 10 par defaut
#$lambda.min
#[1] 0.003150837

#$lambda.1se
#[1] 0.01396017


#L'algorithme realise, en cross validation, des iterations avec des lambdas differents.
#Il en deduit, par encadrement, une estimation du lambda minimum
#Le lambda est relativement petit ; on n'a pas besoin de fortement penaliser le max



proba_pred=predict(model,mat_vdata,type="response")

pred5 <- proba_pred
pred5 <- prediction(pred5,vdata$Y)
roc5 <- performance(pred5,"acc")
plot(roc5,main='Accuracy en fonction du cutoff pour le test')
max5 <- which.max(slot(roc5,"y.values")[[1]])
cut5 <- slot(roc5,"x.values")[[1]][max5]
abline(v=cut5,h=slot(roc5,"y.values")[[1]][max5],col="blue")

dim(proba_pred)
model_pred_admitv <- rep("0",Ntotv)
model_pred_admitv[proba_pred>0.4]<-"1"
confus_validation<-table(model_pred_admitv,vdata$Y)
confus_validation

#model_pred_admitv   0   1
#                0 861   94
#                1  67  522

taux_block=confus_validation[4]/Nposv
taux_block # 0.849026
erclassv=(confus_validation[2]+confus_validation[3])/Ntotv
erclassv # 0.1036269
taux_non_spam_block=confus_validation[2]/Nnegv
taux_non_spam_block # 0.07219828



