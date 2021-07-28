######### 
# Naloufi Manel 
# version :  V3.5.1
######### 

######################################## Chargement des données  #########################################
# Definir le chemin 
setwd("/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat")

# Créer la liste contenant les fichiers csv
ldf_1 <- list.files(pattern="csv") 
myList <- list()

# Boucle pour lire les fichiers 
for (k in 1:length(ldf_1)){
  myList[[k]]<-read.csv(ldf_1[k])
} 

names(myList)<-ldf_1 # Renommer des fichiers  
################################################################################################


##################################### Analyse des données #########################################
######################## Identification du nombre de prédictions raisonnables et inexactes 
x<-sample(1, (length(ldf_1)), replace = TRUE)
tab<-cbind(x,x,x,x,x)

## Le tableau contient : { le numéro de réplique (replicat), le nombre d'estimations raisonnables (nombre-fit), 
# le nombre d'estimations inexactes (nombre-non-fit), le pourcentage d'estimations raisonnables (pourc-fit),
#le pourcentage d'estimations inexactes (pourc-non-fit)}
colnames(tab)<-c("replicat","nombre-fit","nombre-non-fit","pourc-fit","pourc-non-fit")
for (k in 1:length(ldf_1)){
  tab[k,1]<-names(myList[k])
  tab[k,2]<-length(which(myList[[k]]$Fit.50..=="fit"))
  tab[k,3]<-length(which(myList[[k]]$Fit.50..=="non"))
  tab[k,4]<-round((as.integer(tab[k,2])*100)/170, digits = 2)
  tab[k,5]<-round((as.integer(tab[k,3])*100)/170, digits = 2)
  }
tab

# exporter le tableau 
write.csv(tab, file = "/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat/calcule-nb-bien-predit.csv")
################################################################################################


################# Vérification de la normalité de la concentration d'E.coli ####################
# Création du tableau qui contiendra : 
# 1(les données ont une distribution normale) et 0 : les données n'ont pas une distribution normale. x<-sample(1, (length(myList)), replace = TRUE)
tab_pval<-cbind(x,x,x)
tabf_pval<-cbind(x,x,x)
tabn_pval<-cbind(x,x,x)

# Pour chaque analyse : 
# la colonne 2 présente l'analyse pour la concentration d'E.coli mesurée et la colonne 3 pour la concentration d'E.coli prédite par le modèle basé sur la RF. 

# Analyse de l'ensemble de données 
colnames(tab_pval)<-c("replicat","ec-reel","ec-rf")
for (k in 1:length(ldf_1)){
  ### Analyse de la p-value du test de Shapiro 
  tab_pval[k,1]<-names(myList[k])
  if( shapiro.test(myList[[k]][,13])$p.value > 0.05) {tab_pval[k,2]<-1}
  else { tab_pval[k,2]<-0}
  if( shapiro.test(myList[[k]][,14])$p.value > 0.05) {tab_pval[k,3]<-1}
  else { tab_pval[k,3]<-0}
  
}
tab_pval

# Analyse pour la prédiction raisonnable
colnames(tabf_pval)<-c("replicat","ec-reel","ec-rf")
for (k in 1:length(ldf_1)){
  ### Analyse de la p-value du test de Shapiro 
  rf_fit<-myList[[k]][which(myList[[k]][,16]=="fit"),1:15]
  tabf_pval[k,1]<-names(myList[k])
  if( shapiro.test(rf_fit[,13])$p.value > 0.05) {tabf_pval[k,2]<-1}
  else { tabf_pval[k,2]<-0}
  if( shapiro.test(rf_fit[,14])$p.value > 0.05) {tabf_pval[k,3]<-1}
  else { tabf_pval[k,3]<-0}
  
}
tabf_pval

# Analyse pour la prédiction inexacte
colnames(tabn_pval)<-c("replicat","ec-reel","ec-rf")
for (k in 1:length(ldf_1)){
  ### Analyse de la p-value du test de Shapiro 
  rf_non<-myList[[k]][which(myList[[k]][,16]=="non"),1:15]
  tabn_pval[k,1]<-names(myList[k])
  if( shapiro.test(rf_non[,13])$p.value > 0.05) {tabn_pval[k,2]<-1}
  else { tabn_pval[k,2]<-0}
  if( shapiro.test(rf_non[,14])$p.value > 0.05) {tabn_pval[k,3]<-1}
  else { tabn_pval[k,3]<-0}
  
}
tabn_pval

### Les données n'ont pas une distribution normale 
# Ainsi les analyses de corrélation seront effectuées par la méthode de spearman. 
################################################################################################

################################### Analyse de corrélation #######################################
# Création de la table qui contiendra les corrélations  
x<-sample(1, (2*length(myList)), replace = TRUE)
tab_cor<-cbind(x,x,x,x,x,x,x,x,x,x,x)

# Pour chaque réplicat (graine), une analyse de corrélation est effectuée entre la concentration prédite d'E.coli et les paramètres de l'eau pour la prédiction raisonnable et inexacte.
seed<-c("seed0","seed1","seed2","seed3","seed4","seed5","seed6","seed7","seed8","seed9")
colnames(tab_cor)<-c("donnee","Temperature","Conductivity","Turbidity","MES","NH4","NTK","Number of dry days","Rainfall of the day","Rainfall of the day before","Flow")
i=0
for (k in 1:length(ldf_1)){
  # Analyse pour la prédiction raisonnable
  tab_cor[k+i,1]<-paste(seed[i+1],"-reasonable")
  rf_fit<-myList[[k]][which(myList[[k]][,16]=="fit"),1:15]
  tab_cor[k+i,2]<-cor(rf_fit[,14],rf_fit[,3],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,3]<-cor(rf_fit[,14],rf_fit[,4],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,4]<-cor(rf_fit[,14],rf_fit[,5],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,5]<-cor(rf_fit[,14],rf_fit[,6],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,6]<-cor(rf_fit[,14],rf_fit[,7],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,7]<-cor(rf_fit[,14],rf_fit[,8],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,8]<-cor(rf_fit[,14],rf_fit[,9],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,9]<-cor(rf_fit[,14],rf_fit[,10],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,10]<-cor(rf_fit[,14],rf_fit[,11],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,11]<-cor(rf_fit[,14],rf_fit[,12],  method = "spearman", use = "na.or.complete")
  i=i+1
  
  # Analyse pour la prédiction inexacte
  tab_cor[k+i,1]<-paste(seed[i],"-inaccurate")
  rf_non<-myList[[k]][which(myList[[k]][,16]=="non"),1:15]
  tab_cor[k+i,2]<-cor(rf_non[,14],rf_non[,3],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,3]<-cor(rf_non[,14],rf_non[,4],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,4]<-cor(rf_non[,14],rf_non[,5],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,5]<-cor(rf_non[,14],rf_non[,6],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,6]<-cor(rf_non[,14],rf_non[,7],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,7]<-cor(rf_non[,14],rf_non[,8],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,8]<-cor(rf_non[,14],rf_non[,9],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,9]<-cor(rf_non[,14],rf_non[,10],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,10]<-cor(rf_non[,14],rf_non[,11],  method = "spearman", use = "na.or.complete")
  tab_cor[k+i,11]<-cor(rf_non[,14],rf_non[,12],  method = "spearman", use = "na.or.complete")
  
}
################################################################################################

################################ Visualisation des corrélations ################################
# Transformation des valeurs en valeurs numériques 
newtab<-tab_cor
t <- as.data.frame(newtab[,-1])
for (i in 1: ncol(t)){
  t[,i]<-as.character(t[,i])
  t[,i]<-as.numeric(t[,i])
}

# Création de la matrice
mat<-as.matrix(t)
rownames(mat)<-newtab[,1]

# Visualisation par un corrplot 
library(corrplot)
corrplot(mat)
corrplot(mat, method = "number")

### Division en 2 sous-tableaux, un avec la corrélation pour les estimations raisonnables et un pour les estimations inexactes 
j=1
k=1
new1<-newtab[1:10,]
new2<-newtab[1:10,]
for(i in 1:nrow(mat)){
  if (i%%2==0){
    new1[j,]<-newtab[i,]
    j<-j+1
  }
  else {
    new2[k,]<-newtab[i,]
    k<-k+1
  }
}

# Transformer les valeurs en chiffres pour les 2 tableaux et exporter les données
t1 <- as.data.frame(new1[,-1])
for (i in 1: ncol(t1)){
  t1[,i]<-as.character(t1[,i])
  t1[,i]<-as.numeric(t1[,i])
}
write.csv(t1, file = "/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat/tableau_correlationt-non.csv")

t2 <- as.data.frame(new2[,-1])
for (i in 1: ncol(t1)){
  t2[,i]<-as.character(t2[,i])
  t2[,i]<-as.numeric(t2[,i])
}
write.csv(t2, file = "/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat/tableau_correlationt-fit.csv")

# Création de la matrice
mat1<-as.matrix(t1)
mat2<-as.matrix(t2)
rownames(mat1)<-new1[,1]
rownames(mat2)<-new2[,1]

# Visualisation par un corrplot
corrplot(mat1, method="pie")
corrplot(mat2, method="pie")
################################################################################################

########################################### Analyse statistique ###############################
# Test de significativité entre les corrélations 

# Analyse de la normalisation
x<-sample(0, length(t1), replace = TRUE)
tab_pval<-cbind(x,x,x)
for (k in 1:length(t1)){
  
  if( shapiro.test(t1[,k])$p.value > 0.05) {tab_pval[k,2]<-1}
  else { tab_pval[k,2]<-0}
  if( shapiro.test(t1[,k])$p.value > 0.05) {tab_pval[k,3]<-1}
  else { tab_pval[k,3]<-0}
  
}
tab_pval
### Les données ont une distribution normale 

# Test de différence entre les corrélations (test t)
x<-sample(0, length(t1), replace = TRUE)
tab_pval<-cbind(x,x)
for (k in 1:length(t1)){
  tab_pval[k,1]<-colnames(t1)[k]
  tt<-t.test(t1[,k],t2[,k])$p.value
  tab_pval[k,2]<-tt
}
tab_pval

