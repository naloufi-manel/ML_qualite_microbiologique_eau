######### 
# Naloufi Manel 
# version :  V3.5.1
#########

######################################## Chargement des données  #########################################
# Definir le chemin 
setwd("/home/manel/Bureau/BD/donnee+pluvio/smv+vdp-pluvio/donnee-sans-na/smv-vdp/ML/train_seine") # chemin donnee brute

# Créer la liste contenant les fichiers csv
ldf_1 <- list.files(pattern="csv") 
myList <- list()

# Boucle pour lire les fichiers 
for (k in 1:length(ldf_1)){
  myList[[k]]<-read.csv(ldf_1[k])
} 
names(myList)<-ldf_1# Renommer des fichiers  
################################################################################################


##################################### Analyse des données #########################################
#Grouper les 10 jeu test ensemble  
rf_rep<-myList[[1]][,]
for (k in 2:length(ldf_1)){
  r<-myList[[k]][,]
  rf_rep<-rbind(rf_rep,r)
}
rf_rep 

# Visualisation des résultats de la prédiction par le modèle basé sur la RF. 
x=rep(1:12,1)
plot(log(rf_rep$RF_Ecoli_pred),log(rf_rep$Ecolireal), xlab=expression(paste("Concentration en ", italic("E. coli"), "mesurée (Log NPP/100ml)")),ylab =expression(paste("Concentration en ", italic("E. coli"), "prédite (Log NPP/100ml)")), add=TRUE, xlim=c(3,13), ylim=c(3,13))
lines(x,x, col="red")
plot(rf_rep$RF_Ecoli_pred,rf_rep$Ecolireal, xlab="Log(E.coli)",ylab="Log(predicted E.coli)", add=TRUE)
lines(x,x, col="red")
rf_par<-list()

################################################################################################

################################### Analyse des données prédites ##################################
# Division en 2 sous-listes, les estimations raisonnables et les estimations inexactes 
rf_fit<-myList[[1]][which(myList[[k]][,12]=="fit"),]
rf_non<-myList[[1]][which(myList[[k]][,12]=="non"),]

for (k in 2:length(ldf_1)){
r1<-myList[[k]][which(myList[[k]][,12]=="fit"),]
r2<-myList[[k]][which(myList[[k]][,12]=="non"),]
rf_fit<-rbind(rf_fit,r1)
rf_non<-rbind(rf_non,r2)
}
### Définir la fonction not in 
### Identifier les éléments présents dans une liste mais pas dans l'autre 
`%notin%` <- #create a new binary pipe operator
  function (x, table)
    is.na(match(x, table, nomatch = NA_integer_))

################################################################################################

################################### identification des valeurs ###################################
# identification des valeurs des paramètres des observations qui présentent une estimation raisonnable qui ne donne pas une estimation inexacte. 
rf<-list()

for (i in 1: 9){
  list_rf_fit<-unique(rf_fit[,i])
  l2<-list_rf_fit[][list_rf_fit%notin%rf_non[,i]]
  rf[[i]]<-list()
  rf[[i]]<-sort(l2)
}
rf

# exporter la liste et les tableaux
capture.output(rf, file = "my_list-parametre.txt") 
write.csv(rf_fit, file = "/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat/test_fit.csv")
write.csv(rf_non, file = "/home/manel/Bureau/BD/donnee+pluvio/smv-complet/ML/replicat/test_non_fit.csv")
################################################################################################

################################### Visualisation des valeurs ####################################
# Identification des plages de valeurs
seine <- read.csv("~/Bureau/BD/donnee+pluvio/smv+vdp-pluvio/donnee-sans-na/smv-vdp/seine.csv")
summary(seine)
par(mfrow=c(1,2))
hist(rf[[2]], nclass = 5000, xlab = "Temperature", xlim =c(15.6,27.5))
hist(rf[[3]], nclass = 5000, xlab = "Conductivite", xlim =c(425,1407))
