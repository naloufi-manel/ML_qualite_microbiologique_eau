######### 
# Naloufi Manel 
# version :  V3.5.1
######### 

######################################## Chargement des données  #########################################
# definir le chemin 
setwd("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util")

library(stringr)
# créer la liste contenant les fichiers csv
ldf_1 <- list.files(pattern="csv") 
myList <- list()

# Boucle pour lire les fichiers 
for (k in 1:length(ldf_1)){
  myList[[k]]<-read.delim(ldf_1[k])
} 
names(myList)<-ldf_1 # Renommer des fichiers  
################################################################################################


##################################### Analyse des données #########################################
######################## Faire la somme des données de pluviométries par jour  

# Faire une boucle pour analyser tous les fichiers (un fichier par pluviométre)
for (k in 1:length(ldf_1)){
  # Transformer la colonne avec les dates à la bonne forme puis transformer en facteur pour regrouper le meme jour
  myList[[k]]$Horodatage<-str_replace(myList[[k]]$Horodatage, "[:digit:][:digit:]:[:digit:][:digit:]:[:digit:][:digit:]", "")
  myList[[k]]$Horodatage=as.factor(myList[[k]]$Horodatage)
  # Cree un vecteur date qui va contenir toutes les dates du tableau
  date<-sample(1, length(levels(myList[[k]]$Horodatage)), replace = TRUE)
  # Cree un vecteur pluvio qui va contenir la somme des pluies de chaque jour 
  pluvio<-sample(1, length(levels(myList[[k]]$Horodatage)), replace = TRUE)
  # cree le tableau (date et somme des pluies)
  tab_somme_pluvio<-cbind(date, pluvio)
  
  # Boucle pour faire la somme des pluies pour chaque jour 
  j=1
  for (i in levels(myList[[k]]$Horodatage)){
    t<-myList[[k]][myList[[k]]$Horodatage==i,]
    tab_somme_pluvio[j,1]<-i
    tab_somme_pluvio[j,2]<-sum(t$Valeur)
    j=j+1
  } 
  tab_somme_pluvio<-as.data.frame(tab_somme_pluvio)
  # Creation des tableaux pour les periodes des campagnes de prelevement et export des tableaux pour chaque mois 
  t2015<-tab_somme_pluvio[grep(pattern="2015",tab_somme_pluvio$date),]
  t2016<-tab_somme_pluvio[grep(pattern="2016",tab_somme_pluvio$date),]
  t2017<-tab_somme_pluvio[grep(pattern="2017",tab_somme_pluvio$date),]
  t2018<-tab_somme_pluvio[grep(pattern="2018",tab_somme_pluvio$date),]
  t2019<-tab_somme_pluvio[grep(pattern="2019",tab_somme_pluvio$date),]
  t2020<-tab_somme_pluvio[grep(pattern="2020",tab_somme_pluvio$date),]
  
  t2015mai<-t2015[grep(pattern="/05/",t2015$date),]
  t2015juin<-t2015[grep(pattern="/06/",t2015$date),]
  t2015juill<-t2015[grep(pattern="/07/",t2015$date),]
  t2015aout<-t2015[grep(pattern="/08/",t2015$date),]
  t2015sept<-t2015[grep(pattern="/09/",t2015$date),]
  
  t2016mai<-t2016[grep(pattern="/05/",t2016$date),]
  t2016juin<-t2016[grep(pattern="/06/",t2016$date),]
  t2016juill<-t2016[grep(pattern="/07/",t2016$date),]
  t2016aout<-t2016[grep(pattern="/08/",t2016$date),]
  t2016sept<-t2016[grep(pattern="/09/",t2016$date),]
  
  t2017mai<-t2017[grep(pattern="/05/",t2017$date),]
  t2017juin<-t2017[grep(pattern="/06/",t2017$date),]
  t2017juill<-t2017[grep(pattern="/07/",t2017$date),]
  t2017aout<-t2017[grep(pattern="/08/",t2017$date),]
  t2017sept<-t2017[grep(pattern="/09/",t2017$date),]
  
  t2018mai<-t2018[grep(pattern="/05/",t2018$date),]
  t2018juin<-t2018[grep(pattern="/06/",t2018$date),]
  t2018juill<-t2018[grep(pattern="/07/",t2018$date),]
  t2018aout<-t2018[grep(pattern="/08/",t2018$date),]
  t2018sept<-t2018[grep(pattern="/09/",t2018$date),]

  t2019mai<-t2019[grep(pattern="/05/",t2019$date),]
  t2019juin<-t2019[grep(pattern="/06/",t2019$date),]
  t2019juill<-t2019[grep(pattern="/07/",t2019$date),]
  t2019aout<-t2019[grep(pattern="/08/",t2019$date),]
  t2019sept<-t2019[grep(pattern="/09/",t2019$date),]
  
  t2020mai<-t2020[grep(pattern="/05/",t2020$date),]
  t2020juin<-t2020[grep(pattern="/06/",t2020$date),]
  t2020juill<-t2020[grep(pattern="/07/",t2020$date),]
  t2020aout<-t2020[grep(pattern="/08/",t2020$date),]
  t2020sept<-t2020[grep(pattern="/09/",t2020$date),]
  
  write.csv(t2015mai, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"t2015mai.csv"))
  write.csv(t2015juin, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2015juin.csv"))
  write.csv(t2015juill, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2015juil.csv"))
  write.csv(t2015aout, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2015aout.csv"))
  write.csv(t2015sept, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2015sept.csv"))

  write.csv(t2016mai, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"t2016mai.csv"))
  write.csv(t2016juin, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2016juin.csv"))
  write.csv(t2016juill, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2016juil.csv"))
  write.csv(t2016aout, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2016aout.csv"))
  write.csv(t2016sept, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2016sept.csv"))
  
  write.csv(t2017mai, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"t2017mai.csv"))
  write.csv(t2017juin, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2017juin.csv"))
  write.csv(t2017juill, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2017juil.csv"))
  write.csv(t2017aout, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2017aout.csv"))
  write.csv(t2017sept, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2017sept.csv"))
  
  write.csv(t2018mai, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"t2018mai.csv"))
  write.csv(t2018juin, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2018juin.csv"))
  write.csv(t2018juill, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2018juil.csv"))
  write.csv(t2018aout, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2018aout.csv"))
  write.csv(t2018sept, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2018sept.csv"))
  
  write.csv(t2019mai, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"t2019mai.csv"))
  write.csv(t2019juin, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2019juin.csv"))
  write.csv(t2019juill, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2019juil.csv"))
  write.csv(t2019aout, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2019aout.csv"))
  write.csv(t2019sept, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2019sept.csv"))
  
  write.csv(t2020mai, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"t2020mai.csv"))
  write.csv(t2020juin, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2020juin.csv"))
  write.csv(t2020juill, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2020juil.csv"))
  write.csv(t2020aout, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2020aout.csv"))
  write.csv(t2020sept, file = paste("/home/manel/Bureau/DonnВes pluvios Paris/pluvio_util/new/",names(myList[k]),"2020sept.csv"))
  
}

levels(myList[[k]]$Horodatage)
