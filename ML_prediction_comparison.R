######### 
# Naloufi Manel 
# version :  V3.5.1
#########

# Visualisation de la distribution des données de notre jeu de données. 
# Marne
smv <- read.csv("~/Bureau/BD/donnee+pluvio/smv-complet/donnee_brute_sans_na/smv.csv")
titre<-c("Température (°C)", "Conductivité (µS/cm)","Turbidité (FNU)", "MES (mg/L)", "NH4+ (mg(N)/L)", "NTK (mg(N)/L)", "Nombre de jours secs (jours) ", "Pluviométrie du jour (mm)", "Pluviométrie de la veille (mm)", "Débit à Gournay-sur-Marne (m3/s)")
par(mfrow=c(3,4))
par(mar=c(3, 0.5, 1, 2))
#    oma = c(4, 4, 0.2, 0.2))
for (i in 5: length(smv)){
  boxplot(smv[,i], col="grey")
  mtext(side = 1, titre[i-4], line = 1, cex.lab=1.5)
}

boxplot(log(smv$Ecoli), col="grey")
mtext(side = 1, "E.coli (log NPP/100ml)", line = 1, cex.lab=1.5)

# Seine
seine <- read.csv("/home/manel/Bureau/BD/donnee+pluvio/smv+vdp-pluvio/donnee-sans-na/smv-vdp/seine.csv")
titre<-c("Température (°C)", "Conductivité (µS/cm)","Turbidité (FNU)", "Nombre de jours secs (jours) ", "Pluviométrie du jour (mm)", "Pluviométrie de la veille (mm)", "Débit à Austerlitz (m3/s)")
par(mfrow=c(2,4))
par(mar=c(3, 0.5, 1, 2))
for (i in 5: length(seine)){
  boxplot(seine[,i], col="grey")
  mtext(side = 1, titre[i-4], line = 1, cex.lab=1.5)
}
boxplot(log(seine$Ecoli), col="grey")
mtext(side = 1, "E.coli (log NPP/100ml)", line = 1, cex.lab=1.5)

# Visualisation graphique des résultats de performance (RMSE, MAE et RPD) des modèles d'apprentissage automatique.
library(readxl)
setwd("/home/manel/Bureau/BD/donnee+pluvio/smv+vdp-pluvio/donnee-sans-na/smv-vdp/ML/train_smv/erreur")

par(mfrow=c(1,3))
plot_performance <- read_excel("marne.xlsx", sheet = "rmse", 
                               col_types = c("numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric" 
                               ))
boxplot(plot_performance[,1:6], main="RMSE", col="red")

plot_performance <- read_excel("marne.xlsx", sheet = "mae", 
                               col_types = c("numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))
boxplot(plot_performance[,1:6], main="MAE", col="green")

plot_performance <- read_excel("marne.xlsx", sheet = "rpd", 
                               col_types = c("numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))
boxplot(plot_performance[,1:6], main="RPD", col="blue")

plot_performance <- read_excel("seine.xlsx", sheet = "rmse", 
                               col_types = c("numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric" 
                               ))
boxplot(plot_performance[,1:6], main="RMSE", col="red")

plot_performance <- read_excel("seine.xlsx", sheet = "mae", 
                               col_types = c("numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))
boxplot(plot_performance[,1:6], main="MAE", col="green")

plot_performance <- read_excel("seine.xlsx", sheet = "rpd", 
                               col_types = c("numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))
boxplot(plot_performance[,1:6], main="RPD", col="blue")


# Visualisation graphique des résultats de performance (RMSE, MAE et RPD) des modèles d'apprentissage automatique.
library(readxl)
setwd("/home/manel/Bureau/BD/donnee+pluvio/smv+vdp-pluvio/donnee-sans-na/smv-vdp/ML/train_seine/erreur")

par(mfrow=c(1,3))
plot_performance <- read_excel("marne.xlsx", sheet = "rmse", 
                                col_types = c("numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric" 
                                              ))
boxplot(plot_performance[,1:6], main="RMSE", col="red")

plot_performance <- read_excel("marne.xlsx", sheet = "mae", 
                                col_types = c("numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric"))
boxplot(plot_performance[,1:6], main="MAE", col="green")

plot_performance <- read_excel("marne.xlsx", sheet = "rpd", 
                                col_types = c("numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric"))
boxplot(plot_performance[,1:6], main="RPD", col="blue")

plot_performance <- read_excel("seine.xlsx", sheet = "rmse", 
                               col_types = c("numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric" 
                               ))
boxplot(plot_performance[,1:6], main="RMSE", col="red")

plot_performance <- read_excel("seine.xlsx", sheet = "mae", 
                               col_types = c("numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))
boxplot(plot_performance[,1:6], main="MAE", col="green")

plot_performance <- read_excel("seine.xlsx", sheet = "rpd", 
                               col_types = c("numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))
boxplot(plot_performance[,1:6], main="RPD", col="blue")
