######### 
# Naloufi Manel 
# version :  V3.5.1
#########

# Visualisation de la distribution des données de notre jeu de données. 
# Marne
smv <- read.csv("~/Bureau/BD/donnee+pluvio/smv-complet/donnee_brute_sans_na/smv.csv")
par(mfrow=c(2,5))
for (i in 5: length(smv)){
  boxplot(smv[,i], col="grey", ylab=colnames(smv)[i])
}
boxplot(log(smv$Ecoli), col="grey", ylab="Log (E.coli)")

# Seine
seine <- read.csv("/home/manel/Bureau/BD/donnee+pluvio/smv+vdp-pluvio/donnee-sans-na/smv-vdp/seine.csv")
par(mfrow=c(2,4))
for (i in 5: length(seine)){
  boxplot(seine[,i], col="grey", ylab=colnames(seine)[i])
}
boxplot(log(seine$Ecoli), col="grey", ylab="Log (E.coli)")

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
