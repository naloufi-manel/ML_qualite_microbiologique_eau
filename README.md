# ML_qualite_microbiologique_eau
Évaluation des performances des approches d'apprentissage automatique pour prédire la qualité microbienne (Concentration en E.coli) des eaux de surface et optimiser l'effort d'échantillonnage.

Le répertoire contient neuf fichiers :

- ML_Marne_Ecoli_Prediction.ipynb : fichier Python avec les 6 modèles d'apprentissage automatique pour la prédiction de la concentration d'E. coli. en la Marne avec 11 paramétres.
Le fichier contient toutes les étapes de lecture des données, d'analyse descriptive, de division du jeu de données en formation et test, de normalisation des données et de formation et test des 6 modèles d'apprentissage automatique. Les données de test et les prédictions sont ensuite exportées. 
La division du jeu de données en formation et test est faite de manière aléatoire par train_test_split. Le paramètre random_state permet de contrôler la division, ainsi indiquer un nombre entier permet une division reproductible. 
Afin de tester plusieurs divisions, le script a été exécuté 10 fois en changeant à chaque fois la valeur entière du paramètre "random_state" de 0 à 9. 

- Deux fichiers avec les modéles d'apprentissage automatique : (ML_Marne2_Ecoli_Prediction.ipynb et ML_Seine_Ecoli_Prediction.ipynb) respectivement pour l'entrainement avec la Marne avec 8 paramétres et la Seine avec 8 paramétres.
Chaque fichier Python contient les 6 modèles d'apprentissage automatique pour la prédiction de la concentration d'E. coli. 
Le fichier contient toutes les étapes de lecture des données, d'analyse descriptive, de division du jeu de données en formation et test, de normalisation des données et de formation et test des 6 modèles d'apprentissage automatique. Pour chaque fichier, tous les modèles sont testé sur les données de la Seine et de la Marne. 
La division du jeu de données en formation et test est faite de manière aléatoire par train_test_split. Le paramètre random_state permet de contrôler la division, ainsi indiquer un nombre entier permet une division reproductible. Les fichiers contiennent une boucle qui permet d'exécuter le code 10 fois en changeant la valeur du random_state (de 0 à 9). Les données de test et les prédictions sont exportées au fur et à mesure. Les tableaux récapitulatifs des erreurs de performance sont également exportés après la boucle pour. 

- Script_Somme_pluvio.R : Fichier R pour l'analyse des données de pluviométries. À partir des données de chaque pluviométres, la somme des pluies journalieres a été calculer puis extraite pour les periodes correspondants au campagnes de prélevements. 

- ML-based_prediction_comparison.R : Fichier R pour l'analyse descriptive de notre jeu de données.
Visualisation par tracé des erreurs (RMSE, MAE et RDP) pour comparer les performances des 6 modèles d'apprentissage automatique. 

- Deux fichiers (script-Seine_analyse-correlation-rf.R et script-Marne_analyse-correlation-rf.R): Fichiers R pour l'analyse de la corrélation entre les valeurs prédites et les paramètres physico-chimiques et météorologiques respectivement pour la Seine et la Marne.
Détermination du nombre de prédictions raisonnables et de prédictions inexactes pour les 10 jeux de données de test aléatoires.
Analyse de la normalité des données par un test de shapiro, les résultats ont montré que les données n'avaient pas une distribution normale.
Une analyse de corrélation de spearman pour les estimations raisonnables et inexactes a donc été réalisée.
Visualisation des résultats de la corrélation par un correplot.
Analyse statistique pour comparer la corrélation entre les estimations raisonnables et inexactes afin d'identifier les paramètres à optimiser.


- Deux fichiers (script-Seine_identification-parametre_fit.R et script-Marne_identification-parametre_fit.R): fichiers R permettant d'identifier l'ensemble des valeurs permettant une estimation raisonnable respectivement pour les données de la Seine et de la Marne.
Regroupement des 10 ensembles de données de test aléatoires et analyse des résultats de prédiction du modèle basé sur la RF. 
Identification de l'ensemble des valeurs (paramètres physico-chimiques et météorologiques) qui permettent une estimation raisonnable à l'exception de celles qui donnent une estimation inexacte. 
Visualisation des résultats pour les paramètres à optimiser en fonction de la plage de valeurs de notre jeu de données. 
Analyse complémentaire des valeurs de notre jeu de données et des 10 jeux de données de test aléatoires. 
