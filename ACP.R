
library(FactoMineR)
library(factoextra)
library(readxl)

demoPCA <- read_excel("demoPCA.xlsx")
View(demoPCA)




par(mfrow=c(3,2))
hist(data$`Pop sup 65 ans`)


#2 
hist(data$'Population totale')#...
#3

#suppression de la colonne de type "chr"
data=demoPCA[,2:ncol(demoPCA)] 

res.pca=PCA(data,scale.unit = TRUE,graph = TRUE,ncp=5)
res.pca$eig


#les fonctions disponibles
print(res.pca)
res.pca$eig
#pour savoir le nombre de composantes principales PC
res.pca$eig #dans notre analyse, les 2 premières composantes expliquent 80% de la variation : c un % acceptable
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60)) #autre méthode

########cercle de corrélation
#les variables sont représentées sont par leur corrélation
#x est la corrélation d'une variable avec la 1ère PC
#la distance entre la variable et l'origine mesure sa qualité de représentation

res.pca$var$cos2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
#si cos2 d'une variable est =1 alors 1/elle est parfaitement représentée par ces deux PC(carte d'ACP)
                                     #2/elle est plus importante pour interpréter les PC en considération

# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

#########contribution des variables aux PC
res.pca$var$contrib
#Plus la valeur de la contribution est importante, plus la variable contribue à la composante principale en 
#question.
#les 2 commandes suivantes montrent le top 6 des variables contribuant aux PC

# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 2)
# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 2)

#contribution totale à PC1 ET PC2
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 6)
#contrib = [(C1 * Eig1) + (C2 * Eig2)]/(Eig1 + Eig2)
#avec : Eig1 et Eig2 sont les valeurs propres de PC1 et PC2

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

##########graphique des individus
plot(res.pca,choix='ind')
fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)
# Contribution totale sur PC1 et PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)
#####Biplot
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Couleur des variables
                col.ind = "#696969"  # Couleur des individues
)
#Globalement, un biplot peut être interprété comme suit: 
#un individu qui se trouve du même côté d'une variable donnée a une valeur élevée pour cette variable; 
#un individu qui se trouve sur le côté opposé d'une variable donnée a une faible valeur pour cette variable. 

####Elements supplementaires
#VOIR La partie "ES" dans sthda

res.pca <- PCA(data, ind.sup = 52:54, graph=TRUE)
p <- fviz_pca_ind(res.pca, col.ind.sup = "blue", repel = TRUE)
p
