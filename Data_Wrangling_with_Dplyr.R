## MANIPULATION AVEC DES COMMANDES DE BASE R
# Apply(x, margin, fun) : margin 1 pour les lignes et 2 pour les colonnes, fun (mean, sum, summary, etc)
data(iris)
apply(iris[,-5],2,mean)
apply(iris[,-5],2,summary)

# Création Fonction count
nb_val_sup_5 <- function(vecteur){
  length(vecteur[vecteur>5])
}
apply(iris[,-5], 2, nb_val_sup_5)

### Faire des stats par groupe
# BY(x,indice,FUN)  ---> x le dataframe, l'indice est la colonne par laquelle on souhaite grouper
by(iris,iris$Species, summary)
# pour sortir les correlations
by(iris[,-5],iris$Species, cor)
# Désavantage de BY est qu'il travaille sur les dataframmes alors que aggregate travaille avec les vecteurs

# AGGREGATE
aggregate(iris[,-5],as.data.frame(iris$Species),mean)

### PACKAGE DPLYR
library(dplyr)
class(iris)
iris <- as_tibble(iris)

# Selectionner des colonnes
select(iris, Sepal.Length, Petal.Length, Species)
select(iris, Sepal.Length:Petal.Length)
select(iris, - Species)

select(iris, starts_with("Petal"))
select(iris, - starts_with("Sepal"))

select(iris, ends_with("Length"))
select(iris, contains("al"))

## Filter
filter(iris, Sepal.Length >= 5, Sepal.Width>=2)
filter(iris, between(Sepal.Length,4,7))

filter(iris, Species=="setosa")
filter(iris, Species !="setosa")

filter(iris, Species %in% c("setosa","versicolor"))
filter(iris, (Species == "setosa" | Species=="versicolor"))

# Filtrer sur toute une ligne, dc sur toute les variables
filter_all(iris[,-5], any_vars(. >5))
filter_all(iris[,-5], all_vars(. >2))

## Summarise
iris %>%
summarise_each(funs(mean,min),Petal.Length,Sepal.Length)
?summarise_at

