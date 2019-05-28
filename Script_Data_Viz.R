data(iris)

### GRAPHIQUE AVEC LES FONCTIONS DE BASE SOUS R
plot(iris$Sepal.Length,iris$Sepal.Width, xlab = "Longueur", ylab = "Largeur", col="red",
     xlim = c(min(iris$Sepal.Length,iris$Petal.Length),max(iris$Sepal.Length,iris$Petal.Length)),
     ylim = c(min(iris$Sepal.Width,iris$Petal.Width),max(iris$Sepal.Width,iris$Petal.Width)))
lines(iris$Petal.Length, iris$Petal.Width, col="darkblue", type = "p", pch=22)
title(main = "Longueur en fonction de largeur", col="blue")
?pch  # pour changer les formes des points
legend(1,4.2, c("Sépales","Pétales"),col = c("red","darkblue"), pch = 21:22)


### GRAPHIQUE AVEC LES FONCTIONS GGPLOT
library(ggplot2)
g <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width))
g <- g + geom_point()
g

# Colorer selon l'espace
g <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point()
g

# Couleurs et formes des poits différentes selon l'espèce
g <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species, shape=Species)) + 
  geom_point()
g

# Modifier la taille des points
g <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species, shape=Species)) + 
  geom_point(size=3)
g

# Créer un gradient de couleurs
g <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Petal.Length, shape=Species)) + 
  geom_point(size=3)
g <- g + scale_color_gradient(low = "blue",high = "red")
g

# Modifier le fond du graphique
g <- g + theme_dark()
g

# Modifier la position de la légende
g <- g + theme(legend.position = "top")
g

# Enlever le les textes (modalites de species) de la légence
g <- g + theme(legend.text = element_blank())
g

# Modifier couleur, texture du titre de la légende
g <- g + theme(legend.title = element_text(colour = "red", size = 9, face = "bold"))
g

# Modifier couleur, texture, teilles des labels de la légende
g <- g + theme(legend.text = element_text(colour = "blue", size = 5, face = "bold"))
g

# Ajouter un cadre à notre légende
g <- g + theme(legend.background = element_rect(fill = "grey", size=2, linetype = "dotted"))
g

# Ajouter des titres
g <- g + xlab("Longueur") + ylab("Largeur") + ggtitle("L en fonction de largeur")
g

# Modifier le format du titre
g <- g + theme(plot.title = element_text(colour = "steelblue", size = 15, face = "bold"))
g

# Modifier le format du titre des axes
g <- g + theme(axis.title = element_text(colour = "steelblue", size = 10, face = "bold"))
g

# Modifier le format des axes et inclinaisons
g <- g + theme(axis.text = element_text(colour = "steelblue", size = 10, face = "bold", angle = 45))
g

# Modifier la ligne des axes
g <- g + theme(axis.line = element_line(colour = "steelblue", linetype = "dotted"))
g

# Combiner plusieurs graphiques
 g <- g + facet_wrap(~Species) + theme(strip.text = element_text(colour = "steelblue", size = 10, face = "bold"))
g
g <- g + facet_wrap(~Species, ncol = 1)

# Ajouter des annotations
g <- g + annotate("text", x=c(2,4,6),y=0.7, label=c("setosa","Versicolor","Virginica"),
                  colour="steelblue", size=3, fontface="bold")
g

# Ajouter des rectangles sur les annotations
g <- g + annotate("rect", xmin = 0.5, xmax = 2.1, ymin = 0, ymax = 0.6, alpha=0.3, colour="steelblue",size=2)
g

# Ajouter une droite
g <- g + annotate("segment", x=0.5, xend = 4, y=1.5, yend = 0, colour="steelblue", size=1, alpha=0.3)
g

# Histogramme et enrégistrement des graphiques
png("Histogramme iris")
g <- ggplot(iris, aes(x=Petal.Length, fill=Species)) + 
  geom_histogram(color="white",binwidth = 0.5)
g
dev.off()

pdf("Histogramme pdf iris")
g <- ggplot(iris, aes(x=Petal.Length, fill=Species)) + 
  geom_histogram(color="white",binwidth = 0.5)
g
dev.off()

library(plotly)
ggplotly(g)
