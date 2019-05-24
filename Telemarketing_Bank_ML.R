# Lecture du fichier des données
bank_data <- read.csv2("bank.csv")
summary(bank_data)

## VISUALISATION DES DONNEES
library(ggplot2)
# Boxplot Y VS DURATION
g <- ggplot(bank_data,aes(x = y, y=duration, fill=y)) + geom_boxplot()
g
library(plotly)
ggplotly(g)
# Boxplot Y VS AGE
g <- ggplot(bank_data,aes(x = y, y=age, fill=y)) + geom_boxplot()
g
ggplotly(g)
# Barplot Y VS DURATION
g <- ggplot(bank_data,aes(x = y, fill=contact)) + geom_bar()
g
ggplotly(g)

## CREATION JEU DE TEST ET JEU D'ENTRAINEMENT
library(caret)
library(lattice)
#Transformer les variables catégorielles en numérique
dummy_variables <- dummyVars(~.,data = bank_data)
dummy_variables_data <- predict(dummy_variables,newdata = bank_data)
head(dummy_variables_data)

# Transformer la base "dummy_variables_data " en data frame
dummy_variables_data <- as.data.frame(dummy_variables_data)
# Recréer la variable catégorielle à prédire Y
dummy_variables_data$souscription <- ifelse(dummy_variables_data$y.no == 1, "NO","YES")
#Supprimer les variables y.no et y.yes
dummy_variables_data$y.no <- NULL
dummy_variables_data$y.yes <- NULL

# Fixer un paramètre pour être reproductible sur les analyses
set.seed(3033)

# Base Train et Base Test
training_size <- floor(0.7*nrow(dummy_variables_data))   # floor permet de tronquer un nombre à virgule en ne gardant que lentier
indices <- sample(seq_len(nrow(dummy_variables_data)), size = training_size)
data_bank.train <- dummy_variables_data[indices,]
data_bank.test <- dummy_variables_data[-indices,]

## NORMALISATION DES DONNEES 
data_preprocess_value <- preProcess(data_bank.train, method = c("center","scale"))   # Création des paramètres de normalisation sur le jeu d'entrainement uniquement. pas sur le jeu de test pour ne pas apporter de biais sur le jeu de test
data_bank.train.scaled <- predict(data_preprocess_value,data_bank.train)
data_bank.test.scaled <- predict(data_preprocess_value,data_bank.test)

## TRAITER LES DONNEES DESEQUILIBREES
# Caret : downsample (sous-échantillonage) et upsample (sur-échantillonnage)
table(data_bank.train.scaled$souscription)
# NO  YES 
# 2792  372     # le downsample va créer 372 YES et 372 NO. le upsample va créer 2792 NO et 2792 YES
set.seed(3033)
"%ni%" <- Negate("%in%")

# downsample
data_bank.train.scaled.downsample <- downSample(x=data_bank.train.scaled[,colnames(data_bank.train.scaled) %ni% "souscription"],y=as.factor(data_bank.train.scaled$souscription))
names(data_bank.train.scaled.downsample)[names(data_bank.train.scaled.downsample)=="Class"] <- "souscription"
table(data_bank.train.scaled.downsample$souscription)

# upsample
data_bank.train.scaled.upsample <- upSample(x=data_bank.train.scaled[,colnames(data_bank.train.scaled) %ni% "souscription"],y=as.factor(data_bank.train.scaled$souscription))
names(data_bank.train.scaled.upsample)[names(data_bank.train.scaled.upsample)=="Class"] <- "souscription"
table(data_bank.train.scaled.upsample$souscription)

## MODELISATION AVEC NAIVE BAYES DESEQUILIBREE
set.seed(3033)
# Cross validation method
traincontrol_data <- trainControl("repeatedcv", number = 10, repeats = 3)  # Le 10 indique qu'on va train sur 90% et tester sur 10%. puis on le repete 03 fois
naive_bayes_desequilibre <- train(souscription ~., data = data_bank.train.scaled,method="nb",preProcess=NULL)
print(naive_bayes_desequilibre)

# Prédiction avec notre modèle sur le jeu de données tests
prediction_naivebayes_desequilibre <- predict(naive_bayes_desequilibre, newdata = data_bank.test.scaled[,-ncol(data_bank.test.scaled)])

# Cration Matrix de Confusion
confusionMatrix(prediction_naivebayes_desequilibre, as.factor(data_bank.test.scaled$souscription))

## MODELISATION AVEC NAIVE BAYES DOWNSAMPLE
set.seed(3033)
# Cross validation method
traincontrol_data <- trainControl("repeatedcv", number = 10, repeats = 3)  # Le 10 indique qu'on va train sur 90% et tester sur 10%. puis on le repete 03 fois
naive_bayes_downsample <- train(souscription ~., data = data_bank.train.scaled.downsample,method="nb",preProcess=NULL)
print(naive_bayes_downsample)

# Prédiction avec notre modèle sur le jeu de données tests
prediction_naivebayes_downsample <- predict(naive_bayes_downsample, newdata = data_bank.test.scaled[,-ncol(data_bank.test.scaled)])

# Cration Matrix de Confusion
confusionMatrix(prediction_naivebayes_downsample, as.factor(data_bank.test.scaled$souscription))


## MODELISATION AVEC SVM
set.seed(3033)
# Cross validation method
traincontrol_data <- trainControl("repeatedcv", number = 10, repeats = 3)  # Le 10 indique qu'on va train sur 90% et tester sur 10%. puis on le repete 03 fois
svm_desequilibre <- train(souscription ~., data = data_bank.train.scaled,method="svmLinear",preProcess=NULL)
print(svm_desequilibre)

# Prédiction avec notre modèle sur le jeu de données tests
prediction_svm_desequilibre <- predict(svm_desequilibre, newdata = data_bank.test.scaled[,-ncol(data_bank.test.scaled)])

# Cration Matrix de Confusion
confusionMatrix(prediction_svm_desequilibre, as.factor(data_bank.test.scaled$souscription))


## TROUVER LES VARIABLES LES PLUS CONTRIBUTIVES DU MODELE
varImp(naive_bayes_downsample, scale = F)
