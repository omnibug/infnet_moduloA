# Bloco: Business Intelligence e Big Data Analytics: Valor
# Disciplina: Big Data Analytics com R
# Prediction Phase

#clear environment
rm(list=ls())
#clear Console = ctrl+l


# -------------------------
# necessary packages
# -------------------------

# install.packages("party", dependencies=TRUE)
# install.packages("rpart", dependencies=TRUE)
# install.packages("rpart.plot", dependencies=TRUE)
# install.packages("rattle", dependencies=TRUE)
# install.packages("RWeka", dependencies=TRUE)
# install.packages("ipred", dependencies=TRUE)
# install.packages("randomForest", dependencies=TRUE)
# install.packages("C50", dependencies=TRUE)
# install.packages("RColorBrewer", dependencies=TRUE)

# loading packages
library(rpart) # decision trees
library(rpart.plot) # decision trees visualization
library(rattle)	# Visualization
library(party) # decision trees
library(dplyr) # bind rows
library(RWeka) # Algoritmos J48 e PART.
library(ipred) # Bagging.
library(randomForest) # Random Forest.
library(C50) # Algoritmo C5.0.
library(stringr) # string manipulation
require('RColorBrewer') # color paletes for graphs

# SET THE HOME DIR FOR WINDOWS OR LINUX ENVIRONMENTS
#homeDir='C:/Users/Carlos/Documents/MEGAsync/MIT/Modulo_A_Valor/Atual/infnet_moduloA/'
homeDir='/home/carlos/MEGAsync/MIT/Modulo_A_Valor/Atual/infnet_moduloA/' 
setwd(homeDir)

# loading common functions
source('functions/fn_load_data.R') # calculate prediction metrics
source('functions/fn_prediction_metrics.R') # calculate prediction metrics
source('functions/fn_model_randomForest.R') # train and run model randomForest

# functions used only in this script
openPNG <- function(imagename){
  png(filename = paste(getwd(),'/graphs/',imagename,'.png', sep = ''),
      width = 670, height = 379)
}


model_ctree <- function(train, test, formula) {
  # -------------------------
  # Decision trees
  # -------------------------

  # Train the model
  # Conditional Inference Trees {party}
  fit_ctree <- ctree(formula, data=train)
  
  # Confusion????? matrix to check the model's prediction capacity.
  #table(predict(fit_ctree), train$UNS)
  
  # Printing the model's rule and showing the tree
  # what is the p inside the tree node
  #print(fit_ctree)
  #plot(fit_ctree)
  
  # Anothe tree - simpler
  #plot(fit_ctree, type="simple")
  
  # Testando o modelo no conjunto de testes
  predict(fit_ctree, newdata = test)
  #table(testPred, test$UNS)
}

model_rpart <- function(train, test, formula) {
  # -------------------------
  # Train a model using rpart.
  # Recursive Partitioning and Regression Trees
  # -------------------------
  fit_rpart <- rpart(formula, data=train)
  # Resumo.
  #summary(fit_rpart)
  
  # Make predictions
  # The parameter "type='class'" is used for classification trees
  #predictions <- predict(fit_rpart, train[,1:5], type="class")
  
  # Confusion??? matrix for the model
  #table(predictions, train$UNS)
  
  # Make predictions
  # The parameter "type='class'" is used for classification trees
  predict(fit_rpart, test, type="class")
  
  # Confusion??? matrix for the predictions
  #table(predictions, test$UNS)
  
  # Printing the rules
  #print(fit_rpart)
  
  # Graph with 'rpart.plot'.
  #prp(fit_rpart)
  
  # Graph with 'rattle'.
  #fancyRpartPlot(fit_rpart)
}

model_J48 <- function(train, test, formula) {
  # -------------------------
  # ALGORITMO C4.5
  # Cria a ?rvore de forma a maximixar o ganho de informa??o (information gain)
  # (diferen?a de entropia). Chamado de J48 no Weka.
  # Entropia: quantidade necess?ria de informa??o para identificar a classe de um caso
  # Ganho de informa??o: ? a redu??o esperada da entropia ao utilizarmos um atributo na ?rvore.
  # Bom post explicativo (ingl?s): http://stackoverflow.com/questions/1859554/what-is-entropy-and-information-gain
  # -------------------------
  
  # Criando o modelo.
  fit_J48 <- J48(formula, data=train)
  # Resumo.
  #summary(fit_J48)
  # Fazendo predi??es.
  predict(fit_J48, test)
  # Matriz de confus?o das predi??es.
  #table(predictions, test$UNS)
  # Gr?fico da ?rvore.
  #plot(fit_J48)
}

model_PART <- function(train, test, formula) {
  # -------------------------
  # PART
  # Sistema de regras que cria ?rvores de decis?o C4.5 podadas e extrai regras, retirando
  # ent?o os dados podados do conjunto de treinamento. O processo ? repetido at? que 
  # todas as inst?ncias sejam cobertas pelas regras extra?das.
  # 
  # Sem visualiza??o com gr?fico!
  # -------------------------
  
  # Criando o modelo.
  fit_PART <- PART(formula, data=train)
  # Resumo.
  #summary(fit_PART)
  # Fazendo predi??es.
  predict(fit_PART, test)
  # Matriz de confus?o das predi??es.
  #table(predictions, test$UNS)
  #plot(fit_PART)
}

model_bagging <- function(train, test, formula) {
  # -------------------------
  # BAGGING CART
  # Bootstrapped Aggregation (Bagging) ? um m?todo ensemble, ou seja,
  # cria m?ltiplos modelos do mesmo tipo a partir de amostras diferentes 
  # do conjunto completo de dados. Os resultados de cada modelo separado 
  # s?o combinados para oferecer um resultado melhor.
  # Sem visualiza??o com gr?fico!
  # -------------------------
  # Criando o modelo.
  fit_bagging <- bagging(formula, data=train)
  # Resumo.
  #summary(fit_bagging)
  # Fazendo predi??es.
  predict(fit_bagging, test, type="class")
  # Matriz de confus?o das predi??es.
  #table(predictions4, test$UNS)
}

model_C5.0 <- function(train, test, formula) {
  # -------------------------
  # ALGORITMO C5.0
  # Evolu??o do algoritmo C4.5 que teve seu c?digo fonte liberado recentemente.
  # -------------------------
  # Criando o modelo.
  # p par?metro 'trials' indica o n?mero de itera??es desejado.
  fit_C5.0 <- C5.0(formula, data=train, trials=10)
  # Resumo.
  #print(fit_C5.0)
  # Fazendo predi??es.
  predict(fit_C5.0, test)
  # Gr?fico da ?rvore.
  #plot(fit_C5.0)
}

# -------------------------
# MAIN
# -------------------------

# load data sets
load_data()

# variables to store results
i <- 0
model_names <- c()
model_accuracies <- c()

# Setting seed for run
set.seed(12345)

# -------------------------
# Decision trees
# -------------------------
# Formula for ctree
# Specifying UNS as the dependent variable and the others are undependent 
#myFormulactree <- UNS ~ STG + SCG + STR + LPR + PEG
# Formula for dependent variable
# Specifying UNS as the dependent variable and the others are undependent 
myFormula <- UNS~.

# -------------------------
# Decision trees
# -------------------------
#run model - Calculate and print metrics
i <- i + 1
model_names[i] <- 'ctree'
model_accuracies[i] <- prediction_metrics(model_ctree(train, testx, myFormula), testy)
# -------------------------
# Train a model using rpart.
# Recursive Partitioning and Regression Trees
# -------------------------
#run model - Calculate and print metrics
i <- i + 1
model_names[i] <- 'rpart'
model_accuracies[i] <- prediction_metrics(model_rpart(train, testx, myFormula), testy)

# -------------------------
# ALGORITMO C4.5
# Cria a ?rvore de forma a maximixar o ganho de informa??o (information gain)
# -------------------------
#run model - Calculate and print metrics
i <- i + 1
model_names[i] <- 'J48'
model_accuracies[i] <- prediction_metrics(model_J48(train, testx, myFormula), testy)

# -------------------------
# PART
# Sistema de regras que cria ?rvores de decis?o C4.5 podadas e extrai regras
# -------------------------
#run model - Calculate and print metrics
i <- i + 1
model_names[i] <- 'PART'
model_accuracies[i] <- prediction_metrics(model_PART(train, testx, myFormula), testy)

# -------------------------
# BAGGING CART
# Bootstrapped Aggregation (Bagging)
# -------------------------
#run model - Calculate and print metrics
i <- i + 1
model_names[i] <- 'bagging'
model_accuracies[i] <- prediction_metrics(model_bagging(train, testx, myFormula), testy)

# -------------------------
# RANDOM FOREST
# Varia??o do Bagging de ?rvores de decisao
# -------------------------
#run model - Calculate and print metrics
i <- i + 1
model_names[i] <- 'randomForest'
model_accuracies[i] <- prediction_metrics(model_randomForest(train, testx, myFormula), testy)

# -------------------------
# ALGORITMO C5.0
# Evolu??o do algoritmo C4.5 que teve seu c?digo fonte liberado recentemente.
# -------------------------
#run model - Calculate and print metrics
i <- i + 1
model_names[i] <- 'C5.0'
model_accuracies[i] <- prediction_metrics(model_C5.0(train, testx, myFormula), testy)

result <- data.frame(model_names, model_accuracies)
result

fit_randomForest <- randomForest(myFormula, data=train, importance=TRUE)
yhat <- predict(fit_randomForest, testx)
ydiff <- as.character(yhat)==as.character(testy)
print(fit_randomForest)
print(fit_randomForest$importance)
yy <- data.frame(testx, testy, yhat, ydiff)

#yy
randomForestImportance <- fit_randomForest$importance
MeanDecreaseAccuracy <- randomForestImportance[1:5, 5]
MeanDecreaseGini <- randomForestImportance[1:5, 6]

# Opens the graphics file
openPNG('09_barplot_MeanDecreaseAccuracy')
barplot(MeanDecreaseAccuracy[order(MeanDecreaseAccuracy, decreasing = TRUE)],
        col = brewer.pal(5,'Accent'),
        main = 'Mean Decreased Accuracy')
dev.off()

# Opens the graphics file
openPNG('10_barplot_MeanDecreaseGini')
barplot(MeanDecreaseGini[order(MeanDecreaseGini, decreasing = TRUE)],
    col = brewer.pal(5,'Dark2'),
    main = 'Mean Decreased Gini')
dev.off()


# Setting seed for run
set.seed(12345)
myFormula <- UNS~PEG+LPR
prediction_metrics(model_randomForest(train, testx, myFormula), testy)
set.seed(12345)
fit_randomForest <- randomForest(myFormula, data=train, importance=TRUE)
set.seed(12345)
yhat <- predict(fit_randomForest, testx)
ydiff <- as.character(yhat)==as.character(testy)
print(fit_randomForest)
print(fit_randomForest$importance)

