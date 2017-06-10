# -------------------------
# RANDOM FOREST
# Variação do Bagging de árvores de decisão que reduz os atributos de criação de uma 
# árvore de decisão a uma amostra aleatória, a cada ponto de decisão.
#
# Sem visualização com gráfico!
# -------------------------
model_randomForest <- function(train, test, formula) {
  # Criando o modelo.
  fit_randomForest <- randomForest(formula, data=train)
  # Resumo.
  #summary(fit_randomForest)
  # Fazendo predições.
  predict(fit_randomForest, test)
}