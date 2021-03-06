# -------------------------
# RANDOM FOREST
# Varia��o do Bagging de �rvores de decis�o que reduz os atributos de cria��o de uma 
# �rvore de decis�o a uma amostra aleat�ria, a cada ponto de decis�o.
#
# Sem visualiza��o com gr�fico!
# -------------------------
model_randomForest <- function(train, test, formula) {
  # Criando o modelo.
  fit_randomForest <- randomForest(formula, data=train)
  # Resumo.
  #summary(fit_randomForest)
  # Fazendo predi��es.
  predict(fit_randomForest, test)
}