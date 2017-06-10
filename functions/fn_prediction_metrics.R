# -------------------------
# function to calculate and print metrics
# -------------------------
prediction_metrics <- function(predictions, yvalue) {
  #predictions6 <- factor(predictions6, levels = c("very_low", "Low", "Middle", "High"))
  #pred_num <- as.numeric(y)
  class_labels <- c('High','Low','Middle','Very Low')
  # Matriz de confusão das predições.
  conf_matrix <- table(predictions, yvalue)
  # Calculate Accuracy
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  print(paste('Accuracy:',round(accuracy,4)))
  # Calculate Precision
  precision <- diag(conf_matrix) / rowSums(conf_matrix)
  # Calculate Recall
  recall <- (diag(conf_matrix) / colSums(conf_matrix))
  f1 = 2 * precision * recall / (precision + recall) 
  accuracy <- round(accuracy,4)
  recall <- round(recall,4)
  f1 <- round(f1,4)
  metrics3 <- data.frame(precision, recall, f1) 
  print(metrics3)
  accuracy
}
