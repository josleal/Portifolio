# ACURÁCIA, PRECISÃO, RECALL, F1_SCORE

acuracia <- function(matriz_confusao){
  acuracia <- (matriz_confusao[1,1] + matriz_confusao[2,2])/(matriz_confusao[1,1] + matriz_confusao[2,2] + 
                                                               matriz_confusao[1,2] + matriz_confusao[2,1]) 
}  

precisao <- function(matriz_confusao){   
  precisao <- matriz_confusao[1,1]/(matriz_confusao[1,1] + matriz_confusao[1,2])
}

recall <- function(matriz_confusao){
  recall <- matriz_confusao[1,1]/(matriz_confusao[1,1] + matriz_confusao[2,1])
}

f1_score <- function(matriz_confusao){    
  f1_score <- 2 * (matriz_confusao[1,1]/
                     (matriz_confusao[1,1] + matriz_confusao[1,2])) * 
    (matriz_confusao[1,1]/(matriz_confusao[1,1] + matriz_confusao[2,1]))/
    ((matriz_confusao[1,1]/(matriz_confusao[1,1] +  matriz_confusao[1,2])) + 
       (matriz_confusao[1,1]/(matriz_confusao[1,1] + matriz_confusao[2,1])))
}


# RELATÓRIO DAS MÉTRICAS

metricas_class <- function(matriz_confusao, titulo){
  
  acuracia <- (matriz_confusao[1,1] + matriz_confusao[2,2])/(matriz_confusao[1,1] + matriz_confusao[2,2] + 
                                                               matriz_confusao[1,2] + matriz_confusao[2,1])
  precisao <- matriz_confusao[1,1]/(matriz_confusao[1,1] + matriz_confusao[1,2])
  recall <- matriz_confusao[1,1]/(matriz_confusao[1,1] + matriz_confusao[2,1])
  f1_score <- 2 * precisao * recall/(precisao + recall)
  
  return (
    cat(titulo,
        "\n",
        "\nAcuracia   =", round(acuracia,4),
        "\nRecall     =", round(recall,4),
        "\nPrecisao   =", round(precisao,4),
        "\nF1 Score   =", round(f1_score,4)))
}






