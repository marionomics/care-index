poblacion$edad1a5 <- ifelse(poblacion$edad %in% 1:5, 1, 0)
poblacion$edad6a14 <- ifelse(poblacion$edad %in% 6:14, 1, 0)
poblacion$edad15a18 <- ifelse(poblacion$edad %in% 15:18, 1, 0)
poblacion$edad65mas <- ifelse(poblacion$edad %in% 65:max(poblacion$edad), 1, 0)


poblacion[,(ncol(poblacion) - 4):ncol(poblacion)] %>%
  colSums()/nrow(poblacion)
