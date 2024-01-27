heatmap_cor <- function(m,
                        size = 10,
                        main = 'Correlation Heatmap',
                        ...) {
  
  pheatmap(
    
    m,
    breaks = seq(-1, 1, .02),
    cluster_rows = F,
    cluster_cols = F,
    fontsize_number = size,
    display_numbers = T,
    number_format = '%0.2f',
    main = main,
    ...
    
  )
  
}

analyze_models <- function(models,
                           control_method = 'LOOCV',
                           ...) {
  
  train_control <- trainControl(method = control_method)
  
  rf_with_intercept <- train(models$formula_with_intercept, 
                             data = models$train_data, 
                             method = 'rf',
                             trControl = train_control)
  
  rf_without_intercept <- train(models$formula_without_intercept, 
                                data = models$train_data, 
                                method = 'rf',
                                trControl = train_control)
  
  glm_with_intercept <- train(models$formula_with_intercept, 
                              data = models$train_data, 
                              method = 'glm',
                              trControl = train_control)
  
  glm_without_intercept <- train(models$formula_without_intercept, 
                                 data = models$train_data, 
                                 method = 'glm',
                                 trControl = train_control)
  
  pcr_with_intercept <- train(models$formula_with_intercept, 
                              data = models$train_data, 
                              method = 'pcr',
                              trControl = train_control)
  
  pcr_without_intercept <- train(models$formula_without_intercept, 
                                 data = models$train_data, 
                                 method = 'pcr',
                                 trControl = train_control)
  
  list('rf_with_intercept' = rf_with_intercept,
       'rf_without_intercept' = rf_without_intercept,
       'glm_with_intercept' = glm_with_intercept,
       'glm_without_intercept' = glm_without_intercept,
       'pcr_with_intercept' = pcr_with_intercept,
       'pcr_without_intercept' = pcr_without_intercept)
  
}
