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
                           method,
                           control_method = 'LOOCV',
                           inc_name = NULL,
                           ...) {
  
  if (!is.null(models$formula_with_intercept)) {
    
    train_control <- trainControl(method = control_method)
    
    model_with_intercept <- train(form = models$formula_with_intercept, 
                                  data = models$train_data, 
                                  method = method,
                                  trControl = train_control)
    
    model_with_intercept <- train(form = models$formula_without_intercept, 
                                  data = models$train_data, 
                                  method = method,
                                  trControl = train_control)
    
    list('model_with_intercept' = model_with_intercept,
         'model_without_intercept' = model_without_intercept)
    
  } else {
    
    models
    
  }
  
}
