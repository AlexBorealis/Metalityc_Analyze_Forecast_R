forecast_model <- function(team_name,
                           st_name,
                           side = 'home',
                           prob = 0.7,
                           trees = 1000,
                           cor_level = 0.4,
                           a = 0.15,
                           forecast = T,
                           test_data = T, 
                           update_stats = F,
                           graphs = F, 
                           ev_id = NULL,
                           pars = NULL,
                           date = NULL,
                           inc_name = NULL) {
  
  DT <- tab_for_an(side = side,
                   team_name = team_name,
                   st_name = st_name,
                   date = date,
                   inc_name = inc_name,
                   cor_level = cor_level, 
                   update_stats = update_stats, 
                   ev_id = ev_id, 
                   forecast = T)
  
  training.samples <- createDataPartition(DT$forecast[[inc_name]], p = prob, list = F)
  
  train.data <- DT$forecast[training.samples]
  
  if (isTRUE(test_data)) {
    
    test.data <- DT$forecast[-training.samples]
    
  } else {
    
    if (any(class(pars) == 'data.frame')) {
      
      test.data <- pars
      
    } else {
      
      test.data <- as.data.table(t(as.matrix(round(pars, 2), ncol = 1)))
      
    }
    
  }
  
  model_lm <- lm(data = train.data, 
                 formula = DT$formula_with_intercept)
  model_lm1 <- lm(data = train.data, 
                  formula = DT$formula_without_intercept)
  model_rf <- randomForest(data = train.data, 
                           DT$formula_with_intercept,
                           ntree = trees,
                           importance = T)
  #model_arima <- auto.arima(train.data[[inc_name]])
  
  sum_lm <- summary(model_lm1)
  
  new_names <- names(which(sum_lm$coefficients[, "Pr(>|t|)"] < a))
  
  if (length(new_names) > 0) {
    
    new_formula <- reformulate(new_names, response = inc_name)
    new_formula_without_intercept <- reformulate(new_names, response = inc_name, intercept = F)
    
    model_lm <- lm(data = train.data, formula = new_formula)
    model_lm1 <- lm(data = train.data, formula = new_formula_without_intercept)
    model_rf <- randomForest(data = train.data, 
                             DT$formula_with_intercept,
                             ntree = trees,
                             importance = T)
    
  } else {
    
    NULL
    
  }
  
  y <- test.data[[inc_name]]
  y_lm <- predict(model_lm, test.data)
  y_lm1 <- predict(model_lm1, test.data)
  y_rf <- predict(model_rf, test.data)
  #y_arima <- forecast(model_arima, 3)
  y_ensemb <- map_dbl(1:nrow(test.data), \(i) mean(c(y_lm[i], y_lm1[i], y_rf[i]), na.rm = T))
  
  dt_result_train <- data.table(start_time = DT$real_stats[training.samples, start_time],
                                real = train.data[[inc_name]],
                                cur_result = DT$real_stats[training.samples, current_result],
                                h_result = DT$real_stats[training.samples, h_result],
                                lm = model_lm$fitted.values,
                                lm1 = model_lm1$fitted.values,
                                rf = model_rf$predicted) |>
    mutate(delta_lm = round((real - lm)/real * 100, 2),
           delta_lm1 = round((real - lm1)/real * 100, 2),
           delta_rf = round((real - rf)/real * 100, 2)) %>%
    mutate(ensemb = rowMeans(.[, 5:7])) |>
    mutate(delta_ensemb = round((real - ensemb)/real * 100, 2))
  
  dt_result_test <- data.table(start_time = DT$real_stats[-training.samples, start_time],
                               real = test.data[[inc_name]],
                               cur_result = DT$real_stats[-training.samples, current_result],
                               h_result = DT$real_stats[-training.samples, h_result],
                               lm = y_lm,
                               lm1 = y_lm1,
                               rf = y_rf) |>
    mutate(delta_lm = round((real - lm)/real * 100, 2),
           delta_lm1 = round((real - lm1)/real * 100, 2),
           delta_rf = round((real - rf)/real * 100, 2)) %>%
    mutate(ensemb = rowMeans(.[, 5:7])) |>
    mutate(delta_ensemb = round((real - ensemb)/real * 100, 2))
  
  list('result_train' = dt_result_train,
       'result_test' = dt_result_test,
       'results_lm_with_intercept' = model_lm,
       'results_lm_without_intercept' = model_lm1,
       'results_rf' = model_rf,
       'anova' = list('anova_lm' = anova(model_lm),
                      'anova_lm1' = anova(model_lm1)),
       'lm' = list('summary_lm' = summary(model_lm),
                   'summary_lm1' = summary(model_lm1)))
  
}
