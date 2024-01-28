create_models <- function(side = 'home',
                          method = 'rf',
                          update_stats = F,
                          team_name = NULL,
                          st_name = NULL,
                          ev_id = NULL,
                          date = NULL,
                          inc_name = NULL,
                          part = 2,
                          win_pts = 3,
                          p = .7,
                          a = .05,
                          b = .05,
                          env = parent.frame(),
                          ...) {
  
  DT <- tab_for_an(side = side,
                   update_stats = update_stats,
                   team_name = team_name,
                   st_name = st_name,
                   ev_id = ev_id,
                   date = date,
                   inc_name = inc_name,
                   part = part,
                   win_pts = win_pts,
                   a = a,
                   b = b,
                   env = env)
  
  if (!is.null(DT$approximated_data)) {
    
    if (!is.null(inc_name)) {
      
      training.samples <- createDataPartition(DT$approximated_data[[inc_name]], p = p, list = FALSE)
      
    } else {
      
      training.samples <- createDataPartition(DT$approximated_data$pts, p = p, list = FALSE)
      
    }
    
    train.data <- DT$approximated_data[training.samples]
    
    test.data <- DT$approximated_data[-training.samples]
    
    if (method == 'rf') {
      
      model_with_intercept <- randomForest(DT$formula_with_intercept, data = train.data, ...)
      
      model_without_intercept <- randomForest(DT$formula_without_intercept, data = train.data, ...)
      
      y_with_intercept <- predict(model_with_intercept, test.data)
      
      y_without_intercept <- predict(model_without_intercept, test.data)
      
    } else 
      if (method == 'pcr') {
        
        train.data <- train.data |>
          mutate_if(is.factor, as.numeric) |>
          mutate(home_score_full = home_score_full / part,
                 away_score_full = away_score_full / part,
                 pts = pts / part)
        
        test.data <- test.data |>
          mutate_if(is.factor, as.numeric) |>
          mutate(home_score_full = home_score_full / part,
                 away_score_full = away_score_full / part,
                 pts = pts / part)
        
        model_with_intercept <- pcr(DT$formula_with_intercept, data = train.data, ...)
        
        model_without_intercept <- pcr(DT$formula_without_intercept, data = train.data, ...)
        
        y_with_intercept <- predict(model_with_intercept, test.data)
        
        y_without_intercept <- predict(model_without_intercept, test.data)
        
      } else {
        
        train.data <- train.data |>
          mutate_if(is.factor, as.numeric) |>
          mutate(home_score_full = home_score_full / part,
                 away_score_full = away_score_full / part,
                 pts = pts / part)
        
        test.data <- test.data |>
          mutate_if(is.factor, as.numeric) |>
          mutate(home_score_full = home_score_full / part,
                 away_score_full = away_score_full / part,
                 pts = pts / part)
        
        model_with_intercept <- glm(DT$formula_with_intercept, data = train.data, ...)
        
        model_without_intercept <- NULL
        
        y_with_intercept <- predict(model_with_intercept, test.data)
        
        y_without_intercept <- NULL
        
      }
    
    list('model_with_intercept' = list('train_with_intercept' = model_with_intercept,
                                       'test_with_intercept' = y_with_intercept),
         'model_without_intercept' = list('train_without_intercept' = model_without_intercept,
                                          'test_without_intercept' = y_without_intercept),
         'train_data' = train.data,
         'test_data' = test.data,
         'table_resp_depvars' = DT$correlated_data,
         'main_table' = DT$real_stats,
         'formula_with_intercept' = DT$formula_with_intercept,
         'formula_without_intercept' = DT$formula_without_intercept)
    
  } else {
    
    list('result' = DT,
         'message' = str_glue('"{inc_name}" hasn`t got in table of statistics'))
    
  }
  
}
