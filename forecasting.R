create_models <- function(side = 'home',
                          update_stats = F,
                          team_name = NULL,
                          st_name = NULL,
                          ev_id = NULL,
                          date = NULL,
                          inc_name = NULL,
                          win_pts = 3,
                          p = .8,
                          a = .05,
                          env = parent.frame(),
                          ...) {
  
  if (class(try({
    
    tab_for_an(side = side,
               update_stats = update_stats,
               team_name = team_name,
               st_name = st_name,
               ev_id = ev_id,
               date = date,
               inc_name = inc_name,
               win_pts = win_pts,
               a = a,
               env = env)
    
  }, silent = T)) != 'try-error') {
    
    DT <- tab_for_an(side = side,
                     update_stats = update_stats,
                     team_name = team_name,
                     st_name = st_name,
                     ev_id = ev_id,
                     date = date,
                     inc_name = inc_name,
                     win_pts = win_pts,
                     a = a,
                     env = env)
    
    training.samples <- createDataPartition(DT$correlated_data[[inc_name]], p = p, list = F)
    
    train.data <- DT$correlated_data[training.samples]
    
    test.data <- DT$correlated_data[-training.samples]
    
    pcr <- pcr(DT$formula_without_intercept, data = train.data, scale = T)
    
    y_pcr <- predict(pcr, test.data)
    
    glm <- glm(DT$formula_without_intercept, data = train.data)
    
    y_glm <- predict(glm, test.data)
    
    rf <- randomForest(DT$formula_without_intercept, data = train.data)
    
    y_rf <- predict(rf, test.data)
    
    list('model_pcr' = list('train' = pcr,
                            'test' = y_pcr),
         'model_glm' = list('train' = glm,
                            'test' = y_glm),
         'model_rf' = list('train' = rf,
                           'test' = y_rf),
         'train_data' = train.data,
         'test_data' = test.data,
         'main_table' = DT$correlated_data,
         'formula_with_intercept' = DT$formula_with_intercept,
         'formula_without_intercept' = DT$formula_without_intercept)
    
  } else {
    
    print(str_glue('"{inc_name}" hasn`t got in table of statistics'))
    
  }
  
}
