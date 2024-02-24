## Function for a creation of models of independent variables
creation_models_arima <- function(tbl,
                                  ts_vars,
                                  spline = F,
                                  sport = 1,
                                  a = .05,
                                  ...) {
  
  if (is.list(ts_vars)) {
    
    map_args <- ts_vars[[sport_list[id == sport, name]]]
    
  } else {
    
    map_args <- ts_vars
    
  }
  
  ## Creation ARIMA models for independent variables
  main_arima <- map(map_args[map_args %in% colnames(tbl)], \(i) {
    
    ts <- tbl[[i]]
    
    auto.arima(ts,
               allowdrift = T,
               allowmean = T,
               test.args = list(a = a),
               seasonal.test.args = list(a = a,
                                         max.D = 2),
               ...) |> suppressWarnings()
    
  })
  
  names(main_arima) <- map_args[map_args %in% colnames(tbl)]
  
  return(main_arima)
  
  gc(reset = T, full = T)
  
}

## Function for a creation of forecasting values of independent variables
forecasting_models_arima <- function(tbl,
                                     ts_vars,
                                     models,
                                     sport = 1,
                                     h = 15,
                                     ...) {
  
  if (is.list(ts_vars)) {
    
    map_args <- ts_vars[[sport_list[id == sport, name]]]
    
  } else {
    
    map_args <- ts_vars
    
  }
  
  ## Creation table of forecasting values for independent variables
  ### 1) lower boarder of confidence interval
  lower_values <- as.data.table(
    
    map(map_args[map_args %in% colnames(tbl)], \(i) {
      
      f <- forecast(models[[i]], 
                    h = h,
                    ...)
      
      if (any(as.numeric(f$lower[, 1]) <= 0 | is.na(as.numeric(f$lower[, 1])))) {
        
        0
        
      } else {
        
        if (i == 'expected_goals') {
          
          round(f$lower[, 1], 2)
          
        } else {
          
          round(f$lower[, 1], 0)
          
        }
        
      }
      
    })
    
  )
  
  ### 2) mean of confidence interval
  mean_values <- as.data.table(
    
    map(map_args[map_args %in% colnames(tbl)], \(i) {
      
      f <- forecast(models[[i]], 
                    h = h,
                    ...)
      
      if (any(as.numeric(f$mean) <= 0 | is.na(as.numeric(f$mean)) )) {
        
        0
        
      } else {
        
        if (i == 'expected_goals') {
          
          round(f$mean, 2)
          
        } else {
          
          round(f$mean, 0)
          
        }
        
      }
      
    })
    
  )
  
  ### 3) upper boarder of confidence interval
  upper_values <- as.data.table(
    
    map(map_args[map_args %in% colnames(tbl)], \(i) {
      
      f <- forecast(models[[i]], 
                    h = h,
                    ...)
      
      if (any(as.numeric(f$upper[, 1]) <= 0 | is.na(as.numeric(f$upper[, 1])) )) {
        
        0
        
      } else {
        
        if (i == 'expected_goals') {
          
          round(f$upper[, 1], 2)
          
        } else {
          
          round(f$upper[, 1], 0)
          
        }
        
      }
      
    })
    
  )
  
  colnames(lower_values) <- map_args[map_args %in% colnames(tbl)]
  colnames(mean_values) <- map_args[map_args %in% colnames(tbl)]
  colnames(upper_values) <- map_args[map_args %in% colnames(tbl)]
  
  return(list('lower_values' = lower_values,
              'mean_values' = mean_values,
              'upper_values' = upper_values))
  
  gc(reset = T, full = T)
  
}

## Function for a creation of models of independent variables
forecasting_models_rf <- function(tbl,
                                  indep_vars,
                                  dep_vars,
                                  forecast_models,
                                  side = 'home',
                                  level_value = 1,
                                  levels_factor = 2,
                                  sport = 1,
                                  ...) {
  
  if (is.list(dep_vars)) {
    
    map_args <- dep_vars[[sport_list[id == sport, name]]]
    
  } else {
    
    map_args <- dep_vars
    
  }
  
  terms <- dependent_vars(tbl = tbl, indep_vars = indep_vars, dep_vars = dep_vars, sport = sport)
  
  values <- c('lower_values', 'mean_values', 'upper_values')
  
  predict_table <- as.data.table(
    
    map(map_args, \(i) {
      
      if (length(levels(as.factor(tbl[[i]]))) < levels_factor) tbl[[i]] <- as.factor(tbl[[i]])
      
      rf_formula <- reformulate(termlabels = terms[[i]]$incidents, response = i, intercept = F)
      
      dep_vars_for_predict <- randomForest(rf_formula, data = tbl, ...) |> suppressWarnings()
      
      if (i == 'expected_goals') {
        
        round( predict(dep_vars_for_predict, forecast_models[[values[level_value]]]), 2)
        
      } else {
        
        if (is.factor(tbl[[i]])) {
          
          predict(dep_vars_for_predict, forecast_models[[values[level_value]]])
          
        } else {
          
          floor( predict(dep_vars_for_predict, forecast_models[[values[level_value]]]) )
          
        }
        
      }
      
    } )
    
  ) %>% rename_all(~ map_args)
  
  if (side == 'home') {
    
    cbind(forecast_models[[values[level_value]]], predict_table) %>%
      replace(. < 0, 0) %>%
      select(-away_score_full) %>%
      suppressWarnings()
    
  } else {
    
    cbind(forecast_models[[values[level_value]]], predict_table) %>%
      replace(. < 0, 0) %>%
      select(-home_score_full) %>%
      suppressWarnings()
    
  }
  
}
