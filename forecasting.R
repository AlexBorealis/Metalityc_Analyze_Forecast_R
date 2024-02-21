## Function for a creation of models of independent variables
creation_models <- function(tbl,
                            names_of_vars,
                            indep_vars = NULL,
                            bool_indep_vars = T,
                            spline = F,
                            sport = 1,
                            a = .05,
                            ...) {
  
  if (is.list(names_of_vars)) {
    
    map_args <- names_of_vars[[sport_list[id == sport, name]]]
    
  } else {
    
    map_args <- names_of_vars
    
  }
  
  ## Creation ARIMA models for independent variables
  main_arima <- map(map_args[map_args %in% colnames(tbl)], \(i) {
    
    ts <- tbl[[i]]
    
    if (isTRUE(bool_indep_vars)) {
      
      auto.arima(ts,
                 allowdrift = T,
                 allowmean = T,
                 test.args = list(a = a),
                 seasonal.test.args = list(a = a,
                                           max.D = 2),
                 ...) |> suppressWarnings()
      
    } else {
      
      dep_vars <- dependent_vars(tbl = tbl, indep_vars = indep_vars, sport = sport, dep_vars = i)[[i]]$incidents
      
      if (length(dep_vars) == 0) {
        
        auto.arima(ts,
                   allowdrift = T,
                   allowmean = T,
                   test.args = list(a = a,
                                    max.D = 2),
                   seasonal.test.args = list(a = a),
                   ...) |> suppressWarnings()
        
      } else {
        
        xreg <- as.matrix(tbl |> select( all_of(dep_vars) ) )
        
        auto.arima(ts,
                   allowdrift = T,
                   allowmean = T,
                   test.args = list(a = a),
                   seasonal.test.args = list(a = a,
                                             max.D = 2),
                   xreg = xreg,
                   ...) |> suppressWarnings()
        
      }
      
    }
    
  })
  
  names(main_arima) <- map_args[map_args %in% colnames(tbl)]
  
  return(list('models_of_variables' = main_arima))
  
  gc(reset = T, full = T)
  
}

## Function for a creation of forecasting values of independent variables
forecasting_models <- function(tbl,
                               names_of_vars,
                               models,
                               sport = 1,
                               h = 15,
                               bool_indep_vars = T,
                               indep_vars = NULL,
                               forecast_models1 = NULL,
                               forecast_models2 = NULL,
                               ...) {
  
  if (is.list(names_of_vars)) {
    
    map_args <- names_of_vars[[sport_list[id == sport, name]]]
    
  } else {
    
    map_args <- names_of_vars
    
  }
  
  ## Creation table of forecasting values for independent variables
  ### 1) lower boarder of confidence interval
  lower_values <- as.data.table(
    
    map(map_args[map_args %in% colnames(tbl)], \(i) {
      
      if (isTRUE(bool_indep_vars)) {
        
        f <- forecast(models$models_of_variables[[i]], 
                      h = h,
                      ...)
        
      } else {
        
        dep_vars <- dependent_vars(tbl = tbl, indep_vars = indep_vars, sport = sport, dep_vars = i)[[i]]$incidents
        
        if (length(dep_vars) == 0) {
          
          f <- forecast(models$models_of_variables[[i]], 
                        h = h,
                        ...)
          
        } else {
          
          if (is.null(forecast_models2)) {
            
            xreg <- as.matrix( forecast_models1 |>
                                 select( all_of(dep_vars) ) )
            
          } else {
            
            xreg <- as.matrix( forecast_models1 |>
                                 cbind(forecast_models2) |>
                                 select( all_of(dep_vars) ) )
            
          }
          
          f <- forecast(models$models_of_variables[[i]], 
                        h = h,
                        ...,
                        xreg = xreg)
          
        }
        
      }
      
      if (any(as.numeric(f$lower[, 1]) <= 0)) {
        
        0
        
      } else {
        
        round(f$lower[, 1], 0)
        
      }
      
    })
    
  )
  
  ### 2) mean of confidence interval
  mean_values <- as.data.table(
    
    map(map_args[map_args %in% colnames(tbl)], \(i) {
      
      if (isTRUE(bool_indep_vars)) {
        
        f <- forecast(models$models_of_variables[[i]], 
                      h = h,
                      ...)
        
      } else {
        
        dep_vars <- dependent_vars(tbl = tbl, indep_vars = indep_vars, sport = sport, dep_vars = i)[[i]]$incidents
        
        if (length(dep_vars) == 0) {
          
          f <- forecast(models$models_of_variables[[i]], 
                        h = h,
                        ...)
          
        } else {
          
          if (is.null(forecast_models2)) {
            
            xreg <- as.matrix( forecast_models1 |>
                                 select( all_of(dep_vars) ) )
            
          } else {
            
            xreg <- as.matrix( forecast_models1 |>
                                 cbind(forecast_models2) |>
                                 select( all_of(dep_vars) ) )
            
          }
          
          f <- forecast(models$models_of_variables[[i]], 
                        h = h,
                        xreg = xreg,
                        ...)
          
        }
        
      }
      
      if (any(as.numeric(f$mean) <= 0)) {
        
        0
        
      } else {
        
        round(f$mean, 0)
        
      }
      
    })
    
  )
  
  ### 3) upper boarder of confidence interval
  upper_values <- as.data.table(
    
    map(map_args[map_args %in% colnames(tbl)], \(i) {
      
      if (isTRUE(bool_indep_vars)) {
        
        f <- forecast(models$models_of_variables[[i]], 
                      h = h,
                      ...)
        
      } else {
        
        dep_vars <- dependent_vars(tbl = tbl, indep_vars = indep_vars, sport = sport, dep_vars = i)[[i]]$incidents
        
        if (length(dep_vars) == 0) {
          
          f <- forecast(models$models_of_variables[[i]], 
                        h = h,
                        ...)
          
        } else {
          
          if (is.null(forecast_models2)) {
            
            xreg <- as.matrix( forecast_models1 |>
                                 select( dep_vars ) )
            
          } else {
            
            xreg <- as.matrix( forecast_models1 |>
                                 cbind(forecast_models2) |>
                                 select( dep_vars ) )
            
          }
          
          f <- forecast(models$models_of_variables[[i]], 
                        h = h,
                        xreg = xreg,
                        ...)
          
        }
        
      }
      
      if (any(as.numeric(f$upper[, 1]) <= 0)) {
        
        0
        
      } else {
        
        round(f$upper[, 1], 0)
        
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

## Function for a creation models for a dependent variables of 2nd level
getting_scores <- function(tbl,
                           indep_vars,
                           dep_vars,
                           forecast_models,
                           level_value = 1,
                           sport = 1) {
  
  if (sport %in% c(1, 4)) {
    
    dep_vars_for_predict <- dependent_vars(tbl = tbl, dep_vars = dep_vars, indep_vars = indep_vars, sport = sport)
    
    values <- c('lower_values', 'mean_values', 'upper_values')
    
    predict_table <- as.data.table(
      
      map(names(dep_vars_for_predict), \(i) floor( predict(dep_vars_for_predict[[i]]$model, 
                                                           forecast_models[[values[level_value]]]) ) )
      
    ) |> rename_all(~ names(dep_vars_for_predict))
    
    cbind(forecast_models[[values[level_value]]], predict_table) %>% replace(. < 0, 0)
    
  }
  
}
