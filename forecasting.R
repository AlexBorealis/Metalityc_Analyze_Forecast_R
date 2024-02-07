## Creation required variables for forecasting
team_name <- c('Phoenix Suns', 'Dallas Mavericks')

inc_names <- c('two_point_field_g._attempted', 'two_point_field_goals_made',
               'three_point_field_g._attempted', 'three_point_field_goals_made',
               'free_throws_attempted', 'free_throws_made')

sides <- c('home', 'away')

days_between_games_t1 <- c(15, 2, 5, 1, 11, 4, 2, 1, 4, 2)

days_between_games_t2 <- c(14, 5, 1, 2, 16, 2, 1, 2, 8, 2)

inc_name <- inc_names[1]

side <- sides[2]

## Function for a creation of models of independent variables
creation_models <- function(tbl,
                            names_of_vars,
                            indep_vars = T,
                            a = .05,
                            ...) {
  
  ## Creation table independent variables
  table_of_vars <- as.data.table(
    
    map(colnames(tbl$approximated_data), \(i) {
      
      create_ts(tbl = tbl, i, spline = F)
      
    })
    
  )
  
  colnames(table_of_vars) <- colnames(tbl$approximated_data)
  
  ## Creation table of Dicky-Fuller's tests
  main_adf <- map(names_of_vars, \(i) {
    
    ts <- table_of_vars[[i]]
    
    adf.test(ts) |> suppressWarnings()
    
  })
  
  names(main_adf) <- names_of_vars
  
  ## Creation of vectors of differentiation orders
  diff_order <- map_int(names_of_vars, \(i) {
    
    if (main_adf[[i]]$p.value < a) {
      
      d_order <- 0
      
    } else {
      
      d_order <- 1
      
    }
    
  })
  
  names(diff_order) <- names_of_vars
  
  ## Creation ARIMA models for independent variables
  main_arima <- map(names_of_vars, \(i) {
    
    ts <- table_of_vars[[i]]
    
    dep_vars <- dep_vars(tbl = tbl, names_of_vars = i)[[i]]
    
    if (isTRUE(indep_vars)) {
      
      auto.arima(ts,
                 d = diff_order[i],
                 lambda = 'auto',
                 allowdrift = T,
                 allowmean = T,
                 ...) |> suppressWarnings()
      
    } else {
      
      if (length(dep_vars) == 0) {
        
        auto.arima(ts,
                   d = diff_order[i],
                   lambda = 'auto',
                   allowdrift = T,
                   allowmean = T,
                   ...) |> suppressWarnings()
        
      } else {
        
        xreg <- as.matrix(table_of_vars |> select( all_of(dep_vars) ) )
        
        auto.arima(ts,
                   d = diff_order[i],
                   lambda = 'auto',
                   allowdrift = T,
                   allowmean = T,
                   xreg = xreg,
                   ...) |> suppressWarnings()
        
      }
      
    }
    
  })
  
  names(main_arima) <- names_of_vars
  
  table_main_adf <- t(as.data.table(main_adf))
  
  colnames(table_main_adf) <- c('statistic', 'lag.order', 'alternative', 'p.value', 'method', 'data.name')
  
  list('main_table_of_variables' = table_of_vars |> select(all_of(names_of_vars)),
       'main_table_of_Dicky_Fullers_test' = table_main_adf,
       'vector_of_differentiation_orders' = diff_order,
       'models_of_variables' = main_arima)
  
  
}

## Function for a creation of forecasting values of independent variables
forecasting_models <- function(tbl,
                               names_of_vars,
                               models,
                               h = 55,
                               indep_vars = T,
                               forecast_models1 = NULL,
                               forecast_models2 = NULL,
                               ...) {
  
  if (isTRUE(indep_vars)) {
    
    NULL
    
  } else {
    
    dep_vars <- dep_vars(tbl = tbl, names_of_vars = names_of_vars)
    
  }
  
  ## Creation table of forecasting values for independent variables
  ### 1) lower boarder of confidence interval
  lower_values <- as.data.table(
    
    map(names_of_vars, \(i) {
      
      if (isTRUE(indep_vars)) {
        
        f <- forecast(models$models_of_variables[[i]], 
                      h = h,
                      ...)
        
      } else {
        
        if (length(dep_vars[[i]]) == 0) {
          
          f <- forecast(dep_models$models_of_variables[[i]], 
                        h = h,
                        ...)
          
        } else {
          
          if (is.null(forecast_models2)) {
            
            xreg <- as.matrix( forecast_models1 |>
                                 select( dep_vars[[i]] ) )
            
          } else {
            
            xreg <- as.matrix( forecast_models1 |>
                                 cbind(forecast_models2) |>
                                 select( dep_vars[[i]] ) )
            
          }
          
          f <- forecast(dep_models$models_of_variables[[i]], 
                        h = h,
                        ...,
                        xreg = xreg)
          
        }
        
      }
      
      abs(round(f$lower[, 1], 0))
      
    })
    
  )
  
  ### 2) mean of confidence interval
  mean_values <- as.data.table(
    
    map(names_of_vars, \(i) {
      
      if (isTRUE(indep_vars)) {
        
        f <- forecast(models$models_of_variables[[i]], 
                      h = h,
                      ...)
        
      } else {
        
        if (length(dep_vars[[i]]) == 0) {
          
          f <- forecast(models$models_of_variables[[i]], 
                        h = h,
                        ...)
          
        } else {
          
          if (is.null(forecast_models2)) {
            
            xreg <- as.matrix( forecast_models1 |>
                                 select( dep_vars[[i]] ) )
            
          } else {
            
            xreg <- as.matrix( forecast_models1 |>
                                 cbind(forecast_models2) |>
                                 select( dep_vars[[i]] ) )
            
          }
          
          f <- forecast(models$models_of_variables[[i]], 
                        h = h,
                        xreg = xreg,
                        ...)
          
        }
        
      }
      
      abs(round(f$mean, 0))
      
    })
    
  )
  
  ### 3) upper boarder of confidence interval
  upper_values <- as.data.table(
    
    map(names_of_vars, \(i) {
      
      if (isTRUE(indep_vars)) {
        
        f <- forecast(models$models_of_variables[[i]], 
                      h = h,
                      ...)
        
      } else {
        
        if (length(dep_vars[[i]]) == 0) {
          
          f <- forecast(models$models_of_variables[[i]], 
                        h = h,
                        ...)
          
        } else {
          
          if (is.null(forecast_models2)) {
            
            xreg <- as.matrix( forecast_models1 |>
                                 select( dep_vars[[i]] ) )
            
          } else {
            
            xreg <- as.matrix( forecast_models1 |>
                                 cbind(forecast_models2) |>
                                 select( dep_vars[[i]] ) )
            
          }
          
          f <- forecast(models$models_of_variables[[i]], 
                        h = h,
                        xreg = xreg,
                        ...)
          
        }
        
      }
      
      abs(round(f$upper[, 1], 0))
      
    })
    
  )
  
  colnames(lower_values) <- names_of_vars
  colnames(mean_values) <- names_of_vars
  colnames(upper_values) <- names_of_vars
  
  list('lower_values' = lower_values,
       'mean_values' = mean_values,
       'upper_values' = upper_values)
  
}
