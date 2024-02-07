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
                            a = .05,
                            ...) {
  
  ## Creation table independent variables
  table_of_vars <- as.data.table(
    
    map(names_of_vars, \(i) {
      
      create_ts(tbl = tbl, i, spline = F)
      
    })
    
  )
  
  colnames(table_of_vars) <- names_of_vars
  
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
    
    auto.arima(ts,
               d = diff_order[i],
               lambda = 'auto',
               allowdrift = T,
               allowmean = T,
               ...) |> suppressWarnings()
    
  })
  
  names(main_arima) <- names_of_vars
  
  table_main_adf <- t(as.data.table(main_adf))
  
  colnames(table_main_adf) <- c('statistic', 'lag.order', 'alternative', 'p.value', 'method', 'data.name')
  
  list('main_table_of_variables' = table_of_vars,
       'main_table_of_Dicky_Fullers_test' = table_main_adf,
       'vector_of_differentiation_orders' = diff_order,
       'models_of_variables' = main_arima)
  
  
}

## Function for a creation of forecasting values of independent variables
forecasting_models <- function(tbl,
                               days,
                               names_of_vars,
                               models,
                               conf_int,
                               period = NULL,
                               indep_vars = T,
                               ...) {
  
  ## Period of forecasting
  if (isTRUE(indep_vars)) {
    
    period <- cumsum(days) %>% .[length(.)]
    
  } else {
    
    period <- period
    
  }
  
  ## Creation table of forecasting values for independent variables
  ### lower - lower boarder of significant interval
  lower_forecast_values <- as.data.table(
    
    map(names_of_vars, \(i) {
      
      f <- forecast(models$models_of_variables[[i]], 
                    h = period,
                    ...)
      
      if (isTRUE(indep_vars)) {
        
        round(f$lower, 0)[, paste0(conf_int, '%')][days]
        
      } else {
        
        round(f$lower, 0)[, paste0(conf_int, '%')]
        
      }
      
    })
    
  )
  
  
  ### mean - mean value
  mean_forecast_values <- as.data.table(
    
    map(names_of_vars, \(i) {
      
      f <- forecast(models$models_of_variables[[i]], 
                    h = period,
                    ...)
      
      if (isTRUE(indep_vars)) {
        
        round(f$mean, 0)[days]
        
      } else {
        
        round(f$mean, 0)
        
      }
      
    })
    
  )
  
  
  ### upper - upper boarder of significant interval
  upper_forecast_values <- as.data.table(
    
    map(names_of_vars, \(i) {
      
      f <- forecast(models$models_of_variables[[i]],
                    h = period,
                    ...)
      
      if (isTRUE(indep_vars)) {
        
        round(f$upper, 0)[, paste0(conf_int, '%')][days]
        
      } else {
        
        round(f$upper, 0)[, paste0(conf_int, '%')]
        
      }
      
    })
    
  )
  
  colnames(lower_forecast_values) <- names_of_vars
  colnames(mean_forecast_values) <- names_of_vars
  colnames(upper_forecast_values) <- names_of_vars
  
  list('lower_forecast_values' = lower_forecast_values,
       'mean_forecast_values' = mean_forecast_values,
       'upper_forecast_values' = upper_forecast_values)
  
}

## Testing forecasting variables
models <- creation_models(tbl = DT,
                          names_of_vars = DT$independent_variables)

forecast_models <- forecasting_models(tbl = DT,
                                      days = days_between_games_t2,
                                      names_of_vars = DT$independent_variables,
                                      models = models,
                                      level = c(80, 95),
                                      conf_int = 95)

## Forecasting variables
dep_models <- creation_models(tbl = DT,
                              names_of_vars = inc_names,
                              a = .01)

plot(dep_models$main_table_of_variables$free_throws_made, type = 'b', col = 'green')
lines(dep_models$models_of_variables$free_throws_made$fitted, col = 'red')

R2(dep_models$models_of_variables$free_throws_made$fitted,
   dep_models$main_table_of_variables$free_throws_made)

forecast_dep_models <- forecasting_models(tbl = DT,
                                          models = dep_models,
                                          names_of_vars = inc_names,
                                          conf_int = 95,
                                          level = c(80, 95),
                                          indep_vars = F,
                                          period = 3)
