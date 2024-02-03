create_models_basketball <- function(team_name,
                                     side = 'home',
                                     update_stats = F,
                                     method = "CSS-ML",
                                     lambda = NULL,
                                     st_name = NULL,
                                     ev_id = NULL,
                                     date = NULL,
                                     q_conf = 80,
                                     h = 1,
                                     a = .05,
                                     ...) {
  
  DT <- tab_for_an(side = side,
                   update_stats = update_stats,
                   team_name = team_name,
                   st_name = st_name,
                   ev_id = ev_id,
                   date = date,
                   part = 4,
                   sport = 3)
  
  dt_names <- colnames(DT$approximated_data)[-(1:2)]
  
  forecast_ts <- map(dt_names, \(i) {
    
    ts <- DT$approximated_data[[i]]
    
    adf1 <- adf.test(ts) |> suppressWarnings()
    
    adf2 <- adf.test(diff(ts)) |> suppressWarnings()
    
    if (adf1$p.value > a) {
      
      model_arima <- auto.arima(ts, d = 1, lambda = lambda, method = method, allowdrift = T, stationary = F) |> suppressWarnings()
      
      if (adf2$p.value <= adf1$p.value) {
        
        model_arima <- auto.arima(ts, d = 2, lambda = lambda, method = method, allowdrift = T, stationary = F) |> suppressWarnings()
        
      } else {
        
        model_arima
        
      }
      
    } else {
      
      model_arima <- auto.arima(ts, d = 0, lambda = lambda, method = method, allowdrift = T, stationary = F) |> suppressWarnings()
      
    }
    
    forecasts <- forecast(model_arima, h = h, level = q_conf, ...) |> suppressWarnings()
    
    if (any(forecasts$upper[, paste0(q_conf, '%')] > forecasts$mean * 3/2)) {
      
      forecasts$upper[, paste0(q_conf, '%')] <- 2 * forecasts$mean - forecasts$lower[, paste0(q_conf, '%')]
      
    } else {
      
      forecasts$upper[, paste0(q_conf, '%')] <- forecasts$upper[, paste0(q_conf, '%')]
      
    }
    
    if (any(forecasts$lower[, paste0(q_conf, '%')] > forecasts$mean)) {
      
      forecasts$lower[, paste0(q_conf, '%')] <- 0
      
    } else {
      
      forecasts$lower[, paste0(q_conf, '%')] <- forecasts$lower[, paste0(q_conf, '%')]
      
    }
    
    list('model_arima' = model_arima,
         'forecasts' = forecasts,
         'adf1' = adf1,
         'adf2' = adf2)
    
  }, .progress = T)
  
  names(forecast_ts) <- dt_names
  
  forecast_dt <- as.data.table(
    
    map(dt_names, \(i) {
      
      map_df(1:h, \(j) {
        
        data.table(
          
          list(
            
            forecast_ts[[i]]$forecasts$upper[j, paste0(q_conf, '%')],
            forecast_ts[[i]]$forecasts$mean[j],
            forecast_ts[[i]]$forecasts$lower[j, paste0(q_conf, '%')]
            
          )
          
        )
        
      })
      
    })
    
  )
  
  colnames(forecast_dt) <- dt_names
  
  if (side == 'home') {
    
    new_forecast_dt <- forecast_dt |>
      mutate_all(as.numeric) |>
      mutate(two_point_field_g._attempted = round(two_point_field_g._attempted, 0),
             two_point_field_goals = round(round(two_point_field_goals_made, 0) / round(two_point_field_g._attempted, 0), 2),
             two_point_field_goals_made = round(two_point_field_goals_made, 0),
             three_point_field_g._attempted = round(three_point_field_g._attempted, 0),
             three_point_field_goals = round(round(three_point_field_goals_made, 0) / round(three_point_field_g._attempted, 0), 2),
             three_point_field_goals_made = round(three_point_field_goals_made, 0),
             free_throws_attempted = round(free_throws_attempted, 0),
             free_throws = round(round(free_throws_made, 0) / round(free_throws_attempted, 0), 2),
             free_throws_made = round(free_throws_made, 0),
             offensive_rebounds = round(offensive_rebounds, 0),
             defensive_rebounds = round(defensive_rebounds, 0),
             blocks = round(blocks, 0),
             steals = round(steals, 0),
             assists = round(assists, 0),
             turnovers = round(turnovers, 0),
             personal_fouls = round(personal_fouls, 0),
             technical_fouls = round(technical_fouls, 0)) |>
      mutate(field_goals_attempted = two_point_field_g._attempted + three_point_field_g._attempted,
             field_goals = round(round(field_goals_made, 0) / round(field_goals_attempted, 0), 2),
             field_goals_made = round(two_point_field_goals_made, 0) + round(three_point_field_goals_made, 0),
             total_rebounds = round(offensive_rebounds, 0) + round(defensive_rebounds, 0),
             home_score_full = two_point_field_goals_made * 2 + three_point_field_goals_made * 3 + free_throws_made) %>%
      mutate_all(~ifelse(. < 0 | is.infinite(.), 0, .))
    
  } else {
    
    new_forecast_dt <- forecast_dt |>
      mutate_all(as.numeric) |>
      mutate(two_point_field_g._attempted = round(two_point_field_g._attempted, 0),
             two_point_field_goals = round(round(two_point_field_goals_made, 0) / round(two_point_field_g._attempted, 0), 2),
             two_point_field_goals_made = round(two_point_field_goals_made, 0),
             three_point_field_g._attempted = round(three_point_field_g._attempted, 0),
             three_point_field_goals = round(round(three_point_field_goals_made, 0) / round(three_point_field_g._attempted, 0), 2),
             three_point_field_goals_made = round(three_point_field_goals_made, 0),
             free_throws_attempted = round(free_throws_attempted, 0),
             free_throws = round(round(free_throws_made, 0) / round(free_throws_attempted, 0), 2),
             free_throws_made = round(free_throws_made, 0),
             offensive_rebounds = round(offensive_rebounds, 0),
             defensive_rebounds = round(defensive_rebounds, 0),
             blocks = round(blocks, 0),
             steals = round(steals, 0),
             assists = round(assists, 0),
             turnovers = round(turnovers, 0),
             personal_fouls = round(personal_fouls, 0),
             technical_fouls = round(technical_fouls, 0)) |>
      mutate(field_goals_attempted = two_point_field_g._attempted + three_point_field_g._attempted,
             field_goals = round(round(field_goals_made, 0) / round(field_goals_attempted, 0), 2),
             field_goals_made = round(two_point_field_goals_made, 0) + round(three_point_field_goals_made, 0),
             total_rebounds = round(offensive_rebounds, 0) + round(defensive_rebounds, 0),
             away_score_full = two_point_field_goals_made * 2 + three_point_field_goals_made * 3 + free_throws_made) %>%
      mutate_all(~ifelse(. < 0 | is.infinite(.), 0, .))
    
  }
  
  gc(reset = T, full = T)
  
  list('forecast_table' = new_forecast_dt,
       'forecast_data' = forecast_ts,
       'main_table' = DT)
  
}
