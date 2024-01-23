tab_for_an <- function(side = 'home',
                       update_stats = F,
                       team_name = NULL,
                       st_name = NULL,
                       ev_id = NULL,
                       date = NULL,
                       inc_name = NULL,
                       cor_level = 0.3,
                       env = parent.frame()) {
  
  events_id <- need_ids(date = date) |>
    group_by(event_id) |>
    filter(row_number() == 1) |>
    #group_by(tab_name, group_label) |>
    #filter(row_number() %in% 1:10) |>
    as.data.table()
  
  if (isTRUE(update_stats)) {
    
    fun_event_statistics_g(ev_id = events_id$event_id)
    
  } else {
    
    NULL
    
  }
  
  if (side == 'home') {
    
    stats <- need_stats(ev_id = ev_id,
                        st_name = st_name) |>
      select(-value_away) |>
      mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
               gsub(pattern = '_\\(xG\\)', replacement = ''))
    
    main_dt <- events_id[home_participant_name_one %ilike% team_name] |>
      mutate(h_result = ifelse(home_score_full > away_score_full, 'WIN', ifelse(home_score_full == away_score_full, 'DRAW', 'LOST'))) |>
      mutate(pts = ifelse(h_result == 'WIN', 3, ifelse(h_result == 'DRAW', 1, 0))) |>
      inner_join(stats, by = 'event_id') |>
      pivot_wider(names_from = incident_name,
                  values_from = value_home,
                  values_fn = c,
                  values_fill = NA) |>
      as.data.table() %>%
      .[order(start_time)]
    
  } else {
    
    stats <- need_stats(ev_id = ev_id,
                        st_name = st_name) |>
      select(-value_home) |>
      mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
               gsub(pattern = '_\\(xG\\)', replacement = ''))
    
    main_dt <- events_id[away_participant_name_one %ilike% team_name] |>
      mutate(h_result = ifelse(home_score_full < away_score_full, 'WIN', ifelse(home_score_full == away_score_full, 'DRAW', 'LOST'))) |>
      mutate(pts = ifelse(h_result == 'WIN', 3, ifelse(h_result == 'DRAW', 1, 0))) |>
      inner_join(stats, by = 'event_id') |>
      pivot_wider(names_from = incident_name,
                  values_from = value_away,
                  values_fn = c,
                  values_fill = NA) |>
      as.data.table() %>%
      .[order(start_time)]
    
  }
  
  if (!is_empty(main_dt$Red_Cards)) {
    
    main_dt_num <- main_dt |>
      select(-c(group_label:home_participant_name_one,
                current_result, away_participant_name_one,
                h_result, stage_name, Red_Cards)) |>
      mutate_if(is.character, as.numeric)
    
  } else {
    
    main_dt_num <- main_dt |>
      select(-c(group_label:home_participant_name_one,
                current_result, away_participant_name_one,
                h_result, stage_name)) |>
      mutate_if(is.character, as.numeric)
    
  }
  
  main_dt_no_na <- main_dt_num[, map(.SD, zoo::na.approx, n = nrow(main_dt_num)), .SDcols = names(main_dt_num)]
  
  cor <- round(cor(main_dt_no_na), 3) %>%
    ifelse(. != 1 & (. > cor_level | . < -cor_level), ., NA)
  
  if (!is.null(inc_name)) {
    
    dep_vars <- cor[, inc_name] %>% .[!is.na(.)] %>% names()
    
    list('real_stats' = main_dt,
         'correlation_matrix' = cor,
         'correlations' = cor[, inc_name] %>% .[!is.na(.)],
         'dependent_variables' = dep_vars,
         'formula_with_intercept' = reformulate(dep_vars, response = inc_name, env = env),
         'formula_without_intercept' = reformulate(dep_vars, response = inc_name, env = env, intercept = F),
         'repaired_data' =  main_dt_no_na,
         'correlated_data' =  main_dt_no_na |> select(all_of(dep_vars)))
    
  } else {
    
    dep_vars <- cor[, "pts"] %>% .[!is.na(.)] %>% names()
    
    list('real_stats' = main_dt,
         'correlation_matrix' = cor,
         'correlations' = cor[, "pts"] %>% .[!is.na(.)],
         'dependent_variables' = dep_vars,
         'formula_with_intercept' = reformulate(dep_vars, response = 'pts', env = env),
         'formula_without_intercept' = reformulate(dep_vars, response = 'pts', env = env, intercept = F),
         'repaired_data' =  main_dt_no_na,
         'correlated_data' =  main_dt_no_na |> select(all_of(dep_vars)))
    
  }
  
}

generate_stats <- function(tbl,
                           p = 0,
                           d = 0,
                           q = 0,
                           n = 17,
                           acc = 0.2,
                           inc_name) {
  
  repeat {
    
    model_arima <- arima(tbl$repaired_data[[inc_name]], order = c(p, d, q), include.mean = T)
    
    ar = model_arima$coef[grep(names(model_arima$coef), pattern = 'ar')]
    
    ma = model_arima$coef[grep(names(model_arima$coef), pattern = 'ma')]
    
    arima <- arima.sim(list(ar = ar, ma = ma),
                       sd = sd(tbl$repaired_data[[inc_name]]), 
                       mean = mean(tbl$repaired_data[[inc_name]]),
                       n = n)
    
    DT_arima <- data.table(start_time = tbl$real_stats$start_time,
                           real = tbl$repaired_data[[inc_name]],
                           arima = arima) |>
      mutate(delta2 = (real - arima)^2,
             ratio = abs((real - arima)/real)) |>
      mutate(arima = round(arima, 2))
    
    if (mean(DT_arima$ratio) < acc) break
    
  }
  
  return(list('model_arima' = model_arima,
              'table_values' = DT_arima))
  
}
