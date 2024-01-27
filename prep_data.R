tab_for_an <- function(side = 'home',
                       update_stats = F,
                       team_name = NULL,
                       st_name = NULL,
                       ev_id = NULL,
                       date = NULL,
                       inc_name = NULL,
                       win_pts = 3,
                       na_ratio = .8,
                       probs = .15,
                       a = .05,
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
      mutate(h_result = ifelse(home_score_full > away_score_full, 'WIN', 
                               ifelse(home_score_full == away_score_full, 'DRAW', 'LOST'))) |>
      mutate(pts = ifelse(h_result == 'WIN', win_pts, ifelse(h_result == 'DRAW', 1, 0))) |>
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
      mutate(h_result = ifelse(home_score_full < away_score_full, 'WIN', 
                               ifelse(home_score_full == away_score_full, 'DRAW', 'LOST'))) |>
      mutate(pts = ifelse(h_result == 'WIN', win_pts, ifelse(h_result == 'DRAW', 1, 0))) |>
      inner_join(stats, by = 'event_id') |>
      pivot_wider(names_from = incident_name,
                  values_from = value_away,
                  values_fn = c,
                  values_fill = NA) |>
      as.data.table() %>%
      .[order(start_time)]
    
  }
  
  main_dt_num <- main_dt |>
    select(-c(group_label:home_participant_name_one,
              current_result, away_participant_name_one,
              h_result, stage_name)) |>
    mutate_if(is.character, as.numeric) %>%
    .[, map(.SD, zoo::na.approx, na.rm = F), .SDcols = colnames(.)]
  
  interpolate_lst <- map(colnames(main_dt_num), \(i) {
    
    ts <- main_dt_num[[i]]
    
    length_ts <- length(ts[!is.na(ts)])
    
    n <- nrow(main_dt_num)
    
    if (length_ts > n * na_ratio) {
      
      ts
      
    } else {
      
      rpois(n = n, lambda = quantile(ts, probs = probs, na.rm = T))
      
    }
    
  })
  
  names(interpolate_lst) <- colnames(main_dt_num)
  
  main_dt_no_na <- as.data.table(interpolate_lst) |> na.omit()
  
  rcor <- Hmisc::rcorr(as.matrix(main_dt_no_na))
    
  cor <- ifelse(rcor$P < a, round(rcor$r, 3), NA)
  
  if (!is.null(inc_name)) {
    
    dep_vars <- cor[, inc_name] %>% .[!is.na(.)] %>% names()
    
    list('real_stats' = main_dt,
         'correlation_matrix' = cor,
         'correlations' = cor[, inc_name] %>% .[!is.na(.)],
         'dependent_variables' = dep_vars,
         'formula_with_intercept' = reformulate(dep_vars, response = inc_name, env = env),
         'formula_without_intercept' = reformulate(dep_vars, response = inc_name, env = env, intercept = F),
         'approximated_data' =  main_dt_no_na,
         'correlated_data' =  main_dt_no_na |> select(all_of(dep_vars)))
    
  } else {
    
    dep_vars <- cor[, "pts"] %>% .[!is.na(.)] %>% names()
    
    list('real_stats' = main_dt,
         'correlation_matrix' = cor,
         'correlations' = cor[, "pts"] %>% .[!is.na(.)],
         'dependent_variables' = dep_vars,
         'formula_with_intercept' = reformulate(dep_vars, response = 'pts', env = env),
         'formula_without_intercept' = reformulate(dep_vars, response = 'pts', env = env, intercept = F),
         'approximated_data' =  main_dt_no_na,
         'correlated_data' =  main_dt_no_na |> select(all_of(dep_vars)))
    
  }
  
}

test_data <- function(side = 'home',
                      update_stats = F,
                      team_name = NULL,
                      st_name = NULL,
                      ev_id = NULL,
                      date = NULL,
                      inc_name = NULL,
                      win_pts = 3,
                      cor_level = .3,
                      na_ratio = .8,
                      probs = .15,
                      env = parent.frame()) {
  
  DT <- tab_for_an(side = side,
                   team_name = team_name,
                   st_name = st_name,
                   date = date,
                   inc_name = inc_name,
                   cor_level = cor_level, 
                   update_stats = update_stats, 
                   ev_id = ev_id)
  
  
  
}