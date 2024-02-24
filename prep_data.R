tab_for_an <- function(side = 'home',
                       update_stats = F,
                       team_name = NULL,
                       st_name = NULL,
                       ev_id = NULL,
                       date = NULL,
                       part = 2,
                       win_pts = 3,
                       sport = 1) {
  
  # Getting table of event_id for a sending requests for a getting statistics
  table_events_id <- need_ids(date = date,
                              sport = sport) |>
    group_by(event_id) |>
    filter(row_number() == 1) |>
    #group_by(tab_name, group_label) |>
    #filter(row_number() %in% 1:10) |>
    as.data.table()
  
  # In this stage need to use this variant - working with story statistics
  # In the production stage need to use variant 'update_stats = T'
  if (isTRUE(update_stats)) {
    
    fun_event_statistics_g(ev_id = table_events_id$event_id)
    
  } else {
    
    NULL
    
  }
  
  # First stage of preparing table for analyze:
  #1) choose one side parameter
  #2) repair some names in incidents
  #3) pivot table (wide format)
  if (side == 'home') {
    
    if (is.null(st_name)) {
      
      stats <- need_stats(ev_id = ev_id,
                          sport = sport,
                          st_name = st_name) |>
        filter(stage_name != 'Match') |>
        select(-value_away) |>
        mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
                 gsub(pattern = '_\\(xG\\)', replacement = '') |>
                 gsub(pattern = '2', replacement = 'Two') |>
                 gsub(pattern = '3', replacement = 'Three') |>
                 gsub(pattern = '_%', replacement = '') |>
                 tolower() )
      
    } else {
      
      stats <- need_stats(ev_id = ev_id,
                          sport = sport,
                          st_name = st_name) |>
        select(-value_away) |>
        mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
                 gsub(pattern = '_\\(xG\\)', replacement = '') |>
                 gsub(pattern = '2', replacement = 'Two') |>
                 gsub(pattern = '3', replacement = 'Three') |>
                 gsub(pattern = '_%', replacement = '') |>
                 tolower() )
      
    }
    
    main_dt <- table_events_id[home_participant_name_one %ilike% team_name] |>
      mutate(h_result = ifelse(home_score_full > away_score_full, 'WIN', 
                               ifelse(home_score_full == away_score_full, 'DRAW', 'LOST'))) |>
      inner_join(stats, by = 'event_id') |>
      pivot_wider(names_from = incident_name,
                  values_from = value_home,
                  values_fn = c,
                  values_fill = NA) |>
      as.data.table() %>%
      .[order(start_time)]
    
  } else {
    
    if (is.null(st_name)) {
      
      stats <- need_stats(ev_id = ev_id,
                          sport = sport,
                          st_name = st_name) |>
        filter(stage_name != 'Match') |>
        select(-value_home) |>
        mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
                 gsub(pattern = '_\\(xG\\)', replacement = '') |>
                 gsub(pattern = '2', replacement = 'Two') |>
                 gsub(pattern = '3', replacement = 'Three') |>
                 gsub(pattern = '_%', replacement = '') |>
                 tolower() )
      
    } else {
      
      stats <- need_stats(ev_id = ev_id,
                          sport = sport,
                          st_name = st_name) |>
        select(-value_home) |>
        mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
                 gsub(pattern = '_\\(xG\\)', replacement = '') |>
                 gsub(pattern = '2', replacement = 'Two') |>
                 gsub(pattern = '3', replacement = 'Three') |>
                 gsub(pattern = '_%', replacement = '') |>
                 tolower() )
      
    }
    
    main_dt <- table_events_id[away_participant_name_one %ilike% team_name] |>
      mutate(h_result = ifelse(home_score_full < away_score_full, 'WIN', 
                               ifelse(home_score_full == away_score_full, 'DRAW', 'LOST'))) |>
      inner_join(stats, by = 'event_id') |>
      pivot_wider(names_from = incident_name,
                  values_from = value_away,
                  values_fn = c,
                  values_fill = NA) |>
      as.data.table() %>%
      .[order(start_time)]
    
  }
  
  days_between_games <- c(1, difftime(main_dt$start_time[-1], 
                                      main_dt$start_time[-length(main_dt$start_time)],
                                      units = 'days') |> round(0))
  
  main_dt <- main_dt |> mutate(days_between_games = days_between_games)
  
  # Second stage of preparing table for analyze:
  #1) remove character rows
  #2) remove NA values
  #3) interpolate/change time series of statistics parameters
  main_dt_num <- main_dt |>
    select(-c(group_label:home_participant_name_one,
              current_result, away_participant_name_one,
              h_result, stage_name)) |>
    mutate_if(is.character, as.numeric) %>%
    .[, map(.SD, zoo::na.approx, na.rm = F), .SDcols = colnames(.)]
  
  lst <- map(colnames(main_dt_num), \(i) {

    if (length(main_dt_num[[i]][ !is.na( main_dt_num[[i]]) ] ) < .75 * nrow(main_dt_num)) {

      main_dt_num[[i]] <- NULL

    } else {

      main_dt_num[[i]]

    }

  })

  names(lst) <- colnames(main_dt_num)
  
  main_dt_num <- as.data.table(lst)
  
  if (sport %in% c(1, 4)) {
      
      if (is.null(st_name) | st_name == 'match') {
    
        main_dt_no_na <- main_dt_num |>
          na.omit()
        
      } else {
        
        main_dt_no_na <- main_dt_num |>
          mutate(home_score_full = home_score_full / part,
                 away_score_full = away_score_full / part) |>
          na.omit()
        
      }
    
    } else {
    
      main_dt_no_na <- main_dt_num |>
        select(-c(home_score_full,
                  away_score_full)) |>
        na.omit()
    
    }
  
  # Last stage of preparing table for analyze:
  # creation list of results
  
  return(list('real_stats' = main_dt,
              'approximated_data' = main_dt_no_na))
  
  gc(reset = T, full = T)
  
}

create_ts <- function(tbl,
                      except_vars,
                      indep_vars = NULL,
                      all_vars = T,
                      spline = F,
                      sport = 1,
                      days = 20,
                      k = 100) {
  
  if (isTRUE(all_vars)) {
    
    map_args <- colnames( select(tbl$approximated_data, -except_vars[[sport_list[id == sport, name]]]) )
    
  } else {
    
    map_args <- indep_vars
    
  }
  
  main_dt <-  as.data.table(
    
    map(map_args, \(i) {
      
      num_observ <- cumsum(tbl$approximated_data[days_between_games < days, days_between_games])
      
      observ <- tbl$approximated_data[days_between_games < days][[i]]
      
      if (all(observ > 0 & observ < 1)) {
        
        dt_for_join <- data.table(t = num_observ,
                                  event = observ * k)
        
      } else {
        
        dt_for_join <- data.table(t = num_observ,
                                  event = observ)
        
      }
      
      if (isFALSE(spline)) {
        
        dt <- data.table(t = 1:num_observ[length(num_observ)]) |>
          left_join(dt_for_join, by = 't') %>%
          .[, map(.SD, \(i) zoo::na.approx(i, na.rm = F) ), .SDcols = colnames(.)] |>
          na.omit()
        
      } else {
        
        dt <- data.table(t = 1:num_observ[length(num_observ)]) |>
          left_join(dt_for_join, by = 't') %>%
          .[, map(.SD, \(i) zoo::na.spline(i, na.rm = F) ), .SDcols = colnames(.)] |>
          na.omit() |>
          filter(t > min(num_observ))
        
      }
      
      ts(dt$event)
      
    })
    
  )
  
  colnames(main_dt) <- map_args
  
  if (sport == 1) {
    
    main_dt %>%
      mutate_at(vars(-matches('expected_goals')), .funs = ~ round(., 0)) %>%
      mutate_at(vars(matches('expected_goals')), .funs = ~ round(., 2)) %>%
      mutate(days_between_game = 1:nrow(main_dt))
    
  } else {
    
    main_dt %>%
      mutate_at(vars(-matches('expected_goals')), .funs = ~ round(., 0)) %>%
      mutate(days_between_game = 1:nrow(main_dt))
    
  }
  
}
