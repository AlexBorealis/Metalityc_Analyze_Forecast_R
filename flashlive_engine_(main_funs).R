# Main function for a sending the request
get_data <- function(endpoints,
                     n = 1,
                     query = NULL,
                     sport_id = NULL,
                     ranking_id = NULL,
                     event_id = NULL,
                     indent_days = NULL,
                     locale = NULL,
                     timezone = NULL,
                     page = 1,
                     book_id = NULL,
                     team_id = NULL,
                     standing_type = NULL,
                     tournament_stage_id = NULL,
                     tournament_season_id = NULL,
                     season_id = NULL,
                     player_id = NULL,
                     article_id = NULL,
                     entity_id = NULL,
                     category_id = NULL,
                     stand_type = NULL) {
  
  resp <- request(paste0(host_flashlive,
                         endpoints)[n]) |>
    req_url_query("query" = query,
                  "sport_id" = sport_id,
                  "ranking_id" = ranking_id,
                  "event_id" = event_id,
                  "indent_days" = indent_days,
                  "locale" = locale,
                  "timezone" = timezone,
                  "book_id" = book_id,
                  "team_id" = team_id,
                  "standing_type" = standing_type,
                  "tournament_stage_id" = tournament_stage_id,
                  "tournament_season_id" = tournament_season_id,
                  "season_id" = season_id,
                  "player_id" = player_id,
                  "article_id" = article_id,
                  "entity_id" = entity_id,
                  "category_id" = category_id,
                  "stand_type" = stand_type) |>
    req_headers("X-RapidAPI-Key" = api_flashlive,
                "X-RapidAPI-Host" = gsub(x = host_flashlive, pattern = "https://", replacement = "")) |>
    req_retry(is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503),
              max_tries = 5) |>
    req_perform_iterative(next_req = iterate_with_offset(param_name = "page",
                                                         resp_pages = \(resp) page),
                          on_error = "return",
                          max_reqs = 5) |>
    map(\(j) {
      
      if (all(class(j) == "httr2_response")) {
        
        resp_body_json(j, check_type = F)
        
      } else {
        
        print("Error of request, use last_request() and last_response()")
        
      }
      
    })
  
}

fun_sports_list <- function() {
  
  sport_list_flashlive <- get_data(endpoints = endpoints$sports,
                                   n = 1)
  
  DT <- tibble(sports = sport_list_flashlive) |>
    unnest_wider(sports) |>
    unnest(DATA) |>
    unnest_wider(DATA) |>
    as.data.table()
  
  colnames(DT) <- colnames(DT) |>
    tolower()
  
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   user = "postgres", 
                   password = NULL, 
                   dbname = "metalityc_data", 
                   host = "localhost")
  
  dbWriteTable(con, c('sport_data', 'sport_list'),
               overwrite = T,
               row.names = F,
               value = DT)
  
  dbDisconnect(con)
  
}

fun_sports_events_count <- function(loc = locale$locale[1],
                                    tz = 3) {
  
  events_count_flashlive <- get_data(endpoints = endpoints$sports,
                                     n = 2,
                                     timezone = tz,
                                     locale = loc)
  
  DT <- tibble(resp = events_count_flashlive) |>
    unnest(resp) |>
    unnest(resp) |>
    unnest(resp) |>
    unnest_wider(resp) |>
    as.data.table()
  
  colnames(DT) <- tolower(colnames(DT))
  
  DT
  
}

fun_events_summary <- function(loc = locale$locale[1],
                               ev_id) {
  
  event_summary <- get_data(endpoints = endpoints$events,
                            n = 11,
                            event_id = ev_id,
                            locale = loc)
  
  DT <- data.table(tibble(data = event_summary) |>
                     unnest_wider(data) |>
                     unnest(DATA) |>
                     unnest_wider(DATA) |>
                     unnest(ITEMS) |>
                     unnest_wider(ITEMS) |>
                     unnest(INCIDENT_PARTICIPANTS) |>
                     unnest_wider(INCIDENT_PARTICIPANTS) |>
                     unnest_wider(INFO))
  
  #INFO <- as.data.table(event_summary_soccer$INFO)
  
  colnames(DT) <- colnames(DT) |>
    tolower()
  
  DT
  
}

fun_tournament_tables <- function(loc = locale$locale[1],
                                  std_type = 4,
                                  t_season_id,
                                  t_stage_id,
                                  sport = 1) {
  
  tournament_tables <- get_data(endpoints = endpoints$tournament,
                                n = 1,
                                locale = loc,
                                standing_type = standing_type$standing_type[std_type],
                                tournament_season_id = t_season_id,
                                tournament_stage_id = t_stage_id)
  
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   user = "postgres", 
                   password = NULL, 
                   dbname = "metalityc_data", 
                   host = "localhost")
  
  sport_list <- data.table(dbReadTable(con, name = c("sport_data","sport_list")))
  
  sport_name <- tolower(sport_list$name[sport])
  
  if (std_type %in% c(1:4)) {
    
    DT <- data.table(standing_type = standing_type$standing_type[std_type],
                     tibble(resp = tournament_tables) |>
                       unnest_wider(resp) |>
                       unnest(matches("data")) |>
                       unnest_wider(matches("data"), names_sep = "_") |>
                       unnest(matches("rows")) |>
                       unnest_wider(matches(c("rows", "meta"))) |>
                       select_if(~ !is.list(.)))
    
    colnames(DT) <- colnames(DT) |>
      tolower()
    
    dt_name <- str_glue("{sport_name}_tournament_tables")
    
  } else
    if (std_type == 5) {
      
      DT <- data.table(standing_type = standing_type$standing_type[std_type],
                       tibble(resp = tournament_tables) |>
                         unnest_wider(resp) |>
                         unnest(matches("rows")) |>
                         unnest_wider(matches("rows")) |>
                         select_if(~!is.list(.)))
      
      colnames(DT) <- colnames(DT) |>
        tolower()
      
      dt_name <- str_glue("{sport_name}_top_score_players")
      
    } else
      if (std_type == 6) {
        
        NULL
        
      }
  
  dbWriteTable(con, 
               c("sport_data", dt_name),
               overwrite = T,
               row.names = F,
               value = DT)
  
  dbDisconnect(con)
  
}

fun_tournament_stages_data <- function(loc = locale$locale[1],
                                       sport = 1,
                                       t_stage_id) {
  
  tournament_stages_data <- get_data(endpoints = endpoints$tournament,
                                     n = 6,
                                     locale = loc,
                                     tournament_stage_id = t_stage_id)
  
  DT <- data.table(tibble(resp = tournament_stages_data) |>
                     unnest(resp) |>
                     unnest_wider(resp) |>
                     unnest(SEASONS) |>
                     unnest_wider(SEASONS, names_sep = "_"))
  
  colnames(DT) <- colnames(DT) |>
    gsub(pattern = "SEASONS_", replacement = "") |>
    tolower()
  
  DT
  
}

fun_teams_data <- function(loc = locale$locale[1],
                           sport = 1,
                           tm_id) {
  
  teams_data <- get_data(endpoints = endpoints$teams,
                         n = 1,
                         sport_id = sport, 
                         team_id = tm_id,
                         locale = loc)
  
  DT <- data.table(tibble(resp = teams_data) |>
                     unnest(resp) |>
                     unnest_wider(resp))
  
  colnames(DT) <- colnames(DT) |>
    tolower()
  
  DT
  
}

fun_teams_fixtures_events <- function(loc = locale$locale[1],
                                      sport = 1,
                                      tm_id,
                                      pg = 1) {
  
  teams_fixtures <- get_data(endpoints = endpoints$teams,
                             n = 5,
                             sport_id = sport, 
                             team_id = tm_id,
                             locale = loc,
                             page = pg)
  
  DT <- data.table(tibble(resp = teams_fixtures) |>
                     unnest(resp) |>
                     unnest(resp) |>
                     unnest_wider(resp) |>
                     unnest(EVENTS) |>
                     unnest_wider(EVENTS, names_sep = "_") |>
                     unnest_wider(matches("PARTICIPANT_IDS$"), names_sep = "_") |>
                     mutate_at(vars(matches("_start_")), .funs = as.POSIXct) |>
                     select_if(~ !is.list(.)))
  
  colnames(DT) <- colnames(DT) |>
    tolower()
  
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   user = "postgres", 
                   password = NULL, 
                   dbname = "metalityc_data", 
                   host = "localhost")
  
  sport_list <- data.table(dbReadTable(con, name = c("sport_data","sport_list")))
  
  sport_name <- tolower(sport_list$name[sport])
  
  dbWriteTable(con, 
               c("sport_data", str_glue('{sport_name}_team_fixtures')),
               overwrite = T,
               row.names = F,
               value = DT)
  
  dbDisconnect(con)
  
}

fun_teams_transfers <- function(loc = locale$locale[1],
                                sport = 1,
                                tm_id,
                                pg = 1) {
  
  teams_transfers <- get_data(endpoints = endpoints$teams,
                              n = 6, 
                              team_id = tm_id,
                              locale = loc,
                              page = pg)
  
  DT <- data.table(tibble(resp = teams_transfers) |>
                     unnest(resp) |>
                     unnest(resp) |>
                     unnest_wider(resp) |>
                     unnest_wider(matches(c("from", "to", "player")), names_sep = "_"))
  
  colnames(DT) <- colnames(DT) |>
    tolower()
  
  DT
  
}

fun_team_news <- function(loc = locale$locale[1],
                          tm_id) {
  
  team_news <- get_data(endpoints = endpoints$teams,
                        n = 4,
                        team_id = tm_id,
                        local = loc)
  
  DT <- data.table(tibble(resp = team_news) |>
                     unnest(resp) |>
                     unnest(resp) |> 
                     unnest_wider(resp) |>
                     unnest(LINKS) |>
                     unnest_wider(LINKS) |>
                     mutate_if(is.character, ~ replace_na(., "No info")))
  
  colnames(DT) <- colnames(DT) |>
    tolower()
  
  in_group_by_vars <- c('image_variant_id', 'image_variant_url')
  
  group_by_vars <- colnames(DT)[!(colnames(DT) %in% in_group_by_vars)]
  
  DT <- DT[, map(.SD, list), by = DT[, ..group_by_vars]]
  
  DT
  
}

fun_team_results <- function(loc = locale$locale[1],
                             sport = 1,
                             tm_id,
                             pg = 1) {
  
  team_results <- get_data(endpoints = endpoints$teams,
                           n = 3,
                           team_id = tm_id,
                           sport_id = sport,
                           local = loc,
                           page = pg)
  
  DT <- data.table(tibble(resp = team_results) |>
                     unnest(resp) |>
                     unnest(resp) |>
                     unnest_wider(resp) |>
                     unnest(EVENTS) |>
                     unnest_wider(EVENTS, names_sep = "_") |>
                     unnest_wider(matches("PARTICIPANT_IDS$"), names_sep = "_") |>
                     mutate_at(vars(matches("_start_")), .funs = as.POSIXct) |>
                     mutate_if(is.character, ~ replace_na(., "No info")) |>
                     select_if(~ !is.list(.)))
  
  colnames(DT) <- colnames(DT) |>
    tolower()
  
  DT
  
}

fun_team_squad <- function(loc = locale$locale[1],
                           sport = 1,
                           tm_id,
                           pg = 1) {
  
  team_squad <- get_data(endpoints = endpoints$teams,
                         n = 2,
                         team_id = tm_id,
                         sport_id = sport,
                         local = loc,
                         page = pg)
  
  if (team_squad != 'Error of request, use last_request() and last_response()') {
    
    DT <- data.table(tibble(resp = team_squad) |>
                       unnest(resp) |>
                       unnest(resp) |>
                       unnest_wider(resp) |>
                       unnest(ITEMS) |>
                       unnest_wider(ITEMS) |>
                       mutate_if(is.character, ~ replace_na(., "No info")) |>
                       mutate_if(is.integer, ~ replace_na(., 0)))
    
    colnames(DT) <- colnames(DT) |>
      tolower()
    
    DT <- DT[, map(.SD, list), by = .(group_id, group_label, player_type_id)]
    
    DT
    
  } else {
    
    print('Error of request, use last_request() and last_response()')
    
  }
  
}

need_ids <- function(date = NULL,
                     sport = 1) {
  
  sport_name <- tolower(sport_list$name)
  
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   user = "postgres", 
                   password = NULL, 
                   dbname = "metalityc_data", 
                   host = "localhost")
  
  on.exit(dbDisconnect(con))
  
  if (!is.null(date)) {
    
    data.table(
      
      dbGetQuery(con, str_glue('select *
                                from (
                              	
                                	select group_label, event_name,
                                		unnest(tab_name) "tab_name", unnest(start_time) "start_time", unnest(event_id) "event_id", 
                                		unnest(home_participant_name_one) "home_participant_name_one", unnest(home_score_full) "home_score_full",
                                		unnest(current_result) "current_result",
                                		unnest(away_score_full) "away_score_full", unnest(away_participant_name_one) "away_participant_name_one",
                                		unnest(h_result) "h_result" 
                                	from sport_data.{sport_name[sport]}_hth_g) tab
                                where start_time::date > \'{date}\''))
      
    )
    
  } else {
    
    data.table(
      
      dbGetQuery(con, str_glue('select group_label, event_name,
                              		unnest(tab_name) "tab_name", unnest(start_time) "start_time", unnest(event_id) "event_id", 
                              		unnest(home_participant_name_one) "home_participant_name_one", unnest(home_score_full) "home_score_full",
                              		unnest(current_result) "current_result",
                              		unnest(away_score_full) "away_score_full", unnest(away_participant_name_one) "away_participant_name_one",
                              		unnest(h_result) "h_result" 
                              	from sport_data.{sport_name[sport]}_hth_g'))
      
    )
    
  }
  
}

need_stats <- function(ev_id = NULL,
                       sport = 1,
                       st_name = NULL) {
  
  sport_name <- tolower(sport_list$name)
  
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   user = "postgres", 
                   password = NULL, 
                   dbname = "metalityc_data", 
                   host = "localhost")
  
  on.exit(dbDisconnect(con))
    
  if (is.null(st_name)) {
    
    if (is.null(ev_id)) {
      
      data.table(
        
        dbGetQuery(con, str_glue('select event_id, incident_name,
                                    unnest(stage_name) "stage_name",
                                    unnest(value_home) "value_home",
                                    unnest(value_away) "value_away"
                                  from sport_data.{sport_name[sport]}_statistics_g;'))
        
      )
      
    } else {
      
      data.table(
        
        dbGetQuery(con, str_glue('select event_id, incident_name,
                                    unnest(stage_name) "stage_name",
                                    unnest(value_home) "value_home",
                                    unnest(value_away) "value_away"
                                  from sport_data.{sport_name[sport]}_statistics_g
                                  where event_id = \'{ev_id}\';'))
        
      )
      
    }
    
  } else {
    
    if (is.null(ev_id)) {
      
      data.table(
        
        dbGetQuery(con, str_glue('select *
                                  from (
                                    select event_id, incident_name,
                                      unnest(stage_name) "stage_name",
                                      unnest(value_home) "value_home",
                                      unnest(value_away) "value_away"
                                    from sport_data.{sport_name[sport]}_statistics_g) tab
                                  where stage_name ilike \'{st_name}\';'))
        
      )
      
    } else {
      
      data.table(
        
        dbGetQuery(con, str_glue('select *
                                  from (
                                    select event_id, incident_name,
                                      unnest(stage_name) "stage_name",
                                      unnest(value_home) "value_home",
                                      unnest(value_away) "value_away"
                                    from sport_data.{sport_name[sport]}_statistics_g) tab
                                  where event_id = \'{ev_id}\' and stage_name ilike \'{st_name}\';'))
        
      )
      
    }
    
  }
  
}