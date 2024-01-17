# Getting table of tournaments from all world ----
fun_tournament_list_g <- function(loc = locale$locale[1],
                                  sport = 1) {
  
  tournament_list <- get_data(endpoints = endpoints$tournament,
                              n = 7,
                              locale = loc,
                              sport_id = sport) 
  
  DT <- data.table(tibble(resp = tournament_list) |>
                     unnest(resp) |>
                     unnest(resp) |>
                     unnest_wider(resp) |>
                     unnest(STAGES) |>
                     unnest_wider(STAGES)) |>
    mutate_at(vars(matches("out")), .funs = as.integer)
  
  colnames(DT) <- colnames(DT) |>
    tolower()
  
  tbl_names <- colnames(DT)
  
  col_classes <- map_chr(tbl_names, \(i) class(DT[[i]])[1])
  
  group_by_vars <- tbl_names[which(tbl_names == "league_name"):which(tbl_names == "template_id")]
  
  in_group_by_vars <- tbl_names[!(tbl_names %in% group_by_vars)]
  
  DT <- DT[, map(.SD, \(i) paste0("{", toString(i), "}")), by = DT[, ..group_by_vars]] |>
    select(all_of(tbl_names))
  
  field.types <- c(map_chr(col_classes[tbl_names %in% group_by_vars], \(i) {
    
    if (i != "integer") {
      
      "text"
      
    } else {
      
      "int2"
      
    }
    
  }),
  map_chr(col_classes[tbl_names %in% in_group_by_vars], \(i) {
    
    if (i == 'character') {
      
      str_replace(i, pattern = "character", replacement = "text[]")
      
    } else 
      if (i == 'integer') {
        
        str_replace(i, pattern = "integer", replacement = "int2[]")
        
      } else
        if (i == 'numeric') {
          
          str_replace(i, pattern = "numeric", replacement = "float8[]")
          
        } else
          if (i == 'POSIXct') {
            
            str_replace(i, pattern = "POSIXct", replacement = "timestamp[]")
            
          } else {
            
            str_replace(i, pattern = "logical", replacement = "boolean[]")
            
          }
    
  }))
  
  names(field.types) <- tbl_names
  
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   user = "postgres", 
                   password = NULL, 
                   dbname = "metalityc_data", 
                   host = "localhost")
  
  sport_list <- data.table(dbReadTable(con, name = c("sport_data","sport_list")))
  
  sport_name <- tolower(sport_list$name[sport])
  
  dt_name <- str_glue("{sport_name}_g_tournament_list")
  
  dbWriteTable(con, c("sport_data", dt_name),
               overwrite = T,
               row.names = F,
               field.types = field.types,
               value = DT[order(league_name)])
  
  dbDisconnect(con)
  
} 

# Getting table of events from 7 days ago to +7 days after today ----
fun_events_list_g <- function(loc = locale$locale[1],
                              tz = 3,
                              days,
                              sport = 1) {
  
  events_list <- get_data(endpoints = endpoints$events[1],
                          n = 1,
                          sport_id = sport,
                          timezone = tz,
                          indent_days = days,
                          locale = loc)
  
  DT <- data.table(tibble(resp = events_list) |>
                     unnest_wider(resp) |>
                     unnest(matches("data")) |>
                     unnest_wider(matches(c("data", "meta"))) |>
                     unnest(EVENTS) |>
                     unnest_wider(EVENTS, names_sep = "_") |>
                     unnest_wider(matches("PARTICIPANT_IDS$"), names_sep = "_") |>
                     mutate_at(vars(matches("_start_")), .funs = as.POSIXct) |>
                     select_if(~ !is.list(.)) |>
                     select(-matches(c("tss", "zkl", "zku", "stages_count", "events_ime", "datacore"))) |>
                     mutate_at(vars(matches("score_part_")), .funs = as.integer) |>
                     mutate_if(is.numeric, ~ replace_na(., 0)) |>
                     mutate_if(is.character, ~ replace_na(., "No info")) |>
                     mutate_if(is.POSIXct, ~ replace_na(., as.POSIXct(0))) |>
                     mutate_if(is.logical, ~ replace_na(., as.logical(0))) |>
                     mutate_if(is.integer, ~ replace_na(., 0)))
  
  colnames(DT) <- colnames(DT) |>
    tolower()
  
  tbl_names <- colnames(DT)
  
  col_classes <- map_chr(tbl_names, \(i) class(DT[[i]])[1])
  
  group_by_vars <- tbl_names[which(tbl_names == "name"):which(tbl_names == "category_name")]
  
  in_group_by_vars <- tbl_names[!(tbl_names %in% group_by_vars)]
  
  DT <- DT[, map(.SD, \(i) paste0("{", toString(i), "}")), by = DT[, ..group_by_vars]] |>
    select(all_of(tbl_names))
  
  field.types <- c(map_chr(col_classes[tbl_names %in% group_by_vars], \(i) {
    
    if (i != "integer") {
      
      "text"
      
    } else {
      
      "int2"
      
    }
    
  }),
  map_chr(col_classes[tbl_names %in% in_group_by_vars], \(i) {
    
    if (i == 'character') {
      
      str_replace(i, pattern = "character", replacement = "text[]")
      
    } else 
      if (i == 'integer') {
        
        str_replace(i, pattern = "integer", replacement = "int2[]")
        
      } else
        if (i == 'numeric') {
          
          str_replace(i, pattern = "numeric", replacement = "float8[]")
          
        } else
          if (i == 'POSIXct') {
            
            str_replace(i, pattern = "POSIXct", replacement = "timestamp[]")
            
          } else {
            
            str_replace(i, pattern = "logical", replacement = "boolean[]")
            
          }
    
  }))
  
  names(field.types) <- tbl_names
  
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   user = "postgres", 
                   password = NULL, 
                   dbname = "metalityc_data", 
                   host = "localhost")
  
  sport_list <- data.table(dbReadTable(con, name = c("sport_data","sport_list")))
  
  sport_name <- tolower(sport_list$name[sport])
  
  if (days < 0) {
    
    dt_name <- str_glue("{sport_name}_g_event_list_{abs(days)}_days_ago")
    
  } else if (days == 0) {
    
    dt_name <- str_glue("{sport_name}_g_event_list_today")
    
  } else {
    
    dt_name <- str_glue("{sport_name}_g_event_list_{abs(days)}_days_after")
    
  }
  
  dbWriteTable(con, c("sport_data", dt_name),
               overwrite = T,
               row.names = F,
               field.types = field.types,
               value = DT)
  
  dbDisconnect(con)
  
} 

# Getting table of odds of current event ----
fun_event_odds_g <- function(loc = locale$locale[1],
                             ev_id,
                             sport = 1) {
  
  event_odds <- get_data(endpoints = endpoints$events,
                         n = 15,
                         locale = loc,
                         event_id = ev_id)
  
  DT <- data.table(tibble(odds = event_odds) |>
                     unnest_wider(odds) |>
                     unnest(DATA) |>
                     unnest_wider(DATA) |>
                     unnest(PERIODS) |>
                     unnest_wider(PERIODS, names_sep = "_") |>
                     unnest(PERIODS_GROUPS) |>
                     unnest_wider(PERIODS_GROUPS, names_sep = "_") |>
                     unnest(PERIODS_GROUPS_MARKETS) |>
                     unnest_wider(PERIODS_GROUPS_MARKETS, names_sep = "_") |>
                     unnest_wider(matches("ODD_CELL"), names_sep = "_") |>
                     select_if(~ !is.list(.)) |>
                     mutate_at(vars(matches("group_name")), .funs = as.character) |>
                     mutate_if(is.numeric, ~ replace_na(., 0)) |>
                     mutate_if(is.character, ~ replace_na(., "No info")))
  
  colnames(DT) <- colnames(DT) |>
    gsub(pattern = "PERIODS_GROUPS_", replacement = "") |>
    gsub(pattern = "MARKETS_", replacement = "") |>
    tolower()
  
  DT <- DT |>
    pivot_longer(cols = matches("value"),
                 cols_vary = "slowest",
                 names_to = "parts_of_time",
                 values_to = "coefs",
                 values_drop_na = T) |>
    select(-matches('move')) |>
    as.data.table() %>%
    .[order(betting_type, periods_odds_stage)]
  
  tbl_names <- c('key_event_id', colnames(DT))
  
  col_classes <- map_chr(tbl_names, \(i) {
    
    if (i %in% 'key_event_id') {
      
      class(i) <- 'character'
      
    } else {
      
      class(DT[[i]])[1]
      
    }
    
  })
  
  names(col_classes) <- tbl_names
  
  DT <- data.table(key_event_id = ev_id,
                   DT[, map(.SD, \(i) paste0("{", toString(i), "}")), by = .(betting_type, periods_odds_stage)])
  
  group_by_vars <- c('key_event_id', 'betting_type', 'periods_odds_stage')
  
  in_group_by_vars <- tbl_names[!(tbl_names %in% group_by_vars)]
  
  field.types <- c(map_chr(col_classes[tbl_names %in% group_by_vars], \(i) {
    
    if (i != "integer") {
      
      "text"
      
    } else {
      
      "int2"
      
    }
    
  }),
  map_chr(col_classes[tbl_names %in% in_group_by_vars], \(i) {
    
    if (i == 'character') {
      
      str_replace(i, pattern = "character", replacement = "text[]")
      
    } else 
      if (i == 'integer') {
      
      str_replace(i, pattern = "integer", replacement = "int2[]")
        
      } else
        if (i == 'numeric') {
          
          str_replace(i, pattern = "numeric", replacement = "float8[]")
          
        } else
          if (i == 'POSIXct') {
            
            str_replace(i, pattern = "POSIXct", replacement = "timestamp[]")
            
          } else {
            
            str_replace(i, pattern = "logical", replacement = "boolean[]")
            
          }
    
  }))
  
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   user = "postgres", 
                   password = NULL, 
                   dbname = "metalityc_data", 
                   host = "localhost")
  
  sport_list <- data.table(dbReadTable(con, name = c("sport_data","sport_list")))
  
  sport_name <- tolower(sport_list$name[sport])
  
  dbWriteTable(con, c("sport_data", str_glue("{sport_name}_odds_g")),
               overwrite = F,
               append = T,
               row.names = F,
               field.types = field.types,
               value = DT)
  
  dbDisconnect(con)
  
} 

# Getting table of results between opponents ----
fun_event_hth_g <- function(loc = locale$locale[1],
                            ev_id,
                            sport = 1) {
  
  event_hth <- get_data(endpoints = endpoints$events,
                        n = 5,
                        event_id = ev_id,
                        locale = loc)
  
  DT <- data.table(tibble(hth = event_hth) |>
                     unnest_wider(hth) |>
                     unnest(DATA) |>
                     unnest_wider(DATA) |>
                     unnest(GROUPS) |>
                     unnest_wider(GROUPS) |>
                     unnest(ITEMS) |>
                     unnest_wider(ITEMS) |>
                     select_if(~ !is.list(.)) |>
                     mutate_at(vars(matches("start")), .funs = as.POSIXct) |>
                     mutate_at(vars(matches("score")), .funs = as.integer) |>
                     mutate_if(is.character, ~ replace_na(., "No info")) |>
                     select(-matches('name_two')))
  
  colnames(DT) <- colnames(DT) |>
    tolower()
  
  tbl_names <- c('key_event_id', colnames(DT))
  
  col_classes <- map_chr(tbl_names, \(i) {
    
    if (i %in% 'key_event_id') {
      
      class(i) <- 'character'
      
    } else {
      
      class(DT[[i]])[1]
      
    }
    
  })
  
  names(col_classes) <- tbl_names
  
  DT <- data.table(key_event_id = ev_id,
                   DT[, map(.SD, \(i) paste0('{', toString(i), '}')), by = .(group_label, event_name)])
  
  group_by_vars <- c('key_event_id', 'group_label', 'event_name')
  
  in_group_by_vars <- tbl_names[!(tbl_names %in% group_by_vars)]
  
  field.types <- c(map_chr(col_classes[tbl_names %in% group_by_vars], \(i) {
    
    if (i != "integer") {
      
      "text"
      
    } else {
      
      "int2"
      
    }
    
  }),
  map_chr(col_classes[tbl_names %in% in_group_by_vars], \(i) {
    
    if (i != "POSIXct" & i != "logical") {
      
      if (i != "integer") {
        
        str_replace(i, pattern = "character", replacement = "text[]")
        
      } else {
        
        str_replace(i, pattern = "integer", replacement = "int2[]")
        
      }
      
    } else 
      if (i == "logical") {
        
        str_replace(i, pattern = "logical", replacement = "boolean[]")
        
      } else {
        
        str_replace(i, pattern = "POSIXct", replacement = "timestamp[]")
        
      }
    
  }))
  
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   user = "postgres", 
                   password = NULL, 
                   dbname = "metalityc_data", 
                   host = "localhost")
  
  sport_list <- data.table(dbReadTable(con, name = c("sport_data","sport_list")))
  
  sport_name <- tolower(sport_list$name[sport])
  
  dbWriteTable(con, c("sport_data", str_glue("{sport_name}_hth_g")),
               overwrite = T,
               row.names = F,
               field.types = field.types,
               value = DT)
  
  dbDisconnect(con)
  
} 

# Getting table of statistics between opponents ----
fun_event_statistics_g <- function(loc = locale$locale[1],
                                   ev_id,
                                   sport = 1) {
  
  DT <- rbindlist({
    
    foreach(i = ev_id,
            .errorhandling = 'remove') %do% {
              
              event_statistics <- get_data(endpoints = endpoints$events,
                                           n = 10,
                                           event_id = i,
                                           locale = loc)
              
              data.table(event_id = i,
                         tibble(stats = event_statistics) |>
                           unnest_wider(stats) |>
                           unnest(DATA) |>
                           unnest_wider(DATA) |>
                           unnest(GROUPS) |>
                           unnest_wider(GROUPS) |>
                           unnest(ITEMS) |>
                           unnest_wider(ITEMS))
              
            }
    
  }, fill = T)
  
  colnames(DT) <- colnames(DT) |>
    tolower()
  
  if (sport %in% c(1, 3)) {
    
    if (nrow(DT[grep(value_home, pattern = '%')]) > 0) {
      
      DT[grep(value_home, pattern = '%'), value_home := as.numeric(str_remove_all(value_home, pattern = '%')) / 100]
      
      DT[, value_home := as.numeric(value_home)]
      
    } else {
      
      DT[, value_home := as.numeric(value_home)]
      
    }
    
    if (nrow(DT[grep(value_away, pattern = '%')]) > 0) {
      
      DT[grep(value_away, pattern = '%'), value_away := as.numeric(str_remove_all(value_away, pattern = '%')) / 100]
      
      DT[, value_away := as.numeric(value_away)]
      
    } else {
      
      DT[, value_away := as.numeric(value_away)]
      
    }
    
  } else 
    if (sport == 4) {
    
      if (nrow(DT[grep(value_home, pattern = '%')]) > 0) {
        
        DT[grep(value_home, pattern = '%'), value_home := as.numeric({
          
          value_home |> str_remove_all(pattern = '%') |> str_split(pattern = ' ', simplify = T) %>% .[, 1]
          
        }) / 100]
        
        DT[, value_home := as.numeric(value_home)]
        
      } else {
        
        DT[, value_home := as.numeric(value_home)]
        
      }
      
      if (nrow(DT[grep(value_away, pattern = '%')]) > 0) {
        
        DT[grep(value_away, pattern = '%'), value_away := as.numeric({
          
          value_away |> str_remove_all(pattern = '%') |> str_split(pattern = ' ', simplify = T) %>% .[, 1]
          
        }) / 100]
        
        DT[, value_away := as.numeric(value_away)]
        
      } else {
        
        DT[, value_away := as.numeric(value_away)]
        
      }
    
  } else 
    if (sport == 5) {
    
    NULL
    
  } else
    if (sport == 6) {
      
      if (nrow(DT[grep(value_home, pattern = '%')]) > 0) {
        
        DT[grep(value_home, pattern = '%'), value_home := as.numeric({
          
          value_home |> str_remove_all(pattern = '%') |> str_split(pattern = ' ', simplify = T) %>% .[, 1]
          
        }) / 100]
        
        DT[, value_home := as.numeric(value_home)]
        
      } else {
        
        DT[, value_home := as.numeric(value_home)]
        
      }
      
      if (nrow(DT[grep(value_away, pattern = '%')]) > 0) {
        
        DT[grep(value_away, pattern = '%'), value_away := as.numeric({
          
          value_away |> str_remove_all(pattern = '%') |> str_split(pattern = ' ', simplify = T) %>% .[, 1]
          
        }) / 100]
        
        DT[, value_away := as.numeric(value_away)]
        
      } else {
        
        DT[, value_away := as.numeric(value_away)]
        
      }
      
    } else 
    if (sport == 7) {
      
      NULL
      
    }
  
  DT <- DT[order(incident_name),
           map(.SD, \(i) paste0('{', toString(i), '}')),
           keyby = .(event_id, incident_name),
           .SDcols = c('stage_name', 'value_home', 'value_away')][, .(event_id,
                                                                      incident_name,
                                                                      stage_name,
                                                                      value_home,
                                                                      value_away)]
  
  field.types <- c('text', 'text', 'text[]', 'float8[]', 'float8[]')
  
  names(field.types) <- colnames(DT)
  
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   user = "postgres", 
                   password = NULL, 
                   dbname = "metalityc_data", 
                   host = "localhost")
  
  sport_list <- data.table(dbReadTable(con, name = c("sport_data","sport_list")))
  
  sport_name <- tolower(sport_list$name[sport])
  
  dbWriteTable(con, c("sport_data", str_glue("{sport_name}_statistics_g")),
               overwrite = T,
               row.names = F,
               field.types = field.types,
               value = DT)
  
  dbDisconnect(con)
  
} 