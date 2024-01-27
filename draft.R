# Parameters ----
# Team names
team_name <- c('Bournemouth', 'Liverpool')

inc_names <- c('Attacks', 'Ball_Possession', 'Blocked_Shots', 'Completed_Passes', 'Corner_Kicks',
               'Clearances_Completed', 'Dangerous_Attacks', 'Expected_Goals', 'Red_Cards',
               'Fouls', 'Free_Kicks', 'Goal_Attempts', 'Goalkeeper_Saves', 'Offsides',
               'Shots_off_Goal', 'Shots_on_Goal', 'Tackles', 'Throw_ins', 'Total_Passes', 'Yellow_Cards', 
               'home_score_full', 'away_score_full', 'pts')

inc_name <- 'Ball_Possession'

p = .8

#new_form = F

DT <- tab_for_an(team_name = team_name[2], st_name = 'match', 
                 date = Sys.Date() - 700, side = 'away', a = .05,
                 inc_name = ifelse(is.null(inc_name), 'pts', inc_name))

if (is.null(inc_name)) {
  
  training.samples <- createDataPartition(DT$correlated_data$pts, p = p, list = FALSE)
  
} else {
  
  training.samples <- createDataPartition(DT$correlated_data[[inc_name]], p = p, list = FALSE)
  
}

train.data  <- DT$correlated_data[training.samples]

test.data <- DT$correlated_data[-training.samples]

new_test.data <- data.table('quantiles' = c('0%', '25%', '50%', '75%', '100%'),
                            t(map_df(colnames(train.data), \(i) {
                              
                              quantile(train.data[[i]], probs = seq(0, 1, .25))
                              
                            })))

colnames(new_test.data) <- c('quantiles', colnames(test.data))

# Testing methods ----

cr_dt <- create_models(team_name = team_name[2], st_name = 'match',
                       date = Sys.Date() - 700, side = 'away', a = .05,
                       inc_name = ifelse(is.null(inc_name), 'pts', inc_name),
                       ntree = 1000, family = gaussian())

an_dt <- analyze_models(models = cr_dt)

#PCR ----
model_pcr <- pcr(DT$formula_without_intercept, data = train.data, scale = T)

summary(model_pcr)

R2_pcr <- R2(model_pcr)

model_glm <- glm(DT$formula_without_intercept, data = train.data, family = gaussian())

sum_glm <- summary(model_glm)

sum_glm

model_rf <- randomForest(DT$formula_without_intercept, data = train.data, ntrees = 1000)

results <- data.table(new_test.data[, .(quantiles)],
                      new_test.data[, ..inc_name],
                      predict_rf = predict(model_rf, new_test.data),
                      delta2_rf = (new_test.data[, ..inc_name] - predict(model_rf, new_test.data))^2,
                      predict_glm = predict(model_glm, new_test.data),
                      delta2_glm = (new_test.data[, ..inc_name] - predict(model_glm, new_test.data))^2,
                      predict_pcr = predict(model_pcr, new_test.data, 
                                            ncomp = which(as.numeric(R2_pcr$val) >= max(as.numeric(R2_pcr$val) ) * 3/4 )[1])[, 1, 1],
                      delta2_pcr = (new_test.data[, ..inc_name] - predict(model_pcr, new_test.data))^2)

colnames(results) <- c('quantiles', inc_name,
                       'predict_rf', 'delta2_rf',
                       'predict_glm', 'delta2_glm',
                       'predict_pcr', 'delta2_pcr')

results[, .(delta2_rf = sum(delta2_rf),
            delta2_glm = sum(delta2_glm),
            delta2_pcr = sum(delta2_pcr))]

results[, .(real = mean(DT$approximated_data[[inc_name]]),
            predict_rf = mean(predict_rf),
            predict_glm = mean(predict_glm),
            predict_pcr = mean(predict_pcr))]

learn_result <- results[, c(1, 2, 3, 5, 7)] |>
  mutate_if(is.numeric, ~ round(., 2)) %>%
  mutate(predict_ensemb = round(rowMeans(.[, 3:5], na.rm = T), 2))

learn_result

r2_train <- sum((learn_result$away_score_full - learn_result$predict_ensemb)^2)

train.data[, .N, keyby = .(away_score_full)] |>
  mutate(p = round( cumsum(N/sum(N)), 2) ) |>
  mutate(U = round(1/p, 2),
         q = round(1 - p, 2),
         O = round(1/(1 - p), 2))

bayes_correct <- function() {
  
  results <- data.table(new_test.data[, .(quantiles)],
                        new_test.data[, ..inc_name],
                        predict_rf = predict(model_rf, new_test.data),
                        delta2_rf = (new_test.data[, ..inc_name] - predict(model_rf, new_test.data))^2,
                        predict_glm = predict(model_glm, new_test.data),
                        delta2_glm = (new_test.data[, ..inc_name] - predict(model_glm, new_test.data))^2,
                        predict_pcr = predict(model_pcr, new_test.data, 
                                              ncomp = which(as.numeric(R2_pcr$val) >= max(as.numeric(R2_pcr$val) ) * 3/4 )[1])[, 1, 1],
                        delta2_pcr = (new_test.data[, ..inc_name] - predict(model_pcr, new_test.data))^2)
  
  colnames(results) <- c('quantiles', inc_name,
                         'predict_rf', 'delta2_rf',
                         'predict_glm', 'delta2_glm',
                         'predict_pcr', 'delta2_pcr')
  
  learn_result <- results[, c(1, 2, 3, 5, 7)] |>
    mutate_if(is.numeric, ~ round(., 2)) %>%
    mutate(predict_ensemb = round(rowMeans(.[, 3:5], na.rm = T), 2))
  
  ratio <- 
  
  if (abs(ratio) > .2) {
    
    DT <- DT |>
      mutate(predict_ensemb = predict_ensemb * ratio)
    
  } else {
    
    next_predict <- learn_result
    
  }
  
  return(next_predict)
  
  pAB <- pA * pBA / pB
  return(pAB)
  
}

# Just train & test ----
model_pcr <- pcr(DT$formula_without_intercept, data = train.data, scale = T)

summary(model_pcr)

model_glm <- glm(DT$formula_without_intercept, data = train.data, family = gaussian())

model_rf <- randomForest(DT$formula_without_intercept, data = train.data, ntrees = 1000)

y_test = test.data[[inc_name]]
y_rf_test <- predict(model_rf, test.data)
y_pcr_test <- predict(model_pcr, test.data)[, 1, 2]
y_glm_test <- predict(model_glm, test.data)

r2_rf_test = sum((y_test - y_rf_test)^2)
r2_pcr_test = sum((y_test - y_pcr_test)^2)
r2_glm_test = sum((y_test - y_glm_test)^2)

r2_rf_test
r2_pcr_test
r2_glm_test


if (isTRUE(graphs)) {
  
  ggplot(data = train.data |> cbind(start_time = DT$real_stats[training.samples, start_time]), 
         mapping = aes(x = start_time,
                       y = inc_name)) +
    geom_line() +
    geom_line(aes(y = model_lm$fitted.values), colour = 'red') +
    geom_line(aes(y = model_lm1$fitted.values), colour = 'green') +
    geom_line(aes(y = model_rf$predicted), colour = 'orange') +
    geom_line(aes(y = model_arima$fitted), colour = 'purple')
  
  ggplot(data = dt_result_train, mapping = aes(x = start_time,
                                               y = real)) +
    geom_line() +
    geom_line(aes(y = ensemb), colour = 'red')
  
  ggplot(data = dt_result_test, mapping = aes(x = start_time,
                                              y = real)) +
    geom_line() +
    geom_line(aes(y = ensemb), colour = 'red')
  
} else {
  
  NULL
  
}

ggplot(data = DT$real_stats,
       mapping = aes(x = start_time,
                     y = Total_Passes)) +
  geom_line() +
  stat_smooth() +
  geom_point(mapping = aes(y = c(model_glm$fitted.values, predict(model_glm, test.data))),
             colour = 'green') +
  geom_point(mapping = aes(y = c(model_rf$predicted, predict(model_rf, test.data))),
             colour = 'red')

#visualize CV plots
validationplot(model_pcr$model_pcr)
validationplot(model_pcr$model_pcr, val.type = "MSEP")
validationplot(model_pcr$model_pcr, val.type = "R2")

if (p == 1) {
  
  pred_data <- data.table(matrix(colMeans(train.data), nrow = 1))
  colnames(pred_data) <- colnames(train.data)
  
  pred_data <- train.data[17]
  
} else {
  
  pred_data <- test.data
  
}

y <- pred_data[[inc_name]]
y_pcr <- predict(model_pcr, ncomp = 10, pred_data) |> as.numeric()
y_rf <- predict(model_rf, pred_data)

R2_pcr <- sum((y - y_pcr)^2)
R2_rf <- sum((y - y_rf)^2)

data.table(real_value = y,
           pcr_value = y_pcr,
           R2_pcr = R2_pcr,
           rf_value = y_rf,
           R2_rf = R2_rf) |>
  mutate_at(vars(pcr_value, rf_value), ~ round(., 2))

#make this example reproducible
set.seed(1)

#fit PCR model
model <- pcr(reformulate(c('mpg','disp', 'drat','wt', 'qsec'), response = 'hp', intercept = F),
             data=mtcars, scale=TRUE, validation="CV")

#view summary of model fitting
summary(model)

#visualize CV plots
validationplot(model_pcr)
validationplot(model_pcr, val.type="MSEP")

#define training and testing sets
train <- mtcars[1:25, c("hp", "mpg", "disp", "drat", "wt", "qsec")]
y_test <- mtcars[26:nrow(mtcars), c("hp")]
test <- mtcars[26:nrow(mtcars), c("mpg", "disp", "drat", "wt", "qsec")]

#use model to make predictions on a test set
model <- pcr(hp~mpg+disp+drat+wt+qsec, data = train, scale =TRUE, validation = "CV")
predict(model, test, ncomp = 3)

#2
new_signif_names <- names(which(sum_lm$coefficients[, "Pr(>|t|)"] < 0.1))

condition <- new_signif_names[new_signif_names %in% new_names]

if (length(condition) != 0) {
  
  new_formula <- reformulate(condition,
                             response = ifelse(is.null(inc_name), 'pts', DT$forecast[[inc_name]]),
                             intercept = F)
  
} else {
  
  new_formula <- reformulate(new_signif_names,
                             response = ifelse(is.null(inc_name), 'pts', DT$forecast[[inc_name]]),
                             intercept = F)
  
}

model_lm_new <- lm(formula = new_formula, data = train.data)

sum_lm <- summary(model_lm_new)

sum_lm

comp_pas <- sum_lm

principal(r = cor(DT$forecast), nfactors = length(DT$cluster), rotate = 'cluster')

#knots <- quantile(train.data$Dangerous_Attacks, p = c(0.25, 0.5, 0.75))

model_lm_new <- mlm(formula = home_score_full ~ s(Dangerous_Attacks) - 1, data = train.data)

sum_lm <- summary(model_lm_new)

sum_lm

predict(model_glm_new, newdata = data.table(Ball_Possession = mean(DT$forecast$Ball_Possession),
                                            Attacks = mean(DT$forecast$Attacks),
                                            Completed_Passes = mean(DT$forecast$Completed_Passes)))

predict(model_lm_new, data.table(Tackles = 32))

a_sc <- data.table(pts = mean(DT$forecast$pts),
                   Expected_Goals = xG)

h_sc <- data.table(pts = mean(DT$forecast$pts),
                   Shots_on_Goal = mean(DT$forecast$Shots_on_Goal))

new <- data.table(Offsides = mean(DT$forecast$Offsides),
                  Tackles = mean(DT$forecast$Tackles))

predict(model_lm_new, h_sc)

predict(model_lm_new, a_sc)

ggplot(data = DT$real_stats,
       mapping = aes(x = start_time,
                     y = away_score_full),
       colour = 'blue') +
  geom_point() +
  stat_smooth()

model_rf <- randomForest(data = train.data, away_score_full ~ Tackles)

predict(model_rf, data.table(Tackles = 32))

# ARIMA ----

model_arima <- auto.arima(DT$approximated_data[[inc_name]], stationary = T, seasonal = F)

model_poisson <- poisson(link = 'log')

acf <- mcmc(data = DT$approximated_data[[inc_name]]) |> autocorr(lags = c(1:10))

lst_arima_gen <- map(names(DT$approximated_data), \(i) {
  
  ts <- DT$approximated_data[[i]]
  
  length_ts <- length(ts[!is.na(ts)])
  
  mean <- mean(ts, na.rm = T)
  
  sd <- sd(ts, na.rm = T)
  
  n <- nrow(DT$approximated_data)
  
  if (length_ts == n) {
    
    ts
    
  } else if (length_ts > n * 2/3) {
    
    repeat {
      
      model_arima <- auto.arima(ts, stationary = T, seasonal = F)
      
      ar = model_arima$coef[grep(names(model_arima$coef), pattern = 'ar')]
      
      ma = model_arima$coef[grep(names(model_arima$coef), pattern = 'ma')]
      
      DT_arima <- data.table(real = ts,
                             arima = arima.sim(list(ar = ar, ma = ma),
                                               sd = sd, 
                                               mean = mean,
                                               n = n)) |>
        mutate(delta = real - arima,
               ratio = (real - arima)/real)
      
      if (max(abs(DT_arima$delta), na.rm = T) < quantile(ts, probs = .5, na.rm = T)) break
      
    }
    
    as.numeric(DT_arima$arima)
    
  } else {
    
    rpois(n = n, lambda = quantile(ts, probs = .15, na.rm = T))
    
  }
  
})

#summary(ur.df( ts[!is.na(ts)] ))

names(lst_arima_gen) <- colnames(DT$approximated_data)

as.data.table(lst_arima_gen)

sd(DT_arima$real, na.rm = T)/mean(DT_arima$real, na.rm = T)
conf_int(DT_arima$real, se = sd(DT_arima$real, na.rm = T), ci = .80)

ggplot(data = DT_arima,
       mapping = aes(start_time,
                     y = real),
       colour = 'blue') +
  geom_line() +
  geom_line(mapping = aes(y = arima),
            colour = 'red') +
  stat_smooth()

# Bootstrap ----

coef_function <- function(formula, data, indices) {
  
  d <- data[indices,] #allows boot to select sample
  fit <- lm(formula, data = d) #fit regression model
  return(coef(fit)) #return coefficient estimates of model
  
}

boot <- boot(data = DT$forecast, 
             statistic = coef_function,
             R = 1000,
             formula = DT$formula_without_intercept)

boot.ci(boot, type = "bca", index = 1)

# Non linear models

# testing data for multivariate normality, if skew and kurtosis ~ 0 => data is not multivariate normality ----
mardia(dt)

# KMO - test for factor analyze
KMO(dt)

bartlett.test(dt)

# Count factors ----
scree(cor(dt_pre_no_na))

vss(cor(dt_pre_no_na))

fa.parallel(
  
  cor(dt_pre_no_na),
  n.obs = nrow(dt_pre_no_na),
  quant = .95)

nfactors(
  
  cor(dt_pre_no_na),
  n.obs = nrow(dt_pre_no_na)
  
)

fa(
  
  dt_pre_no_na,
  nfactors = 2,
  rotate = 'cluster',
  scores = 'tenBerge'
  
)

# Fuzzy clustering main ----
result <- cmeans(dt, centers = 3, m = 1.2, control = list(set.seed(111)))

list('cmeans_result' = result,
     'table_with_cmeans_result' = cbind(dt_pre |>
                                          select(c(group_label:stage_name, "clust_vars", all_of(dep_vars))),
                                        result$cluster,
                                        result$membership),
     'correlation_matrix' = COR,
     'dependent_variabes' = dep_vars)

# Summary by table ----
cbind(tibble(incdent_name = incidents), 
      map_df(incidents, \(i) summary(dt_pre_num[[i]])))

#insert_data_to_db <- function(tbl,
#                              dt_name) {
  
#  tbl_names <- colnames(tbl)
  
#  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
#                   user = "postgres", 
#                   password = NULL, 
#                   dbname = "metalityc_data", 
#                   host = "localhost")
  
#  lst <- map(1:length(tbl_names), \(i) {
    
#    if (!is.list(tbl[[tbl_names[i]]])) {
      
#      tbl[[tbl_names[i]]]
      
#    } else {
      
#      map_chr(1:nrow(tbl), \(j) paste0("{", toString(tbl[[tbl_names[i]]][[j]]), "}"))
      
#    }
    
#  })
  
#  names(lst) <- tbl_names
  
#  types <- map_chr(1:length(lst), \(i) {
    
#    if (!is.list(tbl[[tbl_names[i]]])) {
      
#      if (!is.integer(tbl[[tbl_names[i]]])) {
        
#        "text"
        
#      } else {
        
#        "int2"
        
#      }
      
#    } else {
      
#      "text[]"
      
#    }
    
#  })
  
#  pgTools::INSERT(con = con,
#                  returning = NULL,
#                  schema = "sport_data",
#                  table = dt_name,
#                  quote_text = T,
#                  double_quote_names = T,
#                  cast = T,
#                  x = lst,
#                  types = types)
  
#  dbDisconnect(con)
  
#}

request(paste0(host_flashlive,
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
                        max_reqs = 5)

discs <- tibble(disc = repurrrsive::discog) %>% 
  unnest_wider(disc) %>% 
  mutate(date_added = as.POSIXct(strptime(date_added, "%Y-%m-%dT%H:%M:%S")))

discs %>% 
  hoist(basic_information,
        title = "title",
        year = "year",
        label = list("labels", 1, "name"),
        artist = list("artists", 1, "name")
  )

discs |> 
  select(-id) |>
  unnest_wider(basic_information)

DT <- data.table(dbGetQuery(con, 'select "name", name_part_1, name_part_2, unnest(events_event_id) "events_event_id",
                                      unnest(events_start_time) "events_start_time", unnest(events_start_utime) "events_start_utime",
                                      unnest(events_stage_type) "events_stage_type", unnest(events_stage) "events_stage",
                                      unnest(events_home_name) "events_home_name", unnest(events_away_name) "events_away_name",
                                      unnest(events_home_score_current) "events_home_score_current", 
                                      unnest(events_away_score_current) "events_away_score_current",
                                      unnest(events_home_score_part_1) "events_home_score_part_1",
                                      unnest(events_home_score_part_2) "events_home_score_part_2",
                                      unnest(events_home_score_part_3) "events_home_score_part_3",
                                      unnest(events_away_score_part_1) "events_away_score_part_1",
                                      unnest(events_away_score_part_2) "events_away_score_part_2",
                                      unnest(events_away_score_part_3) "events_away_score_part_3"
                                  from sport_data.event_list_hockey_3_days_ago;'))

regr <- report[order(-sum_goods), .(vendor_code, rank = 1:.N, relation)]

scatter.smooth(regr$rank, regr$relation, xlab = "rank", ylab = "relation", lpars = list(col = "red"))

a = 0.5

b = -1.5

c = sample(0.5:8.5, 1)

# generate data
x <- c(0, 1, 2, 3, 4, 5)
y <- c(1, 2, 4, 8, 16, 32)

# fit the model 
start_values <- c(a = 4, b = 2)

fit <- nls(y ~ a * exp(b * x),
           start = start_values,
           algorithm = "port",
           control = nls.control(maxiter = 1000))

summary(fit)

model <- glm(regr$relation~(a * exp(b * regr$rank)))

summary(model)

regr_new <- regr |>
  mutate(theory_relation = round(model$coefficients[1] + model$coefficients[2] * rank, 2))

regr_new

data('Boston', package = 'MASS')

set.seed(123)

Boston <- as.data.table(Boston)

training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- Boston[training.samples]

test.data <- Boston[-training.samples]

ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth()

##Linear regression
# Build the model
model <- lm(medv ~ lstat, data = train.data)

# Make predictions
predictions <- model %>% predict(test.data)

# Model performance
data.table(
  
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)

  )

ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)

#Alt poly regr
lm(medv ~ poly(lstat, 2, raw = TRUE), data = train.data)

# Build the model
model1 <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = train.data)

# Make predictions
predictions1 <- model1 %>% predict(test.data)

# Model performance
data.table(
  
  RMSE = RMSE(predictions1, test.data$medv),
  R2 = R2(predictions1, test.data$medv)

)

ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))

# Build the model
model2 <- lm(medv ~ log(lstat), data = train.data)

# Make predictions
predictions2 <- model2 %>% predict(test.data)

# Model performance
data.table(
  
  RMSE = RMSE(predictions2, test.data$medv),
  R2 = R2(predictions2, test.data$medv)
  
)

ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ log(x))

knots <- quantile(train.data$lstat, p = c(0.25, 0.5, 0.75))

# Build the model
model3 <- lm(medv ~ bs(lstat, knots = knots), data = train.data)

# Make predictions
predictions3 <- model3 %>% predict(test.data)

# Model performance
data.table(
  
  RMSE = RMSE(predictions3, test.data$medv),
  R2 = R2(predictions3, test.data$medv)
  
)

ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ bs(x, df = 3))

# Build the model
model4 <- gam(medv ~ s(lstat), data = train.data)

# Make predictions
predictions4 <- model4 %>% predict(test.data)

# Model performance
data.table(
  
  RMSE = RMSE(predictions4, test.data$medv),
  R2 = R2(predictions4, test.data$medv)
  
)

ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))

rows <- 1:nrow(dt_pre)

# Splitting data into train and test data 
partition <- createDataPartition(rows, p = 0.7)$Resample1
train_cl <- dt_pre[partition] 
test_cl <- dt_pre[-partition]

# Feature Scaling 
train_scale <- scale(train_cl[, -c(1:5)] |> mutate_if(is.character, as.numeric)) 
test_scale <- scale(test_cl[, -c(1:5)] |> mutate_if(is.character, as.numeric))

# output to be present as PNG file 
png(file = "KMeansExampleSoccer.png")

# output to be present as PNG file 
png(file = "KMeansExampleSoccer_new.png")

kmspp <- LICORS::kmeanspp(dt, k = 3, nstart = 25, start = 3)

km <- kmeans(dt, centers = 2, nstart = 25)

DT_liv <- data.table(kmeanspp2 = kmspp$cluster,
                     kmeanspp2.1 = ifelse(kmspp$cluster %in% c(1, 2),
                                          'WIN',
                                          'LOST'),
                     dt_pre[, .(h_result,
                                start_time,
                                home_participant_name_one,
                                away_participant_name_one)]) |>
  mutate(res = ifelse(kmeanspp2.1 == h_result, 'yes' , 'no'))

1 - DT_liv$res[DT_liv$res == 'no'] |> length() / (DT_liv$res |> length())

DT_new <- data.table(kmeanspp2 = kmspp$cluster,
                     kmeanspp2.1 = ifelse(kmspp$cluster %in% c(1, 2),
                                          'WIN',
                                          'LOST'),
                     dt_pre[, .(h_result,
                                start_time,
                                home_participant_name_one,
                                away_participant_name_one)]) |>
  mutate(res = ifelse(kmeanspp2.1 == h_result, 'yes', 'no'))

1 - DT_new$res[DT_new$res == 'no'] |> length() / (DT_new$res |> length())

# Visualize the clusters
fviz_cluster(kmspp, data = dt)

# saving the file 
dev.off()

# Loading dataset
dt_pre <- for_analyze_for_cycle_away[order(-ymd_hms(start_time)), .(home_participant_name_one,
                                                                    delta = home_score_full - away_score_full,
                                                                    away_participant_name_one,
                                                                    start_time,
                                                                    h_result,
                                                                    incident_name,
                                                                    value_away)] |>
  pivot_wider(names_from = incident_name,
              values_from = value_away,
              values_fn = c,
              values_fill = as.vector(0.001)) |>
  as.data.table()

rows <- 1:nrow(dt)

# Splitting data into train and test data 
partition <- createDataPartition(rows, p = 0.7)$Resample1
train_cl <- dt[partition] 
test_cl <- dt[-partition]

# Feature Scaling 
train_scale <- scale(train_cl[, -c(1:5)] |> mutate_if(is.character, as.numeric)) 
test_scale <- scale(test_cl[, -c(1:5)] |> mutate_if(is.character, as.numeric)) 

# Fitting KNN Model to training dataset 
classifier_knn <- class::knn(train = as.data.table(train_scale) |> mutate_if(is.numeric, ~ replace_na(., 0)), 
                             test = as.data.table(test_scale) |> mutate_if(is.numeric, ~ replace_na(., 0)), 
                             cl = train_cl$h_result, 
                             k = 2)
# Fuzzy clustering ----
data <- data.table(read.csv(list.files(getwd(), pattern = 'csv')))

head(data)

# Handle missing values
data <- na.omit(data)

data_for_clustering <- data[, c("QUANTITYORDERED", "PRICEEACH", "SALES", "MSRP")]

set.seed(123)

n_cluster <- 5

m <- 2

result <- cmeans(data_for_clustering, centers = n_cluster, m = m)

# Data Membership Degree Matrix
fuzzy_membership_matrix <- result$membership

# Cluster Prototype Evolution Matrices
initial_centers <- result$centers
final_centers <- t(result$centers)

cluster_membership <- as.data.table(result$membership)
data_with_clusters <- cbind(data, cluster_membership)
head(data_with_clusters)

# Factor analyze ----
data('gss12_values')
gss <- data.table(gss12_values)

gss_num <- gss |>
  mutate_if(is.factor, as.integer)

heatmap(polychoric(gss_num)$rho)
mardia(gss_num) # testing data for multivariate normality, if skew and kurtosis ~ 0 => data is not multivariate normality

KMO(gss_num)
bartlett.test(gss_num)

scree(gss_num)
vss(gss_num)
fa.parallel(gss_num)

mardia(DT$cluster)
KMO(DT$cluster)
bartlett.test(DT$forecast)

scree(DT$forecast)
vss(DT$forecast)
fa.parallel(DT$forecast)

nfactors(
  
  cor(DT$forecast),
  n.obs = nrow(DT$forecast)
  
)

FA <- fa(
  
  DT$cluster,
  nfactors = 2,
  rotate = 'cluster',
  scores = 'tenBerge'
  
)
