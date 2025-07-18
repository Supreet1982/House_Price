library(tidyverse)
library(MASS)
library(fitdistrplus)
library(e1071)
library(GGally)
library(effects)
library(caret)
library(car)
library(broom)
library(pdp)
library(Metrics)
library(dplyr)
library(doParallel)
library(xgboost)
library(future.apply)
library(Matrix)


df_full <- dataset

str(df_full)

################################################################################

#DATA CLEANING

#Remove columns

df <- df_full[, -c(1,4,5,6,7,8,9,14,18,24,25,31,47)]

#Combine baths and remove columns

df$bath <- df$bath_full + (0.75*df$bath_3qtr) + (0.5*df$bath_half)
df <- df[,-c(17,18,19)]

#Combine land and improvement values and remove columns

df$Tot_val <- df$land_val + df$imp_val
df <- df[, -c(7,8)]

#Extract Sale_year and remove sale_date

df$Sale_year <- format(as.Date(df$sale_date), '%Y')
df$sale_date <- NULL
df$Sale_year <- as.numeric(df$Sale_year)

df$Pro_Age <- df$Sale_year - df$year_built
df$Sale_year <- NULL
df$year_built <- NULL

#Convert Negative property age to zero

df$Pro_Age[df$Pro_Age < 0] <- 0

#sqft manipulation

cor(df[,c('sqft', 'sqft_lot', 'sqft_1', 'sqft_fbsmt')])

#Create Basement Ratio

df$bmt_ratio <- df$sqft_fbsmt / df$sqft

#Create Lot Efficiency and remove sqft columns

df$lot_eff <- df$sqft / df$sqft_lot

df$sqft <- NULL
df$sqft_1 <- NULL
df$sqft_lot <- NULL
df$sqft_fbsmt <- NULL

#Remove N/As

df <- na.omit(df)

#Garage

df$has_garage <- as.integer(df$gara_sqft > 0)
df$log_garage <- log1p(df$gara_sqft)
df$has_garage <- as.factor(df$has_garage)

df$gara_sqft <- NULL

#Stories

df$stories_capped <- pmin(df$stories, 3)
df$stories_capped <- as.factor(df$stories_capped)
levels(df$stories_capped)[levels(df$stories_capped) == '3'] <- '3+'
df$stories <- NULL

#Factorize predictors

df$noise_traffic <- factor(df$noise_traffic)
df$greenbelt <- factor(df$greenbelt)
df$golf <- factor(df$golf)
df$wfnt <- as.factor(df$wfnt)

cols_to_factors <- grep('^view_', names(df), value = TRUE)

df[cols_to_factors] <- lapply(df[cols_to_factors], factor)

#Grade

df$Grade_group <- cut(df$grade, breaks = c(0,5,7,10,13),
                      labels = c('low', 'Average', 'Above Average', 'Luxury'),
                      right = TRUE)
df$grade <- NULL

#Beds

df$beds_grouped <- ifelse(df$beds >= 6, '6+', as.character(df$beds))
df$beds_grouped <- as.factor(df$beds_grouped)

df$beds <- NULL

#Area (Use k-means)

area_features <- df %>%
  group_by(area) %>%
  summarize(
    bath_mean = mean(bath),
    age_mean = mean(Pro_Age),
    bmt_mean = mean(bmt_ratio),
    lot_eff_mean = mean(lot_eff),
    garage_mean = mean(log_garage),
    has_garage_pct = mean(as.numeric(as.character(has_garage))),
    traffic_mean = mean(as.numeric(as.character(noise_traffic))),
    greenbelt_pct = mean(as.numeric(as.character(greenbelt))),
    stories_mode = as.numeric(names(which.max(table(stories_capped))))
  )
  
area_scaled <- scale(area_features[,-1])

set.seed(1)

km1 <- kmeans(area_scaled, centers = 1, nstart = 25)
km2 <- kmeans(area_scaled, centers = 2, nstart = 25)
km3 <- kmeans(area_scaled, centers = 3, nstart = 25)
km4 <- kmeans(area_scaled, centers = 4, nstart = 25)
km5 <- kmeans(area_scaled, centers = 5, nstart = 25)
km6 <- kmeans(area_scaled, centers = 6, nstart = 25)
km7 <- kmeans(area_scaled, centers = 7, nstart = 25)
km8 <- kmeans(area_scaled, centers = 8, nstart = 25)
km9 <- kmeans(area_scaled, centers = 9, nstart = 25)
km10 <- kmeans(area_scaled, centers = 10, nstart = 25)

var_exp <- data.frame(K = 1:10,
                      bss_tss = c(km1$betweenss/km1$totss,
                                  km2$betweenss/km2$totss,
                                  km3$betweenss/km3$totss,
                                  km4$betweenss/km4$totss,
                                  km5$betweenss/km5$totss,
                                  km6$betweenss/km6$totss,
                                  km7$betweenss/km7$totss,
                                  km8$betweenss/km8$totss,
                                  km9$betweenss/km9$totss,
                                  km10$betweenss/km10$totss))

var_exp %>%
  ggplot(aes(K, bss_tss)) +
  geom_point() +
  geom_line() +
  ggtitle('Elbow Plot')
  
area_features$cluster <- factor(km4$cluster)

df <- left_join(df, area_features[,c('area', 'cluster')], by = 'area')
df$area_cluster <- as.factor(df$cluster)

df$area = NULL

attr(df, 'na.action') <- NULL

#Remove variables

df$city <- NULL
df$zoning <- NULL
df$subdivision <- NULL
df$cluster <- NULL

df2 <- df

################################################################################

#DATA EXPLORATION

#UNIVARIATE ANALYSIS

#Target Distribution

df %>%
  ggplot(aes(sale_price)) +
  geom_histogram(bins=100)

#Check whether the target variable follows Gamma Distribution

descdist(df$sale_price[df$sale_price > 0], boot = 1000)

fit <- fitdist(df$sale_price, 'gamma')
fit$estimate
gofstat(fit)

#Predictor variables analysis

cat_vars <- names(df)[sapply(df, is.factor)]

#Relevel categorical predictors

for (i in cat_vars) {
  tab <- as.data.frame(table(df[,i]))
  max <- which.max(tab[,2])
  level_name <- as.character(tab[max,1])
  df[,i] <- relevel(df[,i], ref = level_name)
}

summary(df[,cat_vars])

#Numeric Variables

num_vars <- names(df)[sapply(df, is.numeric)]

#Check skewness

skewness_values <- sapply(df[,num_vars], skewness)

skew_df <- data.frame(
  Variable = names(skewness_values),
  Skewness = skewness_values
)

write.csv(skew_df, 'Skewness_values.csv', row.names = TRUE)

#BIVARIATE ANALYSIS

#Numeric/Numeric

num_df <- df[sapply(df, is.numeric)]

ggpairs_plot <- ggpairs(sample_n(num_df, 1000))

ggsave('corr_plot.png', plot = ggpairs_plot, width = 10, height = 8, dpi = 300,
       units = 'in')

#Further Modifications

df$log_Tot_val <- log1p(df$Tot_val)
df$sqrt_Pro_Age <- sqrt(df$Pro_Age)
df$log_lot_eff <- log1p(df$lot_eff)

df$Tot_val <- NULL
df$Pro_Age <- NULL
df$lot_eff <- NULL

df %>%
  ggplot(aes(stories_capped, sale_price)) +
  geom_point()

#Remove zero values

df <- df[df$log_Tot_val != 0,]

#Numeric/Categorical

df %>%
  ggplot(aes(stories_capped, sale_price)) + 
  geom_boxplot(fill='lightblue')

df %>%
  ggplot(aes(Grade_group, sale_price)) +
  geom_boxplot(fill='lightblue')

df %>%
  ggplot(aes(area_cluster, sale_price)) +
  geom_boxplot(fill='lightblue')

df %>%
  ggplot(aes(wfnt, sale_price)) +
  geom_boxplot(fill='lightblue')

#Interactions

################################################################################

#MODEL DEVELOPMENT

set.seed(1234)

partition <- createDataPartition(df$sale_price, p = 0.7, list = FALSE)

df_train <- df[partition,]
df_test <- df[-partition,]

mean(df_train$sale_price)
mean(df_test$sale_price)

glm1 <- glm(sale_price ~ ., data = df_train, family = Gamma(link = 'log'))
summary(glm1)

#MODEL DIAGNOSTICS

res_dev <- residuals(glm1, type = 'deviance')
res_pearson <- residuals(glm1, type = 'pearson')
fitted_vals <- fitted(glm1)

plot(fitted_vals, res_dev, xlab = 'Fitted Value', ylab = 'Deviance Residual',
     main = 'Fitted vs Deviance Residual')
abline(h=0, col='red')

qqnorm(res_dev)
qqline(res_dev, col='red')

cooksD <- cooks.distance(glm1)
hat_values <- hatvalues(glm1)

plot(cooksD, type = 'h', main = "Cook's Distance")
abline(h = 4 / nrow(df_train), col='red', lty=2)

plot(hat_values, main = 'Leverage (Hat values)')
abline(h = 2 * mean(hat_values), col='red', lty = 2)

influencePlot(glm1, id.method = 'identity')

#Create a model without the influential point

which(as.numeric(rownames(df_train)) == 165031)

glm_reduced <- glm(sale_price ~ ., family = Gamma(link = 'log'),
                   data = df_train[-114817,])
summary(glm_reduced)

cooksD_red <- cooks.distance(glm_reduced)
hat_values_red <- hatvalues(glm_reduced)

plot(cooksD_red, type = 'h', main = "Cook's Distance reduced GLM")
abline(h = 4 / nrow(df_train), col='red', lty = 2)

plot(hat_values_red, main = 'Leverage (Hat values) Reduced GLM')
abline(h = 2 * mean(hat_values_red), col = 'red', lty = 2)

influencePlot(glm_reduced, id.method='identity')

res_dev_red <- residuals(glm_reduced, type = 'deviance')
fitted_vals_red <- fitted(glm_reduced)

plot(fitted_vals_red, res_dev_red, xlab = 'Fitted Value', 
     ylab = 'Deviance Residual', main = 'Fitted vs Deviance Residual')
abline(h=0, col='red')

qqnorm(res_dev_red)
qqline(res_dev_red, col='red')

#Predictive Accuracy

pred <- predict(glm1, newdata = df_test, type = 'response')
pred_red <- predict(glm_reduced, newdata = df_test, type = 'response')
actual <- df_test$sale_price

RMSE(pred, actual)
RMSE(pred_red, actual)

cbind(
  term = tidy(glm1)$term,
  estimate_full = tidy(glm1)$estimate,
  estimate_reduced = tidy(glm_reduced)$estimate
)

#Overdispersion check

resid_dev <- deviance(glm1)
df_resid <- df.residual(glm1)
dispersion <- resid_dev / df_resid
cat('Dispersion:',dispersion)

resid_dev_red <- deviance(glm_reduced)
df_resid_red <- df.residual(glm_reduced)
dispersion_red <- resid_dev_red / df_resid_red
cat('Dispersion:', dispersion_red)

#Dispersion = 0.2409229

################################################################################

#XGBoost model with Log of target variable, since the target variable is
#right skewed

xgb_ctrl <- trainControl(method = 'cv', number = 5)
xgb_grid <- expand.grid(max_depth = 7, min_child_weight = 1, gamma = 0,
                        nrounds = c(50,100,150,200,250,300),
                        eta = c(0.001, 0.002, 0.01, 0.02, 0.1),
                        colsample_bytree = 0.6, subsample = 0.6)

set.seed(42)

xgb_tuned <- train(log(sale_price) ~ ., data = df_train, method = 'xgbTree',
                   trControl = xgb_ctrl, tuneGrid = xgb_grid)

summary(xgb_tuned)

ggplot(varImp(xgb_tuned), top = 5)

log_pred <- predict(xgb_tuned, newdata = df_test)
final_pred <- exp(log_pred)

RMSE(final_pred, df_test$sale_price)

partial(xgb_tuned, train = df_train, pred.var = 'log_Tot_val', plot = TRUE)
partial(xgb_tuned, train = df_train, pred.var = 'sqrt_Pro_Age', plot = TRUE)
partial(xgb_tuned, train = df_train, pred.var = 'bath', plot = TRUE)

#Prediction Interval

xgb_grid_fixed <- xgb_tuned$bestTune
xgb_ctrl_boot <- trainControl(method = 'none')

bootstrap_preds_with_actuals <- function(model_formula, data, newdata, B = 100,
                                         tr_ctrl, tune_grid, seed = 123,
                                         add_residual = TRUE) {
  set.seed(seed)
  
  #store actuals
  actuals <- newdata$sale_price
  newdata_noy <- newdata[, setdiff(names(newdata), 'sale_price')]
  
  n <- nrow(newdata_noy)
  preds_mat <- matrix(NA, nrow = n, ncol = B)
  
  for (i in 1:B) {
    sample_idx <- sample(1:nrow(data), replace = TRUE)
    boot_data <- data[sample_idx, ]
    
    boot_model <- train(model_formula, data = boot_data, method = 'xgbTree',
                        trControl = tr_ctrl, tuneGrid = tune_grid)
    
    preds <- predict(boot_model, newdata_noy) #log scale
    
    if (add_residual) {
      train_preds <- predict(boot_model, newdata = boot_data)
      train_resid <- log(boot_data$sale_price) - train_preds
      resid_sd <- sd(train_resid)
      preds <- preds + rnorm(length(preds), mean = 0, sd = resid_sd)
    }
    
    preds_mat[, i] <- preds
  }
  
  #Compute point estimate and interval on log scale
  point_est <- apply(preds_mat, 1, mean)
  lower <- apply(preds_mat, 1, quantile, probs = 0.025)
  upper <- apply(preds_mat, 1, quantile, probs = 0.975)
  
  pred_df <- data.frame(
    Predicted = exp(point_est),
    Lower_PI = exp(lower),
    Upper_PI = exp(upper),
    Actual = actuals
  )
  
  #Coverage indicator
  pred_df$Covered <- with(pred_df, Actual >= Lower_PI & Actual <= Upper_PI)
  
  #Summary metrics
  rmse_val <- rmse(pred_df$Actual, pred_df$Predicted)
  mae_val <- mae(pred_df$Actual, pred_df$Predicted)
  coverage <- mean(pred_df$Covered)
  
  cat("RMSE:", round(rmse_val, 2), "\n")
  cat("MAE:", round(mae_val, 2), "\n")
  cat("Coverage Rate (95% PI):", round(coverage, 2), "%\n")
  
  return(pred_df)
}

results <- bootstrap_preds_with_actuals(
  model_formula = log(sale_price) ~ ., data = df_train,
  newdata = df_test, B = 100, tr_ctrl = xgb_ctrl_boot, tune_grid = xgb_grid_fixed
)

#RMSE: 270543.2 
#MAE: 170961 
#Coverage Rate (95% PI): 0.21 %

results

################################################################################

#XGBOOST

df$log_sale_price <- log(df$sale_price)
df_train <- df[partition,]
df_test <- df[-partition,]

mean(df_train$log_sale_price)
mean(df_test$log_sale_price)

target_var <- 'log_sale_price'

train_y <- df_train[[target_var]]

train_x <- df_train[, !(names(df_train) %in% c(target_var, 'sale_price')), 
                        drop = FALSE]

#Convert to numeric matrix

train_x_mat <- model.matrix(~ . -1, data = train_x)

#Dmatrix

dtrain <- xgb.DMatrix(data = train_x_mat, label = train_y)

#Test data

test_x <- df_test[, names(train_x), drop = FALSE]
test_x_mat <- model.matrix(~ . - 1, data = test_x)
dtest <- xgb.DMatrix(data = test_x_mat)

#Define eta grid

eta_grid <- c(0.01, 0.05, 0.1, 0.2)

#Run CV for each eta

cv_results <- lapply(eta_grid, function(eta_val) {
  cv <- xgb.cv(
    data = dtrain,
    objective = "reg:squarederror",
    nrounds = 1000, #large upper limit
    eta = eta_val,
    max_depth = 6,
    nfold = 6,
    early_stopping_rounds = 20,
    verbose = 0
  )
  
  list(
    eta = eta_val,
    best_nrounds = cv$best_iteration,
    best_rmse = min(cv$evaluation_log$test_rmse_mean)
  )
})

#see results

cv_table <- do.call(rbind, cv_results)
print(cv_table)

# Pick the best

best_idx <- which.min(sapply(cv_results, function(x) x$best_rmse))
best_eta <- cv_results[[best_idx]]$eta
best_nrounds <- cv_results[[best_idx]]$best_nrounds

print(best_eta)
print(best_nrounds)

# Final model

train_x_sparse <- Matrix(train_x_mat, sparse = TRUE)
test_x_sparse <- Matrix(test_x_mat, sparse = TRUE)

dim(train_x_sparse)
dim(train_x_mat)
dim(test_x_mat)
dim(test_x_sparse)

dtrain_2 <- xgb.DMatrix(data = train_x_sparse, label = train_y)
dtest_2 <- xgb.DMatrix(data = test_x_sparse)

final_model <- xgboost(
  data = dtrain_2,
  objective = 'reg:squarederror',
  nrounds = best_nrounds,
  eta = best_eta,
  max_depth = 6,
  nthread = 2,
  verbose = 1,
  tree_method = 'hist'
)

#Predict on Train and test data

mean_preds_log <- predict(final_model, dtrain_2)
mean_preds_log_test <- predict(final_model, dtest_2)

#Back transform

mean_preds <- exp(mean_preds_log)
mean_preds_test <- exp(mean_preds_log_test)

rmse(df_test$sale_price, mean_preds_test)

rmse(mean_preds, exp(train_y))

#Prediction Interval

#Parallel plan

future::plan(multisession)

n_boot <- 100    #Adjust for your resources

boot_pred <- future_sapply(1:n_boot, function(i) {
  idx <- sample(1:nrow(train_x), replace = TRUE)
  
  dtrain_boot <- xgb.DMatrix(data = train_x_mat[idx,], label = train_y[idx])
  
  dtest_local <- xgb.DMatrix(data = test_x_mat)
  
  model_boot <- xgboost(
    data = dtrain_boot,
    objective = 'reg:squarederror',
    nrounds = best_nrounds,
    eta = best_eta,
    max_depth = 6,
    nthread = 1,
    verbose = 0
  )
  
  predict(model_boot, dtest_local)
},
future.seed = TRUE  
)

#Compute log scale intervals

lower_log <- apply(boot_pred, 1, quantile, probs = 0.025)
upper_log <- apply(boot_pred, 1, quantile, probs = 0.975)

#Back_transform

lower <- exp(lower_log)
upper <- exp(upper_log)

#Combine

results <- data.frame(
  mean_preds = mean_preds_test,
  lower = lower,
  upper = upper
)

head(results)

write.csv(df, 'df.csv')

################################################################################

#Test data modifications

test_set <- test

test_df <- test_set

test_df <- test_df[, -c(1,3,4,5,6,7,8,13,17,23,24,30,46)]
test_df$bath <- test_df$bath_full + (0.75*test_df$bath_3qtr) + 
  (0.5*test_df$bath_half)

test_df <- test_df[,-c(16,17,18)]

test_df$Tot_val <- test_df$land_val + test_df$imp_val
test_df <- test_df[, -c(6,7)]

test_df$Sale_year <- format(as.Date(test_df$sale_date), '%Y')
test_df$sale_date <- NULL
test_df$Sale_year <- as.numeric(test_df$Sale_year)

test_df$Pro_Age <- test_df$Sale_year - test_df$year_built
test_df$Sale_year <- NULL
test_df$year_built <- NULL

test_df$Pro_Age[test_df$Pro_Age < 0] <- 0

test_df$bmt_ratio <- test_df$sqft_fbsmt / test_df$sqft

mean_sqft_lot_t <- mean(test_df$sqft_lot[test_df$sqft_lot != 0])
test_df$sqft_lot[test_df$sqft_lot == 0] <- mean_sqft_lot_t

test_df$lot_eff <- test_df$sqft / test_df$sqft_lot

test_df$sqft <- NULL
test_df$sqft_1 <- NULL
test_df$sqft_lot <- NULL
test_df$sqft_fbsmt <- NULL

#Remove N/As

test_df <- na.omit(test_df)

test_df$has_garage <- as.integer(test_df$gara_sqft > 0)
test_df$log_garage <- log1p(test_df$gara_sqft)
test_df$has_garage <- as.factor(test_df$has_garage)

test_df$gara_sqft <- NULL

#Stories

test_df$stories_capped <- pmin(test_df$stories, 3)
test_df$stories_capped <- as.factor(test_df$stories_capped)
levels(test_df$stories_capped)[levels(test_df$stories_capped) == '3'] <- '3+'
test_df$stories <- NULL

test_df$noise_traffic <- factor(test_df$noise_traffic)
test_df$greenbelt <- factor(test_df$greenbelt)
test_df$golf <- factor(test_df$golf)
test_df$wfnt <- as.factor(test_df$wfnt)

cols_to_factors_t <- grep('^view_', names(test_df), value = TRUE)

df[cols_to_factors_t] <- lapply(df[cols_to_factors_t], factor)

test_df$Grade_group <- cut(test_df$grade, breaks = c(0,5,7,10,13),
                      labels = c('low', 'Average', 'Above Average', 'Luxury'),
                      right = TRUE)
test_df$grade <- NULL

#Beds

test_df$beds_grouped <- ifelse(test_df$beds >= 6, '6+', as.character(test_df$beds))
test_df$beds_grouped <- as.factor(test_df$beds_grouped)

test_df$beds <- NULL

#Area (Use k-means)

area_features_t <- test_df %>%
  group_by(area) %>%
  summarize(
    bath_mean = mean(bath),
    age_mean = mean(Pro_Age),
    bmt_mean = mean(bmt_ratio),
    lot_eff_mean = mean(lot_eff),
    garage_mean = mean(log_garage),
    has_garage_pct = mean(as.numeric(as.character(has_garage))),
    traffic_mean = mean(as.numeric(as.character(noise_traffic))),
    greenbelt_pct = mean(as.numeric(as.character(greenbelt))),
    stories_mode = as.numeric(names(which.max(table(stories_capped))))
  )

area_scaled_t <- scale(area_features_t[,-1])

set.seed(1)

km1_t <- kmeans(area_scaled_t, centers = 1, nstart = 25)
km2_t <- kmeans(area_scaled_t, centers = 2, nstart = 25)
km3_t <- kmeans(area_scaled_t, centers = 3, nstart = 25)
km4_t <- kmeans(area_scaled_t, centers = 4, nstart = 25)
km5_t <- kmeans(area_scaled_t, centers = 5, nstart = 25)
km6_t <- kmeans(area_scaled_t, centers = 6, nstart = 25)
km7_t <- kmeans(area_scaled_t, centers = 7, nstart = 25)
km8_t <- kmeans(area_scaled_t, centers = 8, nstart = 25)
km9_t <- kmeans(area_scaled_t, centers = 9, nstart = 25)
km10_t <- kmeans(area_scaled_t, centers = 10, nstart = 25)

var_exp_t <- data.frame(K = 1:10,
                      bss_tss = c(km1_t$betweenss/km1$totss,
                                  km2_t$betweenss/km2$totss,
                                  km3_t$betweenss/km3$totss,
                                  km4_t$betweenss/km4$totss,
                                  km5_t$betweenss/km5$totss,
                                  km6_t$betweenss/km6$totss,
                                  km7_t$betweenss/km7$totss,
                                  km8_t$betweenss/km8$totss,
                                  km9_t$betweenss/km9$totss,
                                  km10_t$betweenss/km10$totss))

var_exp_t %>%
  ggplot(aes(K, bss_tss)) +
  geom_point() +
  geom_line() +
  ggtitle('Elbow Plot')

area_features_t$cluster <- factor(km3_t$cluster)

test_df <- left_join(test_df, area_features[,c('area', 'cluster')], by = 'area')
test_df$area_cluster <- as.factor(test_df$cluster)

test_df$area = NULL

attr(test_df, 'na.action') <- NULL

test_df$city <- NULL
test_df$zoning <- NULL
test_df$subdivision <- NULL
test_df$cluster <- NULL

test_df_final <- test_df

test_df$log_Tot_val <- log1p(test_df$Tot_val)
test_df$sqrt_Pro_Age <- sqrt(test_df$Pro_Age)
test_df$log_lot_eff <- log1p(test_df$lot_eff)

test_df$Tot_val <- NULL
test_df$Pro_Age <- NULL
test_df$lot_eff <- NULL


mean_log_tot_val_t <- mean(test_df$log_Tot_val[test_df$log_Tot_val != 0])
test_df$log_Tot_val[test_df$log_Tot_val == 0] <- mean_log_tot_val_t

test_df_final <- test_df

write.csv(test_df, 'test_df.csv')

################################################################################

glm_new <- glm(sale_price ~ log_Tot_val + sqrt_Pro_Age + bath + Grade_group +
                 log_lot_eff, data = df, family = Gamma(link = 'log'))

mu_hat <- predict(glm_new, newdata = test_df, type = 'response')
phi_hat <- summary(glm_new)$dispersion

shape_hat <- 1 / phi_hat
scale_hat <- mu_hat * phi_hat

lower_pi <- qgamma(0.025, shape = shape_hat, scale = scale_hat)
upper_pi <- qgamma(0.975, shape = shape_hat, scale = scale_hat)

pi_data <- data.frame(
  id = test$id,
  pi_lower = lower_pi,
  pi_upper = upper_pi
)

str(pi_data)

write.csv(pi_data, 'submission_3.csv')

################################################################################

df_glm <- df
df_glm$log_sale_price <- NULL

test_df[cols_to_factors] <- lapply(test_df[cols_to_factors], factor)

glm_new_2 <- glm(sale_price ~ ., data = df_glm, family = Gamma(link = 'log'))

mu_hat_2 <- predict(glm_new_2, newdata = test_df, type = 'response')
phi_hat_2 <- summary(glm_new_2)$dispersion

shape_hat_2 <- 1 / phi_hat_2
scale_hat_2 <- mu_hat_2 * phi_hat_2

lower_pi_2 <- qgamma(0.025, shape = shape_hat_2, scale = scale_hat_2)
upper_pi_2 <- qgamma(0.975, shape = shape_hat_2, scale = scale_hat_2)

pi_data_2 <- data.frame(
  id = test$id,
  pi_lower = lower_pi_2,
  pi_upper = upper_pi_2
)

write.csv(pi_data_2, 'submission_5.csv')

################################################################################





