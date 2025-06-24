library(tidyverse)

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

################################################################################

#DATA EXPLORATION








