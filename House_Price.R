library(tidyverse)

df_full <- dataset

str(df_full)

################################################################################

#DATA CLEANING & EXPLORATION

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

cols_to_factors <- grep('^view_', names(df), value = TRUE)

df[cols_to_factors] <- lapply(df[cols_to_factors], factor)
































