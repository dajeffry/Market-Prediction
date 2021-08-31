library(readxl)
# install.packages("tidyverse")
# install.packages("libridate")


library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)
library(ggplot2)

bread_df <- read_excel("data/bread-basket.xlsx")
dim(bread_df)
str(bread_df)
head(bread_df)
summary(bread_df)

bread_df <- bread_df[complete.cases(bread_df),]
bread_df$Item <- as.factor(bread_df$Item)


bread_df$Date <- as.Date(bread_df$date_time)


bread_df$Transaction <- as.numeric(bread_df$Transaction)
head(bread_df)
summary(bread_df)

# Univariate Data Analysis

ggplot(data=bread_df, aes(x = weekday_weekend)) +
  geom_bar(color = "blue")

# Bivariate Data Analysis

ggplot(bread_df, aes(x=weekday_weekend, fill = period_day)) +
  geom_bar(position = "dodge")


bread_df_sorted <- bread_df[order(bread_df$Transaction), ]

itemlist <- ddply(bread_df, c("Transaction"),
                  function(df1)paste(df1$Item, collapse = ","))

itemlist$Transaction <- NULL
colnames(itemlist) <- c("items")

# Write to csv file
write.csv(itemlist, "data/bread basket.csv", 
          quote = FALSE,
          row.names = TRUE)

# IMPORT AS TRANSACTION
tr_df <- read.transactions("data/bread basket.csv",
                           format = "basket",
                           sep = ",")

tr_df
summary(tr_df)
tr1_df <- itemFrequencyPlot(tr_df, topN = 10)

rules <- apriori(tr_df, parameter = list(supp = 0.001,
                                         conf = 0.8, minlen = 2))
rules <- sort(rules, by="confidence", decreasing = TRUE)
rules
inspect(rules)

plot(rules)
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "paracoord")

# Transaksi Weekdays

bread_weekday_df <- bread_df %>%
  filter(weekday_weekend=="weekday")

str(bread_weekday_df)
head(bread_weekday_df)
summary(bread_weekday_df)

itemlist_weekday <- ddply(bread_weekday_df, c("Transaction"),
                  function(df1)paste(df1$Item, collapse = ","))

itemlist_weekday$Transaction <- NULL
colnames(itemlist_weekday) <- c("items")

# Write to csv file
write.csv(itemlist_weekday, "data/bread basket weekday.csv", 
          quote = FALSE,
          row.names = TRUE)

# IMPORT AS TRANSACTION
bread_weekday_tr_df <- read.transactions("data/bread basket weekday.csv",
                           format = "basket",
                           sep = ",")

bread_weekday_tr_df
summary(bread_weekday_tr_df)
itemFrequencyPlot(bread_weekday_tr_df, topN = 10,)

rules_weekday <- apriori(bread_weekday_tr_df, parameter = list(supp = 0.001,
                                          conf = 0.8, minlen = 2))
rules_weekday <- sort(rules_weekday, by="confidence", decreasing = TRUE)
rules_weekday
inspect(rules_weekday)

plot(rules_weekday)
plot(rules_weekday,method = "grouped")
plot(rules_weekday,method = "graph")
plot(rules_weekday,method = "paracoord")


# Transaksi Weekend

bread_weekend_df <- bread_df %>%
  filter(weekday_weekend=="weekend")

str(bread_weekend_df)
head(bread_weekend_df)
summary(bread_weekend_df)

itemlist_weekend <- ddply(bread_weekend_df, c("Transaction"),
                          function(df1)paste(df1$Item, collapse = ","))

itemlist_weekend$Transaction <- NULL
colnames(itemlist_weekend) <- c("items")

# Write to csv file
write.csv(itemlist_weekend, "data/bread basket weekend.csv", 
          quote = FALSE,
          row.names = TRUE)

# IMPORT AS TRANSACTION
bread_weekend_tr_df <- read.transactions("data/bread basket weekend.csv",
                                      format = "basket",
                                      sep = ",")

bread_weekend_tr_df
summary(bread_weekend_tr_df)
itemFrequencyPlot(bread_weekend_tr_df, topN = 10)

rules_weekend <- apriori(bread_weekend_tr_df, parameter = list(supp = 0.002,
                                                            conf = 0.8, minlen = 2))
rules_weekend <- sort(rules_weekend, by="confidence", decreasing = TRUE)
rules_weekend
inspect(rules_weekend)

plot(rules_weekend)
plot(rules_weekend,method = "grouped")
plot(rules_weekend,method = "graph")
plot(rules_weekend,method = "paracoord")

# Transaksi weekday Morning

bread_morning <- bread_df
bread_morning$period_day <- as.factor(bread_morning$period_day)

bread_weekday_morning_df <- bread_weekday_df %>%
  filter(period_day=="morning")

str(bread_weekday_morning_df)
head(bread_weekday_morning_df)
summary(bread_weekday_morning_df)

itemlist_weekday_morning <- ddply(bread_weekday_morning_df, c("Transaction"),
                          function(df1)paste(df1$Item, collapse = ","))

itemlist_weekday_morning$Transaction <- NULL
colnames(itemlist_weekday_morning) <- c("items")

# Write to csv file
write.csv(itemlist_weekday_morning, "data/bread basket weekday morning.csv", 
          quote = FALSE,
          row.names = TRUE)

# IMPORT AS TRANSACTION
bread_weekday_morning_tr_df <- read.transactions("data/bread basket weekday morning.csv",
                                         format = "basket",
                                         sep = ",")

bread_weekday_morning_tr_df
summary(bread_weekday_morning_tr_df)
itemFrequencyPlot(bread_weekday_morning_tr_df, topN = 10)

rules_weekday_morning <- apriori(bread_weekday_morning_tr_df, parameter = list(supp = 0.002,
                                                               conf = 0.7, minlen = 2))
rules_weekday_morning <- sort(rules_weekday_morning, by="confidence", decreasing = TRUE)
rules_weekday_morning
inspect(rules_weekday_morning)

plot(rules_weekday_morning)
plot(rules_weekday_morning,method = "grouped")
plot(rules_weekday_morning,method = "graph")
plot(rules_weekday_morning,method = "paracoord")


# Transaksi Weekday Afternoon

bread_afternoon <- bread_df
bread_afternoon$period_day <- as.factor(bread_afternoon$period_day)

bread_weekday_afternoon_df <- bread_weekday_df %>%
  filter(period_day=="afternoon")

str(bread_weekday_afternoon_df)
head(bread_weekday_afternoon_df)
summary(bread_weekday_afternoon_df)

itemlist_weekday_afternoon <- ddply(bread_weekday_afternoon_df, c("Transaction"),
                                  function(df1)paste(df1$Item, collapse = ","))

itemlist_weekday_afternoon$Transaction <- NULL
colnames(itemlist_weekday_afternoon) <- c("items")

# Write to csv file
write.csv(itemlist_weekday_afternoon, "data/bread basket weekday afternoon.csv", 
          quote = FALSE,
          row.names = TRUE)

# IMPORT AS TRANSACTION
bread_weekday_afternoon_tr_df <- read.transactions("data/bread basket weekday afternoon.csv",
                                                 format = "basket",
                                                 sep = ",")

bread_weekday_afternoon_tr_df
summary(bread_weekday_afternoon_tr_df)
itemFrequencyPlot(bread_weekday_afternoon_tr_df, topN = 10)

rules_weekday_afternoon <- apriori(bread_weekday_afternoon_tr_df, parameter = list(supp = 0.002,
                                                                               conf = 0.7, minlen = 2))
rules_weekday_afternoon <- sort(rules_weekday_afternoon, by="confidence", decreasing = TRUE)
rules_weekday_afternoon
inspect(rules_weekday_afternoon)

plot(rules_weekday_afternoon)
plot(rules_weekday_afternoon,method = "grouped")
plot(rules_weekday_afternoon,method = "graph")
plot(rules_weekday_afternoon,method = "paracoord")

# Transaksi Weekday Evening

bread_evening <- bread_df
bread_evening$period_day <- as.factor(bread_evening$period_day)

bread_weekday_evening_df <- bread_weekday_df %>%
  filter(period_day=="evening")

str(bread_weekday_evening_df)
head(bread_weekday_evening_df)
summary(bread_weekday_evening_df)

itemlist_weekday_evening <- ddply(bread_weekday_evening_df, c("Transaction"),
                                    function(df1)paste(df1$Item, collapse = ","))

itemlist_weekday_evening$Transaction <- NULL
colnames(itemlist_weekday_evening) <- c("items")

# Write to csv file
write.csv(itemlist_weekday_evening, "data/bread basket weekday evening.csv", 
          quote = FALSE,
          row.names = TRUE)

# IMPORT AS TRANSACTION
bread_weekday_evening_tr_df <- read.transactions("data/bread basket weekday evening.csv",
                                                   format = "basket",
                                                   sep = ",")

bread_weekday_evening_tr_df
summary(bread_weekday_evening_tr_df)
itemFrequencyPlot(bread_weekday_evening_tr_df, topN = 10)

rules_weekday_evening <- apriori(bread_weekday_evening_tr_df, parameter = list(supp = 0.01,
                                                                                   conf = 0.8, minlen = 2))
rules_weekday_evening <- sort(rules_weekday_evening, by="confidence", decreasing = TRUE)
rules_weekday_evening
inspect(rules_weekday_evening)

plot(rules_weekday_evening)
plot(rules_weekday_evening,method = "grouped")
plot(rules_weekday_evening,method = "graph")
plot(rules_weekday_evening,method = "paracoord")

# Transaksi Weekday Night

bread_weekday_night_df <- bread_weekday_df %>%
  filter(period_day=="night")

str(bread_weekday_night_df)
head(bread_weekday_night_df)
summary(bread_weekday_night_df)

itemlist_weekday_night <- ddply(bread_weekday_night_df, c("Transaction"),
                                  function(df1)paste(df1$Item, collapse = ","))

itemlist_weekday_night$Transaction <- NULL
colnames(itemlist_weekday_night) <- c("items")

# Write to csv file
write.csv(itemlist_weekday_night, "data/bread basket weekday night.csv", 
          quote = FALSE,
          row.names = TRUE)

# IMPORT AS TRANSACTION
bread_weekday_night_tr_df <- read.transactions("data/bread basket weekday night.csv",
                                                 format = "basket",
                                                 sep = ",")

bread_weekday_night_tr_df
summary(bread_weekday_night_tr_df)
itemFrequencyPlot(bread_weekday_night_tr_df, topN = 7)

rules_weekday_night <- apriori(bread_weekday_night_tr_df, parameter = list(supp = 0.02,
                                                                               conf = 0.8, minlen = 2))
rules_weekday_night <- sort(rules_weekday_night, by="confidence", decreasing = TRUE)
rules_weekday_night
inspect(rules_weekday_night)

plot(rules_weekday_night)
plot(rules_weekday_night,method = "grouped")
plot(rules_weekday_night,method = "graph")
plot(rules_weekday_night,method = "paracoord")

# Transaksi Weekend Morning

bread_weekend_morning_df <- bread_weekend_df %>%
  filter(period_day=="morning")

str(bread_weekend_morning_df)
head(bread_weekend_morning_df)
summary(bread_weekend_morning_df)

itemlist_weekend_morning <- ddply(bread_weekend_morning_df, c("Transaction"),
                                  function(df1)paste(df1$Item, collapse = ","))

itemlist_weekend_morning$Transaction <- NULL
colnames(itemlist_weekend_morning) <- c("items")

# Write to csv file
write.csv(itemlist_weekend_morning, "data/bread basket weekend morning.csv", 
          quote = FALSE,
          row.names = TRUE)

# IMPORT AS TRANSACTION
bread_weekend_morning_tr_df <- read.transactions("data/bread basket weekend morning.csv",
                                                 format = "basket",
                                                 sep = ",")

bread_weekend_morning_tr_df
summary(bread_weekend_morning_tr_df)
itemFrequencyPlot(bread_weekend_morning_tr_df, topN = 10)

rules_weekend_morning <- apriori(bread_weekend_morning_tr_df, parameter = list(supp = 0.005,
                                                                               conf = 0.8, minlen = 2))
rules_weekend_morning <- sort(rules_weekend_morning, by="confidence", decreasing = TRUE)
rules_weekend_morning
inspect(rules_weekend_morning)

plot(rules_weekend_morning)
plot(rules_weekend_morning,method = "grouped")
plot(rules_weekend_morning,method = "graph")
plot(rules_weekend_morning,method = "paracoord")

# Transaksi Weekend Afternoon

bread_weekend_afternoon_df <- bread_weekend_df %>%
  filter(period_day=="afternoon")

str(bread_weekend_afternoon_df)
head(bread_weekend_afternoon_df)
summary(bread_weekend_afternoon_df)

itemlist_weekend_afternoon <- ddply(bread_weekend_afternoon_df, c("Transaction"),
                                  function(df1)paste(df1$Item, collapse = ","))

itemlist_weekend_afternoon$Transaction <- NULL
colnames(itemlist_weekend_afternoon) <- c("items")

# Write to csv file
write.csv(itemlist_weekend_afternoon, "data/bread basket weekend afternoon.csv", 
          quote = FALSE,
          row.names = TRUE)

# IMPORT AS TRANSACTION
bread_weekend_afternoon_tr_df <- read.transactions("data/bread basket weekend afternoon.csv",
                                                 format = "basket",
                                                 sep = ",")

bread_weekend_afternoon_tr_df
summary(bread_weekend_afternoon_tr_df)
itemFrequencyPlot(bread_weekend_afternoon_tr_df, topN = 10)

rules_weekend_afternoon <- apriori(bread_weekend_afternoon_tr_df, parameter = list(supp = 0.004,
                                                                               conf = 0.7, minlen = 2))
rules_weekend_afternoon <- sort(rules_weekend_afternoon, by="confidence", decreasing = TRUE)
rules_weekend_afternoon
inspect(rules_weekend_afternoon)

plot(rules_weekend_afternoon)
plot(rules_weekend_afternoon,method = "grouped")
plot(rules_weekend_afternoon,method = "graph")
plot(rules_weekend_afternoon,method = "paracoord")

# Transaksi Weekend Evening

bread_weekend_evening_df <- bread_weekend_df %>%
  filter(period_day=="evening")

str(bread_weekend_evening_df)
head(bread_weekend_evening_df)
summary(bread_weekend_evening_df)

itemlist_weekend_evening <- ddply(bread_weekend_evening_df, c("Transaction"),
                                  function(df1)paste(df1$Item, collapse = ","))

itemlist_weekend_evening$Transaction <- NULL
colnames(itemlist_weekend_evening) <- c("items")

# Write to csv file
write.csv(itemlist_weekend_evening, "data/bread basket weekend evening.csv", 
          quote = FALSE,
          row.names = TRUE)

# IMPORT AS TRANSACTION
bread_weekend_evening_tr_df <- read.transactions("data/bread basket weekend evening.csv",
                                                 format = "basket",
                                                 sep = ",")

bread_weekend_evening_tr_df
summary(bread_weekend_evening_tr_df)
itemFrequencyPlot(bread_weekend_evening_tr_df, topN = 10)

rules_weekend_evening <- apriori(bread_weekend_evening_tr_df, parameter = list(supp = 0.02,
                                                                               conf = 0.9, minlen = 2))
rules_weekend_evening <- sort(rules_weekend_evening, by="confidence", decreasing = TRUE)
rules_weekend_evening
inspect(rules_weekend_evening)

plot(rules_weekend_evening)
plot(rules_weekend_evening,method = "grouped")
plot(rules_weekend_evening,method = "graph")
plot(rules_weekend_evening,method = "paracoord")

# Transaksi Weekend Night

bread_weekend_night_df <- bread_weekend_df %>%
  filter(period_day=="night")

str(bread_weekend_night_df)
head(bread_weekend_night_df)
summary(bread_weekend_night_df)

itemlist_weekend_night <- ddply(bread_weekend_night_df, c("Transaction"),
                                  function(df1)paste(df1$Item, collapse = ","))

itemlist_weekend_night$Transaction <- NULL
colnames(itemlist_weekend_night) <- c("items")

# Write to csv file
write.csv(itemlist_weekend_night, "data/bread basket weekend night.csv", 
          quote = FALSE,
          row.names = TRUE)

# IMPORT AS TRANSACTION
bread_weekend_night_tr_df <- read.transactions("data/bread basket weekend night.csv",
                                                 format = "basket",
                                                 sep = ",")

bread_weekend_night_tr_df
summary(bread_weekend_night_tr_df)
itemFrequencyPlot(bread_weekend_night_tr_df, topN = 3)

rules_weekend_night <- apriori(bread_weekend_night_tr_df, parameter = list(supp = 0.005,
                                                                               conf = 0.9, minlen = 2))
rules_weekend_night <- sort(rules_weekend_night, by="confidence", decreasing = TRUE)
rules_weekend_night
inspect(rules_weekend_night)

plot(rules_weekend_night)
plot(rules_weekend_night,method = "grouped")
plot(rules_weekend_night,method = "graph")
plot(rules_weekend_night,method = "paracoord")

# Summary weekday weekend
itemFrequencyPlot(bread_weekday_tr_df, topN = 10,)
itemFrequencyPlot(bread_weekend_tr_df, topN = 10)
plot(rules_weekday,method = "paracoord")
plot(rules_weekend,method = "paracoord")

# Summary weekday (morning, afternoon,evening,night)
itemFrequencyPlot(bread_weekday_morning_tr_df, topN = 10)
itemFrequencyPlot(bread_weekday_afternoon_tr_df, topN = 10)
itemFrequencyPlot(bread_weekday_evening_tr_df, topN = 10)
itemFrequencyPlot(bread_weekday_night_tr_df, topN = 7)

plot(rules_weekday_morning,method = "paracoord")
plot(rules_weekday_afternoon,method = "paracoord")
plot(rules_weekday_evening,method = "paracoord")
plot(rules_weekday_night,method = "paracoord")

# Summary weekend (morning,afternoon,evening,night)
itemFrequencyPlot(bread_weekend_morning_tr_df, topN = 10)
itemFrequencyPlot(bread_weekend_afternoon_tr_df, topN = 10)
itemFrequencyPlot(bread_weekend_evening_tr_df, topN = 10)
sitemFrequencyPlot(bread_weekend_night_tr_df, topN = 3)

plot(rules_weekend_morning,method = "paracoord")
plot(rules_weekend_afternoon,method = "paracoord")
plot(rules_weekend_evening,method = "paracoord")
plot(rules_weekend_night,method = "paracoord")
