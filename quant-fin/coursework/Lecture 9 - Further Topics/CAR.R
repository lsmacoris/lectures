library(tidyverse)
library(tidyquant)
library(tidytext)
library(lubridate)
library(textdata)


# Example 1: Gety Images (merger announcement with ShutterStock) ------------------------------------

# Define merger announcement date
event_date <- as.Date("2025-01-07")

# Define windows
event_window <- -10:10
start_date <- event_date - estimation_window - 30

# Define tickers
stock_ticker <- "GETY"     # VivoPower International
market_ticker <- "^GSPC"   # S&P 500 as market proxy

# Get data from Yahoo Finance
start_date <- event_date - estimation_window - 30
end_date <- event_date + 30

prices <- tq_get(c(stock_ticker, market_ticker), 
                 from = start_date, 
                 to = end_date) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, 
               mutate_fun = periodReturn,
               period = "daily", 
               col_rename = "return") %>%
  ungroup()


# Split stock and market returns
returns_data <- prices %>%
  pivot_wider(names_from = symbol, values_from = return) %>%
  rename(stock_return = !!stock_ticker, market_return = !!market_ticker) %>%
  drop_na()

# Estimation window
estimation_data <- returns_data %>%
  filter(date < event_date & date >= event_date - estimation_window)

# Run market model regression
model <- lm(stock_return ~ market_return, data = estimation_data)
summary(model)


# Event window returns
event_data <- returns_data %>%
  filter(between(date, event_date + min(event_window), event_date + max(event_window))) %>%
  mutate(
    day = as.numeric(date - event_date),
    expected_return = predict(model, newdata = .),
    abnormal_return = stock_return - expected_return
  ) %>%
  mutate(cumulative_abnormal_return = cumsum(abnormal_return))

#Plot
ggplot(event_data, aes(x = day, y = cumulative_abnormal_return)) +
  geom_line(color = "steelblue", size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red",size=3) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks=seq(-10,10,1))+
  labs(
    title = "Cumulative Abnormal Returns Around Merger Announcement",
    x = "Day Relative to Announcement",
    y = "Cumulative Abnormal Return (CAR)"
  ) +
  theme_minimal(base_size = 28)


# Filter ±5 day window
car_test_data <- event_data %>%filter(day >= -5 & day <= 5)

# Perform one-sample t-test: H0 is that the mean abnormal return = 0
car_ttest <- t.test(car_test_data$abnormal_return, mu = 0)

# Display results
cat("±5-day CAR =", round(sum(car_test_data$abnormal_return), 4), "\n")
print(car_ttest)


# Bootstrap CAR under the null
set.seed(123)
B <- 10000
bootstrap_cars <- replicate(B, {
  sample(model$residuals, size = nrow(car_test_data), replace = TRUE) |> sum()
})

# Calculate p-value
observed_car <- sum(car_test_data$abnormal_return)
p_value_boot <- mean(abs(bootstrap_cars) >= abs(observed_car))

cat("Bootstrap p-value =", round(p_value_boot, 4), "\n")


# Example 2: Hypera and EMS merger ------------------------------------

# Define merger announcement date
event_date <- as.Date("2024-10-21")

# Define windows
event_window <- -10:10
start_date <- event_date - estimation_window - 30

# Define tickers
stock_ticker <- "HYPE3.SA"   # Hypera Pharma
market_ticker <- "^BVSP"     # Ibovespa as market prox

# Get data from Yahoo Finance
start_date <- event_date - estimation_window - 30
end_date <- event_date + 30

prices <- tq_get(c(stock_ticker, market_ticker), 
                 from = start_date, 
                 to = end_date) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, 
               mutate_fun = periodReturn,
               period = "daily", 
               col_rename = "return") %>%
  ungroup()


# Split stock and market returns
returns_data <- prices %>%
  pivot_wider(names_from = symbol, values_from = return) %>%
  rename(stock_return = !!stock_ticker, market_return = !!market_ticker) %>%
  drop_na()

# Estimation window
estimation_data <- returns_data %>%
  filter(date < event_date & date >= event_date - estimation_window)

# Run market model regression
model <- lm(stock_return ~ market_return, data = estimation_data)
summary(model)


# Event window returns
event_data <- returns_data %>%
  filter(between(date, event_date + min(event_window), event_date + max(event_window))) %>%
  mutate(
    day = as.numeric(date - event_date),
    expected_return = predict(model, newdata = .),
    abnormal_return = stock_return - expected_return
  ) %>%
  mutate(cumulative_abnormal_return = cumsum(abnormal_return))

#Plot
ggplot(event_data, aes(x = day, y = cumulative_abnormal_return)) +
  geom_line(color = "steelblue", size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red",size=3) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks=seq(-10,10,1))+
  labs(
    title = "Cumulative Abnormal Returns Around Merger Announcement",
    x = "Day Relative to Announcement",
    y = "Cumulative Abnormal Return (CAR)"
  ) +
  theme_minimal(base_size = 28)

# Filter ±5 day window
car_test_data <- event_data %>%filter(day >= -5 & day <= 5)

# Perform one-sample t-test: H0 is that the mean abnormal return = 0
car_ttest <- t.test(car_test_data$abnormal_return, mu = 0)

# Display results
cat("±5-day CAR =", round(sum(car_test_data$abnormal_return), 4), "\n")
print(car_ttest)


# Bootstrap CAR under the null
set.seed(123)
B <- 10000
bootstrap_cars <- replicate(B, {
  sample(model$residuals, size = nrow(car_test_data), replace = TRUE) |> sum()
})

# Calculate p-value
observed_car <- sum(car_test_data$abnormal_return)
p_value_boot <- mean(abs(bootstrap_cars) >= abs(observed_car))

cat("Bootstrap p-value =", round(p_value_boot, 4), "\n")



# Example 2: Sentiment Analysis -------------------------------------------


# Extended set of dates and financial headlines
headlines_df <- read.csv('headlines.csv')

# Load the AFINN lexicon (this can be considered a starting point)
afinn_lexicon <- textdata::lexicon_afinn()

# Tokenize text and match sentiment
sentiment_df <- headlines_df %>%
  unnest_tokens(word, text) %>%
  inner_join(sentiment_words, by = "word") %>%
  count(date, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = positive - negative)


ggplot(sentiment_df, aes(x = as.Date(date), y = sentiment_score, fill = sentiment_score > 0)) +
  geom_col() +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "darkgreen")) +
  scale_x_date(date_breaks = "1 days")+
  geom_vline(xintercept = as.Date("2024-10-21"), linetype = "dashed", color = "black") +
  labs(title = "Financial Sentiment Analysis of HYPE3 Merger Announcement",
       x = "Date",
       y = "Sentiment Score (Positive - Negative)",
       fill = "Sentiment") +
  theme_minimal(base_size = 18)+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90))


