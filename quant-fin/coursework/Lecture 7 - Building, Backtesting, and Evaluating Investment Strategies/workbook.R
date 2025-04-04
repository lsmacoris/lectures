library(tidyverse)
library(tidyquant)
library(PortfolioAnalytics)


start_date <- "2018-01-01"
end_date <- "2024-12-31"

tickers=c("PETR3.SA", "ITUB3.SA", "IRBR3.SA", "BBDC4.SA", "ABEV3.SA", 
          "YDUQ3.SA", "BBAS3.SA", "B3SA3.SA", "WEGE3.SA", "RADL3.SA")


# Step 1: Getting the Data ------------------------------------------------

tbl_prices <- tidyquant::tq_get(
  x = tickers,
  get = "stock.prices",
  from = start_date,
  to = end_date
) 

# Step 2: Returns ------------------------------------------------

tbl_returns <- tbl_prices %>%
  group_by(symbol) %>%
  tq_transmute(
    select = adjusted,
    col_rename = "return",
    mutate_fun = monthlyReturn)%>% 
  na.omit()


# Step 3 ------------------------------------------------------------------

tbl_returns %>% 
  group_by(symbol) %>% 
  mutate(cum_return = cumprod(1+return)-1) %>% 
  ggplot(aes(x = date, y = cum_return, fill = symbol)) +
  geom_col() + 
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = 2, color = "red") +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Cumulative Returns",
       fill = 'Asset',
       title = "Cumulative Returns throughout the period",
       subtitle = "Pre/Post COVID-19") +
  facet_grid(symbol~.) +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=45, size = 8))

# Step 4: Returns in Wide Format ------------------------------------------

tbl_returns_wide <- tbl_returns %>% 
  pivot_wider(names_from = symbol, 
              values_from = return)%>% 
  column_to_rownames("date")%>% 
  as.xts()


train_set <- tbl_returns_wide['2015/2021']

# First Portfolio, risk minimization ---------------------------------------------------------

#Type 1
min_var_portfolio <- portfolio.spec(assets = tickers)%>% 
  add.objective(type = "risk", name = "var")%>%
  add.constraint(type = "full_investment")%>%
  add.constraint(type = "box", min = 0, max = 1)%>%

min_var_opt <- train_set %>% 
  optimize.portfolio(
    portfolio = min_var_portfolio,
    trace = TRUE
  )

min_var_opt

#Type 2: adding box constraints

min_var_portfolio <- portfolio.spec(assets = tickers)%>%
  add.objective(type = "risk", name = "var")%>%
  add.constraint(type = "full_investment")%>%
  add.constraint(type = "box", min = 0.05, max = 0.25) 

min_var_opt <- train_set %>% 
  optimize.portfolio(
    portfolio = min_var_portfolio,
    trace = TRUE
  )

min_var_opt

#Type 3: adding diversification

min_var_portfolio <- portfolio.spec(assets = tickers)%>%
  add.objective(type = "risk", name = "var")%>%
  add.constraint(type = "full_investment")%>%
  add.constraint(type = "box", min = 0.05, max = 0.25)%>%
  add.constraint(type="diversification", div_target=0.5)

# Second Portfolio, return maximization -----------------------------------


max_return_portfolio <- portfolio.spec(assets = tickers) %>% 
  add.constraint(type = "full_investment")%>% 
  add.constraint(type = "box", min = 0.05, max = 0.6) %>% 
  add.objective(type = "return", name = "mean")

max_return_opt <- train_set %>% 
  optimize.portfolio(
    portfolio = max_return_portfolio,
    trace = TRUE
  )

max_return_opt

#  Third Portfolio, Max Sharpe --------------------------------------------

max_sharpe_portfolio <- portfolio.spec(assets = tickers) %>% 
  add.constraint(type = "full_investment")%>% 
  add.constraint(type = "box", min = 0.05, max = 0.6) %>% 
  add.objective(type = "return", name = "mean")%>% 
  add.objective(type = "risk", name = "StdDev")

max_sharpe_opt <- train_set%>% 
  optimize.portfolio(
    portfolio=max_sharpe_portfolio,
    maxSR=TRUE, 
    trace=TRUE)

max_sharpe_opt



# Plotting ----------------------------------------------------------------

tibble(assets = tickers, 
       w_sharpe = extractWeights(max_sharpe_opt),
       w_return = extractWeights(max_return_opt),
       w_risk = extractWeights(min_var_opt)) %>% 
  pivot_longer(cols = "w_sharpe":"w_risk")%>% 
  ggplot(aes(x = name, y = value, fill = assets)) +
  geom_col() + 
  geom_text(aes(label = assets),
            position = position_stack(vjust = .6),
            size = 3) + 
  scale_x_discrete(labels = c("Maximize Return", "Minimize Risk", "Max Sharpe Ratio")) +
  scale_y_continuous(labels=scales::percent)+
  labs(x = "", y = "Allocation (%)",
       title = "Asset Allocation by Optimization Criteria")+
  theme_minimal()+
  theme(legend.position = "none")


min_var_performance <- tbl_returns %>% 
  filter(date > as.Date("2021-01-01")) %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = return,
    weights = extractWeights(min_var_opt),
    col_rename = "returns_min_var",
    rebalance_on = "months"
  )

max_return_performance <- tbl_returns %>% 
  filter(date > as.Date("2021-01-01")) %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = return,
    weights = extractWeights(max_return_opt),
    col_rename = "returns_max_return",
    rebalance_on = "months"
  )

max_sharpe_performance <- tbl_returns %>% 
  filter(date > as.Date("2021-01-01")) %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = return,
    weights = extractWeights(max_sharpe_opt),
    col_rename = "returns_max_sharpe",
    rebalance_on = "months"
  )

naive_performance <- tbl_returns %>% 
  filter(date > as.Date("2021-01-01")) %>% 
  tq_portfolio(
    assets_col = symbol,
    returns_col = return,
    weights = 1/length(tickers),
    col_rename = "returns_naive",
    rebalance_on = "months"
  )

naive_performance%>%
  left_join(min_var_performance)%>%
  left_join(max_return_performance)%>%
  left_join(max_sharpe_performance)%>%
  pivot_longer(cols = "returns_naive":"returns_max_sharpe",
               names_to = "strategy",
               values_to = "return") %>% 
  mutate(strategy = case_when(strategy == "returns_max_return" ~ "Maximize Avg. Return",
                                strategy == "returns_min_var" ~ "Minimize risk",
                                strategy == "returns_naive" ~ "Equal Allocation",
                                strategy == "returns_max_sharpe" ~ "Maximize Sharpe Ratio")) %>% 
  group_by(strategy) %>% 
  mutate(cum_return = cumprod(1+return)-1) %>% 
  ggplot(aes(x = date, y = cum_return, color = strategy)) + 
  geom_line(size = 1.2)+ 
  scale_y_continuous(label = scales::percent) +
  labs(title = "Historical Performance over test sample", y = "Retorno acumulado do portfólio",
       x = "",
       color='') + 
  theme_minimal()+
  theme(legend.position = "bottom")


# Rolling Optimization ----------------------------------------------------

rolling_optimization <- tbl_returns_wide %>% 
  optimize.portfolio.rebalancing(
    portfolio = max_return_portfolio,
    rebalance_on = "quarters",
    training_period = 12,
    rolling_window = 12
  )

extractWeights(rolling_optimization)%>%as.data.frame()%>%
  rownames_to_column("date")%>% 
  mutate(date = as.Date(date)) %>% 
  pivot_longer(names_to = 'symbol',values_to = 'weights',cols=where(is.numeric)) %>% 
  ggplot(aes(x = date, y = weights, fill = symbol)) +
  labs(fill = "", x = "", y = "Asset Weight",
       title = "Weights under rolling optimization - Maximize Return") +
  geom_col() +
  scale_x_date(date_breaks = "6 months") +
  scale_y_continuous(labels=scales::percent)+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90))




