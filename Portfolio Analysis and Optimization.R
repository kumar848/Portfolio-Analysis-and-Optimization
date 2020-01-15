library(tidyquant)
library(tidyverse)
library(timetk)
library(broom)
library(glue)
library(PerformanceAnalytics)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(highcharter)
library(dygraphs)
library(quantmod)
library(PortfolioAnalytics)
library(tseries)
library(evaluate)
library(rmarkdown)


### Security Selection

symbols <- c("JPM", "AAPL", "AMZN", "DIS", "PG", "EOG", "UNH", "BA")

prices <- 
  getSymbols(symbols, 
             src = 'yahoo', 
             from = "2009-12-31",
             to = "2019-11-30",
             auto.assign = TRUE, 
             warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>%
  `colnames<-`(symbols)



### Security Returns

prices_monthly <- to.monthly(prices, indexAt = "lastof", OHLC = FALSE)
asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))

chart.CumReturns(asset_returns_xts, wealth.index = TRUE, main = "Security Returns", legend.loc = TRUE)


### Split-Sample Evaluation

returns_estim <- window(asset_returns_xts, start = "2009-12-31", end = "2014-12-31")

returns_eval <- window(asset_returns_xts, start = "2015-01-01", end = "2019-11-30")


### Weight Optimization

max_weight <- rep(0.2, ncol(returns_estim))
min_weight<- rep(0.05, ncol(returns_eval))


opt <- portfolio.optim(returns_estim, riskless = FALSE, shorts = FALSE, reslow = min_weight, reshigh = max_weight)

pf_weights <- opt$pw
names(pf_weights) <- colnames(returns_estim)

barplot(sort(pf_weights, decreasing = TRUE), main = "Portfolio Optimum Weights", ylab = "Weights", xlab = "Stocks", hori = FALSE, col = c("lightblue"), cex.axis = 0.8)



### Portfolio Performance For Estimation Period

bench <- "SPY"

bench_price <- 
  getSymbols(bench, 
             src = 'yahoo', 
             from = "2009-12-31",
             to = "2019-11-30",
             auto.assign = TRUE, 
             warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge)

bench_prices_monthly <- to.monthly(bench_price, indexAt = "lastof", OHLC = FALSE)

bench_returns_xts <- na.omit(Return.calculate(bench_prices_monthly, method = "log"))

bench_returns_estim <- window(bench_returns_xts, start = "2009-12-31", end = "2014-12-31")

bench_returns_eval <- window(bench_returns_xts, start = "2015-01-01", end = "2019-11-30")


Portfolio_Return_est <- Return.portfolio(returns_estim, weights=pf_weights, rebalance_on = "months")
chart.CumReturns(Portfolio_Return_est, wealth.index = TRUE, main = "Growth of $1 Invested in Portfolio", col = c("red"))


table_Portfolio_Return_est <- table.AnnualizedReturns(Portfolio_Return_est, digit = 2, Rf = 0.02/12)

table_bench_returns_estim <- table.AnnualizedReturns(bench_returns_estim, digit = 2, Rf = 0.02/12)

cum_Portfolio_Return_est <- Return.cumulative(Portfolio_Return_est)
cum_bench_returns_estim <-  Return.cumulative(bench_returns_estim)

cbind(cum_Portfolio_Return_est, cum_bench_returns_estim)


cbind(table_Portfolio_Return_est, table_bench_returns_estim)

comp_estim <- cbind(Portfolio_Return_est, bench_returns_estim)
chart.CumReturns(comp_estim, wealth.index = TRUE, main = "Portfolio Vs. Benchmark (Estimation Period)", legend.loc = TRUE)


### Portfolio Performance For Evaluation Period


Portfolio_Return_eval <- Return.portfolio(returns_eval, weights=pf_weights, rebalance_on = "months")
comp_eval <- cbind(Portfolio_Return_eval, bench_returns_eval)
chart.CumReturns(comp_eval, wealth.index = TRUE, main = "Portfolio Vs. Benchmark (Evaluation Period)", legend.loc = TRUE)


cum_Portfolio_Return_eval <- Return.cumulative(Portfolio_Return_eval)
cum_bench_returns_eval <-  Return.cumulative(bench_returns_eval)

cbind(cum_Portfolio_Return_eval, cum_bench_returns_eval)

table_Portfolio_Return_eval <- table.AnnualizedReturns(Portfolio_Return_eval, digit = 2, Rf = 0.02/12)

table_bench_returns_eval <- table.AnnualizedReturns(bench_returns_eval, digit = 2, Rf = 0.02/12)

cbind(table_Portfolio_Return_eval, table_bench_returns_eval)



highchart(type = "stock") %>% 
  hc_title(text = "Monthly Log Returns - Portfolio Vs. SPX") %>%
  hc_add_series(Portfolio_Return_eval, 
                name = "Portfolio") %>%
  hc_add_series(bench_returns_eval, 
                name = "S&P 500") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = TRUE) %>% 
  hc_scrollbar(enabled = TRUE)

prices3 <- 
  getSymbols(symbols, 
             src = 'yahoo', 
             from = "2015-01-01",
             to = "2019-11-30",
             auto.assign = TRUE, 
             warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>%
  `colnames<-`(symbols)

asset_returns_long3 <-  
  prices3 %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

asset_returns_long3 %>% 
  ggplot(aes(x = returns, colour = asset, fill = asset)) +
  stat_density(geom = "line", alpha = 1) +
  geom_histogram(alpha = 0.25, binwidth = .01) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Distribution") +
  xlab("monthly returns") +
  ylab("distribution") +
  
  theme(plot.title = element_text(colour = "cornflowerblue"),  
        strip.text.x = element_text(size = 8, colour = "white"), 
        strip.background = element_rect(colour = "white", fill = "cornflowerblue"), 
        axis.text.x = element_text(colour = "cornflowerblue"), 
        axis.text = element_text(colour = "cornflowerblue"), 
        axis.ticks.x = element_line(colour = "cornflowerblue"), 
        axis.text.y = element_text(colour = "cornflowerblue"), 
        axis.ticks.y = element_line(colour = "cornflowerblue"),
        axis.title = element_text(colour = "cornflowerblue"),
        legend.title = element_text(colour = "cornflowerblue"),
        legend.text = element_text(colour = "cornflowerblue")
  )




### Regression Analysis


symbols2 <- c("JPM", "AAPL", "AMZN", "DIS", "PG")

prices2 <- 
  getSymbols(symbols2, 
             src = 'yahoo', 
             from = "2010-1-31",
             to = "2019-06-30",
             auto.assign = TRUE, 
             warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>%
  `colnames<-`(symbols2)

asset_returns_long <-  
  prices2 %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

w <- c(0.25, 0.25, 0.20, 0.20, 0.10)


portfolio_returns_tq_rebalanced_monthly <- 
  asset_returns_long %>%
  tq_portfolio(assets_col  = asset, 
               returns_col = returns,
               weights     = w,
               col_rename  = "returns",
               rebalance_on = "months")

temp <- tempfile()

base <- 
  "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"

factor <- 
  "Global_3_Factors"

format<-
  "_CSV.zip"

full_url <-
  glue(base,
       factor,
       format,
       sep ="")

download.file(
  full_url,
  temp,
  quiet = TRUE)


Global_3_Factors <- 
  read_csv(unz(temp, "Global_3_Factors.csv"), 
           skip = 6) %>% 
  rename(date = X1) %>% 
  mutate_at(vars(-date), as.numeric) %>% 
  mutate(date = 
           rollback(ymd(parse_date_time(date, "%Y%m") + months(1)))) %>% 
  filter(date >= 
           first(portfolio_returns_tq_rebalanced_monthly$date) & date <= 
           last(portfolio_returns_tq_rebalanced_monthly$date))

ff_portfolio_returns <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  left_join(Global_3_Factors, by = "date") %>% 
  mutate(MKT_RF = Global_3_Factors$`Mkt-RF`/100,
         SMB = Global_3_Factors$SMB/100,
         HML = Global_3_Factors$HML/100,
         RF = Global_3_Factors$RF/100,
         R_excess = round(returns - RF, 4))


ff_dplyr_byhand <-
  ff_portfolio_returns %>% 
  do(model = 
       lm(R_excess ~ MKT_RF + SMB + HML, 
          data = .)) %>% 
  tidy(model, conf.int = T, conf.level = .95)



ff_dplyr_byhand %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  select(-statistic)


ff_dplyr <- lm(R_excess ~ MKT_RF + SMB + HML, 
               ff_portfolio_returns)

summary(ff_dplyr, digits = 2)

ff_dplyr_byhand %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>%
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate, shape = term, color = term)) + 
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  labs(title = "FF 3-Factor Coefficients for Our Portfolio",
       subtitle = "Nothing in this post is investment advice",
       x = "",
       y = "coefficient",
       caption = "data source: Fama French website and yahoo! Finance") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption  = element_text(hjust = 0))
