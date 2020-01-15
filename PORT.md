---
title: "**Portfolio Analysis and Optimization**"
author: "Arsh Kumar, Tanmay Pandit, Leandro Calcagnotto (December 9, 2019)"
date: "**Table of Contents**"
output: 
  html_document:
    keep_md: true
    
---

### Objective Statement
We are trying to evaluate whether a portfolio constructed of companies regarded as sector leaders in the S&P 500 and optimized through mean variance weights can outperform the broader market over a long term on risk adjusted returns. Our analysis also includes a regression analysis to test the characteristics of this portfolio.

### Prerequisites


```r
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
```

### Security Selection
We selected companies regarded as leaders in their respective sectors. These companies have sound balance sheets, sustainable business models, strong revenue and cash flow growth, and very high return on equity. 


```r
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
```

We chose JP Morgan (JPM) from Financials sector, Apple Inc (AAPL) from Technology, Amazon (AMZN) from Consumer Discretionary, Disney (DIS) from Communication Services, Procter and Gamble (PG) from Consumer Staple, EOG Resources (EOG) from Energy, United Health (UNH) from Healthcare, and Boeing (BA) from Industrials.


### Security Returns
Let’s see how the stocks of these companies have performed over the entire analysis period which is from 2010 to 2019. 


```r
prices_monthly <- to.monthly(prices, indexAt = "lastof", OHLC = FALSE)
asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))

chart.CumReturns(asset_returns_xts, wealth.index = TRUE, main = "Security Returns", legend.loc = TRUE)
```

![](PORT_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

* Amazon has been the best performer with the return of over 900 percent, followed by United Health and Apple. Since the market bottomed during the financial crisis, Amazon has averaged about 30% or 17% higher than the average market return. Company is well positioned as the market leader in the e-commerce and public cloud, where the secular shifts remain early (US e-commerce represents ~15% of retail sales). Notably, Amazon’s flexibility in pushing first party vs. third-party inventory and its Prime offering both serve as major advantage in its retail business. What’s more, company has also started to show more profit with its high growing AWS and advertising revenue streams. 

* On the flip side, EOG is the worst performing holding with annualized return of about 6%. This was on the back of precipitous fall in crude oil prices from the peak of $115 per barrel in 2014 due to (1) disappointing growth in oil importers, (2) booming US shale production, and (3) shifting OPEC policies. 


### Split-Sample Evaluation
Before we calculate the optimal weights and construct our portfolio, we thought it would be prudent to split our time period into two halves. We call them estimation or in-sample period and evaluation period (out of sample period). We used in-sample period (2010 to 2014) for model selection and calculation of optimized weights and out of sample period (2015-2019) to evaluate the performance of portfolio constructed through these weights. 


```r
returns_estim <- window(asset_returns_xts, start = "2009-12-31", end = "2014-12-31")

returns_eval <- window(asset_returns_xts, start = "2015-01-01", end = "2019-11-30")
```

### Weight Optimization
Using the portfolio.optim function, we calculate the optimal weights for our portfolio. We specify max (20%) and min (5%) weight constraints for the diversification. This algorithm ensures that no other portfolio exists which has similar return but a smaller variance (volatility). 


```r
max_weight <- rep(0.2, ncol(returns_estim))
min_weight<- rep(0.05, ncol(returns_eval))


opt <- portfolio.optim(returns_estim, riskless = FALSE, shorts = FALSE, reslow = min_weight, reshigh = max_weight)

pf_weights <- opt$pw
names(pf_weights) <- colnames(returns_estim)

barplot(sort(pf_weights, decreasing = TRUE), main = "Portfolio Optimum Weights", ylab = "Weights", xlab = "Stocks", hori = FALSE, col = c("lightblue"), cex.axis = 0.8)
```

![](PORT_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

We find out that portfolio allocates largest weights to United Health, Procter and Gamble and Amazon and lowest weights to JP Morgan and Apple.


### Portfolio Performance For Estimation Period
Using these optimal weights, we create our portfolio.


```r
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
```

![](PORT_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

It produced a whopping return of 140% (or 19% annualized) over this period, comprehensively outperforming the benchmark which was up 95% (14% annualized) for the same period. Similarly, Sharpe ratio of our portfolio was also high (1.36) compared to benchmark (0.93), reflecting better risk adjusted returns. 



```r
table_Portfolio_Return_est <- table.AnnualizedReturns(Portfolio_Return_est, digit = 2, Rf = 0.02/12)

table_bench_returns_estim <- table.AnnualizedReturns(bench_returns_estim, digit = 2, Rf = 0.02/12)

cum_Portfolio_Return_est <- Return.cumulative(Portfolio_Return_est)
cum_bench_returns_estim <-  Return.cumulative(bench_returns_estim)

cbind(cum_Portfolio_Return_est, cum_bench_returns_estim)
```

```
##                   portfolio.returns SPY.Adjusted
## Cumulative Return          1.405315    0.9521565
```

```r
cbind(table_Portfolio_Return_est, table_bench_returns_estim)
```

```
##                           portfolio.returns SPY.Adjusted
## Annualized Return                      0.19         0.14
## Annualized Std Dev                     0.12         0.13
## Annualized Sharpe (Rf=2%)              1.37         0.93
```




```r
comp_estim <- cbind(Portfolio_Return_est, bench_returns_estim)
chart.CumReturns(comp_estim, wealth.index = TRUE, main = "Portfolio Vs. Benchmark (Estimation Period)", legend.loc = TRUE)
```

![](PORT_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Before we conclude anything, we also want to evaluate whether we get the same results for our evaluation period (out-sample period).


### Portfolio Performance For Evaluation Period

Using the same weights, we calculate the portfolio performance again for our evaluation period (2015 to 2019). 


```r
Portfolio_Return_eval <- Return.portfolio(returns_eval, weights=pf_weights, rebalance_on = "months")
comp_eval <- cbind(Portfolio_Return_eval, bench_returns_eval)
chart.CumReturns(comp_eval, wealth.index = TRUE, main = "Portfolio Vs. Benchmark (Evaluation Period)", legend.loc = TRUE)
```

![](PORT_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

We find out that portfolio outperforms the benchmark with cumulative return of 122% or more than double the return of benchmark.



```r
cum_Portfolio_Return_eval <- Return.cumulative(Portfolio_Return_eval)
cum_bench_returns_eval <-  Return.cumulative(bench_returns_eval)

cbind(cum_Portfolio_Return_eval, cum_bench_returns_eval)
```

```
##                   portfolio.returns SPY.Adjusted
## Cumulative Return          1.236571    0.6180403
```

```r
table_Portfolio_Return_eval <- table.AnnualizedReturns(Portfolio_Return_eval, digit = 2, Rf = 0.02/12)

table_bench_returns_eval <- table.AnnualizedReturns(bench_returns_eval, digit = 2, Rf = 0.02/12)

cbind(table_Portfolio_Return_eval, table_bench_returns_eval)
```

```
##                           portfolio.returns SPY.Adjusted
## Annualized Return                      0.18         0.10
## Annualized Std Dev                     0.13         0.12
## Annualized Sharpe (Rf=2%)              1.20         0.67
```




```r
highchart(type = "stock") %>% 
  hc_title(text = "Monthly Log Returns - Portfolio Vs. SPX") %>%
  hc_add_series(Portfolio_Return_eval, 
                name = "Portfolio") %>%
  hc_add_series(bench_returns_eval, 
                name = "S&P 500") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = TRUE) %>% 
  hc_scrollbar(enabled = TRUE)
```

<!--html_preserve--><div id="htmlwidget-1b649d30580d3c561a1b" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-1b649d30580d3c561a1b">{"x":{"hc_opts":{"title":{"text":"Monthly Log Returns - Portfolio Vs. SPX"},"yAxis":{"title":{"text":null}},"credits":{"enabled":false},"exporting":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0},"treemap":{"layoutAlgorithm":"squarified"}},"series":[{"data":[[1422662400000,0.0195809117927055],[1425081600000,0.0577222633423413],[1427760000000,-0.0021843551672005],[1430352000000,0.0115605352070012],[1433030400000,0.00737993111948376],[1435622400000,0.0043756340216059],[1438300800000,0.0229744561474774],[1440979200000,-0.0665866318654134],[1443571200000,-0.00689778753012971],[1446249600000,0.0994220996292881],[1448841600000,-0.00740999898658345],[1451520000000,-0.0106776501923826],[1454198400000,-0.0593062719249814],[1456704000000,-0.0201002523433539],[1459382400000,0.0682157340857668],[1461974400000,0.0359191482877408],[1464652800000,0.0119815982042921],[1467244800000,0.0188393970991656],[1469923200000,0.0229368075730678],[1472601600000,0.00439320881638294],[1475193600000,0.0386095808548461],[1477872000000,-0.00708351519623607],[1480464000000,0.0412308525076359],[1483142400000,0.0210603031585506],[1485820800000,0.0412608745498892],[1488240000000,0.0348089262160711],[1490918400000,0.00627547924146721],[1493510400000,0.0166616825862698],[1496188800000,0.00748717997014881],[1498780800000,0.0127186822507921],[1501459200000,0.0574868205163557],[1504137600000,-0.00611926868694401],[1506729600000,0.00955231247310584],[1509408000000,0.0395493398478446],[1512000000000,0.0568884640348384],[1514678400000,0.0136968282424255],[1517356800000,0.0698740743764568],[1519776000000,-0.0337553115933602],[1522454400000,-0.0322225069169213],[1525046400000,0.0289675826288338],[1527724800000,0.0260951819143029],[1530316800000,0.0264036416413587],[1532995200000,0.0492818642494126],[1535673600000,0.0316238272663329],[1538265600000,0.0204898025006923],[1540944000000,-0.0547783107488092],[1543536000000,0.0203320407203036],[1546214400000,-0.0944317126750869],[1548892800000,0.0928643772039597],[1551312000000,-0.00907273458695634],[1553990400000,0.0144794574205891],[1556582400000,0.0376879892683988],[1559260800000,-0.0581706680758718],[1561852800000,0.0636636949785394],[1564531200000,0.0111633976431795],[1567209600000,-0.0315282059930162],[1569801600000,-0.00205764296361055],[1572480000000,0.0238020215669543],[1575072000000,0.0539889908885784]],"name":"Portfolio"},{"data":[[1422662400000,-0.0300771002363707],[1425081600000,0.0546819695103382],[1427760000000,-0.0158303486067162],[1430352000000,0.00978593409837547],[1433030400000,0.0127742348133468],[1435622400000,-0.0205214403294685],[1438300800000,0.0223380212579993],[1440979200000,-0.0628867676287701],[1443571200000,-0.0258472501355467],[1446249600000,0.0816352418626458],[1448841600000,0.00364817645351323],[1451520000000,-0.0174333003559246],[1454198400000,-0.0510687861544623],[1456704000000,-0.000826343024729503],[1459382400000,0.0651003866970727],[1461974400000,0.00393336339638939],[1464652800000,0.0168685209812569],[1467244800000,0.00346966511644808],[1469923200000,0.0358221084426242],[1472601600000,0.00119661295264706],[1475193600000,5.80613788185858e-005],[1477872000000,-0.0174888989156914],[1480464000000,0.036176059654653],[1483142400000,0.0200691090231375],[1485820800000,0.0177364809877929],[1488240000000,0.0385391114640097],[1490918400000,0.00124926132874581],[1493510400000,0.00987722054136153],[1496188800000,0.0140142464461679],[1498780800000,0.00635472864185171],[1501459200000,0.020345816214431],[1504137600000,0.00291342207906808],[1506729600000,0.0199490340282722],[1509408000000,0.0232906541906202],[1512000000000,0.0301082001351221],[1514678400000,0.0120551104079301],[1517356800000,0.0548281032302675],[1519776000000,-0.0370378479905229],[1522454400000,-0.0277931678114145],[1525046400000,0.00515485470002996],[1527724800000,0.0240181815443901],[1530316800000,0.00573448982628566],[1532995200000,0.0363766377590844],[1535673600000,0.0314211055658937],[1538265600000,0.00592802789627456],[1540944000000,-0.0716081177491521],[1543536000000,0.0183794729320157],[1546214400000,-0.0921684904554025],[1548892800000,0.07702163238541],[1551312000000,0.031901549793945],[1553990400000,0.0179387296971774],[1556582400000,0.0400400390331859],[1559260800000,-0.0658954677181898],[1561852800000,0.0672721072007798],[1564531200000,0.0150062905435249],[1567209600000,-0.0168851039282796],[1569801600000,0.0192710261222393],[1572480000000,0.0218638532463737],[1575072000000,0.0355585023570093]],"name":"S&P 500"}],"navigator":{"enabled":true},"scrollbar":{"enabled":true}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"stock","fonts":[],"debug":false},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

In contrast to the estimation period, we observed that our portfolio experiences high relative volatility (also evident from the standard deviation in the table above). However, returns generated by our portfolio overcompensates for the additional risk, resulting in a higher sharpe ratio. 



```r
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
```

![](PORT_files/figure-html/unnamed-chunk-11-1.png)<!-- -->




### Regression Analysis

In order to test the characteristics of our portfolio, we performed a regression analysis using Fama French 3 factor model (data obtained from Tuck Business School website). Three factor are (1) size of firms, (2) book-to-market values, and (3) excess return on the market. In other words, the three factors used are SMB (small minus big), HML (high minus low) and the portfolio's return less the risk free rate of return. SMB accounts for publicly traded companies with small market caps that generate higher returns, while HML accounts for value stocks with high book-to-market ratios.


```r
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
```

```
## # A tibble: 4 x 6
##   term        estimate std.error p.value conf.low conf.high
##   <chr>          <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
## 1 (Intercept)    0.006     0.002   0.009    0.002     0.011
## 2 MKT_RF         1.02      0.061   0        0.894     1.14 
## 3 SMB           -0.32      0.178   0.074   -0.672     0.032
## 4 HML           -0.251     0.139   0.075   -0.527     0.026
```

```r
ff_dplyr <- lm(R_excess ~ MKT_RF + SMB + HML, 
               ff_portfolio_returns)

summary(ff_dplyr, digits = 2)
```

```
## 
## Call:
## lm(formula = R_excess ~ MKT_RF + SMB + HML, data = ff_portfolio_returns)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.04866 -0.01673 -0.00146  0.01637  0.07626 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.006483   0.002441   2.656   0.0091 ** 
## MKT_RF       1.015900   0.061462  16.529   <2e-16 ***
## SMB         -0.320192   0.177723  -1.802   0.0744 .  
## HML         -0.250673   0.139462  -1.797   0.0751 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02499 on 108 degrees of freedom
## Multiple R-squared:  0.7187,	Adjusted R-squared:  0.7109 
## F-statistic: 91.98 on 3 and 108 DF,  p-value: < 2.2e-16
```

* The coefficients, p values and confidence band of regression model suggests that only market factor is significant. 

* Negative coefficients for SMB and HML suggest that our portfolio is consisted of large cap growth companies. 

* R squared is 72%, suggesting that 72% of the variation in the excess returns is explained by this model. 


```r
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
```

![](PORT_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


### Conclusion
Our analysis suggests that portfolio consisted of companies regarded as sector leaders is in essence a large cap growth portfolio and if optimized through mean variance model and with monthly rebalancing, can outperform the benchmark over the long term on risk adjusted returns.


### Disclaimer
*We hereby certify that the views expressed in research report accurately reflects our personal views about the subject securities. This information is for educational purposes and is not a investment recommendation nor to be representative of professional expertise. All examples and analysis are intended for these purposes and should not be considered as specific investment advice.*
