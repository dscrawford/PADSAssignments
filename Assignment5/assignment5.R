library(quantmod)
library(tidyquant)
library(dplyr)
library(ggplot2)

getSymbols("SPY")

chartSeries(SPY)

SPY_30 <- c("SPY") %>% 
  tq_get(get  = "stock.prices",
         from = "1991-05-01",
         to   = "2021-05-01")

stock_returns_monthly <- SPY_30 %>%
    group_by(symbol) %>%
    tq_transmute(
      select = adjusted,
      mutate_fun = periodReturn,
      period = "monthly",
      col_rename = "Ra"
    )

stock_returns_daily <- SPY_30 %>%
  group_by(symbol) %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "daily",
    col_rename = "Ra"
  )

# Strategy 1: Buy and Hold
str1_money = rep(0, nrow(stock_returns_monthly))
for (i in 1:nrow(stock_returns_monthly)) {
  if (i != 1) {
    str1_money[i] = 1000 + 
      str1_money[i - 1] + 
      (str1_money[i - 1] * stock_returns_monthly$Ra[i])
  }
}

plot(stock_returns_monthly$date, str1_money, type="l")

# Strategy 2: 100 day SMA strategy

# Perform moving average rule
MOVING_AVERAGE_DAYS = 100
SPY_sma <- SMA(SPY_30$adjusted, n=MOVING_AVERAGE_DAYS)
SPY_30$above_sma = SPY_30$adjusted >= SPY_sma
SPY_30$above_sma[1:MOVING_AVERAGE_DAYS] = FALSE
month_intervals = stock_returns_monthly$date

stock_returns_monthly
plot(SPY_30$adjusted, type="l")
lines(SPY_sma, type="l", col="red")
# Strategy type 2
str2_money = rep(0, nrow(SPY_30))
str2_curr_money = 0
holding = TRUE
for (i in 1:nrow(SPY_30)) {
  # Need previous data to make decisions.
  if (i == 1) {
    next
  }
  if (holding) {
  str2_curr_money = str2_curr_money + 
    (str2_curr_money * stock_returns_daily$Ra[i])
  }
  
  # If the stock line goes above the current SMA line -> Buy.
  if (SPY_30$above_sma[i] && !holding) {
    holding = TRUE
  }
  else if (!SPY_30$above_sma[i] && holding){
    holding = FALSE
  }
  
  # If it has been another month, add 1000 dollars.
  if (SPY_30$date[i] %in% month_intervals) {
    str2_curr_money = str2_curr_money + 1000
  }
  
  str2_money[i] = str2_curr_money
}

# Table indicating how much money you would have each year using strat 1 and 2
str1 = data.frame(date = portfolio_returns_monthly$date, str1_money = str1_money)
str2 = data.frame(date = portfolio_returns_daily$date, str2_money = str2_money)
View(inner_join(str1,str2))

# A plot indicating the returns of both strategies over the 30 year period.
plot(SPY_30$date, 
     str2_money, 
     type="l", 
     ylab="Money (USD)", 
     xlab="Date",
     main="Buy and Hold vs. 100 Day SMA Strategy",
     xlim=c(min(stock_returns_monthly$date), max(stock_returns_monthly$date)),
     ylim=c(0, max(str1_money, str2_money)),
     col="blue"
     )
lines(stock_returns_monthly$date, 
      str1_money, 
      type="l", 
      col="red",
      xlim=c(min(stock_returns_monthly$date), max(min(stock_returns_monthly$date))),
      ylim=c(0, max(str1_money, str2_money)))
legend(
  x="topleft",
  legend = c("Buy and Hold (Strategy 1)", "100 Day SMA (Strategy 2)"),
  lty = c(1,1),
  col = c("red","blue")
)

# The total money accumulated at the end of 30 years using the 
# different strategies.
cat(paste("Buy and Hold Strategy: ", format(str1_money[length(str1_money)],
                                            big.mark = ",",
                                            scientific = FALSE),
          "\n",
          "100 SMA Strategy     : ", format(str2_money[length(str2_money)],
                                            big.mark = ",",
                                            scientific = FALSE),
          sep=""))

