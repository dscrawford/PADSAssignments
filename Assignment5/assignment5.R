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

# Question 1
stock_returns_monthly <- SPY_30 %>%
    group_by(symbol) %>%
    tq_transmute(
      select = adjusted,
      mutate_fun = periodReturn,
      period = "monthly",
      col_rename = "Ra"
    )

wts = c(1)
portfolio_returns_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = wts, 
               col_rename  = "Ra")

# Strategy type 1
str1_money = rep(0, nrow(portfolio_returns_monthly))
for (i in 1:nrow(portfolio_returns_monthly)) {
  if (i != 1) {
    str1_money[i] = 1000 + 
      str1_money[i - 1] + 
      (str1_money[i - 1] * portfolio_returns_monthly$Ra[i])
  }
}

plot(portfolio_returns_monthly$date, money, type="l")

# Perform moving average rule
MOVING_AVERAGE_DAYS = 20
SPY_sma <- SMA(SPY_30$adjusted, n=MOVING_AVERAGE_DAYS)
sma_previous_day = append(c(-Inf), SPY_sma[1:length(SPY_sma) - 1])
SPY_30$increasing = SPY_sma > sma_previous_day
SPY_30_months = SPY_30[SPY_30$date %in% portfolio_returns_monthly$date, ]


# 
str2_money = rep(0, nrow(portfolio_returns_monthly))
for (i in 1:nrow(portfolio_returns_monthly)) {
  if (i == 1) {
    next
  }
  if(SPY_30_months$increasing[i])
}

# portfolio_growth_monthly <- stock_returns_monthly %>%
#   tq_portfolio(assets_col   = symbol, 
#                returns_col  = Ra, 
#                weights      = wts, 
#                col_rename   = "investment.growth",
#                wealth.index = TRUE) %>%
#   mutate(investment.growth = investment.growth * myMoney)
# 

# portfolio_growth_monthly %>%
#   ggplot(aes(x = date, y = investment.growth)) +
#   geom_line(size = 2, color = palette_light()[[1]]) +
#   labs(title = "Portfolio Growth",
#        subtitle = "100% SPY",
#        x = "Month", y = "Portfolio Value") +
#   geom_smooth(method = "loess") +
#   theme_tq() +
#   scale_color_tq() +
#   scale_y_continuous(labels = scales::dollar)
# 
# portfolio_growth_monthly <- stock_returns_monthly %>%
#   tq_portfolio(assets_col   = symbol, 
#                returns_col  = Ra, 
#                weights      = wts, 
#                col_rename   = "investment.growth",
#                wealth.index = TRUE) %>%
#   mutate(investment.growth = investment.growth * myMoney)
# 
# stock_returns_monthly <-c("SPY") %>% 
#   tq_get(get  = "stock.prices",
#          from = "1991-05-01",
#          to   = "2021-05-01") %>%
#   group_by(symbol) %>%
#   tq_transmute(
#     select = adjusted,
#     mutate_fun = SMA,
#     period = "monthly",
#     col_rename = "Ra"
#   )
# stock_returns_monthly
