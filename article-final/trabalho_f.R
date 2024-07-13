#### Functions ----
irf_plot <- function(df, x, y, ymin, ymax, title, ylab, ylim_values) {
  plot <- ggplot(df, aes_string(x=x, y=y, ymin=ymin, ymax=ymax)) +
    geom_hline(yintercept = 0, color="black") +
    geom_line(color = "blue") +
    geom_line(aes_string(y=ymin), linetype="dotted", color="red", linewidth = 1) +
    geom_line(aes_string(y=ymax), linetype="dotted", color="red", linewidth = 1) +
    ylim(ylim_values) +
    theme_light() +
    ggtitle(title) +
    ylab(ylab) +
    xlab("") +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11),
          panel.grid = element_blank())
  return(plot)
}

create_dummy_variable <- function(time_series, break_dates) {
  dummy_variable <- rep(0, length(time_series))
  for (i in 1:length(break_dates)) {
    dummy_variable[time(time_series) > break_dates[i]] <- i
  }
  return(dummy_variable)
}

tree_plot <- function(ts1, ts2, ts3,
                      line1, line2, line3) {
  
  par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))  
  
  plot(ts1, main = "Live Cattle Time Series")
  lines(line1)
  
  plot(ts2, main = "Feeder Cattle Time Series")
  lines(line2)
  
  plot(ts3, main = "Corn Time Series")
  lines(line3)
}

structure_breaks <- function(ts) {
  if (!is.ts(ts)) {
    stop("Tem que ser TS!")
  }
  
  ts.fstats <- strucchange::Fstats(ts ~ 1)
  
  ts.breaks <- strucchange::breakpoints(ts ~ 1)
  
  ts.dates <- strucchange::breakdates(ts.breaks)
  
  ts.sctest <- confint(ts.breaks)
  
  return(list(fstats = ts.fstats, breaks = ts.breaks, dates = ts.dates, sctest = ts.sctest))
}


unit_root_tests <- function(df) {
  add_significance_adf <- function(pvalue) {
    if (pvalue < 0.05) {
      return("*")
    } else {
      return("")
    }
  }
  
  add_significance_kpss <- function(pvalue) {
    if (pvalue > 0.05) {
      return("*")
    } else {
      return("")
    }
  }
  
  df_multi <- data.frame(
    Variable = names(df),
    adf.statistic = sapply(df, function(v) adf.test(ts(v), alternative = "stationary")$statistic),
    adf.pvalue = sapply(df, function(v) adf.test(ts(v), alternative = "stationary")$p.value),
    kpss.statistic = sapply(df, function(v) kpss.test(ts(v))$statistic),
    kpss.pvalue = sapply(df, function(v) kpss.test(ts(v))$p.value)
  )
  
  df_multi$adf.significance <- sapply(df_multi$adf.pvalue, add_significance_adf)
  df_multi$kpss.significance <- sapply(df_multi$kpss.pvalue, add_significance_kpss)
  
  row.names(df_multi) <- NULL
  
  df_multi[, c('Variable', 'adf.statistic', 'adf.pvalue', 'adf.significance', 'kpss.statistic', 'kpss.pvalue', 'kpss.significance')]
}

# p-values
cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}

#### Packages ----
pacman::p_load(dplyr, devtools, stringr, strucchange, urca, gridExtra, sandwich, utils, forecast, stats, tseries, dynlm, tidyr, lmtest, openxlsx, lubridate, sjPlot, plotly, htmlwidgets, ggcorplot, rstatix, heatmaply, install = TRUE)

# Data Preco Fechamento 10 Anos Soybean, Soymeal, Soyoil, Corn, Live Cattle, Feeder Cattle
futures <- read.csv2("https://github.com/PedroTL/unicamp-time-series/blob/main/article-final/futures.csv", check.names = FALSE) 

# Dates
futures$Date <- lubridate::dmy(futures$Date)

# Spreads Soybean and Livestock
futures <- futures |>
  dplyr::mutate('Soybean Crush' = ((`Soybean Meal` * 0.022) + ((`Soybean Oil`/100) * 11) - (`Soybean`/100)),
                'Cattle Crush' = ((6 * 400 * `Live Cattle`) - (3 * 500 * `Feeder Cattle`) - (2 * 5000 * `Corn`)) / 1000)|>
  dplyr::filter(Date >= '2014-01-01')


#### Retornos em Log (Para Estacionariedade) ----
futures_log <- futures %>%
  dplyr::mutate_at(2:7, ~ c(NA, diff(log(.)))) %>% 
  dplyr::mutate(across(-1, ~ ifelse(is.finite(.), ifelse(is.na(.), 0, .), 0)))

#### Correlation Matrix ----
cor.coef <- cor(futures_log[,5:7])
#### Live Cattle Graficos ----
# Plot Live
plot_live_cattle <- futures %>%
  plot_ly(x = ~Date, y = ~`Live Cattle`) %>%
  add_lines(name = "Live Cattle", type = "scatter", mode = "lines", line = list(color = "blue"))

# Plot Feeder
plot_feeder_cattle <- futures %>%
  plot_ly(x = ~Date, y = ~`Feeder Cattle`) %>%
  add_lines(name = "Feeder Cattle", type = "scatter", mode = "lines", line = list(color = "green"))

# Plot Corn
plot_corn <- futures %>%
  plot_ly(x = ~Date, y = ~`Corn`) %>%
  add_lines(name = "Corn", type = "scatter", mode = "lines", line = list(color = "yellow"))

# Subplot
subplot_fig_cattle <- subplot(plot_feeder_cattle, plot_live_cattle, plot_corn, nrows = 3, shareX = TRUE) %>%
  layout(
    title = "",
    showlegend = TRUE,
    height = 900,  
    margin = list(t = 100),
    yaxis = list(
      title = "Feeder Cattle (US$)",  
      domain = c(0.75, 1)),
    yaxis2 = list(
      title = "Live Cattle (US$)",  
      domain = c(0.5, 0.75)),
    yaxis3 = list(
      title = "Corn Price (US$)",  
      domain = c(0.25, 0.5)),
    yaxis4 = list(
      title =  "Crush (US$)",  
      domain = c(0, 0.25)))
#### Raiz Unitaria Nivel e Log ----
future_unit_root <- unit_root_tests(futures[, 5:7])
future_unit_root
summary(ur.df(orcam[, 2], type = "none", selectlags = "AIC"))

future_log_unit_root <- unit_root_tests(futures_log[, 5:7])
future_log_unit_root
#### TS Livestock (Daily) ----
futures.ts <- futures |>
  arrange(Date)

live_cattle.ts <- ts(futures.ts$`Live Cattle`, start = c(2014, 1), end = c(2024, 6), frequency = 252)
feeder_cattle.ts <- ts(futures.ts$`Feeder Cattle`, start = c(2014, 1), end = c(2024, 6), frequency = 252)
corn.ts <- ts(futures.ts$`Corn`, start = c(2014, 1), end = c(2024, 6), frequency = 252)
#### Quebra Estrutural Livestock ----
structure_breaks_live_cattle <- structure_breaks(live_cattle.ts)
structure_breaks_feeder_cattle <- structure_breaks(feeder_cattle.ts)
structure_breaks_corn <- structure_breaks(corn.ts)

live_cattle.ts.dates <- structure_breaks_live_cattle$dates
feeder_cattle.ts.dates <- structure_breaks_feeder_cattle$dates
corn.ts.break.dates <- structure_breaks_corn$dates

live_cattle.ts.break.ci <- structure_breaks_live_cattle$sctest
feeder_cattle.ts.break.ci <- structure_breaks_feeder_cattle$sctest
corn.ts.break.ci <- structure_breaks_corn$sctest

tree_plot(live_cattle.ts, feeder_cattle.ts, corn.ts, live_cattle.ts.break.ci, feeder_cattle.ts.break.ci, corn.ts.break.ci)

#### Dummies para Quebra Estrutural (Para Teste Joh) ----
dummy_live <- create_dummy_variable(live_cattle.ts, live_cattle.ts.dates)
dummy_feeder <- create_dummy_variable(feeder_cattle.ts, feeder_cattle.ts.dates)
dummy_corn <- create_dummy_variable(corn.ts, corn.ts.break.dates)
#### Bind Modelo Livestock Cointegration (Daily) ----
live_stock_model <- cbind(live_cattle.ts, feeder_cattle.ts, corn.ts)
#### Seleção Lag Criteria Livestock (Daily) ----
live_stock_lag <- vars::VARselect(live_stock_model, lag.max = 100, type = 'both')
live_stock_lag$selection
live_stock_lag$criteria

# Criando Banco de Dados (Tabela)
lag_stock_citeria_df <- as.data.frame(live_stock_lag$criteria)

# Transpose
lag_stock_citeria_df <- as.data.frame(t(lag_stock_citeria_df)) 

# Round
lag_stock_citeria_df <- lag_stock_citeria_df |>
  mutate(across(everything(), ~ round(.x, 3)))
#### Johansen Test Eigen Livestock (Daily) com dumvar ---- 
joh.test_1_livestock <- ca.jo(live_stock_model, type = "trace", ecdet = "trend", K=2, season = 252, spec = "longrun",
                              dumvar = cbind(dummy_live, dummy_feeder, dummy_corn)
                              )
summary(joh.test_1_livestock)

# r representa a rota da matriz de correção de erros. Quantos relacionamentos de cointegração existem em seu sistema
# quando r = 0 a estatística do teste é igual a 59,88, e é maior que 5% (22,00), rejeitamos a hipótese nula de que r = 0, portanto há pelo menos uma cointegração
# quando r <= 1 a estatística do teste é igual a 13,96, e é maior que 5% (13,75), rejeitamos a hipótese nula de que r <= 1, portanto há mais de uma cointegração
# quando r <= 2 a estatística de teste é igual a 6,29, e é menor que 5% (7,52), não rejeitamos a hipótese nula de que r <= 2, portanto há duas cointegrações


#### Agrupando preço fechamento futuro para mês (Mediana) para utilizar no Impulso Resposta (Month) ----
monthly_data <- futures %>%
  group_by(month = floor_date(Date, "month")) %>%
  summarize(across(c(`Live Cattle`, `Feeder Cattle`, `Corn`), median, na.rm = TRUE)) %>%
  ungroup()

#### TS Livestock (Month) ----
live_cattle.month.ts <- ts(monthly_data$`Live Cattle`, start = c(2014, 1), end = c(2024, 6), frequency = 12)
feeder_cattle.month.ts <- ts(monthly_data$`Feeder Cattle`, start = c(2014, 1), end = c(2024, 6), frequency = 12)
corn.month.ts <- ts(monthly_data$`Corn`, start = c(2014, 1), end = c(2024, 6), frequency = 12)
#### Bind Modelo Livestock Cointegration (Month) (r = 1 do Daily) ----
live_stock_model_month <- cbind(live_cattle.month.ts, feeder_cattle.month.ts, corn.month.ts)

#### Seleção Lag Criteria Livestock (Month) ----
live_stock_lag_month <- vars::VARselect(live_stock_model_month, lag.max = 15, type = 'const')
live_stock_lag_month$selection

#### Johansen Test Eingen Livestock (Month) ----
joh.test_monthly <- ca.jo(live_stock_model_month, type = "eigen", ecdet = "const", K = 14)
summary(joh.test_monthly)

#### VECM para VAR (Month) para Impulso Resposta ----
vecm_model_month <- vars::vec2var(joh.test_monthly, r = 1) 

#### Impulso Resposta (10 Month) ----
irf_results <- vars::irf(vecm_model_month, impulse = c("feeder_cattle.month.ts", "corn.month.ts", "live_cattle.month.ts"), response = c("feeder_cattle.month.ts", "corn.month.ts", "live_cattle.month.ts"), n.ahead = 10, ortho = TRUE)

#### Função para extrair VAR IRF (Month) ----
source_url("https://raw.githubusercontent.com/anguyen1210/var-tools/master/R/extract_varirf.R")

#### Extraindo VAR IRF (Month) ----
single_varirf <- extract_varirf(irf_results)

#### Visualização Impulso Resposta (Month) ----
# feeder to feeder, feeder to corn, feeder to live
# corn to feeder, corn to corn, corn to live
# live to feeder, live to corn, live to live
# Create plots for each impulse-response pair with specified y-axis limits
feeder_feeder <- irf_plot(single_varirf, x="period", y="irf_feeder_cattle.month.ts_feeder_cattle.month.ts", ymin="lower_feeder_cattle.month.ts_feeder_cattle.month.ts", ymax="upper_feeder_cattle.month.ts_feeder_cattle.month.ts", title = "Feeder to Feeder", ylab="", ylim_values=c(-5, 5))
feeder_corn <- irf_plot(single_varirf, x="period", y="irf_feeder_cattle.month.ts_corn.month.ts", ymin="lower_feeder_cattle.month.ts_corn.month.ts", ymax="upper_feeder_cattle.month.ts_corn.month.ts", title = "Feeder to Corn", ylab="", ylim_values=c(-30, 30))
feeder_live <- irf_plot(single_varirf, x="period", y="irf_feeder_cattle.month.ts_live_cattle.month.ts", ymin="lower_feeder_cattle.month.ts_live_cattle.month.ts", ymax="upper_feeder_cattle.month.ts_live_cattle.month.ts", title = "Feeder to Cattle", ylab="", ylim_values=c(-5, 5))

corn_feeder <- irf_plot(single_varirf, x="period", y="irf_corn.month.ts_feeder_cattle.month.ts", ymin="lower_corn.month.ts_feeder_cattle.month.ts", ymax="upper_corn.month.ts_feeder_cattle.month.ts", title = "Corn to Feeder", ylab="", ylim_values=c(-5, 7))
corn_corn <- irf_plot(single_varirf, x="period", y="irf_corn.month.ts_corn.month.ts", ymin="lower_corn.month.ts_corn.month.ts", ymax="upper_corn.month.ts_corn.month.ts", title = "Corn to Corn", ylab="", ylim_values=c(-5, 50))
corn_live <- irf_plot(single_varirf, x="period", y="irf_corn.month.ts_live_cattle.month.ts", ymin="lower_corn.month.ts_live_cattle.month.ts", ymax="upper_corn.month.ts_live_cattle.month.ts", title = "Corn to Live", ylab="", ylim_values=c(-5, 7))

live_feeder <- irf_plot(single_varirf, x="period", y="irf_live_cattle.month.ts_feeder_cattle.month.ts", ymin="lower_live_cattle.month.ts_feeder_cattle.month.ts", ymax="upper_live_cattle.month.ts_feeder_cattle.month.ts", title = "Live to Feeder", ylab="", ylim_values=c(-10, 10))
live_corn <- irf_plot(single_varirf, x="period", y="irf_live_cattle.month.ts_corn.month.ts", ymin="lower_live_cattle.month.ts_corn.month.ts", ymax="upper_live_cattle.month.ts_corn.month.ts", title = "Live to Corn", ylab="", ylim_values=c(-25, 25))
live_live <- irf_plot(single_varirf, x="period", y="irf_live_cattle.month.ts_live_cattle.month.ts", ymin="lower_live_cattle.month.ts_live_cattle.month.ts", ymax="upper_live_cattle.month.ts_live_cattle.month.ts", title = "Live to Live", ylab="", ylim_values=c(-10, 10))

grid.arrange(
  feeder_feeder, feeder_corn, feeder_live,
  corn_feeder, corn_corn, corn_live,
  live_feeder, live_corn, live_live,
  ncol = 3)

#### Decomposição da Variância (Month) ----
fevd_results <- vars::fevd(vecm_model_month, n.ahead = 10)

# Decomposicao Var Feeder
feeder_dec <- as.data.frame(fevd_results$feeder_cattle.month.ts) |>
  mutate(across(everything(), ~ round(.x, 3)))

# Decomposicao Var Live
live_dec <- as.data.frame(fevd_results$live_cattle.month.ts) |>
  mutate(across(everything(), ~ round(.x, 3)))

# Decomposicao Var Corn
corn_dec <- as.data.frame(fevd_results$corn)  |>
  mutate(across(everything(), ~ round(.x, 3)))


#### Decomposicao da Variância (Month) Plot ----
fevd_df <- data.frame(
  Time = rep(1:10, times = 3),
  Variable = rep(c("live_cattle.month.ts", "feeder_cattle.month.ts", "corn.month.ts"), each = 10),
  Self = c(fevd_results$live_cattle.month.ts[, "live_cattle.month.ts"],
           fevd_results$feeder_cattle.month.ts[, "feeder_cattle.month.ts"],
           fevd_results$corn.month.ts[, "corn.month.ts"]),
  Feeder_to_Live = c(fevd_results$live_cattle.month.ts[, "feeder_cattle.month.ts"], rep(NA, 20)),
  Corn_to_Live = c(fevd_results$live_cattle.month.ts[, "corn.month.ts"], rep(NA, 20)),
  Live_to_Feeder = c(rep(NA, 10), fevd_results$feeder_cattle.month.ts[, "live_cattle.month.ts"], rep(NA, 10)),
  Corn_to_Feeder = c(rep(NA, 10), fevd_results$feeder_cattle.month.ts[, "corn.month.ts"], rep(NA, 10)),
  Live_to_Corn = c(rep(NA, 20), fevd_results$corn.month.ts[, "live_cattle.month.ts"]),
  Feeder_to_Corn = c(rep(NA, 20), fevd_results$corn.month.ts[, "feeder_cattle.month.ts"])
)

# Fazendo Data Long
fevd_long <- gather(fevd_df, key = "Source", value = "Variance_Explained", -Time, -Variable)

# GGplot
ggplot(fevd_long, aes(x = Time, y = Variance_Explained, fill = Source)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Variable, scales = "free_y", ncol = 1) +
  labs(x = "Período em meses", y = "Variância Explicada", title = "")
