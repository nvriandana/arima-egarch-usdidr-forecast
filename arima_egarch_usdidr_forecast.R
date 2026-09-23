# ==============================================================================
# TITLE   : Modeling and Forecasting the Volatility of the USD/IDR Exchange Rate
#           Using an ARIMA-GARCH Approach
# AUTHOR  : NOVIAN RIANDANA
# COURSE  : Time Series Analysis
# VERSION : 3.0
# DATE    : 24 April 2026
# TARGET  : Scientific Journal (Sinta 4 indexed) in the field of 
#           Actuarial Science/Applied Mathematics. 
# ==============================================================================
# PHASE 0: ENVIRONMENT SETUP
# ==============================================================================
# ------------------------------------------------------------------------------
# 0.1  Safety Reset – clears any corrupt graphics state from previous runs
# ------------------------------------------------------------------------------
graphics.off()

# ------------------------------------------------------------------------------
# 0.2  Install Missing Packages
#      Uncomment the block below on FIRST RUN ONLY, then re-comment it.
# ------------------------------------------------------------------------------
# install.packages(c("readxl", "tseries", "forecast", "FinTS", "rugarch",
#                    "ggplot2", "xts", "zoo", "moments", "lmtest",
#                    "gridExtra", "scales", "Metrics", "future", "strucchange"),
#                  dependencies = TRUE)

# ------------------------------------------------------------------------------
# 0.3  Load Libraries
# ------------------------------------------------------------------------------
library(readxl)      # Reading Excel files
library(xts)         # Extensible time-series objects
library(zoo)         # Ordered observations / rolling windows
library(tseries)     # Unit-root tests, Jarque-Bera
library(forecast)    # auto.arima(), Arima(), checkresiduals()
library(FinTS)       # ARCH-LM test (ArchTest)
library(rugarch)     # GARCH family specification and fitting
library(ggplot2)     # Publication-quality graphics
library(moments)     # skewness(), kurtosis()
library(lmtest)      # coeftest()
library(scales)      # comma(), date_format() for ggplot2 axes
library(Metrics)     # rmse(), mape() convenience functions
library(strucchange) # Bai-Perron multiple breakpoint test, Chow test (REVISION: Reviewer Pt.5)

cat("================================================================\n")
cat("  All libraries loaded successfully.\n")
cat("================================================================\n\n")

# ==============================================================================
# PHASE 1: DATA PREPARATION & EXPLORATORY DATA ANALYSIS
# ==============================================================================

cat("================================================================\n")
cat("  PHASE 1: Data Import, EDA, and Train-Test Split\n")
cat("================================================================\n\n")

# ------------------------------------------------------------------------------
# 1.1  Import Data
# ------------------------------------------------------------------------------
FILE_PATH <- "C:/Users/novia/Downloads/President University/20252/Linear Regression and Time Series/ARIMA-EGARCH/Novian-dataset-W15.xlsx"
SHEET_NAME <- "Data_Harian"

raw_data <- read_excel(FILE_PATH, sheet = SHEET_NAME)
colnames(raw_data) <- c("Tanggal", "Kurs_Penutupan_USD_IDR", "Log_Return")

raw_data$Tanggal                <- as.Date(raw_data$Tanggal)
raw_data$Kurs_Penutupan_USD_IDR <- as.numeric(raw_data$Kurs_Penutupan_USD_IDR)
raw_data$Log_Return             <- as.numeric(raw_data$Log_Return)
raw_data                        <- na.omit(raw_data)

cat("--- Full Dataset Summary ---\n")
cat(sprintf("  Total observations : %d rows\n", nrow(raw_data)))
cat(sprintf("  Date range         : %s  to  %s\n",
            format(min(raw_data$Tanggal), "%d %B %Y"),
            format(max(raw_data$Tanggal), "%d %B %Y")))
cat(sprintf("  Columns            : %s\n\n",
            paste(colnames(raw_data), collapse = ", ")))

# ------------------------------------------------------------------------------
# 1.2  Train-Test Split
#
#   Training Set (In-Sample)    : April 1,  2021 – March 31, 2025
#   Testing Set  (Out-of-Sample): April 1,  2025 – April 23, 2026
# ------------------------------------------------------------------------------
SPLIT_DATE <- as.Date("2025-03-31")

train_data <- raw_data[raw_data$Tanggal <= SPLIT_DATE, ]
test_data  <- raw_data[raw_data$Tanggal >  SPLIT_DATE, ]

n_train <- nrow(train_data)
n_test  <- nrow(test_data)

cat("--- Train-Test Split Summary ---\n")
cat(sprintf("  Training Set : %d obs  |  %s  to  %s\n",
            n_train,
            format(min(train_data$Tanggal), "%d %B %Y"),
            format(max(train_data$Tanggal), "%d %B %Y")))
cat(sprintf("  Testing Set  : %d obs  |  %s  to  %s\n\n",
            n_test,
            format(min(test_data$Tanggal), "%d %B %Y"),
            format(max(test_data$Tanggal), "%d %B %Y")))

# Plain numeric vectors used by statistical functions
train_return     <- train_data$Log_Return
test_return      <- test_data$Log_Return
train_price      <- train_data$Kurs_Penutupan_USD_IDR
test_price       <- test_data$Kurs_Penutupan_USD_IDR
last_train_price <- tail(train_price, 1)

cat(sprintf("  Anchor price (last Training Set close) : IDR %.2f\n\n",
            last_train_price))

# ------------------------------------------------------------------------------
# 1.2b  Naive Random-Walk Benchmark (computed once, reused by Phase 4.3 & 5)
#       [REVISION: moved earlier so it's available to the multi-model
#        forecast-comparison table added in response to reviewer Pt.3]
# ------------------------------------------------------------------------------
naive_errors <- diff(c(last_train_price, test_price))
MAE_naive    <- mean(abs(naive_errors))
cat(sprintf("  Naive (random-walk) benchmark MAE      : IDR %.4f\n\n", MAE_naive))

# ------------------------------------------------------------------------------
# 1.3  Descriptive Statistics – Training Set
# ------------------------------------------------------------------------------
cat("--- Descriptive Statistics: Log Return (Training Set) ---\n")
desc_train <- c(
  N           = length(train_return),
  Mean        = mean(train_return),
  Std.Dev     = sd(train_return),
  Min         = min(train_return),
  Max         = max(train_return),
  Skewness    = moments::skewness(train_return),
  Ex.Kurtosis = moments::kurtosis(train_return) - 3
)
print(round(desc_train, 7))
cat("\n")

# ------------------------------------------------------------------------------
# 1.4  Colour Palette (used throughout all plots)
# ------------------------------------------------------------------------------
TRAIN_COLOR <- "#1f77b4"   # steel blue  – training actuals
TEST_COLOR  <- "#ff7f0e"   # orange      – testing actuals
FORE_COLOR  <- "#d62728"   # red         – forecast line

# Label factor for ggplot colour-coding
raw_data$Set <- factor(
  ifelse(raw_data$Tanggal <= SPLIT_DATE, "Training Set", "Testing Set"),
  levels = c("Training Set", "Testing Set")
)

# ------------------------------------------------------------------------------
# 1.5  Figure 1 – USD/IDR Daily Closing Price (Full Series, colour-coded)
# ------------------------------------------------------------------------------
p_fig1 <- ggplot(raw_data,
                 aes(x = Tanggal, y = Kurs_Penutupan_USD_IDR, color = Set)) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = as.numeric(SPLIT_DATE + 1),
             linetype = "dashed", color = "gray40", linewidth = 0.7) +
  annotate("text",
           x = SPLIT_DATE + 5,
           y = max(raw_data$Kurs_Penutupan_USD_IDR) * 0.98,
           label = "Split\nPoint", hjust = 0, size = 3.2, color = "gray30") +
  scale_color_manual(values = c("Training Set" = TRAIN_COLOR,
                                "Testing Set"  = TEST_COLOR)) +
  scale_y_continuous(labels = comma) +
  labs(title    = "Figure 1: USD/IDR Daily Closing Exchange Rate (2021-2026)",
       subtitle = "Blue = Training Set  |  Orange = Testing Set",
       x        = "Date",
       y        = "Closing Rate (IDR per 1 USD)",
       color    = NULL) +
  theme_bw(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        plot.subtitle   = element_text(color = "gray40"),
        legend.position = "top")

ggsave("Figure1_Closing_Price_Split.png", plot = p_fig1,
       width = 10, height = 4.5, dpi = 300)
cat("  [Saved] Figure1_Closing_Price_Split.png\n")

# ------------------------------------------------------------------------------
# 1.6  Figure 2 – Daily Log Return (Full Series, colour-coded)
# ------------------------------------------------------------------------------
p_fig2 <- ggplot(raw_data,
                 aes(x = Tanggal, y = Log_Return, color = Set)) +
  geom_line(linewidth = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = as.numeric(SPLIT_DATE + 1),
             linetype = "dashed", color = "gray40", linewidth = 0.7) +
  scale_color_manual(values = c("Training Set" = TRAIN_COLOR,
                                "Testing Set"  = TEST_COLOR)) +
  labs(title    = "Figure 2: Daily Log Return of USD/IDR Exchange Rate (2021-2026)",
       subtitle = "Volatility clustering visible; dashed line = train/test split",
       x        = "Date",
       y        = "Log Return  r(t) = ln(P(t) / P(t-1))",
       color    = NULL) +
  theme_bw(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        plot.subtitle   = element_text(color = "gray40"),
        legend.position = "top")

ggsave("Figure2_Log_Return_Split.png", plot = p_fig2,
       width = 10, height = 4.5, dpi = 300)
cat("  [Saved] Figure2_Log_Return_Split.png\n\n")


# ==============================================================================
# PHASE 1B: STRUCTURAL BREAK TESTING (FULL SAMPLE)
# [REVISION: Added in response to reviewer Pt.5 -- "Since the testing period
#  includes substantial Rupiah depreciation, consider formally testing for
#  structural breaks or regime changes rather than discussing them
#  qualitatively." Two complementary formal tests are run below:
#    (1) Bai-Perron multiple breakpoint test  -- data-driven, no prior date
#        imposed; identifies WHERE a regime shift most likely occurred.
#    (2) Chow test at the Train/Test split    -- confirmatory test of
#        whether the shift coincides specifically with the start of the
#        Testing Set (01 April 2025), which is the date the forecast
#        evaluation in Section III-J/K implicitly assumes.
#  Both use the full-sample log return series (Training + Testing). ]
# ==============================================================================

cat("================================================================\n")
cat("  PHASE 1B: Structural Break Testing (Full Sample)\n")
cat("================================================================\n\n")

# ------------------------------------------------------------------------------
# 1B.1  Bai-Perron Multiple Breakpoint Test (Log Return series, mean-shift model)
# ------------------------------------------------------------------------------
cat("--- Bai-Perron Multiple Breakpoint Test (Log Return, full sample) ---\n")
cat("    H0: no structural break (constant mean) in the log return series\n\n")

bp_data <- data.frame(Log_Return = raw_data$Log_Return)

# h = minimum segment size as a fraction of total sample (10% here, a common
# default that avoids over-fitting spurious breaks in a ~1,300-obs series)
bp_fit <- breakpoints(Log_Return ~ 1, data = bp_data, h = 0.10)

cat("  Model selection across candidate number of breakpoints (RSS / BIC):\n")
print(summary(bp_fit))

bp_optimal_idx <- bp_fit$breakpoints
bp_dates <- if (!is.null(bp_optimal_idx) && !all(is.na(bp_optimal_idx))) {
  raw_data$Tanggal[bp_optimal_idx]
} else {
  NA
}

cat("\n  BIC-optimal breakpoint date(s) detected in the FULL sample:\n")
if (!all(is.na(bp_dates))) {
  print(bp_dates)
  cat(sprintf("\n  ==> %d breakpoint(s) detected. Compare against the Train/Test\n",
              length(bp_dates)))
  cat(sprintf("      split date (%s) and the observed depreciation onset in\n",
              format(SPLIT_DATE, "%d %B %Y")))
  cat("      Figure 15 (approx. mid-2025).\n\n")
} else {
  cat("  No statistically supported breakpoint was detected by the BIC criterion.\n\n")
}

# Figure S1 -- Breakpoints overlaid on the log return series
png("FigureS1_Structural_Breaks_LogReturn.png", width = 2600, height = 1100, res = 300)
plot(raw_data$Tanggal, raw_data$Log_Return, type = "l", col = "gray40",
     main = "Figure S1: Bai-Perron Structural Breaks -- Log Return Series",
     xlab = "Date", ylab = "Log Return")
abline(h = 0, col = "gray70", lty = 3)
if (!all(is.na(bp_dates))) {
  abline(v = bp_dates, col = "red", lty = 2, lwd = 1.5)
}
abline(v = SPLIT_DATE, col = "blue", lty = 3, lwd = 1.5)
legend("topleft",
       legend = c("Detected Breakpoint(s)", "Train/Test Split"),
       col    = c("red", "blue"), lty = c(2, 3), lwd = 1.5,
       bty = "n", cex = 0.8)
dev.off()
cat("  [Saved] FigureS1_Structural_Breaks_LogReturn.png\n\n")

# ------------------------------------------------------------------------------
# 1B.2  Chow Test at the Train/Test Split Date
#       Formal confirmatory test: does the return-generating process shift
#       exactly at the point the Testing Set begins?
# ------------------------------------------------------------------------------
cat("--- Chow Test at the Train/Test Split Point ---\n")
cat("    H0: no structural break at the specified breakpoint (parameter stability)\n\n")

split_index <- which(raw_data$Tanggal == SPLIT_DATE)

chow_test <- sctest(Log_Return ~ 1, data = bp_data,
                     type = "Chow", point = split_index)
print(chow_test)
cat(sprintf("\n  p-value = %.4e  -->  %s\n\n",
            chow_test$p.value,
            ifelse(chow_test$p.value < 0.05,
                   "REJECT H0 => Structural break confirmed at the Train/Test split.",
                   "FAIL TO REJECT H0 => No significant break detected exactly at this point.")))

cat("--- Interpretation (Mean-Level Break Tests) ---\n")
cat("  The Bai-Perron test is data-driven and locates breakpoint(s) in the MEAN\n")
cat("  of the return series without imposing a prior date; the Chow test formally\n")
cat("  evaluates whether a mean-level shift coincides specifically with the start\n")
cat("  of the Testing Set. If neither test finds a significant break, this does\n")
cat("  NOT rule out a regime change in VOLATILITY -- exchange-rate depreciation\n")
cat("  episodes typically manifest as transient volatility bursts rather than a\n")
cat("  persistent shift in the mean return. Section 1B.3 below tests this\n")
cat("  volatility-based hypothesis directly on the squared log return series.\n\n")

# ------------------------------------------------------------------------------
# 1B.3  Structural Break Test on VOLATILITY (Squared Log Return)
#       [ADDED: complements 1B.1/1B.2, which test the MEAN of returns and may
#        be underpowered to detect the kind of regime change most relevant to
#        this paper -- a transient volatility burst during the Rupiah
#        depreciation episode (see Figure 2, Apr-May 2025 window), rather than
#        a permanent shift in the average return level.]
# ------------------------------------------------------------------------------
cat("--- Bai-Perron Breakpoint Test on Squared Log Return (Volatility Proxy) ---\n")
cat("    H0: no structural break in the (unconditional) variance level\n\n")

bp_data_vol <- data.frame(Sq_Log_Return = raw_data$Log_Return^2)
bp_fit_vol  <- breakpoints(Sq_Log_Return ~ 1, data = bp_data_vol, h = 0.10)

cat("  Model selection across candidate number of breakpoints (RSS / BIC):\n")
print(summary(bp_fit_vol))

bp_vol_idx   <- bp_fit_vol$breakpoints
bp_vol_dates <- if (!is.null(bp_vol_idx) && !all(is.na(bp_vol_idx))) {
  raw_data$Tanggal[bp_vol_idx]
} else {
  NA
}

cat("\n  BIC-optimal volatility breakpoint date(s):\n")
if (!all(is.na(bp_vol_dates))) {
  print(bp_vol_dates)
  cat(sprintf("\n  ==> %d volatility breakpoint(s) detected.\n\n", length(bp_vol_dates)))
} else {
  cat("  No statistically supported volatility breakpoint was detected.\n\n")
}

# Chow test on squared returns, at the Train/Test split
chow_test_vol <- sctest(Sq_Log_Return ~ 1, data = bp_data_vol,
                         type = "Chow", point = split_index)
print(chow_test_vol)
cat(sprintf("\n  p-value = %.4e  -->  %s\n\n",
            chow_test_vol$p.value,
            ifelse(chow_test_vol$p.value < 0.05,
                   "REJECT H0 => Volatility regime shift confirmed at the Train/Test split.",
                   "FAIL TO REJECT H0 => No significant volatility shift detected exactly at this point.")))

# Figure S1b -- Volatility breakpoints overlaid on squared log return series
png("FigureS1b_Structural_Breaks_Volatility.png", width = 2600, height = 1100, res = 300)
plot(raw_data$Tanggal, raw_data$Log_Return^2, type = "l", col = "gray40",
     main = "Figure S1b: Bai-Perron Structural Breaks -- Squared Log Return (Volatility)",
     xlab = "Date", ylab = "Squared Log Return")
if (!all(is.na(bp_vol_dates))) {
  abline(v = bp_vol_dates, col = "red", lty = 2, lwd = 1.5)
}
abline(v = SPLIT_DATE, col = "blue", lty = 3, lwd = 1.5)
legend("topleft",
       legend = c("Detected Volatility Breakpoint(s)", "Train/Test Split"),
       col    = c("red", "blue"), lty = c(2, 3), lwd = 1.5,
       bty = "n", cex = 0.8)
dev.off()
cat("  [Saved] FigureS1b_Structural_Breaks_Volatility.png\n\n")

cat("--- Combined Interpretation (Mean vs. Volatility Break Tests) ---\n")
cat("  Reading 1B.1/1B.2 together with 1B.3 distinguishes two competing\n")
cat("  explanations for MASE > 1 (Section III-J of the manuscript):\n")
cat("    (a) If NEITHER mean nor volatility breaks are detected: the\n")
cat("        forecast underperformance is not attributable to an identifiable\n")
cat("        regime shift, lending stronger support to the EMH interpretation.\n")
cat("    (b) If a VOLATILITY break is detected but no MEAN break: the\n")
cat("        depreciation episode is consistent with a transient volatility\n")
cat("        regime change (consistent with EGARCH's own leverage/persistence\n")
cat("        estimates) rather than a permanent shift in the price-forecasting\n")
cat("        model's mean equation -- this should be reported as a genuine\n")
cat("        competing explanation for MASE > 1, not silently folded into the\n")
cat("        EMH narrative.\n")
cat("    (c) If BOTH break: report both and discuss jointly.\n\n")


# ==============================================================================
# PHASE 2: MEAN EQUATION MODELING – ARIMA (TRAINING SET ONLY)
# ==============================================================================

cat("================================================================\n")
cat("  PHASE 2: ARIMA Mean Equation Modeling (Training Set Only)\n")
cat("================================================================\n\n")

# ------------------------------------------------------------------------------
# 2.1  Stationarity Tests
# ------------------------------------------------------------------------------
cat("--- Augmented Dickey-Fuller (ADF) Test ---\n")
adf_train <- adf.test(train_return, alternative = "stationary")
print(adf_train)
cat(sprintf("  p-value = %.4f  -->  %s\n\n",
            adf_train$p.value,
            ifelse(adf_train$p.value < 0.05,
                   "REJECT H0 => Series is stationary at 5% significance level.",
                   "FAIL TO REJECT H0 => Series is non-stationary.")))

cat("--- Phillips-Perron (PP) Test ---\n")
pp_train <- pp.test(train_return, alternative = "stationary")
print(pp_train)
cat(sprintf("  p-value = %.4f  -->  %s\n\n",
            pp_train$p.value,
            ifelse(pp_train$p.value < 0.05,
                   "REJECT H0 => Series is stationary at 5% significance level.",
                   "FAIL TO REJECT H0 => Series is non-stationary.")))

# ------------------------------------------------------------------------------
# 2.2  Figure 3 – ACF of Training Set Log Return
# ------------------------------------------------------------------------------
png("Figure3_ACF_Log_Return.png", width = 2400, height = 1200, res = 300)
acf(train_return, lag.max = 40,
    main = "Figure 3: ACF of Log Return (Training Set)")
dev.off()
cat("  [Saved] Figure3_ACF_Log_Return.png\n")

# ------------------------------------------------------------------------------
# 2.3  Figure 4 – PACF of Training Set Log Return
# ------------------------------------------------------------------------------
png("Figure4_PACF_Log_Return.png", width = 2400, height = 1200, res = 300)
pacf(train_return, lag.max = 40,
     main = "Figure 4: PACF of Log Return (Training Set)")
dev.off()
cat("  [Saved] Figure4_PACF_Log_Return.png\n\n")

# ------------------------------------------------------------------------------
# 2.4  ARIMA Model Selection (fit on Training Set ONLY)
# ------------------------------------------------------------------------------
cat("--- auto.arima() on Training Set (exhaustive search) ---\n")
auto_arima_train <- auto.arima(
  train_return,
  stationary    = TRUE,
  seasonal      = FALSE,
  ic            = "aic",
  stepwise      = FALSE,
  approximation = FALSE,
  trace         = TRUE
)

cat("\n--- Best ARIMA Selected by auto.arima() ---\n")
print(summary(auto_arima_train))
cat(sprintf("\n  Best ARIMA order : (%d, %d, %d)\n",
            auto_arima_train$arma[1],
            auto_arima_train$arma[6],
            auto_arima_train$arma[2]))
cat(sprintf("  AIC = %.4f  |  BIC = %.4f\n\n",
            AIC(auto_arima_train), BIC(auto_arima_train)))

# Manual candidate grid for the paper's comparison table
cat("--- Manual Candidate Grid (AIC / BIC Comparison) ---\n")
candidate_orders <- list(
  c(0,0,0), c(1,0,0), c(0,0,1), c(1,0,1),
  c(2,0,0), c(0,0,2), c(2,0,1), c(1,0,2),
  c(2,0,2), c(3,0,0), c(0,0,3)
)

cand_df <- data.frame(Model = character(),
                      AIC   = numeric(),
                      BIC   = numeric(),
                      stringsAsFactors = FALSE)

for (ord in candidate_orders) {
  tryCatch({
    fit_tmp <- Arima(train_return, order = ord, include.mean = TRUE)
    cand_df <- rbind(cand_df, data.frame(
      Model = sprintf("ARIMA(%d,%d,%d)", ord[1], ord[2], ord[3]),
      AIC   = round(AIC(fit_tmp), 4),
      BIC   = round(BIC(fit_tmp), 4)))
  }, error = function(e) NULL)
}
cand_df <- cand_df[order(cand_df$AIC), ]
rownames(cand_df) <- NULL
print(cand_df)
cat(sprintf("\n  ==> Best candidate (lowest AIC): %s\n\n", cand_df$Model[1]))

# Set the best ARIMA model and extract residuals
best_arima_train  <- auto_arima_train
arima_resid_train <- residuals(best_arima_train)
p_order           <- best_arima_train$arma[1]
q_order           <- best_arima_train$arma[2]

# ------------------------------------------------------------------------------
# 2.5  ARIMA Residual Diagnostics
# ------------------------------------------------------------------------------
cat("--- ARIMA Residual Diagnostics ---\n")

lb_arima <- Box.test(arima_resid_train, lag = 20, type = "Ljung-Box",
                     fitdf = p_order + q_order)
jb_arima <- jarque.bera.test(arima_resid_train)

cat(sprintf("\n  Ljung-Box (lag=20) : p = %.4f  -->  %s\n",
            lb_arima$p.value,
            ifelse(lb_arima$p.value > 0.05,
                   "White noise. [PASS]",
                   "Autocorrelation remains. [FAIL]")))
cat(sprintf("  Jarque-Bera        : p = %.4e  -->  %s\n\n",
            jb_arima$p.value,
            ifelse(jb_arima$p.value > 0.05,
                   "Approximately normal. [PASS]",
                   "Non-normal (fat tails expected in financial returns). [NOTE]")))

# ------------------------------------------------------------------------------
# 2.6  Figure 5 – ARIMA Residuals Time Plot
# ------------------------------------------------------------------------------
png("Figure5_ARIMA_Residuals.png", width = 2600, height = 1100, res = 300)
plot(arima_resid_train, type = "l", col = TRAIN_COLOR,
     main = "Figure 5: ARIMA Residuals (Training Set)",
     ylab = "Residual", xlab = "Observation Index")
abline(h = 0, col = "red", lty = 2)
dev.off()
cat("  [Saved] Figure5_ARIMA_Residuals.png\n")

# ------------------------------------------------------------------------------
# 2.7  Figure 6 – ACF of ARIMA Residuals
# ------------------------------------------------------------------------------
png("Figure6_ACF_ARIMA_Residuals.png", width = 2400, height = 1200, res = 300)
acf(arima_resid_train, lag.max = 40,
    main = "Figure 6: ACF of ARIMA Residuals")
dev.off()
cat("  [Saved] Figure6_ACF_ARIMA_Residuals.png\n")

# ------------------------------------------------------------------------------
# 2.8  Figure 7 – ACF of Squared ARIMA Residuals (ARCH Effect Check)
# ------------------------------------------------------------------------------
png("Figure7_ACF_Squared_ARIMA_Residuals.png", width = 2400, height = 1200, res = 300)
acf(arima_resid_train^2, lag.max = 40,
    main = "Figure 7: ACF of Squared ARIMA Residuals (ARCH Effect Check)")
dev.off()
cat("  [Saved] Figure7_ACF_Squared_ARIMA_Residuals.png\n\n")


# ==============================================================================
# PHASE 3: VOLATILITY EQUATION MODELING – GARCH (TRAINING SET ONLY)
# ==============================================================================

cat("================================================================\n")
cat("  PHASE 3: GARCH Volatility Modeling (Training Set Only)\n")
cat("================================================================\n\n")

# ------------------------------------------------------------------------------
# 3.1  ARCH-LM Test on ARIMA Residuals
# ------------------------------------------------------------------------------
cat("--- ARCH-LM Test on ARIMA Residuals ---\n")
arch_lm_train <- ArchTest(arima_resid_train, lags = 12)
print(arch_lm_train)
cat(sprintf("  p-value = %.4e  -->  %s\n\n",
            arch_lm_train$p.value,
            ifelse(arch_lm_train$p.value < 0.05,
                   "REJECT H0 => Significant ARCH effects => GARCH modeling is warranted.",
                   "FAIL TO REJECT H0 => No significant ARCH effects detected.")))

# ------------------------------------------------------------------------------
# 3.2  GARCH Model Specification, Estimation, and Selection
# ------------------------------------------------------------------------------
cat("--- Fitting GARCH Family Models on Training Set ---\n\n")

make_spec <- function(var_model = "sGARCH", dist = "std") {
  ugarchspec(
    variance.model     = list(model = var_model, garchOrder = c(1, 1)),
    mean.model         = list(armaOrder = c(p_order, q_order),
                              include.mean = TRUE),
    distribution.model = dist
  )
}

spec_list <- list(
  "GARCH(1,1)-Normal"    = make_spec("sGARCH",   "norm"),
  "GARCH(1,1)-Std-t"     = make_spec("sGARCH",   "std"),
  "GJR-GARCH(1,1)-Std-t" = make_spec("gjrGARCH", "std"),
  "EGARCH(1,1)-Std-t"    = make_spec("eGARCH",   "std")
)

fit_list <- lapply(spec_list, function(spec) {
  ugarchfit(spec = spec, data = train_return, solver = "hybrid")
})

# Information Criteria comparison table
ic_rows <- lapply(names(fit_list), function(nm) {
  f  <- fit_list[[nm]]
  ic <- infocriteria(f)
  data.frame(Model  = nm,
             AIC    = round(ic[1], 6),
             BIC    = round(ic[2], 6),
             LogLik = round(likelihood(f), 4),
             stringsAsFactors = FALSE)
})
ic_table <- do.call(rbind, ic_rows)
ic_table <- ic_table[order(ic_table$AIC), ]
rownames(ic_table) <- NULL

cat("--- Model Selection Criteria (Training Set) ---\n")
print(ic_table)

best_model_name <- ic_table$Model[1]
best_garch_fit  <- fit_list[[best_model_name]]
cat(sprintf("\n  ==> Best model (lowest AIC): %s\n", best_model_name))
cat(sprintf("  Using '%s' as the selected GARCH specification.\n\n",
            best_model_name))

cat(sprintf("--- Full Coefficient Table: %s ---\n", best_model_name))
show(best_garch_fit)

# Variance persistence
gc      <- coef(best_garch_fit)
alpha1  <- gc["alpha1"]
beta1   <- gc["beta1"]
persist <- alpha1 + beta1
cat(sprintf("\n  Variance Persistence (alpha1 + beta1) = %.6f\n", persist))
cat(sprintf("  %s\n\n",
            ifelse(persist < 1,
                   "Covariance stationarity holds (persistence < 1).",
                   "WARNING: Persistence >= 1 (IGARCH behaviour).")))

# ------------------------------------------------------------------------------
# 3.3  GARCH Standardized Residual Diagnostics
# ------------------------------------------------------------------------------
cat("--- GARCH Standardized Residual Diagnostics ---\n")
std_resid_train <- residuals(best_garch_fit, standardize = TRUE)

lb_std   <- Box.test(std_resid_train,   lag = 20, type = "Ljung-Box")
lb_std2  <- Box.test(std_resid_train^2, lag = 20, type = "Ljung-Box")
arch_std <- ArchTest(std_resid_train,   lags = 12)
jb_std   <- jarque.bera.test(as.numeric(std_resid_train))

cat(sprintf("\n  Ljung-Box on z(t)      (lag=20) : p = %.4f  -->  %s\n",
            lb_std$p.value,
            ifelse(lb_std$p.value > 0.05,
                   "No autocorrelation. [PASS]",
                   "Autocorrelation remains. [FAIL]")))
cat(sprintf("  Ljung-Box on z(t)^2   (lag=20) : p = %.4f  -->  %s\n",
            lb_std2$p.value,
            ifelse(lb_std2$p.value > 0.05,
                   "No ARCH effects remain. [PASS]",
                   "Residual ARCH effects detected. [FAIL]")))
cat(sprintf("  ARCH-LM on z(t)        (lag=12) : p = %.4e  -->  %s\n",
            arch_std$p.value,
            ifelse(arch_std$p.value > 0.05,
                   "No ARCH effects. [PASS]",
                   "Remaining ARCH effects. [FAIL]")))
cat(sprintf("  Jarque-Bera on z(t)            : p = %.4e  -->  %s\n\n",
            jb_std$p.value,
            ifelse(jb_std$p.value > 0.05,
                   "Approximately normal. [PASS]",
                   "Non-normal (expected with fat-tailed distribution). [NOTE]")))

# ------------------------------------------------------------------------------
# 3.4  Figure 8 – Standardized Residuals Time Plot
# ------------------------------------------------------------------------------
png("Figure8_Standardized_Residuals.png", width = 2600, height = 1100, res = 300)
plot(as.numeric(std_resid_train), type = "l", col = TRAIN_COLOR,
     main = "Figure 8: Standardized Residuals (Training Set)",
     ylab = "Standardized Residual", xlab = "Observation Index")
abline(h = 0, col = "gray50", lty = 2)
dev.off()
cat("  [Saved] Figure8_Standardized_Residuals.png\n")

# ------------------------------------------------------------------------------
# 3.5  Figure 9 – ACF of Standardized Residuals
# ------------------------------------------------------------------------------
png("Figure9_ACF_Std_Residuals.png", width = 2400, height = 1200, res = 300)
acf(as.numeric(std_resid_train), lag.max = 40,
    main = "Figure 9: ACF of Standardized Residuals")
dev.off()
cat("  [Saved] Figure9_ACF_Std_Residuals.png\n")

# ------------------------------------------------------------------------------
# 3.6  Figure 10 – ACF of Squared Standardized Residuals
# ------------------------------------------------------------------------------
png("Figure10_ACF_Squared_Std_Residuals.png", width = 2400, height = 1200, res = 300)
acf(as.numeric(std_resid_train)^2, lag.max = 40,
    main = "Figure 10: ACF of Squared Standardized Residuals")
dev.off()
cat("  [Saved] Figure10_ACF_Squared_Std_Residuals.png\n")

# ------------------------------------------------------------------------------
# 3.7  Figure 11 – Q-Q Plot of Standardized Residuals
# ------------------------------------------------------------------------------
png("Figure11_QQ_Std_Residuals.png", width = 1800, height = 1800, res = 300)
qqnorm(as.numeric(std_resid_train),
       main = "Figure 11: Q-Q Plot of Standardized Residuals")
qqline(as.numeric(std_resid_train), col = "red", lwd = 1.5)
dev.off()
cat("  [Saved] Figure11_QQ_Std_Residuals.png\n\n")

# ------------------------------------------------------------------------------
# 3.8  Figure 12 – In-Sample Conditional Volatility
# ------------------------------------------------------------------------------
cond_vol_train    <- sigma(best_garch_fit) * 100   # expressed as %
vol_train_df      <- data.frame(
  Date    = train_data$Tanggal,
  CondVol = as.numeric(cond_vol_train)
)

p_fig12 <- ggplot(vol_train_df, aes(x = Date, y = CondVol)) +
  geom_line(color = "#2ca02c", linewidth = 0.5) +
  labs(title    = sprintf("Figure 12: In-Sample Conditional Volatility -- %s",
                          best_model_name),
       subtitle = "Estimated daily conditional standard deviation (Training Set)",
       x        = "Date",
       y        = "Conditional Volatility (% per day)") +
  theme_bw(base_size = 12) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"))

ggsave("Figure12_Conditional_Volatility_Train.png", plot = p_fig12,
       width = 10, height = 4, dpi = 300)
cat("  [Saved] Figure12_Conditional_Volatility_Train.png\n\n")


# ==============================================================================
# PHASE 4: OUT-OF-SAMPLE FORECASTING & PRICE RECONSTRUCTION
# ==============================================================================

cat("================================================================\n")
cat("  PHASE 4: Out-of-Sample Forecasting & Price Reconstruction\n")
cat("================================================================\n\n")

# ------------------------------------------------------------------------------
# 4.1  Generate n_test-step-ahead Forecast from the Fitted GARCH Model
# ------------------------------------------------------------------------------
cat(sprintf("  Generating %d-step-ahead out-of-sample forecast ...\n\n", n_test))

garch_forecast <- ugarchforecast(
  fitORspec = best_garch_fit,
  n.ahead   = n_test,
  data      = train_return
)

fore_mean_return <- as.numeric(fitted(garch_forecast))   # E[r_t | Omega_T]
fore_cond_vol    <- as.numeric(sigma(garch_forecast))    # sqrt(Var[r_t | Omega_T])

cat("  Forecast preview (first 6 steps):\n")
print(data.frame(
  Step             = 1:min(6, n_test),
  Date             = head(test_data$Tanggal,   6),
  Fore_Mean_Return = round(head(fore_mean_return, 6), 7),
  Fore_CondVol_pct = round(head(fore_cond_vol * 100, 6), 6)
))
cat("  ...\n\n")

# ------------------------------------------------------------------------------
# 4.2  Price Level Reconstruction
#
#      Formula : P_hat(t) = P_hat(t-1) * exp( r_hat(t) )
#      Anchor  : last observed closing price in the Training Set
#
#      Note: only previously FORECASTED prices are used in the chain.
#            Actual test prices are NEVER used here (no data leakage).
# ------------------------------------------------------------------------------
cat("--- Reconstructing Forecasted Price Levels ---\n")
cat(sprintf("  Anchor price : IDR %.2f\n\n", last_train_price))

fore_price    <- numeric(n_test)
fore_price[1] <- last_train_price * exp(fore_mean_return[1])
if (n_test > 1) {
  for (i in 2:n_test) {
    fore_price[i] <- fore_price[i - 1] * exp(fore_mean_return[i])
  }
}

cat("  Reconstructed price preview (first 6 steps):\n")
print(data.frame(
  Step       = 1:min(6, n_test),
  Date       = head(test_data$Tanggal, 6),
  Actual_IDR = round(head(test_price,  6), 2),
  Fore_IDR   = round(head(fore_price,  6), 2),
  Diff_IDR   = round(head(test_price - fore_price, 6), 2)
))
cat("  ...\n\n")

# ------------------------------------------------------------------------------
# 4.3  Out-of-Sample Forecast Comparison Across ALL Candidate GARCH Models
#      [REVISION: Added in response to reviewer Pt.3 -- "consider including
#       a comparison of forecasting performance (RMSE/MAE/MAPE) across
#       competing models to demonstrate the superiority of the selected
#       EGARCH model," rather than relying on in-sample AIC alone.]
# ------------------------------------------------------------------------------
cat("--- Out-of-Sample Forecast Comparison: All Candidate GARCH Models ---\n\n")

compute_forecast_accuracy <- function(fit, model_name) {
  fc       <- ugarchforecast(fitORspec = fit, n.ahead = n_test, data = train_return)
  mean_ret <- as.numeric(fitted(fc))

  price_hat    <- numeric(n_test)
  price_hat[1] <- last_train_price * exp(mean_ret[1])
  if (n_test > 1) {
    for (i in 2:n_test) {
      price_hat[i] <- price_hat[i - 1] * exp(mean_ret[i])
    }
  }

  err   <- test_price - price_hat
  abs_e <- abs(err)
  pct_e <- abs_e / test_price * 100

  list(
    model     = model_name,
    price_hat = price_hat,
    RMSE      = sqrt(mean(err^2)),
    MAE       = mean(abs_e),
    MAPE      = mean(pct_e),
    MASE      = mean(abs_e) / MAE_naive
  )
}

forecast_comparison_list <- lapply(names(fit_list), function(nm) {
  compute_forecast_accuracy(fit_list[[nm]], nm)
})
names(forecast_comparison_list) <- names(fit_list)

forecast_comparison_df <- do.call(rbind, lapply(forecast_comparison_list, function(x) {
  data.frame(Model = x$model,
             RMSE  = round(x$RMSE, 2),
             MAE   = round(x$MAE, 2),
             MAPE  = round(x$MAPE, 4),
             MASE  = round(x$MASE, 4),
             stringsAsFactors = FALSE)
}))
forecast_comparison_df <- forecast_comparison_df[order(forecast_comparison_df$RMSE), ]
rownames(forecast_comparison_df) <- NULL

cat("--- Table IV: Out-of-Sample Forecast Accuracy Across GARCH-Family Models ---\n")
print(forecast_comparison_df)

best_oos_model <- forecast_comparison_df$Model[1]
cat(sprintf("\n  ==> Best out-of-sample model (lowest RMSE) : %s\n", best_oos_model))
cat(sprintf("  ==> Best in-sample model (lowest AIC)      : %s\n", best_model_name))
cat(sprintf("  %s\n\n",
            ifelse(best_oos_model == best_model_name,
                   "AIC-based model selection is CONFIRMED by out-of-sample forecast accuracy -- strengthens the justification for the EGARCH(1,1)-Std-t choice.",
                   "NOTE: the AIC-best model and the RMSE-best out-of-sample model differ. Report and discuss this trade-off explicitly in Section III-F/J rather than silently keeping the AIC choice.")))

write.csv(forecast_comparison_df, "TableIV_Forecast_Comparison_AllModels.csv",
          row.names = FALSE)
cat("  [Saved] TableIV_Forecast_Comparison_AllModels.csv\n\n")

# ------------------------------------------------------------------------------
# 4.4  Figure S2 -- Bar Chart: Forecast Accuracy Comparison Across Models
# ------------------------------------------------------------------------------
comp_long <- data.frame(
  Model  = rep(forecast_comparison_df$Model, 3),
  Metric = rep(c("RMSE (IDR)", "MAE (IDR)", "MAPE (%)"),
               each = nrow(forecast_comparison_df)),
  Value  = c(forecast_comparison_df$RMSE,
             forecast_comparison_df$MAE,
             forecast_comparison_df$MAPE)
)
comp_long$Metric <- factor(comp_long$Metric,
                           levels = c("RMSE (IDR)", "MAE (IDR)", "MAPE (%)"))

p_figS2 <- ggplot(comp_long, aes(x = Model, y = Value, fill = Model)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  facet_wrap(~ Metric, scales = "free_y", nrow = 1) +
  labs(title = "Figure S2: Out-of-Sample Forecast Accuracy Across GARCH-Family Models",
       x = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(axis.text.x       = element_text(angle = 35, hjust = 1),
        plot.title        = element_text(face = "bold", size = 12),
        strip.background  = element_rect(fill = "gray90"))

ggsave("FigureS2_Model_Comparison_Bar.png", plot = p_figS2,
       width = 11, height = 4.5, dpi = 300)
cat("  [Saved] FigureS2_Model_Comparison_Bar.png\n\n")


# ==============================================================================
# PHASE 5: FORECAST ACCURACY EVALUATION
# ==============================================================================

cat("================================================================\n")
cat("  PHASE 5: Forecast Accuracy -- Error Metrics\n")
cat("================================================================\n\n")

errors      <- test_price - fore_price
abs_err     <- abs(errors)
pct_err     <- abs_err / test_price * 100

# Naive benchmark (persistence: P_{t-1} as forecast of P_t) --
# MAE_naive was already computed in Phase 1.2b; reused here unchanged.

RMSE <- sqrt(mean(errors^2))
MAE  <- mean(abs_err)
MAPE <- mean(pct_err)
MASE <- MAE / MAE_naive

cat("--- Out-of-Sample Error Metrics ---\n")
cat(sprintf("  Forecast horizon : %d business days\n\n", n_test))
cat(sprintf("  RMSE  (Root Mean Square Error)         = %10.4f  IDR\n", RMSE))
cat(sprintf("  MAE   (Mean Absolute Error)            = %10.4f  IDR\n", MAE))
cat(sprintf("  MAPE  (Mean Absolute Percentage Error) = %10.4f  %%\n",  MAPE))
cat(sprintf("  MASE  (Mean Absolute Scaled Error)     = %10.4f\n\n",    MASE))
cat(sprintf("  Interpretation:\n"))
cat(sprintf("    MAPE of %.2f%% means the forecast deviates by %.2f%% on average.\n",
            MAPE, MAPE))
cat(sprintf("    MASE = %.4f  (%s)\n\n",
            MASE,
            ifelse(MASE < 1,
                   "Model OUTPERFORMS the naive random-walk benchmark.",
                   "Model does NOT outperform the naive benchmark.")))

cat("--- Forecast Error Distribution Statistics ---\n")
print(round(c(
  Mean_Error   = mean(errors),
  SD_Error     = sd(errors),
  Min_Error    = min(errors),
  Max_Error    = max(errors),
  Median_Error = median(errors)
), 4))
cat("\n")

# ------------------------------------------------------------------------------
# 5.1  Figure 13 – Daily Forecast Error Bar Chart
# ------------------------------------------------------------------------------
error_df <- data.frame(
  Date      = test_data$Tanggal,
  Error_IDR = errors
)

p_fig13 <- ggplot(error_df, aes(x = Date, y = Error_IDR,
                                fill = ifelse(Error_IDR >= 0,
                                              "Over-forecast",
                                              "Under-forecast"))) +
  geom_bar(stat = "identity", width = 1) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  scale_fill_manual(values = c("Over-forecast"  = FORE_COLOR,
                               "Under-forecast" = TRAIN_COLOR),
                    name   = "Forecast Bias") +
  scale_y_continuous(labels = comma) +
  labs(title    = "Figure 13: Daily Forecast Error (Actual minus Forecasted Price)",
       subtitle = sprintf("Testing Set  |  RMSE = %.2f IDR  |  MAPE = %.4f%%",
                          RMSE, MAPE),
       x        = "Date",
       y        = "Forecast Error (IDR)") +
  theme_bw(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        plot.subtitle   = element_text(color = "gray40"),
        legend.position = "top")

ggsave("Figure13_Forecast_Error.png", plot = p_fig13,
       width = 10, height = 4.5, dpi = 300)
cat("  [Saved] Figure13_Forecast_Error.png\n\n")


# ==============================================================================
# PHASE 6: VALIDATION PLOTS
# ==============================================================================

cat("================================================================\n")
cat("  PHASE 6: Validation Plots\n")
cat("================================================================\n\n")

# -- Assemble plot data frames --
train_plot_df <- data.frame(
  Date   = train_data$Tanggal,
  Price  = train_price,
  Series = "Actual - Training Set"
)
test_actual_df <- data.frame(
  Date   = test_data$Tanggal,
  Price  = test_price,
  Series = "Actual - Testing Set"
)
bridge_df <- data.frame(             # visual continuity anchor
  Date   = SPLIT_DATE,
  Price  = last_train_price,
  Series = "Forecasted - Testing Set"
)
test_fore_df <- data.frame(
  Date   = test_data$Tanggal,
  Price  = fore_price,
  Series = "Forecasted - Testing Set"
)

full_plot_df <- rbind(train_plot_df, test_actual_df, bridge_df, test_fore_df)
full_plot_df$Series <- factor(full_plot_df$Series,
                              levels = c("Actual - Training Set",
                                         "Actual - Testing Set",
                                         "Forecasted - Testing Set"))

series_colors    <- c("Actual - Training Set"    = TRAIN_COLOR,
                      "Actual - Testing Set"     = TEST_COLOR,
                      "Forecasted - Testing Set" = FORE_COLOR)
series_linetypes <- c("Actual - Training Set"    = "solid",
                      "Actual - Testing Set"     = "solid",
                      "Forecasted - Testing Set" = "dashed")
series_sizes     <- c("Actual - Training Set"    = 0.45,
                      "Actual - Testing Set"     = 0.55,
                      "Forecasted - Testing Set" = 0.75)

# ------------------------------------------------------------------------------
# 6.1  Figure 14 – Full-Period Validation Plot (THE MAIN HERO PLOT)
# ------------------------------------------------------------------------------
p_fig14 <- ggplot(full_plot_df,
                  aes(x = Date, y = Price,
                      color     = Series,
                      linetype  = Series,
                      linewidth = Series)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(SPLIT_DATE),
             linetype   = "dotted", color = "gray30", linewidth = 0.6) +
  annotate("text",
           x     = SPLIT_DATE - 30,
           y     = max(raw_data$Kurs_Penutupan_USD_IDR) * 0.975,
           label = "<- Training  |  Testing ->",
           hjust = 1, size = 3.0, color = "gray30") +
  annotate("label",
           x     = min(test_data$Tanggal) +
             as.numeric(diff(range(test_data$Tanggal))) * 0.55,
           y     = min(raw_data$Kurs_Penutupan_USD_IDR) * 1.015,
           label = sprintf("RMSE  = %.2f IDR\nMAPE = %.4f%%\nMASE  = %.4f",
                           RMSE, MAPE, MASE),
           size = 3.2, hjust = 0.5,
           fill = "white", color = "gray20", label.size = 0.3) +
  scale_color_manual(values     = series_colors,    name = NULL) +
  scale_linetype_manual(values  = series_linetypes, name = NULL) +
  scale_linewidth_manual(values = series_sizes,     name = NULL) +
  scale_y_continuous(labels     = comma) +
  scale_x_date(date_breaks      = "6 months",
               date_labels      = "%b\n%Y") +
  labs(
    title    = "Figure 14: USD/IDR Exchange Rate -- Actual vs. Forecasted Price Levels",
    subtitle = sprintf(
      "ARIMA(%d,%d,%d) + %s  |  Forecast Horizon: %d business days (Apr 2025 - Apr 2026)",
      best_arima_train$arma[1],
      best_arima_train$arma[6],
      best_arima_train$arma[2],
      best_model_name,
      n_test),
    x       = "Date",
    y       = "Exchange Rate (IDR per 1 USD)",
    caption = "Blue = Training Actual  |  Orange = Testing Actual  |  Red Dashed = GARCH Forecast"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "gray30", size = 10),
    plot.caption     = element_text(color = "gray50", size = 9, hjust = 0),
    legend.position  = "top",
    legend.key.width = unit(2, "cm"),
    panel.grid.minor = element_blank()
  )

ggsave("Figure14_Validation_Actual_vs_Forecast.png", plot = p_fig14,
       width = 12, height = 5.5, dpi = 300)
cat("  [Saved] Figure14_Validation_Actual_vs_Forecast.png\n")

# ------------------------------------------------------------------------------
# 6.2  Figure 15 – Zoomed Testing Period (with deviation ribbon)
# ------------------------------------------------------------------------------
zoom_df <- rbind(test_actual_df, bridge_df, test_fore_df)
zoom_df$Series <- factor(zoom_df$Series,
                         levels = c("Actual - Testing Set",
                                    "Forecasted - Testing Set"))

zoom_colors    <- c("Actual - Testing Set"     = TEST_COLOR,
                    "Forecasted - Testing Set" = FORE_COLOR)
zoom_linetypes <- c("Actual - Testing Set"     = "solid",
                    "Forecasted - Testing Set" = "dashed")
zoom_sizes     <- c("Actual - Testing Set"     = 0.55,
                    "Forecasted - Testing Set" = 0.75)

ribbon_df <- data.frame(
  Date = test_data$Tanggal,
  ymin = pmin(test_price, fore_price),
  ymax = pmax(test_price, fore_price)
)

p_fig15 <- ggplot(zoom_df,
                  aes(x = Date, y = Price,
                      color     = Series,
                      linetype  = Series,
                      linewidth = Series)) +
  geom_ribbon(data        = ribbon_df,
              aes(x = Date, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE,
              alpha       = 0.15,
              fill        = "gray50") +
  geom_line() +
  scale_color_manual(values     = zoom_colors,    name = NULL) +
  scale_linetype_manual(values  = zoom_linetypes, name = NULL) +
  scale_linewidth_manual(values = zoom_sizes,     name = NULL) +
  scale_y_continuous(labels     = comma) +
  scale_x_date(date_breaks      = "2 months", date_labels = "%b\n%Y") +
  labs(
    title    = "Figure 15: Zoomed View -- Testing Set (Actual vs. Forecasted Price)",
    subtitle = sprintf("RMSE = %.2f IDR  |  MAPE = %.4f%%  |  MASE = %.4f",
                       RMSE, MAPE, MASE),
    x        = "Date",
    y        = "Exchange Rate (IDR per 1 USD)",
    caption  = "Grey ribbon = absolute deviation between actual and forecast"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "gray30"),
    plot.caption     = element_text(color = "gray50", size = 9, hjust = 0),
    legend.position  = "top",
    legend.key.width = unit(2, "cm"),
    panel.grid.minor = element_blank()
  )

ggsave("Figure15_Zoom_TestingSet_Forecast.png", plot = p_fig15,
       width = 10, height = 5, dpi = 300)
cat("  [Saved] Figure15_Zoom_TestingSet_Forecast.png\n")

# ------------------------------------------------------------------------------
# 6.3  Figure 16 – Scatter Plot: Actual vs. Forecasted Price
# ------------------------------------------------------------------------------
fit_df    <- data.frame(Actual = test_price, Forecasted = fore_price)
lm_fit    <- lm(Actual ~ Forecasted, data = fit_df)
r_squared <- summary(lm_fit)$r.squared

p_fig16 <- ggplot(fit_df, aes(x = Forecasted, y = Actual)) +
  geom_point(color = TEST_COLOR, alpha = 0.5, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE,
              color = FORE_COLOR, fill = "pink", linewidth = 0.8) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", color = "gray40", linewidth = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  annotate("text",
           x     = min(fore_price) + diff(range(fore_price)) * 0.05,
           y     = max(test_price) * 0.995,
           label = sprintf("R-squared = %.4f", r_squared),
           hjust = 0, size = 4, color = "gray20") +
  labs(title    = "Figure 16: Scatter Plot -- Actual vs. Forecasted Price (Testing Set)",
       subtitle = "Dashed 45-degree line = perfect forecast  |  Red line = OLS fit",
       x        = "Forecasted Price (IDR)",
       y        = "Actual Price (IDR)") +
  theme_bw(base_size = 12) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"))

ggsave("Figure16_Scatter_Actual_vs_Forecast.png", plot = p_fig16,
       width = 7, height = 6, dpi = 300)
cat("  [Saved] Figure16_Scatter_Actual_vs_Forecast.png\n\n")


# ==============================================================================
# PHASE 7: CONSOLIDATED RESULTS SUMMARY
# ==============================================================================

cat("================================================================\n")
cat("  CONSOLIDATED HYPOTHESIS TEST RESULTS\n")
cat("================================================================\n")

test_summary <- data.frame(
  Test = c(
    "ADF Test (Train Log Return)",
    "PP Test  (Train Log Return)",
    "Ljung-Box   (ARIMA Residuals, lag=20)",
    "Jarque-Bera (ARIMA Residuals)",
    "ARCH-LM     (ARIMA Residuals, lag=12)",
    "Ljung-Box   (GARCH Std. Residuals, lag=20)",
    "Ljung-Box   (GARCH Sq. Std. Residuals, lag=20)",
    "ARCH-LM     (GARCH Std. Residuals, lag=12)",
    "Jarque-Bera (GARCH Std. Residuals)"
  ),
  H0 = c(
    "Unit root (non-stationary)",
    "Unit root (non-stationary)",
    "No autocorrelation in residuals",
    "Residuals are normally distributed",
    "No ARCH effects",
    "No autocorrelation in std. residuals",
    "No autocorrelation in sq. std. residuals",
    "No ARCH effects in std. residuals",
    "Std. residuals are normally distributed"
  ),
  p_value = round(c(
    adf_train$p.value,
    pp_train$p.value,
    lb_arima$p.value,
    jb_arima$p.value,
    arch_lm_train$p.value,
    lb_std$p.value,
    lb_std2$p.value,
    arch_std$p.value,
    jb_std$p.value
  ), 6),
  Decision = c(
    ifelse(adf_train$p.value      < 0.05, "Reject H0",      "Fail to Reject"),
    ifelse(pp_train$p.value       < 0.05, "Reject H0",      "Fail to Reject"),
    ifelse(lb_arima$p.value       > 0.05, "Fail to Reject", "Reject H0"),
    ifelse(jb_arima$p.value       > 0.05, "Fail to Reject", "Reject H0"),
    ifelse(arch_lm_train$p.value  < 0.05, "Reject H0",      "Fail to Reject"),
    ifelse(lb_std$p.value         > 0.05, "Fail to Reject", "Reject H0"),
    ifelse(lb_std2$p.value        > 0.05, "Fail to Reject", "Reject H0"),
    ifelse(arch_std$p.value       > 0.05, "Fail to Reject", "Reject H0"),
    ifelse(jb_std$p.value         > 0.05, "Fail to Reject", "Reject H0")
  ),
  stringsAsFactors = FALSE
)
print(test_summary, row.names = FALSE)

cat("\n================================================================\n")
cat("  STRUCTURAL BREAK TEST SUMMARY  [REVISION: reviewer Pt.5]\n")
cat("================================================================\n")
cat("  -- Mean-level tests (log return) --\n")
cat(sprintf("  Bai-Perron breakpoint(s) detected : %s\n",
            ifelse(all(is.na(bp_dates)),
                   "None",
                   paste(format(bp_dates, "%d %b %Y"), collapse = "; "))))
cat(sprintf("  Chow test at split (%s) : F = %.4f, p = %.4e  -->  %s\n\n",
            format(SPLIT_DATE, "%d %b %Y"),
            chow_test$statistic, chow_test$p.value,
            ifelse(chow_test$p.value < 0.05,
                   "Structural break confirmed",
                   "No break confirmed at this exact point")))
cat("  -- Volatility-level tests (squared log return) --\n")
cat(sprintf("  Bai-Perron breakpoint(s) detected : %s\n",
            ifelse(all(is.na(bp_vol_dates)),
                   "None",
                   paste(format(bp_vol_dates, "%d %b %Y"), collapse = "; "))))
cat(sprintf("  Chow test at split (%s) : F = %.4f, p = %.4e  -->  %s\n\n",
            format(SPLIT_DATE, "%d %b %Y"),
            chow_test_vol$statistic, chow_test_vol$p.value,
            ifelse(chow_test_vol$p.value < 0.05,
                   "Volatility regime shift confirmed",
                   "No volatility shift confirmed at this exact point")))

cat("================================================================\n")
cat("  MULTI-MODEL FORECAST COMPARISON SUMMARY  [REVISION: reviewer Pt.3]\n")
cat("================================================================\n")
print(forecast_comparison_df, row.names = FALSE)
cat(sprintf("\n  Best out-of-sample (lowest RMSE) : %s\n", best_oos_model))
cat(sprintf("  Best in-sample (lowest AIC)      : %s\n\n", best_model_name))

cat("================================================================\n")
cat("  FORECAST ACCURACY SUMMARY (Selected Model)\n")
cat("================================================================\n")
cat(sprintf("  Model                          : ARIMA(%d,%d,%d) + %s\n",
            best_arima_train$arma[1],
            best_arima_train$arma[6],
            best_arima_train$arma[2],
            best_model_name))
cat(sprintf("  Variance Persistence (a1 + b1) : %.6f\n", persist))
cat(sprintf("  Training observations          : %d\n",   n_train))
cat(sprintf("  Testing observations (horizon) : %d\n",   n_test))
cat(sprintf("  RMSE                           : %.4f  IDR\n", RMSE))
cat(sprintf("  MAE                            : %.4f  IDR\n", MAE))
cat(sprintf("  MAPE                           : %.4f  %%\n",  MAPE))
cat(sprintf("  MASE                           : %.4f\n",      MASE))
cat(sprintf("  Scatter R-squared              : %.4f\n",      r_squared))

cat("\n================================================================\n")
cat("  FIGURES SAVED (16 main-text + 2 supplementary)\n")
cat("================================================================\n")
cat("  Figure 1  : Figure1_Closing_Price_Split.png\n")
cat("  Figure 2  : Figure2_Log_Return_Split.png\n")
cat("  Figure S1 : FigureS1_Structural_Breaks_LogReturn.png   [NEW - Pt.5]\n")
cat("  FigS1b    : FigureS1b_Structural_Breaks_Volatility.png [NEW - Pt.5]\n")
cat("  Figure 3  : Figure3_ACF_Log_Return.png\n")
cat("  Figure 4  : Figure4_PACF_Log_Return.png\n")
cat("  Figure 5  : Figure5_ARIMA_Residuals.png\n")
cat("  Figure 6  : Figure6_ACF_ARIMA_Residuals.png\n")
cat("  Figure 7  : Figure7_ACF_Squared_ARIMA_Residuals.png\n")
cat("  Figure 8  : Figure8_Standardized_Residuals.png\n")
cat("  Figure 9  : Figure9_ACF_Std_Residuals.png\n")
cat("  Figure 10 : Figure10_ACF_Squared_Std_Residuals.png\n")
cat("  Figure 11 : Figure11_QQ_Std_Residuals.png\n")
cat("  Figure 12 : Figure12_Conditional_Volatility_Train.png\n")
cat("  Figure S2 : FigureS2_Model_Comparison_Bar.png          [NEW - Pt.3]\n")
cat("  Figure 13 : Figure13_Forecast_Error.png\n")
cat("  Figure 14 : Figure14_Validation_Actual_vs_Forecast.png\n")
cat("  Figure 15 : Figure15_Zoom_TestingSet_Forecast.png\n")
cat("  Figure 16 : Figure16_Scatter_Actual_vs_Forecast.png\n")
cat("----------------------------------------------------------------\n")
cat("  Table IV  : TableIV_Forecast_Comparison_AllModels.csv  [NEW - Pt.3]\n")
cat("================================================================\n")
cat("  SCRIPT COMPLETED SUCCESSFULLY\n")
cat("================================================================\n")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
