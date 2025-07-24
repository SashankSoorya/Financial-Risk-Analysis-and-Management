# Load necessary libraries

library(readxl)
library(forecast)
library(tseries)
library(rugarch)
library(ggplot2)

# Read the data
dataset <- read_excel("D:\\Sashank\\AsssignmentRFiles\\Monthly.xlsx")

# Ensure the Date column is in Date format
dataset$Date <- as.Date(dataset$Date, format="%Y-%m-%d")

# Define stock columns and market return
stock_columns <- colnames(dataset)[2:(ncol(dataset) - 1)]  # Skip the Date column and risk-free rate column
market_column <- "^NSEI"  # Assuming '^NSEI' is the market return column
risk_free_column <- "Monthly Yield %"  # Assuming this is the risk-free rate column

# Extract risk-free rate from the dataset
risk_free_rate <- dataset[[risk_free_column]]  # The last column assumed to be risk-free rate

# Remove risk-free rate column before analysis
dataset_clean <- dataset[, -which(colnames(dataset) == risk_free_column)]

# Create a list to store CAPM models
capm_models <- list()

# Loop through each stock column to calculate excess returns and fit CAPM model
for (stock in stock_columns) {
  # Extract the stock returns
  stock_returns <- dataset[[stock]]
  
  # Extract market returns (assuming the market returns are in ^NSEI column)
  market_returns <- dataset[[market_column]]
  
  # Excess stock returns (subtract the risk-free rate from stock returns)
  excess_stock_returns <- stock_returns - risk_free_rate
  
  # Excess market returns (subtract the risk-free rate from market returns)
  excess_market_returns <- market_returns - risk_free_rate
  
  # CAPM model (regression of excess stock returns on excess market returns)
  capm_model <- lm(excess_stock_returns ~ excess_market_returns)
  capm_models[[stock]] <- capm_model
  
  # Print CAPM model summary for the stock
  cat("CAPM model for", stock, ":\n")
  print(summary(capm_model))
  cat("\n")
}

# Now fit ARIMA, GARCH, and EGARCH models for each stock
# Create lists to store models
arima_models <- list()
garch_models <- list()
egarch_models <- list()
arima_predictions <- list()
garch_predictions <- list()

# Loop for ARIMA Model Fitting
for (stock in colnames(dataset_clean)[2:(ncol(dataset_clean) - 1)]) {  # Skip the Date column
  # Get stock data (returns)
  stock_data <- dataset_clean[[stock]]
  
  # Check for missing data before fitting ARIMA
  if (any(is.na(stock_data))) {
    cat("Missing data found for", stock, ". Skipping ARIMA model fitting.\n")
    next  # Skip if missing data is found
  }
  
  # Augmented Dickey-Fuller (ADF) test for stationarity
  adf_test <- adf.test(stock_data, alternative = "stationary")
  cat("ADF Test for", stock, ":\n")
  print(adf_test)
  cat("\n")
  
  if (adf_test$p.value > 0.05) {
    cat("Stock data for", stock, "is not stationary. Applying differencing.\n")
    stock_data <- diff(stock_data)  # Apply differencing if data is not stationary
  }
  
  # Fit ARIMA model
  arima_model <- auto.arima(stock_data)
  
  # Standardized residuals from ARIMA
  arima_residuals <- residuals(arima_model)
  arima_standardized_residuals <- arima_residuals / sd(arima_residuals)
  
  # Print ARIMA model summary
  cat("ARIMA model for", stock, ":\n")
  print(summary(arima_model))
  cat("\n")
  
  # Forecast with ARIMA model
  arima_forecast <- forecast(arima_model, h = 10)  # Adjust 'h' for desired prediction horizon
  arima_predictions[[stock]] <- arima_forecast
  
  # Print ARIMA forecast results
  cat("Forecast for ARIMA model for", stock, ":\n")
  print(arima_forecast)
  
  # Plot ARIMA predictions
  plot(arima_forecast, main = paste("ARIMA Forecast for", stock), xlab = "Date", ylab = "Returns")
  
  # Plot Standardized Residuals for ARIMA
  #arima_residuals_plot <- ggplot(data.frame(Date = dataset_clean$Date, Residuals = arima_standardized_residuals), aes(x = Date, y = Residuals)) +
  #  geom_line() + 
  #  ggtitle(paste("Standardized Residuals for", stock, "- ARIMA")) + 
  #  xlab("Date") + ylab("Standardized Residuals") + 
  #  theme_minimal()
  #print(arima_residuals_plot)  # Display the ARIMA residuals plot
  
  # ACF and PACF of ARIMA standardized residuals
  acf_vals <- acf(arima_standardized_residuals, plot = FALSE, lag.max = 10)  # ACF values (no plot)
  pacf_vals <- pacf(arima_standardized_residuals, plot = FALSE, lag.max = 10)  # PACF values (no plot)
  
  # ACF plot
  acf_plot <- ggplot(data.frame(Lag = 1:length(acf_vals$acf), ACF = acf_vals$acf), aes(x = Lag, y = ACF)) + 
    geom_segment(aes(xend = Lag, yend = 0)) + 
    geom_point() + 
    ggtitle(paste("ACF of Standardized Residuals for", stock, "- ARIMA")) + 
    xlab("Lags") + ylab("Autocorrelation") + 
    theme_minimal()
  print(acf_plot)  # Display ACF plot
  
  # PACF plot
  pacf_plot <- ggplot(data.frame(Lag = 1:length(pacf_vals$acf), PACF = pacf_vals$acf), aes(x = Lag, y = PACF)) + 
    geom_segment(aes(xend = Lag, yend = 0)) + 
    geom_point() + 
    ggtitle(paste("PACF of Standardized Residuals for", stock, "- ARIMA")) + 
    xlab("Lags") + ylab("Partial Autocorrelation") + 
    theme_minimal()
  print(pacf_plot)  # Display PACF plot
  
  # ACF and PACF for raw stock returns
  acf_stock_vals <- acf(stock_data, plot = FALSE, lag.max = 10)
  pacf_stock_vals <- pacf(stock_data, plot = FALSE, lag.max = 10)
  
  # ACF plot for raw stock returns
  acf_stock_plot <- ggplot(data.frame(Lag = 1:length(acf_stock_vals$acf), ACF = acf_stock_vals$acf), aes(x = Lag, y = ACF)) + 
    geom_segment(aes(xend = Lag, yend = 0)) + 
    geom_point() + 
    ggtitle(paste("ACF of", stock, "Returns")) + 
    xlab("Lags") + ylab("Autocorrelation") + 
    theme_minimal()
  print(acf_stock_plot)  # Display ACF plot for raw stock returns
  
  # PACF plot for raw stock returns
  pacf_stock_plot <- ggplot(data.frame(Lag = 1:length(pacf_stock_vals$acf), PACF = pacf_stock_vals$acf), aes(x = Lag, y = PACF)) + 
    geom_segment(aes(xend = Lag, yend = 0)) + 
    geom_point() + 
    ggtitle(paste("PACF of", stock, "Returns")) + 
    xlab("Lags") + ylab("Partial Autocorrelation") + 
    theme_minimal()
  print(pacf_stock_plot)  # Display PACF plot for raw stock returns
  
  # Ljung-Box test on standardized residuals
  lb_test <- Box.test(arima_standardized_residuals, lag = 20, type = "Ljung-Box")
  lb_pvalues <- lb_test$p.value
  
  # Plot Ljung-Box test p-values
  lb_plot <- ggplot(data.frame(Lag = 1:20, PValue = lb_pvalues), aes(x = Lag, y = PValue)) + 
    geom_line() + 
    geom_point() + 
    ggtitle(paste("Ljung-Box Test p-values for", stock, "- ARIMA")) + 
    xlab("Lags") + ylab("p-value") + 
    theme_minimal()
  print(lb_plot)  # Display Ljung-Box plot
}
garch_predictions <- list()
egarch_predictions <- list()

# Loop for GARCH and EGARCH Model Fitting
n=4 #Change the values of n=2,3,4,5 to get for different stocks
for (stock in colnames(dataset_clean)[n]) {  # Skip the Date column
  # Get stock data (returns)
  stock_data <- dataset_clean[[stock]]
  
  # Check for missing data before fitting GARCH/EGARCH
  if (any(is.na(stock_data))) {
    cat("Missing data found for", stock, ". Skipping GARCH/EGARCH model fitting.\n")
    next  # Skip if missing data is found
  }
  
  # Specify and fit GARCH model with default specifications
  garch_spec <- ugarchspec()  # Default GARCH specification
  cat("GARCH Model Specification (Default):\n")
  print(garch_spec)
  
  garch_model <- tryCatch(
    ugarchfit(spec = garch_spec, data = stock_data),
    error = function(e) {
      cat("Error fitting GARCH model for", stock, ":", e$message, "\n")
      NULL
    }
  )
  
  if (!is.null(garch_model)) {
    cat("\nGARCH Model Fit for", stock, ":\n")
    show(garch_model)
    
    # Forecast with GARCH model
    garch_forecast <- tryCatch(
      ugarchforecast(garch_model, n.ahead = 10),
      error = function(e) {
        cat("Error in GARCH forecast for", stock, ":", e$message, "\n")
        NULL
      }
    )
    
    if (!is.null(garch_forecast)) {
      garch_predictions[[stock]] <- garch_forecast
      # Forecasted mean value (instead of volatility)
      forecasted_mean <- fitted(garch_forecast)
      
      # Print GARCH forecasted mean values
      cat("GARCH Forecasted Mean for", stock, ":\n")
      print(forecasted_mean)
      
      plot(forecasted_mean, type = "l",
           main = paste("GARCH Forecasted Mean for", stock),
           xlab = "Time", ylab = "Predicted Mean Value")
    }
  }
  
  # Specify and fit EGARCH model with default specifications
  egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH"))  # EGARCH specification
  cat("\nEGARCH Model Specification (Default):\n")
  print(egarch_spec)
  
  egarch_model <- tryCatch(
    ugarchfit(spec = egarch_spec, data = stock_data),
    error = function(e) {
      cat("Error fitting EGARCH model for", stock, ":", e$message, "\n")
      NULL
    }
  )
  
  if (!is.null(egarch_model)) {
    cat("\nEGARCH Model Fit for", stock, ":\n")
    show(egarch_model)
    
    # Forecast with EGARCH model
    egarch_forecast <- tryCatch(
      ugarchforecast(egarch_model, n.ahead = 10),
      error = function(e) {
        cat("Error in EGARCH forecast for", stock, ":", e$message, "\n")
        NULL
      }
    )
    
    if (!is.null(egarch_forecast)) {
      egarch_predictions[[stock]] <- egarch_forecast
      # Forecasted mean value (instead of volatility)
      forecasted_mean_egarch <- fitted(egarch_forecast)
      
      # Print EGARCH forecasted mean values
      cat("EGARCH Forecasted Mean for", stock, ":\n")
      print(forecasted_mean_egarch)
      
      plot(forecasted_mean_egarch, type = "l",
           main = paste("EGARCH Forecasted Mean for", stock),
           xlab = "Time", ylab = "Predicted Mean Value")
    }
  }
}
