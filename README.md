# USD/IDR Exchange Rate Volatility Forecasting

ARIMA(4,0,1)-EGARCH(1,1) model with Student-t innovations, applied to 1,318 daily 
USD/IDR observations, forecasting a 277-day out-of-sample horizon.

## Results
- MAPE: 3.98%
- RMSE: 713.11 IDR

## Method
Model selected after testing alternative asset and frequency specifications. 
Full methodology write-up is under review for a Sinta-indexed journal; 
this repository shares the R implementation only.