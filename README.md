# DIY-Google-Trends
This repository contains data and R codes used in Lolić, I., Matošec, M., & Sorić, P. (2024). DIY Google Trends indicators in social sciences: A methodological note. Technology in Society, 102477.

Data: _data_GTrends.csv_ - csv with keyword-based indicators, _cat.csv_ - first and second-order Google categories, _cat1.csv_ - first-order Google categories, _target.csv_ - target variable (retail trade)

R codes include steps for downloading, pre-processing, constructing and validating composite indicators (based on Google Trends data).

# 0 - Data download
This part includes downloading data with package gtrendsR. 

# 1 - Pre-adjustment steps
This code includes breakpoint adjustment, extracting common trend, filtering and differencing.

# 2 - Constructing composite indicators
This code includes constructing composite indicators with PCA, DFM, and XGBoost (default options; you should adjust this part).

# 3 - Indicator validation
This part includes various criteria used to validate constructed indicators.

