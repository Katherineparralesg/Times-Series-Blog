#Histograms
#Histogram of the variable unemployment female
attach(unemployment_female)
names(unemployment_female)
hist(unemployment_female$Number, main = "Histogram of the variable unemployment female", col = "lightsalmon", xlab = "Unemployment female", ylab = "Frequency")

#Histogram of the variable unemployment male
attach(unemployment_male)
names(unemployment_male)
hist(unemployment_male$Number, main = "Histogram of the variable unemployment male", col = "royalblue1", xlab = "Unemployment male", ylab = "Frequency")

#Time Series
#Time series unemployment female 
plot(unemployment_female, main = "Time Series of the unemployment female", col = "lightsalmon", ylab = "Unemployment female", xlab = "Year")

#Time series unemployment female (object)
unemployment_female_ts = ts(unemployment_female$Number, start = c(2013,1), frequency = 12)
print(unemployment_female_ts)
plot(unemployment_female_ts, main = "Time Series of the unemployment female", col = "lightsalmon",  ylab = "Unemployment female", xlab = "Year")

#Time series unemployment male 
plot(unemployment_male, main = "Time Series of the unemployment male", col = "royalblue1", ylab = "Unemployment male", xlab = "Year")

#Time series unemployment male (object)
unemployment_male_ts = ts(unemployment_male$Number, start = c(2013,1), frequency = 12)
print(unemployment_male_ts)
plot(unemployment_male_ts, main = "Time Series of the unemployment male", col = "royalblue1",  ylab = "Unemployment male", xlab = "Year")

#Decomposition of additive time series
#Decomposition of additive time series (Unemployment Female)
unemployment_female_desc = decompose(unemployment_female_ts)
plot(unemployment_female_desc, xlab = "Year",  col = "lightsalmon")

#Decomposition of additive time series (Unemployment Male)
unemployment_male_desc = decompose(unemployment_male_ts)
plot(unemployment_male_desc, xlab = "Year", col = "royalblue1")


#Transformation Of the serie
#Stabilization of variance
unemployment_female_stab = log(unemployment_female_ts)
unemployment_male_stab = log(unemployment_male_ts)
plot(unemployment_female_stab, xlab = "Year", ylab = "Unemployment Female", col = "lightsalmon", main = "Stabilization of variance")
plot(unemployment_male_stab, xlab = "Year", ylab = "Unemployment Male", col = "royalblue1", main = "Stabilization of variance")

#Trend elimination
unemployment_female_trendelim = diff(unemployment_female_stab)
unemployment_male_trendelim = diff(unemployment_male_stab)
plot(unemployment_female_trendelim, xlab = "Year", ylab = "Unemployment Female", col = "lightsalmon", main = "Trend elimination")
plot(unemployment_male_trendelim, xlab = "Year", ylab = "Unemployment Male", col = "royalblue1", main = "Trend elimination")

# Elimination of seasonality
unemployment_female_seasonelim = diff(unemployment_female_trendelim, lag = 12)
unemployment_male_seasonelim = diff(unemployment_male_trendelim, lag = 12)
plot(unemployment_female_seasonelim,  xlab = "Year", ylab = "Unemployment Female", col = "lightsalmon", main = "Elimination of seasonality")
plot(unemployment_male_seasonelim, xlab = "Year", ylab = "Unemployment Male", col = "royalblue1", main = "Elimination of seasonality")

#Dickey-Fuller Test
library(tseries)
#adf.test(unemployment_female_ts)
#adf.test(unemployment_male_ts)

#Autocorrelation function estimation
Acf(unemployment_female_ts, plot = TRUE, main = "ACF - Unemployment Female")
Acf(unemployment_male_ts, plot = TRUE, main = "ACF - Unemployment Male")

#Partial Autocorrelation function estimation
Pacf(unemployment_female_ts, plot = TRUE, main = "PACF - Unemployment Female" )
Pacf(unemployment_male_ts, plot = TRUE, main = "PACF - Unemployment Male" )

#Cross-Correlation Function Estimation
Ccf(unemployment_female_ts,unemployment_male_ts, plot = TRUE, main = "CCF - Unemployment Female & Unemployment Male")
