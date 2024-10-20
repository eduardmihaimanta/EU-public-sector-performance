
# Bridging gaps in EU public sector performance assessment: An analytical approach
# based on PLS perspective

# Install the necesarry libraries
install.packages('pls')
install.packages('tidyverse')
install.packages('readxl')
install.packages('caret')
install.packages('DescTools')
install.packages('writexl')
install.packages('summarytools')
install.packages('vtable')
install.packages('tseries')
install.packages('NLP')
install.packages('factoextra')

# Load the libraries
library(pls)
library(tidyverse)
library(readxl)
library(caret)
library(DescTools)
library(writexl)
library(readxl)
library(moments)
library(tseries)
library(NLP)
library(factoextra)

# Compute the descriptive statistics
contor = 26
stop <-FALSE
array_columns <- strsplit(LETTERS, " ")
letters_alp <- strsplit(LETTERS, " ")
df <- data.frame("", "", "", "", "", "", "", "", "", "")

for(i in letters_alp) {
  for(j in letters_alp){
    S <- paste(i, j, sep ="")
    array_columns <- append(array_columns, S)
    contor <- contor +1;
    if(contor == 81){
      stop <- TRUE
      break
    }
  }
  if(stop){
    break
  }
}

for (i in array_columns){
  data <- read_excel("C:/Users/40726/Desktop/Analytics/Cercetare/European Public Sector Performance/data.xlsx", range = cell_cols(i))
  series <- ts(data)
  jb <- jarque.bera.test(series)
  df[nrow(df) + 1,] = c(mean(series), median(series), max(series), min(series),
                        sd(series), skewness(as.vector(series)), kurtosis(as.vector(series)),
                        jb[["statistic"]][["X-squared"]], jb["p.value"], length(series))
}

write_xlsx(df, 'C:/Users/40726/Desktop/Analytics/Cercetare/European Public Sector Performance/descr_sta.xlsx')

# Import the dataset
data <- read_excel("C:/Users/40726/Desktop/Analytics/European Public Sector Performance/data.xlsx")

# Loess regression
#define k-fold cross validation method
ctrl <- trainControl(method = "cv", number = 5)
grid <- expand.grid(span = seq(0, 1, len = 11), degree = 1)

# GDP per capita transformation to log
df <- data %>%
  arrange(desc(`GDP per capita`)) %>%
  mutate(log_GDP = log(`GDP per capita`))

############## Political stability ##############
# Inspect GDP and Political stability
ggplot(df, aes(x = log_GDP,
               y = `Political stability and absence of violence`))+
  geom_point()+
  xlab('log of GDP per capita') + 
  geom_smooth(method = loess, se = FALSE, span=0.1)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Political stability and absence of violence` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.29 R-sq = 0.39 MAE = 0.22
loess50 <- loess(`Political stability and absence of violence` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Political stability and absence of violence`))
value <- df$`Political stability and absence of violence` - min + 1
expected_value <- smoothed50 - min +1 
gap <- (value-expected_value)-1
sign <- sign(df$`Political stability and absence of violence`)
gap_political_stability <- gap * sign



############## Control of corruption ##############
# Inspect
ggplot(df, aes(x = log_GDP,
               y = `Control of corruption`))+
  geom_point() +
  xlab('log of GDP per capita') + 
  geom_smooth(method = loess, se = FALSE,span=0.1)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Control of corruption` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.35 R-sq = 0.80 MAE = 0.27
loess50 <- loess(`Control of corruption` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Control of corruption`))
value <- df$`Control of corruption` - min + 1
expected_value <- smoothed50 - min +1 
gap <- (value-expected_value)-1
sign <- sign(df$`Control of corruption`)
gap_control_of_corruption <- gap * sign

############## Government effectiveness ##############
# Inspect GDP and Gov eff
ggplot(df, aes(x = log_GDP,
               y = `Government effectiveness`))+
  geom_point() +
  xlab('log of GDP per capita') + 
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Government effectiveness` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.28 R-sq = 0.76 MAE = 0.20
loess50 <- loess(`Government effectiveness` ~ log_GDP, 
                 data=df, 
                 span=0.10) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Government effectiveness`))
value <- df$`Government effectiveness` - min + 1
expected_value <- smoothed50 - min +1 
gap <- (value-expected_value)-1
sign <- sign(df$`Government effectiveness`)
gap_government_effectiveness <- gap * sign

############## Regulatory quality ##############
# Inspect GDP and regulatory quality
ggplot(df, aes(x = log_GDP,
               y = `Regulatory quality`))+
  geom_point() +
  xlab('log of GDP per capita') + 
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Regulatory quality` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.25 R-sq = 0.66 MAE = 0.20
loess50 <- loess(`Regulatory quality` ~ log_GDP, 
                 data=df, 
                 span=0.10) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Regulatory quality`))
value <- df$`Regulatory quality` - min + 1
expected_value <- smoothed50 - min +1 
gap <- (value-expected_value)-1
sign <- sign(df$`Regulatory quality`)
gap_regulatory_quality <- gap * sign

############## Rule of law ##############
# Inspect GDP and rule of law
ggplot(df, aes(x = log_GDP,
               y = `Rule of law`))+
  geom_point() +
  xlab('log of GDP per capita') + 
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Rule of law` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.28 R-sq = 0.78 MAE = 0.21
loess50 <- loess(`Rule of law` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Rule of law`))
value <- df$`Rule of law` - min + 1
expected_value <- smoothed50 - min +1 
gap <- (value-expected_value)-1
sign <- sign(df$`Rule of law`)
gap_rule_of_law <- gap * sign

############## `Voice and Accountability`##############
# Inspect GDP  and voice and accountability
ggplot(df, aes(x = log_GDP,
               y = `Voice and Accoutability`))+
  geom_point() +
  xlab('log of GDP per capita') +
  ylab('`Voice and Accountability`')+
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Voice and Accoutability` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.14 R-sq = 0.81 MAE = 0.11
loess50 <- loess(`Voice and Accoutability` ~ log_GDP, 
                 data=df, 
                 span=0.10) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Voice and Accoutability`))
value <- df$`Voice and Accoutability` - min + 1
expected_value <- smoothed50 - min +1 
gap <- (value-expected_value)-1
sign <- sign(df$`Voice and Accoutability`)
gap_voice_and_accountability <- gap * sign

############## `Individuals using the internet for interaction with public authorities`##############
# Inspect GDP 
ggplot(df, aes(x = log_GDP,
               y = `Individuals using the internet for interaction with public authorities`))+
  geom_point() +
  xlab('log of GDP per capita') +
  ylab('`Individuals using the internet for interaction with public authorities`')+
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Individuals using the internet for interaction with public authorities` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.14 R-sq = 0.81 MAE = 0.11
loess50 <- loess(`Individuals using the internet for interaction with public authorities` ~ log_GDP, 
                 data=df, 
                 span=0.10) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Individuals using the internet for interaction with public authorities`))
value <- df$`Individuals using the internet for interaction with public authorities` 
expected_value <- smoothed50  
gap <- (value-expected_value)-1
sign <- sign(df$`Individuals using the internet for interaction with public authorities`)
gap_individuals_using_internet_for_interaction_public_auth <- gap * sign

############## `DESI`##############
# Inspect GDP and DESI index
ggplot(df, aes(x = log_GDP,
               y = `Digital Economy and Society Index, by Aggregate score`))+
  geom_point() +
  xlab('log of GDP per capita') +
  ylab('`Digital Economy and Society Index`')+
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Digital Economy and Society Index, by Aggregate score` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.14 R-sq = 0.81 MAE = 0.11
loess50 <- loess(`Digital Economy and Society Index, by Aggregate score` ~ log_GDP, 
                 data=df, 
                 span=0.10) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Digital Economy and Society Index, by Aggregate score`))
value <- df$`Digital Economy and Society Index, by Aggregate score` 
expected_value <- smoothed50  
gap <- (value-expected_value)-1
sign <- sign(df$`Digital Economy and Society Index, by Aggregate score`)
gap_DESI <- gap * sign

############## `EGOV`##############
# Inspect GDP and EGOV index
ggplot(df, aes(x = log_GDP,
               y = `E-Government Development Index`))+
  geom_point() +
  xlab('log of GDP per capita') +
  ylab('`E-Government Development Index`')+
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`E-Government Development Index` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.14 R-sq = 0.81 MAE = 0.11
loess50 <- loess(`E-Government Development Index` ~ log_GDP, 
                 data=df, 
                 span=0.10) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`E-Government Development Index`))
value <- df$`E-Government Development Index` - min + 1
expected_value <- smoothed50 - min +1 
gap <- (value-expected_value)-1
sign <- sign(df$`E-Government Development Index`)
gap_EGOV <- gap * sign

############## `Internet access in schools` ##############
ggplot(df, aes(x = log_GDP,
               y = `Internet access in schools`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Internet access in schools` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.65 R-sq = 31.23% MAE = 0.53
loess50 <- loess(`Internet access in schools` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Internet access in schools`))
value <- df$`Internet access in schools`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Internet access in schools`)
gap_internet_in_schools <- gap * sign

############## HDI ##############
# Inspect GDP and HDI
ggplot(df, aes(x = log_GDP,
               y = `Human Development Index`))+
  geom_point() +
  xlab('log of GDP per capita') + 
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Human Development Index` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.01 R-sq = 79.82 MAE = 0.01
loess50 <- loess(`Human Development Index` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Human Development Index`))
value <- df$`Human Development Index` - min + 1
expected_value <- smoothed50 - min +1 
gap <- (value-expected_value)-1
sign <- sign(df$`Human Development Index`)
gap_hdi<- gap * sign

############## `Expected Years of School` ##############
ggplot(df, aes(x = log_GDP,
               y = `Expected Years of School`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Expected Years of School` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 1.05 R-sq = 36.22% MAE = 0.88
loess50 <- loess(`Expected Years of School` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Expected Years of School`))
value <- df$`Expected Years of School`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Expected Years of School`)
gap_expected_years_of_school <- gap * sign


############## ``Mean Years of Schooling ##############
ggplot(df, aes(x = log_GDP,
               y = `Mean Years of Schooling\r\n`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Mean Years of Schooling\r\n` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 1.05 R-sq = 16.19% MAE = 0.79
loess50 <- loess(`Mean Years of Schooling\r\n` ~ log_GDP, 
                 data=df, 
                 span=0.10) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Mean Years of Schooling\r\n`))
value <- df$`Mean Years of Schooling\r\n`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Mean Years of Schooling\r\n`)
gap_mean_years_schooling <- gap * sign


############## `Quality of Life` ##############
ggplot(df, aes(x = log_GDP,
               y = `Quality of Life`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Quality of Life` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 1.05 R-sq = 16.19% MAE = 0.79
loess50 <- loess(`Quality of Life` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Quality of Life`))
value <- df$`Quality of Life`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Quality of Life`)
gap_quality_of_life <- gap * sign

############## `Better Life Index` ##############
ggplot(df, aes(x = log_GDP,
               y = `Better Life Index`))+
  geom_point() +
  xlab('log of GDP per capita') + 
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Better Life Index` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 1.95 R-sq = 6.66 MAE = 1.40
loess50 <- loess(`Better Life Index` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Better Life Index`))
value <- df$`Better Life Index` - min + 1
expected_value <- smoothed50 - min +1 
gap <- (value-expected_value)-1
sign <- sign(df$`Better Life Index`)
gap_better_life <- gap * sign


############## `The Legatum Prosperity Index` ##############
ggplot(df, aes(x = log_GDP,
               y = `The Legatum Prosperity Index`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`The Legatum Prosperity Index` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 1.05 R-sq = 16.19% MAE = 0.79
loess50 <- loess(`The Legatum Prosperity Index` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`The Legatum Prosperity Index`))
value <- df$`The Legatum Prosperity Index`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`The Legatum Prosperity Index`)
gap_legatum_propensity_index <- gap * sign


############## Early leavers ##############
# Inspect GDP 
ggplot(df, aes(x = log_GDP,
               y = `Early leavers from education and training`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Early leavers from education and training` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 4.85 R-sq = 0.21 MAE = 3.43
loess50 <- loess(`Early leavers from education and training` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Early leavers from education and training`))
value <- df$`Early leavers from education and training` 
expected_value <- smoothed50 
gap <- (min-expected_value)-1
sign <- sign(df$`Early leavers from education and training`)
gap_early_leavers <- gap * sign


############## `School enrolment tertiary` ##############
# Inspect GDP 
ggplot(df, aes(x = log_GDP,
               y = `School enrolment tertiary`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`School enrolment tertiary` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 15.92 R-sq = 0.26 MAE = 11.48
loess50 <- loess(`School enrolment tertiary` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`School enrolment tertiary`))
value <- df$`School enrolment tertiary` 
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`School enrolment tertiary`)
gap_school_enrolment_tertiary <- gap * sign

############## `Education Spending %GDP`` ##############
# Inspect GDP 
ggplot(df, aes(x = log_GDP,
               y = `Education Spending %GDP`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Education Spending %GDP` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.84 R-sq = 0.21 MAE = 0.69
loess50 <- loess(`Education Spending %GDP` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Education Spending %GDP`))
value <- df$`Education Spending %GDP`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Education Spending %GDP`)
gap_education_spending_gdp <- gap * sign

############## `Equity of access to health care services` ##############
# Inspect GDP 
ggplot(df, aes(x = log_GDP,
               y = `Equity of access to health care services`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Equity of access to health care services` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 2.91 R-sq = 0.30 MAE = 2.09
loess50 <- loess(`Equity of access to health care services` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Equity of access to health care services`))
value <- df$`Equity of access to health care services` - min + 1
expected_value <- smoothed50 - min +1 
gap <- (value-expected_value)-1
sign <- sign(df$`Equity of access to health care services`)
gap_equity_access_healthcare_serv <- gap * sign

############## `Self-percieved health` ##############
# Inspect GDP 
ggplot(df, aes(x = log_GDP,
               y = `Self-percieved health`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Self-percieved health` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 9.92 R-sq = 0.22 MAE = 7.53
loess50 <- loess(`Self-percieved health` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Self-percieved health`))
value <- df$`Self-percieved health`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Self-percieved health`)
gap_self_percieved_health <- gap * sign

############## `Life Expectancy` ##############
# Inspect GDP 
ggplot(df, aes(x = log_GDP,
               y = `Life expectancy at birth total population`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Life expectancy at birth total population` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 1.43 R-sq = 76.8 MAE = 1.13
loess50 <- loess(`Life expectancy at birth total population` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Life expectancy at birth total population`))
value <- df$`Life expectancy at birth total population`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Life expectancy at birth total population`)
gap_life_expect <- gap * sign

############## `Fertility rate` ##############
# Inspect GDP 
ggplot(df, aes(x = log_GDP,
               y = `Total fertility rate`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(`Total fertility rate` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.16 R-sq = 36.1 MAE = 0.13
loess50 <- loess(`Total fertility rate` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Total fertility rate`))
value <- df$`Total fertility rate`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Total fertility rate`)
gap_fertility_rate <- gap * sign

############## `Infant mortality` ##############
ggplot(df, aes(x = log_GDP,
               y = `Infant mortality`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Infant mortality` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 1.08 R-sq = 54.43 MAE = 0.78
loess50 <- loess(`Infant mortality` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Infant mortality`))
value <- df$`Infant mortality`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Infant mortality`)
gap_infant_mortality <- gap * sign

############## `Current health expenditure per capita` ##############
ggplot(df, aes(x = log_GDP,
               y = `Current health expenditure per capita`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Current health expenditure per capita` ~ log_GDP,
               data = df,
               method = "gamLoess",
               tuneGrid=grid,
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 372 R-sq = 96.33 MAE = 268.71
loess50 <- loess(`Current health expenditure per capita` ~ log_GDP,
                 data=df,
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50)
min <- min(Winsorize(df$`Current health expenditure per capita`))
value <- df$`Current health expenditure per capita`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Current health expenditure per capita`)
gap_health_spending_per_capita <- gap * sign


############## `Quality of Roads` ##############
ggplot(df, aes(x = log_GDP,
               y = `Quality of Roads`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Quality of Roads` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 80 R-sq = 50.42 MAE = 0.63
loess50 <- loess(`Quality of Roads` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 

min <- min(Winsorize(df$`Quality of Roads`))
value <- df$`Quality of Roads`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Quality of Roads`)
gap_quality_of_roads <- gap * sign


############## `Quality of railoard` ##############
ggplot(df, aes(x = log_GDP,
               y = `Quality of railroad infrastructure`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Quality of railroad infrastructure` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.80 R-sq = 44.82 MAE = 0.63
loess50 <- loess(`Quality of railroad infrastructure` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Quality of railroad infrastructure`))
value <- df$`Quality of railroad infrastructure`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Quality of railroad infrastructure`)
gap_quality_railroad_infrastructure <- gap * sign

############## `Quality of port` ##############
ggplot(df, aes(x = log_GDP,
               y = `Quality of port infrastructure`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Quality of port infrastructure` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.70 R-sq = 45.87 MAE = 0.55
loess50 <- loess(`Quality of port infrastructure` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Quality of port infrastructure`))
value <- df$`Quality of port infrastructure`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Quality of port infrastructure`)
gap_quality_port_infrastructure <- gap * sign

############## `Quality of air` ##############
ggplot(df, aes(x = log_GDP,
               y = `Quality of air transport infrastructure`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Quality of air transport infrastructure` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.53 R-sq = 59.66 MAE = 0.42
loess50 <- loess(`Quality of air transport infrastructure` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Quality of air transport infrastructure`))
value <- df$`Quality of air transport infrastructure`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Quality of air transport infrastructure`)
gap_quality_air_infrastructure <- gap * sign

############## Inflation ##############
ggplot(df, aes(x = log_GDP,
               y = Inflation))+
  geom_point() +
  xlab('log of GDP per capita') + 
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
#perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
model <- train(Inflation ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 1.95 R-sq = 6.66 MAE = 1.40
loess50 <- loess(Inflation ~ log_GDP, 
                 data=df, 
                 span=0.8) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$Inflation))
value <- df$Inflation - min + 1
expected_value <- smoothed50 - min +1 
gap <- (value-expected_value)-1
sign <- sign(df$Inflation)
gap_inflation <- gap * sign

############## `General government gross debt` ##############
ggplot(df, aes(x = log_GDP,
               y = `General government gross debt`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`General government gross debt` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 31.60 R-sq = 29.03 MAE = 23.16
loess50 <- loess(`General government gross debt` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`General government gross debt`))
value <- df$`General government gross debt`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`General government gross debt`)
gap_general_government_gross_debt <- gap * sign


############## `Public Expenditure` ##############
ggplot(df, aes(x = log_GDP,
               y = `Public Expenditure`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Public Expenditure` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 5.67 R-sq = 27.78 MAE = 4.50
loess50 <- loess(`Public Expenditure` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Public Expenditure`))
value <- df$`Public Expenditure`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Public Expenditure`)
gap_public_expenditure <- gap * sign

############## `Economic Freedom Summary Index` ##############
ggplot(df, aes(x = log_GDP,
               y = `Economic Freedom Summary Index`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Economic Freedom Summary Index` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 0.27 R-sq = 16.08 MAE = 0.20
loess50 <- loess(`Economic Freedom Summary Index` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Economic Freedom Summary Index`))
value <- df$`Economic Freedom Summary Index`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Economic Freedom Summary Index`)
gap_economic_freedom_index <- gap * sign


############## Unemployment ##############
ggplot(df, aes(x = log_GDP,
               y = Unemployment))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(Unemployment ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 4.20 R-sq = 9.97% MAE = 3.01
loess50 <- loess(Unemployment ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$Unemployment))
value <- df$Unemployment
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$Unemployment)
gap_unemployment <- gap * sign

############## `Gini index` ##############
ggplot(df, aes(x = log_GDP,
               y = `Gini index`))+
  geom_point() +
  xlab('log of GDP per capita') +
  geom_smooth(method = loess, se = FALSE)+
  theme_bw()
model <- train(`Gini index` ~ log_GDP, 
               data = df, 
               method = "gamLoess", 
               tuneGrid=grid, 
               trControl = ctrl)
print(model) # span = 0.5 RMSE = 3.15 R-sq = 25.57% MAE = 2.56
loess50 <- loess(`Gini index` ~ log_GDP, 
                 data=df, 
                 span=0.1) # 50% smoothing span
smoothed50 <- predict(loess50) 
min <- min(Winsorize(df$`Gini index`))
value <- df$`Gini index`
expected_value <- smoothed50
gap <- (value-expected_value)-1
sign <- sign(df$`Gini index`)
gap_gini <- gap * sign


# PLS and dataframe creation using the gap variables 
pls_data <- df %>%
  select(Country,Year,log_GDP) %>%
  cbind(gap_better_life,
        gap_control_of_corruption,
        gap_DESI,
        gap_early_leavers,
        gap_economic_freedom_index,
        gap_education_spending_gdp,
        gap_EGOV,
        gap_electricity_sources,
        gap_equity_access_healthcare_serv,
        gap_expected_years_of_school,
        gap_fertility_rate,
        gap_fixed_broadband_subscription,
        gap_fixed_telephone_subscription,
        gap_general_government_gross_debt,
        gap_gini,
        gap_government_effectiveness,
        gap_hdi,
        gap_health_spendings_as_gdp,
        gap_infant_mortality,
        gap_inflation,
        gap_internet_in_schools,
        gap_internet_users,
        gap_legatum_propensity_index,
        gap_life_expect,
        gap_mean_years_schooling,
        gap_political_stability,
        gap_public_expenditure,
        gap_quality_air_infrastructure,
        gap_quality_of_life,
        gap_quality_of_roads,
        gap_quality_port_infrastructure,
        gap_quality_railroad_infrastructure,
        gap_regulatory_quality,
        gap_rule_of_law,
        gap_school_enrolment_tertiary,
        gap_self_percieved_health,
        gap_unemployment,
        gap_voice_and_accountability)

# Export the intermediate dataset with gaps
write_xlsx(pls_data,"C:/Users/40726/Desktop/Analytics/European Public Sector Performance/pls_data.xlsx")

# Run the PLS model
model_pls <- plsr(log_GDP ~ 		gap_better_life+
                    gap_control_of_corruption+
                    gap_DESI+
                    gap_early_leavers+
                    gap_economic_freedom_index+
                    gap_education_spending_gdp+
                    gap_EGOV+
                    gap_electricity_sources+
                    gap_equity_access_healthcare_serv+
                    gap_expected_years_of_school+
                    gap_fertility_rate+
                    gap_fixed_broadband_subscription+
                    gap_fixed_telephone_subscription+
                    gap_general_government_gross_debt+
                    gap_gini+
                    gap_government_effectiveness+
                    gap_hdi+
                    gap_health_spendings_as_gdp+
                    gap_infant_mortality+
                    gap_inflation+
                    gap_internet_in_schools+
                    gap_internet_users+
                    gap_legatum_propensity_index+
                    gap_life_expect+
                    gap_mean_years_schooling+
                    gap_political_stability+
                    gap_public_expenditure+
                    gap_quality_air_infrastructure+
                    gap_quality_of_life+
                    gap_quality_of_roads+
                    gap_quality_port_infrastructure+
                    gap_quality_railroad_infrastructure+
                    gap_regulatory_quality+
                    gap_rule_of_law+
                    gap_school_enrolment_tertiary+
                    gap_self_percieved_health+
                    gap_unemployment+
                    gap_voice_and_accountability,
                  data = pls_data,
                  scale = TRUE,
                  ncomp = 3,
                  validation = "CV")
summary(model_pls)


# Find the number of dimensions with lowest cross validation error
cv <- RMSEP(model_pls)
best.dims <- which.min(cv$val[estimate = "adjCV", , ]) - 1
validationplot(model_pls)
validationplot(model_pls, val.type="MSEP")
validationplot(model_pls, intercept = FALSE)
coefficients <- coef(model_pls)
coefficients


# Governance pillar 
performance_governance_var <-(  (0.0434785188*df$`Political stability and absence of violence`) +
                                  (-0.1529280523*df$`Control of corruption`) +
                                  (-0.1142171037*df$`Government effectiveness`) + 
                                  (0.0254682012*df$`Regulatory quality`) + 
                                  (-0.129806680*df$`Rule of law`) + 
                                  (0.0347845977*df$`Voice and Accoutability`) +
                                  (-0.002492732*df$`Digital Economy and Society Index, by Aggregate score`)+
                                  (-0.0023946291*df$`E-Government Development Index`)+
                                  (0.0231700474 * df$`Internet access in schools`)  ) /
  (0.0434785188
   -0.1529280523
   -0.1142171037
   +0.0254682012
   -0.129806680
   +0.0347845977
   -0.002492732
   -0.0023946291
   +0.0231700474)
score_theme_inter <- df %>%
  select(Country, Year) %>%
  cbind(performance_governance_var)
score_governance <- score_theme_inter %>%
  group_by(Year) %>%
  mutate(Score =  (100 * (performance_governance_var - min(performance_governance_var))/
                     (max(performance_governance_var)-min(performance_governance_var)))
  )
write_csv(score_governance,"score_governance.csv")

# Economic pillar 
performance_economic <-((0.0000587949*df$`Electricity sources`) +
                            (-0.0175655883*df$`Fixed broadband subscription`) +
                            (-0.0114836596*df$`Fixed telephone subscription`) + 
                            (-0.0127699856*df$`Internet users`) + 
                            (-0.0025208802*df$`Quality of Roads`) + 
                            (-0.0021506618*df$`Quality of railroad infrastructure`) +
                            (0.0065976558*df$`Quality of port infrastructure`)+
                            (0.0018527922*df$`Quality of air transport infrastructure`)+
                            (-0.0697771848 * df$Inflation) +
                            (0.0051469372*df$`Public Expenditure`)+
                            (-0.0017210006*df$`Economic Freedom Summary Index`)+
                            (0.0025882833*df$Unemployment)+
                            (-0.0069510460*df$`Gini index`)) /
  (0.0000587949
   -0.0175655883
   -0.0114836596
   -0.0127699856
   -0.0025208802
   -0.0021506618
   +0.0065976558
   +0.0018527922
   -0.0697771848
   +0.0051469372
   -0.0017210006
   +0.0025882833
   -0.0069510460)
score_economic_inter <- df %>%
  select(Country, Year) %>%
  cbind(performance_economic)
score_economic <- score_economic_inter %>%
  group_by(Year) %>%
  mutate(Score =  (100 * (performance_economic - min(performance_economic))/
                     (max(performance_economic)-min(performance_economic)))
  )
write_csv(score_economic,"score_economic.csv")

# Social pillar 
performance_social <-(  
  (-0.0011298106*df$`Human Development Index`) +
    (-0.0049296672*df$`Expected Years of School`) +
    (0.0105387154*df$`Mean Years of Schooling\r\n`) +
    (0.0012448384*df$`Quality of Life`) +
    (0.0013068995*df$`Better Life Index`) +
    (0.0191716884*df$`The Legatum Prosperity Index`) +
    (0.0052254315*df$`Early leavers from education and training`)+
    (-0.0023416850*df$`School enrolment tertiary`)+
    (0.0169495959 * df$`Education Spending %GDP`) +
    (0.0259230268*df$`Equity of access to health care services`)+
    (-0.0037344768*df$`Self-percieved health`)+
    (-0.0125805879*df$`Life expectancy at birth total population`)+
    (0.0059083905*df$`Total fertility rate`)+
    (0.0039413824*df$`Infant mortality`)+
    (-0.0054324498*df$`Current health expenditure (% of GDP)`)) /
  (
    -0.0011298106
    -0.0049296672
    +0.0105387154
    +0.0012448384
    +0.0013068995
    +0.0191716884
    +0.0052254315
    -0.0023416850
    +0.0169495959
    +0.0259230268
    -0.0037344768
    -0.0125805879
    +0.0059083905
    +0.0039413824
    -0.0054324498
  )
score_social_inter <- df %>%
  select(Country, Year) %>%
  cbind(performance_social)
score_social <- score_social_inter %>%
  group_by(Year) %>%
  mutate(Score =  (100 * (performance_social - min(performance_social))/
                     (max(performance_social)-min(performance_social)))
  )
write.csv(score_social,"score_social.csv")

# Overall score
performance_overall <-(  (0.0434785188*df$`Political stability and absence of violence`) +
                           (-0.1529280523*df$`Control of corruption`) +
                           (-0.1142171037*df$`Government effectiveness`) + 
                           (0.0254682012*df$`Regulatory quality`) + 
                           (-0.129806680*df$`Rule of law`) + 
                           (0.0347845977*df$`Voice and Accoutability`) +
                           (-0.002492732*df$`Digital Economy and Society Index, by Aggregate score`)+
                           (-0.0023946291*df$`E-Government Development Index`)+
                           (0.0231700474 * df$`Internet access in schools`)+
                           (0.0000587949*df$`Electricity sources`) +
                           (-0.0175655883*df$`Fixed broadband subscription`) +
                           (-0.0114836596*df$`Fixed telephone subscription`) + 
                           (-0.0127699856*df$`Internet users`) + 
                           (-0.0025208802*df$`Quality of Roads`) + 
                           (-0.0021506618*df$`Quality of railroad infrastructure`) +
                           (0.0065976558*df$`Quality of port infrastructure`)+
                           (0.0018527922*df$`Quality of air transport infrastructure`)+
                           (-0.0697771848 * df$Inflation) +
                           (0.0051469372*df$`Public Expenditure`)+
                           (-0.0017210006*df$`Economic Freedom Summary Index`)+
                           (0.0025882833*df$Unemployment)+
                           (-0.0069510460*df$`Gini index`) +
                           (-0.0011298106*df$`Human Development Index`) +
                           (-0.0049296672*df$`Expected Years of School`) +
                           (0.0105387154*df$`Mean Years of Schooling\r\n`) +
                           (0.0012448384*df$`Quality of Life`) +
                           (0.0013068995*df$`Better Life Index`) +
                           (0.0191716884*df$`The Legatum Prosperity Index`) +
                           (0.0052254315*df$`Early leavers from education and training`)+
                           (-0.0023416850*df$`School enrolment tertiary`)+
                           (0.0169495959 * df$`Education Spending %GDP`) +
                           (0.0259230268*df$`Equity of access to health care services`)+
                           (-0.0037344768*df$`Self-percieved health`)+
                           (-0.0125805879*df$`Life expectancy at birth total population`)+
                           (0.0059083905*df$`Total fertility rate`)+
                           (0.0039413824*df$`Infant mortality`)+
                           (-0.0054324498*df$`Current health expenditure (% of GDP)`)) /
  (0.0434785188
   -0.1529280523
   -0.1142171037
   +0.0254682012
   -0.129806680
   +0.0347845977
   -0.002492732
   -0.0023946291
   +0.0231700474
   +0.0000587949
   -0.0175655883
   -0.0114836596
   -0.0127699856
   -0.0025208802
   -0.0021506618
   +0.0065976558
   +0.0018527922
   -0.0697771848
   +0.0051469372
   -0.0017210006
   +0.0025882833
   -0.0069510460
   -0.0011298106
   -0.0049296672
   +0.0105387154
   +0.0012448384
   +0.0013068995
   +0.0191716884
   +0.0052254315
   -0.0023416850
   +0.0169495959
   +0.0259230268
   -0.0037344768
   -0.0125805879
   +0.0059083905
   +0.0039413824
   -0.0054324498)
score_performance_overall_inter <- df %>%
  select(Country, Year) %>%
  cbind(performance_overall)
score_overall <- score_performance_overall_inter %>%
  group_by(Year) %>%
  mutate(Score =  100 - (100 * (performance_overall - min(performance_overall))/
                           (max(performance_overall)-min(performance_overall)))
  )
write.csv(score_overall,"scores.csv")


####### Main determinants plots ########

#### Overall scores #########
overall <- read_excel("C:/Users/40726/Desktop/Analytics/European Public Sector Performance/results/determinanti.xlsx", 
                      sheet = "overall")
# Round 2 digits
overall <- overall %>% mutate_if(is.numeric, round, digits=2)
# Arrange the determinant
overall_arranged <- overall %>%
  mutate(determinant_abs = abs(determinant)) %>%
  arrange(desc(determinant_abs))
# Create the plot
ggplot(overall_arranged, aes(x = reorder(variabila, determinant), y = determinant, label = determinant)) + 
  geom_point(stat = 'identity', fill = "black", size = 9) +
  geom_segment(aes(y = 0, 
                   x = reorder(variabila, determinant), 
                   yend = determinant, 
                   xend = reorder(variabila, determinant)), 
               color = "black") +
  geom_text(color = "white", size = 2, parse = TRUE) +
  coord_flip() +
  geom_hline(yintercept = 5, colour = 'red', linetype = 'dashed')+
  ylab('Determinants')+
  xlab('Variables')+
  theme_bw()


#### Governance scores #########
# Read the data
governance <- read_excel("C:/Users/40726/Desktop/Analytics/European Public Sector Performance/results/determinanti.xlsx", 
                         sheet = "governance")
# Round 2 digits
governance <- governance %>% mutate_if(is.numeric, round, digits=2)
# Arrange the determinant
governance_arranged <- governance %>%
  mutate(determinant_abs = abs(determinant)) %>%
  arrange(desc(determinant_abs))
# Plot
ggplot(governance, aes(x = reorder(variabila, determinant), y = determinant, label = determinant)) + 
  geom_point(stat = 'identity', fill = "black", size = 9) +
  geom_segment(aes(y = 0, 
                   x = reorder(variabila, determinant), 
                   yend = determinant, 
                   xend = reorder(variabila, determinant)), 
               color = "black") +
  geom_text(color = "white", size = 2, parse = TRUE) +
  coord_flip() +
  geom_hline(yintercept = 5.2, colour = 'red', linetype = 'dashed')+
  ylab('Determinants')+
  xlab('Variables')


##### Economic scores ####
# Read the data
economic <- read_excel("C:/Users/40726/Desktop/Analytics/European Public Sector Performance/results/determinanti.xlsx", 
                       sheet = "economic")
# Round 2 digits
economic <- economic %>% mutate_if(is.numeric, round, digits=2)
# Plot
ggplot(economic, aes(x = reorder(variabila, determinant), y = determinant, label = determinant)) + 
  geom_point(stat = 'identity', fill = "black", size = 9) +
  geom_segment(aes(y = 0, 
                   x = reorder(variabila, determinant), 
                   yend = determinant, 
                   xend = reorder(variabila, determinant)), 
               color = "black") +
  geom_text(color = "white", size = 2, parse = TRUE) +
  coord_flip() +
  geom_hline(yintercept = 5.4, colour = 'red', linetype = 'dashed')+
  ylab('Determinants')+
  xlab('Variables')


##### Social scores ####
# Read the data
social <- read_excel("C:/Users/40726/Desktop/Analytics/European Public Sector Performance/results/determinanti.xlsx", 
                     sheet = "social")
# Round 2 digits
social <- social %>% mutate_if(is.numeric, round, digits=2)
# Plot
ggplot(social, aes(x = reorder(variabila, determinant), y = determinant, label = determinant)) + 
  geom_point(stat = 'identity', fill = "black", size = 9) +
  geom_segment(aes(y = 0, 
                   x = reorder(variabila, determinant), 
                   yend = determinant, 
                   xend = reorder(variabila, determinant)), 
               color = "black") +
  geom_text(color = "white", size = 2, parse = TRUE) +
  coord_flip() +
  geom_hline(yintercept = 5.4, colour = 'red', linetype = 'dashed')+
  ylab('Determinants')+
  xlab('Variables')


########## Cluster analysis #########

# Data sets used for the cluster analysis
score_social
score_economic
score_governance
# Filter the data for 2021
Social_2021 <- score_social %>%
  rename(social = Score) %>%
  filter(Year == 2021)
Economic_2021 <- score_economic %>%
  rename(economic = Score) %>%
  filter(Year == 2021)
Governance_2021 <- score_governance %>%
  rename(governane = Score) %>%
  filter(Year == 2021)
# Join the datasets
cluster_data_2021 <- Social_2021 %>%
  left_join(Economic_2021, by=c('Country' = 'Country')) %>%
  left_join(Governance_2021, by=c('Country' = 'Country')) %>%
  select(Country,social,economic,governane) %>% 
  column_to_rownames(., var = 'Country')
# Scale the data
df <- scale(cluster_data_2021)
# Run k-means
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)
km.res <- kmeans(df, 4, nstart = 25)
print(km.res)
aggregate(df, by=list(cluster=km.res$cluster), mean)
dd <- cbind(df, cluster = km.res$cluster)
head(dd)
fviz_cluster(km.res, data = df) +
  theme_bw()
# Hierchical kmean
res.hk <-hkmeans(df, 4)
fviz_dend(res.hk, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

# Cluster analysis 2007
Social_2007 <- score_social %>%
  rename(social = Score) %>%
  filter(Year == 2007)
Economic_2007 <- score_economic %>%
  rename(economic = Score) %>%
  filter(Year == 2007)
Governance_2007 <- score_governance %>%
  rename(governane = Score) %>%
  filter(Year == 2007)
cluster_data_2007 <- Social_2007 %>%
  left_join(Economic_2007, by=c('Country' = 'Country')) %>%
  left_join(Governance_2007, by=c('Country' = 'Country')) %>%
  select(Country,social,economic,governane) %>% 
  column_to_rownames(., var = 'Country')
df <- scale(cluster_data_2007)
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)
km.res <- kmeans(df, 4, nstart = 25)
print(km.res)
aggregate(df, by=list(cluster=km.res$cluster), mean)
dd <- cbind(df, cluster = km.res$cluster)
head(dd)
fviz_cluster(km.res, data = df) +
  theme_bw()
# Hierchical kmean
res.hk <-hkmeans(df, 4)
fviz_dend(res.hk, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)