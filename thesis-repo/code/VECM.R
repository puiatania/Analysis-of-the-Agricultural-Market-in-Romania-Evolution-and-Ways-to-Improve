library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(stargazer)
#install.packages("Metrics")
library(Metrics)
library(readxl)
#install.packages("aTSA")
library(aTSA)
library(readr)
#install.packages("tsDyn")
library(tsDyn)
#install.packages("dynlm")
library(dynlm)

indici_de_pret <- read_excel("indici de pret.xlsx")
View(indici_de_pret)

# Declaram variabilele de tip ts
INCRT_RO <- ts(indici_de_pret$INCRT_RO, start = c(2015,1), frequency = 4)
INCRT_EU <-ts(indici_de_pret$INCRT_EU, start = c(2015,1), frequency = 4)
ININV_RO <-ts(indici_de_pret$ININV_RO, start = c(2015,1), frequency = 4)
ININV_EU <-ts(indici_de_pret$ININV_EU, start = c(2015,1), frequency = 4)

#INCRT_RO=indicele preturilor reale pentru bunurile si serviciile consumate in prezent in agricultura in RO
#INCRT_EU=indicele preturilor reale pentru bunurile si serviciile consumate in prezent in agricultura in EU
#ININV_RO=indicele preturilor reale pentru bunurile si serviciile pentru investitii in agricultura in RO
#ININV_EU=indicele preturilor reale pentru bunurile si serviciile pentru investitii in agricultura in EU

# Graficul seriilor
autoplot(cbind(INCRT_RO,INCRT_EU,ININV_RO,ININV_EU)) +
  ylab('') +
  ggtitle('Graficul seriei multivariata -indicii preturilor reale pentru Romania si UE27') +
  theme_bw()

autoplot(cbind(INCRT_RO,INCRT_EU)) +
  ylab('') +
  ggtitle('Graficul seriei multivariata -indicii preturilor reale pentru bunurile si serviciile consumate in prezent in agricultura') +
  theme_bw()

autoplot(cbind(ININV_RO,ININV_EU)) +
  ylab('') +
  ggtitle('Graficul seriei multivariata -indicii preturilor reale pentru bunurile si serviciile pentru investitiile in agricultura') +
  theme_bw()

autoplot(cbind(INCRT_RO,ININV_RO)) +
  ylab('') +
  ggtitle('Graficul seriei multivariata -indicii preturilor reale pentru Romania') +
  theme_bw()

#Corelograme
ggtsdisplay(INCRT_RO)#seria pare nestationara
ggtsdisplay(INCRT_EU)#seria pare nestationara
ggtsdisplay(ININV_RO)#seria pare nestationara
ggtsdisplay(ININV_EU)#seria pare nestationara

#Testarea stationaritatii seriilor
#INCRT_RO
#ADF
adf.variab <- ur.df(INCRT_RO, type = "none", selectlags = "AIC")
summary(adf.variab) # serie nestationara
adf.variab <- ur.df(INCRT_RO, type = "drift", selectlags = "AIC")
summary(adf.variab) # serie nestationara
adf.variab <- ur.df(INCRT_RO, type = "trend", selectlags = "AIC")
summary(adf.variab) # serie nestationara
#KPSS
INCRT_RO %>% ur.kpss() %>% summary() # t > critical values => nonstationary ts
# Philips-Perron
PP.test(INCRT_RO) # p > 0.1 => nonstationary ts
#ndiffs
ndiffs(INCRT_RO)#este nevoie de o diferentiere

#INCRT_EU
#ADF
adf.variab <- ur.df(INCRT_EU, type = "none", selectlags = "AIC")
summary(adf.variab) # serie nestationara
adf.variab <- ur.df(INCRT_EU, type = "drift", selectlags = "AIC")
summary(adf.variab) # serie nestationara
adf.variab <- ur.df(INCRT_EU, type = "trend", selectlags = "AIC")
summary(adf.variab) # serie nestationara
#KPSS
INCRT_EU %>% ur.kpss() %>% summary() # t > critical values => nonstationary ts
# Philips-PerEUn
PP.test(INCRT_EU) # p > 0.1 => nonstationary ts
#ndiffs
ndiffs(INCRT_EU)#este nevoie de o diferentiere

#ININV_RO
#ADF
adf.variab <- ur.df(ININV_RO, type = "none", selectlags = "AIC")
summary(adf.variab) # serie nestationara
adf.variab <- ur.df(ININV_RO, type = "drift", selectlags = "AIC")
summary(adf.variab) # serie nestationara
adf.variab <- ur.df(ININV_RO, type = "trend", selectlags = "AIC")
summary(adf.variab) # serie nestationara
#KPSS
ININV_RO %>% ur.kpss() %>% summary() # t > critical values => nonstationary ts
# Philips-PerEUn
PP.test(ININV_RO) # p > 0.1 => nonstationary ts
#ndiffs
ndiffs(ININV_RO)#este nevoie de o diferentiere

#ININV_EU
#ADF
adf.variab <- ur.df(ININV_EU, type = "none", selectlags = "AIC")
summary(adf.variab) # serie stationara
adf.variab <- ur.df(ININV_EU, type = "drift", selectlags = "AIC")
summary(adf.variab) # serie nestationara
adf.variab <- ur.df(ININV_EU, type = "trend", selectlags = "AIC")
summary(adf.variab) # serie nestationara
#KPSS
ININV_EU %>% ur.kpss() %>% summary() # t > critical values => nonstationary ts
# Philips-PerEUn
PP.test(ININV_EU) # p < 0.1 => stationary ts
#ndiffs
ndiffs(ININV_EU)#este nevoie de o diferentiere

#Corelograme seriilor diferentiate
ggtsdisplay(diff(INCRT_RO))#pare stationara
ggtsdisplay(diff(INCRT_EU))#pare stationara
ggtsdisplay(diff(ININV_RO))#pare stationara
ggtsdisplay(diff(ININV_EU))#pare stationara

#Testarea stationaritatii dupa prima diferentiere
#INCRT_RO
#ADF
adf.variab <- ur.df(diff(INCRT_RO), type = "none", selectlags = "AIC")
summary(adf.variab) # serie stationara
adf.variab <- ur.df(diff(INCRT_RO), type = "drift", selectlags = "AIC")
summary(adf.variab) # serie nestationara
adf.variab <- ur.df(diff(INCRT_RO), type = "trend", selectlags = "AIC")
summary(adf.variab) # serie nestationara
#KPSS
diff(INCRT_RO) %>% ur.kpss() %>% summary() # t < critical values => stationary ts
# Philips-Perron
PP.test(diff(INCRT_RO)) # p < 0.1 => stationary ts
#ndiffs
ndiffs(diff(INCRT_RO)) #nu mai este nevoie de nicio diferentiere

#INCRT_EU
#ADF
adf.variab <- ur.df(diff(INCRT_EU), type = "none", selectlags = "AIC")
summary(adf.variab) # serie stationara la 90%
adf.variab <- ur.df(diff(INCRT_EU), type = "drift", selectlags = "AIC")
summary(adf.variab) # serie nestationara
adf.variab <- ur.df(diff(INCRT_EU), type = "trend", selectlags = "AIC")
summary(adf.variab) # serie nestationara
#KPSS
diff(INCRT_EU) %>% ur.kpss() %>% summary() # t < critical values => stationary ts
# Philips-PerEUn
PP.test(diff(INCRT_EU)) # p > 0.1 => nonstationary ts
#ndiffs
ndiffs(diff(INCRT_EU))#nu mai este nevoie de nici o diferențiere

#ININV_RO
#ADF
adf.variab <- ur.df(diff(ININV_RO), type = "none", selectlags = "AIC")
summary(adf.variab) # serie stationara
adf.variab <- ur.df(diff(ININV_RO), type = "drift", selectlags = "AIC")
summary(adf.variab) # serie nestationara
adf.variab <- ur.df(diff(ININV_RO), type = "trend", selectlags = "AIC")
summary(adf.variab) # serie nestationara
#KPSS
diff(ININV_RO) %>% ur.kpss() %>% summary() # t < critical values => stationary ts
# Philips-PerEUn
PP.test(diff(ININV_RO)) # p < 0.1 => stationary ts
#ndiffs
ndiffs(diff(ININV_RO))#nu mai este nevoie de nici o diferentiere

#ININV_EU
#ADF
adf.variab <- ur.df(diff(ININV_EU), type = "none", selectlags = "AIC")
summary(adf.variab) # serie stationara
adf.variab <- ur.df(diff(ININV_EU), type = "drift", selectlags = "AIC")
summary(adf.variab) # serie stationara
adf.variab <- ur.df(diff(ININV_EU), type = "trend", selectlags = "AIC")
summary(adf.variab) # serie stationara
#KPSS
diff(ININV_EU) %>% ur.kpss() %>% summary() # t < critical values => stationary ts
# Philips-PerEUn
PP.test(diff(ININV_EU)) # p < 0.1 => stationary ts
#ndiffs
ndiffs(diff(ININV_EU))#nu mai este nevoie de nici o diferentiere

# Testul Engle-Granger de detectare a cointegrarii

# H0: Seriile nu sunt cointegrate
# H1: seriile sunt cointegrate
# Aplicam testul de cointegrare pe seriile reziduurilor

coint.test(y =INCRT_RO,X =ININV_RO,d = 1) # => seriile nu sunt cointegrate pe no trend, dar pe linear trend sunt
coint.test(y = INCRT_RO,X =INCRT_RO,d = 1) #  nu exista cointegrare pe no trend, dar pentru linear trend da
coint.test(y = ININV_RO,X = INCRT_RO,d = 1) # seriile nu e cointegrata pe no trend, dar e pe linear trend
coint.test(y = ININV_RO,X = ININV_RO,d = 1) # serii cointegrate atat pe no trend, cat si pe linear trend
coint.test(y = INCRT_RO,X = INCRT_EU,d = 1) # nu exista cointegrare pe no trend, dar exista pe linear trend
coint.test(y = INCRT_EU,X = INCRT_RO,d = 1) # serii cointegrate atat pe no trend, cat si pe linear trend
coint.test(y = INCRT_EU,X = INCRT_EU,d = 1)# nu exista cointegrare pe no trend, dar exista pe linear trend
coint.test(y = ININV_RO,X = ININV_EU,d = 1)# serii cointegrate atat pe no trend, cat si pe linear trend
coint.test(y = ININV_EU,X = ININV_RO,d = 1)# nu exista cointegrare pe no trend, dar exista pe linear trend
coint.test(y = INCRT_EU,X = ININV_EU,d = 1)# nu exista cointegrare pe no trend, dar exista pentru linear trend
coint.test(y = ININV_EU,X = INCRT_EU,d = 1)# nu exista cointegrare pe no trend, dar exista pentru linear trend
coint.test(y = ININV_EU,X = ININV_EU,d = 1)# nu exista cointegrare pe no trend, dar exista pentru linear trend

# Cointegrarea Johansen

# H0: nu exista cointegrare
# H1: exista cel putin o relatie de cointegrare

df <- cbind(INCRT_RO,ININV_RO,INCRT_EU,ININV_EU)
colnames(df) <- cbind('INCRT_RO','ININV_RO','INCRT_EU','ININV_EU')

# Selectarea lagului 
lagselect <- VARselect(df, lag.max = 12, type = 'const')
lagselect$selection 
# 6 laguri conform AIC si FPE
# Pentru a testa Johansen avem nevoie de laguri selectate - 1 => 6 - 1 = 5

# Testul Johansen - metoda Trace
ctest1 <- ca.jo(df, type = 'trace', ecdet = 'const',K=5)
summary(ctest1) 
#exista 2 relatii de cointegrare deoarece pentru r<=2 20.94<24.60

# Testul Johansen - metoda valorilor proprii maxime
ctest2 <- ca.jo(df, type = 'eigen', ecdet = 'const',K=5)
summary(ctest2)
#exista 2 relatii cointegrare deoarece pentru r<=2 13.40<15.67

#VECM
# Modelul VECM - metoda de estimare ML - atunci cand avem mai mult de o relatie de cointegrare
Model2 <- VECM(df,lag = 5, r=2, estim = ('ML'), LRinclude = 'const')
summary(Model2)
#Relatiile de cointegrare sunt date de r1 si r2
#Pentru prima relatie de cointegrare se observă că nu exista nicio relatie intre 
#indicele preturilor reale pentru consum RO (INCRT_RO) si indicele preturilor reale pentru investitii RO (ININV_RO)
#dar intre indicele preturilor reale pentru consume RO(INCRT_RO) si indicele preturilor reale pentru consum EU(INCRT_EU)
#exista o relatie negativa, iar intre INCRT_RO si ININV_EU exista o reltaie pozitiva
#ININV_RO exista o relatie pozitiva puternica pentru ININV_EU si o relatie negativa puternica pentru INCRT_EU
#negativa INCRT_EU

#Ecuatia INCRT_RO nu are relatie pe termen lung cu niciunul dintre indicii analizati, dar
#prezinta o relatie pe termen scurt cu ININV_EU decalat cu 4 laguri
#Ecuatia ININV_RO nu are relatie pe termen lung, dar prezinta o relatie pe termen scurt INIV_EU decalat
#cu 1 lag, pentru INCRT_EU decalat cu 2 laguri, ININV_EU decalat cu 4 laguri si cu 5 laguri
#Ecuatia INCRT_EU nu are relatie pe termen lung cu nici unul dintre indici, dar prezinta o relatie 
#pe termen scurt cu ININV_EU decalat cu 4 laguri
#Ecuatia ININV_EU are relatie pe termen lung, dar prezinta si relatii pe termen scurt pentru INCRT_RO
#decalat cu 1,2,3 si 4 laguri, pentru ININV_RO decalat cu 1,2,3,4 si 5 laguri, pentru INCRT_EU decalat
#cu 2 laguri si pentru ININV_EU decalat cu 2,4 si 5 laguri

# Diagnosticul pe reziduuri

# Trebuie sa transformam obiectul VECM in obiect VAR
Model2VAR <- vec2var(ctest1, r = 2)

# Autocorelare
Serial2 <- serial.test(Model2VAR, lags.pt = 6, type = 'PT.asymptotic')
Serial2 # avem autocorelare in reziduuri

# Heteroschedascitate
Arch2 <- vars::arch.test(Model2VAR, lags.multi = 1, multivariate.only = TRUE)
Arch2 #reziduuri heteroschedastice
Arch2 <- vars::arch.test(Model2VAR, lags.multi = 2, multivariate.only = TRUE)
Arch2 # reziduuri heteroschedastice
Arch2 <- vars::arch.test(Model2VAR, lags.multi = 3, multivariate.only = TRUE)
Arch2 # reziduuri heteroschedastice
Arch2 <- vars::arch.test(Model2VAR, lags.multi = 5, multivariate.only = TRUE)
Arch2 # reziduuri homoschedastice
Arch2 <- vars::arch.test(Model2VAR, lags.multi = 10, multivariate.only = TRUE)
Arch2 # reziduuri homoschedastice
Arch2 <- vars::arch.test(Model2VAR, lags.multi = 15, multivariate.only = TRUE)
Arch2 # reziduuri homoschedastice

# Normalitate
Norm2 <- normality.test(Model2VAR, multivariate.only = TRUE)
Norm2 # reziduurile sunt normal distribuite

# Cauzalitate Granger
modelVar <- VAR(df, p = 5, type = 'const', season = NULL, exog = NULL)
Granger <- causality(modelVar, cause = 'INCRT_RO')
Granger #avem cauzalitate
Granger <- causality(modelVar, cause = 'ININV_RO')
Granger #nu avem cauzalitate
Granger <- causality(modelVar, cause = 'INCRT_EU')
Granger #avem cauzalitate
Granger <- causality(modelVar, cause = 'ININV_EU')
Granger #avem cauzalitate

# Functia de raspuns la impuls
ININV_ROirf <- irf(Model2VAR, impulse = 'INCRT_RO', response = 'ININV_RO', n.ahead= 10, boot = TRUE)
plot(ININV_ROirf, ylab = 'ININV_RO', main = 'INCRT_RO shock to ININV_RO')
#atunci cand intreducem un soc in INCRT_RO, ININV_RO tinda sa scada, dupa isi revine, dupa iar scade
# fiind in scadere pe 10 perioade
INCRT_EUirf <- irf(Model2VAR, impulse = 'INCRT_RO', response = 'INCRT_EU', n.ahead= 10, boot = TRUE)
plot(INCRT_EUirf, ylab = 'INCRT_EU', main = 'INCRT_RO shock to INCRT_EU')
#atunci cand introducem un soc in INCRT_RO, INCRT_EU scade pe 10 perioade
ININV_EUirf <- irf(Model2VAR, impulse = 'INCRT_RO', response = 'ININV_EU', n.ahead= 10, boot = TRUE)
plot(ININV_EUirf, ylab = 'ININV_EU', main = 'INCRT_RO shock to ININV_EU')
#atunci cand introducem un soc in INCRT_RO, ININV_EU scade
# INCRT_EU shock to INCRT_RO
INCRT_ROirf_from_INCRT_EU <- irf(Model2VAR, impulse = 'INCRT_EU', response = 'INCRT_RO', n.ahead= 10, boot = TRUE)
plot(INCRT_ROirf_from_INCRT_EU, ylab = 'INCRT_RO', main = 'INCRT_EU shock to INCRT_RO')
#atunci cand introducem un soc in INCRT_EU, INCRT_RO va creste pana la perioada 3, dupa va scadea mult pana 
#la perioada 5, dupa care va creste si isi va reveni pentru perioada 7, va scadea si va creste iar 
#pentru perioadele 8,9 si dupa 10 perioade va fi in scadere
# INCRT_EU shock to ININV_RO
ININV_ROirf_from_INCRT_EU <- irf(Model2VAR, impulse = 'INCRT_EU', response = 'ININV_RO', n.ahead= 10, boot = TRUE)
plot(ININV_ROirf_from_INCRT_EU, ylab = 'ININV_RO', main = 'INCRT_EU shock to ININV_RO')
#la inceputul perioadei, ININV_RO este in crestere, iar in restul perioadelor prezinta 
#un trend descendent
# INCRT_EU shock to ININV_EU
ININV_EUirf_from_INCRT_EU <- irf(Model2VAR, impulse = 'INCRT_EU', response = 'ININV_EU', n.ahead= 10, boot = TRUE)
plot(ININV_EUirf_from_INCRT_EU, ylab = 'ININV_EU', main = 'INCRT_EU shock to ININV_EU')
#ININV_EU creste pana la perioada 6, dupa care scade pana la perioada 8, iar dupa creste
# ININV_EU shock to INCRT_RO
INCRT_ROirf_from_ININV_EU <- irf(Model2VAR, impulse = 'ININV_EU', response = 'INCRT_RO', n.ahead= 10, boot = TRUE)
plot(INCRT_ROirf_from_ININV_EU, ylab = 'INCRT_RO', main = 'ININV_EU shock to INCRT_RO')
#pana la 5 perioade, INCRT_RO creste, dupa care scade
# ININV_EU shock to ININV_RO
ININV_ROirf_from_ININV_EU <- irf(Model2VAR, impulse = 'ININV_EU', response = 'ININV_RO', n.ahead= 10, boot = TRUE)
plot(ININV_ROirf_from_ININV_EU, ylab = 'ININV_RO', main = 'ININV_EU shock to ININV_RO')
#ININV_RO creste pentru 5 perioade, dupa care scade
# ININV_EU shock to INCRT_EU
INCRT_EUirf_from_ININV_EU <- irf(Model2VAR, impulse = 'ININV_EU', response = 'INCRT_EU', n.ahead= 10, boot = TRUE)
plot(INCRT_EUirf_from_ININV_EU, ylab = 'INCRT_EU', main = 'ININV_EU shock to INCRT_EU')
#la fel pentru INCRT_EU 

# Descompunerea variantei
FEVD1 <- fevd(Model2VAR,n.ahead=10)
FEVD1
plot(FEVD1)
#INCRT_RO va fi influentat dupa primele 4 perioade in proportie de 14.7% de ININV_RO, 15.98% de INCRT_EU
#si 5.03% de ININV_EU
#ININV_RO va fi influentat dupa 4 perioade de INCRT_RO in proportie de 11.27%
#INCRT_EU va fi influentat inca din prima perioada de 7.14% de ININV_RO si 10.08% de INCRT_RO
#ININV_EU va fi influentat inca din prima perioada de INCRT_RO in proportie de 34.28%, de ININV_RO
#in proportie de 16.54% si de INCRT_EU in proportie de 8.77%

# Prognoza 
forecast <- predict(Model2VAR, n.ahead = 4, ci = 0.99) # prognoza pe 4 trimestre

plot(forecast, name = 'INCRT_RO')
plot(forecast, name = 'ININV_RO')
plot(forecast, name = 'INCRT_EU')
plot(forecast, name = 'ININV_EU')

fanchart(forecast, name = 'INCRT_RO')
fanchart(forecast, name = 'ININV_RO')
fanchart(forecast, name = 'INCRT_EU')
fanchart(forecast, name = 'INCRT_EU')


