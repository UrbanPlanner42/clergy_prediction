#Clergy Projections Predictions 

#Clear Env. in R ----
    rm(list=ls())

#Load libraries ----
    library(dplyr)
    library(readr)
    library(lubridate)
    library(party)
    library(forecast)
    library(plotly)
    library(forecast)
    library(tseries)
    library(MASS)
    library(mFilter)

#Load Data

    ac <- read.csv("DATA NOT OPEN",
               na.strings=c("","NA"))

#Clergy Population Selection ----

    ac < - ac[!is.na(ac$ord_date), ]
    ac <- ac[!is.na(ac$date_of_birth),]
    ac <- ac[ac$cler_in_book == "Yes", ] #Clergy Book as survival 
    ac <- ac[ac$perm_deacon == "N", ] #Without Deacon
    
#Creating a Data Frame with the ordain age of each clergy ----

    #Simplify the All Clergy file
    date_df <- ac %>% 
      dplyr::select(client_number, 
                    age, 
                    date_of_birth,
                    ord_date, 
                    us_region, 
                    geo_diocese, 
                    gender, 
                    geo_province, 
                    place_of_birth)
    
    #Deal with Date data
    date_df$date_of_birth    <- as.Date(ac$date_of_birth, "%Y-%m-%d")
    date_df$year_birth     <- year(date_df$date_of_birth)
    date_df$ord_date     <-  as.Date(ac$ord_date, "%Y-%m-%d")
    
    #Figure out the Age of Ordanation
    date_df$days_ordenation <- as.numeric(date_df$ord_date - date_df$date_of_birth)
    date_df$age_ordenation <- (date_df$days_ordenation/365.25)
    date_df$year_ordanation <- year(date_df$ord_date)
    
    #Test
    hist(date_df$age_ordenation, breaks = 50)

# Figure Out in Which Decade the Clergy was born and Create New Data Frame ----
    
    Clergy25_34   <- date_df[date_df$age_ordenation <= 34, ]
    Clergy25_34$CensusGroup <- "25 to 34"
    Clergy35_54   <- date_df[date_df$age_ordenation <= 54 & date_df$age_ordenation > 34, ]
    Clergy35_54$CensusGroup <- "35 to 54"
    Clergy54_More <- date_df[date_df$age_ordenation >= 55, ]
    Clergy54_More$CensusGroup  <- "55 or Higher"
    Clergy_AgeDistribution <- rbind(Clergy25_34, Clergy35_54, Clergy54_More)
    rm(Clergy25_34, Clergy35_54, Clergy54_More, ac)
    
    Clergy_AgeDistribution <- Clergy_AgeDistribution[!is.na(Clergy_AgeDistribution$age_ordenation), ]
    
#Group by Year of Ordanation and Get Total ----
    
    Year_Ordanation <- Clergy_AgeDistribution %>%
                       group_by(Clergy_AgeDistribution$year_ordanation) %>%
                       summarise (Total = n())

#Plot Year of Ordanation ----
    
    p <- plot_ly(Year_Ordanation, 
                 x = Year_Ordanation$`Clergy_AgeDistribution$year_ordanation`, 
                 y = ~Total, 
                 name = 'Number of Ordain by Year', 
                 type = 'scatter', 
                 mode = 'lines')
    p
  
#Create Time Serie Data ----

    TimeSeries_YearOrdanation <- Year_Ordanation[Year_Ordanation$`Clergy_AgeDistribution$year_ordanation` > 1950 &
                                                   Year_Ordanation$`Clergy_AgeDistribution$year_ordanation` <= 2017, ] #Track only from 1950 to 2017
    
    TimeSeries_YearOrdanation <- ts(TimeSeries_YearOrdanation$Total, start = c(1951, 1), frequency = 1) #Track every year
    TimeSeries_YearOrdanation <- tsclean(TimeSeries_YearOrdanation) #Time series clean function
    plot(TimeSeries_YearOrdanation)
    
#Smooth the Time Serie to Avoid Noise ----
    scatter.smooth(TimeSeries_YearOrdanation)

#Augmented Dickey-Fuller Test ----
    adf.test(diff(log(TimeSeries_YearOrdanation)), alternative="stationary", k=0)
    
#Autocorrelations and Choosing Model Order ----
    fit <- auto.arima(log(TimeSeries_YearOrdanation))

#Predict the Clergy Number for 5 years ----
    pred <- predict(fit, n.ahead = 5)
    prediction_ts <- ts.plot(TimeSeries_YearOrdanation, 2.718^pred$pred, log = "y", lty = c(1,3)) 
    
#Get the Number for 5 years ----
    All_Clergy_Prediciton_5years <- 2.718^pred$pred
    
    
################################
    
#Prediction of Male Clergy ----
    male_df <- Clergy_AgeDistribution[Clergy_AgeDistribution$gender == "M", ]
    male_df <- male_df[!is.na(male_df$client_number), ]

#MALE Group by Year of Ordanation and Get Total ----
    
    Year_Ordanation_Male <- male_df %>%
                            group_by(male_df$year_ordanation) %>%
                            summarise (Total = n()) 
    
#MALE Plot Year of Ordanation ----
    
    p <- plot_ly(Year_Ordanation_Male, 
                 x = Year_Ordanation_Male$`male_df$year_ordanation`, 
                 y = ~Total, 
                 name = 'Number of Ordain by Year', 
                 type = 'scatter', 
                 mode = 'lines')
    p

#MALE Create Time Serie Data ----
    
    TimeSeries_YearOrdanation <- Year_Ordanation_Male[Year_Ordanation_Male$`male_df$year_ordanation` > 1950 &
                                                        Year_Ordanation_Male$`male_df$year_ordanation` <= 2017, ] #Track only from 1950 to 2017
    
    TimeSeries_YearOrdanation <- ts(TimeSeries_YearOrdanation$Total, start = c(1951, 1), frequency = 1) #Track every year
    TimeSeries_YearOrdanation <- tsclean(TimeSeries_YearOrdanation) #Time series clean function
    plot(TimeSeries_YearOrdanation)
    
#Smooth the Time Serie to Avoid Noise ----
    scatter.smooth(TimeSeries_YearOrdanation)
    
#Augmented Dickey-Fuller Test ----
    adf.test(diff(log(TimeSeries_YearOrdanation)), alternative="stationary", k=0)
    
#Autocorrelations and Choosing Model Order ----
    fit <- auto.arima(log(TimeSeries_YearOrdanation))
    
#Predict the Clergy Number for 5 years ----
    pred <- predict(fit, n.ahead = 5)
    prediction_ts <- ts.plot(TimeSeries_YearOrdanation, 2.718^pred$pred, log = "y", lty = c(1,3)) 
    
#Get the Number for 5 years ----
    Male_Prediciton_5years <- 2.718^pred$pred

###############################################

    
#Prediction of Female Clergy ----
    female_df <- Clergy_AgeDistribution[Clergy_AgeDistribution$gender == "F", ]
    female_df <- female_df[!is.na(female_df$client_number), ]
    
#Female Group by Year of Ordanation and Get Total ----
    
    Year_Ordanation_Female <- female_df %>%
      group_by(female_df$year_ordanation) %>%
      summarise (Total = n()) 
    
#FEMALE Plot Year of Ordanation ----
    
    p <- plot_ly(Year_Ordanation_Female, 
                 x = Year_Ordanation_Female$`female_df$year_ordanation`, 
                 y = ~Total, 
                 name = 'Number of Ordain by Year', 
                 type = 'scatter', 
                 mode = 'lines')
    p
    
#FEMALE Create Time Serie Data ----
    
TimeSeries_YearOrdanation <- Year_Ordanation_Male[Year_Ordanation_Female$`female_df$year_ordanation` > 1950 &
                                                    Year_Ordanation_Female$`female_df$year_ordanation` <= 2017, ] #Track only from 1950 to 2017
    
    TimeSeries_YearOrdanation <- ts(TimeSeries_YearOrdanation$Total, start = c(1964, 1), frequency = 1) #Track every year
    TimeSeries_YearOrdanation <- tsclean(TimeSeries_YearOrdanation) #Time series clean function
    plot(TimeSeries_YearOrdanation)
    
    #Smooth the Time Serie to Avoid Noise ----
    scatter.smooth(TimeSeries_YearOrdanation)
    
    #Augmented Dickey-Fuller Test ----
    adf.test(diff(log(TimeSeries_YearOrdanation)), alternative="stationary", k=0)
    
    #Autocorrelations and Choosing Model Order ----
    fit <- auto.arima(log(TimeSeries_YearOrdanation))
    
    #Predict the Clergy Number for 5 years ----
    pred <- predict(fit, n.ahead = 5)
    prediction_ts <- ts.plot(TimeSeries_YearOrdanation, 2.718^pred$pred, log = "y", lty = c(1,3)) 
    
    #Get the Number for 5 years ----
    Female_Prediciton_5years <- 2.718^pred$pred
    

    
    