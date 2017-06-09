############################################################################################################################################################
# PROJECT : A & E Attendance predictions

# DETAILS : Holt winters was used for forecasting daily patient attendances
#           based on previous data.
#           Split data for financial year 16/17 into quartiles based on number of attendances 
#           Then split by day. Found average of attendances per hour for each quartile and found the weightings. 
#           The 1st prediction is broken down hourly based on the weightings from the quartile the prediction falls under. 
#           The actual figures are fed in at certain times in the day and weightings may be adjusted based on where the actual cumulative figure now lies.

# AUTHOR  : Amir Munaf

# DATE    : 30/05/17
############################################################################################################################################################


setwd ("C:/Users/AMunaf/Desktop/In day predictions/A & E attendance predictions/A & E attendance predictions")

#install.packages("forecast")
#install.packages("RODBC")

library("forecast")
library("RODBC")

############################################################################################################################################################
# IMPORT DATA FROM SQL
############################################################################################################################################################

# IMPORT LIVE ACTUAL ATTENDANCES

dbhandle <-
  odbcDriverConnect('driver={SQL Server};server=SRHTDW2DB;database=inquire;trusted_connection=true')
actuals <- sqlQuery(
  dbhandle,
  "
  ;with h as (
  select DATEPART(hh,ArrivalDtm) AdmissionHour
  , COUNT(*) Total
  from inquire.[bdm].[AELive]
  where CAST(ArrivalDtm AS DATE) = CAST(GETDATE() AS DATE)
  GROUP BY DATEPART(hh,ArrivalDtm)
  )
  SELECT hour_val AS AdmissionHour
  , DATEPART(dw, GETDATE()) AS DayOfWeek
  , ISNULL(Total,0) AS Total
  FROM inquire.[dbo].HourCount
  LEFT OUTER JOIN h
  ON hour_val = AdmissionHour
  ORDER BY hour_val
  "
)

dbhandle <-
  odbcDriverConnect(
    'driver={SQL Server};server=SRHTDW2DB;database=srft_dwsources;trusted_connection=true'
  )
forecasted <- sqlQuery(
  dbhandle,
  "
  select CAST(ArrivalDtm AS DATE) AS ArrivalDtm,
  datepart(dw, ArrivalDtm) DayOfWeek,
  count(*) Total
  from srft_dwsources.pas.AELayer 
  where ArrivalDtm >='20160401'
  group by CAST(ArrivalDtm AS DATE),
  datepart(dw, ArrivalDtm)
  order by ArrivalDtm"
)


dw <- format(Sys.time(), "%u")
current_hour <- as.numeric(format(Sys.time(), "%H"))

#################################################################################################################
# SUBSET EACH DAY - HOLT WINTERS FOR FORECASTED VALUES
################################################################################################################
if (dw == 1) {
  pa <-
    holt(
      subset(forecasted, DayOfWeek == 1)[, 3],
      alpha = 0.1,
      beta = 0.2,
      initial = "simple",
      h = 1,
      l.start = 320,
      b.start = -14
    )
  day_actuals <- subset(actuals, DayOfWeek == 1)
  day <- NULL
  day$lq_hour <- c(7,4,4,2,5,2,3,4,6,13,15,16,16,24,17,17,11,11,20,14,8,12,8,13)
  day$iqr_hour <- c(6.2,5.9,3.1,3.8,4.3,2.5,3.6,4.6,8.4,16.3,17.5,19.7,21,16.9,16.6,15.4,17.3,15.8,17.7,18.2,14.1,12,11,7.9)
  day$uq_hour <- c(7,5.560976,4.292683,3.756098,3.878049,4.243902,4.195122,5.634146,9.95122,18.80488,20.87805,23.56098,22.41463,19.87805,18.92683,18.39024,19.39024,20.04878,19.53659,17.2439,16.92683,13.97561,10.43902,10)
  day_actuals <- data.frame(day_actuals,day)  
  
} else if (dw == 2) {
  pa <-
    holt(
      subset(forecasted, DayOfWeek == 2)[, 3],
      alpha = 0.4,
      beta = 0.2,
      initial = "simple",
      h = 1,
      l.start = 271,
      b.start = 4
    )
  day_actuals <- subset(actuals, DayOfWeek == 2)
  
  day <- NULL
  day$lq_hour <- c(6.25,4.5,3,2.25,2.75,3,2.75,3.75,9.75,12.75,15.75,17.25,17,11.25,14,18,14,15,15.25,14.75,14,10,9,8.75)
  day$iqr_hour <- c(6.777778,5.185185,3.962963,3.62963,3.259259,2.962963,3.296296,4.777778,8.111111,13.81481,19.14815,19.62963,18,17.77778,15.96296,15.25926,16.81481,15.55556,17,16.59259,15.62963,13.48148,11.2963,9.148148)
  day$uq_hour <- c(6.904762,5.857143,5.095238,4.047619,3.571429,3.809524,3.190476,5.333333,9.809524,17.7619,21.2381,19.57143,21,19.33333,18.04762,18.47619,18.90476,18.2381,20.09524,20.52381,16.04762,14.57143,11.2381,10.38095)
  day_actuals <- data.frame(day_actuals,day)
  
} else if (dw == 3) {
  pa <-
    holt(
      subset(forecasted, DayOfWeek == 3)[, 3],
      alpha = 0.4,
      beta = 0.2,
      initial = "simple",
      h = 1,
      l.start = 262,
      b.start = -15
    )
  day_actuals <- subset(actuals, DayOfWeek == 3)
  day <- NULL
  day$lq_hour <- c(6,5.4,3.6,4.2,1.6,2.4,2.2,4.2,6.6,10,15,15.8,17,15.6,16.2,14.8,16.6,13.2,16.6,16,17.2,12.2,8.4,9.4)
  day$iqr_hour <- c(7.105263,4.578947,3.605263,3.447368,3.684211,3.473684,3.447368,4,8.578947,15.76316,18.89474,17.63158,18.68421,17.10526,17.23684,16.5,15.84211,16.73684,17.13158,17.23684,14.94737,12.55263,10.34211,9.473684)
  day$uq_hour <- c(6.777778,5.555556,4.666667,3.888889,3.555556,4.555556,4.333333,5.444444,10,14.11111,20.66667,18.33333,21.11111,20.22222,21.11111,19,20.11111,17.11111,18.88889,20.66667,16.55556,14.77778,10.88889,9.888889)
  day_actuals <- data.frame(day_actuals,day)
  
} else if (dw == 4) {
  pa <-
    holt(
      subset(forecasted, DayOfWeek == 3)[, 3],
      alpha = 0.4,
      beta = 0.4,
      initial = "simple",
      h = 1,
      l.start = 235,
      b.start = 18
    )
  day_actuals <- subset(actuals, DayOfWeek == 4)
  day <- NULL
  day$lq_hour <- c(5.1,4.1,3.9,4.5,2.6,3.4,2.8,3.6,6.9,13.1,12.9,16.1,17.1,15.3,14.4,15.1,15.1,15.1,14.9,16.4,11.4,12,8.7,7.2)
  day$iqr_hour <- c(8.03,5.16,3.4,3.9,3.1,2.9,3.4,4.8,8.59,15.2,18.4,20.1,17.9,16.5,16.6,16.1,16.5,16.9,17.5,17,14.6,13.1,10.5,8.19)
  day$uq_hour <- c(5.875,6.875,3.375,5.125,4.75,2.875,3.625,3.75,9.125,16,21.25,21.625,19.625,19.375,17.625,17.5,18.625,18.75,17.25,19,17,13,12.375,10.625)
  day_actuals <- data.frame(day_actuals,day)
  
} else if (dw == 5) 
{holt(subset(forecasted, DayOfWeek == 5)[,3], 
      alpha = 0.2, 
      beta = 0.4, 
      initial = "simple", 
      h = 1, l.start = 294, 
      b.start = -40
)
  day_actuals <- subset(actuals, DayOfWeek == 5)
  
  day <- NULL
  day$lq_hour <- c(6.473684,4.894737,	3.473684,	2.684211,	3.631579,	3.157895,	3.210526,	3.315789,	8.368421,	12.47368,	16.21053,	16.15789,	17.68421,	15.21053,	16.78947,	16.26316,	14.73684,	15.15789,	14.31579,	13.73684,	12.10526,	13.31579,	8.368421,	7.263158)
  day$iqr_hour <- c(7.535714,4.964286,4.357143,3.571429,3.928571,3.142857,3.464286,4.892857,8.285714,15.82143,18.82143,19.75,20.53571,17.89286,15.92857,15.46429,17.17857,17,16.17857,14.64286,14.57143,12.03571,10.39286,9.285714)
  day$uq_hour <- c(9,5.666667,4.166667,4.666667,2.5,3.166667,3,7.5,8.166667,16.5,18.83333,18.16667,21.66667,20.16667,16.83333,19.83333,19.33333,19.16667,17.83333,16.66667,14.83333,15,9.5,10.66667)  
  day_actuals <- data.frame(day_actuals,day)
  
}else if (dw == 6)
{holt(subset(forecasted, DayOfWeek == 6)[,3], 
      alpha = 0.4, 
      beta = 0.4, 
      initial = "simple", 
      h = 1, 
      l.start = 287, 
      b.start = -39
)
  day_actuals <- subset(actuals, DayOfWeek == 6)
  
  day <- NULL
  day$lq_hour <- c(6.757576,4.939394,4.333333,4.272727,3.909091,3.69697,3.727273,4.333333,7.090909,11.51515,13.84848,13.63636,16.36364,15.45455,15.12121,14.69697,15.36364,13.06061,13,12.57576,12.42424,12,9.939394,7.787879)
  day$iqr_hour <- c(6.315789,6.263158,5.052632,5.052632,4.578947,4.052632,3.578947,4.736842,7.894737,12.15789,15.89474,16.42105,17.31579,14,14.78947,16.31579,15.52632,13.78947,13.63158,12.42105,13.89474,12.42105,10.10526,9.105263)
  day$uq_hour <- c(11,2,3,3,6,1,3,4,11,12,27,15,21,16,26,17,16,9,16,16,22,12,11,10)
  day_actuals <- data.frame(day_actuals,day)
  
} else if (dw == 7)
{holt(subset(forecasted, DayOfWeek == 7)[,3],
      alpha = 0.4, 
      beta = 0.2, 
      initial = "simple", 
      h = 1,  
      l.start = 304, 
      b.start = 7)
  
  day_actuals <- subset(actuals, DayOfWeek == 7)
  day <- NULL
  day$lq_hour <- c(8.142857,5.714286,5.285714,5.142857,4.285714,2.928571,3.714286,4.571429,8.071429,10.28571,13.92857,14.78571,17.64286,14.07143,15.92857,14.92857,14,13.78571,13.85714,13.64286,14.85714,10.5,10.28571,7.214286)
  day$iqr_hour <- c(7.21875,6.40625,6.1875,5.3125,4.875,5.125,3.65625,5.09375,8.09375,11.90625,16.09375,17.375,18.21875,17.90625,17.21875,17,16.78125,14.71875,15.65625,15.28125,14.3125,13.125,9.875,9.6875)
  day$uq_hour <- c(9.333333,8.833333,6.333333,6.833333,6.333333,4.833333,5.5,5.333333,11.33333,12.5,17.33333,17.16667,19.66667,19.5,18.5,17.5,17,15.16667,16.16667,19,15.16667,14.66667,10.5,10.66667)
  day_actuals <- data.frame(day_actuals,day) 
}

pa <- data.frame(pa)[, 1] 
pa_matrix <- matrix(rep(pa, 24)) # TAKES THE FORECASTED VALUE AND REPLICATES IT 24 TIMES SO THAT IT CAN  BE MERGED LATER ON

#########################################################################################
# Calculate hourly weights
day_actuals$lq_weight_hour <-(day_actuals$lq_hour / sum(day_actuals$lq_hour))
day_actuals$iqr_weight_hour <-(day_actuals$iqr_hour / sum(day_actuals$iqr_hour))
day_actuals$uq_weight_hour <-(day_actuals$uq_hour / sum(day_actuals$uq_hour))

# MERGE FORECASTED 
day_actuals <- data.frame(day_actuals, pa_matrix)
day_actuals$first_prediction <-(day_actuals$iqr_weight_hour * as.numeric(day_actuals$pa_matrix))

# Cumulatives

day_actuals$cum_lq <- cumsum(day_actuals[, 4])
day_actuals$cum_iqr <- cumsum(day_actuals[, 5])
day_actuals$cum_uq <- cumsum(day_actuals[, 6])
day_actuals$cum_actual <- cumsum(day_actuals[, 3])
day_actuals$cum_actual <- cumsum(day_actuals[, 3])
day_actuals$cum_first_prediction <- cumsum(day_actuals[, 11])


#####################################################################################################################################
# Hourly predictions
#####################################################################################################################################

hour_prediction_day <- day_actuals[day_actuals$AdmissionHour %in% (0:current_hour),] # 1/. data upto ADMISSION HOUR
xhour_prediction_day <- day_actuals[day_actuals$AdmissionHour %in% (0:current_hour + 1),] 

# tail of above is then used to obtain the cumulative values for the last hourly timepoint before the prediction

a <- tail(hour_prediction_day$cum_actual, n = 1)
b <- tail(hour_prediction_day$cum_lq, n = 1)
c <- tail(hour_prediction_day$cum_iqr, n = 1)
d <- tail(hour_prediction_day$cum_uq, n = 1)
e <- tail(hour_prediction_day$cum_first_prediction, n = 1)
xxx <- tail(xhour_prediction_day$first_prediction, n = 1)


dat_hour_prediction_day <- day_actuals[day_actuals$AdmissionHour %in% (0:current_hour + 1),] # 2/.

x <- tail(dat_hour_prediction_day$lq_weight_hour, n = 1)
y <- tail(dat_hour_prediction_day$iqr_weight_hour, n = 1)
z <- tail(dat_hour_prediction_day$uq_weight_hour, n = 1)
m <- as.numeric(pa_matrix)


day_hour <- NULL
day_hour$AdmissionHour <- current_hour + 1 # 3/. Prediction hour
day_hour$DayOfWeek <- dw
day_hour$prediction <- NA
day_hour$totalforday <- NA # Total for day is based on conditional logic below
day_hour$first_forecast <- head(day_actuals$pa_matrix, n = 1)


# compare cumulative actual of (prediction hour - 1) -
# if actual is equal/bigger than Cum UQ then prediction = UQweight per hour * forecasted value for the day
# if actual is between UQ and LQ then prediction = IQweight per hour * forecasted value for the day
# if actual is equal/smaller than Cum LQ then prediction = LQweight per hour * forecasted value for the day


if (a >= d) {
  day_hour$prediction <-
    z * m
} else if # if cumulative actual is bigger or equal to UQ
(a < d &
 a > b) {
  day_hour$prediction <-
    y * m
} else if # if cumulative actual is between UQ and LQ
(a < b) {
  day_hour$prediction <-
    x * m
} else if # if cumulative actual is smaller or equal to IQ
(a <= (e + 2) &
 a >= (e - 2)) {
  day_hour$prediction <-
    xxx
} # if cumulative actual is +-1 value away from first prediction then keep the first prediction values
# Threshold of + or - 2 for the first prediction

day_hour <- data.frame(day_hour)# Convert to dataframe
day_hour <-
  data.frame(head(day_hour, n = 1)) # this was done as the output was replicated 24 times, and only needed 1 row

# If else logic split by quartile

if (a < b) {
  # if actual is in LQ range then do
  lq_day_hour <-
    day_actuals[day_actuals$AdmissionHour > current_hour + 1,] # 2/. This is all the hours after the prediction hour
  lq_day_hour$pa_matrix <- as.numeric(lq_day_hour$pa_matrix)
  lq_day_hour$prediction <- lq_day_hour$lq_weight_hour * lq_day_hour$pa_matrix
  lq_day_hour$totalforday <- cumsum(lq_day_hour$prediction)
  day_hour$totalforday <-
    (
      sum(hour_prediction_day$Total) + day_hour$prediction + tail(lq_day_hour$totalforday, n = 1)
    )
} else if (a < d & a > b) {
  iqr_day_hour <- day_actuals[day_actuals$AdmissionHour > current_hour + 1,] # 2/.
  iqr_day_hour$pa_matrix <- as.numeric(iqr_day_hour$pa_matrix)
  iqr_day_hour$prediction <-
    iqr_day_hour$iqr_weight_hour * iqr_day_hour$pa_matrix
  iqr_day_hour$totalforday <- cumsum(iqr_day_hour$prediction)
  day_hour$totalforday <-
    (
      sum(hour_prediction_day$Total) + day_hour$prediction + tail(iqr_day_hour$totalforday, n = 1)
    )
} else if (a >= d) {
  uq_day_hour <- day_actuals[day_actuals$AdmissionHour > current_hour + 1,] # 2/.
  uq_day_hour$pa_matrix <- as.numeric(uq_day_hour$pa_matrix)
  uq_day_hour$prediction <- uq_day_hour$uq_weight_hour * uq_day_hour$pa_matrix
  uq_day_hour$totalforday <- cumsum(uq_day_hour$prediction)
  day_hour$totalforday <-
    (
      sum(hour_prediction_day$Total) + day_hour$prediction + tail(uq_day_hour$totalforday, n = 1)
    )
} else if (a <= (e + 2) & a >= (e - 2)) {
  first_day_hour <- day_actuals[day_actuals$AdmissionHour > current_hour + 1, ]
  first_day_hour$pa_matrix <- as.numeric(first_day_hour$pa_matrix)
  first_day_hour$prediction <- first_day_hour$first_prediction
  first_day_hour$totalforday <- cumsum(first_day_hour$prediction)
  day_hour$totalforday <-
    (
      sum(hour_prediction_day$Total) + day_hour$prediction + tail(first_day_hour$totalforday, n = 1)
    )
}

# Final logic statement - if null values show up then replace with iqr values

day_hour$prediction[is.na(day_hour$prediction)] <- tail(dat_hour_prediction_day$iqr_hour, n = 1)
day_hour$totalforday[is.na(day_hour$totalforday)] <- tail(day_actuals$cum_iqr, n = 1)

# Round up

day_hour$totalforday <- ceiling(day_hour$totalforday) # Rounding everything up
day_hour$prediction <- ceiling(day_hour$prediction)
day_hour$first_forecast <- ceiling(day_hour$first_forecast)


##############################################################################################
# Export to SQL
##############################################################################################


varTypes = c(run_date="datetime")

dbhandle <- odbcDriverConnect('driver={SQL Server};server=SRHTDW2DB;database=nhs_import;trusted_connection=true')
sqlDrop(dbhandle, sqtable = "r.MovingPredictedValueAE", errors = FALSE)
sqlSave(dbhandle, day_hour, tablename = "r.MovingPredictedValueAE", append=TRUE, varTypes = varTypes)

odbcCloseAll()