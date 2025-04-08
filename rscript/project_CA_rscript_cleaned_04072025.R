library(tidyverse)
library(car)
library(leaps)

#Read File
acc=read.csv("US_Accidents_March23_sampled_500k.csv", header = TRUE, sep = ",")
#acc=read.csv("cali_accidents_sampled.csv", header = TRUE, sep = ",")
#acc=read.csv("accident_data_california.csv", header = TRUE, sep = ",")

#Convert Start and End times for accident and add Duration column
 acc$Start_Time <- as.POSIXct(acc$Start_Time, format = "%Y-%m-%d %H:%M:%S")
 acc$End_Time   <- as.POSIXct(acc$End_Time, format = "%Y-%m-%d %H:%M:%S")
 acc$duration <- as.numeric(difftime(acc$End_Time, acc$Start_Time, units = "secs"))

#Filter Data for California
iCAacc=filter(acc,acc$State=="CA")

# Excluding variables not likely to affect duration, including
# ID, Source, Severity, End_Lat, End_Lng, Description, Street, City, County,
# State, Zipcode, Country, Timezone, Airport_Code, Weather_Timestamp,
# Wind_Direction, Weather_Condition, Turning Loop (becuase all valuses are false)
CAacc=subset(iCAacc,select = c(duration, Start_Lat, Start_Lng, Distance.mi., Temperature.F., Wind_Chill.F., Humidity..., Pressure.in., Visibility.mi., Wind_Speed.mph., Precipitation.in., Amenity, Bump, Crossing, Give_Way, Junction, No_Exit, Railway, Station, Stop, Traffic_Calming, Traffic_Signal, Sunrise_Sunset, Civil_Twilight, Nautical_Twilight, Astronomical_Twilight))

# Filter all Day/Night variables for null values
CAacc=filter(CAacc,CAacc$Sunrise_Sunset!="")
CAacc=filter(CAacc,CAacc$Nautical_Twilight!="")
CAacc=filter(CAacc,CAacc$Civil_Twilight!="")
CAacc=filter(CAacc,CAacc$Astronomical_Twilight!="")

# Filter all numerical variables for null values
CAacc=filter(CAacc,CAacc$Temperature.F.!="NA")
CAacc=filter(CAacc,CAacc$Wind_Chill.F.!="NA")
CAacc=filter(CAacc,CAacc$Humidity...!="NA")
CAacc=filter(CAacc,CAacc$Pressure.in.!="NA")
CAacc=filter(CAacc,CAacc$Visibility.mi.!="NA")
CAacc=filter(CAacc,CAacc$Wind_Speed.mph.!="NA")
CAacc=filter(CAacc,CAacc$Precipitation.in.!="NA")

# Filter all durations longer than 1 day
# Some durations are days, weeks or months long (probably unique circumstances)
CAacc=filter(CAacc,CAacc$duration <= 24*60*60)

# Correlation values of numerical variables with duration
RLat=cor(CAacc$Start_Lat,CAacc$duration)
RLon=cor(CAacc$Start_Lng,CAacc$duration)
RDist=cor(CAacc$Distance.mi.,CAacc$duration)
RTemp=cor(CAacc$Temperature.F.,CAacc$duration)
RChill=cor(CAacc$Wind_Chill.F.,CAacc$duration)
RHum=cor(CAacc$Humidity...,CAacc$duration)
RPres=cor(CAacc$Pressure.in.,CAacc$duration)
RVis=cor(CAacc$Visibility.mi.,CAacc$duration)
RWSpd=cor(CAacc$Wind_Speed.mph.,CAacc$duration)
RPrec=cor(CAacc$Precipitation.in.,CAacc$duration)


#Scatter plots of numerical variables with duration
ggplot(CAacc, aes(x=Start_Lat, y=duration)) +
  geom_point() +
  labs(title = "Duration vs Latitude",
       x = "Latitude",
       y = "Duration (s)")
ggplot(CAacc, aes(x=Start_Lng, y=duration)) +
  geom_point() +
  labs(title = "Duration vs Longitude",
       x = "Latitude",
       y = "Duration (s)")
ggplot(CAacc, aes(x=Distance.mi., y=duration)) +
  geom_point() +
  labs(title = "Duration vs Length of Traffic Disruption",
       x = "Disruption length (mi)",
       y = "Duration (s)")
ggplot(CAacc, aes(x=Temperature.F., y=duration)) +
  geom_point() +
  labs(title = "Duration vs Temperature",
       x = "Temperature (degF)",
       y = "Duration (s)")
ggplot(CAacc, aes(x=Wind_Chill.F., y=duration)) +
  geom_point() +
  labs(title = "Duration vs Wind Chill",
       x = "Wind Chill (degF)",
       y = "Duration (s)")
ggplot(CAacc, aes(x=Humidity..., y=duration)) +
  geom_point() +
  labs(title = "Duration vs Humidity",
       x = "Humidity (%)",
       y = "Duration (s)")
ggplot(CAacc, aes(x=Pressure.in., y=duration)) +
  geom_point() +
  labs(title = "Duration vs Barometric Pressure",
       x = "Pressure (in)",
       y = "Duration (s)")
ggplot(CAacc, aes(x=Visibility.mi., y=duration)) +
  geom_point() +
  labs(title = "Duration vs Visibility",
       x = "Visibility (mi)",
       y = "Duration (s)")
ggplot(CAacc, aes(x=Wind_Speed.mph., y=duration)) +
  geom_point() +
  labs(title = "Duration vs Wind Speed",
       x = "Wind Speed (mph)",
       y = "Duration (s)")
ggplot(CAacc, aes(x=Precipitation.in., y=duration)) +
  geom_point() +
  labs(title = "Duration vs Precipation",
       x = "Precipation in last 24h (in)",
       y = "Duration (s)")

#Boxplots of categorical variables
ggplot(CAacc, aes(x=Amenity, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Bump, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Crossing, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Give_Way, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Junction, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=No_Exit, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Railway, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Station, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Stop, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Traffic_Calming, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Traffic_Signal, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Sunrise_Sunset, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Civil_Twilight, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Nautical_Twilight, y=duration)) +
  geom_boxplot()
ggplot(CAacc, aes(x=Astronomical_Twilight, y=duration)) +
  geom_boxplot()

# Scatter plots and box plots show right skewed data for duration
# Plot with log transformation for duration
#Scatter plots of numerical variables with log(duration)
ggplot(CAacc, aes(x=Start_Lat, y=log(duration))) +
  geom_point() +
  labs(title = "Duration vs Latitude",
       x = "Latitude",
       y = "log(Duration) log (s)")
ggplot(CAacc, aes(x=Start_Lng, y=log(duration))) +
  geom_point() +
  labs(title = "Duration vs Longitude",
       x = "Latitude",
       y = "log(Duration) log (s)")
ggplot(CAacc, aes(x=Distance.mi., y=log(duration))) +
  geom_point() +
  labs(title = "Duration vs Length of Traffic Disruption",
       x = "Disruption length (mi)",
       y = "log(Duration) log (s)")
ggplot(CAacc, aes(x=Temperature.F., y=log(duration))) +
  geom_point() +
  labs(title = "Duration vs Temperature",
       x = "Temperature (degF)",
       y = "log(Duration) log (s)")
ggplot(CAacc, aes(x=Wind_Chill.F., y=log(duration))) +
  geom_point() +
  labs(title = "Duration vs Wind Chill",
       x = "Wind Chill (degF)",
       y = "log(Duration) log (s)")
ggplot(CAacc, aes(x=Humidity..., y=log(duration))) +
  geom_point() +
  labs(title = "Duration vs Humidity",
       x = "Humidity (%)",
       y = "log(Duration) log (s)")
ggplot(CAacc, aes(x=Pressure.in., y=log(duration))) +
  geom_point() +
  labs(title = "Duration vs Barometric Pressure",
       x = "Pressure (in)",
       y = "log(Duration) log (s)")
ggplot(CAacc, aes(x=Visibility.mi., y=log(duration))) +
  geom_point() +
  labs(title = "Duration vs Visibility",
       x = "Visibility (mi)",
       y = "log(Duration) log (s)")
ggplot(CAacc, aes(x=Wind_Speed.mph., y=log(duration))) +
  geom_point() +
  labs(title = "Duration vs Wind Speed",
       x = "Wind Speed (mph)",
       y = "log(Duration) log (s)")
ggplot(CAacc, aes(x=Precipitation.in., y=log(duration))) +
  geom_point() +
  labs(title = "Duration vs Precipation",
       x = "Precipation in last 24h (in)",
       y = "log(Duration) log (s)")

#Boxplots with log(duration)
ggplot(CAacc, aes(x=Amenity, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Bump, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Crossing, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Give_Way, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Junction, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=No_Exit, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Railway, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Station, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Stop, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Traffic_Calming, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Traffic_Signal, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Sunrise_Sunset, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Civil_Twilight, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Nautical_Twilight, y=log(duration))) +
  geom_boxplot()
ggplot(CAacc, aes(x=Astronomical_Twilight, y=log(duration))) +
  geom_boxplot()

#Check VIF values
allreg=lm(duration~., data = CAacc)
summary(allreg)
vif(allreg)

# High VIF values for Temperature and Wind Chill
# Check correlation
rTempWC=cor(CAacc$Temperature.F.,CAacc$Wind_Chill.F.)

# Exclude Wind Chill due to high correlation with Temperature
allreg=lm(duration~. -Wind_Chill.F., data = CAacc)
summary(allreg)
vif(allreg)

# High ViF value for Latitude
# Check correlation with Temperature and Longitude
rLatTemp=cor(CAacc$Start_Lat,CAacc$Temperature.F.)
rLatLong=cor(CAacc$Start_Lat,CAacc$Start_Lng)

# High negative correlation between Latitude and Longitude
# California has a diagonal geography where land in the south is further
# east than land in the north
# Exclude Longitude due to High correlation with Latitude
allreg=lm(duration~. -Wind_Chill.F. -Start_Lng, data = CAacc)
summary(allreg)
vif(allreg)

# High VIF values for "Day/Night" variables
# Check overlap between "Day/Night" variables
allSunCivil = CAacc$Sunrise_Sunset == CAacc$Civil_Twilight
allSunNaut = CAacc$Sunrise_Sunset == CAacc$Nautical_Twilight
allSunAst = CAacc$Sunrise_Sunset == CAacc$Astronomical_Twilight
allCivNaut = CAacc$Civil_Twilight == CAacc$Nautical_Twilight
allCivAst = CAacc$Civil_Twilight == CAacc$Astronomical_Twilight
allAstCiv = CAacc$Astronomical_Twilight == CAacc$Civil_Twilight

# Calculates percentage of "Day/Night" variables that are common
ovlpSunCivil=length(which(allSunCivil))/length(CAacc$Sunrise_Sunset)
ovlpSunNaut=length(which(allSunNaut))/length(CAacc$Sunrise_Sunset)
ovlpSunAst=length(which(allSunAst))/length(CAacc$Sunrise_Sunset)
ovlpCivNaut=length(which(allCivNaut))/length(CAacc$Sunrise_Sunset)
ovlpCivAst=length(which(allCivAst))/length(CAacc$Sunrise_Sunset)
ovlpAstCiv=length(which(allAstCiv))/length(CAacc$Sunrise_Sunset)

# Exclude Civil_Twilight variable and check VIF
allreg=lm(duration~. -Wind_Chill.F. -Start_Lng -Civil_Twilight, data = CAacc)
summary(allreg)
vif(allreg)

# Nautical Variable still high
# Exclude Nautical_Twilight variable
allreg=lm(duration~. -Wind_Chill.F. -Start_Lng -Civil_Twilight -Nautical_Twilight, data = CAacc)
summary(allreg)
vif(allreg)

# Remove excluded collinear covariates from data
CAacc=subset(CAacc,select = c(duration, Start_Lat, Distance.mi., Temperature.F., Humidity..., Pressure.in., Visibility.mi., Wind_Speed.mph., Precipitation.in., Amenity, Bump, Crossing, Give_Way, Junction, No_Exit, Railway, Station, Stop, Traffic_Calming, Traffic_Signal, Sunrise_Sunset, Astronomical_Twilight))

# First try with duration variable without transformation
# Use regsubsets to find best model using forward progression
regsets=regsubsets(CAacc$duration~.,CAacc,method = "forward")
sregsets=summary(regsets)
sregsets$adjr2
sregsets

# Best model includes covariates: Latitude, Distance, Temperature, Humidity, Wind Speed, Precipitation, Junction is true, and Sunrise_Sunset is Night
reg=lm(CAacc$duration~CAacc$Start_Lat+CAacc$Distance.mi.+CAacc$Temperature.F.+CAacc$Humidity...+CAacc$Wind_Speed.mph.+CAacc$Precipitation.in.+CAacc$Junction+CAacc$Sunrise_Sunset)
summary(reg)

# Extract fitted values and residuals from model
fitted=reg$fitted.values
residuals=reg$residuals

# Plot residual plot vs fitted values
ggplot(reg, aes(x=fitted, y=residuals)) +
  geom_point() +
  labs(title = "Residual Plot vs Fitted Values",
       x = "Model Fitted Values (s)",
       y = "Model Residuals (s)")

# Residual plot shows skewed pattern going from top left to bottom right

# Check QQ Plot
qqnorm(reg$residuals)
qqline(reg$residuals)

# QQ plot shows very heavy tails


# Use log transformation for duration
# Use regsubsets to find best model using forward progression
lregsets=regsubsets(log(CAacc$duration)~.,CAacc,method = "forward")
slregsets=summary(lregsets)
slregsets$adjr2
slregsets

# Best model includes covariates: Latitude, Distance, Temperature, Humidity, Precipitation, Junction is true, Stop is true and Astronomical_Twilight is Night
lreg=lm(log(CAacc$duration)~CAacc$Start_Lat+CAacc$Distance.mi.+CAacc$Temperature.F.+CAacc$Humidity...+CAacc$Precipitation.in.+CAacc$Junction+CAacc$Stop+CAacc$Astronomical_Twilight)
summary(lreg)

# Extract fitted values and residuals from model
fitted=lreg$fitted.values
residuals=lreg$residuals

# Plot residual plot vs fitted values
ggplot(lreg, aes(x=fitted, y=residuals)) +
  geom_point() +
  labs(title = "Residual Plot vs Fitted Values",
       x = "Model Fitted Values (s)",
       y = "Model Residuals (s)")

# Residual plot shows skewed pattern with long tail down to the right

# Check QQ Plot
qqnorm(lreg$residuals)
qqline(lreg$residuals)

# The QQ Plot shows the heavy right tail has been greatly reduced, but still shows
# heavy left tail

# Distance data is heavily skewed to the right and is an influential variable
# Examine whether transformation of Distance variable with square root
# leads to better model
CAacc$Distance.mi. = sqrt(CAacc$Distance.mi.)


# Plot transformed Distance variable with duration
ggplot(CAacc, aes(x=Distance.mi., y=duration)) +
  geom_point() +
  labs(title = "Duration vs Square Root of Disruption Length",
       x = "Square root of Disruption length (mi)",
       y = "Duration (s)")

# Test transformation of responding variable to log(Duration)
sqregsets=regsubsets(log(CAacc$duration)~.,CAacc,method = "forward")
ssqregsets=summary(sqregsets)
ssqregsets$adjr2
ssqregsets

# Best model includes covariates: Latitude, Distance, Temperature, Humidity, Precipitation, Junction is true, Stop is true and Astronomical_Twilight is Night
sqreg=lm(log(CAacc$duration)~CAacc$Start_Lat+CAacc$Distance.mi.+CAacc$Temperature.F.+CAacc$Humidity...+CAacc$Precipitation.in.+CAacc$Junction+CAacc$Stop+CAacc$Astronomical_Twilight)
summary(sqreg)


# Plot new residual plot for transformed log(Duration) variable
fitted=sqreg$fitted.values
residuals=sqreg$residuals

ggplot(sqreg, aes(x=fitted, y=residuals)) +
  geom_point() +
  labs(title = "Residual Plot vs Fitted Values",
       x = "Model Fitted Values (log (s))",
       y = "Model Residuals (log (s))")

# Values more centred around zero with less of a pattern
# Check QQ plot
qqnorm(sqreg$residuals)
qqline(sqreg$residuals)

# Heavy left tail reduced, distribution of residuals largely follows normal
# distribution

