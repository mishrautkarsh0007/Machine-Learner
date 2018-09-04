
install.packages("lubridate")
library(lubridate)


getwd()
setwd("C:/Users/utkarsh/Documents/Data analytics/Documents")

 
LinRegData <- read.csv(file = "kansas_city_house_sales.csv")
ncol(LinRegData)
View(LinRegData)
head(LinRegData)
tail(LinRegData)
str(LinRegData)



pos <- regexpr('T',LinRegData$date)   #  regexpr('T','HKHKJTKJTLJK')
unique(pos)
LinRegData$date <- substr(LinRegData$date,1,unique(pos)-1)    # substr('jaydeep',4,7)
LinRegData$date <- as.Date(LinRegData$date, format='%Y%m%d')
sum(is.na(LinRegData$date))   #length(which(is.na(LinRegData$date)))
str(LinRegData)


plot(LinRegData$price)
hist(LinRegData$price)
plot(quantile(LinRegData$price, c(.90, .95, .96, .97, .98, .99, 1)), type="l")

library(ggplot2)
x_data <- c(.90, .95, .96, .97, .98, .99, 1)
y_data <- round(quantile(LinRegData$price, c(.90, .95, .96, .97, .98, .99, 1)),0)
qplot(x = x_data, y = y_data,label = round(y_data/1000000,1), geom=c("text","point"), hjust=-0.25)

hist(LinRegData[LinRegData$price>1950000,'price'])
plot(LinRegData[LinRegData$price>1950000,'price'])
getOption("scipen")
opt <- options("scipen" = 20)
sum(LinRegData$price>3000000)

View(LinRegData[LinRegData$price==max(LinRegData$price) | LinRegData$price == 3000000,])

View(LinRegData[LinRegData$price==max(LinRegData$price) | LinRegData$price== 3000000,])
45/nrow(LinRegData)

plot(LinRegData$price)  
hist(LinRegData$price)    
x_data <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
y_data <- round(quantile(LinRegData$price, c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)),1)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
hist(LinRegData[LinRegData$price<150000,'price'])
plot(LinRegData[LinRegData$price<150000,'price'])
sum(LinRegData$price<150000)
LinRegData <- LinRegData[LinRegData$price>150000 & LinRegData$price<3000000,]


hist(LinRegData$price)
plot(LinRegData$price)

hist(log10(LinRegData$price))
plot(density(log10(LinRegData$price)))

qqnorm(LinRegData$price)
qqline(LinRegData$price)

qqnorm(log10(LinRegData$price))
qqline(log10(LinRegData$price))

LinRegData$price_log <- log10(LinRegData$price)


table(year(LinRegData$date))   #The year function is provided by lubridate(). Other similar functions are month() & day()
table(month(LinRegData$date))
plot(table(month(LinRegData$date)))

table(LinRegData$bedrooms)   
View(LinRegData[LinRegData$bedrooms==33,])
LinRegData[LinRegData$bedrooms==33,c(4)] <- 3   # id - 2402100895
View(LinRegData[LinRegData$id==2402100895,])


plot(table(LinRegData$bathrooms))
unique(LinRegData$bathrooms)

hist(LinRegData$sqft_living)   #looks like there are ouliers
plot(LinRegData$sqft_living)  #looks like there are ouliers
hist(log10(LinRegData$sqft_living))
x_data <- c(.90, .95, .96, .97, .98, .99, 1)
y_data <- round(quantile(LinRegData$sqft_living, c(.90, .95, .96, .97, .98, .99, 1)),0)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
hist(LinRegData[LinRegData$sqft_living>5000,'sqft_living'])

sum(LinRegData$sqft_living>7000)

x_data <- c(0, .05, .06, .07, .08, .09, 0.1)
y_data <- round(quantile(LinRegData$sqft_living, c(0, .05, .06, .07, .08, .09, 0.1)),0)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
hist(LinRegData[LinRegData$sqft_living<950,'sqft_living'])
sum(LinRegData$sqft_living<700)


LinRegData <- LinRegData[LinRegData$sqft_living>700 & LinRegData$sqft_living<7000,]

hist(log10(LinRegData$sqft_living))
hist(LinRegData$sqft_living)
plot(LinRegData$sqft_living)
LinRegData$sqft_living_log <- log10(LinRegData$sqft_living)
qqnorm(LinRegData$sqft_living_log)
qqline(LinRegData$sqft_living_log)


plot(LinRegData$sqft_lot)
hist(LinRegData$sqft_lot)   
hist(log10(LinRegData$sqft_lot))  

x_data <- c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)
y_data <- round(quantile(LinRegData$sqft_lot, c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)),1)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
hist(LinRegData[LinRegData$sqft_lot>213000,'sqft_lot'])
sum(LinRegData$sqft_lot>500000)

x_data <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
y_data <- round(quantile(LinRegData$sqft_lot, c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)),1)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
sum(LinRegData$sqft_lot<1000)

LinRegData <- LinRegData[LinRegData$sqft_lot<500000 & LinRegData$sqft_lot>1000,]
hist(log10(LinRegData$sqft_lot))  
qqnorm(log10(LinRegData$sqft_lot)) 
qqnorm(LinRegData$sqft_lot) 

LinRegData$sqft_lot_log <- log10(LinRegData$sqft_lot)
hist(LinRegData$sqft_lot_log)
plot(LinRegData$sqft_lot_log)


table(LinRegData$floors)
hist(LinRegData$floors)   #we need to club some levels here
LinRegData$floors_band <-ifelse(LinRegData$floors>2,2,LinRegData$floors)
hist(LinRegData$floors_band)

table(LinRegData$waterfront) # constant variable, un usuable

table(LinRegData$view) # we need to club some levels here
LinRegData$view_band <-ifelse(LinRegData$view>0,1,LinRegData$view)

table(LinRegData$condition) # create a subset
hist(LinRegData$condition)
sum(LinRegData$condition<3)
LinRegData <-LinRegData[LinRegData$condition>2,]

table(LinRegData$grade)
hist(LinRegData$grade)

LinRegData$grade_band <-ifelse(LinRegData$grade<6,6,ifelse(LinRegData$grade>10,10,LinRegData$grade))
table(LinRegData$grade_band)

plot(LinRegData$sqft_above)   #looks like there are outliers
hist(LinRegData$sqft_above)   
hist(log10(LinRegData$sqft_above))
qqnorm(LinRegData$sqft_above)   
qqnorm(log10(LinRegData$sqft_above))
x_data <- c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)
y_data <- round(quantile(LinRegData$sqft_above, c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)),1)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
hist(LinRegData[LinRegData$sqft_above>4000,'sqft_above'])
sum(LinRegData$sqft_above>5500)

x_data <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
y_data <- round(quantile(LinRegData$sqft_above, c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)),1)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
hist(LinRegData[LinRegData$sqft_above<750,'sqft_above'])
sum(LinRegData$sqft_above<700)

LinRegData <- LinRegData[LinRegData$sqft_above>700 & LinRegData$sqft_above<5500,]
LinRegData$sqft_above_log <- log10(LinRegData$sqft_above)
hist(LinRegData$sqft_above_log)
qqnorm(LinRegData$sqft_above_log)


plot(LinRegData$sqft_basement)
hist(LinRegData$sqft_basement) 


hist(log10(LinRegData$sqft_basement))    # we cant take log as log10(0) is not defined
length(which(LinRegData$sqft_basement>0))/nrow(LinRegData)
x_data <- c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)
y_data <- round(quantile(LinRegData$sqft_basement, c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)),1)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
hist(LinRegData[LinRegData$sqft_basement>1600,'sqft_basement'])
sum(LinRegData$sqft_basement>1900)

LinRegData <- LinRegData[LinRegData$sqft_basement<1900,]
plot(LinRegData$sqft_basement)
hist(LinRegData$sqft_basement) 



hist(LinRegData$yr_built)
plot(LinRegData$yr_built)

hist(LinRegData$yr_renovated)
plot(LinRegData$yr_renovated)
length(which(LinRegData$yr_renovated>0))/nrow(LinRegData)  #almost all is 0. not usable.

min(LinRegData$zipcode)
max(LinRegData$zipcode)
LinRegData$zipcode <- LinRegData$zipcode - min(LinRegData$zipcode)
LinRegData$zipcode_factor <- as.factor(LinRegData$zipcode)

temp_df1 <- as.data.frame(aggregate(price~zipcode, data = LinRegData, sum))
temp_df2 <- as.data.frame(aggregate(sqft_lot~zipcode, data = LinRegData, sum))
temp_df1$sqft_lot <- temp_df2$sqft_lot
temp_df1$price_per_sqft_lot <- temp_df1$price/temp_df1$sqft_lot
hist(temp_df1$price_per_sqft_lot)
zip_vec <- round(quantile(temp_df1$price_per_sqft_lot, c(0.25,0.50,0.75)),1)
temp_df1$zipcode_band <- ifelse(temp_df1$price_per_sqft_lot<=zip_vec[1], 1,
                                (ifelse(temp_df1$price_per_sqft_lot<=zip_vec[2], 2,
                                        (ifelse(temp_df1$price_per_sqft_lot<=zip_vec[3], 3,4)))))
LinRegData$new_col <- temp_df1$zipcode_band


hist(LinRegData$lat)
hist(log10(LinRegData$lat))
qqnorm(LinRegData$lat)
qqnorm(log10(LinRegData$lat))

hist(LinRegData$long)
hist(log10(LinRegData$long))     
min(LinRegData$long)
max(LinRegData$long)
sum(LinRegData$long>0)
LinRegData$long <- abs(LinRegData$long)
hist(LinRegData$long)        
hist(log10(LinRegData$long))
qqnorm(LinRegData$long)
qqnorm(log10(LinRegData$long)) 
x_data <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
y_data <- round(quantile(LinRegData$long, c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)),1)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
hist(LinRegData[LinRegData$long<121.8,'long'])
sum(LinRegData$long<121.7)

LinRegData <- LinRegData[LinRegData$long >= 121.7,]
hist(LinRegData$long)
qqnorm(LinRegData$long)



LinRegData$Sales_year <- year(LinRegData$date)
LinRegData$Sales_month <- month(LinRegData$date)

LinRegData$per_living <- (LinRegData$sqft_living*100)/LinRegData$sqft_lot
hist(LinRegData$per_living)

LinRegData$per_above <- (LinRegData$sqft_above*100)/LinRegData$sqft_lot
hist(LinRegData$per_above)

LinRegData$age <- LinRegData$Sales_year - LinRegData$yr_built
hist(LinRegData$age)

LinRegData$age_renovated <- ifelse(LinRegData$yr_renovated>0,LinRegData$Sales_year-LinRegData$yr_renovated,LinRegData$age)
hist(LinRegData$age_renovated)

LinRegData$renovated_flag <- ifelse(LinRegData$yr_renovated>0,1,0)



plot(LinRegData$age,LinRegData$age_renovated)


colnames(LinRegData)
LinRegData_bkup <- LinRegData
LinRegData <- LinRegData[,-c(1:3,6,7,13,15:17)]


