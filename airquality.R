library(tidyverse)
library(httr)
library(curl)
library(jsonlite)
library(stats)
library(ggplot2)


#### Get desired parameter code

parameters_link <- "https://aqs.epa.gov/data/api/list/parametersByClass?email=lvg23@scarletmail.rutgers.edu&key=cobaltheron82&pc=ALL"

parametercodesdataframe <- parameters_link %>% jsonlite::fromJSON() %>% as.data.frame()

parametercodesdataframe

colnames(parametercodesdataframe)

finalparametercodes_df <- parametercodesdataframe[5:6]

finalparametercodes_df

colnames(finalparametercodes_df)

parametercode <- function(x){
  return (finalparametercodes_df[finalparametercodes_df$Data.value_represented == x,][1,1])
}
parametercode('Smoke')

###### Get desired state code. 

state_codes <- "https://aqs.epa.gov/data/api/list/states?email=lvg23@scarletmail.rutgers.edu&key=cobaltheron82"
statecodes_df <- state_codes %>% jsonlite::fromJSON() %>% as.data.frame()
statecodes_df

colnames(statecodes_df)

finalstatecodes_df <- statecodes_df[5:6]

finalstatecodes_df

colnames(finalstatecodes_df)

statecode <- function(x){
  return (finalstatecodes_df[finalstatecodes_df$Data.value_represented == x,][1,1])
}
statecode('North Carolina')

#### Get desired county by the given state code.

c_url1<- "https://aqs.epa.gov/data/api/list/countiesByState?email=lvg23@scarletmail.rutgers.edu&key=cobaltheron82&state="
c_url2 <- "39"
countyurl <- paste(c_url1, c_url2,sep = "")
countyurl

tempdf <- countyurl %>% jsonlite::fromJSON() %>% as.data.frame()

tempdf <- tempdf[5:6]

tempdf[tempdf$Data.value_represented == 'Adams',][1,1]

countycode <- function(statename, countyname){
  c_url1<- "https://aqs.epa.gov/data/api/list/countiesByState?email=lvg23@scarletmail.rutgers.edu&key=cobaltheron82&state="
  c_url2 <- statecode(statename)
  countyurl <- paste(c_url1, c_url2,sep = "")
  tempdf <- countyurl %>% jsonlite::fromJSON() %>% as.data.frame()
  tempdf <- tempdf[5:6]
  return (tempdf[tempdf$Data.value_represented == countyname,][1,1])
}
countycode('North Carolina', 'Wake')

countycode('New York', 'Queens')

##Daily Summary Data by County


urlpart1 <- "https://aqs.epa.gov/data/api/dailyData/byCounty?email=test@aqs.api&key=test&param="
urlpart2<- "&bdate="
urlpart3 <- "&edate="
urlpart4 <- "&state="
urlpart5 <- "&county="


tablefunction <- function(statename, countyname, parametername, year1){
  urlpart1 <- "https://aqs.epa.gov/data/api/dailyData/byCounty?email=lvg23@scarletmail.rutgers.edu&key=cobaltheron82&param="
  urlpart2<- "&bdate="
  urlpart3 <- "&edate="
  urlpart4 <- "&state="
  urlpart5 <- "&county="
  stateinput <- statecode(statename)
  countyinput <- countycode(statename, countyname)
  parameterinput <- parametercode(parametername)
  bdateinput <- paste(year1, "0101", sep = "")
  edateinput <- paste(year1, "1231", sep = "")
  urlinput <- paste(urlpart1,parameterinput,urlpart2,bdateinput,urlpart3,edateinput,urlpart4, stateinput, urlpart5, countyinput, sep = "")
  table_df <- urlinput %>% jsonlite::fromJSON() %>% as.data.frame()
  table_df <- cbind(table_df[16:16], table_df[23:23])
  table_df<-table_df[complete.cases(table_df),]
  datelist<-unique(table_df$Data.date_local) #creating a list of all dates within the given time range. (for the for loop)
  
  colnameslist<-colnames(table_df) #Taking note of the column names (because we'll need to create the tempdf within the for loop with the same column names)
  #Currently, the dataframe has multiple values for single dates... the following for loop makes sure only the max parameter value is retained for each corresponding date. 
  for (i in datelist) {
    if (nrow(table_df[table_df$Data.date_local == i,]) > 1)
    {
      maxvalue <- max(table_df[table_df$Data.date_local == i,]$Data.first_max_value, na.rm = TRUE)
      table_df <- table_df[table_df$Data.date_local != i,]
      tempdf <- data.frame(i, maxvalue)
      names(tempdf) <- c("Data.date_local","Data.first_max_value")
      table_df <- rbind(table_df, tempdf)
    }
  }
  
  
  table_df <- table_df[order(as.Date(table_df$Data.date_local, format="%Y-%m-%d")),] #Sorting the dataframe one last time to sequence the dates chronologically.
  return(table_df)
}

tablefunction("North Carolina", "Wake", "PM2.5 - Local Conditions", 2019)


#### Creating two data frames from the function "tablefunction:" one for 2019 and one for 2020.

df_year3 <- as.data.frame(tablefunction("North Carolina", "Wake", "PM2.5 - Local Conditions", 2021))
df_year3

class(df_year1)

df_year4 <- as.data.frame(tablefunction("North Carolina", "Wake", "PM2.5 - Local Conditions", 2020))
df_year4
class(df_year2)


df_year1 <- as.data.frame(tablefunction("North Carolina", "Wake", "PM2.5 - Local Conditions", 2019))
df_year1

class(df_year1)

df_year2 <- as.data.frame(tablefunction("North Carolina", "Wake", "PM2.5 - Local Conditions", 2020))
df_year2
class(df_year2)


class(df_year1$Data.date_local)

a<-as.factor(df_year1$Data.date_local)
a
abis<-strptime(a,format="%Y-%m-%d")
abis #defining what is the original format of your date
b<-as.Date(abis,format="%Y-%m-%d", tz = "EST") #defining what is the desired format of your date
b
class(b)
head(b)
str(b)
#b is df_year1$Data.date_local

class(df_year2$Data.date_local)
v<-as.factor(df_year2$Data.date_local)
v
ebis<-strptime(v,format="%Y-%m-%d")
ebis #defining what is the original format of your date
d<-as.Date(ebis,format="%Y-%m-%d", tz = "EST") #defining what is the desired format of your date
d
class(d)
head(d)
str(d)

#d is df_year2$Data.date_local



ggp <- ggplot(NULL, aes(x= Data.date_local,y=Data.first_max_value)) +
  geom_point(data = df_year1, col = 'red') + 
  geom_point(data = df_year2, col = 'blue') + 
  labs(x = "Days", y = "PM2.5 Max Value", title = "Wake County, NC Max PM2.5 Value for 2019 and 2020")

ggp

################################################################
df_year1 <- as.data.frame(tablefunction("New York", "New York", "PM2.5 - Local Conditions", 2019))
df_year1

class(df_year1)


df_year2 <- as.data.frame(tablefunction("New York", "New York", "PM2.5 - Local Conditions", 2020))
df_year2
class(df_year2)

######
class(df_year1$Data.date_local) #"character"
df_year1$Data.date_local <- as.Date(df_year1$Data.date_local, format='%y-%m-%d')
class(df_year1$Data.date_local)

class(df_year2$Data.date_local) "numeric"
df_year2$Data.date_local <- as.Date(df_year2$Data.date_local, format='%y/%m/%d')
class(df_year2$Data.date_local)

class(bothyearsdf$Data.first_max_value) "numeric"
class(bothyearsdf$Data.date_local) #"character"
bothyearsdf$Data.date_local <- as.Date(df_year2$Data.date_local, format='%y/%m/%d')
class(bothyearsdf$Data.date_local)#"date"

head(bothyearsdf$Data.date_local)
bothyearsdf$Data.date_local

#####

bothyearsdf <- rbind(df_year1, df_year2)
bothyearsdf
class(bothyearsdf)
write.csv(bothyearsdf, "Wake,NC2019and2020,csv")

##***
w <- ggplot(bothyearsdf,
            aes(x = as.Date(Data.date_local),
                y = Data.first_max_value)) +
  geom_point() + 
  geom_smooth(color = 'red') +
  labs(x = "Days", y = "PM2.5 Max Value",
       title = "New York, NY Max PM2.5 Value for 2019 and 2020")

w
##***


severalyears <- function(statename, countyname, parametername, year1, year2, year3, year4){
  df1 <- tablefunction(statename, countyname, parametername, year1)
  df2 <- tablefunction(statename, countyname, parametername, year2)
  df3 <- tablefunction(statename, countyname, parametername, year3)
  df4 <- tablefunction(statename, countyname, parametername, year4)
  all <- rbind(df1, df2, df3, df4)
  allyears <- data.frame(all)
  allyears
}

Severalyears <- as.data.frame(severalyears("North Carolina", "Wake", "PM2.5 - Local Conditions", 2017, 2018, 2019, 2020))
write.csv(Severalyears,"Wake,NC17,18,19,20")


k <- ggplot(Severalyears,
            aes(x = as.Date(Data.date_local),
                y = Data.first_max_value)) +
  geom_point() + 
  geom_smooth(color = 'red') +
  labs(x = "Days", y = "PM2.5 Max Value",
       title = "Wake, NC Max PM2.5 Value for 2017 to 2020")
k

finalfunction<- function(statename, countyname, parametername, year1, year2){
  
  df1 <- tablefunction(statename, countyname, parametername, year1)
  df2 <- tablefunction(statename, countyname, parametername, year2)
  
  yearcolumn <- c(year1, year2)
  avg1 <- mean(df1$Data.first_max_value)
  avg2 <- mean(df2$Data.first_max_value)
  averagecolumn <- c(avg1, avg2)
  
  min1 <- min(df1$Data.first_max_value)
  min2 <- min(df2$Data.first_max_value)
  mincolumn <- c(min1, min2)
  
  max1 <- max(df1$Data.first_max_value)
  max2 <- max(df2$Data.first_max_value)
  maxcolumn <- c(max1, max2)
  
  sd1 <- sd(df1$Data.first_max_value)
  sd2 <- sd(df2$Data.first_max_value)
  sdcolumn <- c(sd1, sd2)
  
  sdcolumn <- c(sd(df1$Data.first_max_value), sd(df2$Data.first_max_value))
  df <- data.frame(yearcolumn, averagecolumn, mincolumn, maxcolumn, sdcolumn)
  
  df
  gridExtra::grid.table(df)
}



statecode("New Jersey")
countycode("New Jersey", "Bergen")
parametercode("PM2.5 - Local Conditions")

#NOTE: We are collecting the Max PM2.5 (or any other parameter) value for each day throughout the year. 
#So, the averagecolumn corresponds to the average of all these max values.
#the mincolumn corresponds to the minimum value out of all these max values
#and so on...

#### Comparing 2016 and 2020 PM2.5 levels for Bergen County, New Jersey.

finalfunction("New Jersey", "Bergen", "PM2.5 - Local Conditions", 2016, 2020)

####The average max pollution value per day increased in Bergen County for 2020.

finalfunction("New York", "New York", "PM2.5 - Local Conditions", 2016, 2020 )

finalfunction("New Jersey", "Middlesex", "PM2.5 - Local Conditions", 2016, 2020)

class(finalfunction)
Finalfunction <- as.data.frame(finalfunction)

install.packages("gridExtra")
library(gridExtra)
grid.table(finalfunction("New York", "New York", "PM2.5 - Local Conditions", 2016, 2020 ))


