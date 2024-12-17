library(tidyverse)
library(readxl)
library(lubridate)
library(esquisse)
library(dplyr)
library(data.table)
library(anytime)
library(tidyr)
library(ggpubr)
library("gridExtra")
library(emmeans)
library(multcompView)
library(multcomp)
library(car)

# N2O flux data ---------------------------------------------------

#read in n2o data
n2oflux<-read_excel("/Users/emstuch/Desktop/Reinhart study/R scripts/Associated Excel Files/finalizeddata_all_data repository.xlsx", sheet = "N2O fluxes")
View(n2oflux)

#set timestamp to a less wonky format
n2oflux2<-n2oflux
n2oflux2$Date <-ymd_hms(n2oflux2$Date,tz=Sys.timezone())
View(n2oflux2)
str(n2oflux2)

n2oflux3<- n2oflux2 %>% mutate(`Short Date` = as_date(n2oflux2$Date), logN2O = log(n2oflux2$`N2O (nmol/m2/s)`), `N2O flux` = n2oflux2$`N2O (nmol/m2/s)`)
View(n2oflux3)

n2oflux3$Chamber<-as.factor(n2oflux3$Chamber)
str(n2oflux3)

n2oflux3 <- reorder(desc(n2oflux3$Chamber))

#Now filtering the data by chamber so I can calculate the mean, median, and SD to calculate the degree of skew

# Early growing season chamber data ---------------------------------------------------

early <- n2oflux3 %>%
  filter(`Short Date` >= "2022-05-13" & `Short Date` <= "2022-07-07")
View(early)
max(early$`N2O flux`)

# NODE 1 ------------------------------------------------------------------
##### 
#N1C1
N1C1 <- early %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N1C1)

#N1C1 SumStats
n1c1SumStats <- plyr::ddply(N1C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c1SumStats

##### 
#N1C2
N1C2 <- early %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N1C2)

#N1C2 SumStats
n1c2SumStats <- plyr::ddply(N1C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c2SumStats

##### 
#N1C3
N1C3 <- early %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N1C3)

#N1C3 SumStats
n1c3SumStats <- plyr::ddply(N1C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c3SumStats

##### 
#N1C4
N1C4 <- early %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N1C4)

#N1C4 SumStats
n1c4SumStats <- plyr::ddply(N1C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c4SumStats

# NODE 2 ------------------------------------------------------------------
##### 
#N2C1
N2C1 <- early %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N2C1)

#N2C1 SumStats
n2c1SumStats <- plyr::ddply(N2C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n2c1SumStats

##### 
#N2C2
N2C2 <- early %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N2C2)

#N2C2 SumStats
n2c2SumStats <- plyr::ddply(N2C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n2c2SumStats

##### 
#N2C3
N2C3 <- early %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N2C3)

#N2C3 SumStats
n2c3SumStats <- plyr::ddply(N2C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n)  )
n2c3SumStats

##### 
#N2C4
N2C4 <- early %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N2C4)

#N2C4 SumStats
n2c4SumStats <- plyr::ddply(N2C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n2c4SumStats

# NODE 3 ------------------------------------------------------------------
##### 
#N3C1
N3C1 <- early %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N3C1)

#N3C1 SumStats
n3c1SumStats <- plyr::ddply(N3C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n3c1SumStats

##### 
#N3C2
N3C2 <- early %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N3C2)

#N3C2 SumStats
n3c2SumStats <- plyr::ddply(N3C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n)  )
n3c2SumStats

##### 
#N3C3
N3C3 <- early %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N3C3)

#N3C3 SumStats
n3c3SumStats <- plyr::ddply(N3C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n3c3SumStats

##### 
#N3C4
N3C4 <- early %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N3C4)

#N3C4 SumStats
n3c4SumStats <- plyr::ddply(N3C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n3c4SumStats

# NODE 4 ------------------------------------------------------------------
##### 
#N4C1
N4C1 <- early %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N4C1)

#N4C1 SumStats
n4c1SumStats <- plyr::ddply(N4C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n4c1SumStats

##### 
#N4C2
N4C2 <- early %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N4C2)

#N4C2 SumStats
n4c2SumStats <- plyr::ddply(N4C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n)  )
n4c2SumStats

##### 
#N4C3
N4C3 <- early %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N4C3)

#N4C3 SumStats
n4c3SumStats <- plyr::ddply(N4C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n4c3SumStats

##### 
#N4C4
N4C4 <- early %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N4C4)

#N4C4 SumStats
n4c4SumStats <- plyr::ddply(N4C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n4c4SumStats

# Skew calcs - EARLY SEASON --------------------------------

skewdata <- rbind(n1c1SumStats,n1c2SumStats,n1c3SumStats,n1c4SumStats,
                  n2c1SumStats,n2c2SumStats,n2c3SumStats,n2c4SumStats,
                  n3c1SumStats,n3c2SumStats,n3c3SumStats,n3c4SumStats,
                  n4c1SumStats,n4c2SumStats,n4c3SumStats,n4c4SumStats)
View(skewdata)

#####
#Adding column to data frame with Skew calculation 
skewdata_early <- skewdata %>%
  mutate(Skew = (3*(mean - median)) / sd) %>%
  mutate("Time Interval" = "Early")
View(skewdata_early)

# Late growing season chamber data ---------------------------------------------------

late <- n2oflux3 %>%
  filter(`Short Date` >= "2022-07-08" & `Short Date` <= "2022-10-31")
View(late)
max(late$`N2O flux`)

# NODE 1 ------------------------------------------------------------------
##### 
#N1C1
N1C1 <- late %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N1C1)

#N1C1 SumStats
n1c1SumStats <- plyr::ddply(N1C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c1SumStats

##### 
#N1C2
N1C2 <- late %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N1C2)

#N1C2 SumStats
n1c2SumStats <- plyr::ddply(N1C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c2SumStats

##### 
#N1C3
N1C3 <- late %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N1C3)

#N1C3 SumStats
n1c3SumStats <- plyr::ddply(N1C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c3SumStats

##### 
#N1C4
N1C4 <- late %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N1C4)

#N1C4 SumStats
n1c4SumStats <- plyr::ddply(N1C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c4SumStats

# NODE 2 ------------------------------------------------------------------
##### 
#N2C1
N2C1 <- late %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N2C1)

#N2C1 SumStats
n2c1SumStats <- plyr::ddply(N2C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n2c1SumStats

##### 
#N2C2
N2C2 <- late %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N2C2)

#N2C2 SumStats
n2c2SumStats <- plyr::ddply(N2C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n2c2SumStats

##### 
#N2C3
N2C3 <- late %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N2C3)

#N2C3 SumStats
n2c3SumStats <- plyr::ddply(N2C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n)  )
n2c3SumStats

##### 
#N2C4
N2C4 <- late %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N2C4)

#N2C4 SumStats
n2c4SumStats <- plyr::ddply(N2C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n2c4SumStats

# NODE 3 ------------------------------------------------------------------
##### 
#N3C1
N3C1 <- late %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N3C1)

#N3C1 SumStats
n3c1SumStats <- plyr::ddply(N3C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n3c1SumStats

##### 
#N3C2
N3C2 <- late %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N3C2)

#N3C2 SumStats
n3c2SumStats <- plyr::ddply(N3C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n)  )
n3c2SumStats

##### 
#N3C3
N3C3 <- late %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N3C3)

#N3C3 SumStats
n3c3SumStats <- plyr::ddply(N3C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n3c3SumStats

##### 
#N3C4
N3C4 <- late %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N3C4)

#N3C4 SumStats
n3c4SumStats <- plyr::ddply(N3C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n3c4SumStats

# NODE 4 ------------------------------------------------------------------
##### 
#N4C1
N4C1 <- late %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N4C1)

#N4C1 SumStats
n4c1SumStats <- plyr::ddply(N4C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n4c1SumStats

##### 
#N4C2
N4C2 <- late %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N4C2)

#N4C2 SumStats
n4c2SumStats <- plyr::ddply(N4C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n)  )
n4c2SumStats

##### 
#N4C3
N4C3 <- late %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N4C3)

#N4C3 SumStats
n4c3SumStats <- plyr::ddply(N4C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n4c3SumStats

##### 
#N4C4
N4C4 <- late %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N4C4)

#N4C4 SumStats
n4c4SumStats <- plyr::ddply(N4C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n4c4SumStats

# Skew calcs - LATE SEASON --------------------------------

skewdata <- rbind(n1c1SumStats,n1c2SumStats,n1c3SumStats,n1c4SumStats,
                  n2c1SumStats,n2c2SumStats,n2c3SumStats,n2c4SumStats,
                  n3c1SumStats,n3c2SumStats,n3c3SumStats,n3c4SumStats,
                  n4c1SumStats,n4c2SumStats,n4c3SumStats,n4c4SumStats)
View(skewdata)

#####
#Adding column to data frame with Skew calculation 
skewdata_late <- skewdata %>%
  mutate(Skew = (3*(mean - median)) / sd)%>%
  mutate("Time Interval" = "Late")
View(skewdata_late)

# Non growing season chamber data ---------------------------------------------------

nongrow <- n2oflux3 %>%
  filter(`Short Date` >= "2022-11-01" & `Short Date` <= "2023-04-30")
View(nongrow)
max(nongrow$`N2O flux`)

# NODE 1 ------------------------------------------------------------------
##### 
#N1C1
N1C1 <- nongrow %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N1C1)

#N1C1 SumStats
n1c1SumStats <- plyr::ddply(N1C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c1SumStats

##### 
#N1C2
N1C2 <- nongrow %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N1C2)

#N1C2 SumStats
n1c2SumStats <- plyr::ddply(N1C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c2SumStats

##### 
#N1C3
N1C3 <- nongrow %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N1C3)

#N1C3 SumStats
n1c3SumStats <- plyr::ddply(N1C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c3SumStats

##### 
#N1C4
N1C4 <- nongrow %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N1C4)

#N1C4 SumStats
n1c4SumStats <- plyr::ddply(N1C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c4SumStats

# NODE 2 ------------------------------------------------------------------
##### 
#N2C1
N2C1 <- nongrow %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N2C1)

#N2C1 SumStats
n2c1SumStats <- plyr::ddply(N2C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n2c1SumStats

##### 
#N2C2
N2C2 <- nongrow %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N2C2)

#N2C2 SumStats
n2c2SumStats <- plyr::ddply(N2C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n2c2SumStats

##### 
#N2C3
N2C3 <- nongrow %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N2C3)

#N2C3 SumStats
n2c3SumStats <- plyr::ddply(N2C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n)  )
n2c3SumStats

##### 
#N2C4
N2C4 <- nongrow %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N2C4)

#N2C4 SumStats
n2c4SumStats <- plyr::ddply(N2C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n2c4SumStats

# NODE 3 ------------------------------------------------------------------
##### 
#N3C1
N3C1 <- nongrow %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N3C1)

#N3C1 SumStats
n3c1SumStats <- plyr::ddply(N3C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n3c1SumStats

##### 
#N3C2
N3C2 <- nongrow %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N3C2)

#N3C2 SumStats
n3c2SumStats <- plyr::ddply(N3C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n)  )
n3c2SumStats

##### 
#N3C3
N3C3 <- nongrow %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N3C3)

#N3C3 SumStats
n3c3SumStats <- plyr::ddply(N3C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n3c3SumStats

##### 
#N3C4
N3C4 <- nongrow %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N3C4)

#N3C4 SumStats
n3c4SumStats <- plyr::ddply(N3C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n3c4SumStats

# NODE 4 ------------------------------------------------------------------
##### 
#N4C1
N4C1 <- nongrow %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N4C1)

#N4C1 SumStats
n4c1SumStats <- plyr::ddply(N4C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n4c1SumStats

##### 
#N4C2
N4C2 <- nongrow %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N4C2)

#N4C2 SumStats
n4c2SumStats <- plyr::ddply(N4C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n)  )
n4c2SumStats

##### 
#N4C3
N4C3 <- nongrow %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N4C3)

#N4C3 SumStats
n4c3SumStats <- plyr::ddply(N4C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n4c3SumStats

##### 
#N4C4
N4C4 <- nongrow %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N4C4)

#N4C4 SumStats
n4c4SumStats <- plyr::ddply(N4C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n4c4SumStats

# Skew calcs - NON GROWING SEASON --------------------------------

skewdata <- rbind(n1c1SumStats,n1c2SumStats,n1c3SumStats,n1c4SumStats,
                  n2c1SumStats,n2c2SumStats,n2c3SumStats,n2c4SumStats,
                  n3c1SumStats,n3c2SumStats,n3c3SumStats,n3c4SumStats,
                  n4c1SumStats,n4c2SumStats,n4c3SumStats,n4c4SumStats)
View(skewdata)

#####
#Adding column to data frame with Skew calculation 
skewdata_nongrow <- skewdata %>%
  mutate(Skew = (3*(mean - median)) / sd)%>%
  mutate("Time Interval" = "Non growing")
View(skewdata_nongrow)

# Whole year chamber data ---------------------------------------------------

whole <- n2oflux3
View(whole)
max(whole$`N2O flux`)

# NODE 1 ------------------------------------------------------------------
##### 
#N1C1
N1C1 <- whole %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N1C1)

#N1C1 SumStats
n1c1SumStats <- plyr::ddply(N1C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c1SumStats

##### 
#N1C2
N1C2 <- whole %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N1C2)

#N1C2 SumStats
n1c2SumStats <- plyr::ddply(N1C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c2SumStats

##### 
#N1C3
N1C3 <- whole %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N1C3)

#N1C3 SumStats
n1c3SumStats <- plyr::ddply(N1C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c3SumStats

##### 
#N1C4
N1C4 <- whole %>%
  filter(Node %in% 
           "Node 1") %>%
  filter(`Chamber ID` %in% "b1-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N1C4)

#N1C4 SumStats
n1c4SumStats <- plyr::ddply(N1C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n1c4SumStats

# NODE 2 ------------------------------------------------------------------
##### 
#N2C1
N2C1 <- whole %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N2C1)

#N2C1 SumStats
n2c1SumStats <- plyr::ddply(N2C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n2c1SumStats

##### 
#N2C2
N2C2 <- whole %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N2C2)

#N2C2 SumStats
n2c2SumStats <- plyr::ddply(N2C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n2c2SumStats

##### 
#N2C3
N2C3 <- whole %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N2C3)

#N2C3 SumStats
n2c3SumStats <- plyr::ddply(N2C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n)  )
n2c3SumStats

##### 
#N2C4
N2C4 <- whole %>%
  filter(Node %in% 
           "Node 2") %>%
  filter(`Chamber ID` %in% "b2-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N2C4)

#N2C4 SumStats
n2c4SumStats <- plyr::ddply(N2C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n2c4SumStats

# NODE 3 ------------------------------------------------------------------
##### 
#N3C1
N3C1 <- whole %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N3C1)

#N3C1 SumStats
n3c1SumStats <- plyr::ddply(N3C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n3c1SumStats

##### 
#N3C2
N3C2 <- whole %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N3C2)

#N3C2 SumStats
n3c2SumStats <- plyr::ddply(N3C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n)  )
n3c2SumStats

##### 
#N3C3
N3C3 <- whole %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N3C3)

#N3C3 SumStats
n3c3SumStats <- plyr::ddply(N3C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n3c3SumStats

##### 
#N3C4
N3C4 <- whole %>%
  filter(Node %in% 
           "Node 3") %>%
  filter(`Chamber ID` %in% "b3-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N3C4)

#N3C4 SumStats
n3c4SumStats <- plyr::ddply(N3C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n3c4SumStats

# NODE 4 ------------------------------------------------------------------
##### 
#N4C1
N4C1 <- whole %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-1") %>%
  filter(Chamber %in% "Chamber 1")
View(N4C1)

#N4C1 SumStats
n4c1SumStats <- plyr::ddply(N4C1, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n4c1SumStats

##### 
#N4C2
N4C2 <- whole %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-2") %>%
  filter(Chamber %in% "Chamber 2")
View(N4C2)

#N4C2 SumStats
n4c2SumStats <- plyr::ddply(N4C2, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n)  )
n4c2SumStats

##### 
#N4C3
N4C3 <- whole %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-3") %>%
  filter(Chamber %in% "Chamber 3")
View(N4C3)

#N4C3 SumStats
n4c3SumStats <- plyr::ddply(N4C3, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n4c3SumStats

##### 
#N4C4
N4C4 <- whole %>%
  filter(Node %in% 
           "Node 4") %>%
  filter(`Chamber ID` %in% "b4-4") %>%
  filter(Chamber %in% "Chamber 4")
View(N4C4)

#N4C4 SumStats
n4c4SumStats <- plyr::ddply(N4C4, c("`Chamber`", "Node"), summarise,
                            n = length(`N2O flux`),
                            mean = mean(`N2O flux`),
                            median = median(`N2O flux`),
                            sd = sd(`N2O flux`),
                            se = sd / sqrt(n) )
n4c4SumStats

# Skew calcs - WHOLE YEAR --------------------------------

skewdata <- rbind(n1c1SumStats,n1c2SumStats,n1c3SumStats,n1c4SumStats,
                  n2c1SumStats,n2c2SumStats,n2c3SumStats,n2c4SumStats,
                  n3c1SumStats,n3c2SumStats,n3c3SumStats,n3c4SumStats,
                  n4c1SumStats,n4c2SumStats,n4c3SumStats,n4c4SumStats)
View(skewdata)



#####
#Adding column to data frame with Skew calculation 
skewdata_wholeyear <- skewdata %>%
  mutate(Skew = (3*(mean - median)) / sd)%>%
  mutate("Time Interval" = "Whole year")
View(skewdata_wholeyear)

# Skew histograms -multipanel fig --------------------------------

skewall <- rbind(skewdata_early,skewdata_late,skewdata_nongrow,
                 skewdata_wholeyear)
View(skewall)

#####
#now modifying the Chamber names so they're titled Chamber 1-16, rather
#than Chamber 1-4

# Create a sequence of Chamber values from 1 to 16 for each Node
chamber_seq <- rep(1:16, times = 4)

# Create a sequence of Chamber names "Chamber 1", "Chamber 2", ..., "Chamber 16"
chamber_names <- paste("Chamber", 1:16)

# Repeat the sequence of Chamber names for each set of 16 Chambers
chamber_names_all <- rep(chamber_names, times = 4)

# Mutate the data frame to update the Chamber and Time Interval columns
skewall <- skewall %>%
  mutate(
    Chamber = chamber_names_all)   # Update Chamber column  

skewall$Chamber <- factor(skewall$Chamber, levels = paste("Chamber", 1:16))

# View the updated data frame
View(skewall)

skewall$`Time Interval` <- factor(skewall$`Time Interval`, levels = c("Early", "Late", "Non growing", "Whole year"),
                        labels = c("Early growing season", "Late growing season", "Non-growing season", "Whole year"))
View(skewall)

#####
#now making histogram
library(ggplot2)
library(ggpattern)

outline_colors <- rainbow(16)  # Adjust the palette as needed
pattern_list <- c("stripe", "crosshatch", "dots", "grid") 

#custom_grey <- c("grey77","grey49", "grey28", "grey10")
custom_grey <- grey.colors(16)

# Create the histogram plot
hist <- ggplot(skewall, aes(x = Skew, fill = Chamber)) +  #color = Node)) +
  geom_histogram(bins = 30L, binwidth = .3, size = 1.5) +
  scale_fill_manual(values = colorspace::rainbow_hcl(16)) +
  #scale_color_brewer(palette = "Set1", direction = 10) +
  #scale_fill_manual(values = custom_grey) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20L, face = "bold"), 
        axis.title.x = element_text(size = 20L, face = "bold"),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 50, size= 11, hjust = 1),
        legend.title = element_text(size=15, face = "bold"), 
        legend.text = element_text(size=15),
        legend.key = ) +
  facet_wrap(vars(`Time Interval`))
hist

#now making a histogram where there aren't chamber-specific colors (as per
#the request of reviewer 1)
# Create the histogram plot
hist <- ggplot(skewall, aes(x = Skew)) +  #color = Node)) +
  geom_histogram(bins = 30L, binwidth = .3, size = 1.5) +
  scale_fill_hue(direction = 1) +
  labs(
    x = expression(bold(paste("Metric of skew for ",N[2],"O  flux"))),
    y = "Count of skew values") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20L, face = "bold", color = "black"), 
        axis.title.x = element_text(size = 20L, face = "bold", color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(size= 15, color = "black"),
        legend.title = element_text(size=15, face = "bold", color = "black"), 
        legend.text = element_text(size=15, color = "black"),
        strip.text = element_text(size = 15, color = "black"),
        legend.key = ) +
  facet_wrap(vars(`Time Interval`))
hist

ggsave("metric of skew histogram.pdf",hist,device="pdf",
       path= "/Users/emstuch/Desktop/Reinhart study/Paper drafts/Results/Figures", 
       width=7, height=8, dpi=1000)

#####
#now calculating the mean skew value for each season - useful for the results
#section to be able to talk about which seasons skewed even more right

#renaming the Time Interval column to just Time so it works better with the
#naming convention in the model
skewall2 <- dplyr::rename(skewall, Time = `Time Interval`)
View(skewall2)

#SumStats to calculate the mean
skewmeans <- plyr::ddply(skewall2, c("Time"), summarise,
                            n = length(`Skew`),
                            mean = mean(`Skew`),
                            median = median(`Skew`),
                            sd = sd(`Skew`),
                            se = sd / sqrt(n) )
skewmeans

#now running a quick one-way anova to compare skew means
skewmodel<-lm(Skew~Time, data=skewall2)
anova(skewmodel) 
summary(skewmodel) #no sig differences among any of the means

#do the pairwise comparisons just to confirm, but they should all have the same letter
#in the cld
compskew<-emmeans(skewmodel,pairwise~Time)
compskew

#now setting up compact letter display
cld<-glht(skewmodel, linfct = mcp(Time ="Tukey"))
cld(cld)


#  SKEW VALS COMPARISONS ---------------------------------------
#####
#now reading in the dataset with the time percentages for early, late, 
#non-growing, and whole year time percentages for the five lowest skew values

percentskew<-read_excel("/Users/emstuch/Desktop/Reinhart study/R scripts/Associated Excel Files/finalizeddata_all_data repository.xlsx", sheet = "Skew vs other drivers")
View(percentskew)
str(percentskew)
percentskew$Node<-as.factor(percentskew$Node)
percentskew$Chamber<-as.factor(percentskew$Chamber)
percentskew$Season<-as.factor(percentskew$Season)
percentskew$Strategy<-as.factor(percentskew$Strategy)
str(percentskew)
View(percentskew)

#re-ordering the strategies so plots are in same order as Jiacheng's plots
percentskew$Strategy <- factor(percentskew$Strategy, levels = c("IQR", "iForest", "2SD", "4SD"),
                           labels = c("1.5 x IQR", "Isolation Forest", "2 SD", "4 SD"))
percentskew$Season <- factor(percentskew$Season, levels = c("Early-Growing", "Late-Growing", "Post-Harvest", "Whole Year"),
                         labels = c("Early growing season", "Late growing season", "Non-growing season", "Whole Year"))
View(percentskew)

#re-ordering the dataset so the values with the five smallest skew values 
#are first

percentskew_subset <- percentskew %>%
  arrange(Skew) %>%  # Sort by skew values in ascending order
  slice(1:20) 
View(percentskew_subset)

#####
#Running stats for percentage of time that are hot moments

#now running a  one-way anova to compare the percent of hot time among the 
#different methods, but for the five least skewed values in the dataset
skewmodel_hottimes<-lm(Percentage~Strategy, data=percentskew_subset)
anova(skewmodel_hottimes) 
summary(skewmodel_hottimes) #no sig differences among any of the means

#do the pairwise comparisons to see which method has more time attributed
#to hot moments 
compskew_hottimes<-emmeans(skewmodel_hottimes,pairwise~Strategy)
compskew_hottimes

#now setting up compact letter display
cld<-glht(skewmodel_hottimes, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#re-ordering the dataset so the values with the five largest skew values 
#are first

percentskew_subset2 <- percentskew %>%
  arrange(desc(Skew)) %>%  # Sort by skew values in ascending order
  slice(1:20) 
View(percentskew_subset2)

#now running a  one-way anova to compare the percent of hot time among the 
#different methods, but for the five most skewed values in the dataset
skewmodel_hottimes2<-lm(Percentage~Strategy, data=percentskew_subset2)
anova(skewmodel_hottimes2) 
summary(skewmodel_hottimes2) #no sig differences among any of the means

#do the pairwise comparisons to see which method has more time attributed
#to hot moments 
compskew_hottimes2<-emmeans(skewmodel_hottimes2,pairwise~Strategy)
compskew_hottimes2

#now setting up compact letter display
cld<-glht(skewmodel_hottimes2, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#####
#Running stats for contribution of cumulative flux

#now running a  one-way anova to compare the contribution of cumulative flux
#among the different methods, but for the five least skewed values in the dataset
skewmodel_hottimes<-lm(`Percentage flux contribution`~Strategy, data=percentskew_subset)
anova(skewmodel_hottimes) 
summary(skewmodel_hottimes) #no sig differences among any of the means

#do the pairwise comparisons to see which method has a bigger contrib.
#to the cumulative flux
compskew_hottimes<-emmeans(skewmodel_hottimes,pairwise~Strategy)
compskew_hottimes

#now setting up compact letter display
cld<-glht(skewmodel_hottimes, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#now running a  one-way anova to compare the contribution of cumulative flux
#among the different methods, but for the five most skewed values in the dataset
skewmodel_hottimes2<-lm(`Percentage flux contribution`~Strategy, data=percentskew_subset2)
anova(skewmodel_hottimes2) 
summary(skewmodel_hottimes2) #no sig differences among any of the means

#do the pairwise comparisons to see which method has more time attributed
#to hot moments 
compskew_hottimes2<-emmeans(skewmodel_hottimes2,pairwise~Strategy)
compskew_hottimes2

#now setting up compact letter display
cld<-glht(skewmodel_hottimes2, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#####
#Running stats for thresholds

#now running a  one-way anova to compare the contribution of cumulative flux
#among the different methods, but for the five least skewed values in the dataset
skewmodel_threshold<-lm(`Thresholds`~Strategy, data=percentskew_subset)
anova(skewmodel_threshold) 
summary(skewmodel_threshold) #no sig differences among any of the means

#do the pairwise comparisons to see which method has a bigger contrib.
#to the cumulative flux
compskew_thresholds<-emmeans(skewmodel_threshold,pairwise~Strategy)
compskew_thresholds

#now setting up compact letter display
cld<-glht(skewmodel_threshold, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#now running a  one-way anova to compare the contribution of cumulative flux
#among the different methods, but for the five most skewed values in the dataset
skewmodel_threshold2<-lm(`Thresholds`~Strategy, data=percentskew_subset2)
anova(skewmodel_threshold2) 
summary(skewmodel_threshold2) #no sig differences among any of the means

#do the pairwise comparisons to see which method has a bigger contrib.
#to the cumulative flux
compskew_thresholds2<-emmeans(skewmodel_threshold2,pairwise~Strategy)
compskew_thresholds2

#now setting up compact letter display
cld<-glht(skewmodel_threshold2, linfct = mcp(Strategy ="Tukey"))
cld(cld)

#####
#Now making a multipanel boxplot comparing most skewed vs. least skewed within each method

#first add a column to each dataframe so we can know which skew values are
#low and which are high

# Add a new column "Level of skew" with the value "Low" for each row
percentskew_subset <- percentskew_subset %>%
  mutate(`Level of skew` = "Low")
View(percentskew_subset)

# Add a new column "Level of skew" with the value "High" for each row
percentskew_subset2 <- percentskew_subset2 %>%
  mutate(`Level of skew` = "High")
View(percentskew_subset2)

#Now stitch together the two data frames
percentskew_lowhigh <- rbind(percentskew_subset,percentskew_subset2)
View(percentskew_lowhigh)

#setting up the Dark2 palette so it accommodates monochromatic vision
# Extract the Dark2 palette colors
dark2_colors <- brewer.pal(n = 8, name = "Dark2")

# Manually reorder the colors
# Assuming that the lightest yellow-brown is the 6th color, the dark brown is the 8th, and the grey is the 1st.
# The green and pink can go next as the 5th and 7th colors respectively.
reordered_colors <- c(dark2_colors[6], dark2_colors[7], dark2_colors[8], 
                      dark2_colors[4], dark2_colors[5])

#Now making boxplot for the low and high skew values for hot moment thresholds
threshold <- ggplot(percentskew_lowhigh) +
  aes(x = `Level of skew`, y = `Thresholds`, fill = `Level of skew`) +
  geom_boxplot() +
  scale_fill_manual(values = c("High" = reordered_colors[1], "Low" = reordered_colors[2])) +  # Same color for both High and Low
  theme_classic() +
  labs(y = expression(bold(paste("Threshold")))) +
  theme(
    plot.title = element_text(size = 16L, hjust = 0.5),
    plot.subtitle = element_text(size = 16L,hjust = 0.5, color = "black"),
    plot.caption = element_text(size = 16L, hjust = 0.5, color = "black"),
    axis.title.y = element_text(size = 20L,face = "bold", color = "black"),
    axis.title.x = element_text(size = 20L,face = "bold", color = "black", vjust = 1L),
    axis.text.y = element_text(size = 15L, color = "black"),
    axis.text.x = element_text(size = 15L, angle = 27L,hjust = 1L,vjust = 1L, color = "black"),
    legend.text = element_text(size = 12L, color = "black"),
    legend.title = element_text(size = 12L, color = "black"),
    strip.text = element_text(size = 16, face = "bold", color = "black"),
    plot.margin = unit(c(1, 1, 1, 2.3), "cm")  # Adjust plot margins if needed
  ) +
  facet_wrap(vars(Strategy), scales = "free_y", nrow = 1)
threshold

ggsave("hot moment thresholds_lowvshighskew.pdf",threshold,device="pdf",
       path= "/Users/emstuch/Desktop/Reinhart study/Paper drafts/Results/Figures", 
       width=12, height=5, dpi=1000)

#Now making boxplot for the low and high skew values for hot moment time percentages
hottimes <- ggplot(percentskew_lowhigh) +
  aes(x = `Level of skew`, y = `Percentage hot time`, fill = `Level of skew`) +
  geom_boxplot() +
  scale_fill_manual(values = c("High" = reordered_colors[1], "Low" = reordered_colors[2])) +  # Same color for both High and Low
  theme_classic() +
  labs(y = expression(bold(paste("Time in hot moments (%)")))) +
  theme(
    plot.title = element_text(size = 16L, hjust = 0.5),
    plot.subtitle = element_text(size = 16L,hjust = 0.5, color = "black"),
    plot.caption = element_text(size = 16L, hjust = 0.5, color = "black"),
    axis.title.y = element_text(size = 20L,face = "bold", color = "black"),
    axis.title.x = element_text(size = 20L,face = "bold", color = "black", vjust = 1L),
    axis.text.y = element_text(size = 15L, color = "black"),
    axis.text.x = element_text(size = 15L, angle = 27L,hjust = 1L,vjust = 1L, color = "black"),
    legend.text = element_text(size = 12L, color = "black"),
    legend.title = element_text(size = 12L, color = "black"),
    strip.text = element_text(size = 16, face = "bold", color = "black"),
    plot.margin = unit(c(1, 1, 1, 2.3), "cm")  # Adjust plot margins if needed
  ) +
  facet_wrap(vars(Strategy), scales = "free_y", nrow = 1)
hottimes

ggsave("percent hot times_lowvshighskew.pdf",hottimes,device="pdf",
       path= "/Users/emstuch/Desktop/Reinhart study/Paper drafts/Results/Figures", 
       width=12, height=5, dpi=1000)

#Now making boxplot for the low and high skew values for hot moment thresholds
fluxcontrib <- ggplot(percentskew_lowhigh) +
  aes(x = `Level of skew`, y = `Percentage flux contribution`, fill = `Level of skew`) +
  geom_boxplot() +
  scale_fill_manual(values = c("High" = reordered_colors[1], "Low" = reordered_colors[2])) +  # Same color for both High and Low
  theme_classic() +
  labs(y = expression(bold(paste("Contribution to cumulative flux (%)")))) +
  theme(
    plot.title = element_text(size = 16L, hjust = 0.5),
    plot.subtitle = element_text(size = 16L,hjust = 0.5, color = "black"),
    plot.caption = element_text(size = 16L, hjust = 0.5, color = "black"),
    axis.title.y = element_text(size = 20L,face = "bold", color = "black"),
    axis.title.x = element_text(size = 20L,face = "bold", color = "black", vjust = 1L),
    axis.text.y = element_text(size = 15L, color = "black"),
    axis.text.x = element_text(size = 15L, angle = 27L,hjust = 1L,vjust = 1L, color = "black"),
    legend.text = element_text(size = 12L, color = "black"),
    legend.title = element_text(size = 12L, color = "black"),
    strip.text = element_text(size = 16, face = "bold", color = "black"),
    plot.margin = unit(c(1, 1, 1, 2.3), "cm")  # Adjust plot margins if needed
  ) +
  facet_wrap(vars(Strategy), scales = "free_y", nrow = 1)
fluxcontrib

ggsave("contrib to cumulative flux_lowvshighskew.pdf",fluxcontrib,device="pdf",
       path= "/Users/emstuch/Desktop/Reinhart study/Paper drafts/Results/Figures", 
       width=12, height=5, dpi=1000)


# Frequency dist histograms BY CHAMBER ------------------------------------

#####
#now modifying the Chamber names so they're titled Chamber 1-16, rather
#than Chamber 1-4 - DOING THIS FOR THE N2O FLUX DATASET (n2oflux3)

# First mutate the data frame to remove Chamber 5
n2oflux3_2 <- n2oflux3 %>%
  filter(!(Chamber %in% "Chamber 5"))
View(n2oflux3_2)

# Load the dplyr package
library(dplyr)

# Function to generate new Chamber ID names
rename_chamber <- function(chamber_id) {
  group <- substr(chamber_id, 1, 2)  # Extract group (e.g., "b1", "b2")
  chamber_num <- substr(chamber_id, 4, 4)  # Extract chamber number (e.g., "1", "2")
  paste0("Chamber ", chamber_num)  # Create new name
}

# Mutate the "Chamber ID" column
n2oflux3_2 <- n2oflux3_2 %>%
  mutate(`Chamber ID` = sapply(`Chamber ID`, rename_chamber))

# Check the result
View(n2oflux3_2)

n2oflux3_2_node2 <- n2oflux3_2 %>% filter(Node %in% 
         "Node 2") %>%
  filter(!(`Short Date` == "2022-03-28")) %>%
  filter(!(`Short Date` == "2022-03-23")) %>%
  filter(!(`Short Date` == "2022-03-26")) %>%
  filter(!(`Short Date` == "2022-04-01"))
  
 
View(n2oflux3_2_node2)

n2oflux3_2_node3 <- n2oflux3_2 %>% filter(Node %in% 
                                            "Node 3")
View(n2oflux3_2_node3)

# Now making the histogram 
freqhist <- ggplot(n2oflux3_2) +
  aes(x = `N2O flux`,  fill = "white") +
  geom_histogram(bins = 30, binwidth = 3, color= "black", linewidth = .5) +
  labs(x = expression(bold(paste(N[2], "O flux (nmol m"^-2, "s"^-1, ")"))), 
       y = "Frequency of flux values") +
  scale_fill_identity() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20L, face = "bold", color = "black"), 
        axis.title.x = element_text(size = 20L, face = "bold", color = "black"),
        axis.text = element_text(size = 20, color = "black"),
        axis.text.x = element_text(angle = 50, size = 20, hjust = 1, color = "black"),
        legend.position = "none",  # Remove legend since all bars are the same color
        legend.key = element_blank(),
        strip.text = element_text(size = 20, face = "bold", color = "black")) +  # Adjust facet label text size
  facet_grid(rows = vars(Node), cols = vars(`Chamber ID`)) 
freqhist

ggsave("flux frequency histogram.pdf",freqhist,device="pdf",
       path= "/Users/emstuch/Desktop/Reinhart study/Paper drafts/Results/Figures", 
       width=20, height=10, dpi=1000)

#I just want to see what the histogram will look like if we only include the 
#top 5% of N2O fluxes

# Calculate the 95th percentile of N2O flux
percentile_95 <- quantile(n2oflux3_2$`N2O flux`, probs = 0.95, na.rm = TRUE)

# Filter the dataset to include only values in the top 10% of fluxes
n2oflux3_2_top_5 <- n2oflux3_2 %>%
  filter(`N2O flux` >= percentile_95)

View(n2oflux3_2_top_5)

# Now making the histogram 
freqhist5percent <- ggplot(n2oflux3_2_top_5) +
  aes(x = `N2O flux`,  fill = "white") +
  geom_histogram(bins = 30, binwidth = 3, color= "black", linewidth = .5) +
  labs(x = expression(bold(paste(N[2], "O flux (nmol m"^-2, "s"^-1, ")"))), 
       y = "Frequency of flux values") +
  scale_fill_identity() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20L, face = "bold", color = "black"), 
        axis.title.x = element_text(size = 20L, face = "bold", color = "black"),
        axis.text = element_text(size = 20, color = "black"),
        axis.text.x = element_text(angle = 50, size = 20, hjust = 1, color = "black"),
        legend.position = "none",  # Remove legend since all bars are the same color
        legend.key = element_blank(),
        strip.text = element_text(size = 20, face = "bold", color = "black")) +  # Adjust facet label text size
  facet_grid(rows = vars(Node), cols = vars(`Chamber ID`), scales = c("free_y")) 
freqhist5percent

ggsave("flux frequency histogram top 5 percent.pdf",freqhist5percent,device="pdf",
       path= "/Users/emstuch/Desktop/Reinhart study/Paper drafts/Results/Figures", 
       width=20, height=10, dpi=1000)




