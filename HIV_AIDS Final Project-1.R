library(tidyverse)
#Export main data 
data <- read.csv(file = "/Users/laurenkim/Desktop/HIV:AIDS_Final_CSV.csv", stringsAsFactors = FALSE)
data$TOTAL.NUMBER.OF.HIV.DIAGNOSES <- as.numeric(as.character(data$TOTAL.NUMBER.OF.HIV.DIAGNOSES))

#HIV Diagnosis 
#Avg HIV Diagnosis by YEAR
a <- data %>% 
  group_by(YEAR) %>% 
  summarize(mean(TOTAL.NUMBER.OF.HIV.DIAGNOSES,na.rm=TRUE))

#Avg HIV Diagnosis by NEIGHBORHOOD
b <- data %>%
  group_by(NEIGHBORHOOD) %>%
  summarize(mean(TOTAL.NUMBER.OF.HIV.DIAGNOSES,na.rm=TRUE))
b <- b[-c(37),]

#Avg HIV Diagnosis by RACE
c <- data %>% 
  group_by(RACE.ETHNICITY) %>%
  summarize(mean(TOTAL.NUMBER.OF.HIV.DIAGNOSES, na.rm = TRUE))
c <- c[-c(6),]

#Avg HIV Diagnosis by AGE
d <- data %>%
  group_by(AGE) %>%
  summarize(mean(TOTAL.NUMBER.OF.HIV.DIAGNOSES, na.rm = TRUE))
d <- d[-c(7),]

#Plot bar graph of HIV Rate by Race 
graph_HIV_Race <- ggplot(c, aes(x = RACE.ETHNICITY, y = `mean(TOTAL.NUMBER.OF.HIV.DIAGNOSES, na.rm = TRUE)`, fill = RACE.ETHNICITY)) +
  geom_col()+labs(title="The Rate of HIV Diagnoses by Race",y="Avg Number of HIV Diagnoses",x="Race/Ethnicity", 
                  theme_update(plot.title = element_text(hjust = 0.5)))
graph_HIV_Race + theme(axis.text.x= element_text(face = "bold", size = 10))

#Plot bar graph HIV Rate by Age
graph_HIV_Age <- ggplot(d, aes(x = AGE, y = `mean(TOTAL.NUMBER.OF.HIV.DIAGNOSES, na.rm = TRUE)`, fill = AGE)) + 
  geom_col()+labs(title="The Rate of HIV Diagnoses by Age", y="Avg Number of HIV Diagnoses",x="Age",
                  theme_update(plot.title=element_text(hjust = 0.5)))
graph_HIV_Age + theme(axis.text.x = element_text(face = "bold", size = 10))

#Plot HIV Rate by Neighborhood #problems with plotting this 
graph_HIV_Neighborhood <- ggplot(b, aes(x = NEIGHBORHOOD, y = `mean(TOTAL.NUMBER.OF.HIV.DIAGNOSES, na.rm = TRUE)`,
                                        fill = NEIGHBORHOOD)) +
  geom_col() + labs(title = "The Rate of HIV Diagnoses by Neighborhood", y = "Avg Number of HIV Diagnosis", x = "Neighborhood",
                    theme_update(plot.title=element_text(hjust = 0.5)))

#Plot line graph HIV Rate by Year
graph_HIV_Year <- ggplot(a, aes(x = YEAR, y = `mean(TOTAL.NUMBER.OF.HIV.DIAGNOSES, na.rm = TRUE)`, fill = YEAR)) + 
  geom_line()+labs(title = "The Rate of HIV Diagnoses by Year", y = "Avg Number of HIV Diagnoses", x="Year",
                   theme_update(plot.title=element_text(hjust = 0.5)))
graph_HIV_Year + theme(axis.text.x = element_text(face = "bold", size = 14))

#Export NYC Condom Availability Program - HIV condom distribution locations
data_distribution <- read.csv(file = "/Users/laurenkim/Desktop/NYC_Condom_Availability_Program_-_HIV_condom_distribution_locations.csv")

#Categorize the neighborhoods into each borough 
Manhattan <- c("Central Harlem - Morningside Heights", "Chelsea - Clinton",
               "East Harlem", "Gramercy Park - Murray Hill", "Greenwich Village - SoHo",
               "Lower Manhattan","Union Square - Lower Eastside", "Upper Eastside", "Upper Westside",
               "Washington Heights - Inwood")

Brooklyn <- c("Bedford Stuyvesant - Crown Heights", "Bensonhurst - Bay Ridge",
              "Borough Park", "Canarsie - Flatlands", "Coney Island - Sheepshead Bay",
              "East Flatbush - Flatbush", "East New York", "Gramercy Park - Murray Hill",  
              "Sunset Park", "Williamsburg - Bushwick")

Queens <- c("Flushing - Clearview","Fresh Meadows", "Jamaica", "Long Island City - Astoria", 
            "Ridgewood - Forest Hills", "Rockaway", "Southeast Queens", "Southwest Queens", 
            "West Queens")

Bronx <- c("Crotona - Tremont", "Downtown - Heights - Park Slope",
           "Fordham - Bronx Park", "High Bridge - Morrisania",
           "Hunts Point - Mott Haven", "Kingsbridge - Riverdale", 
           "Northeast Bronx", "Pelham - Throgs Neck")

Staten_Island <- c("Port Richmond","South Beach - Tottenville",
                   "Stapleton - St. George", "Willowbrook")

library(dplyr)
#Remove NA variables 
data%>%drop_na

#Find HIV rate by borough 
data <- data %>%
  mutate(borough = ifelse(NEIGHBORHOOD %in% c("Central Harlem - Morningside Heights","Chelsea - Clinton",
                               "East Harlem","Gramercy Park - Murray Hill","Greenwich Village - SoHo",
                               "Lower Manhattan","Union Square - Lower Eastside","Upper Eastside","Upper Westside",
                               "Washington Heights - Inwood"),"Manhattan",
                             ifelse(NEIGHBORHOOD %in% c("Bedford Stuyvesant - Crown Heights","Bensonhurst - Bay Ridge",
                                    "Borough Park","Canarsie - Flatlands","Coney Island - Sheepshead Bay",
                                    "East Flatbush - Flatbush","East New York","Gramercy Park - Murray Hill", 
                                    "Sunset Park","Williamsburg - Bushwick"),"Brooklyn", 
                                    ifelse(NEIGHBORHOOD %in% c("Flushing - Clearview","Fresh Meadows","Jamaica","Long Island City - Astoria", 
                                           "Ridgewood - Forest Hills","Rockaway","Southeast Queens","Southwest Queens",
                                           "West Queens"),"Queens",
                                           ifelse(NEIGHBORHOOD %in% c("Crotona - Tremont","Downtown - Heights - Park Slope",
                                                    "Fordham - Bronx Park","High Bridge - Morrisania",
                                                  "Hunts Point - Mott Haven","Kingsbridge - Riverdale", 
                                                  "Northeast Bronx","Pelham - Throgs Neck"),"Bronx","Staten_Island"
                                                  )))))

data_borough <- data%>%group_by(borough)%>%summarize(sum(TOTAL.NUMBER.OF.HIV.DIAGNOSES,na.rm = TRUE))
colnames(data_borough) <- c("Borough", "Total Number of HIV Diagnosis")

#Find the number of HIV condom distributuion location by borough 
data_distribution$num<-1
data_clinics <- data_distribution%>%group_by(X.8)%>%summarize(sum(num))
data_clinics <- data_clinics[-c(1),]
colnames(data_clinics) <- c("Borough", "Total Number of HIV Clinics")
data_clinics<- data_clinics%>%mutate(`Borough` =c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten_Island"))

#graph the HIV condom distribution location by borough
graph_location <- ggplot(data_clinics, aes(x = Borough, y = `Total Number of HIV Clinics`,
                                           fill = Borough)) +
  geom_col() + labs(title = "The Total Number of HIV Condom Distribution Location", y = "HIV Condom Distribution Location",
                    x = "Borough", theme_update(plot.title=element_text(hjust = 0.5)))

#graph the total number of HIV diagnosis by borough 
graph_total_diagnosis_borough <- ggplot(data_borough, aes(x = Borough, y = `Total Number of HIV Diagnosis`,
                                                          fill = Borough)) + 
  geom_col() + labs(title = "The Total Number of HIV Diagnosis", y = "Tota Number of HIV Diagnosis",
                  x = "Borough", theme_update(plot.title=element_text(hjust = 0.5)))

#Combine data frames of total number of HIV diagnosis and total number of HIV clinics 
data_HIVdiagnosis_clinics <- data_clinics%>%left_join(data_borough,by=c("Borough"))

#linear regression model on Year
#Year
a1 <- lm(data$TOTAL.NUMBER.OF.HIV.DIAGNOSES~data$YEAR)
summary(a1)

#ANOVA test on Neighborhood
b1 <- aov(data$TOTAL.NUMBER.OF.HIV.DIAGNOSES ~ data$NEIGHBORHOOD, data=data)
summary(b1)

#ANOVA test on Age
c1 <- aov(data$TOTAL.NUMBER.OF.HIV.DIAGNOSES ~ data$AGE, data=data)
summary(c1)
data["AGE"]

#ANOVA test on Race
d1 <- aov(data$TOTAL.NUMBER.OF.HIV.DIAGNOSES ~ data$RACE.ETHNICITY, data=data)
summary(d1)
plot(d1)
boxplot(data$RACE.ETHNICITY ~ data$TOTAL.NUMBER.OF.HIV.DIAGNOSES, data=aov.out, frame = FALSE)

#Citations 
citation(package = "dplyr")
citation("tidyverse")
citation("ggplot2")
