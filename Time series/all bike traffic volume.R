#Data import
Bike_Data <- read.csv(file="bike_traffic_csv.csv",  stringsAsFactors = FALSE)

#Parsing dates
df1 <-  parse_date_time(Bike_Data$Time, "%a,%d-%b-%Y-%H:%M")

#convert to dataframe
df2 <- as.data.frame(df1)

#a step of data cleaning
df3 <- select(Bike_Data, 2:27)

# joining two dataframes
df4 <- data.frame ( df2, df3)

#Inserting month column
df4$month <- month(df4$df1)

#Inserting day column
df4$day <- day(df4$df1)

#Inserting hour column
df4$hour <- hour(df4$df1)

#data grouping
df4.grp <- df4  %>%  group_by(hour, month)

#Column rename
df4.grp <- rename(df4.grp, Dates  = df1)

#Calculating mean hours for each month in Jannowitzbruecke.Nord
Jannowitzbruecke.Nord <- df4.grp %>%  summarize(mean(Jannowitzbruecke.Nord, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Jannowitzbruecke.Nord
Jannowitzbruecke.Nord %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Jannowitzbruecke.Nord bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Jannowitzbruecke.Sued
Jannowitzbruecke.Sued <- df4.grp %>%  summarize(mean(Jannowitzbruecke.Sued, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Jannowitzbruecke.Sued
Jannowitzbruecke.Sued %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Jannowitzbruecke.Sued bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Invalidenstrasse.Ost
Invalidenstrasse.Ost <- df4.grp %>%  summarize(mean(Invalidenstrasse.Ost , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Invalidenstrasse.Ost
Invalidenstrasse.Ost %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Invalidenstrasse.Ost bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Invalidenstrasse.West
Invalidenstrasse.West <- df4.grp %>%  summarize(mean(Invalidenstrasse.West , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Invalidenstrasse.West
Invalidenstrasse.West %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Invalidenstrasse.West bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Oberbaumbruecke.Ost
Oberbaumbruecke.Ost <- df4.grp %>%  summarize(mean(Oberbaumbruecke.Ost , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Oberbaumbruecke.Ost
Oberbaumbruecke.Ost %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Oberbaumbruecke.Ost bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")


#Calculating mean hours for each month in Oberbaumbruecke.West
Oberbaumbruecke.West <- df4.grp %>%  summarize(mean(Oberbaumbruecke.West , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Oberbaumbruecke.West
Oberbaumbruecke.West %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Oberbaumbruecke.West bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")


#Calculating mean hours for each month in Frankfurter.Allee.Ost
Frankfurter.Allee.Ost <- df4.grp %>%  summarize(mean(Frankfurter.Allee.Ost , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)


#Plotting mean hours for each month in Frankfurter.Allee.Ost
Frankfurter.Allee.Ost %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Frankfurter.Allee.Ost bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Frankfurter.Allee.West
Frankfurter.Allee.West <- df4.grp %>%  summarize(mean(Frankfurter.Allee.West , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Frankfurter.Allee.West
Frankfurter.Allee.West %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Frankfurter.Allee.West bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Berliner.Strasse.Nord
Berliner.Strasse.Nord <- df4.grp %>%  summarize(mean(Berliner.Strasse.Nord , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Berliner.Strasse.Nord
Berliner.Strasse.Nord %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Berliner.Strasse.Nord bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")


#Calculating mean hours for each month in Berliner.Strasse.Sued
Berliner.Strasse.Sued <- df4.grp %>%  summarize(mean(Berliner.Strasse.Sued , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Berliner.Strasse.Sued
Berliner.Strasse.Sued %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Berliner.Strasse.Sued bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Schwedter.Steg
Schwedter.Steg <- df4.grp %>%  summarize(mean(Schwedter.Steg , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Schwedter.Steg
Schwedter.Steg %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Schwedter.Steg bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Prinzregentenstrasse
Prinzregentenstrasse <- df4.grp %>%  summarize(mean(Prinzregentenstrasse , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Prinzregentenstrasse
Prinzregentenstrasse %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Prinzregentenstrasse bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Klosterstrasse.Nord
Klosterstrasse.Nord <- df4.grp %>%  summarize(mean(Klosterstrasse.Nord , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Klosterstrasse.Nord
Klosterstrasse.Nord %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Klosterstrasse.Nord bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Klosterstrasse.Sued
Klosterstrasse.Sued <- df4.grp %>%  summarize(mean(Klosterstrasse.Sued , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Klosterstrasse.Sued
Klosterstrasse.Sued %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Klosterstrasse.Sued bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Breitenbachplatz.Ost
Breitenbachplatz.Ost <- df4.grp %>%  summarize(mean(Breitenbachplatz.Ost , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Breitenbachplatz.Ost
Breitenbachplatz.Ost %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Breitenbachplatz.Ost bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Breitenbachplatz.West
Breitenbachplatz.West <- df4.grp %>%  summarize(mean(Breitenbachplatz.West , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Breitenbachplatz.West
Breitenbachplatz.West %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Breitenbachplatz.West bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Yorckstrasse.Ost
Yorckstrasse.Ost <- df4.grp %>%  summarize(mean(Yorckstrasse.Ost , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Yorckstrasse.Ost
Yorckstrasse.Ost %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Yorckstrasse.Ost bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Yorckstrasse.West
Yorckstrasse.West <- df4.grp %>%  summarize(mean(Yorckstrasse.West , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Yorckstrasse.West
Yorckstrasse.West %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Yorckstrasse.West bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Monumentenstrasse
Monumentenstrasse <- df4.grp %>%  summarize(mean(Monumentenstrasse , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Monumentenstrasse
Monumentenstrasse %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Monumentenstrasse bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Matiendorfer.Damm.Nord
Matiendorfer.Damm.Nord <- df4.grp %>%  summarize(mean(Matiendorfer.Damm.Nord , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Matiendorfer.Damm.Nord
Matiendorfer.Damm.Nord %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Matiendorfer.Damm.Nord bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Mariendorfer.Damm.Sued
Mariendorfer.Damm.Sued <- df4.grp %>%  summarize(mean(Mariendorfer.Damm.Sued , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Mariendorfer.Damm.Sued
Mariendorfer.Damm.Sued %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Mariendorfer.Damm.Sued bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Maybachufer
Maybachufer <- df4.grp %>%  summarize(mean(Maybachufer , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Maybachufer
Maybachufer %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Maybachufer bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Kaisersteg
Kaisersteg <- df4.grp %>%  summarize(mean(Kaisersteg , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Kaisersteg
Kaisersteg %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Kaisersteg bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Alberichstrasse
Alberichstrasse <- df4.grp %>%  summarize(mean(Alberichstrasse , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Alberichstrasse
Alberichstrasse %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Alberichstrasse bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Paul.und.Paula.Uferwg
Paul.und.Paula.Uferwg <- df4.grp %>%  summarize(mean(Paul.und.Paula.Uferwg , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Paul.und.Paula.Uferwg
Paul.und.Paula.Uferwg %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Paul.und.Paula.Uferwg bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Markstrasse
Markstrasse <- df4.grp %>%  summarize(mean(Markstrasse , na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Markstrasse
Markstrasse %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Markstrasse bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month",
        caption = "Correlaid-x-challenge-Berlin")
