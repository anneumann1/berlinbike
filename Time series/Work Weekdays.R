#Creating only weekdays dataframe
df_weekdays1 <- filter(Bike_Data, !grepl("Sat",Time))
df_weekdays2 <- filter(df_weekdays1, !grepl("Sun",Time))

#Parsing dates
df_weekdays3 <-  parse_date_time(df_weekdays2$Time, "%a,%d-%b-%Y-%H:%M")

#convert to dataframe
df_weekdays4 <- as.data.frame(df_weekdays3)

#a step of data cleaning
df_weekdays5 <- select(df_weekdays2, 2:27)

# joining two dataframes
df_weekdays6 <- data.frame ( df_weekdays4, df_weekdays5)

#Inserting month column
df_weekdays6$month <- month(df_weekdays6$df_weekdays3)

#Inserting day column
df_weekdays6$day <- day(df_weekdays6$df_weekdays3)

#Inserting hour column
df_weekdays6$hour <- hour(df_weekdays6$df_weekdays3)

#data grouping
df_weekdays6.grp <- df_weekdays6  %>%  group_by(hour, month)

#Column rename
df_weekdays6.grp <- rename(df_weekdays6.grp, Dates  = df_weekdays3)

#Calculating mean hours for each month in Jannowitzbruecke.Nord
Jannowitzbruecke.Nord.weekdays <- df_weekdays6.grp %>%  summarize(mean(Jannowitzbruecke.Nord, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Jannowitzbruecke.Nord
Jannowitzbruecke.Nord.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Jannowitzbruecke.Nord bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Jannowitzbruecke.Sued.weekdays
Jannowitzbruecke.Sued.weekdays <- df_weekdays6.grp %>%  summarize(mean(Jannowitzbruecke.Sued, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Jannowitzbruecke.Sued.weekdays
Jannowitzbruecke.Sued.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Jannowitzbruecke.Sued.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Invalidenstrasse.Ost
Invalidenstrasse.Ost.weekdays <- df_weekdays6.grp %>%  summarize(mean(Invalidenstrasse.Ost, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Invalidenstrasse.Ost.weekdays
Invalidenstrasse.Ost.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Invalidenstrasse.Ost.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")


#Calculating mean hours for each month in Invalidenstrasse.West
Invalidenstrasse.West.weekdays <- df_weekdays6.grp %>%  summarize(mean(Invalidenstrasse.West, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Invalidenstrasse.West.weekdays
Invalidenstrasse.West.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Invalidenstrasse.West.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Oberbaumbruecke.Ost
Oberbaumbruecke.Ost.weekdays <- df_weekdays6.grp %>%  summarize(mean(Oberbaumbruecke.Ost, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Oberbaumbruecke.Ost.weekdays
Oberbaumbruecke.Ost.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Oberbaumbruecke.Ost.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Oberbaumbruecke.West
Oberbaumbruecke.West.weekdays <- df_weekdays6.grp %>%  summarize(mean(Oberbaumbruecke.West, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Oberbaumbruecke.West.weekdays
Oberbaumbruecke.West.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Oberbaumbruecke.West.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Frankfurter.Allee.Ost
Frankfurter.Allee.Ost.weekdays <- df_weekdays6.grp %>%  summarize(mean(Frankfurter.Allee.Ost, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Frankfurter.Allee.Ost.weekdays
Frankfurter.Allee.Ost.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Frankfurter.Allee.Ost.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Frankfurter.Allee.West
Frankfurter.Allee.West.weekdays <- df_weekdays6.grp %>%  summarize(mean(Frankfurter.Allee.West, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Frankfurter.Allee.West.weekdays
Frankfurter.Allee.West.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Frankfurter.Allee.West.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")


#Calculating mean hours for each month in Berliner.Strasse.Nord
Berliner.Strasse.Nord.weekdays <- df_weekdays6.grp %>%  summarize(mean(Berliner.Strasse.Nord, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Berliner.Strasse.Nord.weekdays
Berliner.Strasse.Nord.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Berliner.Strasse.Nord.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Berliner.Strasse.Sued
Berliner.Strasse.Sued.weekdays <- df_weekdays6.grp %>%  summarize(mean(Berliner.Strasse.Sued, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Berliner.Strasse.Sued.weekdays
Berliner.Strasse.Sued.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Berliner.Strasse.Sued.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Schwedter.Steg
Schwedter.Steg.weekdays <- df_weekdays6.grp %>%  summarize(mean(Schwedter.Steg, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Schwedter.Steg.weekdays
Schwedter.Steg.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Schwedter.Steg.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Prinzregentenstrasse
Prinzregentenstrasse.weekdays <- df_weekdays6.grp %>%  summarize(mean(Prinzregentenstrasse, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Prinzregentenstrasse.weekdays
Prinzregentenstrasse.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Prinzregentenstrasse.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Klosterstrasse.Nord
Klosterstrasse.Nord.weekdays <- df_weekdays6.grp %>%  summarize(mean(Klosterstrasse.Nord, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Klosterstrasse.Nord.weekdays
Klosterstrasse.Nord.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Klosterstrasse.Nord.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Klosterstrasse.Sued
Klosterstrasse.Sued.weekdays <- df_weekdays6.grp %>%  summarize(mean(Klosterstrasse.Sued, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Klosterstrasse.Sued.weekdays
Klosterstrasse.Sued.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Klosterstrasse.Sued.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Breitenbachplatz.Ost
Breitenbachplatz.Ost.weekdays <- df_weekdays6.grp %>%  summarize(mean(Breitenbachplatz.Ost, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Breitenbachplatz.Ost.weekdays
Breitenbachplatz.Ost.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Breitenbachplatz.Ost.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Breitenbachplatz.West
Breitenbachplatz.West.weekdays <- df_weekdays6.grp %>%  summarize(mean(Breitenbachplatz.West, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Breitenbachplatz.West.weekdays
Breitenbachplatz.West.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Breitenbachplatz.West.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Yorckstrasse.Ost
Yorckstrasse.Ost.weekdays <- df_weekdays6.grp %>%  summarize(mean(Yorckstrasse.Ost, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Yorckstrasse.Ost.weekdays
Yorckstrasse.Ost.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Yorckstrasse.Ost.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Yorckstrasse.West
Yorckstrasse.West.weekdays <- df_weekdays6.grp %>%  summarize(mean(Yorckstrasse.West, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Yorckstrasse.West.weekdays
Yorckstrasse.West.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Yorckstrasse.West.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Monumentenstrasse
Monumentenstrasse.weekdays <- df_weekdays6.grp %>%  summarize(mean(Monumentenstrasse, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Monumentenstrasse.weekdays
Monumentenstrasse.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Monumentenstrasse.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Matiendorfer.Damm.Nord
Matiendorfer.Damm.Nord.weekdays <- df_weekdays6.grp %>%  summarize(mean(Matiendorfer.Damm.Nord, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Matiendorfer.Damm.Nord.weekdays
Matiendorfer.Damm.Nord.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Matiendorfer.Damm.Nord.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Mariendorfer.Damm.Sued
Mariendorfer.Damm.Sued.weekdays <- df_weekdays6.grp %>%  summarize(mean(Mariendorfer.Damm.Sued, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Mariendorfer.Damm.Sued.weekdays
Mariendorfer.Damm.Sued.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Mariendorfer.Damm.Sued.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")


#Calculating mean hours for each month in Maybachufer
Maybachufer.weekdays <- df_weekdays6.grp %>%  summarize(mean(Maybachufer, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Maybachufer.weekdays
Maybachufer.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Maybachufer.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Kaisersteg
Kaisersteg.weekdays <- df_weekdays6.grp %>%  summarize(mean(Kaisersteg, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Kaisersteg.weekdays
Kaisersteg.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Kaisersteg.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Alberichstrasse
Alberichstrasse.weekdays <- df_weekdays6.grp %>%  summarize(mean(Alberichstrasse, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Alberichstrasse.weekdays
Alberichstrasse.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Alberichstrasse.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Paul.und.Paula.Uferwg
Paul.und.Paula.Uferwg.weekdays <- df_weekdays6.grp %>%  summarize(mean(Paul.und.Paula.Uferwg, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Paul.und.Paula.Uferwg.weekdays
Paul.und.Paula.Uferwg.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Paul.und.Paula.Uferwg.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Markstrasse
Markstrasse.weekdays <- df_weekdays6.grp %>%  summarize(mean(Markstrasse, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Markstrasse.weekdays
Markstrasse.weekdays %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Markstrasse.weekdays bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekdays",
        caption = "Correlaid-x-challenge-Berlin")









#Creating only weekends dataframe
df_Sat <- filter(Bike_Data, grepl("Sat",Time))
df_Sun <- filter(Bike_Data, grepl("Sun",Time))
df_Weekends <- full_join (df_Sat, df_Sun)

