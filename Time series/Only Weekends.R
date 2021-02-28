#Creating only weekends dataframe
df_weekends1 <- filter(Bike_Data, !grepl("Mon",Time))
df_weekends2 <- filter(df_weekends1, !grepl("Tue",Time))
df_weekends3 <- filter(df_weekends2, !grepl("Wed",Time))
df_weekends4 <- filter(df_weekends3, !grepl("Thu",Time))
df_weekends5 <- filter(df_weekends4, !grepl("Fri",Time))

#Parsing dates
df_weekends6 <-  parse_date_time(df_weekends5$Time, "%a,%d-%b-%Y-%H:%M")

#convert to dataframe
df_weekends7 <- as.data.frame(df_weekends6)

#a step of data cleaning
df_weekends8 <- select(df_weekends5, 2:27)

# joining two dataframes
df_weekends9 <- data.frame ( df_weekends7, df_weekends8)

#Inserting month column
df_weekends9$month <- month(df_weekends9$df_weekends6)

#Inserting day column
df_weekends9$day <- day(df_weekends9$df_weekends6)

#Inserting hour column
df_weekends9$hour <- hour(df_weekends9$df_weekends6)

#data grouping
df_weekends9.grp <- df_weekends9  %>%  group_by(hour, month)

#Column rename
df_weekends9.grp <- rename(df_weekends9.grp, Dates  = df_weekends6)

#Calculating mean hours for each month in Jannowitzbruecke.Nord
Jannowitzbruecke.Nord.weekends <- df_weekends9.grp %>%  summarize(mean(Jannowitzbruecke.Nord, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Jannowitzbruecke.Nord
Jannowitzbruecke.Nord.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Jannowitzbruecke.Nord bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Jannowitzbruecke.Sued.weekends
Jannowitzbruecke.Sued.weekends <- df_weekends9.grp %>%  summarize(mean(Jannowitzbruecke.Sued, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Jannowitzbruecke.Sued.weekends
Jannowitzbruecke.Sued.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Jannowitzbruecke.Sued.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Invalidenstrasse.Ost
Invalidenstrasse.Ost.weekends <- df_weekends9.grp %>%  summarize(mean(Invalidenstrasse.Ost, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Invalidenstrasse.Ost.weekends
Invalidenstrasse.Ost.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Invalidenstrasse.Ost.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Invalidenstrasse.West
Invalidenstrasse.West.weekends <- df_weekends9.grp %>%  summarize(mean(Invalidenstrasse.West, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Invalidenstrasse.West.weekends
Invalidenstrasse.West.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Invalidenstrasse.West.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Oberbaumbruecke.Ost
Oberbaumbruecke.Ost.weekends <- df_weekends9.grp %>%  summarize(mean(Oberbaumbruecke.Ost, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Oberbaumbruecke.Ost.weekdays
Oberbaumbruecke.Ost.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Oberbaumbruecke.Ost.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Oberbaumbruecke.West
Oberbaumbruecke.West.weekends <- df_weekends9.grp %>%  summarize(mean(Oberbaumbruecke.West, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Oberbaumbruecke.West.weekends
Oberbaumbruecke.West.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Oberbaumbruecke.West.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Frankfurter.Allee.Ost
Frankfurter.Allee.Ost.weekends <- df_weekends9.grp %>%  summarize(mean(Frankfurter.Allee.Ost, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Frankfurter.Allee.Ost.weekends
Frankfurter.Allee.Ost.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Frankfurter.Allee.Ost.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Frankfurter.Allee.West
Frankfurter.Allee.West.weekends <- df_weekends9.grp %>%  summarize(mean(Frankfurter.Allee.West, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Frankfurter.Allee.West.weekends
Frankfurter.Allee.West.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Frankfurter.Allee.West.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Berliner.Strasse.Nord
Berliner.Strasse.Nord.weekends <- df_weekends9.grp %>%  summarize(mean(Berliner.Strasse.Nord, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Berliner.Strasse.Nord.weekends
Berliner.Strasse.Nord.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Berliner.Strasse.Nord.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Berliner.Strasse.Sued
Berliner.Strasse.Sued.weekends <- df_weekends9.grp %>%  summarize(mean(Berliner.Strasse.Sued, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Berliner.Strasse.Sued.weekends
Berliner.Strasse.Sued.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Berliner.Strasse.Sued.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Schwedter.Steg
Schwedter.Steg.weekends <- df_weekends9.grp %>%  summarize(mean(Schwedter.Steg, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Schwedter.Steg.weekends
Schwedter.Steg.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Schwedter.Steg.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Prinzregentenstrasse
Prinzregentenstrasse.weekends <- df_weekends9.grp %>%  summarize(mean(Prinzregentenstrasse, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Prinzregentenstrasse.weekends
Prinzregentenstrasse.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Prinzregentenstrasse.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Klosterstrasse.Nord
Klosterstrasse.Nord.weekends <- df_weekends9.grp %>%  summarize(mean(Klosterstrasse.Nord, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Klosterstrasse.Nord.weekends
Klosterstrasse.Nord.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Klosterstrasse.Nord.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Klosterstrasse.Sued
Klosterstrasse.Sued.weekends <- df_weekends9.grp %>%  summarize(mean(Klosterstrasse.Sued, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Klosterstrasse.Sued.weekends
Klosterstrasse.Sued.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Klosterstrasse.Sued.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Breitenbachplatz.Ost
Breitenbachplatz.Ost.weekends <- df_weekends9.grp %>%  summarize(mean(Breitenbachplatz.Ost, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Breitenbachplatz.Ost.weekends
Breitenbachplatz.Ost.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Breitenbachplatz.Ost.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Breitenbachplatz.West
Breitenbachplatz.West.weekends <- df_weekends9.grp %>%  summarize(mean(Breitenbachplatz.West, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Breitenbachplatz.West.weekends
Breitenbachplatz.West.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Breitenbachplatz.West.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Yorckstrasse.Ost
Yorckstrasse.Ost.weekends <- df_weekends9.grp %>%  summarize(mean(Yorckstrasse.Ost, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Yorckstrasse.Ost.weekends
Yorckstrasse.Ost.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Yorckstrasse.Ost.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Yorckstrasse.West
Yorckstrasse.West.weekends <- df_weekends9.grp %>%  summarize(mean(Yorckstrasse.West, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Yorckstrasse.West.weekends
Yorckstrasse.West.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Yorckstrasse.West.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Monumentenstrasse
Monumentenstrasse.weekends <- df_weekends9.grp %>%  summarize(mean(Monumentenstrasse, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Monumentenstrasse.weekends
Monumentenstrasse.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Monumentenstrasse.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Matiendorfer.Damm.Nord
Matiendorfer.Damm.Nord.weekends <- df_weekends9.grp %>%  summarize(mean(Matiendorfer.Damm.Nord, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Matiendorfer.Damm.Nord.weekends
Matiendorfer.Damm.Nord.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Matiendorfer.Damm.Nord.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Mariendorfer.Damm.Sued
Mariendorfer.Damm.Sued.weekends <- df_weekends9.grp %>%  summarize(mean(Mariendorfer.Damm.Sued, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Mariendorfer.Damm.Sued.weekends
Mariendorfer.Damm.Sued.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Mariendorfer.Damm.Sued.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Maybachufer
Maybachufer.weekends <- df_weekends9.grp %>%  summarize(mean(Maybachufer, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Maybachufer.weekends
Maybachufer.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Maybachufer.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Kaisersteg
Kaisersteg.weekends <- df_weekends9.grp %>%  summarize(mean(Kaisersteg, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Kaisersteg.weekends
Kaisersteg.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Kaisersteg.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Alberichstrasse
Alberichstrasse.weekends <- df_weekends9.grp %>%  summarize(mean(Alberichstrasse, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Alberichstrasse.weekends
Alberichstrasse.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Alberichstrasse.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Paul.und.Paula.Uferwg
Paul.und.Paula.Uferwg.weekends <- df_weekends9.grp %>%  summarize(mean(Paul.und.Paula.Uferwg, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Paul.und.Paula.Uferwg.weekends
Paul.und.Paula.Uferwg.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Paul.und.Paula.Uferwg.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

#Calculating mean hours for each month in Markstrasse
Markstrasse.weekends <- df_weekends9.grp %>%  summarize(mean(Markstrasse, na.rm = TRUE)) %>%  rename(TrafficVolumeAverage = 3)

#Plotting mean hours for each month in Markstrasse.weekends
Markstrasse.weekends %>% ggplot(aes(hour, TrafficVolumeAverage)) +
  geom_step() + 
  facet_wrap(vars(month))+
  labs( title ="Markstrasse.weekends bike traffic volume",
        subtitle = "Hourly average traffic volume in 2019 per each month only in weekends",
        caption = "Correlaid-x-challenge-Berlin")

































