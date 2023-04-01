library(tidyverse)
library(readxl)
library(scales)
library(ggtext)
library(ggthemes)

# read temperature data

temp <- read.csv("G:/R/environmental/global temperature data/Environment_Temperature_change_E_All_Data_NOFLAG.csv",  fileEncoding="latin1")

# call country_code

countrycode <- read.csv("G:/R/environmental/global temperature data/FAOSTAT_data_11-24-2020.csv")


# Data Wrangling and cleaning 

#part - 1

# --- a. to raname column
temp <- rename(temp, Country.Name = Area)

# arrange the temperature data by temp and Months
temp <- arrange(temp, Months)

# create a column with season name 
temp <- mutate(temp, Months = recode(Months, "Dec\u0096Jan\u0096Feb" = 'Winter', "Mar\u0096Apr\u0096May" = 'Spring', "Jun\u0096Jul\u0096Aug" = 'Summer', "Sep\u0096Oct\u0096Nov" = 'Fall'))


# --- b. filter data for certain condition

temp <- filter(temp, Element == "Temperature change")


# --- c. To drop columns from contrycode dataframe

countrycode <- select(countrycode, -Country.Code,
                      -M49.Code,-ISO2.Code,-Start.Year,-End.Year)
countrycode <- rename(countrycode, Country.Name = Country,Country.Code = ISO3.Code)


# --- d. To merge two dataframe

df_temp_country <- left_join(temp, countrycode, by = c("Country.Name" = "Country.Name"))


# --- e. drop data from df_temp_country

df_temp_country <- select(df_temp_country, -Area.Code,-Months.Code,-Element.Code,-Element,-Unit)


# --- f. To reshape dataframe 

df_temp_country <- gather(df_temp_country, year, tem_change, -Country.Code, -Country.Name, -Months)

df_temp_country <- mutate(df_temp_country, year = stringr::str_extract(year, "[0-9]+"))


# --- question:: 1. Which are the top ten countries that have experienced the 
# greatest temperature changes in the last decade?

# --- create copy of orginal dataset

df_c <- df_temp_country %>%
  select(Country.Name, year, tem_change) %>%
  filter(year %in% c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019')) %>%
  group_by(Country.Name) %>%
  summarise(tem_change = mean(tem_change)) %>%
  arrange(desc(tem_change)) %>%
  top_n(10,tem_change)


# now visualise the top 10 country suffer from tempereture

# --- Graph No. 1

top_ten_countries_temp_change <- ggplot(data = df_c, aes(x = reorder(Country.Name, tem_change), y = tem_change, fill = Country.Name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(tem_change, 2)), 
            size = 3, 
            nudge_y = 0.1) +
  
  ggtitle("Top 10 countries/regions that have highest temperature change in the last decades") +
  
  scale_fill_manual(values = c(rep("#a9e0ff", 9), "#ffa1a1"), 
                    guide = F) +
  
  scale_x_discrete(labels = label_wrap(3))+
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 10, 
                                   hjust = 1)) +
  ylab(expression("Temperature change (°C)")) +
  xlab("Countries") +
  labs(caption = "Source: FAOSTAT\n Data Cover Period: 1961–2019") +
  
  coord_flip()

top_ten_countries_temp_change


# --- Question:: 2. 2.What are the ten countries that suffer 
#from temperature change at the very least in the last ten years?

# ------ graph 2

temp_least <- df_temp_country %>% 
  select(c(Country.Name, year, tem_change)) %>% 
   filter(year %in% c(2010:2019)) %>%
    group_by(Country.Name) %>%
     summarize(tem_change = mean(tem_change)) %>%
      arrange(tem_change) %>%
       head(10)


temp_least_graph <- ggplot(data = temp_least, aes(x = reorder(Country.Name, tem_change), y = tem_change, text = tem_change)) +
  
  geom_bar(stat = "identity", width = 0.5, fill = "#a9e0ff") +
  geom_text(aes(label = round(tem_change, 3)), 
            size = 3, 
            nudge_y = 0.015) + 
  
  ggtitle("Top 10 countries/region that have lowest temperature change in the last decades") +
  xlab("Countries") +
  ylab("Temperature change (°C)") +
  labs(caption = "Source: FAOSTAT\n Data Cover Period: 1961–2019") +
  theme_minimal() +
  scale_x_discrete(labels = label_wrap(5))+
  theme(axis.text.x = element_text(hjust = 1)) +
  coord_flip() +
  
  theme(panel.grid.major.y = element_line(color = "transparent"),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          color = "grey80"),
        panel.grid.minor.x = element_line(linetype = "dashed",
                                          color = "grey70"))

temp_least_graph



# --- Question:: 3. 3. Is there any remarkable trend between the years according to World, annex I countries and non-annex I countries? 
#If there is, can we split these as periods?


# --- graph 3

df0 <- temp %>% filter(Months == 'Meteorological year')

#df 1

df1 <- df0 %>% filter(Country.Name == 'World')

df1 <- df1 %>% gather(key = "year1", value = "tem_change1",
                      -c(Area.Code, Country.Name, Months.Code, Months,
                         Element.Code, Element, Unit )) %>%
  select(year1, tem_change1)


# as.character(df1$year1)
df1$year <- c(1961:2019)

#df2

df2 <- df0 %>% filter(Country.Name == 'Annex I countries')

df2 <- df2 %>% gather(key = "year2", value = "tem_change2",
                      -c(Area.Code, Country.Name, Months.Code, Months,
                         Element.Code, Element, Unit )) %>%
  select(year2, tem_change2)

df2$year <- c(1961:2019)

#df 3

df3 <- df0 %>% filter(Country.Name == 'Non-Annex I countries')

df3 <- df3 %>% gather(key = "year3", value = "tem_change3",
                      -c(Area.Code, Country.Name, Months.Code, Months,
                         Element.Code, Element, Unit )) %>%
  select(year3, tem_change3)

df3$year <- c(1961:2019)

# Create the graph
graph_3 <- ggplot() +
  geom_point(data = df1, aes(x = year, y = tem_change1), color = "#7df1ae", size = 1) +
  geom_line(data = df2, aes(x = year, y = tem_change2), color = "#ffa1a1") +
  annotate("text", x = 2014, y = 2.2, label = "The Hottest Record (1.67°C)", 
           color = "red", fontface = "bold", size = 3) +
  geom_line(aes(x = year, y = tem_change3), color = "grey80", data = df3) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1961, 2019, by = 5)) + 
  scale_color_discrete(name = " ",labels = c("Annex I countries", 
                                             "World",
                                             "Non-Annex I countries")) +
  
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank())+
  
  labs(x = 'Year',
       y = "Temperature change (°C)",
       title = "Trend Analysis of Temperature Change by Country Classification:\nWorld, Annex I and Non-Annex I Countries",
       caption = "FAOSTAT\nData Cover Period: 1961-2019")

graph_3 

# -------------------


# --- Qustion:: 4. Is there any significant difference between seasons?

# --- graph 4 

# Create new dataframe with only the columns of interest
df_a <- temp %>% filter(Months == 'Meteorological year')

#df 1

df_a <- temp %>% filter(Country.Name == 'World')

df_b <- df_a %>% gather(key = "year", value = "tem_change",
                        -c(Area.Code, Country.Name, Months.Code, Months,
                           Element.Code, Element, Unit )) 


df_world = df_b %>% filter(Country.Name == 'World')

df_world <- df_world %>% select(Country.Name, year, Months, tem_change)


#winter

winter <- df_world %>% filter(Months == "Winter") %>%
  group_by(year) %>%
  summarise(tem_change = mean(tem_change))

winter$year_w <- c(1961:2019)
min(winter$tem_change)

#spring

spring <- df_world %>% filter(Months == "Spring") %>%
  group_by(year) %>%
  summarise(tem_change = mean(tem_change))

spring$year_sp <- c(1961:2019)

# ---- find the max temperature
max(spring$tem_change)

#summer

summer <- df_world %>% filter(Months == "Summer") %>%
  group_by(year) %>%
  summarise(tem_change = mean(tem_change))

summer$year_s <- c(1961:2019)
max(summer$tem_change)

#fall

fall <- df_world %>% filter(Months == "Fall") %>%
  group_by(year) %>%
  summarise(tem_change = mean(tem_change))

fall$year_f <- c(1961:2019)

max(fall$tem_change)

# ---- seasonal graph 

seasonal_graph <- ggplot() +
  geom_line(data = summer, aes(x = year_s, y = tem_change, ), color = "#ffa1a1") +
  geom_line(data = winter, aes(x = year_w, y = tem_change,
  ), color = "grey80") +
  
  annotate("text", x = 2015, y = 2.5, label = "The Hottest Winter (2.17°C)", 
           color = "blue", fontface = "bold", size = 3) +
  annotate("text", x = 2015, y = 2, label = "The Hottest Spring (2°C)", 
           color = "#eb8c46", fontface = "bold", size = 3) +
  annotate("text", x = 2015, y = 1.5, label = "The Hottest Summer (1.36°C)", 
           color = "#ffa1a1", fontface = "bold", size = 3) +
  annotate("text", x = 2012, y = 1.7, label = "The Hottest Fall (1.47°C)", 
           color = "#7df1ae", fontface = "bold", size = 3) +
  
  geom_point(data = spring, aes(x = year_sp, y = tem_change,
  ),color = "#eb8c46") +
  geom_point(data = fall, aes(x = year_f, y = tem_change,
  ),color = "#7df1ae") +
  scale_x_continuous(breaks = seq(1961, 2019, 5)) +
  
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank())+
  
  labs(x = 'Year',
       y = "Temperature change (°C)",
       title = "Seasonal variation of Temperature Change between 1961 and 2019",
       caption = "FAOSTAT\nData Cover Period: 1961-2019")

seasonal_graph


# --- heat map chart

data_tem_heat <- df_temp_country %>%
  select(Months, year, tem_change) %>%
  filter(Months %in% c('January', 'February', 'March', 'April', 'May', 'June', 
                       'July','August', 'September', 'October', 'November', 'December')) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  group_by(Months, year) %>%
  summarise(temp_mean = mean(tem_change, na.rm = T))

# --- now check the value of month from the dataset data_tem_heat
# --- rename month name to month number

data_tem_heat <- data_tem_heat %>% 
  mutate(month_num = case_when(Months == "January" ~ 1,
                               Months == "February" ~ 2,
                               Months == "March" ~ 3,
                               Months == "April" ~ 4,
                               Months == "May" ~ 5,
                               Months == "June" ~ 6,
                               Months == "July" ~ 7,
                               Months == "August" ~ 8,
                               Months == "September" ~ 9,
                               Months == "October" ~ 10,
                               Months == "November" ~ 11,
                               Months == "December" ~ 12))

# --- factor for month number
data_tem_heat$mon_name <- factor(data_tem_heat$month_num, 
                                 levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))



# Create the heat map 4

graph_heat <- ggplot(data_tem_heat, aes(year, mon_name)) +
  geom_tile(aes(fill = temp_mean)) +
  scale_fill_gradient(low = "white", high = "#de5f6b", "Temperature(°C)",
                      breaks = c(0.2, 0.6, 1, 1.4, 1.8)) +
  theme_minimal() +
  scale_y_discrete(labels = c('January', 'February', 'March', 'April', 'May', 'June', 
                              'July','August', 'September', 'October', 'November', 'December')) +
  scale_x_discrete(breaks = seq(1961, 2019, 5)) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  labs(title = "Global Temperature Changes: A Heat Map of Data from 1961-2019",
       x = "Year",
       y = "Month",
       caption = "FAOSTAT\nData Cover Period: 1961-2019")

graph_heat

#filter years to see the yearly average
f_y <- data_tem_heat %>% 
  filter(year == 1961) %>%
  select(Months, year, temp_mean)

mean(f_y$temp_mean)


# ----- time series plot by continent -------#

# --- no - 5

# ---------------------------------------------#

con_met_y <- temp %>% filter(Months == 'Meteorological year' & Element == 'Temperature change')

# --- ASIA

filter_asia <- con_met_y %>% filter(Country.Name == 'Asia')

filter_asia = filter_asia %>% filter(Months == 'Meteorological year')

filter_asia <- filter_asia %>% gather(key = "year", value = "tem_change",
                                      -c(Area.Code, Country.Name, Months.Code, Months,
                                         Element.Code, Element, Unit )) 

filter_asia <- filter_asia %>% select(Country.Name, year, Months, tem_change, Element)

filter_asia <- filter_asia %>% 
  mutate(year_asia = (1961:2019))



# --- EUROPE

filter_europe <- con_met_y %>% filter(Country.Name == 'Europe')

filter_europe = filter_europe %>% filter(Months == 'Meteorological year')

filter_europe <- filter_europe %>% gather(key = "year", value = "tem_change",
                                          -c(Area.Code, Country.Name, Months.Code, Months,
                                             Element.Code, Element, Unit )) 

filter_europe <- filter_europe %>% select(Country.Name, year, Months, tem_change, Element)

filter_europe <- filter_europe %>% 
  mutate(year_europe = (1961:2019))


# --- africa

filter_afr <- con_met_y %>% filter(Country.Name == 'Africa')

#filter_afr <- filter_afr %>% filter(Months == 'Meteorological year')

filter_afr <- filter_afr %>% gather(key = "year", value = "tem_change",
                                    -c(Area.Code, Country.Name, Months.Code, Months,
                                       Element.Code, Element, Unit )) 

filter_afr <- filter_afr %>% select(Country.Name, year, Months, tem_change, Element)

filter_afr <- filter_afr %>% 
  mutate(year_afr = (1961:2019))


# --- Northern America

filter_na <- con_met_y %>% filter(Country.Name == 'Northern America')

filter_na <- filter_na %>% filter(Months == 'Meteorological year')

filter_na <- filter_na %>% gather(key = "year", value = "tem_change",
                                  -c(Area.Code, Country.Name, Months.Code, Months,
                                     Element.Code, Element, Unit )) 

filter_na <- filter_na %>% select(Country.Name, year, Months, tem_change, Element)

filter_na <- filter_na %>% 
  mutate(year_na = (1961:2019))


# --- South America

filter_sa <- con_met_y %>% filter(Country.Name == 'South America')

filter_sa <- filter_sa %>% filter(Months == 'Meteorological year')

filter_sa <- filter_sa %>% gather(key = "year", value = "tem_change",
                                  -c(Area.Code, Country.Name, Months.Code, Months,
                                     Element.Code, Element, Unit )) 

filter_sa <- filter_sa %>% select(Country.Name, year, Months, tem_change, Element)

filter_sa <- filter_sa %>% 
  mutate(year_sa = (1961:2019))



# --- Oceania

filter_oce <- con_met_y %>% filter(Country.Name == 'Oceania')

#filter_oce <- filter_oce %>% filter(Months == 'Meteorological year')

filter_oce <- filter_oce %>% gather(key = "year", value = "tem_change",
                                    -c(Area.Code, Country.Name, Months.Code, Months,
                                       Element.Code, Element, Unit )) 

filter_oce <- filter_oce %>% select(Country.Name, year, Months, tem_change, Element)

filter_oce <- filter_oce %>% 
  mutate(year_oce = (1961:2019))


# --- Antarctica

filter_ant <- con_met_y %>% filter(Country.Name == 'Antarctica')

#filter_ant <- filter_ant %>% filter(Months == 'Meteorological year')

filter_ant <- filter_ant %>% gather(key = "year", value = "tem_change",
                                    -c(Area.Code, Country.Name, Months.Code, Months,
                                       Element.Code, Element, Unit )) 

filter_ant <- filter_ant %>% select(Country.Name, year, Months, tem_change, Element)

filter_ant <- filter_ant %>% 
  mutate(year_ant = (1961:2019))


# ---- continent temperature change graph 

continent_graph <- ggplot() +
  geom_line(data = filter_asia, aes(x = year_asia, y = tem_change), 
            color = "#ffa1a1") +
  
  geom_line(data = filter_europe, aes(x = year_europe, y = tem_change), 
            color = "grey70") +
  
  geom_line(data = filter_afr, aes(x = year_afr, y = tem_change)
            ,color = "#eb8c46") +
  
  geom_line(data = filter_na, aes(x = year_na, y = tem_change)
            ,color = "#7df1ae") +
  
  geom_line(data = filter_sa, aes(x = year_sa, y = tem_change)
            ,color = "#a174fd") +
  
  geom_line(data = filter_oce, aes(x = year_oce, y = tem_change)
            ,color = "#ffe28a") +
  
  geom_line(data = filter_ant, aes(x = year_ant, y = tem_change)
            ,color = "#c9c9ff") +
  
  scale_x_continuous(breaks = seq(1961, 2019, 5)) +
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank())+
  
  labs(x = 'Year',
       y = "Temperature change (°C)",
       title = "Temperature changes on continents between 1961 and 2019",
       caption = "FAOSTAT\nData Cover Period: 1961-2019")

continent_graph



# --- no --- 6

# --- Mean annual temperature changes measured over the land, global and regional
##  trends by decades

# ------ year : 1961-2000

phase_1 <- con_met_y %>% 
  gather(key = "year", value = "tem_change",
         -c(Area.Code, Country.Name, Months.Code, Months,
            Element.Code, Element, Unit ))


#separate or split text and number for a column

phase_1 <- phase_1 %>%
  separate(year,
           into = c("text", "num"),
           sep = "(?<=[A-Za-z])(?=[0-9])")

# --- change column name num to year

phase_1 <- rename(phase_1, year = num)

# --- filter continent

phase_1 <- phase_1 %>%
  filter(Country.Name %in% c("Asia", "Europe", "Northern America", 
                             "South America", "Oceania", "Antarctica",
                             "Africa") & year >= 1961 & year <2001)%>%
  group_by(Area.Code, Country.Name) %>%
  summarise(avg1961_2000 = mean(tem_change), .groups="keep") %>%
  ungroup()


# ------ year : 2000-2019

phase_2 <- con_met_y %>% 
  gather(key = "year", value = "tem_change",
         -c(Area.Code, Country.Name, Months.Code, Months,
            Element.Code, Element, Unit ))


#separate or split text and number for a column

phase_2 <- phase_2 %>%
  separate(year,
           into = c("text", "num"),
           sep = "(?<=[A-Za-z])(?=[0-9])")

# --- change column name num to year

phase_2 <- rename(phase_2, year = num)

# --- filter continent

phase_2 <- phase_2 %>%
  filter(Country.Name %in% c("Asia", "Europe", "Northern America", 
                             "South America", "Oceania", "Antarctica",
                             "Africa") & year >= 2001 & year <= 2019)%>%
  group_by(Area.Code, Country.Name) %>%
  summarise(avg2001_2019 = mean(tem_change), .groups="keep") %>%
  ungroup()

# join two two phases

phase_1_2 <- full_join(phase_1, phase_2, by=c("Area.Code", "Country.Name")) %>% 
  gather(average, Value, 3:4)


# --- plot decades

# ---- no-6 graph

plot_2_phase <- phase_1_2 %>% mutate(Country.Name.fac = factor(Country.Name, levels=c("Asia", "Africa", "Antarctica",
                                                                                      "Europe", "Northern America", "Oceania", 
                                                                                      "South America"))) %>%
  ggplot(aes(x=Country.Name.fac, y=Value, fill=average)) +
  geom_bar(stat="identity", width=.6, position=position_dodge2()) +
  
  geom_text(aes(label = round(Value,2)),position = position_dodge(.55),
            vjust = -0.25, color = "black") +
  
  theme_gdocs() +
  scale_fill_manual(values=c("#FFD966", "#d34942"), labels = c("1961–2000", "2001–2019")) +
  
  theme(panel.grid.major.y = element_line(size = 0.15, linetype = 'solid', colour = "lightgrey"), 
        panel.grid.major.x = element_blank()) +
  theme(legend.position = c(.1, .95), legend.direction = "horizontal", 
        legend.justification = "left", legend.text = element_text(margin = margin(t = 2), face="bold")) +
  labs(x="", y="Temperature change (°C)") + theme(legend.title = element_blank()) +
  theme(legend.background = element_rect(fill = "transparent")) + theme(plot.background
                                                                        =element_blank()) +
  theme(axis.title.y=element_text(margin = margin(t=0, r=3, b=0, l=-5))) +
  theme(axis.ticks = element_blank(), axis.text = element_text(size = 11)) +
  ### below to avoid labels to overlap
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))


plot_2_phase



