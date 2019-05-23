library(dplyr)
library(ggplot2)
library(lubridate)


data <- read.csv(file.choose(), stringsAsFactors = FALSE) 
  nbdata<-data%>%select(Year,Category,Sex) 

nbdata %>% mutate(dummy = if_else(Sex=="Male" ,1,2)) -> nbdata
######### PLOTING THE GENDER IMBALANCE IN THE NOBEL PRIZE DISTRIBUTION IN DIFFERENT FIELDS###########

########PHYSICS###########
phy <- ggplot(filter(nbdata, Category=="Physics"),aes(x = Year ,fill = factor(Sex)))+
  geom_dotplot(binwidth = 1,method = "histodot", stackgroups = TRUE)+
  labs(y = "", x = "Years", title = "Gender Imbaance in Physics Winners", 
       subtitle = "1901-2016")+
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill=guide_legend(title=""))
plot(phy)
############ECONOMICS#####################################
eco <- ggplot(filter(nbdata, Category=="Economics"),aes(x = Year ,fill = factor(Sex)))+
  geom_dotplot(binwidth = 1,method = "histodot", stackgroups = TRUE)+
  labs(y = "", x = "Years", title = "Gender Imbaance in Economics Winners", 
       subtitle = "1901-2016")+
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill=guide_legend(title=""))
plot(eco)
#################PHYSIOLOGY OR MEDICINE##################

med <- ggplot(filter(nbdata, Category=="Medicine"),aes(x = Year ,fill = factor(Sex)))+
  geom_dotplot(binwidth = 1,method = "histodot", stackgroups = TRUE)+
  labs(y = "", x = "Years", title = "Gender Imbaance in Physiology or Medicine Winners", 
       subtitle = "1901-2016")+
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill=guide_legend(title=""))
plot(med)
#######################CHEMISTRY#########################


chem <- ggplot(filter(nbdata, Category=="Chemistry"),aes(x = Year ,fill = factor(Sex)))+
  geom_dotplot(binwidth = 1,method = "histodot", stackgroups = TRUE)+
  labs(y = "", x = "Years", title = "Gender Imbaance in CHEMISTRY Winners", 
       subtitle = "1901-2016")+
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(fill=guide_legend(title=""))
plot(chem)


# Counting the number of (possibly shared) Nobel Prizes handed
# out between 1901 and 2016
data%>% count()

# Counting the number of prizes won by male and female recipients.
data%>%
  group_by(Sex) %>%
  count()

# Counting the number of prizes won by different nationalities.
data %>%
  group_by(Birth.Country) %>%
  count()%>%
  head(20)
View(nbdata)

# Calculating the proportion of India born winners per decade
prop_usa_winners <- data %>% 
  mutate(usa_born_winner= Birth.Country=="India")%>% 
  mutate(decade= floor(Year / 10) * 10)%>% 
  group_by(decade) %>%
  summarize(proportion=
              mean(usa_born_winner, na.rm = TRUE))
prop_usa_winners
# Plotting India born winners
ggplot(prop_usa_winners, aes(x=decade, y=proportion))+geom_line()

# Calculating the proportion of female laureates per decade
prop_female_winners <- data %>%
  mutate(female_winner= Sex=="Female")%>% 
  mutate(decade= floor(Year / 10) * 10)%>%
  group_by(decade,Category) %>%
  summarize(proportion=
              mean(female_winner, na.rm = TRUE)) 

# Plotting the proportion of female laureates per decade

ggplot(prop_female_winners, aes(x=decade, y=proportion, color=Category))+geom_line()
#we see that nobel prize in peace has more female winner.

# First woman to win a Nobel Prize
data %>%
  filter(Sex=="Female")%>%
  top_n(1, desc(Year))%>%select(Year,Full.Name,Category)
# Calculating the age of Nobel Prize winners###########################
x <- as.Date(data$Birth.Date, format = "%Y"); 
nobel_age <-data%>%
  mutate(age=(Year-year(x)))

##################### Plotting the age of Nobel Prize winners###############

ggplot(nobel_age, aes(x=Year, y=age))+geom_point(alpha = 0.6) + geom_smooth(se = FALSE)

#  faceted by the category of the Nobel Prize###############################

ggplot(nobel_age, aes(x=Year, y=age))+geom_point(alpha = 0.6,color='red') +
  geom_smooth(se = FALSE )+
  facet_wrap(~ Category)


##################### The oldest winner of a Nobel Prize as of 2016######################
nobel_age %>% top_n(1, age)%>%select(Year,Full.Name,Category)

# The youngest winner of a Nobel Prize as of 2016#########################
nobel_age %>% top_n(1, desc(age))%>%select(Year,Full.Name,Category)
