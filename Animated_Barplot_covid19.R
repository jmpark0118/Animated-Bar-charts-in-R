##### DATA PREPROCESSING #####

library(tidyverse)
library(janitor)
setwd('C:/Users/jeong/Documents/GITHUB/Animated-Bar-charts-in-R')
covid_tidy <- read_csv("data/covid.csv")

## select required columns
colnames(covid_tidy)
colnames(covid_tidy)[3] <- 'value'
colnames(covid_tidy)[2] <- 'country_name'

# covid_tidy <- covid %>% 
#   mutate_at(vars(contains("YR")),as.numeric) %>%  #YR이 들어간 열을 수치형으로 변환
#   gather(year,value,3:11) %>% 
#   janitor::clean_names() %>%                      #_, 숫자, 문자만 남기고 모두 제거
#   mutate(year = as.numeric(stringr::str_sub(year,1,4)))
# write_csv(covid_tidy, "covid_tidy.csv")
# head(covid_tidy)


##### REQUIRED LIBRARIES FOR ANIMATED PLOTS #####

library(tidyverse)
library(gganimate)

## 상위 15개의 국가만 보이게 함
covid_formatted <- covid_tidy %>%
  mutate(weeknumber=as.numeric(strftime(Date, format = "%V")),
         monthno=as.numeric(strftime(Date, format = "%m"))) %>%
  group_by(weeknumber, country_name) %>% 
  select(-Date) %>% 
  summarise(value=sum(value)) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value, ties.method = "random"),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value/1e3))) %>%
  group_by(weeknumber, country_name) %>% 
  filter(rank <=10) %>%
  arrange(weeknumber, rank) %>% 
  ungroup()
covid_formatted <- covid_formatted %>% 
  filter(weeknumber>=10, weeknumber<=45)
head(covid_formatted)

staticplot = ggplot(covid_formatted, 
                    aes(rank, group = country_name, 
                        fill = as.factor(country_name),
                        color = as.factor(country_name))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country_name, " ")), vjust = 0.2, hjust = 1, size = 8) +
  geom_text(aes(y=value, label = Value_lbl), hjust=0, size = 8) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey30" ),
        panel.grid.minor.x = element_line( size=.1, color="grey30" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", color="grey10", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey30"),
        plot.caption =element_text(size=10, hjust=0.5, face="italic", color="grey30"),
        plot.background=element_blank(),
        plot.margin = margin(2, 4, 2, 8, "cm"))

anim = staticplot + 
  transition_states(weeknumber, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Confirmed numbers of Covid 19 per Week : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "Confirmed numbers of Covid19 | Data Source: World Bank Data")

# For GIF
animate(anim, nframes = 800, fps = 20,  width = 1200, height = 900, 
        end_pause = 5,
        renderer = gifski_renderer("gganim_covid19.gif"))

