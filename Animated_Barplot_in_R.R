##### DATA PREPROCESSING #####

library(tidyverse)
library(janitor)
setwd('C:/Users/jeong/Documents/GITHUB/Animated Bar charts in R')
gdp <- read_csv("GDP_Data.csv")

## select required columns
colnames(gdp)
gdp <- gdp %>% select(3, 4, 7:15) 

gdp <- gdp[1:217,]                                #filter only country rows(국가 단위만 남기고 제거)
gdp_tidy <- gdp %>% 
  mutate_at(vars(contains("YR")),as.numeric) %>%  #YR이 들어간 열을 수치형으로 변환
  gather(year,value,3:11) %>% 
  janitor::clean_names() %>%                      #_, 숫자, 문자만 남기고 모두 제거
  mutate(year = as.numeric(stringr::str_sub(year,1,4)))
write_csv(gdp_tidy, "gdp_tidy.csv")
head(gdp_tidy)


##### REQUIRED LIBRARIES FOR ANIMATED PLOTS #####

library(tidyverse)
library(gganimate)

## 상위 15개의 국가만 보이게 함
gdp_formatted <- gdp_tidy %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value/1e9))) %>%
  group_by(country_name) %>% 
  filter(rank <=15) %>%
  arrange(year, rank) %>% 
  ungroup()
head(gdp_formatted)

staticplot = ggplot(gdp_formatted, 
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
  transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'GDP per Year : {closest_state}',  
       subtitle  =  "Top 15 Countries",
       caption  = "GDP in Billions USD | Data Source: World Bank Data")

# For GIF
animate(anim, 200, fps = 20,  width = 1200, height = 900, 
        renderer = gifski_renderer("gganim.gif"))

