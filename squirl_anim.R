library(tidyverse)
library(lubridate)
library(gganimate)
library(ggrepel)

squirel <- read_csv('./data/nyc_squirrels.csv')
squirel$dateP <- as.POSIXct(squirel$date,,origin="1970-01-01",tz="America/New_York")

squirel$hour <-  lubridate::hour(squirel$dateP)

#ggplot(squirel,aes(hour))+
#  geom_histogram(binwidth=.1)

squirl_state <- squirel %>% 
  transmute(
    state=case_when(
      running==TRUE ~ 'running',
      climbing==TRUE ~ 'climbing',
      chasing==TRUE ~ 'chasing',
      eating==TRUE ~ 'eating',
      foraging==TRUE ~ 'foraging',
      TRUE ~ 'other'))

squirel$state <- squirl_state$state


running_hourly <- squirel %>% 
  select(primary_fur_color,state,hour) %>% 
  filter(!is.na(primary_fur_color)) %>% 
  group_by(hour,primary_fur_color,state) %>% 
  tally()
#summarise(count=count)

#filtering black squirls as we have few data
anim <- ggplot(running_hourly %>% filter(primary_fur_color!='Black'),aes(x='',y=n,fill=state,label=state,order=state))+
  geom_bar(aes(y=n),stat = 'identity',position='fill')+
  facet_wrap(vars(primary_fur_color))+
  coord_polar('y',start = 0) +
  ggplot2::xlab(element_blank())+
  ggplot2::ylab(element_blank())+
  #geom_text_repel(position='fill',force=0,aes(x=1.5,y=2))+
  ggtitle('Hour of day:  {closest_state}')+
  transition_states(hour) +
  theme(strip.text.x = element_text(size = 18),legend.position = 'bottom',
        title=element_text(size=20),
        legend.title=element_text(size=25),
        legend.text=element_text(size=20), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  #  enter_fade() +
  exit_fade() +
  ease_aes('cubic-in-out') + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","tomato2", "papayawhip", "cornsilk3"))


anim




animate(anim, 350, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("nyc-squirel_noblk.gif")) 



animate(anim, 
        nframes = 500, #more frames for make it smoother but longer to render
        fps = 15 #how many frames are shown per second
)
