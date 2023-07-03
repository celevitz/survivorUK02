## Carly Levitz
## July 3 2023
## Purpose: analyze the confessionals by gender & episode

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(openxlsx)
library(RColorBrewer)
library(ggimage)
library(showtext)
library(ggtext)
library(gghighlight)

font_add("fa-brands", "/Users/carlylevitz/downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solid", "/Users/carlylevitz/downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf")

font_add_google("Barlow", "bar")
showtext_auto()
ft <- "bar"

accent <- "#00CED1"
bg <- "white"

twitter <- str_glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- str_glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
reddit <- str_glue("<span style='font-family:fa-brands; color:{accent}'>&#xf1a1;</span>")
floppy <- str_glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- str_glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- str_glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this

tvimage <- "/Users/carlylevitz/Documents/Data/images/television_emojipedia.png"
faceimage <-"/Users/carlylevitz/Documents/Data/images/neutral-face_emojipedia.png"

textsize <- 8

directory <- "/Users/carlylevitz/Documents/Data/survivor/"
setwd(directory)

## Functions to convert seconds to minutes / hours
## Thanks to Dan Oehm's code!
to_minSec <- function(seconds) {
  mins <- floor(seconds/60)
  secs <- str_pad(seconds - mins*60, width = 2, pad = 0)
  paste0(mins, ":", secs)
}

to_hrMinSec <- function(seconds) {
  hrs <- floor(seconds/3600)
  mins <- floor(seconds/60) - hrs*60
  secs <- seconds - mins*60 - hrs*3600
  mins <- str_pad(mins, width = 2, pad = 0)
  secs <- str_pad(secs, width = 2, pad = 0)
  paste0(hrs, ":", mins, ":", secs)
}

## Bring in the data
confessionals <- read.xlsx("UK02data.xlsx",sheet="Confessionals") %>%
  mutate(gender = case_when(castaway %in% c("Bridget","Drew","Helen"
                                        ,"Meeta","Sarah","Susannah") ~ "female"
                            ,TRUE ~ "male")) %>%
  select(episode,castaway,castaway_id,gender
         ,confessional_count,confessional_time)

##
## Data analysis
epiratio <- 
  # total players in the game
  confessionals %>%
    group_by(episode) %>%
    summarise(N=n()) %>%
  # number of players of each gender in the game
  full_join(confessionals %>%
              group_by(episode,gender) %>%
              summarise(n=n()) %>%
              pivot_wider(names_from = gender,values_from = n) %>%
              rename(female_N = female,male_N = male)) %>%
  # total confessional duration in that episode
  full_join(confessionals %>%
              group_by(episode) %>%
              summarise(total = sum(confessional_time))) %>%
  # total confessional duration by gender in that episode
  full_join(confessionals %>%
              group_by(episode,gender) %>%
              summarise(subtotal=sum(confessional_time)) %>%
              pivot_wider(names_from = gender,values_from = subtotal)) %>%
  # Percent of cast that is female; % of confessionals that were for a woman
  mutate(womencast = female_N/N
         ,womenconf = female/total
         ,total = to_minSec(total)
         ,diff = womencast - womenconf
         ,diffcategory = case_when(diff == 0 ~ 
                               "same representation on cast as in confessionals"
                       ,diff > 0 ~ "women less represented in confessionals"
                       ,diff < 0 ~ "women more representd in confessionals")
         ,tv = "/Users/carlylevitz/Documents/Data/images/television_emojipedia.png"
         ,face="/Users/carlylevitz/Documents/Data/images/neutral-face_emojipedia.png")


## ## ## ##
## Graphing
## ## ## ##
titletext <- str_wrap("In Survivor UK: Panama, women were under-represented in confessionals in seven of 12 episodes"
                      ,60)
subtitletext <- str_wrap("A notable outlier is episode six. The content of this episode was mostly the immunity challenge, so there were few confessionals overall (less than two minutes total).\n\nData were collected using the survivoR confessional counter app (see link below)."
                         ,95)
captiontext <- str_glue("{twitter}{space}@carlylevitz{github}{space}
                      github.com/celevitz/survivorUK02<br>{reddit}{space}
                        u/Consistent-Lion-2125{floppy}{space}
                        CRAN.R-project.org/package=survivoR")

epiratio %>%
  ggplot(aes(x=0,y=episode)) +
  # Add lines for whether women are over or under represented
  geom_rect(data = epiratio %>% filter(diffcategory == 
                                     "women less represented in confessionals")
            ,aes(xmin=womenconf,xmax=womencast,ymin=episode,ymax=episode)
            ,color="#ffb129") +
  geom_rect(data = epiratio %>% filter(diffcategory == 
                                       "women more representd in confessionals")
            ,aes(xmin=womencast,xmax=womenconf,ymin=episode,ymax=episode)
            ,color="#84ccff") +
  # Add images for the two types of data
  geom_image(aes(x=womencast,y=episode,image=face)) +
  geom_image(aes(x=womenconf,y=episode,image=tv)) +
  # Add a table of pertinent information
  geom_text(aes(x=rep(1.05,12),y=episode,label=N),hjust=.5,size=5
            ,family = ft,color="gray25") +
  geom_text(aes(x=rep(1.15,12),y=episode,label=total),hjust=.5,size=5
            ,family = ft,color="gray25") +
  annotate("text",x=1.05,y=13,label="# of\ncastaways",hjust=.5,size=5
            ,family = ft,color="gray25") +
  annotate("text",x=1.15,y=13,label="confessional\ntime",hjust=.5,size=5
            ,family = ft,color="gray25") +
  # legend
  geom_image(aes(x=0,y=13,image=tvimage))+
  annotate("text",x=0.05,y=13,hjust=0
           ,label="% of episode's confessional time by women") +
  geom_image(aes(x=0.5,y=13,image=faceimage))+
  annotate("text",x=0.55,y=13,hjust=0
           ,label="% of castaways in each episode that were women") +
  # Format
  scale_x_continuous(lim = c(0,1.2),breaks=seq(0,1,.1)
                     ,labels=paste0(seq(0,1,.1)*100,"%")) +
  scale_y_continuous(lim = c(0,13),breaks=seq(1,12,1),labels=seq(1,12,1)) +
  xlab("Percent") +
  labs(title=titletext
       ,subtitle=subtitletext
       ,caption = captiontext) +
  theme_minimal() +
  theme(legend.position = "none"
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()#element_line(color="gray25")
        ,axis.line = element_blank()#element_line(color="gray25")
        ,axis.text = element_text(color="gray25",size = 20)
        ,axis.title = element_text(color="gray25",size = 20
                                   ,face="bold",family = ft)
        ,plot.title = element_text(color="gray25",size = 30
                                   ,face="bold",family = ft)
        ,plot.subtitle = element_text(color="gray25",size=20,family = ft)
        ,plot.caption = element_markdown(color="gray25" ,size=20,hjust=.5
                                         ,family = ft)
        ,plot.margin = margin(t=25,r=10,l=10,b=10))



dev.print(png, file = "UK02_ConfessionalRatioByGender.png"
          , width = 900, height = 900)
dev.off()



