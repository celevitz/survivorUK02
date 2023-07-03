## Carly Levitz
## 2023-07-01
## Purpose: analyze season 2's confessional data


rm(list=ls())

library(ggplot2)
library(tidyverse)
library(openxlsx)
library(showtext)
library(RColorBrewer)
library(ggtext)

font_add("fa-brands", "/Users/carlylevitz/downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solid", "/Users/carlylevitz/downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf")

font_add_google("Barlow", "bar")
showtext_auto()
ft <- "bar"

accent <- brewer.pal(n = 9, name = "PuRd")[9]
bg <- "snow"

twitter <- str_glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- str_glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
reddit <- str_glue("<span style='font-family:fa-brands; color:{accent}'>&#xf1a1;</span>")
floppy <- str_glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- str_glue("<span style='color:{bg};font-size:1px'>'</span>")

textsize <- 8

directory <- "/Users/carlylevitz/Documents/Data/survivor/"
setwd(directory)

## Functions to convert seconds to minutes / hours
## Thanks to Dan Oehm's code!
  to_minSec <- function(seconds) {
    mins <- floor(seconds/60)
    secs <- str_pad(round(seconds - mins*60,0), width = 2, pad = 0)
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
  select(episode,castaway,castaway_id,confessional_count,confessional_time)

confessionals <- confessionals %>%
  # Add on average for each castaway
  full_join(
    confessionals %>%
      group_by(castaway) %>%
      summarise(total = sum(confessional_time)
                ,mean = round(mean(confessional_time),1)) %>%
      pivot_longer(!castaway, names_to = "episode"
                   ,values_to = "confessional_time") %>%
      mutate(episode = case_when(episode == "mean" ~ 13
                                 ,episode == "total" ~ 14) ) ) %>% 
  bind_rows(
    # add on average for each episode
    confessionals %>%
      ungroup() %>% group_by(episode) %>%
      summarise(total = sum(confessional_time)
                ,mean = round(mean(confessional_time),1)) %>%
      pivot_longer(!episode,names_to = "castaway"
                   ,values_to = "confessional_time")
  )  %>%
  mutate(castaway = factor(castaway,levels=c("total","mean"
                                             ,"Sarah","Lee","Tayfun","Meeta"
                                             ,"Helen","Alastair","Drew","Dave"
                                             ,"Bridget","John","Susannah"
                                             ,"Jonny"))
   ,category = factor(case_when(episode %in% c(13,14) ~ "NA"
            ,castaway %in% c("total","mean") ~ "NA"
            ,confessional_time == 0 ~ "0:00"
            ,confessional_time < 30 & confessional_time != 0~ "0:01 to 0:29"
            ,confessional_time >=30 & confessional_time < 60 ~ "0:30 to 0:59"
            ,confessional_time >=60 & confessional_time < 90 ~ "1:00 to 1:29"
            ,confessional_time >=90 & confessional_time < 120 ~ "1:30 to 1:59"
            ,confessional_time >=120 & confessional_time < 180 ~ "2:00 to 2:59"
            ,confessional_time >=180 ~ "3:00 or more")
      ,levels=c("0:00","0:01 to 0:29","0:30 to 0:59","1:00 to 1:29"
                ,"1:30 to 1:59","2:00 to 2:59","3:00 or more","NA") )
      ,labelnames = to_minSec(confessional_time)  
  ) 

  # % share of the confessionals
    share <- confessionals %>%
      select(!(c(confessional_count,category))) %>%
      filter(!(castaway %in% c("total","mean")) & !(episode %in% c(13,14))) %>%
      ungroup() %>% group_by(episode) %>%
      mutate(episodedurationtotal = sum(confessional_time)
             ,percent = confessional_time/episodedurationtotal )
    
    share <- share %>%
      bind_rows(
        share %>%
          ungroup() %>% group_by(episode) %>%
          summarise(sd = round(sd(percent),3)
                    ,mean = round(mean(percent),3)) %>%
          pivot_longer(!episode,names_to = "castaway"
                       ,values_to = "percent")
      ) %>%
      mutate(castaway = factor(castaway,levels=c("sd","mean"
                                               ,"Sarah","Lee","Tayfun","Meeta"
                                               ,"Helen","Alastair","Drew","Dave"
                                               ,"Bridget","John","Susannah"
                                               ,"Jonny"))
             ,category = factor(case_when(castaway %in% c("sd","mean") ~ "NA"
                                  ,percent == 0 ~ "0%"
                                  ,0 < percent & percent < .05 ~ "0.1% to 4.9%"
                                  ,.05 <= percent & percent < .1 ~ "5% to 9.9%"
                                  ,.1 <= percent & percent < .2 ~ "10% to 19.9%"
                                  ,.2 <= percent & percent < .3 ~ "20% to 29.9%"
                                  ,percent >= .3 ~ "30% or greater")
                                ,levels=c("0%","0.1% to 4.9%","5% to 9.9%"
                                          ,"10% to 19.9%","20% to 29.9%"
                                          ,"30% or greater","NA") )
             ,labelnames=ifelse(category == "NA",paste0(percent*100,"%")
                                ,paste0(round(percent*100,1),"%") )
             # to plot the share on the same figure as time, change the x axis
             # which in this case is episode
             ,episode = episode + 15)
    
  # Bring time & share data back together
    alldata <-  confessionals %>%
      mutate(type = "time") %>%
      select(!confessional_count) %>%
      bind_rows(share %>%
                  select(!c(confessional_time,episodedurationtotal)) %>%
                  rename(confessional_time = percent) %>%
                  mutate(type = "percent")) %>%
      mutate(castaway = factor(castaway,levels=c("sd","mean","total"
                                               ,"Sarah","Lee","Tayfun","Meeta"
                                               ,"Helen","Alastair","Drew","Dave"
                                               ,"Bridget","John","Susannah"
                                               ,"Jonny")) )
    
    
## Graph it    
titletext <- str_wrap("Confessional screentime (minutes and % of total) in Survivor UK Panama (Season 2)"
                      ,90)
subtitletext <- str_wrap("John and Susannah had the most confessional screentime. Episode six was unique in that the vast majority of the episode was focused on the individual immunity challenge and there were few confessionals shown.\n\nData gathered using the Confessional Timing app from the survivoR R package (Twitter @danoehm)."
                         ,160)
captiontext <- str_glue("{twitter}{space}@carlylevitz{github}{space}
                      github.com/celevitz/survivorUK02{reddit}{space}
                        u/Consistent-Lion-2125{floppy}{space}
                        CRAN.R-project.org/package=survivoR")
    
alldata %>%
ggplot(aes(x=episode,y=castaway,label=labelnames,fill=category)) +
  geom_tile() +
  geom_text(data = alldata %>% filter(castaway %in% c("mean","sd","total") |
                                      episode %in% c(13,14))
            ,color=brewer.pal(n = 9, name = "PuRd")[9],size=6) +
  geom_text(data = alldata %>% filter(!c(castaway %in% c("mean","sd","total")) &
                                      !c(episode %in% c(13,14)))
            ,color="gray10",size=6) +
  geom_text(data = alldata %>% filter(episode ==27)
            ,color="gray10",size=6) +
  scale_x_continuous(lim=c(0,27),breaks=c(seq(1,14,1),seq(16,27,1))
                   ,labels=c(paste0("Ep",seq(1,12,1)),"mean","total"
                             ,paste0("Ep",seq(1,12,1)))
                   ,position="top") +
  scale_fill_manual(values=c(brewer.pal(n = 7, name = "PuRd"),"snow"
                             ,brewer.pal(n = 6, name = "PuRd"))) +
  xlab("") +
  labs(title=titletext
     ,subtitle=subtitletext
     ,caption = captiontext) +
  theme_minimal() +
  theme(legend.position = "none"
      ,panel.grid = element_blank()
      ,axis.ticks = element_blank()
      ,axis.text.y = element_text(color=brewer.pal(n = 9, name = "PuRd")[9]
                                ,size = 20)
      ,axis.text.x = element_text(color=brewer.pal(n = 9, name = "PuRd")[9]
                                  ,size = 16)
      ,axis.title = element_text(color=brewer.pal(n = 9, name = "PuRd")[9]
                                 ,size = 20,face="bold")
      ,plot.title = element_text(color=brewer.pal(n = 9, name = "PuRd")[9]
                                 ,size = 30,face="bold")
      ,plot.subtitle = element_text(color=brewer.pal(n = 9,name = "PuRd")[9]
                                    ,size=20)
      ,plot.caption = element_markdown(color=brewer.pal(n = 9, name = "PuRd")[9]
                                   ,size=20,hjust=.5)
      ,panel.background = element_rect(color="snow", fill="snow")
      ,plot.background  = element_rect(color="snow", fill="snow")
      ,plot.margin = margin(t=25,r=10,l=10,b=10))




dev.print(png, file = "UK02_ConfessionalData_Duration.png"
          , width = 1600, height = 900)
dev.off()





