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

font_add_google("Barlow", "bar")
showtext_auto()
ft <- "bar"

textsize <- 8

directory <- "/Users/carlylevitz/Documents/Data/survivor/"
setwd(directory)

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
              ,confessional_time == 0 ~ "0"
              ,confessional_time < 25 ~ "1 to 24"
              ,confessional_time >=25 & confessional_time < 50 ~ "25 to 49"
              ,confessional_time >=50 & confessional_time < 75 ~ "50 to 74"
              ,confessional_time >=75 & confessional_time < 100 ~ "75 to 99"
              ,confessional_time >=100 & confessional_time < 125 ~ "100 to 124"
              ,confessional_time >=125 & confessional_time < 150 ~ "125 to 149"
              ,confessional_time >=150 & confessional_time < 175 ~ "150 to 174"
              ,confessional_time >=175 ~ "175+")
      ,levels=c("0","1 to 24","25 to 49","50 to 74","75 to 99"
            ,"100 to 124","125 to 149","150 to 174","175+"
            ,"NA"))  
  ) 




## heat map of confessional duration by person & episode
titletext <- str_glue("Screentime (minutes) in Survivor UK Panama (Season 2)")
subtitletext <- str_glue("Data gathered using the Confessional Timing app from github.com/doehm/survivoR (Twitter @danoehm)")
captiontext <- str_glue("Visualization and data capture by Twitter @carlylevitz <br> Tools: rstats, ggplot, tidyverse, rcolorbrewer, survivoR /// code github.com/celevitz/survivorUK02")

confessionals %>%
  ggplot(aes(x=episode,y=castaway,fill=category
             ,label=round(confessional_time/60,1))) +
  geom_tile() +
  geom_text(data=confessionals %>% filter(!(episode %in% c(13,14)) &
                                      !(castaway %in% c("total","mean")) &
                                      !(category %in% c("175+","150 to 174"
                                                        ,"125 to 149"
                                                        ,"100 to 124")))
            ,color="gray10",size=8) + 
  geom_text(data=confessionals %>% filter(episode %in% c(13,14) | 
                                      castaway %in% c("total","mean"))
            ,color=brewer.pal(n = 9, name = "PuRd")[9],size=8) + 
  geom_text(data=confessionals %>% filter(category %in% c("175+","150 to 174"
                                                    ,"125 to 149"
                                                    ,"100 to 124"))
            ,color="snow",size=8) +
  scale_x_continuous(lim=c(0,17),breaks=seq(1,14,1)
                     ,labels=c(paste0("Ep",seq(1,12,1)),"mean","total")
                     ,position="top") +
  scale_fill_manual(values=c(brewer.pal(n = 9, name = "PuRd"),"snow")) +
  xlab("") +
  labs(title=titletext
       ,subtitle=subtitletext
       ,caption = captiontext) +
  theme_minimal() +
  theme(legend.position = "none"
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,axis.text = element_text(color=brewer.pal(n = 9, name = "PuRd")[9]
                                      ,size = 20)
        ,axis.title = element_text(color=brewer.pal(n = 9, name = "PuRd")[9]
                                       ,size = 20,face="bold")
        ,plot.title = element_text(color=brewer.pal(n = 9, name = "PuRd")[9]
                                       ,size = 30,face="bold")
        ,plot.subtitle = element_text(color=brewer.pal(n = 9,name = "PuRd")[9]
                                          ,size=20)
        ,plot.caption = element_text(color=brewer.pal(n = 9, name = "PuRd")[9]
                                         ,size=20,hjust=.5)
        ,panel.background = element_rect(color="snow", fill="snow")
        ,plot.background  = element_rect(color="snow", fill="snow")
        ,plot.margin = margin(t=25,r=10,l=10,b=10))

dev.print(png, file = "UK02_ConfessionalData_Duration.png"
          , width = 1200, height = 900)
dev.off()

## 3d. % of episode each castaway had
title3d <- "Castaways' share of each episode's confessionals: 
                Survivor UK Panama"
subtitle3d <- str_glue("Data gathered using the Confessional Timing app from 
                         github.com/doehm/survivoR (Twitter @danoehm)<br><br>
                         Percents are the relative amount of screentime each
                         confessional had in each episode.Each column will
                         add up to 100%.<br><br>The mean is the average percent
                         of the episode's confessional time that castaway had.
                         <br><br>The sd is the standard deviation of the mean; 
                         higher standard deviations mean greater variability
                         across<br>castaways in terms of their screentime.")

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
                                             ,"Nick","JJ","Uzma","Sarah","Jayne"
                                             ,"Adrian","Simon","Andy","James"
                                             ,"Pete","Zoe","Eve","Mick"
                                             ,"Richard","Jackie","Charlotte"))
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
         ,labelnames=ifelse(category == "NA",paste0(percent*100,"%"),paste0(round(percent*100,1),"%") ))

share %>%
  ggplot(aes(x=episode,y=castaway,fill =category
             ,label=labelnames)) +
  geom_tile() +
  geom_text(data=share %>% filter(category %in% 
                                    c("0%","0.1% to 4.9%","5% to 9.9%"))
            ,color="gray10",size=7) +
  geom_text(data=share %>% filter(category %in% 
                                    c("10% to 19.9%","20% to 29.9%"
                                      ,"30% or greater"))
            ,color="snow",size=7) +
  geom_text(data = share %>% filter(category %in% "NA")
            ,color=brewer.pal(n = 9, name = "PuRd")[9],size=7) +
  scale_x_continuous(lim=c(0,15),breaks=seq(1,14,1)
                     ,labels=paste0("EP",seq(1,14,1))
                     ,position="top") +
  scale_fill_manual(values=c(brewer.pal(n = 6, name = "PuRd"),"snow")) +
  xlab("") +
  labs(title=title3d
       ,subtitle=subtitle3d
       ,caption = captiontext) +
  theme_minimal() +
  theme(legend.position = "none"
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,axis.text = element_markdown(color=brewer.pal(n = 9, name = "PuRd")[9]
                                      ,size = 20)
        ,axis.title = element_markdown(color=brewer.pal(n = 9, name = "PuRd")[9]
                                       ,size = 20,face="bold")
        ,plot.title = element_markdown(color=brewer.pal(n = 9, name = "PuRd")[9]
                                       ,size = 30,face="bold"
                                       ,margin=margin(t=5,b=5,l=5,r=5)
                                       ,hjust=.5)
        ,plot.subtitle = element_markdown(color=brewer.pal(n = 9, name = "PuRd")[9]
                                          ,size=20)
        ,plot.caption = element_markdown(color=brewer.pal(n = 9, name = "PuRd")[9]
                                         ,size=20,hjust=.5)
        ,panel.background = element_rect(color="snow", fill="snow")
        ,plot.background  = element_rect(color="snow", fill="snow")
        ,plot.margin = margin(t=25,r=10,l=10,b=10))

dev.print(png, file = paste(savedirectory
                            ,"UK02_ConfessionalData_Share.png",sep="")
          , width = 1200, height = 900)
dev.off()  











