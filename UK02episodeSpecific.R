## Carly Levitz
## 2023-06-30
## Purpose: analyze episode 1 data

rm(list=ls())

episodeofchoice <- 12

library(ggplot2)
library(tidyverse)
library(openxlsx)
library(showtext)

font_add_google("Barlow", "bar")
showtext_auto()
ft <- "bar"

textsize <- 8

directory <- "/Users/carlylevitz/Documents/Data/survivor/"
setwd(directory)


challenges <- read.xlsx("UK02data.xlsx",sheet="Challenge Results") %>%
  filter(episode <= episodeofchoice)
votehx <- read.xlsx("UK02data.xlsx",sheet="Vote History") %>%
  filter(episode <= episodeofchoice)
confessionals <- read.xlsx("UK02data.xlsx",sheet="Confessionals") %>%
  filter(episode <= episodeofchoice) %>%
  mutate(gender = case_when(castaway %in% c("Bridget","Drew","Helen"
                                            ,"Meeta","Sarah","Susannah"
                                            ) ~ "female"
                            ,TRUE ~ "male"))
tribes <- read.xlsx("UK02data.xlsx",sheet="Castaways") %>%
  select(castaway,castaway_id,original_tribe)
bootorder <- read.xlsx("UK02data.xlsx",sheet="Castaways") %>%
  mutate(stillin = case_when(episode <= episodeofchoice ~ "OUT"
                             ,TRUE ~ "IN")) %>%
  select(castaway,castaway_id,result_number,stillin)

## win-loss record through this episode
  winlossT <- challenges %>%
    filter(outcome_type == "Tribal") %>%
    group_by(castaway,castaway_id,result) %>%
    summarise(n=n()) %>%
    pivot_wider(names_from = "result",values_from = n) %>%
    mutate(Won = case_when(is.na(Won) ~ 0
                             ,TRUE ~ Won)
           ,Lost = case_when(is.na(Lost) ~ 0
                            ,TRUE ~ Lost)
           ,Win_LossT = paste0(as.character(Won),"-",as.character(Lost) )
           ,WinPercentT = paste0(round(sum(Won)/sum(Won+Lost)*100,0),"%") ) %>%
    select(castaway,castaway_id,Win_LossT,WinPercentT)
  
  winlossI <- challenges %>%
    filter(outcome_type == "Individual") %>%
    group_by(castaway,castaway_id,result) %>%
    summarise(n=n()) %>%
    pivot_wider(names_from = "result",values_from = n) %>%
    mutate(Won = case_when(is.na(Won) ~ 0
                           ,TRUE ~ Won)
           ,Lost = case_when(is.na(Lost) ~ 0
                             ,TRUE ~ Lost)
           ,Win_LossI = paste0(as.character(Won),"-",as.character(Lost) )
           ,WinPercentI = paste0(round(sum(Won)/sum(Won+Lost)*100,0),"%") ) %>%
    select(castaway,castaway_id,Win_LossI,WinPercentI)
  
  sitouts <- challenges %>%
    group_by(castaway,castaway_id) %>%
    filter(sit_out == TRUE) %>%
    summarise(sitoutcount = n()) 
  
## tribal council records through this episode
  attendance <- votehx %>%
    filter(!(is.na(version))) %>%
    group_by(castaway,castaway_id) %>%
    summarise(TCattended = n()) %>%
    select(castaway,castaway_id,TCattended) 
  
  tcsreceivedvotesat <- votehx %>%
    filter(!(is.na(version)) & !(is.na(vote))) %>%
    group_by(vote,vote_id) %>%
    select(vote,vote_id,episode) %>%
    distinct() %>%
    mutate(receivedvotesat = n()) %>%
    select(vote,vote_id,receivedvotesat) %>%
    rename(castaway = vote,castaway_id = vote_id) %>%
    distinct() 
  
  numbervotesreceived <- votehx %>%
    filter(!(is.na(version)) & !(is.na(vote))) %>%
    group_by(vote,vote_id) %>%
    mutate(votesreceived = n()) %>%
    select(vote,vote_id,votesreceived) %>%
    rename(castaway = vote,castaway_id = vote_id) %>%
    distinct() 
  
  votesthatbootedthem <- votehx %>%
    filter(!(is.na(version)) & !(is.na(vote))) %>%
    filter(vote_id == voted_out_id) %>%
    group_by(vote,vote_id) %>%
    mutate(torchsnuffedvotes = n()) %>%
    select(vote,vote_id,torchsnuffedvotes) %>%
    rename(castaway = vote,castaway_id = vote_id) %>%
    distinct() 
  
  successfulboots <- votehx %>%
    select(version,castaway,castaway_id,episode,order,vote_order
           ,vote_id,voted_out_id,tie) %>%
    # exclude ties and non-votes
    filter(!(is.na(version)) & tie == "FALSE" & !(is.na(vote_id))) %>%
    # how many times have people voted?
    group_by(castaway,castaway_id,episode,order,vote_order) %>%
    mutate(eachtimevotedis1 = n()) %>%
    ungroup() %>%
    group_by(castaway,castaway_id) %>%
    mutate(totalvotes = sum(eachtimevotedis1)) %>%
    # get the successes
    mutate(successful = ifelse(vote_id == voted_out_id,1,0)
           # aggregate successes
           ,successful = sum(successful)
           #get percent success rate
           ,successfulpercent = paste0(round(successful/totalvotes*100,0)
                                       ,"%")) %>%
    select(castaway,castaway_id,successfulpercent) %>%
    distinct()

## Confessionals
  conf <- confessionals %>%
    group_by(castaway,castaway_id) %>%
    summarise(confessional_count = sum(confessional_count)
           ,confessional_time = sum(confessional_time))
  
## Bring it all together
  combined <- tribes %>%
    full_join(bootorder) %>%
    full_join(winlossT) %>%
    full_join(winlossI) %>%
    full_join(sitouts) %>%
    full_join(attendance) %>%
    full_join(numbervotesreceived) %>%
    full_join(tcsreceivedvotesat) %>%
    full_join(votesthatbootedthem) %>%
    full_join(successfulboots) %>%
    full_join(conf)  %>%
    mutate(sitoutcount = ifelse(is.na(sitoutcount),0,sitoutcount)
           ,TCattended = ifelse(is.na(TCattended),0,TCattended)
           ,receivedvotesat = ifelse(is.na(receivedvotesat),0
                                     ,receivedvotesat)
           ,votesreceived = ifelse(is.na(votesreceived),0
                                        ,votesreceived)
           ,torchsnuffedvotes = ifelse(is.na(torchsnuffedvotes),"NA"
                                   ,torchsnuffedvotes)
           ,successfulpercent = ifelse(is.na(successfulpercent)
                                       ,"NA",successfulpercent)) 
  
  combined$castaway <- factor(combined$castaway,
                levels = combined$castaway[order(combined$result_number
                                          ,decreasing = TRUE)])
  
  
  
## Graph it!

  
  combined %>%
    ggplot(aes(x=0,y=0)) +
    geom_text(aes(x=rep(0,12),y=castaway,label=original_tribe,family=ft)
              ,size=6) +
    geom_text(aes(x=rep(10,12),y=castaway,label=Win_LossT,family=ft)
              ,size=textsize) +
    geom_text(aes(x=rep(20,12),y=castaway,label=WinPercentT,family=ft)
              ,size=textsize) +
    geom_text(aes(x=rep(30,12),y=castaway,label=sitoutcount,family=ft)
              ,size=textsize) +
    geom_text(aes(x=rep(40,12),y=castaway,label=Win_LossI,family=ft)
              ,size=textsize) +
    geom_text(aes(x=rep(50,12),y=castaway,label=WinPercentI,family=ft)
              ,size=textsize) +
    geom_text(aes(x=rep(60,12),y=castaway,label=TCattended,family=ft)
              ,size=textsize) +
    geom_text(aes(x=rep(70,12),y=castaway,label=receivedvotesat,family=ft)
              ,size=textsize) +
    geom_text(aes(x=rep(80,12),y=castaway,label=votesreceived,family=ft)
              ,size=textsize) +
    geom_text(aes(x=rep(90,12),y=castaway,label=torchsnuffedvotes,family=ft)
              ,size=textsize) +
    geom_text(aes(x=rep(100,12),y=castaway,label=successfulpercent,family=ft)
              ,size=textsize) +
    geom_text(aes(x=rep(110,12),y=castaway,label=confessional_count,family=ft)
              ,size=textsize) +
    geom_text(aes(x=rep(125,12),y=castaway
                  ,label=as.duration(confessional_time),family=ft)
              ,size=textsize) +
    geom_vline(xintercept=5)+
    geom_vline(xintercept=55)+
    geom_vline(xintercept=105)+
    scale_x_continuous(lim=c(0,130),name="",position="top"
                       ,breaks=c(seq(0,110,10),125)
                       ,labels=c("Starting tribe","Win-Loss\n(Tribal)"
                                 ,"Win %\n(Tribal)","Sit-outs"
                                 ,"Win-Loss\n(Individual)","Win %\n(Individual)"
                                 ,"Tribal Councils\nattended"
                                 ,"TCs at which\nreceived votes"
                                 ,"Votes\nreceived","Votes that\nsent them home"
                                 ,"Successful\nboots (%)","Confessional\ncounts"
                                 ,"Confessional time")) +
    labs(title = "Survivor UK: Panama"
         ,subtitle = "Source: manual data entry and @danoehm's survivoR R package's confessional counter /// Visualization: Twitter @carlylevitz \ncode: github.com/celevitz/survivorUK02 /// Tools: rstats, ggplot2, tidyverse, ggtext, survivoR"
         ,caption = "Helen was eliminated based on a sudden-death trivia contest; Jonny and Susanna were never voted out") +
    theme_minimal() +
    theme(axis.ticks.x=element_blank()
          ,axis.line.x = element_blank() 
          ,axis.text.x = element_text(family = ft,size=18)
          ,axis.text.y = element_text(family = ft,size=18)
          ,axis.title = element_blank()
          ,panel.grid = element_blank()
          ,plot.title = element_text(family=ft,size=30,face="bold")
          ,plot.subtitle = element_text(family = ft,size=18)
          ,plot.caption = element_text(family = ft,size = 18))
  
  
  dev.print(png, file = "SurvivorUK02_Stats.png", width = 1800, height = 900)
  dev.off()
  