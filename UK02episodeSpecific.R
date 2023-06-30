## Carly Levitz
## 2023-06-30
## Purpose: analyze episode 1 data

rm(list=ls())

episodeofchoice <- 2

library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)
library(showtext)

font_add_google("Barlow", "bar")
showtext_auto()
ft <- "bar"

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
  filter(episode <= episodeofchoice) %>%
  select(castaway,castaway_id,result_number)

## win-loss record through this episode
  winloss <- challenges %>%
    group_by(castaway,castaway_id,result) %>%
    summarise(n=n()) %>%
    pivot_wider(names_from = "result",values_from = n) %>%
    mutate(Won = case_when(is.na(Won) ~ 0
                             ,TRUE ~ Won)
           ,Lost = case_when(is.na(Lost) ~ 0
                            ,TRUE ~ Lost)
           ,Win_Loss = paste0(as.character(Won),"-",as.character(Lost) )
           ,WinPercent = paste0(round(sum(Won)/sum(Won+Lost)*100,0),"%") ) %>%
    select(castaway,castaway_id,Win_Loss,WinPercent)
  
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
  
  numbervotesreceived <- votehx %>%
    filter(!(is.na(version))) %>%
    group_by(vote,vote_id) %>%
    mutate(votesreceived = n()) %>%
    select(vote,vote_id,votesreceived) %>%
    rename(castaway = vote,castaway_id = vote_id) %>%
    distinct() 
  
  
  successfulboots <- votehx %>%
    select(version,castaway,castaway_id,episode,order,vote_order
           ,vote_id,voted_out_id,tie) %>%
    # exclude ties
    filter(!(is.na(version)) & tie == "FALSE") %>%
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
    full_join(winloss) %>%
    full_join(sitouts) %>%
    full_join(attendance) %>%
    full_join(numbervotesreceived) %>%
    full_join(successfulboots) %>%
    full_join(conf)  %>%
    mutate(sitoutcount = ifelse(is.na(sitoutcount),0,sitoutcount)
           ,TCattended = ifelse(is.na(TCattended),0,TCattended)
           ,votesreceived = ifelse(is.na(votesreceived),0
                                        ,votesreceived)
           ,successfulpercent = ifelse(is.na(successfulpercent)
                                       ,"NA",successfulpercent)) 
  
  combined$castaway <- factor(combined$castaway,
                levels = combined$castaway[order(is.na(combined$result_number)
                                          ,combined$confessional_time
                                          ,decreasing = FALSE)])
  
  
  
## Graph it!
  
  combined %>%
    ggplot(aes(x=confessional_time,y=castaway,label=castaway)) +
    geom_text() 
  
  