#=====================================================================
# 2020/06/29
# College major segregation
# Fumiya Uchikoshi, uchikoshi@princeton.edu
# Ryota Mugiyama
# Megumi Oguro
#=====================================================================

######################################################################
# Loading packages
######################################################################
library(gdata)
library(tidyverse) 
library(dplyr)
library(ggthemes)
library(knitr)

######################################################################
# Set directory
######################################################################
setwd("/Users/fumiyau/Dropbox (Princeton)/09.CollegeMajor/CollegeMajorSegregation/2.Data/NameEdited/")

######################################################################
# lapply for 1986-2019 (except for 1999)
######################################################################

All_2015 <- lapply(1:4,function(i){
  lapply(2015:2019,function(j){
  read_excel(paste(j,".xlsx",sep=""), sheet = i,skip=4) %>%
    dplyr::select(2,5,6) %>% 
    mutate(year = j,
           inst = i)
    })
    })

All_1986 <- lapply(1:4,function(i){
  lapply(c(1986:1998,2000:2014),function(j){
    read_excel(paste(j,".xls",sep=""), sheet = i,skip=4) %>%
      dplyr::select(2,5,6) %>% 
      mutate(year = j,
             inst = i,
             `男...5`=as.numeric(paste(男...5)),
             `女...6`=as.numeric(paste(女...6)))
    })
    })

df_all1 <- do.call(bind_rows, c(All_1986,All_2015)) %>% 
  filter(`...1`!= "" ) 

######################################################################
# lapply for 1999 and 1985
######################################################################
All_1999 <- lapply(1:4,function(i){
    read_excel(paste("1999.xls",sep=""), sheet = i,skip=4) %>%
      dplyr::select(2,4,5) %>% 
      mutate(year = 1999,
             inst = i,
             `男...4`=as.numeric(paste(男...4)),
             `女...5`=as.numeric(paste(女...5)))
  })

All_1985 <- lapply(1:4,function(i){
  read_excel(paste("1985.xls",sep=""), sheet = i,skip=3) %>%
    dplyr::select(1,3,4) %>% 
    mutate(year = 1985,
           inst = i,
           `男...3`=as.numeric(paste(男...3)),
           `女...4`=as.numeric(paste(女...4)))
})

df_1999 <- do.call(bind_rows, All_1999) %>% 
  filter(`...1`!= "" ) 
df_1985 <- do.call(bind_rows, All_1985) 

######################################################################
# lapply for 1975-1984 
######################################################################

All_1975 <- lapply(1:4,function(i){
  lapply(c(1975:1984),function(j){
    read_excel(paste(j,".xls",sep=""), sheet = i,skip=4) %>%
      dplyr::select(1,3,4) %>% 
      mutate(year = j,
             inst = i,
             `男...3`=as.numeric(paste(男...3)),
             `女...4`=as.numeric(paste(女...4)))
  })
})
df_all2 <- do.call(bind_rows, All_1975) %>% 
  filter(`...1`!= "" ) 

######################################################################
# Combine
######################################################################

df_all <- bind_rows(df_all2,df_1985,df_1999,df_all1) %>%
  arrange(year, decreasing = TRUE) %>% 
  arrange(inst) %>% 
  mutate(major=`...1`) %>% 
  filter(major != "人文科学" & major != "社会科学" & major != "理学" & major != "工学" &
           major != "保健" & major != "商船" & major != "家政" &
           major != "教育" & major != "芸術" ) %>% 
  filter(!grepl("この表", major)) %>% 
  mutate(num=data.table::rowid(year,inst)) %>% 
  filter((year >= 1984 | (year < 1984 & num != 31 & num != 73))) %>% #14020 to 13948
  filter((year >= 1986 | year <= 1983 | (year >= 1984 & year <= 1985 & num != 31 & num != 70))) %>%  # 13948 to 13932
  dplyr::select(major,year,men=`男`,women=`女`,type=inst) %>% 
  mutate(men=if_else(is.na(men)==TRUE,0,men),
         women=if_else(is.na(women)==TRUE,0,women))
  write.csv(df_all, "df_all.csv")
