#=====================================================================
# 2020/06/29
# College major segregation (graduates)
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
library(readxl)

######################################################################
# Set directory
######################################################################
setwd("/Users/fumiyau/Dropbox (Princeton)/09.CollegeMajor/CollegeMajorSegregation/2.Data/NameEdited/Graduates")

######################################################################
# lapply for 1986-2019 (except for 1999 and 2005)
######################################################################

All_2015 <- lapply(1:4,function(i){
  lapply(2015:2019,function(j){
    read_excel(paste(j,".xlsx",sep=""), sheet = i,skip=4) %>%
      dplyr::select(2,5,6,23,24) %>% #total graduates and transfers
      mutate(year = j,
             inst = i)
  })
})

#excludes 1999,2005
All_1986 <- lapply(1:4,function(i){
  lapply(c(1986:1998,2000:2004,2006:2014),function(j){
    read_excel(paste(j,".xls",sep=""), sheet = i,skip=4) %>%
      dplyr::select(2,5,6,23,24) %>% 
      mutate(year = j,
             inst = i,
             `男...5`=as.numeric(paste(男...5)),
             `女...6`=as.numeric(paste(女...6)),
             `男...23`=as.numeric(paste(男...23)),
             `女...24`=as.numeric(paste(女...24)))
  })
})

######################################################################
# lapply for 1985, 1999 and 2005
######################################################################
All_1999 <- lapply(1:4,function(i){
  lapply(c(1999,2005),function(j){
    read_excel(paste(j,".xls",sep=""), sheet = i,skip=4) %>%
      dplyr::select(2,4,5,22,23) %>% 
    mutate(year = j,
           inst = i,
           `男...4`=as.numeric(paste(男...4)),
           `女...5`=as.numeric(paste(女...5)),
           `男...22`=as.numeric(paste(男...22)),
           `女...23`=as.numeric(paste(女...23)))
})
})
  
df_all1 <- do.call(bind_rows, c(All_1986,All_1999,All_2015)) %>% 
  filter(`...1`!= "" ) %>% 
  arrange(year)

All_1985 <- lapply(1:4,function(i){
  read_excel(paste("1985.xls",sep=""), sheet = i,skip=3) %>%
    dplyr::select(1,3,4,21,22) %>% 
    mutate(year = 1985,
           inst = i,
           `男...3`=as.numeric(paste(男...3)),
           `女...4`=as.numeric(paste(女...4)),
           `男...21`=as.numeric(paste(男...21)),
           `女...22`=as.numeric(paste(女...22)))
})


######################################################################
# lapply for 1975-1984 
######################################################################

All_1975 <- lapply(1:4,function(i){
  lapply(c(1975:1984),function(j){
    read_excel(paste(j,".xls",sep=""), sheet = i,skip=4) %>%
      dplyr::select(1,3,4,21,22) %>% 
      mutate(year = j,
             inst = i,
             `男...3`=as.numeric(paste(男...3)),
             `女...4`=as.numeric(paste(女...4)),
             `男...21`=as.numeric(paste(男...21)),
             `女...22`=as.numeric(paste(女...22)))
  })
})

#1980-1981でその他が1つ減る（人文社会科学）
#1976-77で医学の欄がなくなっている（6年制別掲は同じ）

#1983/1984で6年制の表番号が8->7になる
#在籍者だと獣医学と畜産学は同じになっているが卒業者では異なる

######################################################################
# Combine
######################################################################
df_all1 <- do.call(bind_rows, c(All_1975,All_1985,All_1986,All_1999,All_2015)) %>% 
  filter(`...1`!= "" ) %>% 
  arrange(year)

######################################################################
# 6年制卒業者
######################################################################

######################################################################
# lapply for 1986-2019 (except for 1999 and 2005)
######################################################################

All_2015m <- lapply(2015:2019,function(j){
    read_excel(paste(j,".xlsx",sep=""), sheet = 7,skip=4) %>%
      dplyr::select(3,6,7,24,25) %>% #total graduates and transfers
      mutate(year = j) %>% 
    filter(`...3`=="医学"|`...3`=="歯学"|`...3`=="薬学"|`...3`=="獣医学畜産学") %>% 
    mutate(inst=data.table::rowid(`...3`)) 
})

#excludes 1999
All_1986m <- lapply(c(1986:1998,2000:2014),function(j){
    read_excel(paste(j,".xls",sep=""), sheet = 7,skip=4) %>%
      dplyr::select(3,6,7,24,25) %>% 
      mutate(year = j,
             `男...6`=as.numeric(paste(男...6)),
             `女...7`=as.numeric(paste(女...7)),
             `男...24`=as.numeric(paste(男...24)),
             `女...25`=as.numeric(paste(女...25))) %>% 
    filter(`...3`=="医学"|`...3`=="歯学"|`...3`=="薬学"|`...3`=="獣医学畜産学"|`...3`=="獣医学"|`...3`=="医学専門学群") %>% 
    mutate(inst=data.table::rowid(`...3`))
})

All_1999m <- read_excel("1999.xls", sheet = 7,skip=4) %>%
      dplyr::select(2,4,5,22,23) %>% 
      mutate(year = 1999,
             `男...4`=as.numeric(paste(男...4)),
             `女...5`=as.numeric(paste(女...5)),
             `男...22`=as.numeric(paste(男...22)),
             `女...23`=as.numeric(paste(女...23))) %>% 
  filter(`...2`=="医学"|`...2`=="歯学"|`...2`=="薬学"|`...2`=="獣医学畜産学"|`...2`=="獣医学"|`...2`=="医学専門学群") %>% 
  mutate(inst=data.table::rowid(`...2`))

All_1985m <- read_excel("1985.xls", sheet = 7,skip=3) %>%
    dplyr::select(2,4,5,22,23) %>% 
    mutate(year = 1985,
           `男...4`=as.numeric(paste(男...4)),
           `女...5`=as.numeric(paste(女...5)),
           `男...22`=as.numeric(paste(男...22)),
           `女...23`=as.numeric(paste(女...23))) %>% 
  filter(`...2`=="医学"|`...2`=="歯学"|`...2`=="薬学"|`...2`=="獣医学畜産学"|`...2`=="獣医学"|`...2`=="医学専門学群") %>% 
  mutate(inst=data.table::rowid(`...2`))

######################################################################
# lapply for 1975-1984 
######################################################################
All_1984m <- read_excel("1984.xls", sheet = 7,skip=4) %>%
  dplyr::select(2,4,5,22,23) %>% 
  mutate(year = 1984,
         `男...4`=as.numeric(paste(男...4)),
         `女...5`=as.numeric(paste(女...5)),
         `男...22`=as.numeric(paste(男...22)),
         `女...23`=as.numeric(paste(女...23))) %>% 
  filter(`...2`=="医学"|`...2`=="歯学"|`...2`=="薬学"|`...2`=="獣医学畜産学"|`...2`=="獣医学"|`...2`=="医学専門学群") %>% 
  mutate(inst=data.table::rowid(`...2`))

All_1980m <- lapply(c(1980:1983),function(j){
    read_excel(paste(j,".xls",sep=""), sheet = 8,skip=4) %>%
      dplyr::select(2,4,5,22,23) %>% 
      mutate(year = j,
             `男...4`=as.numeric(paste(男...4)),
             `女...5`=as.numeric(paste(女...5)),
             `男...22`=as.numeric(paste(男...22)),
             `女...23`=as.numeric(paste(女...23))) %>% 
    filter(`...2`=="医学"|`...2`=="歯学"|`...2`=="医学専門学群"&`...2`!="薬学") %>% #薬学はNAなので除外
    mutate(inst=data.table::rowid(`...2`))
})

All_1975m <- lapply(c(1975:1979),function(j){
  read_excel(paste(j,".xls",sep=""), sheet = 9,skip=4) %>%
    dplyr::select(2,4,5,22,23) %>% 
    mutate(year = j,
           `男...4`=as.numeric(paste(男...4)),
           `女...5`=as.numeric(paste(女...5)),
           `男...22`=as.numeric(paste(男...22)),
           `女...23`=as.numeric(paste(女...23))) %>% 
    filter(`...2`=="医学"|`...2`=="歯学"|`...2`=="医学専門学群") %>% 
    mutate(inst=data.table::rowid(`...2`))
})

df_all2 <- do.call(bind_rows, c(All_1975m,All_1980m,All_1986m,All_2015m)) %>% 
  bind_rows(All_1984m,All_1985m,All_1999m) %>% 
  arrange(year)

######################################################################
# 商船学科卒業者
######################################################################
All_1981s <- lapply(c(1981:1983),function(j){
  read_excel(paste(j,".xls",sep=""), sheet = 7,skip=4) %>%
    dplyr::select(1,3,4,21,22) %>% 
    mutate(year = j,
           `男...3`=as.numeric(paste(男...3)),
           `女...4`=as.numeric(paste(女...4)),
           `男...21`=as.numeric(paste(男...21)),
           `女...22`=as.numeric(paste(女...22))) %>% 
    filter(`...1`=="商船学" | `...1`=="商船－商船学") %>% #薬学はNAなので除外
    mutate(inst=2)
})

All_1980s <- lapply(c(1980),function(j){
  read_excel(paste(j,".xls",sep=""), sheet = 7,skip=4) %>%
    dplyr::select(1,4,5,22,23) %>% 
    mutate(year = j,
           `男...4`=as.numeric(paste(男...4)),
           `女...5`=as.numeric(paste(女...5)),
           `男...22`=as.numeric(paste(男...22)),
           `女...23`=as.numeric(paste(女...23))) %>% 
    filter(`...1`=="商船-商船学" ) %>% #薬学はNAなので除外
    mutate(inst=2)
})

All_1975s <- lapply(c(1975:1979),function(j){
  read_excel(paste(j,".xls",sep=""), sheet = 8,skip=4) %>%
    dplyr::select(1,3,4,21,22) %>% 
    mutate(year = j,
           `男...3`=as.numeric(paste(男...3)),
           `女...4`=as.numeric(paste(女...4)),
           `男...21`=as.numeric(paste(男...21)),
           `女...22`=as.numeric(paste(女...22)),
           inst=2) %>% 
    filter(`...1`=="商船－商船学")
})

#商船
df_all3 <- do.call(bind_rows, c(All_1975s,All_1980s,All_1981s)) %>% 
  arrange(year) %>% 
  mutate(major="商船",
         `男...4`=if_else(is.na(`男...4`)==TRUE,0,`男...4`),
         `男...2`=if_else(is.na(`男...2`)==TRUE,0,`男...2`),
         `女...3`=if_else(is.na(`女...3`)==TRUE,0,`女...3`),
         `女...5`=if_else(is.na(`女...5`)==TRUE,0,`女...5`),
         men=`男...2`-`男...4`,
         women=`女...3`-`女...5`,
         numx=43) %>% 
  dplyr::select(year,inst,major,men,women,numx)

######################################################################
# Merge
######################################################################
df_all <- bind_rows(df_all1,df_all2) %>% 
  mutate(`男...4`=if_else(is.na(`男...4`)==TRUE,0,`男...4`),
         `男...2`=if_else(is.na(`男...2`)==TRUE,0,`男...2`),
         `女...3`=if_else(is.na(`女...3`)==TRUE,0,`女...3`),
         `女...5`=if_else(is.na(`女...5`)==TRUE,0,`女...5`)) %>% 
  mutate(men=`男...2`-`男...4`,
       women=`女...3`-`女...5`) %>% 
  dplyr::select(major=`...1`,men,women,year,inst) %>% 
  arrange(year, decreasing = TRUE) %>% 
  arrange(inst) %>% 
  filter(major != "人文科学" & major != "社会科学" & major != "理学" & major != "工学" &
           major != "保健" & major != "商船" & major != "家政" &
           major != "教育" & major != "芸術" ) %>% 
  filter(!grepl("昭和", major)) %>% 
  filter(!grepl("平成", major)) %>% 
  filter(!grepl("関係学科", major)) %>% 
  filter(is.na(as.numeric(paste(major)))==TRUE) %>% 
  filter(if_else(major=="社会",1,0)==0) %>% 
  filter(!grepl("人文", major)) %>% 
  filter(!grepl("国際関係学部", major)) %>% 
  filter(!grepl("人間関係科学", major)) %>% 
  filter(!grepl("確認用", major)) %>% 
  mutate(other=if_else(major=="その他",1,0)) %>% 
  mutate(num=data.table::rowid(year,inst,other)) %>% 
  filter(if_else(year<1986 & major=="農学"&num==25,1,0)==0) %>% 
  mutate(numx=data.table::rowid(year,inst)) %>% 
  mutate(majorn=paste(numx,major))

df_all_v1<-df_all %>%  #人文科学から水産まで
  filter(numx<38) %>% 
  dplyr::select(year,inst,major,men,women,numx)

#保健＋6年制（獣医学含む）
df_all_v2 <- df_all %>%
  filter(major=="薬学"|major=="医学"|major=="歯学"|major=="看護学"|major=="医学専門学群"|(num == 6 & major == "その他")|major=="獣医学") %>% 
  dplyr::select(year,inst,major,men,women) %>% 
  mutate(numx = case_when(
    major == "医学" ~ 38,
    major == "歯学" ~ 39,
    major == "薬学" ~ 40,
    major == "看護学" ~ 41,
    major == "医学専門学群" ~ 42,
    major == "その他" ~ 42,
    major == "獣医学" ~ 35
  ))

#商船＋家政
df_all_v3 <- df_all %>%
  filter(major=="商船学"|major=="家政学"|major=="食物学"|major=="被服学"|major=="住居学"| major == "児童学") %>% 
  bind_rows(df_all3) %>% 
  dplyr::select(year,inst,major,men,women) %>% 
  mutate(numx = case_when(
    major == "商船" ~ 43,
    major == "商船学" ~ 43,
    major == "家政学" ~ 44,
    major == "食物学" ~ 45,
    major == "被服学" ~ 46,
    major == "住居学" ~ 47,
    major == "児童学" ~ 48
  )) 
  
#教育
df_all_v4b <- df_all %>%
    filter(num ==7 & major=="その他"&year<2008 |(num ==8 & major=="その他"&year>=2008))
  
df_all_v4 <- df_all %>%
  filter(grepl("学校|育|幼稚園課程|特別教科課程|教育", major)) %>% 
  bind_rows(df_all_v4b) %>% 
  dplyr::select(year,inst,major,men,women) %>% 
  mutate(numx = case_when(
    major == "教育学" ~ 49,
    major == "小学校課程" ~ 50,
    major == "中学校課程" ~ 51,
    major == "高等学校課程" ~ 52,
    major == "特別教科課程" ~ 53,
    major == "盲学校課程" ~ 54,
    major == "聾学校課程" ~ 55,
    major == "中等教育学校課程" ~ 59,
    major == "養護学校課程" ~ 56,
    major == "幼稚園課程" ~ 57,
    major == "体育学" ~ 58,
    major == "体育専門学群" ~ 59,
    major == "障害児教育課程" ~ 59,
    major == "障害児童教育課程" ~ 59,
    major == "特別支援教育課程" ~ 59,
    major == "その他" ~ 59
  ))

#芸術 (2008年以降家政にその他ができているので注意)
df_all_v5 <- df_all %>%
  filter(major=="美術"|major=="デザイン"|major=="音楽"|major=="芸術専門学群"|
           (num ==8 & major=="その他"&year<2008)|(num ==9 & major=="その他"&year>=2008)) %>% 
  dplyr::select(year,inst,major,men,women) %>% 
  mutate(numx = case_when(
    major == "美術" ~ 60,
    major == "デザイン" ~ 61,
    major == "音楽" ~ 62,
    major == "芸術専門学群" ~ 63,
    major == "その他" ~ 63
  ))

#その他（教養課程はなくなる）およびその他のその他は元から削除
  
df_all_v6 <- df_all %>%
  filter(grepl("教養学|総合科学", major)) %>% 
  dplyr::select(year,inst,major,men,women) %>% 
  mutate(numx = 64)

#create box for missing merchant
year <- c(rep(1975,3),rep(1981,3),rep(1982,3))
inst <- rep(c(1,3,4),3)

df_all_v7 <- data.frame(year,inst) %>% 
  mutate(men=0,
         women=0,
         numx=43)

df_all_integ <- bind_rows(df_all_v1,df_all_v2,df_all_v3,df_all_v4,df_all_v5,df_all_v6,df_all_v7) %>% 
  dplyr::select(-major) %>% 
  group_by(year,inst,numx) %>% 
  summarise_all(list(mean))

write.csv(df_all_integ, "/Users/fumiyau/Dropbox (Princeton)/09.CollegeMajor/CollegeMajorSegregation/2.Data/NameEdited/df_all_grad.csv")


