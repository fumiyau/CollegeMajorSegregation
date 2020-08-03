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
# Assign colors
######################################################################
cbp1 <- c("#A6CEE3", "#E0E0E0", "#E6AB02", "#1F78B4",
          "#878787", "#A6761D")
cbp2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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

######################################################################
# Data binding & labeling
######################################################################
label <- read_csv("../../Labeling.csv") %>% 
  mutate(major=factor(major,levels=major))

df_all_integ <- bind_rows(df_all_v1,df_all_v2,df_all_v3,df_all_v4,df_all_v5,df_all_v6,df_all_v7) %>% 
  dplyr::select(-major) %>% 
  mutate(inst=case_when(
    inst == 1 ~ "Total",
    inst == 2 ~ "National & Public",
    inst == 3 ~ "National & Public",
    inst == 4 ~ "Private")) %>% 
  group_by(year,inst,numx) %>% 
  summarise_all(list(sum)) %>% 
  left_join(label) %>% 
  mutate(
    stem=if_else(numx==27,0,stem), ###change craft into non-STEM
    stem = case_when(
      stem == 0 ~ "Non-STEM",
      stem == 1 ~ "STEM",
      stem == 2 ~ "Health"
    ),
    stem = factor(stem,levels=c("Non-STEM","STEM","Health")),
    inst = factor(inst,levels=c("Total","National & Public","Private"))
  )

#write.csv(df_all_integ, "/Users/fumiyau/Dropbox (Princeton)/09.CollegeMajor/CollegeMajorSegregation/2.Data/NameEdited/df_all_grad.csv")

######################################################################
# Get proportion
######################################################################
df_all_integ %>% 
  filter(year==1975 | year==1990| year == 2005 | year == 2019) %>% 
  mutate(total=men+women) %>% 
  select(inst,year,stem,total) %>% 
  group_by(year,inst,stem) %>% 
  summarise_all(list(sum)) %>% 
  mutate(sum=sum(total),
         row=100*total/sum) %>% 
  select(year,inst,stem,row) %>% 
  arrange(inst,year) %>% 
  pivot_wider(names_from = "stem", 
              values_from = "row") %>% 
write.csv("/Users/fumiyau/Dropbox (Princeton)/09.CollegeMajor/CollegeMajorSegregation/2.Data/NameEdited/rowprop.csv")

######################################################################
# Get distribution for Appendix 1
######################################################################
df_cont <- df_all_integ %>% 
  group_by(year,inst) %>% 
  mutate(mensum=sum(men),
         womensum=sum(women),
         contMitMt=men/mensum,
         contFitFt=women/womensum,
         scorex=100*(1/2)*(abs(contMitMt-contFitFt)),
         score=sum(scorex))

df_cont %>% filter(year==1975 | year == 2019) %>% 
  dplyr::select(year,inst,major,contMitMt,contFitFt) %>% 
  pivot_wider(names_from = c("inst","year"), 
              values_from = c(contMitMt,contFitFt)) %>% 
  write.csv("../../../3.Results/appendix1.csv")

df_cont %>% filter(year==1975 | year == 2019) %>% 
  filter(major == "Literature") %>% 
  dplyr::select(year,inst,major,mensum,womensum) %>% 
  pivot_wider(names_from = c("inst"), 
              values_from = c(mensum,womensum))

######################################################################
# Get distribution for Appendix 2
######################################################################
df_cont %>% filter(year==1975 | year ==1990 | year ==2005 | year == 2019) %>% 
  dplyr::select(year,inst,major,scorex) %>% 
  pivot_wider(names_from = c("inst","year"), 
              values_from = c(scorex)) %>% 
  write.csv("../../../3.Results/appendix2.csv")

######################################################################
# Pick up major contributors
######################################################################
df_cont_rel <- df_cont %>% 
  filter(inst=="Total" & (year==1975 | year==2019)) %>% 
  arrange(year,inst,scorex) %>% 
  group_by(year) %>% 
  mutate(score_order=cumsum(scorex),
         score_rel_order=as.numeric(score_order/score)) %>% 
  filter(score_rel_order>(1/3))
unique(df_cont_rel$major)

df_cont_rel1975 <-  df_cont_rel %>% 
  filter(year==1975) %>% 
  ungroup() %>% 
  dplyr::select(numx)%>% mutate(flag1=1)

df_cont_rel2019 <-  df_cont_rel %>% 
  filter(year==2019) %>% 
  ungroup() %>% 
  dplyr::select(numx)%>% mutate(flag2=1)

df_cont_long <- df_cont %>% 
  dplyr::select(year,inst,numx,major,contMitMt,contFitFt) %>% 
  pivot_longer(
    cols = starts_with("cont"),
    names_to = "sex",
    names_prefix = "cont",
    values_to = "value",
    values_drop_na = TRUE
  )

df_cont_longx <- df_cont_long %>% 
  left_join(df_cont_rel1975) %>% 
  left_join(df_cont_rel2019) %>% 
  filter(flag1==1 | flag2==1) %>% 
  filter((year == 2019 | year ==1975)) 
unique(df_cont_longx$major)

#year <- c(1975, 2019)
#flag <- c(1,1)
#flags <- tibble(year, flag)

for (i in c("Total","National & Public","Private")){
  df_cont_longx <- df_cont_long %>% 
    #   left_join(flags, by = "year") %>% 
    left_join(df_cont_rel1975) %>% 
    left_join(df_cont_rel2019) %>% 
    filter(flag1==1 | flag2==1) %>% 
    #    mutate(score_label = as.numeric(round(flag * value,2))) %>% 
    #    mutate(score_point = round(flag * value,2)) %>% 
    filter(inst==i) 
  ggplot(df_cont_longx,aes(x=year, y= value, group=sex)) + 
    geom_line(aes(linetype=sex)) + 
#    geom_point(aes(y = score_point, x = year, group=sex)) + 
#    geom_text(vjust = "inward", hjust = "inward") + 
    theme_few()+xlab("")+ylab("")+theme(legend.title=element_blank())+
    theme(axis.text.x=element_text(angle=60,vjust=1,hjust=1))+
    facet_wrap(~major)+
    xlim(1975, 2020)+ ylim(0,0.4)
  ggsave(height=6,width=12,dpi=200, filename= paste("../../../3.Results/Top13_",i,"_grads.pdf",sep=""),  family = "Helvetica")
}


######################################################################
# Excluding others for sensitivity analysis
######################################################################

df_cont_rist <- df_all_integ %>% 
  filter(numx !=4 & numx != 8 & numx != 14 & numx != 28 & 
           numx != 37 & numx != 42 & numx != 59 & numx != 63) %>% 
  group_by(year,inst) %>% 
  mutate(mensum=sum(men),
         womensum=sum(women),
         contMitMt=men/mensum,
         contFitFt=women/womensum,
         scorex=100*(1/2)*(abs(contMitMt-contFitFt)),
         score=sum(scorex)) %>% 
  filter(year== 1975 | year == 2019)

######################################################################
# Descriptive trends
######################################################################

#################
#### overall ####
#################
year <- c(1975, 1990, 2005, 2019)
flag <- c(1,1,1,1)
flags <- tibble(year, flag)
df_cont %>% 
  filter(numx==1) %>% 
  left_join(flags, by = "year") %>% 
  mutate(score_label = as.numeric(round(flag * score,1))) %>% 
  mutate(score_point = round(flag * score,1)) %>% 
  ggplot(aes(x=year, y= score, label = as.factor(score_label))) + geom_line() + 
  geom_point(aes(y = score_point, x = year)) + 
  geom_text(vjust = 1.5, hjust = 1) + 
  xlim(1970, 2020) + ylim(0,100) +
  theme_few()+xlab("")+ylab("")+theme(legend.title=element_blank(), 
                                      legend.position = "none")+
  facet_wrap(~inst, ncol = 3)

#df_cont %>% 
#  filter(numx==1) %>% 
#  ggplot(aes(x=year, y= score, group=inst, color=inst)) + geom_line(aes(linetype=inst)) +
#  theme_few()+xlab("")+ylab("")+theme(legend.title=element_blank())+
#  scale_color_manual(values=cbp2)
ggsave(height=4,width=8,dpi=200, filename="../../../3.Results/Overal_inst_grads.pdf",  family = "Helvetica")

table2 <- df_all_integ %>% 
  group_by(year,inst) %>% 
  mutate(mensum=sum(men),
         womensum=sum(women),
         contMitMt=men/mensum,
         contFitFt=women/womensum,
         scorex=100*(1/2)*(abs(contMitMt-contFitFt))) %>% 
  group_by(year,inst,stem) %>% 
  mutate(score=sum(scorex)) %>% #summarise by stem
  filter(major=="Literature" | major == "Mathematics" | major == "Medicine") %>% 
  filter(year==1975 | year == 1990 | year == 2005 | year == 2019) %>% 
  dplyr::select(year,inst,stem,score) %>% 
  pivot_wider(names_from = c("stem"), 
              values_from = score) %>% 
  arrange(inst,year)

write.csv(table2, "../../../3.Results/D_stem.csv")

#################
#### by STEM ####
#################
df_cont_stem <- df_all_integ %>% 
  group_by(year,inst) %>% 
  mutate(mensum=sum(men),
         womensum=sum(women),
         contMitMt=men/mensum,
         contFitFt=women/womensum,
         scorex=100*(1/2)*(abs(contMitMt-contFitFt))) %>% 
  group_by(year,inst,stem) %>% 
  mutate(score=sum(scorex))

df_cont_stem %>% 
  filter(numx==1 | numx == 9 | numx == 38) %>% 
  mutate(stem=as.factor(stem)) %>% 
  ggplot(aes(x=year, y= score, group=stem, color=stem)) + geom_line(aes(linetype=stem))  + facet_wrap(~major)+
  facet_wrap(~inst)+theme_few()+xlab("")+ylab("")+theme(legend.title=element_blank(),legend.position="bottom")+
  scale_color_manual(values=cbp2)
ggsave(height=6,width=9,dpi=200, filename="../../../3.Results/Overal_inst_stem_grads.pdf",  family = "Helvetica")

######################################################################
# Decomposition
######################################################################

All <- lapply(c(1975,1990,2005,2019),function(i){
  lapply(c("Total","National & Public","Private"),function(j){
    df_cont %>% ungroup() %>% 
      dplyr::select(inst,year,major,v_Mj=men,v_Fj=women) %>% 
      filter(year==i & inst == j) %>% 
      pivot_longer(
        cols = starts_with("v_"),
        names_to = "sex",
        values_to = "value",
        values_drop_na = TRUE
      ) %>% 
      pivot_wider(names_from = c("sex"), 
                  values_from = value) %>% 
      mutate(v_Tj=v_Mj+v_Fj)%>% 
      pivot_longer(
        cols = starts_with("v_"),
        names_to = "sex",
        names_prefix = "v_",
        values_to = "value",
        values_drop_na = TRUE
      ) %>% 
      pivot_wider(names_from = "sex", 
                  values_from = value) %>% 
      group_by(inst) %>% 
      mutate(M=sum(Mj),
             F=sum(Fj),
             T=sum(Tj),
             Dj=Fj/F - Mj/M,
             Dj = 100/2 * abs(Dj),
             D = sum(Dj))
  })
})

df_all <- do.call(bind_rows, c(All)) %>% 
  pivot_wider(names_from = "year", 
              values_from = c("Mj","Fj","Tj","M","F","T","Dj","D")) %>% 
  left_join(label) %>% 
  mutate(stem = case_when(
    stem == 0 ~ "Non-STEM",
    stem == 1 ~ "STEM",
    stem == 2 ~ "Health"),
    comp2j_19752019 = 100 / 2 * abs((Fj_2019/Tj_2019*Tj_1975)/(F_2019/T_2019*T_1975) -
                                      (Mj_2019/Tj_2019*Tj_1975)/(M_2019/T_2019*T_1975)) - Dj_1975,
    comp1j_19752019 = Dj_2019 - 100 / 2 * abs((Fj_2019/Tj_2019*Tj_1975)/(F_2019/T_2019*T_1975) -
                                                (Mj_2019/Tj_2019*Tj_1975)/(M_2019/T_2019*T_1975)),
    comp2j_19752019 = if_else(is.na(comp2j_19752019)==T,0,comp2j_19752019),
    comp1j_19752019 = if_else(is.na(comp1j_19752019)==T,0,comp1j_19752019),
    #1975-1990    
    comp2j_19751990 = 100 / 2 * abs((Fj_1990/Tj_1990*Tj_1975)/(F_1990/T_1990*T_1975) -
                                      (Mj_1990/Tj_1990*Tj_1975)/(M_1990/T_1990*T_1975)) - Dj_1975,
    comp1j_19751990 = Dj_1990 - 100 / 2 * abs((Fj_1990/Tj_1990*Tj_1975)/(F_1990/T_1990*T_1975) -
                                                (Mj_1990/Tj_1990*Tj_1975)/(M_1990/T_1990*T_1975)),
    comp2j_19751990 = if_else(is.na(comp2j_19751990)==T,0,comp2j_19751990),
    comp1j_19751990 = if_else(is.na(comp1j_19751990)==T,0,comp1j_19751990),
    #1990-2005 
    comp2j_19902005 = 100 / 2 * abs((Fj_2005/Tj_2005*Tj_1990)/(F_2005/T_2005*T_1990) -
                                      (Mj_2005/Tj_2005*Tj_1990)/(M_2005/T_2005*T_1990)) - Dj_1990,
    comp1j_19902005 = Dj_2005 - 100 / 2 * abs((Fj_2005/Tj_2005*Tj_1990)/(F_2005/T_2005*T_1990) -
                                                (Mj_2005/Tj_2005*Tj_1990)/(M_2005/T_2005*T_1990)),
    comp2j_19902005 = if_else(is.na(comp2j_19902005)==T,0,comp2j_19902005),
    comp1j_19902005 = if_else(is.na(comp1j_19902005)==T,0,comp1j_19902005),
    #2005-2019
    comp2j_20052019 = 100 / 2 * abs((Fj_2019/Tj_2019*Tj_2005)/(F_2019/T_2019*T_2005) -
                                      (Mj_2019/Tj_2019*Tj_2005)/(M_2019/T_2019*T_2005)) - Dj_2005,
    comp1j_20052019 = Dj_2019 - 100 / 2 * abs((Fj_2019/Tj_2019*Tj_2005)/(F_2019/T_2019*T_2005) -
                                                (Mj_2019/Tj_2019*Tj_2005)/(M_2019/T_2019*T_2005)),
    comp2j_20052019 = if_else(is.na(comp2j_20052019)==T,0,comp2j_20052019),
    comp1j_20052019 = if_else(is.na(comp1j_20052019)==T,0,comp1j_20052019)
  ) %>% 
  group_by(inst,stem) %>% 
  mutate(comp2_19752019 = sum(comp2j_19752019),
         comp1_19752019 = sum(comp1j_19752019),
         comp2_19751990 = sum(comp2j_19751990),
         comp1_19751990 = sum(comp1j_19751990),
         comp2_19902005 = sum(comp2j_19902005),
         comp1_19902005 = sum(comp1j_19902005),
         comp2_20052019 = sum(comp2j_20052019),
         comp1_20052019 = sum(comp1j_20052019))%>% 
  filter(major=="Literature" | major == "Mathematics" | major == "Medicine") %>% 
  dplyr::select(inst, stem, compD_1975=D_1975, compD_1990=D_1990, compD_2005=D_2005, compD_2019=D_2019, 
                comp1_19751990, comp1_19752019, comp1_19902005, comp1_20052019,
                comp2_19751990, comp2_19752019, comp2_19902005, comp2_20052019)

comp1 <- dplyr::select(df_all, starts_with("comp1_")) %>% 
  pivot_longer(
    cols = starts_with(c("comp1_")),
    names_to = c("year"),
    names_prefix = "comp1_",
    values_to = c("comp1")) %>% 
  mutate(year=case_when(
    year == "19751990" ~ 1975,
    year == "19752019" ~ 2019,
    year == "19902005" ~ 1990,
    year == "20052019" ~ 2005,
  ))

comp2 <- dplyr::select(df_all, starts_with("comp2_")) %>% 
  pivot_longer(
    cols = starts_with(c("comp2_")),
    names_to = c("year"),
    names_prefix = "comp2_",
    values_to = c("comp2"))%>% 
  mutate(year=case_when(
    year == "19751990" ~ 1975,
    year == "19752019" ~ 2019,
    year == "19902005" ~ 1990,
    year == "20052019" ~ 2005,
  ))

df_d <- df_all[c(1,4,7,10),] %>% ungroup() %>% 
  dplyr::select(inst,starts_with("compD_")) %>% 
  pivot_longer(
    cols = starts_with(c("compD_")),
    names_to = c("year"),
    names_prefix = "compD_",
    values_to = c("D")) %>% 
  mutate(year=as.numeric(year))

df <- comp1 %>% 
  left_join(comp2) %>% 
  left_join(df_d) %>% 
  group_by(inst,stem) %>%
  mutate(Ds=if_else(year==1975,D,0),
         Ds=sum(Ds),
         Dr=D/Ds)

dfx <- comp1 %>% 
  left_join(comp2) %>% group_by(inst,stem,year) %>% 
  pivot_longer(
    cols = comp1:comp2,
    names_to = c("cont"),
    values_to = c("value")) %>% 
  mutate(cont=if_else(cont=="comp2","Sex composition","Major mix"),
         cont=paste(cont," (",stem,")",sep=""),
         cont=factor(cont,levels=c("Major mix (Non-STEM)","Major mix (STEM)",
                                   "Major mix (Health)","Sex composition (Non-STEM)",
                                   "Sex composition (STEM)","Sex composition (Health)")),
         year=case_when(
           year == 1975 ~ "1975-1990",
           year == 1990 ~ "1990-2005",
           year == 2005 ~ "2005-2019",
           year == 2019 ~ "1975-2019"
         ),
         year=factor(year,levels=c("1975-1990","1990-2005","2005-2019","1975-2019")))

ggplot(dfx, aes(x=year, y=value, fill=cont,group=cont)) + 
  geom_bar(stat="identity")+xlab("")+ylab("")+theme_few() +facet_wrap(~inst)+scale_fill_manual(values=cbp1)+
  theme(legend.title=element_blank(), legend.position = "bottom")+theme(axis.text.x=element_text(angle=60,vjust=1,hjust=1))
ggsave(height=5,width=8,dpi=200, filename="../../../3.Results/Decomp_rev_grads.pdf",  family = "Helvetica")

dfx %>% 
  filter(year=="1975-2019" & inst =="Total") %>% 
  ungroup() %>% 
  mutate(score=sum(value)) 

df_cont %>% filter(year==1975 | year ==2019) %>% 
  filter(numx==1 & inst =="Total") %>% 
  dplyr::select(year, score)
  
  
