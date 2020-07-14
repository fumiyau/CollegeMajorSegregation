#=====================================================================
# 2020/06/29
# College major segregation (enrolls)
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
library(ggrepel)
library(knitr)
library(readxl)

######################################################################
# Set directory
######################################################################
setwd("/Users/fumiyau/Dropbox (Princeton)/09.CollegeMajor/")
setwd("CollegeMajorSegregation/2.Data/NameEdited/Enrolls")

######################################################################
# Assign colors
######################################################################
cbp1 <- c("#A6CEE3", "#E0E0E0", "#E6AB02", "#1F78B4",
          "#878787", "#A6761D")
cbp2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
  filter((year >= 1984 | (year < 1984 & num != 31 & num != 73))) %>% #14020 to 13948 omit 農学 & その他
  filter((year >= 1986 | year <= 1983 | (year >= 1984 & year <= 1985 & num != 31 & num != 70))) %>%  # 13948 to 13932　omit 農学 & その他
  dplyr::select(major,year,men=`男`,women=`女`,inst) %>% 
  mutate(men=if_else(is.na(men)==TRUE,0,men),
         women=if_else(is.na(women)==TRUE,0,women))
#write.csv(df_all, "df_all.csv")

df_temp <- df_all %>% 
  filter(!grepl("昭和|平成|令和", major)) %>% 
  filter(is.na(as.numeric(paste(major)))==TRUE) %>% 
  filter(!grepl("人文", major)) %>% 
  filter(!grepl("国際関係学部", major)) %>% 
  filter(!grepl("人間関係科学", major)) %>% 
  mutate(other=if_else(major=="その他",1,0)) %>% 
  mutate(num=data.table::rowid(year,inst,other)) %>% 
  mutate(numx=data.table::rowid(year,inst)) %>% 
  mutate(majorn=paste(numx,major))
  
df_all_v1<-df_temp %>%  #人文科学から水産まで
  filter(numx<38) %>% 
  dplyr::select(year,inst,major,men,women,numx)

df_all_v2 <- df_temp %>%
  filter(grepl("薬学", major)|(grepl("医学", major) & !grepl("獣医学",major))|grepl("歯学", major)|major=="看護学"|
           (num == 6 & major == "その他") ) %>% 
  dplyr::select(year,inst,major,men,women) %>% 
  mutate(numx = case_when(
    grepl("医学専門学群", major) ~ 42,
    grepl("医学", major) ~ 38,
    grepl("歯学", major)  ~ 39,
    grepl("薬学", major) ~ 40,
    major == "看護学" ~ 41,
    major == "その他" ~ 42
  ))

#商船＋家政
df_all_v3 <- df_temp %>%
  filter(major=="商船学"|major=="家政学"|major=="食物学"|major=="被服学"|major=="住居学"| major == "児童学") %>% 
  dplyr::select(year,inst,major,men,women) %>% 
  mutate(numx = case_when(
    major == "商船学" ~ 43,
    major == "家政学" ~ 44,
    major == "食物学" ~ 45,
    major == "被服学" ~ 46,
    major == "住居学" ~ 47,
    major == "児童学" ~ 48
  )) 


#教育
df_all_v4b <- df_temp %>%
  filter(num ==7 & major=="その他"&year<2008 |(num ==8 & major=="その他"&year>=2008)) #2008年以降は家政にその他があるため

df_all_v4 <- df_temp %>%
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
df_all_v5 <- df_temp %>%
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

df_all_v6 <- df_temp %>%
  filter(grepl("教養学|総合科学|教養課程", major)) %>% 
  dplyr::select(year,inst,major,men,women) %>% 
  mutate(numx = case_when(
    grepl("教養学", major) ~ 64,
    grepl("総合科学", major) ~ 65,
    grepl("文科", major)  ~ 66,
    grepl("理科|埋科", major) ~ 67,
    grepl("その他", major) ~ 68
  ))

######################################################################
# Data binding & labeling
######################################################################
label <- read_csv("../../Labeling.csv") %>% 
  mutate(major=factor(major,levels=major))

df_all_integ <- bind_rows(df_all_v1,df_all_v2,df_all_v3,df_all_v4,df_all_v5,df_all_v6) %>% 
  dplyr::select(-major) %>% 
  group_by(year,inst,numx) %>% 
  summarise_all(list(sum)) %>% 
  filter(numx != 68) %>% ###教養その他は存在しないため
  left_join(label) %>% 
  mutate(
    stem=if_else(numx==27,0,stem), ###change craft into non-STEM
    stem = case_when(
    stem == 0 ~ "Non-STEM",
    stem == 1 ~ "STEM",
    stem == 2 ~ "Health"
  ),
  inst=case_when(
    inst == 1 ~ "Total",
    inst == 2 ~ "National",
    inst == 3 ~ "Public",
    inst == 4 ~ "Private"),
  stem = factor(stem,levels=c("Non-STEM","STEM","Health")),
  inst = factor(inst,levels=c("Total","National","Public","Private"))
  )

#write.csv(df_all_integ, "../../df_all_integ.csv")
  
######################################################################
# Get contribution and pick up major contributors
# memo 18個は少し多いかもしれないのでthreshholdをもう少し高めにしてもいいかもしれない
######################################################################
df_cont <- df_all_integ %>% 
  group_by(year,inst) %>% 
  mutate(mensum=sum(men),
         womensum=sum(women),
         contMitMt=men/mensum,
         contFitFt=women/womensum,
         scorex=100*(1/2)*(abs(contMitMt-contFitFt)),
         score=sum(scorex))

# Pick up top contributions
df_cont_rel <- df_cont %>% 
  filter(inst=="Total") %>% 
  arrange(year,inst,scorex) %>% 
  group_by(year) %>% 
  mutate(score_order=cumsum(scorex),
         score_rel_order=as.numeric(score_order/score)) %>% 
  filter(score_rel_order>0.25)
 
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

d_ends <- df_cont_long %>% 
  group_by(inst,major) %>% 
  top_n(1, year) %>% 
  mutate(end=TRUE)

d_begins <- df_cont_long %>% 
  group_by(inst,major) %>% 
  top_n(-1, year) %>% 
  mutate(begin=TRUE)

df_cont_long <- df_cont_long %>% 
  

for (i in c("Total","National","Public","Private")){
  df_cont_longx <- df_cont_long %>% 
    left_join(df_cont_rel1975) %>% 
    left_join(df_cont_rel2019) %>% 
    filter(flag1==1 | flag2==1) %>% 
    filter(inst==i) %>% 
    left_join(d_ends) %>% 
    left_join(d_begins) %>% 
    mutate(ends = if_else( end == TRUE | begin == TRUE, TRUE, FALSE),
           value2=round(value,2))
  
  ggplot(df_cont_longx,aes(x=year, y= value, group=sex, label = value2)) + geom_line(aes(linetype=sex)) + facet_wrap(~major)+
    theme_few()+xlab("")+ylab("")+theme(legend.title=element_blank())+
    theme(axis.text.x=element_text(angle=60,vjust=1,hjust=1))+
    geom_text_repel(data = subset(df_cont_longx,ends==TRUE))
  
  ggsave(height=6,width=12,dpi=200, filename= paste("../../../3.Results/Top18_",i,".pdf",sep=""),  family = "Helvetica")
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
         score=sum(scorex))

######################################################################
# Descriptive trends
######################################################################

#################
#### overall ####
#################
df_cont %>% 
  filter(numx==1) %>% 
  ggplot(aes(x=year, y= score, group=inst, color=inst)) + geom_line(aes(linetype=inst)) +
  theme_few()+xlab("")+ylab("")+theme(legend.title=element_blank())+
  scale_color_manual(values=cbp2)
ggsave(height=6,width=9,dpi=200, filename="../../../3.Results/Overal_inst.pdf",  family = "Helvetica")

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
  facet_wrap(~inst)+theme_few()+xlab("")+ylab("")+theme(legend.title=element_blank())+
  scale_color_manual(values=cbp2)
ggsave(height=6,width=9,dpi=200, filename="../../../3.Results/Overal_inst_stem.pdf",  family = "Helvetica")

######################################################################
# Decomposition
######################################################################

All <- lapply(c(1975,1990,2005,2019),function(i){
  lapply(c("Total","National","Public","Private"),function(j){
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
  theme(legend.title=element_blank())+theme(axis.text.x=element_text(angle=60,vjust=1,hjust=1))
ggsave(height=6,width=8,dpi=200, filename="../../../3.Results/Decomp_rev.pdf",  family = "Helvetica")