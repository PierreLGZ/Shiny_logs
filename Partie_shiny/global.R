
######## Packages ########
list.of.packages <-
  c(
    "DT",
    "tidyverse",
    "tidyr",
    "readr",
    "ggraph",
    "data.table",
    "tidytext",
    
    "shiny",
    "shinyjs",
    "visNetwork",
    "magrittr",
    "igraph",
    
    "circlize",
    "dendextend",
    "sqldf",
    "ape",
    
    "ggplot2",
    "ggdendro",
    "ggpubr",
    "plotly",
    "highcharter",
    "ggmap"
    
  )
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)


#####################################################
#                 DONNEES Globale                  # 
#####################################################
setwd("/Users/pierre/Documents/M2/Cours_M2/Challenge/Projet/Samy/Groupe_8")
brutes <- read.csv("myData.log", sep="")
  
analyse<-sqldf("select ipsrc, count(*) as nombre, 
    count(distinct dstport) as cndstport,
    sum( case when action like 'Permit' then 1 else 0 ENd) as permit,
    sum( case when action like 'Permit' and dstport < 1024 then 1 else 0 ENd) as inf1024permit,
    sum( case when action like 'Permit' and dstport >= 1024 then 1 else 0 ENd) as sup1024permit,
    sum( case when action like 'Permit' and (dstport = 21 OR dstport = 22 OR dstport = 3389 OR dstport = 3306) 
    then 1 else 0 ENd) as adminpermit,
    sum(case when action like 'Deny' then 1 else 0 ENd) as deny,
    sum(case when action like 'Deny' and dstport < 1024 then 1 else 0 ENd) as inf1024deny,
    sum(case when action like 'Deny' and dstport >= 1024 then 1 else 0 ENd) as sup1024deny,
    sum( case when action like 'Deny' and (dstport = 21 OR dstport = 22 OR dstport = 3389 OR dstport = 3306) 
    then 1 else 0 ENd) as admindeny from brutes group by ipsrc ")
  
rownames(analyse)<- analyse$ipsrc
analyse <- analyse[,-1]
  
brutes$datetime <-strptime(brutes$datetime, "%Y-%m-%d %H:%M:%S")
brutes$datetime_month<-format(brutes$datetime, format = "%m")
brutes$datetime_year<-format(brutes$datetime, format = "%Y")
brutes$datetime_day<-format(brutes$datetime, format = "%d")
brutes$datetime_hour<-brutes$datetime$hour
brutes$datetime_min<-brutes$datetime$min
brutes$datetime_year<- as.numeric(brutes$datetime_year)
brutes$datetime_day<- as.numeric(brutes$datetime_day)
brutes$datetime_month<- as.numeric(brutes$datetime_month)
  
  
table_ip <- brutes %>% group_by(ipsrc) %>%
    summarize(deny=sum(action=="Deny"),permit=sum(action=="Permit"),n=n()) %>% 
    arrange(desc(n))
n_ip = length(unique(table_ip$n))
  
rule_by_proto <- brutes[,c("policyid","proto","action")]
rules_by_port <- rule_by_proto %>% group_by(policyid) %>% count
n_rules = max(rules_by_port$n)
  
table_port <- brutes %>% group_by(dstport) %>%
    summarize(deny=sum(action=="Deny"),permit=sum(action=="Permit"),n=n()) %>%
    arrange(desc(n)) 
n_port = max(table_port$n)
  
ip_rel <- brutes %>% group_by(ipsrc,ipdst) %>% count %>% arrange(desc(n))
n_network <- max(ip_rel$n)

