library(tidyverse)
library(ggplot2)
library(dplyr)
library(survival)
library(stats)
library (nnet)
library(car)
library(aod)
library(rstatix)
library(ggpubr)

setwd("~/OneDrive - University of Nebraska-Lincoln/Sundaywascientist/datasheets/outreach")
list.files()
#files with visitor data
aa<-read_csv("assembleanarachnid.csv")
ds<-read_csv("dancingspiders.csv")
ff<-read_csv("funfactsaboutschizocosa.csv")
int<-read_csv("intro.csv")
pr<-read_csv("prize.csv")
dn<-read_csv("spiderdatenight.csv")
ah<-read_csv("whereistheairhair.csv")

#plot of each station visitors vary by time
ggplot(data=aa, aes(x=time, y=aa_net_visitors)) +
  geom_bar(stat="identity", position="dodge", color='black')+
  theme_bw() + ylab("Net Visitors") + xlab("Time")+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', 
                                 angle = 60, vjust = 0.5),
        axis.text.y=element_text(face="bold", color='black'))

#merge all visitor data
merge_list<-list(aa, ds, ff, int, pr, dn, ah)
merged<-merge_list %>% reduce(full_join, by='time')
merged<-merged %>% rename(`Intro\nTable`=in_net_visitor,
                          `Fun Facts About\nSchizocosa`= ff_net_visitors,
                          `Dancing\nSpiders`= ds_net_visitors,
                          `Where is the\nAir, Hair?`= ah_net_visitors,
                          `Spider\nDate Night`= dn_net_visitors,
                          `Assemble an\nArachnid`= aa_net_visitors,
                          `Prize\nTable`=pr_net_visitors)

#graph for netvisitors by time and stations
net_visitors<- merged %>% pivot_longer(cols=c("Intro\nTable", "Fun Facts About\nSchizocosa",
                                              "Dancing\nSpiders", "Where is the\nAir, Hair?", 
                                              "Spider\nDate Night","Assemble an\nArachnid", 
                                              "Prize\nTable"),
                    names_to='stations',
                    values_to='net_visitors') %>% select(time, stations, net_visitors)


net_visitors$stations<-factor(net_visitors$stations,
                              levels=c("Intro\nTable", "Fun Facts About\nSchizocosa",
                                       "Dancing\nSpiders", "Where is the\nAir, Hair?", 
                                       "Spider\nDate Night","Assemble an\nArachnid", 
                                       "Prize\nTable"))

color<-c("12:00-12:30"="#451D66", "12:30-13:00"="#784D9B", "13:00-13:30"="#A585BF",
         "13:30-14:00"="#BF85A7", "14:00-14:30"="#C72F89", "14:30-15:00"="#7A1551")



ggplot(net_visitors ,aes(x=stations, fill=time, y=net_visitors))+
  geom_bar(stat="identity", position="dodge", color='black')+
  theme_bw() +
  labs_pubr() + ylab("Net Visitors") + xlab("Stations")+
  scale_fill_manual(name = "Time", values= color )+
  scale_y_continuous(breaks = seq(0, 36, by=2))+
  facet_wrap(~stations, nrow=2, scales = "free_x")+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())


#total visitors by station
total<-merged[c(2,4,6,8,10,12,14)]
all<-total %>% filter(row(total) == 6) %>% 
  pivot_longer(cols=c('aa_cummulative_visitors', 'ds_cummulative_visitors', 
                      'dn_cummulative_visitors', 'ah_cummulative_visitors', 
                      'ff_cummulative_visitors','in_cummulative_visitors', 
                      'pr_cummulative_visitors'),
                      names_to='stations',
                      values_to='total_visitors')
all$stations<-factor(all$stations,
                              levels=c("in_cummulative_visitors", "ff_cummulative_visitors",
                                       "ds_cummulative_visitors", "ah_cummulative_visitors",
                                       "dn_cummulative_visitors", "aa_cummulative_visitors",
                                       "pr_cummulative_visitors"))
visitorxticks<-c("Intro\nTable", "Fun Facts About\nSchizocosa",
                 "Dancing\nSpiders", "Where is the\nAir, Hair?", 
                 "Spider\nDate Night","Assemble an\nArachnid", 
                 "Prize\nTable")

ggplot(all ,aes(x=stations, y=total_visitors))+
  geom_bar(stat="identity", position="dodge", color='black', fill=c("#C72F89"))+
  theme_bw() +
  labs_pubr() + ylab("Total Visitors") + xlab("Stations")+
  scale_x_discrete(labels= visitorxticks)+scale_y_continuous(breaks = seq(0, 100, by=10))

#files with votes and ticket no.
votes<-read_csv("votes.csv")
tk_aa<-read_csv("ticketno_assembleanarachnid.csv")
tk_ds<-read_csv("ticketno_dancingspiders.csv" )
tk_ff<-read_csv("ticketno_funfactsaboutschizocosa.csv")
tk_dn<-read_csv("ticketno_spiderdatenight.csv")
tk_ah<-read_csv("whereistheairhair.csv")

#file with station codes
stations<-read_csv("table_list.csv")

#survey files
qs<-read_csv("survey.csv")
da<-read_csv("demographic_age.csv")
dc<-read_csv("demographic_child.csv")
dg<-read_csv("demographic_gender.csv")
dr<-read_csv("demographic_race.csv")
dsp<-read_csv("demographic_spanish.csv")

#graph for survey answers
qs<-qs %>% rename(`Strongly Agree`=strongly_agree, 
                  `Somewhat Agree`=somewhat_agree,
                  `Neither Agree Nor Disagree`=neither_agree_nor_disagree,
                  `Somewhat Disagree`=somewhat_disagree,
                  `Strongly Disagree`=strongly_disagree)
qs1<-qs %>% pivot_longer(cols=c("Strongly Agree", "Somewhat Agree",
                                "Neither Agree Nor Disagree",
                                "Somewhat Disagree", "Strongly Disagree"),
                                names_to='ratings',
                                values_to='number_of_people') %>% 
  select(statements, ratings, number_of_people)

qs1$ratings<-factor(qs1$ratings,
                              levels=c("Strongly Agree", "Somewhat Agree", "Neither Agree Nor Disagree", "Somewhat Disagree",
                                       "Strongly Disagree"))

qs1$statements<-factor(qs1$statements, levels=c("My interest in spiders brought me to this event.",
                                                "I enjoyed learning about communication in spiders.",
                                                "I feel more positively towards spiders after attending this event.",
                                                "I will come back to outreach events about spiders."))

state<-c("I will come back\nto outreach events\nabout spiders.",
        "I feel more positively\ntowards spiders after\nattending this event.",
        "I enjoyed learning\nabout communication\nin spiders.",
        "My interest in\nspiders brought me\nto this event.")
         
surveycolor <- c("Strongly Agree"="#C72F89", "Somewhat Agree"="#784D9B",
                 "Neither Agree Nor Disagree"="#7A1551",
                 "Somewhat Disagree"="#451D66",
                 "Strongly Disagree"="#A585BF") 

ggplot(qs1 ,aes(x=statements,
                fill=ratings, y=number_of_people))+
  geom_bar(stat="identity", position=position_dodge2(reverse=TRUE),
           color="black")+
  theme_bw() +
  labs_pubr() + ylab("Number of People") + xlab("Statements")+
  scale_x_discrete(labels= state)+
  scale_fill_manual(name="", values = surveycolor)+
  scale_y_continuous(breaks = seq(0, 16, by=2)) + coord_flip()+
  facet_wrap(~statements, nrow=4, scales="free_y")+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank())

#graph for age group
ggplot(da, aes(x=`Age Group`, y=Numbers)) + 
  geom_bar(stat="identity", position="dodge", color="black", fill=c("#C72F89"))+
  theme_bw() + ylab("Number of People")+ xlab("Age Group (in years)")+
  labs_pubr()+ scale_y_continuous(breaks = seq(0, 12, by=3))

#graph for gender
dg<-dg %>% mutate(Gender=ifelse(Gender=="Non-binary/third gender", "Non-binary/\nthird gender", Gender)) %>% 
  mutate(Gender=ifelse(Gender=="Prefer to self-describe", "Prefer to\nself-describe", Gender)) %>% 
  mutate(Gender=ifelse(Gender=="Prefer not to say", "Prefer not\nto say", Gender))

ggplot(dg, aes(x=Gender, y=Numbers)) + 
  geom_bar(stat="identity", position="dodge", color="black", fill=c("#C72F89"))+
  theme_bw() + ylab("Number of People")+
  labs_pubr()+ scale_y_continuous(breaks = seq(0, 15, by=3))


#graph for votes
votes<-votes %>% rename(Kids=vote_tk, Adults=vote_ta)
votes_mani<- votes %>% pivot_longer(cols=c('Kids', 'Adults'),
                                       names_to='votes',
                                       values_to='numbers')
votes_mani$stations<-factor(votes_mani$stations, levels=c('funfactsaboutschizocosa', 'dancingspiders',
                                     'whereistheairhair', 'spiderdatenight',
                                     'assembleanarachnid'))
votes_mani$votes<-factor(votes_mani$votes, levels=c('Kids', 'Adults'))

votesticks<- c("Fun Facts About\nSchizocosa", "Dancing\nSpiders",
                  "Where is the\nAir, Hair?", "Spider\nDate Night", "Assemble an\nArachnid")

votecolor<-c("Adults"="#784D9B", "Kids"="#C72F89")

ggplot(votes_mani ,aes(x=stations, fill=votes, y=numbers))+
  geom_bar (stat="identity", position="stack",
           color="black")+
  theme_bw() +
  labs_pubr() + ylab("Number of votes") + xlab("Stations")+
  scale_x_discrete(labels= votesticks)+
  scale_fill_manual(name = "Votes", values= votecolor)

