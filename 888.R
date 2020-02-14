library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(quanteda)
library(tidytext)
library(topicmodels)
library(factoextra)
library(cluster)
library(streamgraph)
library(plotly)
library(sentimentr)
library(Rtsne)
library(caret)
library(class)
library(tidymodels)
library(xgboost)
library(rvest)
install.packages('leaflet')
library(leaflet)

setwd('Desktop/Meetup/')
raw_cat<-read_csv('meetups-data-from-meetupcom/categories.csv')
raw_cit<-read_csv('meetups-data-from-meetupcom/cities.csv')
raw_eve<-read_csv('meetups-data-from-meetupcom/events.csv')
raw_g_top<-read_csv('meetups-data-from-meetupcom/groups_topics.csv')
raw_grp<-read_csv('meetups-data-from-meetupcom/groups.csv')
raw_m_top<-read_csv('meetups-data-from-meetupcom/members_topics.csv')
raw_mem<-read_csv('meetups-data-from-meetupcom/members.csv')
raw_top<-read_csv('meetups-data-from-meetupcom/topics.csv')
raw_ven<-read_csv('meetups-data-from-meetupcom/venues.csv')

#########categories: 
#33 categories (all unique); no missing value
skimr::skim(raw_cat)
head(raw_cat,5)
length(unique(raw_cat$category_id))
#########cities:

skimr::skim(raw_cit)
head(raw_cit,5)
unique(raw_cit$city)

##########event
#drop meaningless cols (all rows the same): event_status,event_url,fee.description,photo_url,venue.localized_country_name,visibility,why
skimr::skim(raw_eve)
head(raw_eve,5)
events<-raw_eve %>% select(-c(event_status,event_url,fee.description,photo_url,venue.localized_country_name,visibility,why))
length(unique(events$group_id)) #341
length(unique(events$event_id))  #5897

###########group & topic (一个topic对应很多groups)
skimr::skim(raw_g_top)
nrow(raw_g_top)
#unique topic VS unique group
length(unique(raw_g_top$topic_id)) #961
length(unique(raw_g_top$group_id)) #11876 (31212 in total??)


###########group （名字，描述，创建人，所属categories，member的人数 等）
skimr::skim(raw_grp)
length(unique(raw_grp$group_id)) #16330 (all unique)
#drop meaningless cols (all rows the same):group_photo.base_url,group_photo.highres_link,group_photo.photo_link
  #group_photo.thumb_link,group_photo.type,join_mode,link,organizer.photo.base_url,organizer.photo.highres_link,organizer.photo.photo_link,organizer.photo.thumb_link
  #organizer.photo.type,urlname,group_photo.photo_id
groups<-raw_grp %>% select(-c(group_photo.base_url,group_photo.highres_link,group_photo.photo_link,group_photo.thumb_link,group_photo.type,join_mode,link,
                              organizer.photo.base_url,organizer.photo.highres_link,organizer.photo.photo_link,organizer.photo.thumb_link,
                              organizer.photo.type,urlname,group_photo.photo_id))


##########members_topics (一个topic对应很多members，即row有很多重复的topics)
skimr::skim(raw_m_top)
length(unique(raw_m_top$topic_id))  #1623 (961 in group & topic)
length(unique(raw_m_top$member_id))  #605790
nrow(raw_m_top)

#########members (一个member对应/参加多个groups)
skimr::skim(raw_mem)
length(unique(raw_mem$member_id))  #1087923
members<-raw_mem %>% select(-link)


#########topics (topic 自身的信息，如描述、名字，以及每个topic共有多少member)
skimr::skim(raw_top) 
length(unique(raw_top$topic_id)) # 2509
topics<-raw_top %>% select(-link)
length(unique(topics$topic_id))
##########venue
skimr::skim(raw_ven)

##################################
group_temp <- groups
group_temp$created <- format(group_temp$created,"%Y")
skim(group_temp)
View(group_temp)
group_category <- group_temp %>% select(category.shortname, lat, lon, created) %>% filter(category.shortname %in% c('tech', 'socializing', 'career-business', 'outdoors-adventure', 'language')) %>% group_by(category.shortname)
View(group_category)
skim(group_category)
group_category$location <- ifelse(group_category$category.shortname == 'tech', '#ed1c40',
                                  ifelse(group_category$category.shortname == 'socializing', 'yellow', 
                                         ifelse(group_category$category.shortname == 'career-business', 'blue',
                                                ifelse(group_category$category.shortname == 'outdoors-adventure', 'green',
                                                         ifelse(group_category$category.shortname == 'language', 'purple', 'black')))))
group_category <- group_category[-c(which(grepl(-1, group_category$lon))), ]

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(group_category$lon,
                   group_category$lat,
                   color = group_category$location,
                   radius = 0.5,
                   fill = T,
                   fillOpacity = 0.2,
                   opacity = 0.6,
                   popup = paste(group_category$category.shortname,group_category$lat,group_category$lon,sep = "")) %>%
  addLegend("topright",
            colors = c("#ed1c40","yellow", "blue", "green", "purple"),
            labels = c("tech",
                       "socializing",
                       "career-business",
                       "outdoors-adventure",
                       "language"),
            opacity = 2.0)

##################################

##how many years does the data cover?
head(groups$created)
years<-unique(year(groups$created))  #2002-2017
year_span<-length(years) #16 years

##popularity of (33) categories based on No.of groups among 3 cities
  #exclude cities that are not NY, Chicago and SF
cities<-raw_cit %>% filter(state == "NY"|state == "IL"|state == "CA")
groups <-groups %>% filter(state == "NY"|state == "IL"|state == "CA")
  #then join dataset "groups" and 'categories'
grp_cate<-inner_join(groups,categories)
View(grp_cate)
length(unique(grp_cate$category_id)) #still 33

order<-with(grp_cate,table(category.name)) %>%  sort() %>% names()
grp_cate %>% ggplot(aes(x=factor(category.name,level=order),fill=state)) +
  geom_bar(position = 'stack') +
  scale_fill_manual(values=c("#ef4566", "#f69a9a", "#f9cdae",'#cbcba9','#83ae9b')) +
  #theme(axis.text.x=element_text(angle = 30, hjust = 0)) +
  coord_flip()

##Top popular groups based on No. of members (among 3 cities)
View(groups)
order1<-groups$group_name[rev(order(groups$members,decreasing = T)[1:20])] 
groups[order(groups$members,decreasing = T),] %>% head(20) %>% 
  ggplot(aes(x=factor(group_name,level=order1),y=members)) +
  geom_col(fill="#ef4566") +
  coord_flip()


##Among three cities, the trends of popular categories/topics over years
  ##as shown in 'categories graph', tech, career/business, socializing are top three categories
  ##categories (No. of groups)

tmp<-groups %>% filter(category.name=='tech'|category.name=='career/business'|category.name=='socializing') %>% 
  mutate(year_created=factor(year(created)))

ggplot(tmp, aes(x=year_created,color=category.name,shape=category.name)) + geom_point(stat='count',size=5)+
  facet_grid(.~state) +
  theme(axis.text.x=element_text(angle = 60, hjust = 1)) 
  

  ####topics (members): join 'groups' and 'groups_topics' on group_id, ?? 
groups

##Number of new members over years among 3 cities
View(members)
members <-members %>% mutate(year_joined=factor(year(joined))) %>% 
  filter(state == "NY"|state == "IL"|state == "CA"|state == "ca") 
members$state[members$state=='ca']<-'CA'

members %>% ggplot(aes(x=year_joined,fill=state)) +
  geom_bar(position = 'stack') +
  scale_fill_manual(values=c("#ef4566", "#f69a9a", "#f9cdae",'#cbcba9','#83ae9b'))+
  theme(axis.text.x=element_text(angle = 30, hjust = 1)) 

##Number of new events over years among 3 cities
View(events)
events <-events %>% mutate(year_created=factor(year(group.created))) %>% 
  filter(venue.state == "NY"|venue.state == "IL"|venue.state == "CA"|venue.state == "Il") 
events$venue.state[events$venue.state=='Il']<-'IL'

events %>% ggplot(aes(x=year_created,fill=venue.state)) +
  geom_bar(position = 'stack') +
  scale_fill_manual(values=c("#ef4566", "#f69a9a", "#f9cdae",'#cbcba9','#83ae9b'))+
  theme(axis.text.x=element_text(angle = 30, hjust = 1)) 



       