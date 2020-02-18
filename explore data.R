## Explore Data
### How Many Years Does The Data Cover?
head(groups$created)
years<-unique(year(groups$created)) 
year_span<-length(years)
paste('Data covers from', min(years) ,'to', max(years) ,' with total', year_span ,'years.')

### Number of New Members Over Years Among 3 States  (NY, Chicago and SF)
# Exclude cities that are not NY, Chicago and SF
members <-members %>%
  mutate(year_joined=factor(year(joined))) %>% 
  filter(state == "NY"|state == "IL"|state == "CA"|state == "ca") 
members$state[members$state=='ca']<-'CA'
# Plot
members %>% ggplot(aes(x=year_joined,fill=state)) +
  geom_bar(position = 'stack') +
  scale_fill_manual(values=c("#ef4566", "#f69a9a", "#f9cdae",'#cbcba9','#83ae9b'))+
  theme(axis.text.x=element_text(angle = 30, hjust = 1))+
  labs(title ="Number of New Members Over Years Among 3 States" , x = "Year", y = "Number of New Members")

### Number of New Events Over Years Among 3 States(NY, Chicago and SF)
# Exclude cities that are not NY, Chicago and SF
events <-events %>% 
  mutate(year_created=factor(year(group.created))) %>% 
  filter(venue.state == "NY"|venue.state == "IL"|venue.state == "CA"|venue.state == "Il") 
events$venue.state[events$venue.state=='Il']<-'IL'
#Plot
events %>% ggplot(aes(x=year_created,fill=venue.state)) +
  geom_bar(position = 'stack') +
  scale_fill_manual(values=c("#ef4566", "#f69a9a", "#f9cdae",'#cbcba9','#83ae9b'))+
  theme(axis.text.x=element_text(angle = 30, hjust = 1))+
  labs(title ="Number of New Events Over Years Among 3 States " , x = "Year", y = "Number of New Events")

### Popularity of (33) Categories Based on No.of Groups Among 3 States (NY, Chicago and SF)
# Exclude cities that are not NY, Chicago and SF
cities<-raw_cit %>% filter(state == "NY"|state == "IL"|state == "CA")
groups <-groups %>% filter(state == "NY"|state == "IL"|state == "CA")
# Join dataset "groups" and 'categories' The number of category id is still 33.
grp_cate<-inner_join(groups,categories)
length(unique(grp_cate$category_id)) 
order<-with(grp_cate,table(category.name)) %>%  sort() %>% names()
# Plot
grp_cate %>% ggplot(aes(x=factor(category.name,level=order),fill=state)) +
  geom_bar(position = 'stack') +
  scale_fill_manual(values=c("#ef4566", "#f69a9a", "#f9cdae",'#cbcba9','#83ae9b')) +
  #theme(axis.text.x=element_text(angle = 30, hjust = 0)) +
  coord_flip()+
  labs(title ="Popularity of (33) Categories Based on No.of Groups Among 3 States " , x = "Category", y = "Count")
### Top 5 categories Based on No.of Groups Among 3 States (NY, Chicago and SF)

grp_cate %>%
  count(category.name,state) %>%
  mutate(category.name = fct_reorder(category.name, n, .desc = TRUE)) %>%
  arrange(desc(n)) %>%
  head(7)%>%
  ggplot(aes(x = category.name, y = n, fill=state)) + geom_bar(stat = 'identity')+
  scale_fill_manual(values=c("#ef4566", "#f69a9a", "#f9cdae",'#cbcba9','#83ae9b')) +
  labs(title ="Top 3 Categories Based on No.of Groups Among 3 States " , x = "Category", y = "Count")
  coord_flip()

  
### Top Popular Groups Based on No.of Members Among 3 States (NY, Chicago and SF)
order1<-groups$group_name[rev(order(groups$members,decreasing = T)[1:20])] 
# Plot
groups[order(groups$members,decreasing = T),] %>% head(20) %>% 
  ggplot(aes(x=factor(group_name,level=order1),y=members)) +
  geom_col(fill="#ef4566") +
  coord_flip()+
  labs(title ="Top Popular Groups Based on No. of Members Among 3 States" , x = "Group Name", y = "Number of Members")
#########  how to put group names which in same ares(ny,chicago, sf) together? ##keep this or delete?

### Popularity Trend of Top 3 Categories Based on No.of Groups Among 3 States (NY, Chicago and SF)
tmp<-groups %>% filter(category.name=='tech'|category.name=='career/business'|category.name=='socializing') %>% 
  mutate(year_created=factor(year(created)))
# Plot
ggplot(tmp, aes(x=year_created,color=category.name,shape=category.name)) + geom_point(stat='count',size=5)+
  facet_grid(.~state) +
  theme(axis.text.x=element_text(angle = 60, hjust = 1))+
  labs(title ="Popularity Trend of Top 3 Categories Based on No.of Groups Among 3 States" , x = "Year", y = "Count")

## add line
tmp %>% 
  select(category.name,year_created,state) %>%
  count(category.name,state,year_created) %>%
  ggplot(aes(x = year_created,y =n,color=category.name,shape=category.name, group = category.name))+
  geom_point()+
  geom_line() +
  facet_grid(.~state)+
  theme(axis.text.x=element_text(angle = 60, hjust = 1))+
  labs(title ="Popularity Trend of Top 3 Categories Based on No.of Groups Among 3 States" , x = "Year", y = "Count")










