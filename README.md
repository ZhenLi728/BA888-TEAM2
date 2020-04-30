# BA888-TEAM2
## Instructions for exploring code works
1. This is our all works for building a recommendation system. We used data source from Meetup public data; https://www.kaggle.com/sirpunch/meetups-data-from-meetupcom

2. After you download all data source from above, you can run our codes accordingly; Note: You must run codes in orders below and make sure your working directory is correctly located to run all codes.
	1. Run cleaning_data.Rmd for data processing
	2. Run Association_rule_topic.Rmd to figure out the foundation(Topic association rules) of our recommendation system; Note: If you want to visualize Rshiny interactive plots of association rule, you should uncomment the code "ruleExplorer(train_rule3)"; This RMD file includes information of topic association rules interactive plot, prediction accuracy of recommendation model, and two baselines of prediction accuracy for evaluating our model's prediction accuracy. 
	3. Run groupRecommendation.Rmd to figure out actual groups' recommendations by predicting topics that users are likely to choose based on our 27 topic association rules. 
	4. If you want to specifically view our 27 association topic rules, you can open topic_rules.csv uploaded. 
	5. If you want to see our geographical Rshiny interative plots, please run Map of Distribution of Top 5 Categories among 

4. Other sources for our project can be referred below;
	1. Dashboard (Exploratory Data Analysis) :   https://public.tableau.com/profile/mandi7631#!/
	2. Poster: https://drive.google.com/a/bu.edu/file/d/1wmYQ65fSYtPRrpSKq-4xa_DR5ggCDx-W/view?usp=sharing
	3. Slide: https://docs.google.com/presentation/d/1NeErr9ZekAddFQW-l8YtMaCzFw8-NYIhaVQjQvpdQaA/edit?usp=sharing
	4. Written paper: https://docs.google.com/document/d/1HGxGdTWktbdz0nr02ShExLOYFRrqnSXfHXo9u9BEE9c/edit?usp=sharing




