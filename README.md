# BA888-TEAM2
1. This is our all works for building a recommendation system. We used data source from Meetup public data; https://www.kaggle.com/sirpunch/meetups-data-from-meetupcom

2. After you download all data source from above, you can run our codes accordingly; Note: You must run codes in orders below.
	1. Run cleaning_data.Rmd for data processing
	2. Run Association_rule_topic.Rmd to figure out the foundation(Topic association rules) of our recommendation system; Note: If you want to visualize Rshiny interactive plots of association rule, you should uncomment the code "ruleExplorer(train_rule3)"; This RMD file includes information of topic association rules interactive plot, prediction accuracy of recommendation model, and two baselines of prediction accuracy for evaluating our model's prediction accuracy. 
	3. Run groupRecommendation.Rmd to figure out actual groups' recommendations by predicting topics that users are likely to choose based on our 27 topic association rules. 
	4. If you want to specifically view our 27 association topic rules, you can open topic_rules.csv uploaded.  
