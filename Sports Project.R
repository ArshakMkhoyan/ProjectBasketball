library(readr)
project <- read_csv("~/Downloads/[10-17-2017]-[06-08-2018]-combined-stats.csv")
summary(project)
str(project)
#scater plot of 3point shots using coordinates
plot(project$original_x,project$original_y)
free_shot=project[project$event_type=='free throw',]
end_of_period= project%>%
  group_by(game_id)%>%
  filter(event_type=='end of period')
shot=project[project$event_type=='shot',]

pt3 = grepl(' 3PT ', project$description)
pt3 = project[pt3,]
plot(pt3$original_x,pt3$original_y)
plot(pt3$converted_x,pt3$converted_y)

range(na.omit(project$converted_x))
range(na.omit(project$converted_y))
# 50 feet for x coordinate, 94 for y one

range(na.omit(project$original_x))
range(na.omit(project$original_y))

#turnover types frequencies
table(project[project$event_type=='turnover',]$event_type, project[project$event_type=='turnover',]$type)
ggplot(project[project$event_type=='turnover',], aes(x=type))+geom_bar()+ggtitle('turnover types frequencies')+coord_flip()

total_points=project%>% 
  group_by(player, team)%>%
  summarise(total_points=sum(na.omit(points)))%>%
  arrange(desc(total_points))

#Regressions of final result using the outcome of first, second, and thitd quaters

#Substitutions 
subs=filter(project,event_type=='sub')
teams_subs=project%>%
  filter(event_type=='sub')%>%
  group_by(remaining_time)%>%
  filter(play_id==max(play_id))

#plots
shot=project[project$event_type=='shot',]
courtplot <- function(feat) {
  feat <- substitute(feat)
  shot %>% 
    ggplot(aes(x = converted_x, y = converted_y)) +
    geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
    scale_color_brewer(palette = "Set1") +
    theme_void() +
    ggtitle(paste(feat))
}
courtplot(shot$type)

#Distance affecting the shot
all_shots_miss=filter(project, event_type=='miss'|event_type=='shot')
all_shots_miss$distance_bins <- cut(all_shots_miss$shot_distance, breaks = 10)
all_shots_miss$event_type=as.factor(all_shots_miss$event_type)

#The difference is in +geom_bar()
ggplot(data = all_shots_miss, aes(x = distance_bins)) + geom_bar() + ggtitle("Shot Distribution by distance") +
  theme(axis.text.x = element_blank())

#Shots miss and made for each distance range
ggplot(data = all_shots_miss, aes(x = distance_bins))+geom_bar(aes(fill = event_type), stat = "count", position = "fill") + ggtitle("Shot Distribution") +
  theme(axis.text.x = element_blank())

#Accuracy of the shot type
prob=prop.table(table(all_shots_miss$type, all_shots_miss$event_type),1) 
prob=as.data.frame.matrix(prob)
prob$type=rownames(prob)
ggplot(prob, aes(x = reorder(type, shot), y = 1)) +
  geom_point(aes(y = shot), size = 3, color = " dark blue", stat = "identity") +
  coord_flip() +
  labs(y = "Accuracy", x = "", title = "Accuracy by Shot_type")


all_shots_miss$time_bins <- cut(as.numeric(all_shots_miss$remaining_time), breaks = 12)
ggplot(data = all_shots_miss, aes(x = time_bins))+geom_bar(aes(fill = event_type), stat = "count", position = "fill") +
  theme(axis.text.x = element_blank())+ggtitle("Accuracy of shot by time remaining")

ggplot(data = all_shots_miss, aes(x = time_bins)) + geom_bar()+theme(axis.text.x = element_blank())


nm=all_shots_miss%>%
  group_by(shot_bins)%>%
  summarise(number=n())
for (i in 1:length(nm$shot_bins)){
  all_shots_miss$number_shots[all_shots_miss$shot_bins==nm$shot_bins[i]]=nm$number[i]
}

#Regression of final result

end_of_period= project%>%
  group_by(game_id)%>%
  filter(event_type=='end of period')%>%
  mutate(Mod.Diff.H.A=abs(home_score-away_score))
first_period=end_of_period%>%
  filter(period==1)%>%
  mutate(result=ifelse(home_score>away_score,'home',ifelse(away_score>home_score,'away','draw')))%>%
  select(result)
last_period=end_of_period%>%
  filter(period==4)%>%
  mutate(result=ifelse(home_score>away_score,'home',ifelse(away_score>home_score,'away','draw')))%>%
  select(result)
table(first_period$result,last_period$result)

#difference 10
first_period10=end_of_period%>%
  filter(period==1, Mod.Diff.H.A>=10)%>%
  mutate(result=ifelse(home_score>away_score,'home',ifelse(away_score>home_score,'away','draw')))%>%
  select(result)
last_period10=end_of_period[-3:-1,][end_of_period$period==1& end_of_period$Mod.Diff.H.A>=10,]%>%
  mutate(result=ifelse(home_score>away_score,'home',ifelse(away_score>home_score,'away','draw')))%>%
  select(result)
table(first_period10$result,last_period10$result)

kmeans.ani(total_points$total_points, 3)










