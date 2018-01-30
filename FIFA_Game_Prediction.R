#the goal here is to combine player FIFA data with 
library(readr)
library(dplyr)
Player_attr = read_csv('data/PlayerAttributeData.csv')
Player_pers = read_csv('data/PlayerPersonalData.csv')
options(warn=-1)
sum(is.na(Player_attr))
sum(is.na(Player_pers))

Player_stats_total = merge(Player_attr, Player_pers, by = 'X1')
sum(is.na(Player_stats_total))
#this is the sum of the two so we're good to go

epl_clubs = c('Manchester United', 'Chelsea', 'Manchester City',
              'Arsenal', 'Tottenham Hotspur', 'Leicester City',
              'Southampton', 'Everton', 'Swansea City', 'Stoke City', 'West Ham United', 'Bournemouth', 'Crystal Palace',
              'West Bromwich Albion', 'Burnley', 'Newcastle United', 'Liverpool', 'Huddersfield Town', 'Brighton & Hove Albion', 'Watford')
English_player_stats <- subset(Player_stats_total, Club %in% epl_clubs)

#there are some cells that have a plus sign, we want everything before that 

example = "95+2"
substring(example, 0, regexpr("+", example)+1)
#that works perfectly, let's do that on every

for(colindex in c(2:18,20:36)){
  English_player_stats[,colindex] = sapply(English_player_stats[,colindex], function(x) substring(x, 0, regexpr("+", x)+1) )
}

#there are tons of ways to test out if this worked
#my personal go to functions
sum(is.na(as.integer(English_player_stats$Acceleration)))
mean(as.integer(English_player_stats$Acceleration))

#now the next thing we need to do is create summaries for each team 


practice <- English_player_stats %>%
  mutate()
  group_by(Club) %>% 
  summarise(Acc = mean(as.integer(top_n(n=5, wt = Acceleration)$Acceleration)))

