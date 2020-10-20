print(paste('Start Time:',Sys.time()))
library(tidyverse)
library(baseballr)
library(tictoc)
tic('Run time')

# Dates for stats
start_date_p <- as.character(Sys.Date() - 30)
end_date <- as.character(Sys.Date() - 1)
start_date_b <- as.character(Sys.Date() - 15)

# Import FanDuel player list
players <- read.csv('baseball.csv')
players <- players %>% filter(Injury.Indicator == '') %>%
  select(Id, Position, Name = Nickname, FPPG, Played, Salary, Game,
         Team, Opponent, Probable.Pitcher, Batting.Order)
players$Value <- players$FPPG / (players$Salary / 1000)

# Rankings
# Join pitching ratings of opponents to offense players
p_stats <- daily_pitcher_bref(t1 = start_date_p, t2 = end_date)
p_stats$Name <- gsub('é','e',p_stats$Name)
d_rank <- players %>% filter(Position == 'P' & Probable.Pitcher == 'Yes') %>% arrange(FPPG)
d_rank <- left_join(d_rank, p_stats, by = 'Name')
d_rank$Rank <- ((d_rank$HR / d_rank$IP)*10) + d_rank$WHIP + d_rank$ERA +
  (d_rank$H / d_rank$IP) + (d_rank$BB / d_rank$IP) - (d_rank$SO9 * .1)
d_rank <- d_rank[,c('Team.x', 'Rank')]
colnames(d_rank) <- c('Opponent','Matchup')
offense <- players %>% filter(Position != 'P')
offense <- left_join(offense, d_rank, by = 'Opponent')

# Join offense ratings of opponents to defense teams
b_stats <- daily_batter_bref(t1 = start_date_b, t2 = end_date)
b_stats$Name <- gsub('é','e',b_stats$Name)
o_rank <- players %>% filter(Position != 'P')
o_rank <- left_join(o_rank, b_stats, by = 'Name')
o_rank$FPPG <- ((o_rank$X1B * 3) + (o_rank$X2B * 6) + (o_rank$X3B * 9) + (o_rank$HR * 12) +
                  (o_rank$BB * 3) + (o_rank$HBP * 3) + (o_rank$R * 3.2) +
                  (o_rank$RBI * 3.5) + (o_rank$SB * 6)) / o_rank$G
o_rank <- o_rank[complete.cases(o_rank$bbref_id),]
offense <- merge(offense, o_rank, all.x = T, by = c('Id','Position','Name','FPPG'))
offense <- offense[,1:13]
colnames(offense) <- c('Id','Position','Name','FPPG','Played','Salary','Game','Team',
                       'Opponent','Probable.Pitcher','Batting.Order','Value','Matchup')
o_team_rank <- players %>% filter(Position != 'P' & FPPG >= mean(FPPG)) %>% group_by(Team) %>%
  summarize(TeamFPPG = mean(FPPG)) %>% arrange(TeamFPPG)
o_team_rank$Rank <- nrow(o_team_rank):1
o_team_rank <- o_team_rank[,-2]
colnames(o_team_rank) <- c('Opponent','Matchup')
defense <- players %>% filter(Position == 'P')
defense <- left_join(defense, o_team_rank, by = 'Opponent')

players <- rbind(offense, defense)

pitchers <- players %>% filter(Probable.Pitcher == 'Yes' & Salary >= mean(Salary))
batters <- players %>% filter(Position != 'P'
                              #& Batting.Order != 0
                              )

batters$Position[batters$Position %in% c('C','1B')] <- 'C/1B'

cb1 <- batters %>% filter(batters$Position == 'C/1B')
b2 <- batters %>% filter(batters$Position == '2B')
b3 <- batters %>% filter(batters$Position == '3B')
ss <- batters %>% filter(batters$Position == 'SS')
of <- batters %>% filter(batters$Position == 'OF')

cb1 <- cb1 %>% filter(FPPG >= mean(FPPG) & Played >= mean(Played) & Salary >= mean(Salary))
b2 <- b2 %>% filter(FPPG >= mean(FPPG) & Played >= mean(Played) & Salary >= mean(Salary))
b3 <- b3 %>% filter(FPPG >= mean(FPPG) & Played >= mean(Played) & Salary >= mean(Salary))
ss <- ss %>% filter(FPPG >= mean(FPPG) & Played >= mean(Played) & Salary >= mean(Salary))
of <- of %>% filter(FPPG >= mean(FPPG) & Played >= mean(Played) & Salary >= mean(Salary))

lineup_comb <- data.frame(matrix(ncol = 14, nrow = 0))
colnames(lineup_comb) <- c('P','CB1','B2','B3','SS',
                           'OF1','OF2','OF3','UTIL','Salary','FPPG',
                           'Matchup','Value','Dup')

for (i in 1:50000) {
  p1 <- pitchers[sample(nrow(pitchers),1), ]
  p2 <- cb1[sample(nrow(cb1),1), ]
  p3 <- b2[sample(nrow(b2),1), ]
  p4 <- b3[sample(nrow(b3),1), ]
  p5 <- ss[sample(nrow(ss),1), ]
  p68 <- of[sample(nrow(of),3), ]
  p9 <- batters[sample(nrow(batters),1), ]
  lineup <- rbind(p1[1:13],p2[1:13],p3[1:13],p4[1:13],p5[1:13],p68[1:13],p9[1:13])
  bat_stats <- rbind(p2,p3,p4,p5,p68,p9)
  lineup_check <- lineup %>% group_by(Team) %>% mutate(Count = n())
  if(max(lineup_check$Count) < 5){
    new_row <- data.frame(P = paste(p1$Id,p1$Name,sep = ':'),
                          CB1 = paste(p2$Id,p2$Name,sep = ':'),
                          B2 = paste(p3$Id,p3$Name,sep = ':'),
                          B3 = paste(p4$Id,p4$Name,sep = ':'),
                          SS = paste(p5$Id,p5$Name,sep = ':'),
                          OF1 = paste(p68$Id[1],p68$Name[1],sep = ':'),
                          OF2 = paste(p68$Id[2],p68$Name[2],sep = ':'),
                          OF3 = paste(p68$Id[3],p68$Name[3],sep = ':'),
                          UTIL = paste(p9$Id,p9$Name,sep = ':'),
                          Salary = sum(lineup$Salary),
                          FPPG = sum(lineup$FPPG),
                          Matchup = sum(lineup$Matchup) * 0.75,
                          Value = sum(lineup$Value)/2)
    new_row$Score <- as.numeric(new_row$FPPG) + as.numeric(new_row$Matchup) +
      as.numeric(new_row$Value)
    new_row[t(apply(new_row,1,duplicated))] <- NA
    if(any(is.na(new_row))){
      next } else {
      if(new_row$Salary <= 35000){
        lineup_comb <- rbind(lineup_comb,new_row)
      }
    }
  }
  if(i %in% seq(from = 5000, to = 45000, by = 5000)){
    pct <- i / 500
    print(paste(pct,'% Complete', sep = ''))
  }
}

lineup_comb <- lineup_comb[order(-lineup_comb$Score),1:ncol(lineup_comb)]
lineup_comb <- lineup_comb %>% dplyr::distinct()
top50 <- lineup_comb[1:50,]
write.csv(top50, 'optimal_baseball_fanduel.csv')
rm(list = ls())
print(paste('End Time:',Sys.time()))
toc()
