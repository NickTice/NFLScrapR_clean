library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)

# Random Forest
library(randomForest)
library(ROCR)
library(MLmetrics)
library(DataCombine)


seasons <- 2014:2018
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

pbp_rp <- pbp %>%
  filter(rush == 1 | pass == 1, !is.na(epa))

games.trainO = pbp_rp %>%
  filter(down <=4) %>%
  group_by(game_id,posteam) %>%
  summarize(
    posteam_type=max(posteam_type),home_team=max(home_team),away_team=max(away_team), week=max(week), home_spread=-max(spread_line), away_spread=max(spread_line),total=max(total_line),home_score=max(home_score), away_score=max(away_score), 
    win=as.integer(max(home_score)>max(away_score)), cover= as.integer((home_score-away_score)>=away_spread),over=as.integer((max(home_score)+max(away_score))>=total),
    roof=max(roof), surface=max(surface), temp=max(temp), wind=max(wind), stadium=max(stadium_id),
    shotgun.O=sum(shotgun)/n(),no_huddle.O=sum(no_huddle)/n(), qb_scramble.O=sum(qb_scramble)/n(), qb_hit.O=sum(qb_hit, na.rm = TRUE)/sum(pass), pass.O=sum(pass),
    air_yds.O=median(air_yards, na.rm = TRUE), cp.O=median(cp, na.rm=TRUE),qb_epa=median(qb_epa), epa.O=median(epa), rush_epa=median(ifelse(pass==0, epa, NA), na.rm = TRUE), pass_epa=median(ifelse(pass==1, epa, NA), na.rm = TRUE),                                      
    first_down_perc.O=sum(first_down)/n(), first_down_rp_ratio.O=sum(first_down_rush)/sum(first_down_pass), third_down_conversion.O=sum(third_down_converted)/(sum(third_down_converted)+sum(third_down_failed)),
    interception.O=sum(interception)/sum(pass), penalty.O=sum(penalty)/n(), tackled_for_loss.O=sum(tackled_for_loss)/(n()-sum(pass)), fumble_lost.O=sum(fumble_lost)/n(), sack.O=sum(sack)/sum(pass) 
  )  

games.trainD = pbp_rp %>%
  filter(down <=4) %>%
  group_by(game_id, defteam) %>%
  summarize(
    qb_hit.D=sum(qb_hit, na.rm = TRUE)/sum(pass), pass.D=sum(pass),air_yds.D=median(air_yards, na.rm = TRUE), 
    cp.D=median(cp, na.rm=TRUE),epa.D=median(epa), first_down_perc.D=sum(first_down)/n(), first_down_rp_ratio.D=sum(first_down_rush)/sum(first_down_pass), third_down_conversion.D=sum(third_down_converted)/(sum(third_down_converted)+sum(third_down_failed)),
    interception.D=sum(interception)/sum(pass), penalty.D=sum(penalty)/n(), tackled_for_loss.D=sum(tackled_for_loss)/(n()-sum(pass)), sack.D=sum(sack)/sum(pass) 
  )  

x=1:nrow(games.trainD)
games.trainD$order=x

x=1:nrow(games.trainO)
games.trainO$order=x


games.train=merge(games.trainO, games.trainD, by.x="order", by.y="order")


# indexing 
odd_indexes<-seq(1,nrow(games.train),2)
even_indexes=seq(2,nrow(games.train),2)
games.odd=games.train[odd_indexes,]
games.even=games.train[even_indexes,]

# Spread, win and cover simplified into one column
games.even$spread=games.even$home_spread
games.odd$spread=games.odd$away_spread
games.odd$win=as.integer(!games.odd$win)
games.odd$cover=as.integer(!games.odd$cover)


# rearrange by order
games = bind_rows(games.odd, setNames(games.even, names(games.odd))) %>% 
  arrange(order)


games=subset(games, select = -c(home_spread, away_spread, week))
games=subset(games, select = -c(game_id.x, game_id.y, order))
games <- games[, c(1:4, 48, 5:47)]

# character to factor
games.simp=games
games.simp$posteam_type=as.factor(games.simp$posteam_type)
games.simp$roof=as.factor(games.simp$roof)
games.simp$stadium=as.factor(games.simp$stadium)
games.simp$surface=as.factor(games.simp$surface)
# temp 70 and wind 0 for na in those categories
games.simp$temp=ifelse(is.na(games.simp$temp)==TRUE, 70, games.simp$temp)
games.simp$wind=ifelse(is.na(games.simp$wind)==TRUE, 0, games.simp$wind)

games.simp=DropNA(games.simp, "home_score")
games.simp=DropNA(games.simp, "third_down_conversion.O")

# more column simplification
score.O = ifelse(games.simp$posteam_type=="away", games.simp$away_score, games.simp$home_score)
score.D = ifelse(games.simp$posteam_type=="away", games.simp$home_score, games.simp$away_score)

pass_epa.D = -games.simp$pass_epa
rush_epa.D = -games.simp$rush_epa

odd_indexes<-seq(1,nrow(games.simp),2)
even_indexes=seq(2,nrow(games.simp),2)
index=c(rbind(even_indexes,odd_indexes))

pass_epa.D=pass_epa.D[index]
rush_epa.D=rush_epa.D[index]

games.simp=cbind(games.simp, score.O,  score.D, pass_epa.D, rush_epa.D)


games.final=subset(games.simp, select = -c(posteam, defteam, qb_scramble.O, no_huddle.O, pass.D, home_score, away_score, home_team, away_team, pass.O, 
                                           qb_epa))


games.final=games.final[,c(2,3,1,7,8,11,9,10,12,13,14:35,37,40,41,36,38,39,5,6,4)]


