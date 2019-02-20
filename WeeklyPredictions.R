# pbp = data frame with play by play data from 2009-2018
# Type
#   1: Handoff
#   2: Dropback
#   3: Dropback resulting in a penalty
#   4: Handoff resulting in a penalty

#Summarise weekly offensive stats
off_w <- pbp %>% filter(!is.na(epa) & Type %in% c(1,2,3,4)) %>%
  group_by(posteam,Season,week) %>%
  summarise(OffRunPlays=sum(Type==1)+sum(Type==4),
            OffDropbacks=sum(Type==2)+sum(Type==3),
            OffPlays=n(),
            OffEPAperRun=sum((Type==1)*epa+(Type==4)*epa)/OffRunPlays,
            OffEPAperDropback=sum((Type==2)*epa+(Type==3)*epa)/OffDropbacks,
            OffEPAperPlay=sum(epa)/n()) %>%
  rename(Team=posteam) %>% filter(OffRunPlays > 1)

#Summarise weekly defensive stats
def_w <- pbp %>% filter(!is.na(epa) & Type %in% c(1,2,3,4)) %>%
  group_by(defteam,Season,week) %>%
  summarise(DefRunPlays=sum(Type==1)+sum(Type==4),
            DefDropbacks=sum(Type==2)+sum(Type==3),
            DefPlays=n(),
            DefEPAperRun=sum((Type==1)*epa+(Type==4)*epa)/DefRunPlays,
            DefEPAperDropback=sum((Type==2)*epa+(Type==3)*epa)/DefDropbacks,
            DefEPAperPlay=sum(epa)/n()) %>%
  rename(Team=defteam) %>% filter(DefRunPlays > 1)

#Join into one data frame.
#Compute some other columns
teams_w <- off_w %>% left_join(def_w,by=c("Team","Season","week"))
teams_w <- teams_w %>% mutate(DropbackDiff = OffEPAperDropback - DefEPAperDropback,
                              RunDiff = OffEPAperRun - DefEPAperRun,
                              EPAperPlayDiff = OffEPAperPlay - DefEPAperPlay,
                              OffDef = OffEPAperPlay + DefEPAperPlay, #Indicates whether offense or defense was better in the game
                              Plays = OffPlays + DefPlays)

#teamgames = A dataframe with all games of all teams.
#Join the game results to the data frame
teams_w <- teams_w %>% left_join(teamgames,by=c("Team","Season","week"))

#Add a column for the game number
teams_w <- teams_w %>% ungroup(week) %>% group_by(Team,Season) %>% mutate(Game=row_number(week))

#Resort the colums for convenience
teams_w <- teams_w %>% select(game_id,Team,Season,week,Game,Opp,Home,For,Allowed,PtsDiff,Wins,Losses,OffRunPlays:Plays)

#Add a columns with cumulative data from previous games
teams_w <- teams_w %>% arrange(Team,Season,week) %>%
          group_by(Team,Season) %>%
          mutate(Wins_cum = cumsum(Wins)[Game],
                 Losses_cum = cumsum(Losses)[Game],
                 PtsDiff_cum = cumsum(PtsDiff)[Game],
                 OffEPAperPlay_cum = cummean(OffEPAperPlay)[Game],
                 OffEPAperRun_cum = cummean(OffEPAperRun)[Game],
                 OffEPAperDropback_cum = cummean(OffEPAperDropback)[Game],
                 DefEPAperPlay_cum = cummean(DefEPAperPlay)[Game],
                 DefEPAperRun_cum = cummean(DefEPAperRun)[Game],
                 DefEPAperDropback_cum = cummean(DefEPAperDropback)[Game])

#Shift the columns to throw away information about the current game
teams_w <- teams_w %>% mutate(Wins_pre = lag(Wins_cum),
                              Losses_pre = lag(Losses_cum),
                              PtsDiff_pre = lag(PtsDiff_cum),
                              OffEPAperPlay_pre = lag(OffEPAperPlay_cum),
                              OffEPAperRun_pre = lag(OffEPAperRun_cum),
                              OffEPAperDropback_pre = lag(OffEPAperDropback_cum),
                              DefEPAperPlay_pre = lag(DefEPAperPlay_cum),
                              DefEPAperRun_pre = lag(DefEPAperRun_cum),
                              DefEPAperDropback_pre = lag(DefEPAperDropback_cum))

#Add columns for last season numbers
teams_s <- teams_w %>% group_by(Team,Season) %>%
           summarise(OffEPAperPlay_last_year = mean(OffEPAperPlay),
                     DefEPAperPlay_last_year = mean(DefEPAperPlay)) %>% ungroup() %>%
           mutate(Season = Season + 1) %>% filter(Season < 2019)
teams_w <- teams_w %>% left_join(teams_s,by=c("Team","Season"))

#Compute R^2. Metrics predict itself.
rsquares <- teams_w %>% ungroup() %>% filter(Game > 1) %>%
            group_by(Game) %>%
            summarise(O = cor(OffEPAperPlay,OffEPAperPlay_pre)^2,
                      OR = cor(OffEPAperRun,OffEPAperRun_pre)^2,
                      OD = cor(OffEPAperDropback,OffEPAperDropback_pre)^2,
                      D = cor(DefEPAperPlay,DefEPAperPlay_pre)^2,
                      DR = cor(DefEPAperRun,DefEPAperRun_pre)^2,
                      DD = cor(DefEPAperDropback,DefEPAperDropback_pre)^2,
                      WL = cor(Wins,Wins_pre/(Game-1))^2,
                      PD = cor(PtsDiff,PtsDiff_pre)^2
                      )
#Plot them
a <- ggplot(data=rsquares,aes(x=Game)) + theme_classic() +  theme(panel.background = element_rect(fill="#eeeeee"),
                                                                  plot.background = element_rect(fill="#eeeeee"),
                                                                  legend.background = element_rect(fill="#eeeeee"))
a <- a + geom_smooth(aes(y=O,color="EPA/play",linetype="Offense"),se=F)# + geom_point(aes(y=O,color="Offense")) 
a <- a + geom_smooth(aes(y=OD,color="EPA/dropback",linetype="Offense"),se=F)# + geom_point(aes(y=OD,color="Pass Offense"))
a <- a + geom_smooth(aes(y=OR,color="EPA/run",linetype="Offense"),se=F)# + geom_point(aes(y=OR,color="Run Offense"))
a <- a + geom_smooth(aes(y=D,color="EPA/play",linetype="Defense"),se=F)# + geom_point(aes(y=D,color="Defense"))
a <- a + geom_smooth(aes(y=DD,color="EPA/dropback",linetype="Defense"),se=F)# + geom_point(aes(y=DD,color="Pass Defense"))
a <- a + geom_smooth(aes(y=DR,color="EPA/run",linetype="Defense"),se=F)# + geom_point(aes(y=DR,color="Run Defense"))
a <- a + geom_smooth(aes(y=PD,color="Point Differential"),se=F)# + geom_point(aes(y=PD,color="Point Differential"))
a <- a + geom_smooth(aes(y=WL,color="Win%"),se=F)# + geom_point(aes(y=WL,color="Win/Loss"))
a <- a + scale_x_continuous(breaks=2:16)
a <- a + scale_y_continuous(breaks=0.01*c(0,2.5,5,7.5,10,12.5),labels=scales::percent_format())
a <- a + scale_color_brewer(name="Metric",palette = "Set1")
a <- a + scale_linetype_manual(values=c("dotted","dashed"),name="Off/Def")
a <- a + labs(x="Game #", y="R^2 when predicting the metric itself with previous games")
ggsave("Viz/WeeklyPredictions.png",a,dpi=800,width=9,height=6)


#Compute R^2. Metrics predict point differential
rsquares_PD <- teams_w %>% ungroup() %>% filter(Game > 1) %>%
  group_by(Game) %>%
  summarise(O = cor(PtsDiff,OffEPAperPlay_pre)^2,
            OR = cor(PtsDiff,OffEPAperRun_pre)^2,
            OD = cor(PtsDiff,OffEPAperDropback_pre)^2,
            D = cor(PtsDiff,DefEPAperPlay_pre)^2,
            DR = cor(PtsDiff,DefEPAperRun_pre)^2,
            DD = cor(PtsDiff,DefEPAperDropback_pre)^2,
            WL = cor(PtsDiff,Wins_pre/(Game-1))^2,
            PD = cor(PtsDiff,PtsDiff_pre)^2,
            OAndD = summary(lm(PtsDiff ~ OffEPAperPlay_pre + DefEPAperPlay_pre))$r.squared
  )

a <- ggplot(data=rsquares_PD,aes(x=Game)) + theme_classic() + theme(panel.background = element_rect(fill="#eeeeee"),
                                                                    plot.background = element_rect(fill="#eeeeee"),
                                                                    legend.background = element_rect(fill="#eeeeee"))
a <- a + geom_smooth(aes(y=O,color="EPA/play",linetype="Offense"),se=F)# + geom_point(aes(y=O,color="Offense")) 
a <- a + geom_smooth(aes(y=OD,color="EPA/dropback",linetype="Offense"),se=F)# + geom_point(aes(y=OD,color="Pass Offense"))
a <- a + geom_smooth(aes(y=OR,color="EPA/run",linetype="Offense"),se=F)# + geom_point(aes(y=OR,color="Run Offense"))
a <- a + geom_smooth(aes(y=D,color="EPA/play",linetype="Defense"),se=F)# + geom_point(aes(y=D,color="Defense"))
a <- a + geom_smooth(aes(y=DD,color="EPA/dropback",linetype="Defense"),se=F)# + geom_point(aes(y=DD,color="Pass Defense"))
a <- a + geom_smooth(aes(y=DR,color="EPA/run",linetype="Defense"),se=F)# + geom_point(aes(y=DR,color="Run Defense"))
a <- a + geom_smooth(aes(y=PD,color="Point Differential"),se=F)# + geom_point(aes(y=PD,color="Point Differential"))
a <- a + geom_smooth(aes(y=WL,color="Win%"),se=F)# + geom_point(aes(y=WL,color="Win/Loss"))
a <- a + geom_smooth(aes(y=OAndD,color="Z Off + Def"),se=F)# + geom_point(aes(y=WL,color="Win/Loss"))
a <- a + scale_x_continuous(breaks=2:16)
a <- a + scale_y_continuous(breaks=0.01*c(0,2.5,5,7.5,10,12.5),labels=scales::percent_format())
a <- a + scale_color_brewer(name="Metric",palette = "Set1")
a <- a + scale_linetype_manual(values=c("dotted","dashed"),name="Off/Def")
a <- a + labs(x="Game #", y="R^2 when predicting point differential with previous games")
ggsave("Viz/WeeklyPredictions_PD.png",a,dpi=800,width=9,height=6)


#Compute R^2 with preseason priors
rsquares_PD_priors <- teams_w %>% ungroup() %>% filter(Game > 1 & Season > 2009) %>%
                      group_by(Game) %>%
                      summarise(OAndD = summary(lm(PtsDiff ~ OffEPAperPlay_pre +
                                                             DefEPAperPlay_pre))$r.squared,
                                OAndD_prior = summary(lm(PtsDiff ~ OffEPAperPlay_pre +
                                                                   DefEPAperPlay_pre + 
                                                                   OffEPAperPlay_last_year + 
                                                                   DefEPAperPlay_last_year))$r.squared,
                                prior = summary(lm(PtsDiff ~ OffEPAperPlay_last_year + 
                                                     DefEPAperPlay_last_year))$r.squared
                               )

a <- ggplot(data=rsquares_PD_priors,aes(x=Game)) + theme_classic() + theme(legend.position = "bottom",
                                                                           panel.background = element_rect(fill="#eeeeee"),
                                                                           plot.background = element_rect(fill="#eeeeee"),
                                                                           legend.background = element_rect(fill="#eeeeee"))
a <- a + geom_smooth(aes(y=OAndD,color="Previous games"),se=F) + geom_point(aes(y=OAndD,color="Previous games"))
a <- a + geom_smooth(aes(y=OAndD_prior,color="Previous games + last season"),se=F) + geom_point(aes(y=OAndD_prior,color="Previous games + last season"))
a <- a + geom_smooth(aes(y=prior,color="Last season"),se=F) + geom_point(aes(y=prior,color="Last season"))
a <- a + scale_x_continuous(breaks=2:16)
a <- a + scale_y_continuous(breaks=0.01*c(0,2.5,5,7.5,10,12.5,15),labels=scales::percent_format())
a <- a + scale_color_brewer(name="",palette = "Set1")
a <- a + labs(x="Game #", y="R^2 when predicting point differential with previous team efficiency")
ggsave("Viz/WeeklyPredictions_PD_prior.png",a,dpi=800,width=8,height=7)



