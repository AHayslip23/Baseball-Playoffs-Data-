baseball <- read.csv('Teams2020.csv')

library(dplyr)

baseball <- baseball %>%
  mutate(baseball, DivisionWinner = ifelse(DivWin == 'Y', 1, 0))

View(baseball)

baseball <- baseball %>%
  mutate(baseball, WildCardWinner = ifelse(WCWin == 'Y', 1, 0))

baseball <- baseball %>%
  mutate(baseball, LeagueWinner = ifelse(LgWin == 'Y', 1, 0))

baseball <- baseball %>%
  mutate(baseball, WSWinner = ifelse(WSWin == 'Y', 1, 0))

baseball <- baseball %>%
  mutate(baseball, playoffs = ifelse(DivisionWinner == 1 |WildCardWinner == 1, 1, 0))

WildCard <- baseball %>%
  filter(yearID == '1995' | yearID =='1996' | yearID == '1997' | yearID == '1998' | yearID =='1999' | yearID =='2000' | yearID =='2001' | yearID =='2002' | yearID =='2003' | yearID =='2004' | yearID =='2005' | yearID =='2006' | yearID =='2007' | yearID =='2008' | yearID =='2009' | yearID =='2010' | yearID =='2011' | yearID =='2012' | yearID == '2013' | yearID == '2014' | yearID == '2015' | yearID == '2016' | yearID == '2017'| yearID == '2018' | yearID == '2019')

View(WildCard)

View(WildCard1Statistics)

WildCard1Statistics <- WildCard %>%
  select(Rank, G, Ghome, L, R, AB, H, X2B, X3B, HR, BB, SO, SB, HBP, SF, RA, ER, ERA, CG, SHO, SV, IPouts, HA, HRA, BBA, SOA, E, DP, W)

library(caTools)
split = sample.split(WildCard1Statistics$W, SplitRatio = 0.75)
train = subset(WildCard1Statistics, split == T)
test = subset(WildCard1Statistics, split == F)

View(train)
View(test)

regressor <- lm(formula = W ~. ,
                data = train)

summary(regressor)

#remove X3B triples 

regressor <- lm(formula = W ~ G + Ghome + L + R + AB + H + X2B + RA + HR + BB + SO + SB +  HBP + SF + RA + ER + ERA + CG + SHO + SV + IPouts + HA + HRA + BBA + SOA + E + DP,
                data = train)
summary(regressor)

#remove saves 
regressor <- lm(formula = W ~ G + Ghome + L + R + AB + H + X2B + RA + HR + BB + SO + SB +  HBP + SF + RA + ER + ERA + CG + SHO + IPouts + HA + HRA + BBA + SOA + E + DP,
                data = train)
summary(regressor)

#remove shutouts 
regressor <- lm(formula = W ~ G + Ghome + L + R + AB + H + X2B + RA + HR + BB + SO + SB +  HBP + SF + RA + ER + ERA + CG  + IPouts + HA + HRA + BBA + SOA + E + DP,
                data = train)
summary(regressor)

#remove runs allowed 
regressor <- lm(formula = W ~ G + Ghome + L + R + AB + H + X2B + HR + BB + SO + SB +HBP + SF + ER + ERA + CG  + IPouts + HA + HRA + BBA + SOA + E + DP,
                data = train)
summary(regressor)

#IP Outs needs to be removed 
regressor <- lm(formula = W ~ G + Ghome + L + R + AB + H + X2B + HR + BB + SO + SB +HBP + SF + ER + ERA + CG + HA + HRA + BBA + SOA + E + DP,
                data = train)
summary(regressor)

#remove doubles 
regressor <- lm(formula = W ~ G + Ghome + L + R + AB + H + HR + BB + SO + SB +HBP + SF + ER + ERA + CG + HA + HRA + BBA + SOA + E + DP,
                data = train)
summary(regressor)

#remove hits 
regressor <- lm(formula = W ~ G + Ghome + L + R + AB + HR + BB + SO + SB +HBP + SF + ER + ERA + CG + HA + HRA + BBA + SOA + E + DP,
                data = train)
summary(regressor)

#home runs allowed 
regressor <- lm(formula = W ~ G + Ghome + L + R + AB + HR + BB + SO + SB +HBP + SF + ER + ERA + CG + HA + BBA + SOA + E + DP,
                data = train)
summary(regressor)

#remove home games 
regressor <- lm(formula = W ~ G + L + R + AB + HR + BB + SO + SB +HBP + SF + ER + ERA + CG + HA + BBA + SOA + E + DP,
                data = train)
summary(regressor)

#remove errors 
regressor <- lm(formula = W ~ G + L + R + AB + HR + BB + SO + SB +HBP + SF + ER + ERA + CG + HA + BBA + SOA + DP,
                data = train)
summary(regressor)

#remove ERA 
regressor <- lm(formula = W ~ G + L + R + AB + HR + BB + SO + SB +HBP + SF + ER + CG + HA + BBA + SOA + DP,
                data = train)
summary(regressor)

#remove AB's 
regressor <- lm(formula = W ~ G + L + R + HR + BB + SO + SB +HBP + SF + ER + CG + HA + BBA + SOA + DP,
                data = train)
summary(regressor)

#remove SB's 
regressor <- lm(formula = W ~ G + L + R + HR + BB + SO +HBP + SF + ER + CG + HA + BBA + SOA + DP,
                data = train)
summary(regressor)

#remove BB's 
regressor <- lm(formula = W ~ G + L + R + HR + SO +HBP + SF + ER + CG + HA + BBA + SOA + DP,
                data = train)
summary(regressor)

#remove HBP's 
regressor <- lm(formula = W ~ G + L + R + HR + SO + SF + ER + CG + HA + BBA + SOA + DP,
                data = train)
summary(regressor)

#remove Runs 
regressor <- lm(formula = W ~ G + L + HR + SO + SF + ER + CG + HA + BBA + SOA + DP,
                data = train)
summary(regressor)

#remove Strikeouts 
regressor <- lm(formula = W ~ G + L + HR + SF + ER + CG + HA + BBA + SOA + DP,
                data = train)
summary(regressor)

#remove HR 
regressor <- lm(formula = W ~ G + L + SF + ER + CG + HA + BBA + SOA + DP,
                data = train)
summary(regressor)

#99.8% 
#formula = W ~ G + L + SF + ER + CG + HA + BBA + SOA + DP 

View(WildCard1)

WildCardWinners <- WildCard %>%
  filter(WCWin == 'Y')
View(WildCardWinners)

library(ggplot2)
WildCardTeams <- ggplot(data = WildCardWinners, aes(x=teamID)) + geom_bar(stat="count", color = "red", fill="blue")
WildCardTeams
#the team that benefited the most from that wild card being added was the red sox 

#how many wins did it usually take to qualify as a wild card 
WildCardTeams <- ggplot(data = WildCardWinners, aes(x=W)) + geom_bar(stat="count", color="red", fill="blue")
WildCardTeams
#most wild card teams from 1995 to 2012 required 95 wins to qualify 
#the next highest was 90 

#Which Divisions got Most of the Wild Cards 
WildCardDivisions <- ggplot(data = WildCardWinners, aes(x=divID)) + geom_bar(stat="count", color = "yellow", fill="green") 
WildCardDivisions
#the east divisions (AL East and NL East benefited the most from adding the first Wild Card)


playoffs <- baseball

playoffs


playoffs <- playoffs %>%
  filter(playoffs == 1)

View(playoffs)

#correaltion beween BB and Runs for playoff teams  
Hatteberg <- playoffs %>%
  select(BB, R)

split = sample.split(Hatteberg$R, SplitRatio = 0.75)
train = subset(Hatteberg, split == T)
test = subset(Hatteberg, split == F)

regressor <- lm(formula = R ~ BB,
                data = train)
summary(regressor)

y_pred = predict(regressor, newdata = train)
y_pred

ggplot() + 
  geom_point(aes(x=train$BB, y=train$R),
             color = "yellow") + 
  geom_line(aes(x=train$BB, y=predict(regressor, newdata = train)),
            color ="green") + 
  ggtitle("correlation between Walks and Runs for Playoff Teams") + 
  xlab("Walks") + 
  ylab("Runs")

#there is a moderate correlation betweeb walks and runs scored 

#what about correlation between home runs and runs scored? 
threeoutcome <- playoffs %>%
  select(HR, R, BB, SO)

split = sample.split(threeoutcome$R, splitRatio = 0.75)
train = subset(threeoutcome, split == T)
test = subset(threeoutcome, split == F)

regressor <- lm(formula = R ~ HR,
                data = train)
summary(regressor)

y_pred = predict(regressor, newdata = test)
print(y_pred)

ggplot() + 
  geom_point(aes(x=train$HR, y=train$R),
             color = "blue") +
  geom_line(aes(x=train$HR, y=predict(regressor, newdata = train)),
            color = "orange") + 
  ggtitle("Correaltion Between Home Runs and Runs for Playoff Teams") + 
  xlab("Home Runs") + 
  ylab("Runs")
  



#team that wins the division most frequently 
DivisionWinnersData <- baseball %>%
  filter(DivWin == 'Y')
View(DivisionWinnersData)

DivisionWinners <- ggplot(data=DivisionWinnersData, aes(teamID)) + geom_bar(stat="count", color="red", fill="black")
DivisionWinners
#atlanta has the most division titles 
#second place is the dodgers
#third place are the yankees 

#for teams that have won the division, anaheim is the team that wins the division the least frequently 

#multiple regression runs for playoff teams 
#what factors can predict why runs are scored 

View(playoffs)

playoffs <- playoffs %>%
  filter(yearID == '1995' | yearID =='1996' | yearID == '1997' | yearID == '1998' | yearID =='1999' | yearID =='2000' | yearID =='2001' | yearID =='2002' | yearID =='2003' | yearID =='2004' | yearID =='2005' | yearID =='2006' | yearID =='2007' | yearID =='2008' | yearID =='2009' | yearID =='2010' | yearID =='2011' | yearID =='2012' | yearID == '2013' | yearID == '2014' | yearID == '2015' | yearID == '2016' | yearID == '2017'| yearID == '2018' | yearID == '2019')

playoffs <- playoffs %>%
  filter(playoffs == 1)

runs <- playoffs %>%
  select(X2B, X3B, HR, BB, SO, SB, CS, HBP, SF, R)

library(caTools)
set.seed(123)

split = sample.split(runs$R, SplitRatio = 0.75)
train = subset(runs, split == T) 
test = subset(runs, split == F)

regressor <- lm(formula = R ~. ,
                data = train)

summary(regressor)

#get rid of caught stealing 
regressor <- lm(formula = R ~ X2B + X3B + HR + BB + SO + SB + HBP + SF,
                data = train)
summary(regressor)
#get rid of hit by pitches 

regressor <- lm(formula = R ~ X2B + X3B + HR + BB + SO + SB + SF,
                data = train)
summary(regressor)

#all variables in the model are statistically significant, 82.74% correlation 
#R = X2B + X3B + HR + BB + SO + SB + SF 
  

View(runs)

RunsAllowed <- playoffs %>%
  select(ER, ERA, CG, SHO, SV, IPouts, HA, HRA, BBA, SOA, E, DP, RA)

set.seed(123)
split = sample.split(RunsAllowed$RA, SplitRatio = 0.75)
train = subset(RunsAllowed, split == T) 
test = subset(RunsAllowed, split == F)

regressor <- lm(formula = RA ~. ,
                data = train)
summary(regressor)

#remove double plays 
regressor <- lm(formula = RA ~ ER + ERA + CG + SHO + SV + IPouts + HA + HRA + BBA + SOA + E,
                data = train)

summary(regressor)

#remove home runs allowed 
regressor <- lm(formula = RA ~ ER + ERA + CG + SHO + SV + IPouts + HA + BBA + SOA + E,
                data = train)

summary(regressor)

#remove walks allowed 
regressor <- lm(formula = RA ~ ER + ERA + CG + SHO + SV + IPouts + HA + SOA + E,
                data = train)

summary(regressor)

#remove hits allowed 
regressor <- lm(formula = RA ~ ER + ERA + CG + SHO + SV + IPouts + SOA + E,
                data = train)

summary(regressor)

#remove complete games 
regressor <- lm(formula = RA ~ ER + ERA + SHO + SV + IPouts + SOA + E,
                data = train)

summary(regressor)

#remove shutouts 
regressor <- lm(formula = RA ~ ER + ERA + SV + IPouts + SOA + E,
                data = train)

summary(regressor)

#remove inning pitches outs
regressor <- lm(formula = RA ~ ER + ERA + SV + SOA + E,
                data = train)

summary(regressor)

#remove ERA 
regressor <- lm(formula = RA ~ ER + SV + SOA + E,
                data = train)

summary(regressor)

#remove strikeouts 
regressor <- lm(formula = RA ~ ER + SV + E,
                data = train)

summary(regressor)

#remove saves 
regressor <- lm(formula = RA ~ ER + E,
                data = train)

summary(regressor)

#histograms runs scored for playoff teams 
runshistogram <- ggplot(data=playoffs, aes(x=R)) + geom_histogram(bins=10, binwidth=10, alpha = 0.7, color = 'red', fill='black')
runshistogram
#between 795 and 805 runs 

runsallowedhistrogram <- ggplot(data=playoffs, aes(x=RA)) + geom_histogram(bins=10, binwidth=10, alpha=0.7, color = 'green', fill = 'blue')
runsallowedhistrogram
#between 665 and 675 runs 

playoffswinsboxplot <- ggplot(data=playoffs, aes(x=franchID, y=W)) + geom_boxplot(aes(fill=teamID))
playoffswinsboxplot

padresplayoffs <- playoffs %>%
  filter(franchID == 'SDP')
View(padresplayoffs)

padresplayoffsbarplot <- ggplot(data=padresplayoffs, aes(x=yearID, y=W)) + geom_bar(stat="identity", color = "yellow", fill="brown")
padresplayoffsbarplot

dodgersplayoffs <- playoffs %>%
  filter(franchID == 'LAD')
View(dodgersplayoffs)

dodgersplayoffsbarplot <- ggplot(data=dodgersplayoffs, aes(x=yearID, y=W)) + geom_bar(stat="identity", color = "red", fill="blue")
dodgersplayoffsbarplot   

worldserieswinners <- playoffs %>%
  filter(WSWinner == 1)

View(worldserieswinners)

worldseries <- ggplot(data=worldserieswinners, aes(x=DivWin)) + geom_bar(stat="count", color = "red", fill="yellow")
worldseries
#most teams that have won the world series from the inception of the wild card era have won the division 
#7 teams have won the world series as wild cards 
#18 teams have won the world series as division winners 

View(americanleaguewinners)

americanleaguewinners <- playoffs %>%
  filter(lgID == 'AL')


americanleaguewinners <- americanleaguewinners %>%
  filter(LeagueWinner == 1)

americanleaguechampion <- ggplot(data=americanleaguewinners, aes(x=DivWin)) + geom_bar(stat="count", color="red", fill="blue")
americanleaguechampion
#four times a wild card winner came out of the american league to make the world series 
#most recently 2014 kansas city royals 

nationalleaguewinners <- playoffs %>%
  filter(lgID == 'NL')

nationalleaguewinners <- nationalleaguewinners %>%
  filter(LeagueWinner == 1)

View(nationalleaguewinners)


nationalleaguechampion <- ggplot(data=nationalleaguewinners, aes(x=DivWin)) + geom_bar(stat="count", color="red", fill="blue")
nationalleaguechampion

#nine times a wild card winner came out of the national league to make the world series 
#most recently 2019 washington nationals

View(worldserieswinners)

worldserieschampions <- ggplot(data=worldserieswinners, aes(x=franchID)) + geom_bar(stat="count", color="red", fill="green")
worldserieschampions
#out of 25 observations
#yankees have won 5
#red sox have won 4
#giants won three 
#cardinals and marlins won 2

americanleaguerep <- ggplot(data=americanleaguewinners, aes(x=franchID)) + geom_bar(stat="count", color="red", fill="black")
americanleaguerep
#yankees and red sox made the world series most frequently out of this group 

nationalleaguerep <- ggplot(data=nationalleaguewinners, aes(x=franchID)) + geom_bar(stat="count", color="red", fill="black")
nationalleaguerep