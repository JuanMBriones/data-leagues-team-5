library(fbRanks)
library(dplyr)
library(ggplot2)

d1011 <- read.csv("https://github.com/JuanMBriones/data-leagues-team-5/raw/fred/PostWork-8/csv/SP1-1011.csv")
d1112 <- read.csv("https://github.com/JuanMBriones/data-leagues-team-5/raw/fred/PostWork-8/csv/SP1-1112.csv")
d1213 <- read.csv("https://github.com/JuanMBriones/data-leagues-team-5/raw/fred/PostWork-8/csv/SP1-1213.csv")
d1314 <- read.csv("https://github.com/JuanMBriones/data-leagues-team-5/raw/fred/PostWork-8/csv/SP1-1314.csv")
d1415 <- read.csv("https://github.com/JuanMBriones/data-leagues-team-5/raw/fred/PostWork-8/csv/SP1-1415.csv")
d1516 <- read.csv("https://github.com/JuanMBriones/data-leagues-team-5/raw/fred/PostWork-8/csv/SP1-1516.csv")
d1617 <- read.csv("https://github.com/JuanMBriones/data-leagues-team-5/raw/fred/PostWork-8/csv/SP1-1617.csv")
d1718 <- read.csv("https://github.com/JuanMBriones/data-leagues-team-5/raw/fred/PostWork-8/csv/SP1-1718.csv")
d1819 <- read.csv("https://github.com/JuanMBriones/data-leagues-team-5/raw/fred/PostWork-8/csv/SP1-1819.csv")
d1920 <- read.csv("https://github.com/JuanMBriones/data-leagues-team-5/raw/fred/PostWork-8/csv/SP1-1920.csv")

d1011S <- select(d1011, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1112S <- select(d1112, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1213S <- select(d1213, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1314S <- select(d1314, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1415S <- select(d1415, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1516S <- select(d1516, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1617S <- select(d1617, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1718S <- select(d1718, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1819S <- select(d1819, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1920S <- select(d1920, Date:FTAG, Max.2.5:Avg.2.5.1)
d1920S <- select(d1920S, -Time)

d1011S <- mutate(d1011S, Date = as.Date(Date, format = "%d/%m/%y"))
d1112S <- mutate(d1112S, Date = as.Date(Date, format = "%d/%m/%y"))
d1213S <- mutate(d1213S, Date = as.Date(Date, format = "%d/%m/%y"))
d1314S <- mutate(d1314S, Date = as.Date(Date, format = "%d/%m/%y"))
d1415S <- mutate(d1415S, Date = as.Date(Date, format = "%d/%m/%y"))
d1516S <- mutate(d1516S, Date = as.Date(Date, format = "%d/%m/%y"))
d1617S <- mutate(d1617S, Date = as.Date(Date, format = "%d/%m/%y"))
d1718S <- mutate(d1718S, Date = as.Date(Date, format = "%d/%m/%y"))
d1819S <- mutate(d1819S, Date = as.Date(Date, format = "%d/%m/%Y"))
d1920S <- mutate(d1920S, Date = as.Date(Date, format = "%d/%m/%Y"))

d1019S <- rbind(d1011S, d1112S, d1213S, d1314S, d1415S, d1516S, d1617S, d1718S, d1819S)

d1019S <- rename(d1019S,  Max.2.5.O = BbMx.2.5, 
                 Avg.2.5.O = BbAv.2.5, 
                 Max.2.5.U = BbMx.2.5.1,
                 Avg.2.5.U = BbAv.2.5.1)

d1920S <- rename(d1920S,  Max.2.5.O = Max.2.5, 
                 Avg.2.5.O = Avg.2.5, 
                 Max.2.5.U = Max.2.5.1,
                 Avg.2.5.U = Avg.2.5.1)

d1019S <- select(d1019S, colnames(d1920S))

d1020S <- rbind(d1019S, d1920S)

d1020S <- rename(d1020S, date = Date, home.team = HomeTeam, home.score = FTHG, away.team = AwayTeam, away.score = FTAG)

data <- select(d1020S, date, home.team, home.score, away.team, away.score:Avg.2.5.U) 

head(data, n = 2L); tail(data, n = 2L)

md <- data %>% select(date:away.score)
df <- create.fbRanks.dataframes(scores.file = "https://github.com/JuanMBriones/data-leagues-team-5/raw/fred/PostWork-8/csv/match.data.csv")
teams <- df$teams; scores <- df$scores

head(teams, n = 2L); dim(teams); head(scores, n = 2L); dim(scores)

f <- scores$date 
fu <- unique(f) 
Ym <- format(fu, "%Y-%m") 
Ym <- unique(Ym) 
places <- which(Ym[15]==format(scores$date, "%Y-%m")) 
ffe <- scores$date[max(places)] 

train <- scores %>% filter(date <= ffe)
test <- scores %>% filter(date > ffe)

head(train, n = 1); tail(train, n = 1)
head(test, n = 1); tail(test, n = 1)

traindate <- unique(train$date)
testdate <- unique(test$date)

ranks <- rank.teams(scores = scores, teams = teams, 
                    min.date = traindate[1], 
                    max.date = traindate[length(traindate)])

pred <- predict(ranks, date = testdate[1])

phs <- pred$scores$pred.home.score 
pas <- pred$scores$pred.away.score 
pht <- pred$scores$home.team 
pat <- pred$scores$away.team 

phs <- NULL; pas <- NULL; pht <- NULL; pat <- NULL
for(i in 1:(length(unique(scores$date))-170)){
  ranks <- rank.teams(scores = scores, teams = teams, 
                      min.date = unique(scores$date)[i], 
                      max.date = unique(scores$date)[i+170-1], 
                      silent = TRUE,
                      time.weight.eta = 0.0005)
  pred <- predict(ranks, date = unique(scores$date)[i+170],
                  silent = TRUE)
  
  phs <- c(phs, pred$scores$pred.home.score) 
  pas <- c(pas, pred$scores$pred.away.score) 
  pht <- c(pht, pred$scores$home.team) 
  pat <- c(pat, pred$scores$away.team) 
}

buenos <- !(is.na(phs) | is.na(pas))
phs <- phs[buenos] 
pas <- pas[buenos] 
pht <- pht[buenos] 
pat <- pat[buenos] 
momio <- data %>% filter(date >= unique(scores$date)[171]) 
momio <- momio[buenos,]
mean(pht == momio$home.team); mean(pat == momio$away.team)
mean(phs + pas > 2.5 & momio$home.score + momio$away.score > 2.5)
mean(phs + pas < 2.5 & momio$home.score + momio$away.score < 2.5)
hs <- momio$home.score
as <- momio$away.score

mean(phs + pas > 3) 
mean(phs + pas > 3 & hs + as > 2.5)/mean(phs + pas > 3) 
mean(phs + pas < 2.1) 
mean(phs + pas < 2.1 & hs + as < 2.5)/mean(phs + pas < 2.1) 

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Max.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Max.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Max.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Max.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
  labs(x = "NÃºmero de Apuesta", 
       y = "Capital",
       title = "Realizando una secuencia de apuestas") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  # color, Ã¡ngulo y estilo de las abcisas y ordenadas 
p

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Avg.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Avg.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Avg.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
  labs(x = "NÃºmero de Apuesta", 
       y = "Capital",
       title = "Realizando una secuencia de apuestas") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  
p
