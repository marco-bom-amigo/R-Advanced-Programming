util <- read.csv("Machine-Utilization.csv")
util$Utilization <- 1 - util$Percent.Idle
head(util, 15)
str(util)
summary(util)

?POSIXct
util$PosixTime <- as.POSIXct(util$Timestamp, format = "%d/%m/%Y %H:%M")
util$Timestamp <- NULL
util <- util[,c(4,1,2,3)]
head(util, 15)
summary(util)

RL1 <- util[util$Machine == "RL1",]
RL1$Machine <- factor(RL1$Machine)

util_stats_rl1 <- c( min( RL1$Utilization, na.rm = T)
                     , mean(RL1$Utilization, na.rm = T)
                     , max( RL1$Utilization, na.rm = T)
)
util_under_90 <- length(which(RL1$Utilization < .9)) > 0
list_rl1 <- list("RL1", util_stats_rl1, util_under_90)

names(list_rl1)
names(list_rl1) <- c("Machine", "Stats", "LowTreshold")
rm(list_rl1)

list_rl1 <- list(Machine="RL1", Stats=util_stats_rl1, LowTreshold=util_under_90)
names(list_rl1)

list_rl1[1]
list_rl1[[1]]
list_rl1$Machine

list_rl1[2]
typeof(list_rl1[2])
list_rl1[[2]]
typeof(list_rl1[[2]])
list_rl1$Stats
typeof(list_rl1$Stats)

list_rl1[[2]][3]
list_rl1$Stats[3]

list_rl1$UnknownHours <- RL1[is.na(RL1$Utilization),"PosixTime"]
list_rl1[5] <- "New Information"  
list_rl1[5] <- NULL  

list_rl1$Data <- RL1
summary(list_rl1)
str(list_rl1)

list_rl1[[4]][1]
list_rl1$UnknownHours[1]

list_rl1[2:3]
list_rl1[c(1,4)]
sublist_rl1 <- list_rl1[c("Machine", "Stats")]
sublist_rl1[[2]][2]
sublist_rl1$Stats[2]

library(ggplot2)

p <- ggplot(data = util)
p + geom_line(aes( x      = PosixTime
                   , y      = Utilization
                   , colour = Machine)
              , size = 1.2) +
  facet_grid(Machine ~ .) +
  geom_hline( yintercept = .9
              , colour = "Gray"
              , size = 1.2
              , linetype = 3)
?linetype

plot <- p + geom_line(aes( x      = PosixTime
                           , y      = Utilization
                           , colour = Machine)
                      , size = 1.2) +
  facet_grid(Machine ~ .) +
  geom_hline( yintercept = .9
              , colour = "Gray"
              , size = 1.2
              , linetype = 3)

list_rl1$Plot <- plot
