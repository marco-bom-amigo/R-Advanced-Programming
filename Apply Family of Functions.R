setwd("./Weather Data")
getwd()

Chicago      <- read.csv("Chicago-F.csv"     , row.names = 1)
Houston      <- read.csv("Houston-F.csv"     , row.names = 1)
NewYork      <- read.csv("NewYork-F.csv"     , row.names = 1)
SanFrancisco <- read.csv("SanFrancisco-F.csv", row.names = 1)

is.data.frame(Chicago)
is.data.frame(Houston)
is.data.frame(NewYork)
is.data.frame(SanFrancisco)

Chicago      <- as.matrix(Chicago)
Houston      <- as.matrix(Houston)
NewYork      <- as.matrix(NewYork)
SanFrancisco <- as.matrix(SanFrancisco)

is.matrix(Chicago)
is.matrix(Houston)
is.matrix(NewYork)
is.matrix(SanFrancisco)

Weather <- list(Chicago = Chicago, Houston = Houston, NewYork = NewYork, SanFrancisco = SanFrancisco)
Weather[2]
Weather[[2]]
Weather$Houston

?by
apply(Chicago, 1, mean)
apply(Chicago, 1, max)
apply(Chicago, 1, min)

output <- NULL
for (i in 1:5) {
  output[i] <- mean(Chicago[i,])
}
names(output) <- row.names(Chicago)

apply(Chicago, 1, mean)

?lapply
lapply(Weather, t)

rbind(Chicago, NewRow = 1:12)
lapply(Weather, rbind, NewRow = 1:12)

?rowMeans
lapply(Weather, rowMeans)

Weather[[1]][1,1]
Weather$Chicago[1,1]
lapply(Weather, "[", 1, 1)
lapply(Weather, "[", 1, )
lapply(Weather, "[", , 3)

lapply(Weather, function(x) cumsum(x[1,]))

# Varia??o!!!!
lapply(Weather, function(x) round((x[1,] - x[2,]) / x[2,],2))

?sapply
lapply(Weather, "[", 1, 7)
sapply(Weather, "[", 1, 7)
t(sapply(Weather, "[", 1, 10:12))

# 1. A table showing the annual averages of each observed metric for every city
round(t(sapply(Weather, rowMeans)), 2)

# 2. A table showing by how much temperature fluctuates each month from min to
#    max (in %). Take min temperature as the base
round(t(sapply(Weather, function(x) ((x[1,] - x[2,]) / x[2,]) * 100)), 2)

# 3. A table showing the annual maximums of each observed metric for every city
t(sapply(Weather, apply, 1, max))
t(sapply(Weather, function (x) apply(x, 1, max)))

# 4. A table showing the annual minimums of each observed metric for every city
t(sapply(Weather, apply, 1, min))
t(sapply(Weather, function (x) apply(x, 1, min)))

# 5. A table showing in which months the annual maximums of each metric were
#    observed in every city (Advanced)
names(which.max(Chicago[1,]))
apply(Chicago, 1, function(x) which.max(x))
t(sapply(Weather, function(y) apply(y, 1, function(x) names(which.max(x)))))

t(sapply(Weather, function(y) apply(y, 1, function(x) names(which.min(x)))))
