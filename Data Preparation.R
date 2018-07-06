fin <- read.csv("Future-500.csv", na.strings = c(""))

head(fin, 10)
tail(fin, 10)
summary(fin)
str(fin)
typeof(fin)

fin$ID        <- factor(fin$ID)
fin$Inception <- factor(fin$Inception)

a <- c("12", "13", "14", "12", "12")
typeof(a)

b <- as.numeric(a)
typeof(b)

z <- factor(c("12", "13", "14", "12", "12"))
x <- as.numeric(as.character(z))

rm(a); rm(b); rm(x); rm(z)

?sub
fin$Expenses <- gsub(" Dollars", "", fin$Expenses)
fin$Expenses <- gsub(","       , "", fin$Expenses)

fin$Revenue  <- gsub("\\$", "", fin$Revenue)
fin$Revenue  <- gsub(","  , "", fin$Revenue)

fin$Growth   <-  gsub("%"  , "", fin$Growth)

fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue  <- as.numeric(fin$Revenue)
fin$Growth   <- as.numeric(fin$Growth)

fin[!complete.cases(fin),]

fin[fin$Revenue == 9746272,]
fin[which(fin$Revenue == 9746272),]

fin[is.na(fin$Expenses),]
fin[is.na(fin$Industry),]

fin_backup <- fin

fin[!complete.cases(fin),]
fin <- fin[!is.na(fin$Industry),]

rownames(fin) <- 1:nrow(fin)
rownames(fin) <- NULL

fin[is.na(fin$State) & fin$City == "New York"     , "State"] <- "NY"
fin[is.na(fin$State) & fin$City == "San Francisco", "State"] <- "CA"

med_empl_retail <- median(fin[fin$Industry == "Retail", "Employees"], na.rm = T)
fin[is.na(fin$Employees & fin$Industry == "Retail"), "Employees"] <- med_empl_retail

med_empl_finserv <- median(fin[fin$Industry == "Financial Services", "Employees"], na.rm = T)
fin[is.na(fin$Employees & fin$Industry == "Financial Services"), "Employees"] <- med_empl_finserv

med_growth_constr <- median(fin[fin$Industry == "Construction", "Growth"], na.rm = T)
fin[is.na(fin$Growth & fin$Industry == "Construction"), "Growth"] <- med_growth_constr

med_rev_constr <- median(fin[fin$Industry == "Construction", "Revenue"], na.rm = T)
fin[is.na(fin$Revenue & fin$Industry == "Construction"), "Revenue"] <- med_rev_constr

med_exp_constr <- median(fin[fin$Industry == "Construction", "Expenses"], na.rm = T)
fin[is.na(fin$Expenses) & fin$Industry == "Construction", "Expenses"] <- med_exp_constr

fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit), "Revenue"] - fin[is.na(fin$Profit), "Expenses"]
fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses), "Revenue"] - fin[is.na(fin$Expenses), "Profit"]


library(ggplot2)
p <- ggplot(data = fin)
p + geom_point(aes(x = Revenue, y = Expenses, colour = Industry, size = Profit))

d <- ggplot(data = fin, aes(x = Revenue, y = Expenses, colour = Industry))
d + geom_point() + geom_smooth(fill = NA, size = 1.2)

f <- ggplot(data = fin, aes(x = Industry, y = Growth, colour = Industry))
f + geom_boxplot(size = 1)
f + geom_jitter() + geom_boxplot(size = 1, alpha = 0.5, outlier.colour = NA)

