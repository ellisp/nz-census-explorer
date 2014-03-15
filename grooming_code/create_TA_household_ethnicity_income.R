
library(census2013)

C01Income_T4$Year <- 2001
C06Income_T4$Year <- 2006
Cincome_T4$Year <- 2013


Income_T4_all <- rbind(C01Income_T4, C06Income_T4, Cincome_T4)

Income_T4_all$Ethnicity <- rename.levels(Income_T4_all$Ethnicity, orig="Mäori", new="Maori")

IncomeTA <- subset(Income_T4_all, Geography_Type == "TA")

nc <- nchar(as.character(IncomeTA$Geography))
IncomeTA$NAME <- gsub(" District", "", substring(IncomeTA$Geography, 5, nc))
IncomeTA$NAME <- gsub(" City", "", IncomeTA$NAME)


IncomeTA_median <- dcast(IncomeTA, NAME + Year ~ Ethnicity, 
                         function(x){mean(x, na.rm=TRUE)}, 
                         value.var = "Median_Household_Income_Dollars")
save(IncomeTA_median, file = "shiny/IncomeTA_median.rda")
