
library(census2013)

C01Income_T4$Year <- 2001
C06Income_T4$Year <- 2006
Cincome_T4$Year <- 2013


Income_T4_all <- rbind(C01Income_T4, C06Income_T4, Cincome_T4)

Income_T4_all$Ethnicity <- rename.levels(Income_T4_all$Ethnicity, orig="Mäori", new="Maori")

census_combined <- subset(Income_T4_all, Geography_Type %in% c("TA", "Region"))
census_combined <- census_combined[ -grep("Area Outside", census_combined$Geography), ]

nc <- nchar(as.character(census_combined$Geography))
census_combined$NAME <- with(census_combined, gsub(" District", "", substring(Geography, 
                                                        ifelse(Geography_Type == "TA", 5, 4)
                                                                              , nc)))
census_combined$NAME <- gsub(" City", "", census_combined$NAME)
census_combined$NAME <- gsub(" Region", "", census_combined$NAME)


census_combined <- dcast(census_combined, Geography_Type + NAME + Year ~ Ethnicity, 
                         function(x){mean(x, na.rm=TRUE)}, 
                         value.var = "Median_Household_Income_Dollars")

census_combined$Geography_Type <- rename.levels(census_combined$Geography_Type,
                                                orig = c("TA", "Region"),
                                                ne = c("Territorial Authority", "Regional Council"))

ethnicities <- names(census_combined)[-(1:3)]

save(census_combined, file = "shiny/census_combined.rda")
save(ethnicities, file="shiny/ethnicities.rda")


