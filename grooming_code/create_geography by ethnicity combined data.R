#=============preliminary material=============
# Note - there are problems running this script by source(), to do with the encoding of Mäori.

rm(list=ls())
library(census2013)
source("grooming_code/add year variable to tables from census library.R")





combined_cast_tabs <- list()

#================create a range of combined variables======================
# This section of code smells - it's repetitive.  There's probably a way to refactor it.

#-----------2 variables from Income_T4_all---------------
Income_T4_all <- rbind(C01Income_T4, C06Income_T4, Cincome_T4)

Income_T4_all$Ethnicity <- rename.levels(Income_T4_all$Ethnicity, 
                                         orig=c("Mäori", "Total responding with at least one ethnic group", "Total Responding with at least one ethnicity"), 
                                         new=c("Maori", "Total responding with at least one ethnicity", "Total responding with at least one ethnicity"))

target <- "Median_Household_Income_Dollars"
combined_cast_tabs[[1]] <- dcast(Income_T4_all, Geography_Type + Geography + Year ~ Ethnicity, 
                         function(x){mean(x, na.rm=TRUE)}, 
                         value.var = target)
combined_cast_tabs[[1]]$variable <- target
names(combined_cast_tabs[[1]])[names(combined_cast_tabs[[1]])==
                                 "Total Responding with at least one ethnicity"] <-"Total responding with at least one ethnicity"
  

target <- "Total_People"
combined_cast_tabs[[2]] <- dcast(Income_T4_all, Geography_Type + Geography + Year ~ Ethnicity, 
                                 function(x){mean(x, na.rm=TRUE)}, 
                                 value.var = target)
combined_cast_tabs[[2]]$variable <- target
names(combined_cast_tabs[[2]])[names(combined_cast_tabs[[2]])==
                                 "Total Responding with at least one ethnicity"] <-"Total responding with at least one ethnicity"


#--------------------2 from the Dwell_HomeT2B-------------------------------

Rental_T2_all <- rbind(C01Dwell_HomeT2b, C06Dwell_HomeT2b, CDwell_Home_T2b)
names(Rental_T2_all)[names(Rental_T2_all) == "Ethnicity_of_Reference_Person"] <- "Ethnicity"

Rental_T2_all$Ethnicity <- rename.levels(Rental_T2_all$Ethnicity, 
                                         orig=c("Mäori", "Total responding with at least one ethnic group"), 
                                         new=c("Maori", "Total responding with at least one ethnicity"))



target <-"Median_Rent_Dollars"
combined_cast_tabs[[3]] <- dcast(Rental_T2_all, Geography_Type + Geography + Year ~ Ethnicity, 
                                 function(x){mean(x, na.rm=TRUE)}, 
                                 value.var = target)
combined_cast_tabs[[3]]$variable <- target

target <- "Total_Rented_Private_Occupied_Dwellings"
combined_cast_tabs[[4]] <- dcast(Rental_T2_all, Geography_Type + Geography + Year ~ Ethnicity, 
                                 function(x){mean(x, na.rm=TRUE)}, 
                                 value.var = target)
combined_cast_tabs[[4]]$variable <- target

#---------------------------2 from Work_T3 on unemployed---------------

Unempl_T3_all <- rbind(C01Work_T3, C06Work_T3, CWork_T3)


Unempl_T3_all$Ethnicity <- rename.levels(Unempl_T3_all$Ethnicity, 
                                         orig=c("Mäori", "Total responding with at least one ethnic group"), 
                                         new=c("Maori", "Total responding with at least one ethnicity"))

target <-"Unemployed"
combined_cast_tabs[[5]] <- dcast(Unempl_T3_all, Geography_Type + Geography + Year ~ Ethnicity, 
                                 function(x){mean(x, na.rm=TRUE)}, 
                                 value.var = target)
combined_cast_tabs[[5]]$variable <- target

target <- "Unemployment_Rate_Percent"
combined_cast_tabs[[6]] <- dcast(Unempl_T3_all, Geography_Type + Geography + Year ~ Ethnicity, 
                                 function(x){mean(x, na.rm=TRUE)}, 
                                 value.var = target)
combined_cast_tabs[[6]]$variable <- target


#---------------------------2 from Education_T1---------------

Educ_T1_all <- rbind(C01Education_T1, C06Education_T1, CEducation_T1)
Educ_T1_all$Ethnicity <- rename.levels(Educ_T1_all$Ethnicity, 
                                       orig=c("Mäori", "Total responding with at least one ethnic group"), 
                                       new=c("Maori", "Total responding with at least one ethnicity"))
Educ_T1_all <- Educ_T1_all[Educ_T1_all$Age_Group == "Total", ]

tmp <- ddply(Educ_T1_all, .(Geography_Type, Geography, Year, Ethnicity), summarise, 
             Proportion_with_no_education = sum(Total_People[Level_of_education == "None"]) / 
               sum(Total_People[Level_of_education == "Total"]) * 100,
             Proportion_with_higher_education = sum(Total_People[Level_of_education %in% c(
               "Level 7/Bachelors and above", "L7/Bachelors and above", "L7/Bachelor and above")
                                                                 ]) / 
               sum(Total_People[Level_of_education == "Total"]) * 100
             )

target <- "Proportion_with_no_education"
combined_cast_tabs[[7]] <- dcast(tmp, Geography_Type + Geography + Year ~ Ethnicity, 
                                 function(x){mean(x, na.rm=TRUE)}, 
                                 value.var = target)
combined_cast_tabs[[7]]$variable <- target

target <- "Proportion_with_higher_education"
combined_cast_tabs[[8]] <- dcast(tmp, Geography_Type + Geography + Year ~ Ethnicity, 
                                 function(x){mean(x, na.rm=TRUE)}, 
                                 value.var = target)
combined_cast_tabs[[8]]$variable <- target

lapply(combined_cast_tabs, names)

#==============processing of census_combined dataset====================

combined_cast_tabs2 <- do.call("rbind", combined_cast_tabs)
census_combined <- subset(combined_cast_tabs2, Geography_Type %in% c("TA", "Region"))

names(census_combined)[names(census_combined) == "Geography"] <- "NAME"
census_combined <- census_combined[ -grep("Area Outside", census_combined$NAME), ]

nc <- nchar(as.character(census_combined$NAME))
census_combined$NAME <- with(census_combined, gsub(" District", "", substring(NAME, 
                                                        ifelse(Geography_Type == "TA", 5, 4)
                                                                              , nc)))
census_combined$NAME <- gsub(" City", "", census_combined$NAME)
census_combined$NAME <- gsub(" Region", "", census_combined$NAME)

census_combined$Geography_Type <- rename.levels(census_combined$Geography_Type,
                                                orig = c("TA", "Region"),
                                                ne = c("Territorial Authority", "Regional Council"))
census_combined$NAME <- factor(census_combined$NAME)
census_combined$variable <- factor(gsub("_", " ", census_combined$variable, fixed=TRUE))

names(census_combined)[names(census_combined) == "Total People"] <- "All people regardless of ethnicity"








ethnicities <- names(census_combined)[ !names(census_combined) 
                                       %in% c("NAME", "Year", "variable", "Geography_Type")]
variables <- data.frame(name=unique(census_combined$variable))
variables$name <- as.character(variables$name)
variables$label <- "comma"                        
variables$label[grep("ercent", variables$name)] <- "percent"
variables$label[grep("roportion", variables$name)] <- "percent"
variables$label[grep("ollars", variables$name)] <- "dollar"




census_combined[census_combined$variable %in% variables$name[variables$label =="percent"] , ethnicities] <-
  census_combined[census_combined$variable %in% variables$name[variables$label =="percent"] , ethnicities] / 100

census_combined_m <- melt(census_combined[, c("variable", ethnicities)],
                          id.vars="variable", variable.name="Ethnicity")

ranges <- ddply(census_combined_m, .(variable), summarise,
                Min = min(value, na.rm=TRUE),
                Max = max(value, na.rm=TRUE))
variables <- merge(variables, ranges, by.x="name", by.y="variable")



save(census_combined, file = "shiny/census_combined.rda")
save(ethnicities, file="shiny/ethnicities.rda")
save(variables, file="shiny/variables.rda")

# summary(subset(census_combined, variable=="Proportion with higher education"))
