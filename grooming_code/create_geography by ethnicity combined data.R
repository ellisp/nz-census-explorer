#=============preliminary material=============
library(census2013)
source("grooming_code/add year variable to tables from census library.R")



combined_cast_tabs <- list()

#================create a range of combined variables


#-----------Income_T4_all---------------
Income_T4_all <- rbind(C01Income_T4, C06Income_T4, Cincome_T4)
Income_T4_all$Ethnicity <- rename.levels(Income_T4_all$Ethnicity, orig="Mäori", new="Maori")

target <- "Median_Household_Income_Dollars"
combined_cast_tabs[[1]] <- dcast(Income_T4_all, Geography_Type + Geography + Year ~ Ethnicity, 
                         function(x){mean(x, na.rm=TRUE)}, 
                         value.var = target)
combined_cast_tabs[[1]]$variable <- target

target <- "Total_People"
combined_cast_tabs[[2]] <- dcast(Income_T4_all, Geography_Type + Geography + Year ~ Ethnicity, 
                                 function(x){mean(x, na.rm=TRUE)}, 
                                 value.var = target)
combined_cast_tabs[[2]]$variable <- target






#==============processing of census_combined dataset====================

combined_cast_tabs <- do.call("rbind", combined_cast_tabs)
census_combined <- subset(combined_cast_tabs, Geography_Type %in% c("TA", "Region"))

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

ethnicities <- names(census_combined)[ !names(census_combined) 
                                       %in% c("NAME", "Year", "variable", "Geography_Type")]
variables <- unique(census_combined$variable)
save(census_combined, file = "shiny/census_combined.rda")
save(ethnicities, file="shiny/ethnicities.rda")
save(variables, file="shiny/variables.rda")


