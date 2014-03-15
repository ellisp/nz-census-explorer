#=============preliminary material=============
library(census2013)
source("grooming_code/add year variable to tables from census library.R")



combined_cast_tabs <- list()

#================create a range of combined variables


#-----------2 variables from Income_T4_all---------------
Income_T4_all <- rbind(C01Income_T4, C06Income_T4, Cincome_T4)
Income_T4_all$Ethnicity <- rename.levels(Income_T4_all$Ethnicity, orig="Mäori", new="Maori")

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

Rental_T2_all$Ethnicity <- rename.levels(Rental_T2_all$Ethnicity, orig="Mäori", new="Maori")



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

ethnicities <- names(census_combined)[ !names(census_combined) 
                                       %in% c("NAME", "Year", "variable", "Geography_Type")]
variables <- as.character(unique(census_combined$variable))
save(census_combined, file = "shiny/census_combined.rda")
save(ethnicities, file="shiny/ethnicities.rda")
save(variables, file="shiny/variables.rda")


