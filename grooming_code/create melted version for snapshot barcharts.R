variables_we_want <- c("Median Household Income", 
                          "Median Rent", 
                          "Proportion of people in area", 
                          "Proportion with higher education",
                          "Proportion with no education",
                        "Unemployment Rate Percent")

census_m <- melt(subset(census_combined, variable %in% variables_we_want), 
     id.vars=c("Geography_Type", "NAME", "Year", "variable"), 
                  variable.name="Ethnicity")

census_m <- subset(census_m, ! Ethnicity %in% 
                     c("Total responding with at least one ethnicity", 
                       "Not Elsewhere Included",
                       "Other Ethnicity",
                       "Middle Eastern/Latin American/African",
                       "All people regardless of ethnicity"))
census_m$value[substring(census_m$variable, 2, 8) == "roporti"] <- census_m$value[substring(census_m$variable, 2, 8) == "roporti"] * 100
census_m$value[grep("ercent", census_m$variable)] <- census_m$value[grep("ercent", census_m$variable)] * 100

#-----------------------

TAs <- as.character(unique(subset(census_m, Geography_Type == "Territorial Authority")$NAME))
Regions <- as.character(unique(subset(census_m, Geography_Type == "Regional Council")$NAME))

#---------------

save(census_m, file="shiny/data/census_m.rda")
save(TAs, file="shiny/data/TAs.rda")
save(Regions, file="shiny/data/Regions.rda")
