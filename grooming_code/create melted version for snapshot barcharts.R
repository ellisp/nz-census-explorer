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


reg1 <- "Northland"
reg2 <- "Auckland"
GeoType <- "Regional Council"
ThisYear <- 2013

census2 <- subset(census_m, Year==ThisYear &
                    NAME %in% c(reg1, reg2) &
                    Geography_Type == GeoType)

ggplot(census2, aes(x=NAME, fill=Ethnicity, weight=value)) +
  geom_bar(position="dodge") +
  facet_wrap(~variable, scales="free_y", ncol=2) +
  scale_y_continuous("", label=comma) +
  labs(x="") +
  theme_bw(base_family=MyFont) +
  scale_fill_brewer(palette="Set1") +
  theme(panel.margin = unit(2, "lines"))