

# Get vector of all the table names
tabs <- unique(TableVariables$Table)

# Add the relevant year as a column to each name, using the 2nd and 3rd characters of its name to decide
for (i in tabs){
  tmp <- get(i)
  if(substring(i, 2, 3) == "01"){
    tmp$Year <- 2001
  } else {
    if(substring(i, 2, 3) == "06"){
      tmp$Year <- 2006
    } else {
      tmp$Year <- 2013
    }
  }
  assign(i, tmp)
}
