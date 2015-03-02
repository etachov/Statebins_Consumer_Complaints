library(dplyr) #data manipulatione
library(lubridate) #dealing with daes
library(statebins) # for our binmaps

## read in a csv of CFPB complaints data on various financial products. In the future connect to API.
complaints <- read.csv("Consumer_Complaints.csv", header = TRUE, stringsAsFactors = FALSE)

## convert date strings to date format and add a year variable
complaints$Date.received <- mdy(complaints$Date.received)
complaints$Year <- year(complaints$Date.received)

## let's look at complaints about mortages by state in 2013
mortgages_2013 <- filter(complaints, Product == "Mortgage", Year == "2013", State != "") %>%
  group_by(State) %>%
  summarise(Value = n())

## the complaints file only uses state abbreviations. we need to convert to full names to make statebins
state_abbrvs <- read.csv("usa_abbrvs.csv", header = TRUE, stringsAsFactors = FALSE)

## the abbreviations file includes Territories so we need to filter down just States, Commonwealths, and District (i.e. Washington DC)
state_abbrvs <- state_abbrvs %>% 
  filter(Status %in% c("State", "State (Commonwealth)", "Federal district")) %>%
  select(Name, ANSI.2.)

## change the column name to State so its easy to merge
names(state_abbrvs)[2] <- "State"
mortgages_2013_states <- merge(mortgages_2013, state_abbrvs)


## without accounting for state population, its difficult to identify any states with unusually high numbers of mortgage complaints
## we can get good census data from census.gov. the cleaned data is included in the respository and the code for cleaning the raw data is commmented out below.

#state_pop <- read.csv("http://www.census.gov/2010census/csv/pop_change.csv", header = TRUE, stringsAsFactors = FALSE)
#names(state_pop) <- as.character(unlist(state_pop[2,]))

## drop the first 7 rows, which contain regional figures and header text

#state_pop <- state_pop[-1:-7, ]
#names(state_pop)[1] <- "Name"
#names(state_pop)[12] <- "Population"
#state_pop <- state_pop %>% select(Name, Population)
#write.csv(state_pop, "state_pop.csv", row.names = F)

state_pop <- read.csv("state_pop.csv", header = TRUE, stringsAsFactors = FALSE)

## merge the population data with our mortgage complaint data and create a new variable
mortgage_2013_f <- merge(mortgage_2013_states, state_pop) %>%
  mutate(Complaint.Pop = Value/Population)

## now we'll make our statebins map
mortgage_bins <- statebins(mortgage_2013_f, 
                state_col = "State", 
                value_col = "Complaint.Pop",
                text_color = "black", 
                font_size = 6, 
                legend_title = "Mortgage Complaints per Capita by State - 2013",
                legend_position = "bottom") +
                labs(title = "CFPB Mortgage Complaints 2013 Per Capita") +
                theme(plot.title = element_text(size = 30,
                                                hjust = 0,
                                                vjust = 0,
                                                face = "bold")) 

mortgage_bins
## print our chart
ggsave("mortgage_bins.png", mortgage_bins)
