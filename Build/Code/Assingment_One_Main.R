#Assignment One

# Clear memory, install and load libraries ####
rm(list=ls()) 

# Question 1
#install.packages("ipumsr") #install ipumsr package 
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(ipumsr)
library(stargazer)


### Obtain required datasets ####

#Getting data from IPUMS API
#set_ipums_api_key("59cba10d8a5da536fc06b59dd464392279524273bfdd688cd0efdd56", save = TRUE) #Sets your personal key and saves it
tst <- get_metadata_nhgis("time_series_tables") #List of the time series tables available

# Question 2
tst$geog_levels[[1]] #View the geographic levels available in the dataset (from first table)

data_names <- c("A00","A57","B57","B18","CL6","B69") #These are the tables we are using
data_ext<- define_extract_nhgis(
  description = "ECON 691",
  time_series_tables =  list(
    tst_spec("A00", "state"),
    tst_spec("A57", "state"),
    tst_spec("B57", "state"),
    tst_spec("B18", "state"),
    tst_spec("CL6", "state"),
    tst_spec("BX3", "state")
  )
)

ts<-submit_extract(data_ext)
wait_for_extract(ts)
filepath <- download_extract(ts)
dat <- read_nhgis(filepath) #load the downloaded ipums data


#Obtain Census Map Data
cen.states <- c("04", "08", "35", "49") #FIPS for Arizona, Colorado, New Mexico, Utah 
cen.stat <- get_acs(geography = "state", 
                    survey = "acs5",
                    variables = "B01003_001E", 
                    year = 2020,
                    state = cen.states, # filter for the required states
                    geometry = TRUE)

cen.map <- cen.stat %>%
  select(GEOID, NAME, geometry)  %>%
  mutate(STATEFP = GEOID) 


# Obtain UFO data and State Abbreviations data
ufo <- read.csv(file = "./Data/scrubbed.csv", header = TRUE, as.is = TRUE, sep = ",") #UFO
st_abb <- read.csv(file = "./Data/st_abb.csv") #State-Abbreviations & FIPS-Codes



#Basic Clan of data####


# Question 3 and 4
dat2 <- dat %>%
  select(STATEFP, ends_with(c("1970", "1980", "1990", "2000", "2010", "105", "2020", "205"))) %>%
  filter(!is.na(STATEFP)) %>% #Remove NA values
  pivot_longer(!STATEFP, names_to = "series", values_to = "estimate") %>% #Pivot the data to long format
  mutate(series = str_replace(series, "105", "2010"),
         series = str_replace(series, "205", "2020"),
         year = substr(series, 6, nchar(series)),
         series = substr(series, 1, 5)) %>%
  distinct(STATEFP, series, year, .keep_all = TRUE) %>% #Remove duplicates
  filter(!is.na(estimate)) %>% #Remove NA values
  pivot_wider(id_cols = c(STATEFP, year), names_from = series, values_from = estimate) %>% #Pivot the data
  select(-B18AE)  %>% #Remove the column B18AE
  mutate(und18 = rowSums(across(B57AA:B57AD)) / A00AA, #Under 18 Population as a share of total population
         over65 = rowSums(across(B57AP:B57AR)) / A00AA, #Over 65 Population as a share of total population
         white = (B18AA) / A00AA, #White Population as a share of total population
         black = (B18AB) / A00AA, #Black Population as a share of total population
         asian = (B18AD) / A00AA, #Asian Population as a share of total population
         other = (B18AC) / A00AA, #Something other than the above including multi-race as a share of total population
         lessHS = (BX3AA+BX3AB + BX3AC + BX3AG + BX3AH + BX3AI) / A00AA, #Less than High School Education as a share of total population
         hscol =  (BX3AD+BX3AJ) / A00AA, #12th Grade and some college as a share of total population
         ungrd =  (BX3AE+BX3AK) / A00AA, #4 years of college or Bach Degree as a share of total population
         advdg =  (BX3AF+BX3AL) / A00AA, #More than 4 years or advanced degree as a share of total population
         pov =  (CL6AA) / A00AA, #Share of population under Poverty Line as a share of total population
         ln_pop = log(A00AA)) %>%  #Natural Log of Population 
  select(STATEFP, year, und18:ln_pop)


# Filter the UFO dataset for U.S. only and merge with the state abbreviations data

# Question 5
ufo.us <- ufo %>%
  filter(country == "us") %>% #Filter for US only
  select(-comments) %>% #Remove the comments column
  mutate(date = as.Date(str_split_i(datetime," ", 1), "%m/%d/%Y"), #Extract the date from the datetime column
         year = year(date),
         decade = year - year %% 10) %>%
  filter(decade > 1959) %>% #Filter for decades after 1959
  count(state, decade) %>% #Count the number of sightings by state and decade
  mutate(Abbr = toupper(state), #Convert the state names to uppercase
         year = as.numeric(decade)) %>% #Convert the decade to numeric
  full_join(., st_abb, by = "Abbr") %>% #Join the data with the state abbreviations data
  filter(!is.na(n)) %>% #Remove NA values
  rename("GEOID" = "Code") %>% #Rename the Code column to GEOID
  mutate(GEOID = str_pad(as.character(GEOID), width = 2, side = "left", pad="0"), #Pad the GEOID column
         ln_n = log(n)) #Natural Log of the number of sightings


# Join the datasets and Map it ####

# Question 6 and 8
core <- cen.map %>%
  left_join(., dat2, by = "STATEFP") %>%
  mutate(decade = as.numeric(year)) %>%  
  select(-c(year)) %>%
  left_join(., ufo.us, by = c("GEOID", "decade")) %>%
  select(!c(GEOID, state, year, Abbr)) %>%
  filter(!is.na(n)) 


#Non-Race Variable Graphic Visualization#####

#Question 7
ggplot(core) +
  geom_sf(aes(fill = ungrd)) +
  scale_fill_gradient2(low = "white", high = "blue", na.value = NA, 
                       name = "ungrd",
                       limits = c(0, .5)) +
  theme_bw()+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5) # Align the title to the center
  ) +
  labs(title = "Figure One: Percentage of Population with a 4 Years College/Bachelor Degree Across the Decades") +
  facet_wrap(~ decade) 
ggsave("./Analysis/Output/Figure1.png", dpi = 600, width = 10, height = 8)


#Race Variable Graphic Visualization
ggplot(core) +
  geom_sf(aes(fill = black)) +
  scale_fill_gradient2(low = "white", high = "blue", na.value = NA, 
                       name = "black",
                       limits = c(0, 1)) +
  theme_bw()+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5) # Align the title to the center
  ) +
  labs(title = "Figure Two: Percentage of Black Population Across the Decades") +
  facet_wrap(~ decade)
ggsave("./Analysis/Output/Figure2.png", dpi = 600, width = 10, height = 8)


# Question 11
#Summary Statistics
var1 <- c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black",
          "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only",
          "Undergraduate Degree", "Advanced Degree", "Percent below 2X Poverty Line", 
          "Ln of Population","Decade","Number of Signthings", "Ln of Sightings")

#Save the summary statistics in a table
stargazer(as.data.frame(core), type = "latex", out = "./Analysis/Output/Table1.txt",
          title = "Table One - Summary Statistics",
          covariate.labels = var1)


# Question 12
#Regression Analysis

mod1 <- lm(ln_n ~ und18 + over65 + white + black + asian + other + lessHS + hscol + ungrd + ln_pop,
           data=core)

mod2 <- lm(ln_n ~ und18 + over65 + white + black + asian + other + lessHS + hscol + ungrd + ln_pop + as.factor(decade),
           data=core)

mod3 <- lm(ln_n ~ und18 + over65 + white + black + asian + other + lessHS + hscol + ungrd + ln_pop + as.factor(decade) + as.factor(State),
           data=core)

var2 <- c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black",
          "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only",
          "Undergraduate Degree", "LN of Population")

# Question 13
stargazer(mod1, mod2, mod3,
          omit = ".State.",
          type = "html",
          title = "Table Two - Regression Results",
          out = "./Analysis/Output/Table2.html",
          add.lines=list(c("State F.E.", "NO", "NO", "YES")),
          dep.var.labels = "LN(Sightings)",
          covariate.labels=var2)


### Interpretation of the results #####

# The first model is a linear regression model with the natural log of the number of sightings as the dependent variable and the percentage of the population under 18, over 65, white, black, Asian, other race, people with: less than high school education, high school education only, undergraduate degree, and natural log of population as independent variables. The second model includes decade fixed effects, which capture the effect of each decade on the number of sightings, while the third model includes both decade and state fixed effects (to capture the effect of each state on the number of sightings).
# The results show that the percentage of the population; under 18, with less than high school education, with high school education only; with undergraduate degree have more has a positive and significant effect on the number of UFO sightings. Similarly, the log of population is positive, which implies that places with higher populations have more UFO sightings. None of the race variables have a significant effect on the number of UFO sighting, except for the percentage of the population that is black, which has a negative and significant effect on the number of sightings. 
# After controlling for decade fixed effects, we no longer see a significant impact by most of the variables except for the percentage of black population, which remains negative and significant, and the percentage of the population with an undergraduate degree and the log of population, which both remain positive and significant, but with lower magnitudes. The introduction of state fixed effects led to no significant impact by any of the variables. Neither of the state and decade fixed effects are significant, which implies that the number of UFO sightings is not significantly different across states and decades, after controlling for the other variables in the model.

## I controlled for poverty and advanced degree in the initial model, but they were not significant, so I excluded them from the main model. The initial model with all variables is presented in the appendix for reference. This was done so to give a more concise result and give room for more degree of freedom in the model.


##### END OF CODE ####



## Appendix ####

# After running the initial code below, with all variables, I found that having advanced degree and poverty has no impact on the UFO sightings. Also, the standard errors are not reported in the third model (with year and state FE), likely due to the small sample size, as there is no degree of freedom left (20 samples, and 20 covariates including the intercept). Thus, I have excluded the advance degree and poverty variables from the main model.
# I present the initial model with all variables below for reference.

#mod1 <- lm(ln_n ~ und18 + over65 + white + black + asian + other + lessHS + hscol + ungrd + advdg + pov + ln_pop, data=core)
#mod2 <- lm(ln_n ~ und18 + over65 + white + black + asian + other + lessHS + hscol + ungrd + advdg + pov + ln_pop + as.factor(decade), data=core)
#mod3 <- lm(ln_n ~ und18 + over65 + white + black + asian + other + lessHS + hscol + ungrd + advdg + pov + ln_pop + as.factor(decade) + as.factor(State),
#           data=core)

#var2 <- c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black",
#          "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only",
#          "Undergraduate Degree", "Advanced Degree", "Percent below 2X Poverty Line", 
#          "LN of Population")
#stargazer(mod1, mod2, mod3,
#          omit = ".State.",
#          type = "html",
#          title = "Table Two - Regression Results",
#          out = "./Analysis/Output/Table3.html",
#          add.lines=list(c("State F.E.", "NO", "NO", "YES")),
#          dep.var.labels = "LN(Sightings)",
#          covariate.labels=var2)


