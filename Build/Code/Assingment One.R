#Assignment One
rm(list=ls())

library()
library()
library(ipumsr)
library(stargazer)
library(tidyverse)

#Getting data from IPUMS API
set_ipums_api_key("59cba10d8a5da536fc06b59de3274ff804374719b3d836ec3ff81d8f", save = TRUE)

tst <- get_metadata_nhgis("time_series_tables")
#View(tst)
tst$years[[1]]
print(n = 25, tst$years[[1]])

#Insert Answer to Question 2 Here####
print(n=25, tst$time_series[[55]])

#data_names <- c("A00","A57","B57","B18","CL6","B69") #These are the tables we are using
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

dat <- read_nhgis(filepath)
#View(dat)

#Obtain Census Map Data

state <- c('56', '38,', '46', '27')

cen.stat <- get_acs(geography = "state", 
                    survey = "acs5",
                    variables = "B01003_001E", 
                    year = 2020,
                    geometry = TRUE)


cen.map <- cen.stat %>%
  select(GEOID, NAME, geometry)  %>%
  mutate(STATEFP = GEOID) 

#Basic Clan of data####

dat2 <- dat %>%
  select(STATEFP, ends_with(c("1970", "1980", "1990", "2000", "2010", "2020"))) %>%
  filter(!is.na(STATEFP)) %>%
  pivot_longer(cols = -STATEFP, names_to = "series", values_to = "estimate") %>%
  mutate(
    series = str_replace(series, "105", "2010"),
    series = str_replace(series, "205", "2020"),
    year = substr(series, 6, nchar(series)),
    series = substr(series, 1, 5)
  ) %>%
  distinct(STATEFP, series, year, .keep_all = TRUE) %>%
  filter(!is.na(estimate)) %>%
  pivot_wider(id_cols = c(STATEFP, year), names_from = series, values_from = estimate) %>%
  select(-starts_with("B18AE")) %>%  
  mutate(
    und18 = rowSums(across(matches("^B57A[A-D].*")), na.rm = TRUE) / A00AA,
    over65 = rowSums(across(matches("^B57A[P-R].*")), na.rm = TRUE) / A00AA,
    white = B18AA/ A00AA,                      # White Population
    black = B18AB/ A00AA,                      # Black Population
    asian = B18AC/ A00AA,                      # Asian Population
    other = rowSums(across(matches("^B18A[D-E].*")), na.rm = TRUE), # Something other than the above including multi-race
    lessHS = (BX3AA + BX3AB + BX3AC + BX3AG + BX3AH + BX3AI) / A00AA,
    hscol = rowSums(across(matches("^BX3A[D-J].*")), na.rm = TRUE) / A00AA, # 12th Grade and some college
    ungrd = rowSums(across(matches("^BX3A[E-K].*")), na.rm = TRUE) / A00AA, # 4 years of college or Bach Degree
    advdg = rowSums(across(matches("^BX3A[F-L].*")), na.rm = TRUE) / A00AA, # More than 4 years or advanced degree
    pov = CL6AA / A00AA,                 # Share of population under Poverty Line
    ln_pop = log(A00AA)                 # Natural Log of Population
  ) %>%
  select(STATEFP, year, und18:ln_pop)

#UFO 

ufo <- read.csv(file = "./Data/scrubbed.csv", header = TRUE, as.is = TRUE, sep = ",")


st_abb <- read.csv(file = "./Data/st_abb.csv")
#https://www.bls.gov/respondents/mwr/electronic-data-interchange/appendix-d-usps-state-abbreviations-and-fips-codes.htm




#Cleaning Data####
# This code cleans the data.It consists number of sightings, date, year by state. 
# It filters for decades, counts state and decade.It ensures consistent GEOID formatting, and calculates the natural log of sightings.


ufo.us <- ufo %>%
  filter(country == "us") %>%
  select(-comments) %>%
  mutate(date = as.Date(str_split_i(datetime," ", 1), "%m/%d/%Y"),
         year = year(date),
         decade = year - year %% 10) %>%
  filter(decade > 1969) %>%
  count(state, decade) %>%
  mutate(Abbr = toupper(state),
         year = as.numeric(decade)) %>%
  full_join(., st_abb, by = "Abbr") %>%
  filter(!is.na(n)) %>%
  rename("GEOID" = "Code") %>%
  mutate(GEOID = str_pad(as.character(GEOID), width = 2, side = "left", pad="0"),
         ln_n = log(n))

table(ufo.us$year)


#Join the data and Map it

core <- cen.map %>%
  left_join(dat2, by = "STATEFP") %>%
  mutate(decade = as.numeric(year)) %>%  # Ensure `year` is properly included
  select(-ends_with(".x"), -ends_with(".y"))

#Non-Race Variable Graphic Visualization#

ggplot(core) +
  geom_sf(aes(fill = und18)) + 
  scale_fill_gradient2(low = "white", high = "blue", na.value = NA, 
                       name = "Under 18 (%)",
                       limits = c(0, .5)) +   
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom") +
  labs(title = "Figure One: Percentage of Population Under 18 Across the Decades") +
  facet_wrap(~ decade)

ggsave("./Analysis/Output/Figure1.png", dpi = 600)

#Race Variable Graphic Visualization

ggplot(core) +
  geom_sf(aes(fill = white)) +  # Race variable
  scale_fill_gradient2(low = "white", high = "blue", na.value = NA, 
                       name = "Black (%)",  
                       limits = c(0, 1)) + 
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom") +
  labs(title = "Figure Two: Percentage of Black Population Across the Decades") + 
  facet_wrap(~ decade)

ggsave("./Analysis/Output/Figure2.png", dpi = 600)

#Join core with ufo data
core_2 <- core %>%
  left_join(ufo.us, by = c("GEOID", "decade")) %>%
  mutate(year = decade)

# Filter out rows with no sightig data
core_2 <- core_2 %>%
  filter(!is.na(n))

core_2 <- core_2 %>%
  select(-ends_with(".x"), -ends_with(".y"))

#Non-Race Variable Graphic Visualization

ggplot(core_2) +
  geom_sf(aes(fill = und18)) + 
  scale_fill_gradient2(low = "white", high = "blue", na.value = NA, 
                       name = "Under 18 (%)", 
                       limits = c(0, .5)) + 
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom") +
  labs(title = "Figure One: Percentage of Population Under 18 Across the Decades") +
  facet_wrap(~ decade)

ggsave("./Analysis/Output/Figure3.png", dpi = 600)

#Race Variable Graphic Visualization

ggplot(core_2) +
  geom_sf(aes(fill = white)) +  # Race variable
  scale_fill_gradient2(low = "white", high = "blue", na.value = NA, 
                       name = "Black (%)",
                       limits = c(0, 1)) +  
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom") +
  labs(title = "Figure Two: Percentage of Black Population Across the Decades") + 
  facet_wrap(~ decade)

ggsave("./Analysis/Output/Figure4.png", dpi = 600)


#Summary Statistics

var1 <- c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black",
          "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only",
          "Undergraduate Degree", "Advanced Degree", "Percent below 2X Poverty Line", 
          "LN of Population","Decade","Number of Signthings", "LN of Sightings")

stargazer(as.data.frame(core_2), type = "latex", out = "./Analysis/Output/Table1.txt",
          title = "Table One - Summary Statistics",
          covariate.labels = c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black",
                               "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only",
                               "Undergraduate Degree", "Advanced Degree", "Percent below 2X Poverty Line", 
                               "LN of Population","Decade","Number of Signthings", "LN of Sightings"))

#Regression Analysis

mod1 <- lm(ln_n ~ und18 + over65 + white + black + asian + other + lessHS
           + hscol + ungrd + advdg + pov + ln_pop, data = core_2)
summary(mod1)  

mod2 <- lm(ln_n ~ und18 + over65 + white + black + asian + other + lessHS
           + hscol + ungrd + advdg + pov + ln_pop +  factor(year), data = core_2)
summary(mod2)

mod3 <- lm(ln_n ~ und18 + over65 + white + black + asian + other + lessHS
           + hscol + ungrd + advdg + pov + ln_pop +  factor(year) + factor(state), data = core_2)
summary(mod3)

var2 <- c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black",
          "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only",
          "Undergraduate Degree", "Advanced Degree", "Percent below 2X Poverty Line", 
          "LN of Population","Decade","Number of Signthings", "LN of Sightings",
          "Decade (Fixed Effect)","State (Fixed Effect)")

stargazer(
  mod1, mod2, mod3,
  omit = "factor\\(STATEFP\\)",  
  type = "latex",
  title = "Table Two - Regression Results",
  out = "./Analysis/Output/Table2.txt",
  add.lines = list(
    c("State F.E.", "No", "No", "Yes"),  
    c("Decade F.E.", "No", "Yes", "Yes")
  ),
  dep.var.labels = "LN(Sightings)",
  covariate.labels = var2
)

