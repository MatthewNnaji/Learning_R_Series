# Data Wrangling｜遊び場 ################################################################

## Libraries and Init =================================================================
library(tidyverse)
library(dplyr)
library(ggplot2)

library(datasets)
library(help = "datasets")

library(readxl)

library(readr)
caschool <- read_csv("C:/Users/oxonn/OneDrive/Desktop/R files/ECN241_TUT/caschool.csv")
View(caschool)

## Tasks ===========================================================================
#~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・
# [1] basic ggplot, Setting tibbles, Subset of tibble, filtering, rename/replace
# [6] read_excel(), Wide ⇌ Long, Separate Columns, Unite Columns
# [10] String Tidying I & II, Case Study I, II & III [14]
#~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・


### [1] basic ggplot ------------------------------------------------------------------------------

qplot(Height,Volume, data=trees)

### [2] Setting tibbles ------------------------------------------------------------------------------

view(CO2_tib)
CO2_tib <- as_tibble(CO2)
CO2_tib |>
  print(n=Inf)
qplot(Treatment, uptake, data=CO2_tib)

### [3] Subset of Tibble  ------------------------------------------------------------------------------

CO2_tib$Treatment
unique(caschool$county)
Bea[[2]]

### [4] Filtering ------------------------------------------------------------------------------

filter(caschool, county=="Butte")
Sacramentoschools <- filter(caschool, county == 'Sacramento')
Testing <- filter(Bea, county=='Sacramento' | el_pct <0.5)
NewTest <- filter(Bea, (county=='Alameda' | county == 'Butte') & computer >=50)

### [5] Rename/replace------------------------------------------------------------------------------

Bea <- rename(Bea, county = County)
people['anna', 'Name'] <- 'Anna' # (MISTAKE) from the anna row, in name column replaces the vlaue with anna (MISTAKE)
people <- people[-4] #deleted 4th column
people <- people[-3, ] #deleted 3rd row
people[3,2] <- 'phil' #Renamed cphil to phil; on the 3rd row on second column(Name)
people[3,'Name'] <- 'carrie' #Renamed the 3rd value on the Name column to carrie

### [6a] read_delim+fwf() ------------------------------------------------------------------------------
"more on both is on Ch3[5] and Ch3[6]"
# all other read files are an abbreviation of this file; this is a more general case for different delimiters
# tab-delimited = read_delim(file=.txt, delim='\t')
stoppages <- read_delim(file='http://594442.youcanlearnit.net/workstoppages.txt', delim='^')

#for fixed-width: check the file directly
#The function takes the data set; and an array with the lengths of each of the fixed widths (so it knows where to split for each one)
# If there's still whitespace; use str_trim() from stringr
### [6b]【重要】read_xl()  ------------------------------------------------------------------------------

# First attempt:
brokenrice <- read_excel("ラーニング/Ex_Files_Data_Wrangling_in_R/Exercise Files/breakfast.xlsx") #これ、滅茶苦茶でしょ!?

# Because how the file is it looks like we need to skip five lines, which will remove the column names
# So lets create a vector with column names:
breakfast <- c("Year", "FreeStudents", "ReducedStudents", "PaidStudents", "TotalStudents", 
           "MealsServed", "PercentFree")

# And then try reading the file again
hakumai <- read_excel("ラーニング/Ex_Files_Data_Wrangling_in_R/Exercise Files/breakfast.xlsx",skip=5, col_names = breakfast)
glimpse(hakumai)

# I'll do a little quick manipulation of this tibble. 
# First, convert the numbers of students and meals to real values
hakumai <- hakumai %>%
  mutate(FreeStudents=FreeStudents*1000000,
         ReducedStudents=ReducedStudents * 1000000,
         PaidStudents = PaidStudents * 1000000,
         TotalStudents = TotalStudents * 1000000,
         MealsServed = MealsServed * 1000000,
         PercentFree = PercentFree/100)

#this is similar to (remembering piping)
"hakumai <- hakumai(breakfast,(FreeStudents=FreeStudents*1000000, ReducedStudents=ReducedStudents * 1000000,PaidStudents = PaidStudents * 1000000,TotalStudents = TotalStudents * 1000000,MealsServed = MealsServed * 1000000,PercentFree = PercentFree/100))"

### [7] Wide ⇌ Long ------------------------------------------------------------------------------

#From Wide => Long;
pew <- read_csv("http://594442.youcanlearnit.net/pew.csv")
view(pew)
pew.long <- pivot_longer(pew, !religion, names_to='income', values_to='freq')
view(pew.long)
?pivot_longer

#From Long => wide
weather <- read_csv("http://594442.youcanlearnit.net//mexicanweather.csv")
view(weather)
weather.wide <- weather |>
  pivot_wider(names_from=element, values_from=value)
view(weather.wide)
ls()

### [8] Separate Columns------------------------------------------------------------------------------


# Load the tidyverse and read in the Medicare payments dataset
library(tidyverse)
names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")
types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, skip=1, col_types = types)

# Take a look at the diagnosis-related group unique values
unique(inpatient$DRG)

# Let's try separating this on the hyphen
inpatient_separate <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),'-')

# What's going on with those warning rows?  Let's look at row 45894
inpatient$DRG[45894]

# Let's separate with character position instead
inpatient_separate <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),4)
# inpatient_separate <-separate(inpatient,oldcolumn, c(vector of two new columns),the character position you wish to split at)

# And take a look at the data now
glimpse(inpatient_separate)



### [9] Unite Columns ------------------------------------------------------------------------------


# Load the tidyverse and the food inspections dataset
library(tidyverse)

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

glimpse(inspections)

# Create a new column called Regions that combines City and State
regional_inspections <- unite(inspections,Region,City,State,sep=", ")

# Let's look at the data
glimpse(regional_inspections)

# Whoops. I didn't want to DELETE the City and State columns.  Let's try again.
regional_inspections <- unite(inspections,Region,City,State,sep=", ", remove=FALSE)
glimpse(regional_inspections)

# And take a look at the unique regions
unique(regional_inspections$Region)



### [10] String tidying I ------------------------------------------------------------------------------

# The core of this SECTION: changing erroneous strings
# dataframe <- mutate(dataframe, dataframe$column=ifelse(column %in% falsecolumn(create yourself), "New value you create", column"saying then remain the same")) 


# Load the tidyverse and the food inspections dataset
library(tidyverse)

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

# Create a new column called Regions that combines City and State
regional_inspections <- unite(inspections,Region,City,State,sep=", ", remove=FALSE)

# And take a look at the unique regions
unique(regional_inspections$Region)

# We need to load stringr separately
library(stringr)

# Let's handle the uppercase/lowercase issues by converting everything to uppercase
regional_inspections <- regional_inspections %>%
  mutate(Region=str_to_upper(Region))


# What were the results of that?
unique(regional_inspections$Region)

# Let's take care of a few misspellings of Chicago
badchicagos <- c('CCHICAGO, IL', 'CHCICAGO, IL', 'CHICAGOCHICAGO, IL', 'CHCHICAGO, IL', 'CHICAGOI, IL')

regional_inspections <- regional_inspections %>%
  mutate(Region=ifelse(Region %in% badchicagos, 'CHICAGO, IL', Region)) 
      # 1 mutate (database, the part we want to change) -> doing the multi line pipe is purely the same
      # 2 ifelse() -> only changing it if certain conditions are met
      # 3 so we made an array of false values / can simply just check its not one of the true values for all 50 states
      # 4 if it is (the true part of ifelse) then change it to the true chicago
      # 5 else; then region will change to region (in effect saying IGNORE)


# And see what's left
unique(regional_inspections$Region)

# There are some "CHICAGO, NA" values that we can clearly correct to "CHICAGO, IL"
regional_inspections <- regional_inspections %>%
  mutate(Region=ifelse(Region=='CHICAGO, NA', 'CHICAGO, IL', Region)) 

# But we don't know what to do with "NA, IL", "NA, NA", or "INACTIVE, IL"
# so let's set those to missing values
nachicagos <- c('NA, IL', 'NA, NA', 'INACTIVE, IL')

regional_inspections <- regional_inspections %>%
  mutate(Region=ifelse(Region %in% nachicagos, NA, Region)) 

# How did we do?
unique(regional_inspections$Region)

### [11]  String tidying II ------------------------------------------------------------------------------
# SUMMARY: REMOVING WHITESPACE / UNNEEDED CHARS
namesMedic <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")
typesMedic = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = namesMedic, skip=1, col_types = typesMedic)

# Separate at the fourth position
inpatient_separate <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),4) 
#c is simply a vertical vector/list of all the columns you want for new titles

# 「DRGcode
# ..DRGdescription
# . x3
#  .x4             」etc.
?separate()

# And take a look at the data now
glimpse(inpatient_separate)

# Trim the DRGcode field
inpatient_separate <- inpatient_separate %>%
  mutate(DRGcode=str_trim(DRGcode))

glimpse(inpatient_separate)

# The DRGdescription field has a hyphen in front so we need to do something different
inpatient_separate <- inpatient_separate %>%
  mutate(DRGdescription=str_sub(DRGdescription, 3))

glimpse(inpatient_separate)

?str_replace()

### [12] Case Study I------------------------------------------------------------------------------
# ～ Wrangling Case Study 1: Coal Consumption ～~～~～~～~～~～~～~～~～~～~～~～~～~～~～~～~～~～
"best to write notes on this; for step-by-step analysis watch the LinkedinLearning"
" ① renaming colnames()| ② pivot_longer() | ③ mutate(as.integer()) |"
" ④ separating datsets | ⑤ ggplot2 visualisation |"
#1: Import the data read_csv()
#2: Remove any title rows
#3: Most data will be in half wide/long format (for easy to read benefits); however for R we need to convert to purely long
#4: pivot_longer(); converts dataset from 232x31 to 6960x3 [a LOT longer!!]
#5: Some values weren't rendered correctly => mutate(coal, year <-as.integer(year))
#6: Some column names were wrong => colnames(coal)[1] <- 'Region'
#7: World+Continents in same column as countries; we split into separate tibbles; or add another column
#8: Visualising using ggplot2:

### [13] Case Study II ------------------------------------------------------------------------------
#  ～ Wrangling Case Study 2: Water Quality ～~～~～~～~～~～~～~～~～~～~～~～~～~～~～~～~～~～~～
"notes; for full see video"
"1. select | 2. lubridate | 3. recode() | 4. which() "
#1: Use water <- select(water,('WATERTYPE')) etc; only select the cols you need
#2: use filteredwater <- water %>% filter(typeParamter = 'pH' | typeParameter = 'Temp')
#3: Mutating the 'Type' variables to as.factor()
#4: Using lubridate; converting the <chr> dates to mdy_hms(dates)
#5: Using unit_recode() to change values written as 'Feet'/Ft to Fahrenheit(F)
#6: Filtering and changing using which()

# Let's take a look at the data a different way
summary(filtered_water)

# It would be helpful to convert some of these to factors
filtered_water <- filtered_water %>%
  mutate(siteType=as.factor(siteType),
         parameterType=as.factor(parameterType),
         parameter=as.factor(parameter),
         unit=as.factor(unit))

summary(filtered_water)

# And sampleTime should be a date/time object
filtered_water <- filtered_water %>%
  mutate(sampleTime=mdy_hms(sampleTime))

summary(filtered_water)

# Why are some of these measurements in feet?
filtered_water %>% filter(unit=='Feet')

# Looks like that is supposed to be Farenheit, so fix it
filtered_water <- filtered_water %>%
  mutate(unit=recode(unit, 'Feet'='Deg. Fahrenheit')) 

summary(filtered_water)

# Remove MG/L
filtered_water <- filtered_water %>%
  filter(unit!='MG/L')
"-----------------------------------------"
# Let's find our Fahrenheit values in the dataset
fahrenheit <- which(filtered_water$unit=='Deg. Fahrenheit')

# And convert them to Celsius
filtered_water$result[fahrenheit] <- (filtered_water$result[fahrenheit] - 32) * (5/9)
### [14] Case Study III------------------------------------------------------------------------------
#  ～ Wrangling Case Study 3: Social Security Disability ～~～~～~～~～~～~～~～~～~～~～~～~～~～~～
"notes; for full see video"
"1. adding and removing columns | 2. paste() | 3. conditional offsetting | 4. which() "
#1: Import the dataset
#2: this again is a long_wide fusion with vertical fiscal years; horizontal month + month itnernet
#3: pivot_logner to FY, month and application
#4: Need to separate so month separates Oct_Total from Oct_Internet
#5: SEPARATE FUNCTION: the first part of a string it takes as a column; and the rest it takes as all new columns
#5: ssa_long <- ssa_long %>%
      #separate(month, c('month', 'application_method'), sep='_')

#6: Next; we need to rename Months so they are uniform (Oct vs March vs Jun vs July vs Feb vs August)
# ssa_long <- ssa_long %>%
      #mutate(month=substr(month,1,3))


#7: For calculations; replace the string "FYXX" with the number 20XX
ssa_long <- ssa_long %>%
  mutate(Fiscal_Year=str_replace(Fiscal_Year, "FY", "20"))

# 8: Paste - allows for combining of strings
paste('01', ssa_long$month, ssa_long$Fiscal_Year)
#9a: Mutate where there is no previous argument = creates new column; created new dates column :))
ssa_long <- ssa_long %>%
  mutate(date=dmy(paste("01", ssa_long$month, ssa$Fiscal_Year)))

#9b: Removing columns: Select the ones you need; or filter and make the new tibble
# Remove Fiscal_Year and month columns
ssa_long <- ssa_long %>%
  select(-Fiscal_Year, -month)

#10: Adjusting fiscal year: the US fiscal year starts 3 months before calendar.
# Hence; for 2016 we need to subtract 1 year from the values oct [converting to real dates]
ssa_long <- ssa_long %>%
  mutate(Fiscal_Year=as.numeric(Fiscal_Year)) %>%
  mutate(Fiscal_Year=ifelse(month(date)>=10, Fiscal_Year-1, Fiscal_Year)) %>%
  mutate(date=dmy(paste("01", month, Fiscal_Year))) #paste; returning to numeric

#11: After fixing the date column; we can split the total + internet using pivot_wider
