
## Project:  FILL THIS OUT
# Located:   FILL THIS OUT
# File Name: FILL THIS OUT
# Date:      FILL THIS OUT
# Who:       FILL THIS OUT


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################


### Settings + Packages
library(psych)
library(dplyr)
library(pscl)

### Load data 
GSS <- read.csv("GSS2022.csv")

####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################


## Steps of cleaning variables Clear vars
# Step 1: Examine variable and coding schema: Table() / summary() 
# Step 2: Recode (if necessary/warrented): mutate(), ifelse(), etc
# Step 3: Confirm: table() / summary()



############                     DEPENDENT VARIABLE                     ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$getahead)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, all_work = ifelse(getahead == 1, 1, 0))
GSS <- mutate(GSS, some_work = ifelse(getahead == 2, 1, 0))
GSS <- mutate(GSS, luck = ifelse(getahead == 3, 1, 0))
# STEP 3: Confirm creation (if necessary)
table(GSS$getahead, GSS$all_work)
table(GSS$getahead, GSS$some_work)
table(GSS$getahead, GSS$luck)
############                  INDEPENDENT VARIABLE                    ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$degree)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, less_then_hs = ifelse(degree == 0, 1, 0))
GSS <- mutate(GSS, highschool = ifelse(degree == 1, 1, 0))
GSS <- mutate(GSS, jr_college = ifelse(degree == 2, 1, 0))
GSS <- mutate(GSS, Bachelor = ifelse(degree == 3, 1, 0))
GSS <- mutate(GSS, Graduate = ifelse(degree == 4, 1, 0))
# STEP 3: Confirm creation (if necessary)
table(GSS$degree, GSS$less_then_hs)
table(GSS$degree, GSS$highschool)
table(GSS$degree, GSS$jr_college)
table(GSS$degree, GSS$Bachelor)
table(GSS$degree, GSS$Graduate)
############                  Control VARIABLE                    ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$sex)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, yes_man = ifelse(sex == 1, 1, 0))
GSS <- mutate(GSS, yes_women = ifelse(sex == 2, 1, 0))
# STEP 3: Confirm creation (if necessary)
table(GSS$sex, GSS$yes_man)
table(GSS$sex, GSS$yes_women)
############                  Control VARIABLE                    ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$race)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, white = ifelse(sex == 1, 1, 0))
GSS <- mutate(GSS, black = ifelse(race == 2, 1, 0))
GSS <- mutate(GSS, other = ifelse(race == 2, 1, 0))
# STEP 3: Confirm creation (if necessary)
table(GSS$race, GSS$white)
table(GSS$race, GSS$black)
table(GSS$race, GSS$other)
# STEP 2: Recode if necessary or justify if not necessary

# STEP 3: Confirm creation (if necessary)

############                  Control VARIABLE                    ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$res16)
# STEP 2: Recode if necessary or justify if not necessary
GSS <- mutate(GSS, county_nofarm = ifelse(res16 == 1, 1, 0))
GSS <- mutate(GSS, farm = ifelse(res16 == 2, 1, 0))
GSS <- mutate(GSS, town = ifelse(res16 == 3, 1, 0))
GSS <- mutate(GSS, largetown = ifelse(res16 == 4, 1, 0))
GSS <- mutate(GSS, bigcitysuburb = ifelse(res16 == 5, 1, 0))
GSS <- mutate(GSS, city = ifelse(res16 == 6, 1, 0))
# STEP 3: Confirm creation (if necessary)
table(GSS$divorce, GSS$county_nofarm)
table(GSS$divorce, GSS$farm)
table(GSS$divorce, GSS$town)
table(GSS$divorce, GSS$largetown)
table(GSS$divorce, GSS$bigcitysuburb)
table(GSS$divorce, GSS$city)
############                  Control VARIABLE                    ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############
# STEP 1: Examine variable and coding schema 
table(GSS$sexornt)
# STEP 2: Recode if necessary or justify if not necessary
GSS <- mutate(GSS, homosexual = ifelse(sexornt == 1, 1, 0))
GSS <- mutate(GSS, bisexual = ifelse(sexornt == 2, 1, 0))
GSS <- mutate(GSS, heterosexual = ifelse(sexornt == 3, 1, 0))
# STEP 3: Confirm creation (if necessary)
table(GSS$sexornt, GSS$homosexual)
table(GSS$sexornt, GSS$ bisexual)
table(GSS$sexornt, GSS$heterosexual)

############                  Control VARIABLE                    ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############
# STEP 1: Examine variable and coding schema 
table(GSS$satfin)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, satisfied = ifelse(satfin == 0, 1, 0))
GSS <- mutate(GSS, kinda_satisfied = ifelse(satfin == 1, 1, 0))
GSS <- mutate(GSS, not_satisfied = ifelse(satfin == 2, 1, 0))
# STEP 3: Confirm creation (if necessary)
table(GSS$satfin, GSS$satisfied)
table(GSS$satfin, GSS$kinda_satisfied)
table(GSS$satfin, GSS$not_satisfied)







####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep
my_varlist <- c("getahead", "degree", "all_work","some_work","luck",
                "less_then_hs","highschool","jr_college","Bachelor","Graduate",
                "yes_man","yes_women",
                "county_nofarm","farm","town","largetown","bigcitysuburb","city",
                "black", "white", "other", "homosexual", "bisexual", "heterosexual", 
                "satisfied", "kinda_satisfied", "not_satisfied")



### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)


####################################################################################
############              PHASE 3: Descriptive Statistics     ############
####################################################################################
# TABLE 1: DESCRIPTIVE STATISTICS HERE
describe(my_dataset)

####################################################################################
############              PHASE 4: Contingency Table + Chi2                  ############
####################################################################################


# TABLE 2: CONTINGENCY TABLE HERE
table(my_dataset$degree, my_dataset$getahead)
chisq.test(table(my_dataset$degree, my_dataset$getahead))

####################################################################################
############              PHASE 4: corr matrix                  ############
####################################################################################
cor_matrix <- cor(my_dataset)
cor_allwork <- cor_matrix["all_work", ]
print(round(cor_allwork, 2))






####################################################################################
############             regression                  ############
####################################################################################
model1a <- glm(all_work ~ white + black + other,
               data = my_dataset, family = binomial)
summary(model1a)
pR2(model1a)



model2a <- glm(all_work ~ yes_man + yes_women + homosexual + bisexual + heterosexual,
               data = my_dataset, family = binomial)
summary(model2a)
pR2(model2a)


model3a <- glm(all_work ~ less_then_hs + highschool + jr_college + Bachelor + Graduate +
                 satisfied + kinda_satisfied + not_satisfied +
                 county_nofarm + farm + town + largetown + bigcitysuburb + city,
               data = my_dataset, family = binomial)
summary(model3a)
pR2(model3a)

model4a <- glm(all_work ~ white + black + other + yes_man + yes_women + homosexual + 
                 bisexual + heterosexual + less_then_hs + highschool + jr_college + 
                 Bachelor + Graduate + satisfied + kinda_satisfied + not_satisfied +
                 county_nofarm + farm + town + largetown + bigcitysuburb + city,
               data = my_dataset, family = binomial)
summary(model4a)
pR2(model4a)






