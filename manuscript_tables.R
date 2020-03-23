library(tidyverse)
if(!require(texreg)) install.packages("texreg"); library(texreg)

#######################################
####   DOWNLOAD DATA FROM GITHUB   ####
#######################################
df_full <- read_csv("https://raw.githubusercontent.com/HunterRatliff1/NSG_geography/master/public_data/regression.csv") %>%
  mutate(region   = factor(region),
         division = factor(division)) %>%
  mutate(male   = male * 100,
         Over65 = Over65 *100) %>%
  
  mutate(region       = forcats::fct_relevel(region, "NORTHEAST"),
         division     = forcats::fct_relevel(division, "New England"),
         Urbanization = forcats::fct_relevel(Urbanization, "Metro"))


## --------------------    define `df`    --------------------
## This is our main data, after excluding counties with missing
## rates and a few outliers with high cooks distances or other
## problems on diagnostic plots
df <- df_full %>%
  ## Outliers
  filter(!GEOID %in% c("38053","20055","30085","12119","30021")) %>%
  filter(!is.na(Deaths)) # drop counties with missing rates


## ---------------    `df_compare_missing`    ---------------
## Add a column with a logical flag indicating if the county
## is excluded in the main analysis
df_compare_missing <- df_full %>%
  mutate(Exclude = is.na(Deaths) | GEOID %in% c("38053","20055","30085","12119","30021")) %>%
  group_by(Exclude) 



########################################
#      CODEBOOK FOR MAIN VARIABLES     #
#  Definitions of key columns in       #
#  the data.frame                      #
########################################

## >>  GEOID, NAME, ST  
##
## These are unique identifiers for each county. Not used in the analysis,
## but are used for plotting the maps

## >> dist_cent
## This is the main measure of distance in the analysis

## >> dist_min
## Measure of distance in sensitivity analysis

## >> male, Over65
## Vectors showing the percent of the county that is male and 
## percent that are 65 or older (based on ACS data)

## >> Urbanization
## Factors for level of urbanization, derived from 2013 RUCC codes

## >> Crude, AgeAdj
## Crude and age adjusted TBI mortality rates. NA if rates are not provided

## >> StdPop
## Standardized population used by the CDC to calcualte the crude and age 
## adjusted rates. Not used directly in the analysis
  



###############################
####   Tests for Table 1   ####
###############################
qplot(dist_cent, Exclude, geom="boxplot", data=df_compare_missing)
qplot(dist_cent, fill=Exclude, geom="density", alpha=.5, data=df_compare_missing)
t.test(dist_cent ~ Exclude, data=df_compare_missing)

qplot(StdPop, fill=Exclude, geom="density", alpha=.5, data=df_compare_missing) + scale_x_log10()
wilcox.test(StdPop ~ Exclude, data=df_compare_missing)

mosaicplot(Urbanization~Exclude, data=df_compare_missing)
chisq.test(df_compare_missing$Urbanization, df_compare_missing$Exclude)

qplot(male, Exclude, geom="boxplot", data=df_compare_missing)
qplot(male, fill=Exclude, geom="density", alpha=.5, data=df_compare_missing)
t.test(male ~ Exclude, data=df_compare_missing)

qplot(Over65, Exclude, geom="boxplot", data=df_compare_missing)
qplot(Over65, fill=Exclude, geom="density", alpha=.5, data=df_compare_missing)
t.test(Over65 ~ Exclude, data=df_compare_missing)



##############################################
####   Anova for bivariate urbanization   ####
##############################################
## at the end of the "bivariate parahraph"
urban_anova <- aov(Crude~Urbanization, data=df)
car::Anova(urban_anova, type="III")

# homogeneity of variance :(
bartlett.test(Crude~Urbanization, data=df)
car::leveneTest(Crude~Urbanization, data=df)


oneway.test(Crude~Urbanization, data=df,
            var.equal=FALSE)
kruskal.test(Crude~Urbanization, data=df)



#####################################
####   TABLE 2 & 3: Regressions   ###
#####################################
## This is our main regression
base_fit  <- lm(Crude  ~ dist_cent + male + Over65 + Urbanization, data=df)

## list of SLR's to pass to table 2
list_of_models <- list(
  lm(Crude~dist_cent, data=df),
  lm(Crude~male, data=df),
  lm(Crude~Over65, data=df),
  lm(Crude~Urbanization, data=df),
  base_fit
)
  
## re-create table 2
texreg::screenreg(list_of_models,
                  ci.force = TRUE, 
                  custom.model.names = c("Distance", "Male", "65+", "Urban","Multivariate")
)

## re-create table 3
screenreg(ci.force=T, custom.model.names=c("Base", "Age Adjusted", "Sens 1", "Sens 2"),
          list(
            base_fit,
            update(base_fit, AgeAdj~.),
            update(base_fit, .~. + dist_min - dist_cent),
            update(base_fit, .~. + dist_min - dist_cent, subset = dist_min!=0)
))
