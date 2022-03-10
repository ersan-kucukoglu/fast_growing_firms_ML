#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.

# Chapter 17
# CH17A
# using the bisnode-firmd dataset
# version 0.9 2020-09-10
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP

# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)

library(rattle)
library(caret)
library(pROC)
library(ranger)
library(rpart)
library(partykit)
library(rpart.plot)
library(tidyverse)
library(lubridate)
library(RColorBrewer)


# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, data used
setwd("~/Desktop/BA-Courses/Data_Analysis3/")          
# alternative: give full path here, 
#            example data_dir="C:/Users/bekes.gabor/Dropbox (MTA KRTK)/bekes_kezdi_textbook/da_data_repo"

# load theme and functions
source("Assignment3/code/theme_bg.R")
source("Assignment3/code/da_helper_functions.R")


###########################################################
# Import data
###########################################################


data <- read_csv("~/Desktop/BA-Courses/Data_Analysis3/Assignment3/data/raw/cs_bisnode_panel.csv")
glimpse(data)


to_filter <- sapply(data, function(x) sum(is.na(x)))
sort(to_filter[to_filter > 0])

# drop variables with many NAs 200k
data <- data %>%
  select(-c(exit_date,exit_year,COGS,net_dom_sales,net_exp_sales,wages,finished_prod,D)) %>%
  filter(between(year, 2010, 2015))

###########################################################
# label engineering
###########################################################

# add all missing year and comp_id combinations -
# originally missing combinations will have NAs in all other columns
data <- data %>%
  complete(year, comp_id)

# generate status_alive; if sales larger than zero and not-NA, then firm is alive
data  <- data %>%
  mutate(status_alive = sales > 0 & !is.na(sales) %>%
           as.numeric(.))

data <- data %>%
  group_by(comp_id) %>% 
  mutate(pct_change = (sales/lag(sales) - 1) * 100)

#data <- data %>% mutate(sales = ifelse(year == 2011 & sales == 0, 1, sales))

data <- data %>%
  group_by(comp_id) %>% 
  mutate(previous_growth = (lag(sales, 2)/lag(sales, 3) - 1) * 100)


summary(data$sales)

data <- data %>%
  mutate(sales = ifelse(sales < 0, 1, sales),
         ln_sales = ifelse(sales > 0, log(sales), 0),
         sales_mil=sales/1000000,
         sales_mil_log = ifelse(sales > 0, log(sales_mil), 0))

data$sales_mil_log_sq <- (data$sales_mil_log)^2 


# Change in sales
data <- data %>%
  group_by(comp_id) %>%
  mutate(d1_sales_mil_log = sales_mil_log - Lag(sales_mil_log, 1) ) %>%
  ungroup()


# replace w 0 for new firms + add dummy to capture it
data <- data %>%
  mutate(age = (year - founded_year) %>%
           ifelse(. < 0, 0, .),
         new = as.numeric(age <= 1) %>% #  (age could be 0,1 )
           ifelse(balsheet_notfullyear == 1, 1, .),
         d1_sales_mil_log = ifelse(new == 1, 0, d1_sales_mil_log),
         new = ifelse(is.na(d1_sales_mil_log), 1, new),
         d1_sales_mil_log = ifelse(is.na(d1_sales_mil_log), 0, d1_sales_mil_log))

data <- data %>%
  mutate(flag_low_d1_sales_mil_log = ifelse(d1_sales_mil_log < -1.5, 1, 0),
         flag_high_d1_sales_mil_log = ifelse(d1_sales_mil_log > 1.5, 1, 0),
         d1_sales_mil_log_mod = ifelse(d1_sales_mil_log < -1.5, -1.5,
                                       ifelse(d1_sales_mil_log > 1.5, 1.5, d1_sales_mil_log)),
         d1_sales_mil_log_mod_sq = d1_sales_mil_log_mod^2
  )

data  <- data %>%
  filter(pct_change != is.na(pct_change))
  
describe(data$pct_change)

# fast growth is equivalent to doubling sales year-on-year
data <- data %>%
  group_by(comp_id) %>%
  mutate(fast_growth = (pct_change > 100) %>%
           as.numeric(.)) %>%
  ungroup()

table(data$fast_growth)

###########################################################
# sample design
###########################################################


data %>% filter(!is.na(fast_growth)) %>% 
  group_by(year) %>% summarise(n())
# we have most observations from 2014

describe(data$sales)

# look at cross section
data <- data %>%
  filter((year == 2014) & (status_alive == 1)) %>%
  # look at firms below 10m euro revenues and above 1000 euros
  filter(!(sales_mil > 10)) %>%
  filter(!(sales_mil < 0.001))


Hmisc::describe(data$fast_growth)
###########################################################
# Feature engineering
###########################################################

# create age variable
data <- data %>%
  mutate(age = (2014 - year(founded_date))) %>% 
  filter(age > 0)

# change some industry category codes
data <- data %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
  )


table(data$ind2_cat)

# Firm characteristics
data <- data %>%
  mutate(age2 = age^2,
         foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))

###########################################################
# look at more financial variables, create ratios
###########################################################

# assets can't be negative. Change them to 0 and add a flag.
data <-data  %>%
  mutate(flag_asset_problem=ifelse(intang_assets<0 | curr_assets<0 | fixed_assets<0,1,0  ))
table(data$flag_asset_problem)

data <- data %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))

# generate total assets
data <- data %>%
  mutate(total_assets_bs = intang_assets + curr_assets + fixed_assets)
summary(data$total_assets_bs)


pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit_loss_year", "personnel_exp")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets" )

# divide all pl_names elements by sales and create new column for it
data <- data %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
data <- data %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))


########################################################################
# creating flags, and winsorizing tails
########################################################################

# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

data <- data %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))


# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "inc_bef_tax_pl", "profit_loss_year_pl", "share_eq_bs")

data <- data %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))


# dropping flags with no variation
variances<- data %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

data <- data %>%
  select(-one_of(names(variances)[variances]))

########################################################################
# additional
# including some imputation
########################################################################

# CEO age
data <- data %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

data <- data %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))

# number emp, very noisy measure
data <- data %>%
  mutate(labor_avg_mod = ifelse(is.na(labor_avg), mean(labor_avg, na.rm = TRUE), labor_avg),
         labor_avg_mod_sq = labor_avg_mod **2, 
         flag_miss_labor_avg = as.numeric(is.na(labor_avg)))

summary(data$labor_avg)
summary(data$labor_avg_mod)

data <- data %>%
  select(-labor_avg)

# create factors
data <- data %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(data$ind2_cat))))

data <- data %>%
  mutate(fast_growth_f = factor(fast_growth, levels = c(0,1)) %>%
           recode(., `0` = 'not_fast_growth', `1` = "fast_growth"))

# no more imputation, drop obs if key vars missing
data <- data %>%
  filter(!is.na(liq_assets_bs),!is.na(foreign))

# drop missing
data <- data %>%
  filter( !is.na(material_exp_pl), !is.na(m_region_loc))
Hmisc::describe(data$age)

# drop exit_date and birth_year
data <- data %>% select(-c('birth_year'))

# drop unused factor levels
data <- data %>%
  mutate_at(vars(colnames(data)[sapply(data, is.factor)]), funs(fct_drop))

# impute values
data <- data %>%
  mutate(flag_previous_growth = ifelse(is.na(previous_growth) | is.nan(previous_growth), 1, 0 ),
         previous_growth = ifelse(is.na(previous_growth) | is.nan(previous_growth), 0, previous_growth ))

ggplot(data = data, aes(x=inc_bef_tax_pl, y=as.numeric(fast_growth))) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method="loess", se=F, colour="black", size=1.5, span=0.9) +
  labs(x = "Income before taxes",y = "Fast Growth distribution") +
  theme_bw() +
  scale_x_continuous(limits = c(-1.5,1.5), breaks = seq(-1.5,1.5, 0.5))

# export data to csv and rds
write_csv(data, "~/Desktop/BA-Courses/Data_Analysis3/Assignment3/data/clean/bisnode_firms_clean.csv")
saveRDS(data, "~/Desktop/BA-Courses/Data_Analysis3/Assignment3/data/clean/bisnode_firms_clean.rds")

