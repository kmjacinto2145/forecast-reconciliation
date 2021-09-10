#Installs required packages
library(StMoMo)
library(readxl)    
library(dplyr)
library(tidyr)
library(demography)
library(tidyverse)
library(comprehenr)
library(forecast)
library(purrr)

#constants
H = 20

#import data
lc_female = readRDS("models/lc_female.rds")
lc_male = readRDS("models/lc_male.rds")
lc_total = readRDS("models/lc_total.rds")

merge_two_lists_alternating = function(list1, list2) {
    
    new_list = list()
    
    if (length(list1) >= length(list2)) {
        longer_list = list1
        shorter_list = list2
    } else {
        longer_list = list2
        shorter_list = list1
    }
    
    shorter = length(shorter_list)
    longer = length(longer_list)
    
    i = 1
    for (j in 1:longer) {
        
        new_list[[i]] = longer_list[[j]]
        # names(new_list[[i]]) = names(longer_list[[j]])
        i = i + 1
        if (j <= shorter) {
            new_list[[i]] = shorter_list[[j]]
            # names(new_list[[i]]) = names(shorter_list[[j]])
            i = i + 1
        }
    }
    
    new_names = c(rbind(names(longer_list), names(shorter_list)))
    
    names(new_list) = new_names
    
    return(new_list)
}

#combines male and female models together, alternating
lc_combined = merge_two_lists_alternating(lc_female, lc_male)

lc_combined_regions = lc_combined[3:length(lc_combined)] #gets just prefectures
lc_combined_japan = lc_combined[1:2] #gets japan only

lc_total_regions = lc_total[2:length(lc_total)]
lc_total_japan = list(lc_total[[1]])
names(lc_total_japan) = "Japan_total"

lc_combined_total = c(lc_total_japan, lc_combined_japan, 
                      lc_total_regions, lc_combined_regions)

###############################################################################
#GET BASE FORECASTS
###############################################################################

base_forecast = function(model_list, h, min_age, max_age) {
    
    base_forecast_all_ages = list()
    
    i = 1
    for (age in min_age:max_age) {
        print(age)
        age_matrix = matrix(nrow = 0, ncol = h)
        j = 1
        for (model in model_list) {
            base = forecast(model, h = h) #h-period forecast for single model
            Rhat = base$rates[c(as.character(age)),] #gets forecast for one age at a time
            age_matrix = rbind(age_matrix, Rhat) #combines forecast with other splits
            j = j + 1
        }
        
        #changes row names to match the model names
        rownames(age_matrix) = names(model_list)
        
        #adds data to list
        base_forecast_all_ages[[i]] = age_matrix
        i = i + 1
    }
    
    #labels the list with the ages
    names(base_forecast_all_ages) = min_age:max_age
    return(base_forecast_all_ages)
}

#Base forecasts
lc_combined_total_b = base_forecast(lc_combined_total, H, 0, 100)

#Get projected summing matrices from bottom-up analysis
St_forecasted = readRDS("forecasts/summing_matrices_forecasted2.rds")

################################################################################
### REVISED FORECASTS
################################################################################

ages = 0:100
years = 1:H

revised_forecasts_all = list()
i = 1

for (age in ages) {
    print(age)
    revised_forecasts_age = list()
    for (year in years) {
        #Summing matrix
        S = St_forecasted[[as.character(age)]][[year]]
        
        #Base forecasts
        R = lc_combined_total_b[[as.character(age)]][,year]
        
        #Calculate revised forecasts
        revised_forecasts_year = S %*% solve(t(S) %*% S) %*% t(S) %*% R
        
        revised_forecasts_age[[year]] = revised_forecasts_year
    }
    revised_forecasts_all[[i]] = revised_forecasts_age
    i = i + 1
}

names(revised_forecasts_all) = ages

saveRDS(revised_forecasts_all, file = "forecasts/optimal_comb.rds")
