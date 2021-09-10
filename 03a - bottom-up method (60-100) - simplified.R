#Installs required packages
library(StMoMo)
library(readxl)    
library(dplyr)
library(tidyr)
library(demography)
library(tidyverse)
library(comprehenr)
library(forecast)

#constants
H = 20

#import data
lc_female_60 = readRDS("models/lc_female_60.rds")
lc_male_60 = readRDS("models/lc_male_60.rds")

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
lc_combined = merge_two_lists_alternating(lc_female_60, lc_male_60)
lc_combined_regions = lc_combined[3:length(lc_combined)] #gets just prefectures

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
lc_combined_b = base_forecast(lc_combined, H, 60, 100)
lc_combined_regions_b = base_forecast(lc_combined_regions, H, 60, 100)

###############################################################################
#GET REVISED FORECASTS
###############################################################################

#This is in case you re-run the code
ages = 60:100
summing_matrices_forecasted = readRDS("forecasts/summing_matrices_forecasted.rds")


Rt_all = list()
i = 1
for (age in ages) {
    print(age)
    Rt_age = list()
    for (year in 1:H) {
        St = summing_matrices_forecasted[[as.character(age)]][[year]]
        bt = lc_combined_regions_b[[as.character(age)]][,year]
        Rt = St %*% bt
        Rt_age[[year]] = Rt
    }
    Rt_all[[i]] = Rt_age
    i = i + 1
}

names(Rt_all) = ages

saveRDS(Rt_all, file = "forecasts/bottom_up2_60.rds")
