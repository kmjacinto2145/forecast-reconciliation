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
lc_combined_b = base_forecast(lc_combined, H, 0, 100)
lc_combined_regions_b = base_forecast(lc_combined_regions, H, 0, 100)

###############################################################################
#GET PROJECTED SUMMING MATRICES
###############################################################################

#Creates an indexed matrix of the exposures by age, year, sex, and region
Ext_combined <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(Ext_combined) <- c("Age", "Year", "Ext", "Sex", "Region")

for (model in lc_combined) {
    ext = model$Ext
    ext = as.data.frame(ext)
    ext$Age = rownames(ext)
    ext = pivot_longer(data = ext, 
                       names_to = "Year", 
                       values_to = "Ext", 
                       cols = -one_of(c("Age")))
    ext$Sex = model$data$series
    ext$Region = model$data$label
    
    Ext_combined = rbind(Ext_combined, ext)
}

Ext_total = Ext_combined %>% 
    group_by(Age, Year, Region) %>% 
    summarise(Ext = sum(Ext))
Ext_total$Sex = "Total"
Ext_combined = rbind(Ext_combined, Ext_total)
Ext_combined$Age = as.numeric(Ext_combined$Age)
Ext_combined$Year = as.numeric(Ext_combined$Year)

#slice to get just common years
Ext_years = Ext_combined[,c("Age", "Year", "Region")] %>% 
    group_by(Age, Region) %>% 
    summarise(Min_year = min(Year))

start_year = max(Ext_years$Min_year)
Ext_combined = Ext_combined[Ext_combined$Year >= start_year,]

#get vectors to loop through
ages = unique(Ext_combined$Age)
years = unique(Ext_combined$Year)
prefectures = unique(Ext_combined$Region)
prefectures = prefectures[prefectures != "Japan"]

#List of all summing matrices by age and year
St_all_ages_years = list()

i = 1
for (age in ages) {
    print(age)
    
    St_age = list()
    
    j = 1
    
    for (year in years) {
        print(year)
        #Creates the summing matrix
        St = matrix(nrow = 0, ncol = length(prefectures) * 2)
        
        #Part 1: Get projections for Japan
        
        #E_Japan*T,t
        japan_total = Ext_combined[(Ext_combined$Age == age) & 
                                       (Ext_combined$Year == year) & 
                                       (Ext_combined$Region == "Japan") &
                                       (Ext_combined$Sex == "Total"),"Ext"] %>% 
            pull()
        
        #E_Japan*F,t
        japan_female = Ext_combined[(Ext_combined$Age == age) & 
                                        (Ext_combined$Year == year) & 
                                        (Ext_combined$Region == "Japan") &
                                        (Ext_combined$Sex == "female"),"Ext"] %>% 
            pull()
        #E_Japan*M,t
        japan_male = Ext_combined[(Ext_combined$Age == age) & 
                                      (Ext_combined$Year == year) & 
                                      (Ext_combined$Region == "Japan") &
                                      (Ext_combined$Sex == "male"),"Ext"] %>% 
            pull()
        
        #Gets the 94 elements that comprise the numerator of the rows
        numerators = Ext_combined[(Ext_combined$Age == age) & 
                                      (Ext_combined$Year == year) & 
                                      (Ext_combined$Region != "Japan") &
                                      (Ext_combined$Sex != "Total"),]
        
        #Adds region and sex
        numerators$Region <- factor(numerators$Region, levels = prefectures)
        numerators$Sex <- factor(numerators$Sex, levels = c("female", "male"))
        
        #Reorders so that regions are together
        numerators = numerators[order(numerators$Region, numerators$Sex),]
        
        #Numerators for rows 2 and 3 of the matrix
        numerators_female = to_vec(for (rowname in rownames(numerators)) 
            if (numerators[rowname,"Sex"] == "female") 
                numerators[rowname,"Ext"] 
            else 0)
        numerators_male = to_vec(for (rowname in rownames(numerators)) 
            if (numerators[rowname,"Sex"] == "male") 
                numerators[rowname,"Ext"] 
            else 0)
        
        #Rows 1-3 of the matrix
        japan_total = numerators$Ext/japan_total
        japan_female = numerators_female/japan_female
        japan_male = numerators_male/japan_male
        
        St = rbind(St, japan_total, japan_female, japan_male)
        
        #Part 2: Get projections for region totals
        region_total = Ext_combined[(Ext_combined$Age == age) & 
                                        (Ext_combined$Year == year) & 
                                        (Ext_combined$Region != "Japan") &
                                        (Ext_combined$Sex == "Total"),]
        
        region_total$Region <- factor(region_total$Region, levels = prefectures)
        region_total_vec = region_total[order(region_total$Region),] %>% pull(Ext)
        
        #Matrix of numerators for part 2
        num = matrix(nrow = 0, ncol = length(prefectures) * 2)
        
        #Only shows value if prefecture is correct
        for (region in prefectures) {
            current_numerators = numerators
            current_numerators$Is_region = ifelse(
                current_numerators$Region == region, 1, 0
            )
            row = current_numerators$Ext * current_numerators$Is_region
            num = rbind(num, row)
        }
        
        #Matrix of denominators for part 2
        denom = matrix(data = rep(region_total_vec, 
                                  each = length(prefectures) * 2), 
                       ncol = length(prefectures) * 2, 
                       byrow = T)
        
        St = rbind(St, num/denom)
        
        #Part 3: identity matrix, representing the revised forecasts 
        #of the most disaggregated level (which would be unchanged)
        disagg_matrix = diag(length(prefectures) * 2)
        St = rbind(St, disagg_matrix)
        
        St_age[[j]] = St
        
        j = j + 1
    }
    
    St_all_ages_years[[i]] = St_age
    
    i = i + 1
}

# saveRDS(St_all_ages_years, file = "forecasts/summing_matrix.rds")
# St_all_ages_years = readRDS("forecasts/summing_matrix.rds")

pt_list = list() #List of all non-zero elements in the summing matrices, by age

i = 1
for (age in St_all_ages_years) {
    print(i - 1)
    
    #Create matrix of all values in each summing matrix
    age_matrix = matrix(nrow = 13536, ncol = 0)
    for (year in age) {
        year_vector = c(year) #Vectorises each summing matrix
        age_matrix = cbind(age_matrix, year_vector)
    }
    
    #Removes zero rows from age_matrix
    age_matrix = age_matrix[rowSums(age_matrix[,]) > 0,]
    colnames(age_matrix) = years
    
    pt_list[[i]] = age_matrix
    i = i + 1
}

#Perform auto.arima forecasting for each series
pt_forecast_all_list = list()

i = 1
for (age in pt_list) {
    print(i - 1)
    j = 1
    pt_forecast_age = list()
    for (row in 1:nrow(age)) {
        series = age[row,]
        forecast_model = auto.arima(series)
        forecasts = forecast(forecast_model, h = H)
        pt_forecast_age[[j]] = forecasts
        j = j + 1
    }
    pt_forecast_all_list[[i]] = pt_forecast_age
    i = i + 1
}

names(pt_forecast_all_list) = ages

# saveRDS(pt_list, file = "pt_list.rds")
# saveRDS(pt_forecast_all_list, file = "pt_forecast_all_list.rds")

# pt_list = readRDS("pt_list.rds")
# pt_forecast_all_list = readRDS("pt_forecast_all_list.rds")


#Need the non-zero/zero pattern of a summing matrix
St_example = St_all_ages_years[[1]][[1]]
St_vector = c(St_example)
St_nonzero_indices = which(St_vector > 0)

#Generate summing matrices for each age and year
summing_matrices_forecasted = list()
i = 0
for (age in pt_forecast_all_list) {
    print(i)
    pt_forecasted_age = list()
    for (h in 1:H) {
        
        pt_forecasted = to_vec(for(pt in age) pt$mean[h])
        
        #convert pt vector into matrix
        pt_zeroes = rep(0, 13536)
        
        pt_forecasted = replace(pt_zeroes, St_nonzero_indices, pt_forecasted)
        
        pt_forecasted_matrix = matrix(pt_forecasted, 
                                      ncol = length(prefectures) * 2)
        pt_forecasted_age[[h]] = pt_forecasted_matrix
    }
    summing_matrices_forecasted[[i + 1]] = pt_forecasted_age
    i = i + 1
}

names(summing_matrices_forecasted) = ages

saveRDS(summing_matrices_forecasted, file = "forecasts/summing_matrices_forecasted2.rds")
#summing_matrices_forecasted = readRDS("forecasts/summing_matrices_forecasted.rds")

###############################################################################
#GET REVISED FORECASTS
###############################################################################
Rt_all = list()
for (age in ages) {
    print(age)
    Rt_age = list()
    for (year in 1:H) {
        St = summing_matrices_forecasted[[as.character(age)]][[year]]
        bt = lc_combined_regions_b[[as.character(age)]][,year]
        Rt = St %*% bt
        Rt_age[[year]] = Rt
    }
    Rt_all[[age + 1]] = Rt_age
}

names(Rt_all) = ages

#saveRDS(Rt_all, file = "forecasts/bottom_up.rds")
#saveRDS(Rt_all, file = "forecasts/bottom_up2.rds")
