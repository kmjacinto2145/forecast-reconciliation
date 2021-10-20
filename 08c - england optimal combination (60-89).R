#Installs required packages
library(StMoMo)
library(dplyr)
library(tidyr)
library(comprehenr)
library(forecast)
library(ggplot2)
library(reshape2)
library(demography)
library(purrr)
library(readxl)
library(parallel)

#Constants
H = 20
AGES = 60:89
YEARS = 2001:2018

#Import data
lc_decile_sex = readRDS("england_models 60-89/lc_decile_sex.rds")
eng_mort_decile_sex = readRDS("england_models 60-89/eng_mort.rds")
eng_mort_decile_total = readRDS("england_models 60-89/eng_mort_decile_total.rds")
eng_mort_eng_sex = readRDS("england_models 60-89/eng_mort_total_sex.rds")
eng_mort_eng_total = readRDS("england_models 60-89/eng_mort_total.rds")

#Import functions for getting the qxt and Ext
source("07d - get_qxt_Ext functions.R")

###############################################################################
#BASE FORECASTS
###############################################################################


#Get base forecasts for decile total
lc_decile_total = list()

for (decile in 1:10) {
    print(decile)

    #Get subset of deaths_population table and convert into matrix (ages x years)
    Ext = get_qxt_Ext_dectot(eng_mort_decile_total, decile, "Ext")
    qxt = get_qxt_Ext_dectot(eng_mort_decile_total, decile, "qxt")
    
    ages = AGES
    years = YEARS
    name = paste(decile, "Total", sep = "_")
    
    #Convert into demogdata type
    table_demogdata = demogdata(qxt, Ext, ages, years, 
                                "mortality", name, "Total")
    
    #Convert into StMoMo dataset
    table_stmomo = StMoMoData(data = table_demogdata)
    
    #Weights
    wxt <- genWeightMat(ages = table_stmomo$ages, 
                        years = table_stmomo$years, 
                        clip = 0)
    
    #Fit model
    LC = lc()
    LCfit <- fit(LC, 
                 data = table_stmomo, 
                 ages.fit = table_stmomo$ages, 
                 wxt = wxt)
    
    #Set to lc_decile_total
    lc_decile_total[[name]] = LCfit
    
}

#Get base forecasts for england by sex
lc_england_sex = list()

for (sex in c("Female", "Male")) {

    #Get subset of deaths_population table and convert into matrix (ages x years)
    Ext = get_qxt_Ext_engsex(eng_mort_eng_sex, sex, "Ext")
    qxt = get_qxt_Ext_engsex(eng_mort_eng_sex, sex, "qxt")
    
    ages = AGES
    years = YEARS
    name = paste("England", sex, sep = "_")
    
    #Convert into demogdata type
    table_demogdata = demogdata(qxt, Ext, ages, years, 
                                "mortality", name, sex)
    
    #Convert into StMoMo dataset
    table_stmomo = StMoMoData(data = table_demogdata)
    
    #Weights
    wxt <- genWeightMat(ages = table_stmomo$ages, 
                        years = table_stmomo$years, 
                        clip = 0)
    
    #Fit model
    LC = lc()
    LCfit <- fit(LC, 
                 data = table_stmomo, 
                 ages.fit = table_stmomo$ages, 
                 wxt = wxt)
    
    #Set to lc_decile_total
    lc_england_sex[[name]] = LCfit
    
}

#Get base forecasts for england total
lc_england_total = list()
#Get subset of deaths_population table and convert into matrix (ages x years)
Ext = get_qxt_Ext_engtot(eng_mort_eng_total, "Ext")
qxt = get_qxt_Ext_engtot(eng_mort_eng_total, "qxt")

ages = AGES
years = YEARS
name = paste("England", "Total", sep = "_")

#Convert into demogdata type
table_demogdata = demogdata(qxt, Ext, ages, years, 
                            "mortality", name, "Total")

#Convert into StMoMo dataset
table_stmomo = StMoMoData(data = table_demogdata)

#Weights
wxt <- genWeightMat(ages = table_stmomo$ages, 
                    years = table_stmomo$years, 
                    clip = 0)

#Fit model
LC = lc()
LCfit <- fit(LC, 
             data = table_stmomo, 
             ages.fit = table_stmomo$ages, 
             wxt = wxt)

#Set to lc_decile_total
lc_england_total[[name]] = LCfit

#Combine all lc models together
lc_all = c(lc_england_total, lc_england_sex, lc_decile_total, lc_decile_sex)


# saveRDS(lc_all, "england_models 60-89/lc_all.rds")



#Calculate base forecasts for each model
eng_base_forecasts_oc = list()

i = 1
for (model in lc_all) {
    forecasts = forecast(model, h = H)
    eng_base_forecasts_oc[[i]] = forecasts$rates
    i = i + 1
}

names(eng_base_forecasts_oc) = names(lc_all)

years = 2019:2038
ages = AGES

#Reshape base forecasts
eng_base_forecasts_oc_shaped = list()

for (year in years) {
    for (age in ages) {
        tmp_vec = sapply(eng_base_forecasts_oc, function(series){
            #Get the value at the specified age and year for each series
            return(series[as.character(age), as.character(year)])
        })
        
        eng_base_forecasts_oc_shaped[[as.character(year)]][[as.character(age)]] = tmp_vec
    }
}

###############################################################################
#REVISED FORECASTS
###############################################################################

#Import summing matrices
St_list = readRDS("england_models 60-89/St_list.rds")
names(St_list) = years

#Calculate revised forecasts
R_forecasts_oc = list()

for (year in years) {
    for (age in ages) {
        R = eng_base_forecasts_oc_shaped[[as.character(year)]][[as.character(age)]]
        S = St_list[[as.character(year)]][[as.character(age)]]
        
        result = S %*% solve(t(S) %*% S) %*% t(S) %*% R
        
        R_forecasts_oc[[as.character(year)]][[as.character(age)]] = as.vector(result)
    }
}

saveRDS(R_forecasts_oc, "england_models 60-89/forecasts_oc.rds")

