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

#Import data
deaths = read.csv("input_data/imd_deaths_19812018.csv", header = TRUE)
population = read.csv("input_data/imd_populations_20012018.csv", header = TRUE)

#Combine data together
eng_mort = merge(deaths, population, by = c("year", "sex", "imd", "age"))

#Calculate death rate
eng_mort$rate = eng_mort$deaths / eng_mort$population

get_qxt_Ext = function(data, decile, gender, type) {
    
    if (type == "Ext") {
        print(sex)
        subset = data %>% 
            filter(imd == decile, sex == gender) %>%
            select(year, age, population) %>%
            pivot_wider(id_cols = c("year", "age"), names_from = "year", values_from = "population") %>%
            as.data.frame()
    } else if (type == "qxt") {
        print(sex)
        subset = data %>% 
            filter(imd == decile, sex == gender) %>%
            select(year, age, rate) %>%
            pivot_wider(id_cols = c("year", "age"), names_from = "year", values_from = "rate") %>%
            as.data.frame()
    } else {
        stop("Parameter 'type' must be either 'Ext' or 'qxt'.")
    }
    subset = subset[order(subset$age),]
    rownames(subset) = subset$age
    subset = subset %>%
        select(-age) %>%
        as.matrix()
    
    return(subset)
}

lc_decile_sex = list()
#Fit models for each sex and decile
for (decile in 1:10) {
    print(decile)
    for (sex in c("Female", "Male")) {
        
        #Get subset of deaths_population table and convert into matrix (ages x years)
        Ext = get_qxt_Ext(eng_mort, decile, sex, "Ext")
        qxt = get_qxt_Ext(eng_mort, decile, sex, "qxt")
        
        ages = 0:89
        years = 2001:2018
        name = paste(decile, sex, sep = "_")
        
        #Convert into demogdata type
        table_demogdata = demogdata(qxt, 
                                    Ext, 
                                    ages, 
                                    years, 
                                    "mortality", 
                                    name, 
                                    sex)
        
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
        
        #Set to lc_england
        lc_decile_sex[[name]] = LCfit
        
    }
}

# saveRDS(lc_decile_sex, "england_models/lc_decile_sex.rds")

# saveRDS(eng_mort, "england_models/eng_mort.rds")

#Get data for total
eng_mort_decile_total = eng_mort %>%
    select(-rate) %>%
    group_by(year, imd, age) %>%
    summarise(deaths = sum(deaths), 
              population = sum(population)) %>%
    mutate(rate = deaths/population)

eng_mort_total_sex = eng_mort %>% 
    select(-rate) %>% 
    group_by(year, sex, age) %>% 
    summarise(deaths = sum(deaths),
              population = sum(population)) %>%
    mutate(rate = deaths/population)

eng_mort_total = eng_mort_total_sex %>% 
    group_by(year, age) %>% 
    summarise(deaths = sum(deaths), 
              population = sum(population)) %>%
    mutate(rate = deaths/population)

# saveRDS(eng_mort_decile_total, "england_models/eng_mort_decile_total.rds")
# saveRDS(eng_mort_total_sex, "england_models/eng_mort_total_sex.rds")
# saveRDS(eng_mort_total, "england_models/eng_mort_total.rds")
