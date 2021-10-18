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

#Import data
lc_england = readRDS("england_models/lc_england.rds")
eng_mort_decile_sex = readRDS("england_models/eng_mort.rds")
eng_mort_eng_sex = readRDS("england_models/eng_mort_total_sex.rds")
eng_mort_eng_total = readRDS("england_models/eng_mort_total.rds")

#Base forecasts
H = 20

#Forecast 20 years ahead for each model
lc_england_forecasts = list()

i = 1
for (model in lc_england) {
    forecasts = forecast(model, h = H)
    lc_england_forecasts[[i]] = forecasts$rates
    i = i + 1
}

names(lc_england_forecasts) = names(lc_england)

##SUMMING MATRIX

pt_get = function(data){
    print(i)
    forecasts_matrix = matrix(nrow = 0, ncol = H)
    for (current_age in 0:89) {
        series = data %>% 
            filter(age == current_age) %>%
            select(year, pt)
        
        rownames(series) = series$year
        series = select(series, pt) #1d array
        
        #Fits auto.arima
        forecasts = auto.arima(series)
        
        #Produces forecasts
        forecasts = forecast(forecasts, h = H)
        
        #Appends forecasts to forecast_vec
        forecasts_matrix = rbind(forecasts_matrix, forecasts$mean)
        
    }
    i = i + 1
    return(forecasts_matrix)
}

#DECILE-TOTAL
eng_mort_decile_sex = eng_mort_decile_sex %>% 
    group_by(year, imd, age) %>% 
    summarise(pop_total = sum(population)) %>%
    merge(eng_mort_decile_sex, by = c("year", "imd", "age"))
    
eng_mort_decile_sex$pt = eng_mort_decile_sex$population / eng_mort_decile_sex$pop_total

eng_mort_decile_sex_list = list()
for (decile in 1:10) {
    for (gender in c("Female", "Male")) {
        name = paste(decile, gender, sep = "_")
        subset = eng_mort_decile_sex %>% 
            filter(imd == decile, sex == gender) %>%
            select(year, age, population, rate, pt)
        
        eng_mort_decile_sex_list[[name]] = subset
    }
}

i = 1
pt_decile_total_proj = eng_mort_decile_sex_list %>% mclapply(pt_get)

#ENGLAND-SEX
eng_mort_decile_sex2 = eng_mort_decile_sex %>%
    select(year, imd, age, sex, deaths, population)

eng_mort_eng_sex = eng_mort_eng_sex %>% 
    group_by(year, sex, age) %>% 
    summarise(pop_total = sum(population)) %>%
    merge(eng_mort_decile_sex2, by = c("year","sex", "age")) %>%
    mutate(pt = population/pop_total,
           rate = deaths/population)

eng_mort_eng_sex_list = list()
for (decile in 1:10) {
    for (gender in c("Female", "Male")) {
        name = paste(decile, gender, sep = "_")
        subset = eng_mort_eng_sex %>% 
            filter(imd == decile, sex == gender) %>%
            select(year, age, population, rate, pt)
        
        eng_mort_eng_sex_list[[name]] = subset
    }
}

pt_eng_sex_proj = eng_mort_eng_sex_list %>% mclapply(pt_get)

#ENGLAND-TOTAL
eng_mort_eng_total = eng_mort_eng_total %>% 
    group_by(year, age) %>% 
    summarise(pop_total = sum(population)) %>%
    merge(eng_mort_decile_sex2, by = c("year", "age")) %>%
    mutate(pt = population/pop_total,
           rate = deaths/population)

eng_mort_eng_total_list = list()
for (decile in 1:10) {
    for (gender in c("Female", "Male")) {
        name = paste(decile, gender, sep = "_")
        subset = eng_mort_eng_total %>% 
            filter(imd == decile, sex == gender) %>%
            select(year, age, population, rate, pt)
        
        eng_mort_eng_total_list[[name]] = subset
    }
}

pt_eng_total_proj = eng_mort_eng_total_list %>% mclapply(pt_get)

#Split eng_sex into eng_female and eng_male
pt_eng_female_proj = pt_eng_sex_proj[seq(1, 20, by = 2)]
pt_eng_male_proj = pt_eng_sex_proj[seq(2, 20, by = 2)]


#Rownames and colnames
rowcolnames = function(data){
    rownames(data) = 0:89
    colnames(data) = 1:H
    return(data)
}

pt_decile_total_proj = pt_decile_total_proj %>% lapply(rowcolnames)
pt_eng_female_proj = pt_eng_female_proj %>% lapply(rowcolnames)
pt_eng_male_proj = pt_eng_male_proj %>% lapply(rowcolnames)
pt_eng_total_proj = pt_eng_total_proj %>% lapply(rowcolnames)


##Combine summing matrix lists together
#Convert lists


h_age = function(l, ncol) {
    total_list = list()
    
    for (h in 1:20) {
        print(h)
        tmp_matrix = matrix(nrow = 0, ncol = ncol)
        
        for (age in 0:89) {
            tmp_vec = sapply(l, function(m) {
                return(m[as.character(age),h])
            })
            tmp_matrix = rbind(tmp_matrix, tmp_vec)
        }
        total_list[[as.character(h)]] = tmp_matrix
    }
    return(total_list)
}

zero_vec = rep(x = 0, times = 10)

age_h = function(l, type) {
    total_list = list()
    for (age in 0:89) {
        print(age)

        for (h in 1:H) {
            tmp_vec = sapply(l, function(m) {
                return(m[as.character(age),h])
            })
            
            if (type == "country_female") {
                tmp_matrix = matrix(nrow = 0, ncol = 10)
                tmp_matrix = rbind(tmp_matrix, tmp_vec, zero_vec)
                tmp_vec = c(tmp_matrix)
            } else if (type == "country_male") {
                tmp_matrix = matrix(nrow = 0, ncol = 10)
                tmp_matrix = rbind(tmp_matrix, zero_vec, tmp_vec)
                tmp_vec = c(tmp_matrix)
            } else if (type == "decile") {
                tmp_vec_f = tmp_vec[seq(1,20,by=2)]
                tmp_vec_m = tmp_vec[seq(2,20,by=2)]
                tmp_matrix = matrix(nrow = 0, ncol = 10)
                zero_matrix = matrix(0, nrow = 20, ncol = 10)
                tmp_matrix = rbind(tmp_vec_f, tmp_vec_m, zero_matrix)
                tmp_vec = c(tmp_matrix)
                tmp_vec = tmp_vec[1:(length(tmp_vec) - 20)]
                tmp_vec = matrix(tmp_vec, nrow = 10, ncol = 20, byrow = TRUE)
            }
            
            total_list[[as.character(age)]][[as.character(h)]] = tmp_vec
        }
        
    }
    return(total_list)
}

pt_decile_total_matrix = age_h(pt_decile_total_proj, "decile")
pt_eng_female_matrix = age_h(pt_eng_female_proj, "country_female")
pt_eng_male_matrix = age_h(pt_eng_male_proj, "country_male")
pt_eng_total_matrix = age_h(pt_eng_total_proj, "other")
pt_decile_sex_matrix = diag(20)


#Construct summing matrices
St_list = list()
for (h in 1:H) {
    print(h)
    for (age in 0:89) {
        St = matrix(nrow = 0, ncol = 20)
        
        St_1 = pt_eng_total_matrix[[as.character(age)]][[as.character(h)]]
        St_2 = pt_eng_female_matrix[[as.character(age)]][[as.character(h)]]
        St_3 = pt_eng_male_matrix[[as.character(age)]][[as.character(h)]]
        St_4 = pt_decile_total_matrix[[as.character(age)]][[as.character(h)]]
        St_5 = pt_decile_sex_matrix
        
        St = rbind(St, St_1, St_2, St_3, St_4, St_5)
        St_list[[as.character(h)]][[as.character(age)]] = St
    }
}

# saveRDS(St_list, "england_models/St_list.rds")

#Shape base forecasts
B_forecasts = list()
for (h in 1:H) {
    for (age in 0:89) {
        tmp_vec = sapply(lc_england_forecasts, function(m) {
            return(m[as.character(age),h])
        })
        
        B_forecasts[[as.character(h)]][[as.character(age)]] = tmp_vec
    }
}

#Get revised forecasts
R_forecasts = list()
for(h in 1:H) {
    print(h)
    for (age in 0:89) {
        B = B_forecasts[[as.character(h)]][[as.character(age)]]
        St = St_list[[as.character(h)]][[as.character(age)]]
        R_forecasts[[as.character(h)]][[as.character(age)]] = St %*% B
    }
}

# saveRDS(R_forecasts, "england_models/forecasts_bu.rds")
