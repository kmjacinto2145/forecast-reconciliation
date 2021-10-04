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
library(parallel)
library(future)

#Number of prefectures
PREFEC_COUNT = 47

#Data import
#function to read all sheets in a single excel workbook
readExcelAllsheets <- function(filename, tibble = FALSE) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    return(x)
}

#function to process data and project lee-carter models
#for all datasets in list

jmdToLC = function(table, sex, age_min, age_max, y_start, y_end) {
    
    #creates a lee-carter model object
    LC = lc() #CHANGED TO LOG LINK
    
    #Truncates the data
    table = table[(table$Age >= age_min) & (table$Age <= age_max),]
    table = table[(table$Year >= y_start) & (table$Year <= y_end),]
    
    #Shapes the deaths dataset
    if (sex %in% c("total", "male", "female")) {
        Dxt = table[,c("Year", "Age", paste0("d_", sex))]
        Dxt = spread(Dxt, Year, paste0("d_", sex))        
    } else {
        stop("Variable 'sex' needs to be either 'total', 'female', or 'male'.")
    }
    
    rownames(Dxt) = Dxt[,"Age"]
    Dxt = subset(Dxt, select = -Age)
    
    ages = as.double(rownames(Dxt))
    
    #Shapes the exposure dataset
    if (sex %in% c("total", "male", "female")) {
        Ext = table[,c("Year", "Age", paste0("E_", sex))]
        Ext = spread(Ext, Year, paste0("E_", sex))        
    } else {
        stop("Variable 'sex' needs to be either 'total', 'female', or 'male'.")
    }
    
    rownames(Ext) = Ext[,"Age"]
    Ext = subset(Ext, select = -Age)
    
    years = y_start:y_end #all years of the table
    
    qxt = Dxt/Ext #death rate table
    #qxt[qxt > 1] = 1 #sometimes there is an error in which qxt is greater than 1. This occurs for Iwate, for instance. This code removes this error by assuming qxt = 1
    
    name = deparse(substitute(series)) #name of the dataset
    
    #create demogdata object
    table_demogdata = demogdata(qxt, Ext, ages, 
                                years, "mortality", name, sex)
    
    #convert demogdata object into StMoMo object to be able to model
    table_stmomo = StMoMoData(data = table_demogdata)
    
    #weights
    wxt <- genWeightMat(ages = table_stmomo$ages, 
                        years = table_stmomo$years, clip = 0)
    
    #fit model
    LCfit <- fit(LC, data = table_stmomo, ages.fit = table_stmomo$ages, 
                 wxt = wxt)
    
    return(LCfit)
}

expandingWindowLC = function(table, sex) {
    
    forecasts_list = list()
    
    for (h in 0:9) {
        print(h)
        lc = jmdToLC(table, sex, 0, 100, 1975, 2009 + h)
        lc_forecast = forecast(lc, h = 10 - h)
        forecasts_list[[as.character(h)]] = lc_forecast$rates
    }
    return(forecasts_list)
}

#Data import
jmd = readExcelAllsheets("cleaned_data/jmd.xlsx")
japan_total = jmd[["Japan"]]

#get male datasets
jmd_male = list()

i = 1
for (table in jmd) {
    jmd_male[[i]] = table[,c("Year", "Age", "d_male", "E_male")]
    i = i + 1
}
names(jmd_male) = paste(names(jmd), "male", sep = "_") 

#get female datasets
jmd_female = list()

i = 1
for (table in jmd) {
    jmd_female[[i]] = table[,c("Year", "Age", "d_female", "E_female")]
    i = i + 1
}
names(jmd_female) = paste(names(jmd), "female", sep = "_") 

###############################################################################
# ERROR CALCULATION
###############################################################################

#Functions
source("06d - mafe rmsfe functions.R")

forecastMatrixToList = function(data) {
    data_list = list()
    for (i in 1:10) {
        if (i == 10) {
            data_list[[as.character(i-1)]] = data
        } else {
            data_list[[as.character(i-1)]] = data[,1:(11-i)]
            data = data[,-(1:(11-i))]
        }
    }
    return(data_list)
}


ptFemaleGet = function(prefec_data){
    print("New series") #Print indicator
    
    proj_matrix = matrix(ncol = 55, nrow = 0) #Matrix of projected pt
    
    for (age in 0:100) {
        cat("\r",age)
        
        forecast_vec = c()
        
        #Selects only data that matches the current age
        prefec_data_2 = prefec_data[prefec_data$Age == age,]
        
        #Gets just year and pt_female columns
        prefec_data_2 = prefec_data_2 %>% select(Year, pt_female)
        
        for (h in 1:10) {
            #Selects only years in the window
            series = prefec_data_2 %>%
                filter(Year < (2009 + h)) 
            rownames(series) = series$Year
            series = series %>% select(pt_female) #1d array
            
            #Fits auto.arima
            forecasts = series %>% auto.arima()
            
            #Produces forecasts
            forecasts = forecasts %>% forecast(h = h)
            
            #Appends forecasts to forecast_vec
            forecast_vec = append(forecast_vec, forecasts$mean)
            
        }
        proj_matrix = proj_matrix %>% rbind(forecast_vec)
    }
    rownames(proj_matrix) = 0:100
    return(proj_matrix)
}

ptMaleGet = function(prefec_data){
    print("New series") #Print indicator
    proj_matrix = matrix(ncol = 55, nrow = 0) #Matrix of projected pt
    
    for (age in 0:100) {
        cat("\r",age)
        forecast_vec = c()
        
        #Selects only data that matches the current age
        prefec_data_2 = prefec_data[prefec_data$Age == age,]
        
        #Gets just year and pt_male columns
        prefec_data_2 = prefec_data_2 %>% select(Year, pt_male)
        
        for (h in 1:10) {
            #Selects only years in the window
            series = prefec_data_2 %>%
                filter(Year < (2009 + h)) 
            rownames(series) = series$Year
            series = series %>% select(pt_male) #1d array
            
            #Fits auto.arima
            forecasts = series %>% auto.arima()
            
            #Produces forecasts
            forecasts = forecasts %>% forecast(h = h)
            
            #Appends forecasts to forecast_vec
            forecast_vec = append(forecast_vec, forecasts$mean)
            
        }
        proj_matrix = proj_matrix %>% rbind(forecast_vec)
    }
    rownames(proj_matrix) = 0:100
    return(proj_matrix)
}


#PREFECTURE-TOTAL
base_forecasts = readRDS("exp_window/indep_forecasts_prefecsex.rds")
jmd_prefecture_total = jmd %>% purrr::list_modify("Japan" = NULL)

#Calculates summing matrix ratios for each year and age
jmd_prefecture_total = jmd_prefecture_total %>%
    lapply(function(prefec_data){
        prefec_data = prefec_data[prefec_data$Age <= 100,]
        prefec_data$pt_female = prefec_data$E_female / prefec_data$E_total
        prefec_data$pt_male = prefec_data$E_male / prefec_data$E_total
        return(prefec_data)
    })

#Get projected pt_female
pt_female_proj = list()

pt_female_proj = jmd_prefecture_total %>%
    mclapply(ptFemaleGet)

#Get projected pt_male
pt_male_proj = jmd_prefecture_total %>%
    mclapply(ptMaleGet)

#saveRDS(pt_female_proj, "exp_window/pt_female_proj.rds")
#saveRDS(pt_male_proj, "exp_window/pt_male_proj.rds")

pt_female_proj = readRDS("exp_window/pt_female_proj.rds")
pt_male_proj = readRDS("exp_window/pt_male_proj.rds")

#Combine the two pt_proj lists together
pt_combined_proj = c(pt_female_proj, pt_male_proj)
names(pt_combined_proj) = 1:length(pt_combined_proj)

#Binds the 10 matrices in each of the prefecture-sex sublists into one matrix
base_forecasts_vecd = base_forecasts %>% 
    lapply(function(data){
        data_matrix = matrix(nrow = 101, ncol = 0)
        for (m in data) {
            print(nrow(m))
            data_matrix = data_matrix %>% cbind(m)
        }
        return(data_matrix)
    })

#Calculates revised forecasts for prefectures
revised_forecasts_prefec = list()

for (i in 1:PREFEC_COUNT) {
    fprod = pt_combined_proj[[i]] * base_forecasts_vecd[[i]]
    mprod = pt_combined_proj[[i + PREFEC_COUNT]] * 
        base_forecasts_vecd[[i + PREFEC_COUNT]]
    
    revised_forecast = fprod + mprod
    revised_forecasts_prefec[[i]] = revised_forecast
}

names(revised_forecasts_prefec) = names(pt_female_proj)

#Add death rate to observed prefecture mortality data
jmd_prefecture_total = lapply(jmd_prefecture_total, function(data){
    data$q = data$d_total / data$E_total
    return(data)
})

revised_forecasts_prefec = revised_forecasts_prefec %>% 
    lapply(forecastMatrixToList)

#Calculate final errors
prefec_errors_bu = fullErrorCalc(forecasted = revised_forecasts_prefec,
                                 observed = jmd_prefecture_total)


#saveRDS(revised_forecasts_prefec, "exp_window/revised_forecasts_prefec.rds")

################################################################################
##JAPAN


jmd_japan = jmd[["Japan"]]
japan_total = jmd[["Japan"]]

#Calculates summing matrix ratios for each year and age
jmd_prefecture_total2 = jmd %>% purrr::list_modify("Japan" = NULL)

jmd_prefecture_total2 = jmd_prefecture_total2 %>%
    lapply(function(prefec_data){
        prefec_data = merge(prefec_data, jmd_japan, by = c("Year", "Age"))
        prefec_data = prefec_data[prefec_data$Age <= 100,]
        prefec_data = prefec_data %>% arrange(Year, Age)

        #Prefecture exposure / Japan exposure
        prefec_data$pt_female = prefec_data$E_female.x / prefec_data$E_total.y
        prefec_data$pt_male = prefec_data$E_male.x / prefec_data$E_total.y
        prefec_data = prefec_data %>% select(Year, Age, pt_female, pt_male)
        return(prefec_data)
    })

#Get projected pt_female
pt_jpn_female_proj = jmd_prefecture_total2 %>%
    mclapply(ptFemaleGet)


#Get projected pt_male
pt_jpn_male_proj = jmd_prefecture_total2 %>%
    mclapply(ptMaleGet)

#saveRDS(pt_jpn_female_proj, "exp_window/pt_jpn_female_proj.RDS")
#saveRDS(pt_jpn_male_proj, "exp_window/pt_jpn_male_proj.RDS")

pt_jpn_female_proj = readRDS("exp_window/pt_jpn_female_proj.rds")
pt_jpn_male_proj = readRDS("exp_window/pt_jpn_male_proj.rds")

pt_jpn_proj = c(pt_jpn_female_proj, pt_jpn_male_proj)
names(pt_jpn_proj) = 1:length(pt_combined_proj)

#Calculates revised forecasts for japan total
revised_forecasts_japan = matrix(data = rep(0, 101 * 55), nrow = 101, ncol = 55)

for (i in 1:length(pt_jpn_proj)) {
    revised_forecasts_japan = revised_forecasts_japan + 
        (pt_jpn_proj[[i]] * base_forecasts_vecd[[i]])
}

revised_forecasts_japan = list(revised_forecasts_japan)

revised_forecasts_japan = revised_forecasts_japan %>% 
    lapply(forecastMatrixToList)

#japan_total = japan_total %>% filter(Year >= 1975)

japan_total = list(japan_total)

japan_total = lapply(japan_total, function(data){
    data$q = data$d_total / data$E_total
    return(data)
})

japan_total_errors_bu = fullErrorCalc(forecasted = revised_forecasts_japan,
                                 observed = japan_total)

# saveRDS(revised_forecasts_japan, "exp_window/revised_forecasts_japan.rds")




################################################################################
#### JAPAN-SEX
#Calculates summing matrix ratios for each year and age
jmd_prefecture_total3 = jmd %>% purrr::list_modify("Japan" = NULL)

jmd_prefecture_total3 = jmd_prefecture_total3 %>%
    lapply(function(prefec_data){
        prefec_data = merge(prefec_data, jmd_japan, by = c("Year", "Age"))
        prefec_data = prefec_data[prefec_data$Age <= 100,]
        prefec_data = prefec_data %>% arrange(Year, Age)
        
        #Prefecture exposure / Japan exposure
        prefec_data$pt_female = prefec_data$E_female.x / prefec_data$E_female.y
        prefec_data$pt_male = prefec_data$E_male.x / prefec_data$E_male.y
        prefec_data = prefec_data %>% select(Year, Age, pt_female, pt_male)
        return(prefec_data)
    })

#Get projected pt_female
pt_jpn_female_proj = jmd_prefecture_total3 %>%
    mclapply(ptFemaleGet)

#Get projected pt_male
pt_jpn_male_proj = jmd_prefecture_total3 %>%
    mclapply(ptMaleGet)

#Save projected pt
#saveRDS(pt_jpn_female_proj, "exp_window/pt_jpn_female_proj_sex.rds")
#saveRDS(pt_jpn_male_proj, "exp_window/pt_jpn_male_proj_sex.rds")

#Import projected pt
pt_jpn_female_proj = readRDS("exp_window/pt_jpn_female_proj_sex.rds")
pt_jpn_male_proj = readRDS("exp_window/pt_jpn_male_proj_sex.rds")

pt_jpn_sex_proj = c(pt_jpn_female_proj, pt_jpn_male_proj)
names(pt_jpn_sex_proj) = 1:length(pt_combined_proj)

#Calculates revised forecasts for japan by sex
revised_forecasts_jpn_sex = list()
revised_forecasts_jpn_sex[["Female"]] = matrix(data = rep(0, 101 * 55), 
                                                 nrow = 101, ncol = 55)
revised_forecasts_jpn_sex[["Male"]] = matrix(data = rep(0, 101 * 55), 
                                               nrow = 101, ncol = 55)

for (i in 1:PREFEC_COUNT) {
    revised_forecasts_jpn_sex[["Female"]] = revised_forecasts_jpn_sex[["Female"]] + 
        (pt_jpn_sex_proj[[as.character(i)]] * base_forecasts_vecd[[i]])
    revised_forecasts_jpn_sex[["Male"]] = revised_forecasts_jpn_sex[["Male"]] + 
        (pt_jpn_sex_proj[[as.character(i + PREFEC_COUNT)]] * 
             base_forecasts_vecd[[i + PREFEC_COUNT]])
}

revised_forecasts_jpn_sex = revised_forecasts_jpn_sex %>% 
    lapply(forecastMatrixToList)


japan_male = jmd_male[["Japan_male"]]
japan_female = jmd_female[["Japan_female"]]

#Add death rate to observed sex mortality data
japan_female$q = japan_female$d_female / japan_female$E_female 
japan_male$q = japan_male$d_male / japan_male$E_male

japan_female = japan_female %>% filter(Year >= 1975)
japan_male = japan_male %>% filter(Year >= 1975)

#Combine male and female data together
japan_sex_observed = c(list(japan_female), list(japan_male))

japan_sex_errors_bu = fullErrorCalc(revised_forecasts_jpn_sex, 
                                    japan_sex_observed)

# saveRDS(revised_forecasts_jpn_sex, "exp_window/revised_forecasts_jpn_sex.rds")
