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
    lapply(function(prefec_data){
        print(deparse(substitute(prefec_data)))
        proj_matrix = matrix(ncol = 55, nrow = 0)
        
        for (age in 0:100) {
            #print(paste("Age:", as.character(age), sep = " "))
            
            forecast_vec = c()
            
            prefec_data_2 = prefec_data[prefec_data$Age == age,]
            prefec_data_2 = prefec_data_2 %>% select(Year, pt_female)
            
            for (h in 1:10) {
                series = prefec_data_2 %>%
                    filter(Year < (2009 + h)) 
                rownames(series) = series$Year
                series = series %>% select(pt_female)
                
                forecasts = series %>% auto.arima()
                forecasts = forecasts %>% forecast(h = h)
                forecast_vec = append(forecast_vec, forecasts$mean)
                
            }
            proj_matrix = proj_matrix %>% rbind(forecast_vec)
        }
        rownames(proj_matrix) = 0:100
        return(proj_matrix)
    })

#Get projected pt_male
pt_male_proj = jmd_prefecture_total %>%
    lapply(function(prefec_data){
        print(deparse(substitute(prefec_data)))
        proj_matrix = matrix(ncol = 55, nrow = 0)
        
        for (age in 0:100) {
            #print(paste("Age:", as.character(age), sep = " "))
            
            forecast_vec = c()
            
            prefec_data_2 = prefec_data[prefec_data$Age == age,]
            prefec_data_2 = prefec_data_2 %>% select(Year, pt_male)
            
            for (h in 1:10) {
                series = prefec_data_2 %>%
                    filter(Year < (2009 + h)) 
                rownames(series) = series$Year
                series = series %>% select(pt_male)
                
                forecasts = series %>% auto.arima()
                forecasts = forecasts %>% forecast(h = h)
                forecast_vec = append(forecast_vec, forecasts$mean)
                
            }
            proj_matrix = proj_matrix %>% rbind(forecast_vec)
        }
        rownames(proj_matrix) = 0:100
        return(proj_matrix)
    })

#saveRDS(pt_female_proj, "exp_window/pt_female_proj.rds")
#saveRDS(pt_male_proj, "exp_window/pt_male_proj.rds")
