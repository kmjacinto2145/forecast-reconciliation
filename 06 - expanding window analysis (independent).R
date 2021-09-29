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

###############################################################################
# FUNCTIONS
###############################################################################

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

###############################################################################
# DATA PREP
###############################################################################


#import data
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
# MODELLING
###############################################################################


#get lee-carter models for total
indep_forecasts_total = expandingWindowLC(japan_total, "total")

#get lee-carter models for sex
japan_male = jmd_male[["Japan_male"]]
japan_female = jmd_female[["Japan_female"]]

indep_forecasts_male = expandingWindowLC(japan_male, "male")
indep_forecasts_female = expandingWindowLC(japan_female, "female")

#get lee-carter models for prefecture
jmd_prefecture_total = jmd %>% purrr::list_modify("Japan" = NULL)

indep_forecasts_prefectures = list()
i = 1
for (prefecture in jmd_prefecture_total) {
    current = expandingWindowLC(prefecture, "total")
    indep_forecasts_prefectures[[i]] = current
    i = i + 1
}
names(indep_forecasts_prefectures) = names(jmd_prefecture_total)

#get lee-carter models for prefecture-sex
jmd_prefecfemale = jmd_female %>% 
    purrr::list_modify("Japan_female" = NULL)
jmd_prefecmale = jmd_male %>% 
    purrr::list_modify("Japan_male" = NULL)

indep_forecasts_prefecsex = list()
i = 1
for (prefecture in jmd_prefecfemale) {
    current = expandingWindowLC(prefecture, "female")
    indep_forecasts_prefecsex[[i]] = current
    i = i + 1
}
for (prefecture in jmd_prefecmale) {
    current = expandingWindowLC(prefecture, "male")
    indep_forecasts_prefecsex[[i]] = current
    i = i + 1
}


# saveRDS(indep_forecasts_prefecsex, "exp_window/indep_forecasts_prefecsex.rds")
# saveRDS(indep_forecasts_total, "exp_window/indep_forecasts_total.rds")
# saveRDS(indep_forecasts_male, "exp_window/indep_forecasts_male.rds")
# saveRDS(indep_forecasts_female, "exp_window/indep_forecasts_female.rds")
# saveRDS(indep_forecasts_prefectures, "exp_window/indep_forecasts_prefectures.rds")




################################################################################
# ERROR CALCULATION
################################################################################

#Functions
indexedToCartesian = function(data) {
    #Convert indexed dataframe into cartesian matrix,
    #with ages as the rows and years as the columns
    data = data %>% 
        select(c(Year, Age, q)) %>%
        filter(Year >= 2010 & Year < 2020) %>%
        filter(Age <= 100) %>%
        pivot_wider(names_from = Year, values_from = q) %>%
        as.data.frame()
    rownames(data) = data$Age
    data = data %>% select(-Age)
    return(data)
}

cbindObserved = function(data) {
    data = do.call(cbind, data)
    data = list(data)
    names(data) = "Observed"
    return(data)
}

#Calculates MAFE
calcMafe = function(data){
    for (i in 0:9) {
        #print(i)
        data[[as.character(i)]] = abs(data[[as.character(i)]] - data$Observed)
        
        if (i != 9){
            data$Observed = data$Observed[,-1]    
        }
    }
    
    return(data[names(data) != "Observed"])
}

#Gets a final, combined MAFE value for each of the ten years

combineMafe = function(data){
    mafe_vec = c()
    for (h in 1:10) {
        #print(h)
        a = c() #Vector of all errors that have a h-value of h.
        for (i in 0:(10 - h)){
            #print(i)
            if (i == 0){
                a = append(a, data[[as.character(i)]][,h])
            } else if (i == 9) {
                a = append(a, data[[i]])
            } else {
                a = append(a, data[[i]][,h])
            }
        }
        mafe_vec = append(mafe_vec, mean(a))
    }
    return(mafe_vec)
}

#Calculates RMSFE

calcRmsfe = function(data){
    for (i in 0:9) {
        #print(i)
        data[[as.character(i)]] = (data[[as.character(i)]] - data$Observed)^2
        
        if (i != 9){
            data$Observed = data$Observed[,-1]    
        }
    }
    
    return(data[names(data) != "Observed"])
}

#Gets a final, combined RMSFE value for each of the ten years

combineRmsfe = function(data){
    rmsfe_vec = c()
    for (h in 1:10) {
        #print(h)
        a = c()
        for (i in 0:(10 - h)){
            #print(i)
            if (i == 0){
                a = append(a, data[[as.character(i)]][,h])
            } else if (i == 9) {
                a = append(a, data[[i]])
            } else {
                a = append(a, data[[i]][,h])
            }
        }
        rmsfe_vec = append(rmsfe_vec, sqrt(mean(a)))
    }
    return(rmsfe_vec)
}

#Combines all the above functions into one
fullErrorCalc = function(forecasted, observed) {
    observed = observed %>%
        lapply(indexedToCartesian) %>%
        lapply(cbindObserved)
    
    #Combines the observed and forecasted data together
    forecasted_observed = Map(c, forecasted, observed)
    
    mafe = forecasted_observed %>%
        lapply(calcMafe) %>%
        lapply(combineMafe)
    
    mafe = Reduce(`+`, mafe)
    mafe = mafe / length(forecasted_observed)
    
    rmsfe = forecasted_observed %>%
        lapply(calcRmsfe) %>%
        lapply(combineRmsfe)
    
    rmsfe = Reduce(`+`, rmsfe)
    rmsfe = rmsfe / length(forecasted_observed)
    
    return(list("MAFE" = mafe, 
                "RMSFE" = rmsfe))
}

#################### PREFECTURE-SEX

# START HERE
indep_forecasts_prefecsex = readRDS("exp_window/indep_forecasts_prefecsex.rds")
names(indep_forecasts_prefecsex) = names(c(jmd_prefecfemale, jmd_prefecmale))

jmd_prefecfemale = jmd_female %>% 
    purrr::list_modify("Japan_female" = NULL)
jmd_prefecmale = jmd_male %>% 
    purrr::list_modify("Japan_male" = NULL)

#Add death rate to observed prefecture-sex mortality data
jmd_prefecfemale = lapply(jmd_prefecfemale, function(data){
    data$q = data$d_female / data$E_female 
    return(data)
})

jmd_prefecmale = lapply(jmd_prefecmale, function(data){
    data$q = data$d_male / data$E_male 
    return(data)
})

#Combine male and female data together
jmd_prefecsex_observed = c(jmd_prefecfemale, jmd_prefecmale)

prefecsex_errors = fullErrorCalc(indep_forecasts_prefecsex, 
                                 jmd_prefecsex_observed)

#################### PREFECTURE
indep_forecasts_prefectures = readRDS("exp_window/
                                      indep_forecasts_prefectures.rds")
names(indep_forecasts_prefectures) = names(jmd_prefecture_total)

#Add death rate to observed prefecture mortality data
jmd_prefecture_total = lapply(jmd_prefecture_total, function(data){
    data$q = data$d_total / data$E_total
    return(data)
})

prefec_errors = fullErrorCalc(indep_forecasts_prefectures, 
                              jmd_prefecture_total)

#################### JAPAN-SEX
indep_forecasts_male = readRDS("exp_window/indep_forecasts_male.rds")
indep_forecasts_female = readRDS("exp_window/indep_forecasts_female.rds")

japan_male = jmd_male[["Japan_male"]]
japan_female = jmd_female[["Japan_female"]]

#Add death rate to observed sex mortality data
japan_female$q = japan_female$d_female / japan_female$E_female 
japan_male$q = japan_male$d_male / japan_male$E_male 

#Combine male and female data together
jmd_sex_observed = c(list(japan_female), list(japan_male))
indep_forecasts_sex = c(list(indep_forecasts_female), 
                        list(indep_forecasts_male))

japan_sex_errors = fullErrorCalc(indep_forecasts_sex, 
                              jmd_sex_observed)

#################### JAPAN
indep_forecasts_total = readRDS("exp_window/indep_forecasts_total.rds")

japan_total$q = japan_total$d_total / japan_total$E_total

japan_total = c(list(japan_total))
indep_forecasts_total = c(list(indep_forecasts_total))

japan_errors = fullErrorCalc(indep_forecasts_total, japan_total)
