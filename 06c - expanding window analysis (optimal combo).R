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

## CONSTANTS
PREFEC_COUNT = 47

## FUNCTIONS

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
# DATA PREPARATION
###############################################################################

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

Next steps: obtain full summing matrix and revised forecasts

## PREFECTURE-SEX
#Import revised forecasts for prefecture-sex (identical to base)
R_prefec_sex = readRDS("exp_window/indep_forecasts_prefecsex.rds")
names(R_prefec_sex) = c(names(jmd_female %>% 
                                  purrr::list_modify("Japan_female" = NULL)), 
                        names(jmd_male %>% 
                                  purrr::list_modify("Japan_male" = NULL)))

#Summing matrix for prefecture-sex
S_prefec_sex = diag(PREFEC_COUNT * 2)

left = S_prefec_sex %*% solve(t(S_prefec_sex) %*% S_prefec_sex) %*% t(S_prefec_sex)

#Convert all matrices in each prefecsex to one matrix
R_prefec_sex = mclapply(R_prefec_sex, function(data){
    m = do.call(cbind,data)
    colnames(m) = 1:55
    return(m)
})

#Import revised forecasts
R_prefecture = readRDS("exp_window/revised_forecasts_prefec.rds")

#Import components of summing matrix
pt_female_proj = readRDS("exp_window/pt_female_proj.rds")
pt_male_proj = readRDS("exp_window/pt_male_proj.rds")
pt_combined_proj = c(pt_female_proj, pt_male_proj)
names(pt_combined_proj) = c(names(jmd_female %>% 
                                      purrr::list_modify("Japan_female" = NULL)), 
                            names(jmd_male %>% 
                                      purrr::list_modify("Japan_male" = NULL))
                            )

