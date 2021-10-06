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

#Import projected pt for japan-sex
pt_jpn_sex_female = readRDS("exp_window/pt_jpn_female_proj_sex.rds")
names(pt_jpn_sex_female) = names(jmd_female %>% 
                                     purrr::list_modify("Japan_female" = NULL))
pt_jpn_sex_male = readRDS("exp_window/pt_jpn_male_proj_sex.rds")
names(pt_jpn_sex_male) = names(jmd_male %>% 
                                   purrr::list_modify("Japan_male" = NULL))

#Import projected pt for japan-total
pt_jpn_total_female = readRDS("exp_window/pt_jpn_female_proj.rds")
pt_jpn_total_male = readRDS("exp_window/pt_jpn_male_proj.rds")
pt_jpn_total = c(pt_jpn_total_female, pt_jpn_total_male)
names(pt_jpn_total) = names(pt_combined_proj)

#Convert column names into integers
pt_jpn_sex_female = mclapply(pt_jpn_sex_female, function(data){
    colnames(data) = 1:55
    return(data)
})

pt_jpn_sex_male = mclapply(pt_jpn_sex_male, function(data){
    colnames(data) = 1:55
    return(data)
})

pt_jpn_total = mclapply(pt_jpn_total, function(data){
    colnames(data) = 1:55
    return(data)
})

#Convert pt lists into list of matrices - rows: h-series, columns: prefecsex
St_jpn_total = list()

for (age in 0:100) {
    print(age)
    tmp_matrix = matrix(nrow = 0, ncol = PREFEC_COUNT * 2)
    
    for (series in 1:55) {
        tmp_vec = sapply(pt_jpn_total, function(m) {
            return(m[as.character(age),series])
        })
        tmp_matrix = rbind(tmp_matrix, tmp_vec)
    }
    St_jpn_total[[as.character(age)]] = tmp_matrix
}

#Row 2 of St: japan-female
zero_vec = rep(x = 0, times = PREFEC_COUNT)

St_jpn_female = list()

for (age in 0:100) {
    print(age)
    tmp_matrix = matrix(nrow = 0, ncol = PREFEC_COUNT * 2)
    
    for (series in 1:55) {
        tmp_vec = sapply(pt_jpn_sex_female, function(m) {
            return(m[as.character(age),series])
        })
        
        tmp_vec = c(tmp_vec, zero_vec)

        tmp_matrix = rbind(tmp_matrix, c(tmp_vec))
    }
    colnames(tmp_matrix) = names(pt_jpn_total)
    
    St_jpn_female[[as.character(age)]] = tmp_matrix
}

#Row 3 of St: Japan-male
St_jpn_male = list()

for (age in 0:100) {
    print(age)
    tmp_matrix = matrix(nrow = 0, ncol = PREFEC_COUNT * 2)
    
    for (series in 1:55) {
        tmp_vec = sapply(pt_jpn_sex_male, function(m) {
            return(m[as.character(age),series])
        })
        
        tmp_vec = c(zero_vec, tmp_vec)
        
        tmp_matrix = rbind(tmp_matrix, c(tmp_vec))
    }
    colnames(tmp_matrix) = names(pt_jpn_total)
    
    St_jpn_male[[as.character(age)]] = tmp_matrix
}

#Reorder columns
colorder = to_vec(for (i in 1:PREFEC_COUNT) c(i, i + PREFEC_COUNT))
St_jpn_total = St_jpn_total %>% mclapply(function(data) {
    return(data[,colorder])
})

St_jpn_female = St_jpn_female %>% mclapply(function(data) {
    return(data[,colorder])
})

St_jpn_male = St_jpn_male %>% mclapply(function(data) {
    return(data[,colorder])
})

#Add rows for prefecture totals
St_prefec_total = list()

for (age in 0:100) {
    print(age)
    tmp_list = list()
    
    for (series in 1:55) {
        tmp_vec = sapply(pt_combined_proj, function(m) {
            return(m[as.character(age),series])
        })
        prefec_vec = c()
        for (prefec in 1:PREFEC_COUNT) {
            prefec_vec = append(prefec_vec, c(tmp_vec[prefec], 
                                              tmp_vec[prefec + PREFEC_COUNT],
                                              rep(0, times = PREFEC_COUNT * 2)))
        }
        prefec_matrix = matrix(prefec_vec, ncol = PREFEC_COUNT * 2, byrow = T)
        prefec_matrix = prefec_matrix[1:47,]
        tmp_list[[as.character(series)]] = prefec_matrix
    }
    St_prefec_total[[as.character(age)]] = tmp_list
}

St_prefecsex =  diag(PREFEC_COUNT * 2)

#Get indices of rows with each h-value
hx = list()

h = 1
lim = 10
for (i in 1:55) {
    hx[[as.character(h)]] = append(hx[[as.character(h)]], i)
    
    if (h == lim) {
        h = 1
        lim = lim - 1
    } else {
        h = h + 1
    }
}

St_list = list()
for (series in 1:55) {
    print(series)
    St_age = list()
    for (age in 0:100) {
        St = matrix(nrow = 0, ncol = PREFEC_COUNT * 2)
        
        St_1 = St_jpn_total[[as.character(age)]][series,]
        St_2 = St_jpn_female[[as.character(age)]][series,]
        St_3 = St_jpn_male[[as.character(age)]][series,]
        St_4 = St_prefec_total[[as.character(age)]][[as.character(series)]]
        St_5 = St_prefecsex
        
        St = rbind(St, St_1, St_2, St_3, St_4, St_5)
        St_age[[as.character(age)]] = St
    }
    St_list[[as.character(series)]] = St_age
}

