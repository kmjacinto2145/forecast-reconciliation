#Installs required packages
library(StMoMo)
library(readxl)    
library(dplyr)
library(tidyr)
library(demography)
library(tidyverse)

#function to read all sheets in a single excel workbook
read_excel_allsheets <- function(filename, tibble = FALSE) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    return(x)
}

#import data
jmd = read_excel_allsheets("cleaned_data/jmd.xlsx")

#get male datasets
jmd_male = list()

i = 1

for (table in jmd) {
    jmd_male[[i]] = table[,c("Year", "Age", "d_male", "E_male")]
    i = i + 1
}

names(jmd_male) = names(jmd)

#get female datasets
jmd_female = list()

i = 1

for (table in jmd) {
    jmd_female[[i]] = table[,c("Year", "Age", "d_female", "E_female")]
    i = i + 1
}

names(jmd_female) = names(jmd)

#function to process data and project lee-carter models
#for all datasets in list

lee_carter_series = function(series, sex, age_min, age_max) {
    
    lc_series = list() #creates list object to add models to
    
    i = 1
    for (table in series) {
        print(names(series)[i])
        
        #creates a lee-carter model object
        LC = lc()
        
        #Truncates the data
        table = table[(table$Age >= age_min) & (table$Age <= age_max),]
        
        #Shapes the deaths dataset
        Dxt = table[,c("Year", "Age", paste("d_", sex, sep = ""))]
        Dxt = spread(Dxt, Year, paste("d_", sex, sep = ""))
        rownames(Dxt) = Dxt[,"Age"]
        Dxt = subset(Dxt, select = -Age)
        
        ages = as.double(rownames(Dxt))
        
        #Shapes the exposure dataset
        Ext = table[,c("Year", "Age", paste("E_", sex, sep = ""))]
        Ext = spread(Ext, Year, paste("E_", sex, sep = ""))
        rownames(Ext) = Ext[,"Age"]
        Ext = subset(Ext, select = -Age)
        
        years = as.integer(unique(table$Year)) #all years of the table
        
        qxt = Dxt/Ext #death rate table
        #qxt[qxt > 1] = 1 #sometimes there is an error in which qxt is greater than 1. This occurs for Iwate, for instance. This code removes this error by assuming qxt = 1
        
        name = names(series[i]) #name of the dataset
        
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
        
        #add model to list
        lc_series[[i]] = LCfit
        
        i = i + 1
    }
    
    names(lc_series) = paste(names(series), sex, sep = "_") #set names
    
    return(lc_series)
}

#lee-carter models
lc_female = lee_carter_series(jmd_female, "female", 0, 100)
lc_male = lee_carter_series(jmd_male, "male", 0, 100)
lc_female_60 = lee_carter_series(jmd_female, "female", 60, 100)
lc_male_60 = lee_carter_series(jmd_male, "male", 60, 100)

#Save lc lists
saveRDS(lc_female, file = "models/lc_female.rds")
saveRDS(lc_male, file = "models/lc_male.rds")
saveRDS(lc_female_60, file = "models/lc_female_60.rds")
saveRDS(lc_male_60, file = "models/lc_male_60.rds")