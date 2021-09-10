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

#function to process data and project lee-carter models
#for all datasets in list

lee_carter_series = function(series, age_min, age_max) {
    
    lc_series = list() #creates list object to add models to
    
    i = 1
    for (table in series) {
        print(names(series)[i])
        
        #creates a lee-carter model object
        LC = lc() #CHANGED TO LOG LINK
        
        #Truncates the data
        table = table[(table$Age >= age_min) & (table$Age <= age_max),]
        
        #Shapes the deaths dataset
        Dxt = table[,c("Year", "Age", "d_male", "d_female")]
        Dxt$d_total = Dxt$d_female + Dxt$d_male
        Dxt = subset(Dxt, select = -c(d_female, d_male))
        Dxt = spread(Dxt, Year, "d_total")
        rownames(Dxt) = Dxt[,"Age"]
        Dxt = subset(Dxt, select = -Age)
        
        ages = as.double(rownames(Dxt))
        
        #Shapes the exposure dataset
        Ext = table[,c("Year", "Age", "E_male", "E_female")]
        Ext$E_total = Ext$E_female + Ext$E_male
        Ext = subset(Ext, select = -c(E_female, E_male))
        Ext = spread(Ext, Year, "E_total")
        rownames(Ext) = Ext[,"Age"]
        Ext = subset(Ext, select = -Age)
        
        years = as.integer(unique(table$Year)) #all years of the table
        
        qxt = Dxt/Ext #death rate table
        #qxt[qxt > 1] = 1 #sometimes there is an error in which qxt is greater than 1. This occurs for Iwate, for instance. This code removes this error by assuming qxt = 1
        
        name = names(series[i]) #name of the dataset
        
        #create demogdata object
        table_demogdata = demogdata(qxt, Ext, ages, 
                                    years, "mortality", name, "total")
        
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
    
    names(lc_series) = paste(names(series), "total", sep = "_") #set names
    
    return(lc_series)
}

#get lee-carter models for total
lc_total = lee_carter_series(jmd, 0, 100)
lc_total_60 = lee_carter_series(jmd, 60, 100)

saveRDS(lc_total, file = "models/lc_total.rds")
saveRDS(lc_total_60, file = "models/lc_total_60.rds")