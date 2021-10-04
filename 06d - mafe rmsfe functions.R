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