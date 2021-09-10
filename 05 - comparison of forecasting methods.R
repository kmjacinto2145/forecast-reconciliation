#Installs required packages
library(StMoMo)
library(dplyr)
library(tidyr)
library(comprehenr)
library(forecast)
library(ggplot2)
library(reshape2)

#Number of forecasting periods
H = 20
ages = 0:100

#Import LC model for Japan total
lc_total = readRDS("models/lc_total.rds")

#Import adjusted forecasts of the two methods
bottom_up = readRDS("forecasts/bottom_up2.rds")
optimal_combo = readRDS("forecasts/optimal_comb.rds")

#Get just the Japan model
lc_japan_total = lc_total[[1]]

#Forecast the Japan LC model
lc_japan_forecasts = forecast(lc_japan_total, h = H)
lc_japan_forecasts = lc_japan_forecasts$rates

################################################################################

##OPTIMAL COMBINATION

#Get the Japan forecasts from the optimal combo forecasts
oc_japan_forecast_all = matrix(nrow=length(ages),ncol=H)
i = 1

for (age_forecasts in optimal_combo) {
    oc_japan_forecast_age = to_vec(for(year in age_forecasts) year[1,])
    oc_japan_forecast_all[i,] = oc_japan_forecast_age
    i = i + 1
}

rownames(oc_japan_forecast_all) = ages

#Calculate forecast error matrix for optimal combo and plot error
lc_oc_diff = lc_japan_forecasts - oc_japan_forecast_all
#heatmap(lc_oc_diff, Rowv = NA, Colv = NA)

#Calculate absolute forecast error matrix for optimal combo and plot error
lc_oc_absdiff = abs(lc_oc_diff)
heatmap(lc_oc_absdiff, Rowv = NA, Colv = NA, main = "Optimal Combination MAFE heatmap", xlab = "Year", ylab = "Age")
# lc_oc_absdiff_df = melt(lc_oc_absdiff, varnames = c("Age", "Year"), value.name = "Difference")
# 
# ggplot(lc_oc_absdiff_df, aes(Age, Year)) +
#     geom_tile(aes(fill = Difference))

#Calculate mafe for optimal combo
oc_mafe = mean(lc_oc_absdiff)

#Get squared forecast error matrix for optimal combo
lc_oc_sqdiff = lc_oc_diff^2

#Calculate rmsfe for optimal combo
oc_rmsfe = sqrt(mean(lc_oc_sqdiff))

################################################################################

##BOTTOM-UP

#Get the Japan forecasts from the optimal combo forecasts
bu_japan_forecast_all = matrix(nrow=length(ages),ncol=H)
i = 1

for (age_forecasts in bottom_up) {
    bu_japan_forecast_age = to_vec(for(year in age_forecasts) year[1,])
    bu_japan_forecast_all[i,] = bu_japan_forecast_age
    i = i + 1
}

rownames(bu_japan_forecast_all) = ages

#Calculate forecast error matrix for bottom-up and plot error
lc_bu_diff = lc_japan_forecasts - bu_japan_forecast_all
#heatmap(lc_bu_diff, Rowv = NA, Colv = NA)

#Calculate absolute forecast error matrix for bottom-up and plot error
lc_bu_absdiff = abs(lc_bu_diff)
heatmap(lc_bu_absdiff, Rowv = NA, Colv = NA, main = "Bottom-up MAFE heatmap", xlab = "Year", ylab = "Age")

#Calculate mafe for bottom-up
bu_mafe = mean(lc_bu_absdiff)

#Get squared forecast error matrix for bottom-up
lc_bu_sqdiff = lc_bu_diff^2

#Calculate rmsfe for optimal combo
bu_rmsfe = sqrt(mean(lc_bu_sqdiff))

################################################################################
# COMPARISON OF PROJECTIONS - BY AGE
################################################################################

selected_ages = c(0,10,20,65,100)

for (age in selected_ages) {
    age_matrix = matrix(data = c(lc_japan_forecasts[age + 1,], 
                                 bu_japan_forecast_all[age + 1,],
                                 oc_japan_forecast_all[age + 1,]),
                        ncol = H, nrow = 3, byrow = TRUE)
    age_df = as.data.frame(t(age_matrix))
    colnames(age_df) = c("Lee-Carter", "Bottom-up", "Optimal combination")
    age_df$Year = 1:H
    age_df = melt(age_df,id.vars = "Year", variable.name = "Model type", value.name = "Death rate")
    
    projection_chart = ggplot(age_df, aes(x=Year, y=`Death rate`, group = `Model type`)) +
        geom_line(aes(color=`Model type`)) +
        geom_point() +
        ggtitle(paste("Death rate projections at age", age))
    
    print(projection_chart)
}

################################################################################
# COMPARISON OF PROJECTIONS - BY YEAR
################################################################################
selected_years = c(1,10,20)

for (year in selected_years) {
    year_matrix = matrix(data = c(lc_japan_forecasts[,year], 
                                 bu_japan_forecast_all[,year],
                                 oc_japan_forecast_all[,year]),
                        ncol = length(ages), nrow = 3, byrow = TRUE)
    year_df = as.data.frame(t(year_matrix))
    colnames(year_df) = c("Lee-Carter", "Bottom-up", "Optimal combination")
    year_df$Age = 0:100
    year_df = melt(year_df,id.vars = "Age", variable.name = "Model type", value.name = "Death rate")
    year_df$`Log death rate` = sapply(year_df$`Death rate`, FUN = log)
    
    projection_chart = ggplot(year_df, aes(x=Age, y=`Log death rate`, group = `Model type`)) +
        geom_line(aes(color=`Model type`)) +
        ggtitle(paste("Log death rate projections at year", year))
    
    print(projection_chart)
}

plot(lc_total$Japan_total$bx)
