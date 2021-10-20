#Installs required packages
library(tidyverse)
library(ggplot2)

#Constants
years = 1:20
ages = 0:89

#Import data
ind = readRDS("england_models/forecasts_indep.rds")
bu = readRDS("england_models/forecasts_bu.rds")
oc = readRDS("england_models/forecasts_oc.rds")


get.age.year.matrices = function(data, rownum) {
    tmp_mat = matrix(ncol = 0, nrow = length(ages))
    for (year in years) {
        tmp_vec = c()
        for (age in ages) {
            tmp_vec = append(tmp_vec, data[[as.character(2018 + year)]][[as.character(age)]][rownum])
        }
        tmp_mat = cbind(tmp_vec, tmp_mat)
    }
    colnames(tmp_mat) = 2019:2038
    rownames(tmp_mat) = ages
    return(tmp_mat)
}

eng_ind = get.age.year.matrices(ind, 1)
eng_bu = get.age.year.matrices(bu, 1)
eng_oc = get.age.year.matrices(oc, 1)

eng_ind_df = as.data.frame(eng_ind)
eng_ind_df$Age = rownames(eng_ind_df)
eng_ind_df$Type = "Independent"
eng_ind_df = pivot_longer(eng_ind_df, cols = as.character(2019:2038), names_to = "Year")

eng_bu_df = as.data.frame(eng_bu)
eng_bu_df$Age = rownames(eng_bu_df)
eng_bu_df$Type = "Bottom-up"
eng_bu_df = pivot_longer(eng_bu_df, cols = as.character(2019:2038), names_to = "Year")

eng_oc_df = as.data.frame(eng_oc)
eng_oc_df$Age = rownames(eng_oc_df)
eng_oc_df$Type = "Optimal combination"
eng_oc_df = pivot_longer(eng_oc_df, cols = as.character(2019:2038), names_to = "Year")

eng_df = rbind(eng_ind_df, eng_bu_df, eng_oc_df)
eng_df = rename(eng_df, Death.rate = value)
eng_df$Age = as.numeric(eng_df$Age)
eng_df$Year = as.numeric(eng_df$Year)

get_proj_chart = function(age) {
    data = eng_df %>% filter(Age == age)
    projection_chart = ggplot(data, aes(x=Year, y=Death.rate, group = Type)) +
        geom_line(aes(color=Type)) +
        geom_point() +
        ggtitle(paste("Death rate projections at age", age))
    print(projection_chart)
}

get_proj_chart(20)

get_age_chart = function(year) {
    data = eng_df %>% filter(Year == year)
    age_chart = ggplot(data, aes(x=Age, y=log(Death.rate), group = Type)) +
        geom_line(aes(color=Type)) +
        ggtitle(paste("Log death rate projections at year", year))
    print(age_chart)
                             
}

get_age_chart(2025)
