#Decile total
get_qxt_Ext_dectot = function(data, decile, type) {
    
    if (type == "Ext") {
        subset = data %>% 
            filter(imd == decile) %>%
            select(year, age, population) %>%
            pivot_wider(id_cols = c("year", "age"), names_from = "year", values_from = "population") %>%
            as.data.frame()
    } else if (type == "qxt") {
        subset = data %>% 
            filter(imd == decile) %>%
            select(year, age, rate) %>%
            pivot_wider(id_cols = c("year", "age"), names_from = "year", values_from = "rate") %>%
            as.data.frame()
    } else {
        stop("Parameter 'type' must be either 'Ext' or 'qxt'.")
    }
    subset = subset[order(subset$age),]
    rownames(subset) = subset$age
    subset = subset %>%
        select(-age) %>%
        as.matrix()
    
    return(subset)
}

#England-sex
get_qxt_Ext_engsex = function(data, gender, type) {
    
    if (type == "Ext") {
        subset = data %>% 
            filter(sex == gender) %>%
            select(year, age, population) %>%
            pivot_wider(id_cols = c("year", "age"), names_from = "year", values_from = "population") %>%
            as.data.frame()
    } else if (type == "qxt") {
        subset = data %>% 
            filter(sex == gender) %>%
            select(year, age, rate) %>%
            pivot_wider(id_cols = c("year", "age"), names_from = "year", values_from = "rate") %>%
            as.data.frame()
    } else {
        stop("Parameter 'type' must be either 'Ext' or 'qxt'.")
    }
    subset = subset[order(subset$age),]
    rownames(subset) = subset$age
    subset = subset %>%
        select(-age) %>%
        as.matrix()
    
    return(subset)
}

#England-sex
get_qxt_Ext_engtot = function(data, type) {
    
    if (type == "Ext") {
        subset = data %>% 
            select(year, age, population) %>%
            pivot_wider(id_cols = c("year", "age"), names_from = "year", values_from = "population") %>%
            as.data.frame()
    } else if (type == "qxt") {
        subset = data %>% 
            select(year, age, rate) %>%
            pivot_wider(id_cols = c("year", "age"), names_from = "year", values_from = "rate") %>%
            as.data.frame()
    } else {
        stop("Parameter 'type' must be either 'Ext' or 'qxt'.")
    }
    subset = subset[order(subset$age),]
    rownames(subset) = subset$age
    subset = subset %>%
        select(-age) %>%
        as.matrix()
    
    return(subset)
}