#Import required packages
library(rvest)
library(stringr)
library(tidyr)

NUMBER_OF_REGIONS = 48

#Imports table of deaths
death_tables = vector(mode = "list", length = NUMBER_OF_REGIONS)

for (i in 1:NUMBER_OF_REGIONS - 1) {
    print(i)
    
    #Import data
    url = paste("http://www.ipss.go.jp/p-toukei/JMD/", 
                sprintf("%02d", i), "/STATS/Deaths_1x1.txt", sep = "")
    region_deaths = read_html(url)
    
    #Get text
    region_deaths_text = region_deaths %>% html_nodes("p") %>% html_text()
    
    #Separate values by "\r\n". Results in a string shaped like a table
    region_deaths_text = region_deaths_text %>% str_split("\r\n", simplify = T)
    
    #Transpose
    region_deaths_text = t(region_deaths_text)
    
    #Isolates just the data from the resulting string
    region_deaths_text = region_deaths_text[3:length(region_deaths_text)]
    
    #Converts the string into a table data type
    region_deaths = read.table(textConnection(region_deaths_text), 
                               header = TRUE)
    
    #Converts ages to integers
    region_deaths$Age = as.integer(region_deaths$Age)
    
    #Adds the table to death tables
    death_tables[[i + 1]] = region_deaths
}

#Imports table of exposed to risk
exposure_tables = vector(mode = "list", length = NUMBER_OF_REGIONS)

for (i in 1:NUMBER_OF_REGIONS - 1) {
    print(i)
    url = paste("http://www.ipss.go.jp/p-toukei/JMD/", 
                sprintf("%02d", i), "/STATS/Exposures_1x1.txt", sep = "")
    region_exposures = read_html(url)
    
    region_exposures_text = region_exposures %>% html_nodes("p") %>% html_text()
    region_exposures_text = region_exposures_text %>% str_split("\r\n", 
                                                                simplify = T)
    region_exposures_text = t(region_exposures_text)
    region_exposures_text = region_exposures_text[
        3:length(region_exposures_text)
    ]
    
    region_exposures = read.table(textConnection(region_exposures_text), 
                                  header = TRUE)
    region_exposures$Age = as.integer(region_exposures$Age)
    
    exposure_tables[[i + 1]] = region_exposures
}

#combines the deaths and exposures tables together for each prefecture
combined_tables = vector(mode = "list", length = NUMBER_OF_REGIONS)

for (i in 1:NUMBER_OF_REGIONS) {
    print(i)
    
    #merges tables
    region_combined = merge(death_tables[i], exposure_tables[i], 
                            by = c("Year", "Age"))
    
    #change names
    colnames(region_combined) = c("Year", "Age", "d_female", 
                                  "d_male", "d_total", "E_female", 
                                  "E_male", "E_total")
    
    #Change "110+" (which is given as NA in the table) to "110"
    region_combined$Age = replace_na(region_combined$Age, 110)
    
    #Sort database by year and age
    region_combined = region_combined[with(region_combined, order(Year, Age)),]
    
    #add to combined table
    combined_tables[[i]] = region_combined
}

#Get region names
region_names = vector(length = NUMBER_OF_REGIONS)

region_names[[1]] = "Japan"

for (i in 2:NUMBER_OF_REGIONS - 1) {
    print(i)
    url = paste("http://www.ipss.go.jp/p-toukei/JMD/", 
                sprintf("%02d", i), "/STATS/Deaths_1x1.txt", sep = "")
    region = read_html(url)
    
    #Get text
    region = region %>% html_nodes("p") %>% html_text()
    
    #Splits text on commas
    region = region %>% str_split(",", simplify = T)
    
    #Gets text with the prefecture name
    region = region[,1]
    
    #Gets just the prefecture name
    region = region %>% strsplit("\\.")
    region = region[[1]][2]
    
    #Adds prefecture name to list
    region_names[[i + 1]] = region
}

#Set the regions as the table names
names(combined_tables) = region_names

#Export to Excel spreadsheet
library(openxlsx)
write.xlsx(combined_tables, file = "cleaned_data/jmd.xlsx")
