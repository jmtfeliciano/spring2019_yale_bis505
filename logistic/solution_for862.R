# Solution for 8.6.2
# By Josemari Feliciano


setwd("/Users/jfeliciano/Documents/spring2019_yale_bis505/logistic")

library(readr)
taxo_data <- read_table("8_6_2_Taxo.txt", 
                        col_names = c("dose","gcf_use","neut_cases","treat_cases"))

write.csv(taxo_data, file="formatted_taxo_data.csv")


web_link_data <- "https://raw.githubusercontent.com/neonseri/spring2019_yale_bis505/master/nonparametric/modified_clothwaste.txt"
cloth_waste_df <- read.table(file=web_link_data, header=TRUE)

https://raw.githubusercontent.com/neonseri/spring2019_yale_bis505/master/logistic/formatted_taxo_data.csv



web_link_data <- "https://raw.githubusercontent.com/neonseri/spring2019_yale_bis505/master/logistic/formatted_taxo_data.csv"
taxo_data <- read.table(file=web_link_data, header=TRUE)
