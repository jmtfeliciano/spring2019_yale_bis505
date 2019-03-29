# Solution for 8.6.2
# By Josemari Feliciano


setwd("/Users/jfeliciano/Documents/spring2019_yale_bis505/logistic")

library(readr)
taxo_data <- read_table("8_6_2_Taxo.txt", 
                        col_names = c("dose","gcf_use","neut_cases","treat_cases"))

