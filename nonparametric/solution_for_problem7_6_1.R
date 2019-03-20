# Sample General Solution for Nonparametric lab
# By Seri Feliciano
# Please focus on interpretation so wrote the general solution for you 
# Make sure to label any outputs and to answer questions from the book as best as you can!  




# Problem 7.6.1
#  Description: The data refer to five suppliers of the Levi-Strauss clothing manufacturing plant in Albuquerque. 
#  The firm's quality control department collects weekly data on percent-age waste (run-up) relative to what can 
#  be achieved by computer layouts of patterns on cloth. It is possible to have negative values, which indicate that 
#  the plant employees beat the computer in controlling waste. Under question are differences among the five supplier 
#  plants.Number of cases:95.  Variable Names:1.  PT1 = Run-up for Plant 12.  PT2 = Run-up for Plant 23.  PT3 = Run-up 
#  for Plant 34.  PT4 = Run-up for Plant 45.  PT5 = Run-up for Plant 5

# Loading the data (I saved the file under my github account)
web_link_data <- "https://raw.githubusercontent.com/neonseri/spring2019_yale_bis505/master/nonparametric/modified_clothwaste.txt"
cloth_waste_df <- read.table(file=web_link_data, header=TRUE)

# Inspect the data, notice the bottom part of the data!  Always inspect your data! 
cloth_waste_df

# Suspicion:  columns with * are probaby not numerical.  
# these are most likely of character type.  check manually or use sapply() with class
# don't worry about sapply() but class is applied to all columns of cloth_waste_df and prints out their types
sapply(cloth_waste_df, class)

# Most people would actually use complicated for-loops and even the dplyr package
# But we know the problematic character, "*", ahead of time, 
# I can use feed read.table with `na.strings = c("*")` parameter.  
cloth_waste_df_ver2 <- read.table(file=web_link_data, header = TRUE, na.strings = c("*"))

# verify that all columns are indeed numeric now
sapply(cloth_waste_df_ver2, class)

#### BOOK PROMPT:  "Examine a boxplot for percentage run-up by each of the five suppliers." ####
boxplot(cloth_waste_df_ver2 , use.cols = TRUE)

#### Answer you need to answer based on the output:  
####    Is there evidence all five have the same variability?  (Tied to the next question closely)

#### Next Question:
####    Is a one-way ANOVA an appropriate way to compare the five suppliers?
####       - Key assumptions that you want to check, each look normal?  
####       - Each with roughly equal variance? 
####            - Consider the outliers in considering variability!
####             - Remember from your lecture and lab the following notion:
####                  Nonparametric methods are insensitive to model assumptions and outliers but have reduced power.




#### Does a nonparametric approach reach a different conclusion?
#### Since we are dealing with more than 2 groups, best to use mood's test
#### Need to use RVAideMemoire's mood.medtest()
#### NOTE:  I will offer the chi-square method too at the very end by offering you a generalized code!

#### If you don't have the package, install it and any dependencies it may need.
#### Then load the package
if(!require("RVAideMemoire")){
  install.packages("RVAideMemoire", dependencies = TRUE)
}
# NOTE:  even when installed properly, the package will likely give you an error message
# Simply ignore this and proceed.


library("RVAideMemoire")





# MAJOR ISSUE:  data given to us in 'wide format' but exact problems given during lab is in 'long format'
# there are many ways to do this but easiest way is to use tidyr from the tidyverse family
# Feel free to skip this block of code but do note that I will create a cloth_waste_df_ver3 dataframe

# Again, WE DO NOT EXPECT YOU TO UNDERSTAND THIS. 
# Just understand that the correct format is called cloth_waste_df_ver3
if(!require("tidyr")){
  install.packages("tidyr", dependencies = TRUE)
}
library("tidyr")
cloth_waste_df_ver3 <- cloth_waste_df_ver2 %>% gather(pt_group, run_up, PT1:PT5)

# if you do not understand the distinction between long vs wide, run both
cloth_waste_df_ver2
cloth_waste_df_ver3



# Now can use RVAideMemoire's mood.medtest()

mood.medtest(run_up ~ pt_group, cloth_waste_df_ver3)

# What is the p-value (remember this uses median)?  What is the conclusion?

# These might help you to give more context and answer some of the last few questions from the book.  
# Perhaps talk about outliers?  Why median is better than mean in this context?  etc.

data.frame(crude_median=median(cloth_waste_df_ver3$run_up, na.rm = TRUE),
           pt1_median=median(cloth_waste_df_ver3[cloth_waste_df_ver3$pt=="PT1","run_up"], na.rm = TRUE),
           pt2_median=median(cloth_waste_df_ver3[cloth_waste_df_ver3$pt=="PT2","run_up"], na.rm = TRUE),
           pt3_median=median(cloth_waste_df_ver3[cloth_waste_df_ver3$pt=="PT3","run_up"], na.rm = TRUE),
           pt4_median=median(cloth_waste_df_ver3[cloth_waste_df_ver3$pt=="PT4","run_up"], na.rm = TRUE))




#### ALTERNATIVE METHOD:  Using chi-square test for independence.
#### The book manually counted the number above and below the crude median
### My code below will generalize it

# run to see how it looks like again
cloth_waste_df_ver2

# what is the crude median?  note: median needs a single vector!  
crude_median <- median(as.vector(t(cloth_waste_df_ver2)), na.rm = TRUE)
# alternative trick:  median(unlist(cloth_waste_df_ver2), na.rm = TRUE)

# create a matrix (generalized based on wide format of 'cloth_waste_df_ver2')
matrix_above_below <- matrix(NA, ncol=ncol(cloth_waste_df_ver2), nrow=2, 
                             dimnames = list(c("above median", "below median"), colnames(cloth_waste_df_ver2)) )

# inspect what this general matrix looks like 
matrix_above_below

# remember that crude_median from earlier is the crude median
for(col in 1:ncol(cloth_waste_df_ver2)){
  current_group <- cloth_waste_df_ver2[,col]
  # important to remove NAs because it can affect our calculations/logical operators
  current_group <- na.omit(current_group)
  # calculates above below median. 
  # print matrix_above_below in your console and remember that row 1 = above median, row 2 = below median
  matrix_above_below[1,col] <- sum(current_group > crude_median)
  matrix_above_below[2,col] <- sum(current_group < crude_median)
}

# see the completed product of our for loop
matrix_above_below

# perform chi-square test 
chisq.test(matrix_above_below) 







