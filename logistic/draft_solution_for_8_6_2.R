# DRAFT Solution for 8.6.2
# By Josemari Feliciano



# set your directory
setwd("/Users/jfeliciano/Documents/spring2019_yale_bis505/logistic")

par(mfrow=c(1,2))

library(tidyr) # install if you don't have this
tax_data <- read_table("toxoplasmosis.txt", col_names = FALSE)

left_data <- tax_data[,1:3]
right_data <- tax_data[,4:6]

names(left_data) <- c("pos", "tested", "rain")
names(right_data) <- c("pos", "tested", "rain")

toxo <- data.frame(rbind(left_data,right_data))

resp <- cbind(toxo$pos, toxo$tested - toxo$pos)
rain <- toxo$rain

logit_model <- glm(resp ~ rain, family = binomial)
logit_coeff <- summary(logit_model)$coefficients

rain_x <- 0 : 4000
ln <- exp(logit_coeff[1] + logit_coeff[2] * rain_x)

plot(rain_x, ln / (1 + ln), type = "l", xlab = "what ever is x",
     ylab = "what ever is y", ylim=c(0,1), col = "gold", cex.lab = 1.5,
     main= "blah blah blah") 

points(toxo$rain,toxo$pos/toxo$tested)

rain_2 <- rain*rain
rain_3 <- rain*rain*rain

logit_cube_model <- glm(resp ~ rain + rain_2 + rain_3, family = binomial)
logit_cube_coeff <- summary(logit_cube_model)$coefficients

rain_x <- 0 : 4000
ln_version2 <- exp(logit_cube_coeff[1] + logit_cube_coeff[2] * rain_x + logit_cube_coeff[3] * (rain_x^2)  + logit_cube_coeff[4] * (rain_x ^3) )

lines(rain_x, ln_version2 / (1 + ln_version2), col="orangered") 

legend("topleft", legend=c("linear model", "cubic model"),
       col=c("gold", "orangered"), lty=c(1,1), cex=0.6)




# SAME AS PLOT BUT NARROWER BETWEEN WHERE THE POINTS ARE
# EXTRA STUFF BUT SHORTER SYNTAX (deviates a lot from Prof's syntax)
# some tricks you don't need to know about ... did it because i am short in time
lower_x <- floor(min(toxo$rain)/100)*100
upper_x <- ceiling(max(toxo$rain)/100)*100

plot(toxo$rain,toxo$pos/toxo$tested, xlab = "what ever is x",
     ylab = "what ever is y", ylim=c(0,1),
     xlim=c(lower_x, upper_x), 
     cex.lab = 1.5, main= "blah blah blah (but closer)") 

linear_rain <- function(x) (exp(logit_coeff[1] + logit_coeff[2] * x) / (1 + exp(logit_coeff[1] + logit_coeff[2] * x)))
cubic_rain <-  function(x) ( (exp(logit_cube_coeff[1] + logit_cube_coeff[2] * x + logit_cube_coeff[3] * (x^2)  + logit_cube_coeff[4] * (x ^3) ))     / (1 + (exp(logit_cube_coeff[1] + logit_cube_coeff[2] * x + logit_cube_coeff[3] * (x^2)  + logit_cube_coeff[4] * (x ^3) ))))

plot(linear_rain, lower_x, upper_x, add=TRUE, col="gold")
plot(cubic_rain, lower_x, upper_x, add=TRUE, col="orangered")

legend("topleft", legend=c("linear model", "cubic model"),
       col=c("gold", "orangered"), lty=c(1,1), cex=0.6)



# THINGS TO CONSIDER
# - Compare p-values of models in both models
# - Look into points and how spread they are on the y-axis
# - Given the spread of the points, which one is better?
#     - Given the spread of the points, maybe both are bad?  Discuss.
# - Rain a good or bad predictor?  Argue it anyway you see fit!  



# RUN IF YOU ARE CURIOUS ABOUT UP TO DEGREE 2 Model
# DO NOT INCLUDE IN YOUR LAB REPORT 
# if you are curious what the up-to squared model looks like
logit_squared_model <- glm(resp ~ rain + rain_2, family = binomial)
logit_squared_coeff <- summary(logit_squared_model)$coefficients
squared_rain <- function(x) (exp(logit_squared_coeff[1] + logit_squared_coeff[2] * x + logit_squared_coeff[3] * x * x) / (1 + exp(logit_squared_coeff[1] + logit_squared_coeff[2] * x + logit_squared_coeff[3] * x* x)))
plot(squared_rain, lower_x, upper_x, add=TRUE, col="green")





