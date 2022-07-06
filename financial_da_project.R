#' ---
#' title: 'DSA 302 Project'
#' subtitle: Part 1
#' output:
#'   word_document: default
#'   html_notebook: default
#' ---
#' Project prepared by: Aditya Chand, Lavanya Jindal, Liew Wee Don, Muhammad Naufal, Lee Wai Ying Chramaine
#' 
#' # Part 1
#' 
#' 
#' ## Question 1 
#' 
#' Plot the bond prices versus their maturities
#' 
## ----settings, echo=FALSE----------------------------------------------------------------------------------------
# set the global markdown settings
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
basedir ="D:/User/Documents/SMU CONTENT/Year 4 Sem 1/DSA302/DSA302 CWP/project" 


#' 
#' 
## ----------------------------------------------------------------------------------------------------------------
library(xts)
library(MASS)
library(dplyr)
library(tidyr)
options(digits=4)

#' 
#' Read in data `ZCBP.txt`
#' 
## ----------------------------------------------------------------------------------------------------------------
ZCBP = read.table(paste0(basedir,"/ZCBP.txt"), header = T)
ZCBP = ZCBP[order(ZCBP$time),] %>% `row.names<-`(NULL)
head(ZCBP)

#' 
#' 
#' 
#' 
## ----------------------------------------------------------------------------------------------------------------

plot(ZCBP$time, ZCBP$price,main="Bond prices against maturities", xlab="Time to maturity", ylab="Price")


#' 
#' ----
#' 
#' ## Question 2 
#' 
#' Plot the empirical forward rates as computed in equation (3) versus maturities. 
#' 
#' 
## ----------------------------------------------------------------------------------------------------------------

forward_rates = -(diff(log(ZCBP$price)))/diff(ZCBP$time)
f_0 = -log(0.01*ZCBP$price[1])/ZCBP$time[1] # find forward rate at time 0, init
# cbind(0,f_0)

# cbind(ZCBP[1:(nrow(ZCBP)-1),],forward_rates)
ZCBP_1 = rbind(cbind(0,f_0),cbind(ZCBP[1:(nrow(ZCBP)-1),1],forward_rates)) %>%
  `colnames<-`(c('time','forward_rates')) %>% data.frame()
# skip first row for rate
# ZCBP_1 = cbind(ZCBP[2:nrow(ZCBP),],forward_rates) # skip first row for rate
head(ZCBP_1)
View(ZCBP_1)

#' 
#' 
## ----plot--------------------------------------------------------------------------------------------------------

plot(ZCBP_1$time, ZCBP_1$forward_rates,main="Empirical forward rates against maturities",xlab="Time to maturity", ylab="Forward rates")
# plot(ZCBP_1$time, forward_rates,main="Empirical forward rates against maturities",xlab="Time to maturity", ylab="Price")


#' 
#' ----
#' 
#' ## Question 3
#' 
#' Smooth the empirical forward rates using second order and third order polynomials. You may apply `optim()` function to minimize the sum of squares of errors of the fitted and empirical forward rates. Superimpose the smoothed curves versus the empirical forward rates. 
#' 
#' 
#' Fit first model: $$ \hat{forward\_rate} = \hat\beta_0 + \hat\beta_1time + \hat\beta_2time^2 + \hat\beta_3time^3 $$
#' 
#' 
## ----------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,1))

# fit poly of order 1,2,3

fit1=lm(forward_rates ~ time +  I(time^2) + I(time^3),data = ZCBP_1)

summary(fit1)
coefs = matrix(nrow = 3)
coefs_1 =fit1$coefficients # extract the coef estimates

# plot
plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Forward Rates")

chip_1 <- function(x) coefs_1[1] + x*coefs_1[2] + (x^2)*coefs_1[3] + (x^3)*coefs_1[4] # state model specs

curve(chip_1, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), add=T, col='red')


legend(0,0.04,c("Smoothed by polynomial degree(2,3)"),lty=c(1),
       box.lty=1,lwd=2,col=c("red"),cex=0.7)


#' 
#' 
#' Fit second model: $$ \hat{forward\_rate} = \hat\beta_0 + \hat\beta_1time + \hat\beta_2time^2 $$
#' 
## ----------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,1))

# fit poly of order 1,2

fit2=lm(forward_rates ~ time +  I(time^2),data = ZCBP_1)

summary(fit2)
coefs_2 =fit2$coefficients # extract the coef estimates

# plot
plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Forward Rates")

chip_2 <- function(x) coefs_2[1] + x*coefs_2[2] + (x^2)*coefs_2[3]  # state model specs

curve(chip_2, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), add=T, col='red')

legend(0,0.04,c("Smoothed by polynomial degree(2)"),lty=c(1),
       box.lty=1,lwd=2,col=c("red"),cex=0.7)



#' 
#' 
#' 
#' 
#' Fit third model: $$ \hat{forward\_rate} = \hat\beta_0 + \hat\beta_1time + \hat\beta_3time^3 $$
#' 
#' 
## ----------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,1))

# fit poly of order 1,2

fit3=lm(forward_rates ~ time +  I(time^3),data = ZCBP_1)

summary(fit3)
coefs_3 =fit3$coefficients # extract the coef estimates

# plot
plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Forward Rates")

chip_3 <- function(x) coefs_3[1] + x*coefs_3[2] + (x^3)*coefs_3[3]  # state model specs

curve(chip_3, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), add=T, col='red')

legend(0,0.04,c("Smoothed by polynomial degree(3)"),lty=c(1),
       box.lty=1,lwd=2,col=c("red"),cex=0.7)


#' 
#' 
#' Superimpose all three plots
#' 
## ----------------------------------------------------------------------------------------------------------------
# superimpose all three curves
plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Forward Rates")

chip_1 <- function(x) coefs_1[1] + x*coefs_1[2] + (x^2)*coefs_1[3] + (x^3)*coefs_1[4] # state model specs

curve(chip_1, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), add=T, col='red', lwd = 2)

chip_2 <- function(x) coefs_2[1] + x*coefs_2[2] + (x^2)*coefs_2[3]  # state model specs

curve(chip_2, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), add=T, col='blue', lwd=2)

chip_3 <- function(x) coefs_3[1] + x*coefs_3[2] + (x^3)*coefs_3[3]  # state model specs

curve(chip_3, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), add=T, col='green', lwd=2)

legend(0,0.04,c("Smoothed by degree(2,3)","Smoothed by degree(2)","Smoothed by degree(3)"),lty=c(1),
       box.lty=1,lwd=1,col=c("red",'blue','green'),cex=0.7)



#' 
#' 
#' 
#' 
#' Explanation for `lm()`
#' 
#' 
#' Consider the polynomial:
#' 
#' $$ \beta_0 + \beta_1x + \beta_2x^2 + ... + \beta_kx^k$$
#' 
#' Observe that the polynomial is non linear in x but that it is linear in beta. if we're trying to estimate beta, this is linear regression. Linearity in Beta is what matters. When estimating the above equation by least squares, all of the results of linear regression will hold. Hence we can use R2 as the model diagnostic as the equation of SST = SSE + SSR holds.
#' 
#' ----
#' 
#' 
#' ## Question 4 
#' 
#' Estimate the empirical spot rates for t in $(t_1, t_n)$ using equation (4). You may apply the function `cut()` (or other methods of your choice to determine which interval t falls in) and `cumsum()` to evaluate equation (4).
#' 
#' 
#' 
#' *for formula (4), part 1 of formula changes but part 2 does not change *
#' 
#' To find the spot rates, we will find the cum sum at each interval of t up till t = 29 where t is denoted as the year. Hence we will have a total of 29 intervals and 29 spot rates. 
#' 
#' The spot rate at time t is given as the sum of cumulative product of forward rates * (time interval between forward rate and the next forward rate) and the final forward rate * (time interval between forward rate and t)
#' 
#' 
## ----------------------------------------------------------------------------------------------------------------

year_intervals = cut(ZCBP_1$time,seq(0,30), right = FALSE) # find out which forward rates belong to which interval. 
ZCBP_2 = cbind(ZCBP_1$time,ZCBP_1$forward_rates,year_intervals) %>% `colnames<-`(c('time','forward_rates','year')) %>% data.frame() # create a new dataframe with the year indicated

spot_rates = c(1:max(ZCBP_2$year)) # create a new vector to store the spot rates

all_cum_sum = cumsum(ZCBP_2$forward_rates*diff(c(ZCBP_2$time,29)))
# create new df with cum sum values
ZCBP_3 = cbind(ZCBP_2,all_cum_sum)%>% `colnames<-`(c('time','forward_rates','year','cumsum'))

# ZCBP_3$cumsum[3] + ZCBP_3$forward_rates[4]*(1-ZCBP_3$time[4])

for (i in 1:max(spot_rates)) {

  ind= sum(ZCBP_3$year<=i) # final spot rate before t
  
  spot_rates[i] = (ZCBP_3$cumsum[ind-1] + ZCBP_3$forward_rates[ind] * (i - ZCBP_3$time[ind]))/i

}

spot_rates_df = cbind(spot_rates,seq(1:29)) %>% `colnames<-`(c('spot_rates','time')) %>% data.frame()

View(spot_rates_df[1:15,])
View(spot_rates_df[16:dim(spot_rates_df)[1],])


#' 
#' We find 29 spot rates for t 1 < t < 30
#' 
#' ---
#' 
#' ## Question 5
#' 
#' Smooth the empirical spot rates using second order and third order polynomials. You may apply the `optim()` function to minimize the sum of squares of errors of the fitted and empirical spot rates. Superimpose the smoothed curves versus the empirical spot rates.
#' 
#' Fit first model: $$ \hat{spot\_rate} = \hat\beta_0 + \hat\beta_1time + \hat\beta_2time^2 + \hat\beta_3time^3 $$
#' 
## ----------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,1))


fit5=lm(spot_rates ~ time +  I(time^2) + I(time^3),data = spot_rates_df)

summary(fit5)

coefs_5 =fit5$coefficients # extract the coef estimates

# plot
plot(spot_rates_df$time, spot_rates_df$spot_rates, main = "Empirical spot rates against maturities", xlab="Time to maturity", ylab="Spot Rates",)


chip_5 <- function(x) coefs_5[1] + x*coefs_5[2] + (x^2)*coefs_5[3] + (x^3)*coefs_5[4]  # state model specs

curve(chip_5, min(spot_rates_df$time),max(spot_rates_df$time),n=length(spot_rates_df$time), add=T, col='red')

legend("bottomright",c("Smoothed by polynomial degree(2,3)"),lty=c(1),
       box.lty=1,lwd=2,col=c("red"),cex=0.7)



#' 
#' 
#' Fit second model: $$ \hat{spot\_rate} = \hat\beta_0 + \hat\beta_1time + \hat\beta_2time^2 $$
#' 
#' 
## ----------------------------------------------------------------------------------------------------------------

fit6=lm(spot_rates ~ time +  I(time^2) ,data = spot_rates_df)

summary(fit6)
coefs_6 =fit6$coefficients # extract the coef estimates

# plot
plot(spot_rates_df$time, spot_rates_df$spot_rates, main = "Empirical spot rates against maturities", xlab="Time to maturity", ylab="Spot Rates",)

chip_6 <- function(x) coefs_6[1] + x*coefs_6[2] + (x^2)*coefs_6[3]  # state model specs

curve(chip_6, min(spot_rates_df$time),max(spot_rates_df$time),n=length(spot_rates_df$time), add=T, col='red')

legend("bottomright",c("Smoothed by polynomial degree(2)"),lty=c(1),
       box.lty=1,lwd=2,col=c("red"),cex=0.7)



#' 
#' 
#' 
#' 
#' 
#' Fit third model: $$ \hat{spot\_rate} = \hat\beta_0 + \hat\beta_1time + \hat\beta_2time^3 $$
#' 
#' 
## ----------------------------------------------------------------------------------------------------------------

fit7=lm(spot_rates ~ time +  I(time^3) ,data = spot_rates_df)

summary(fit7)
coefs_7 =fit7$coefficients # extract the coef estimates

# plot
plot(spot_rates_df$time, spot_rates_df$spot_rates, main = "Empirical spot rates against maturities", xlab="Time to maturity", ylab="Spot Rates",)

chip_7 <- function(x) coefs_7[1] + x*coefs_7[2] + (x^3)*coefs_7[3]  # state model specs

curve(chip_7, min(spot_rates_df$time),max(spot_rates_df$time),n=length(spot_rates_df$time), add=T, col='red')

legend("bottomright",c("Smoothed by polynomial degree(3)"),lty=c(1),
       box.lty=1,lwd=2,col=c("red"),cex=0.7)



#' 
#' Superimpose all three plots
#' 
## ----------------------------------------------------------------------------------------------------------------
# superimpose all three curves
plot(spot_rates_df$time, spot_rates_df$spot_rates, main = "Empirical spot rates against maturities", xlab="Time to maturity", ylab="Spot Rates",)


chip_5 <- function(x) coefs_5[1] + x*coefs_5[2] + (x^2)*coefs_5[3] + (x^3)*coefs_5[4]  # state model specs

curve(chip_5, min(spot_rates_df$time),max(spot_rates_df$time),n=length(spot_rates_df$time), add=T, col='red', lwd=1)


chip_6 <- function(x) coefs_6[1] + x*coefs_6[2] + (x^2)*coefs_6[3]  # state model specs

curve(chip_6, min(spot_rates_df$time),max(spot_rates_df$time),n=length(spot_rates_df$time), add=T, col='blue', lwd=1)


chip_7 <- function(x) coefs_7[1] + x*coefs_7[2] + (x^3)*coefs_7[3]  # state model specs

curve(chip_7, min(spot_rates_df$time),max(spot_rates_df$time),n=length(spot_rates_df$time), add=T, col='green', lwd=1)

legend('bottomright',c("Smoothed by degree(2,3)","Smoothed by degree(2)","Smoothed by degree(3)"),lty=c(1),
       box.lty=1,lwd=1,col=c("red",'blue','green'),cex=0.7)



#' 
#' 
#' 
#' ## Question 6
#' 
#' Comment on your results 
#' 
## ----------------------------------------------------------------------------------------------------------------


curve(chip_1, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), col='red', ylim=c(0,0.07), ylab = "rates", xlab = "time to maturity", main="Comparison of forward rates, spot rates over time")
curve(chip_5, min(spot_rates_df$time),max(spot_rates_df$time),n=length(spot_rates_df$time), add=T, col='blue')

legend("bottomright",c("Smoothed forward rate", "Smoothed spot rate"),lty=c(1),
       box.lty=1,lwd=2,col=c("red", "blue"),cex=0.7)



#' 
#' 
#' Bond price always move in the opposite direction of interest rates. 
#' 
#' The ytm of a zero coupon bond of maturity n years is called the n-year sopt rate and is denoted by y_n. One uses the n-year spot rate to discount a payment n years from now. 
#' 
#' Often short term rates are lower than long term rates. This makes sense since long term bonds are riskier, because long term bond prices fluctuate more with interest rate changes. 
#' 
#' We use the forward rates to find the price of a bond. The forward rates is an aggreement to buy or sell an asset at some fixed future date at a fixed price.  
#' 
#' When spot rates turn out to be lower (higher) than implied by forward curve, the forward price will increase (decrease). A trader expecting lower future spot rates (than implied by the current forward rates) would purchase the forward contract to profit from its appreciation.
#' 
#' 
#' <!-- Check if equation (2) holds -->
#' 
#' <!-- ```{r} -->
#' 
#' <!-- predicted_spot_rates = predict(fit5) -->
#' <!-- # predicted_spot_rates -->
#' 
#' <!-- predicted_forward_rates = predict(fit1) -->
#' <!-- # predicted_forward_rates -->
#' <!-- empirical_spot_rate = seq(1:29) -->
#' <!-- for (i in 1:29) { -->
#' <!--   ind = ZCBP$time <= i -->
#' <!--   empirical_spot_rate[i] = sum(predicted_forward_rates[ind])/i -->
#' <!-- } -->
#' 
#' <!-- ?nls -->
#' <!-- # plot(seq(1:29), empirical_spot_rate, ylim=c(0,0.07)) -->
#' <!-- plot(seq(1:29), empirical_spot_rate) -->
#' <!-- curve(chip_1, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), col='red', ylab = "rates", xlab = "time", main="Comparison of forward rates, spot rates over time", add=T) -->
#' <!-- curve(chip_5, min(spot_rates_df$time),max(spot_rates_df$time),n=length(spot_rates_df$time), add=T, col='blue') -->
#' 
#' <!-- legend("bottomright",c("Smoothed forward rate", "Smoothed spot rate"),lty=c(1), -->
#' <!--        box.lty=1,lwd=2,col=c("red", "blue"),cex=0.7) -->
#' 
#' <!-- # empirical_spot_rate -->
#' <!-- ``` -->
#' 
#' 
#' 
#' <!-- ## Appendix -->
#' 
#' 
#' <!-- We first find all the cumulative sums for each forward rate that we have. -->
#' <!-- ```{r echo=FALSE, results=FALSE} -->
#' <!-- # ZCBP_1[1,] -->
#' <!-- # ZCBP_1$time -->
#' <!-- # diff(ZCBP_1$time[1:nrow(ZCBP_1)]) -->
#' <!-- # cumsum(ZCBP_1$forward_rates[2:nrow(ZCBP_1)]*diff(ZCBP_1$time)) -->
#' 
#' <!-- ZCBP_1$time -->
#' <!-- all_cum_sum = ZCBP_1$forward_rates[1]*ZCBP_1$time[2] +  -->
#' <!-- cumsum(ZCBP_1$forward_rates[2:nrow(ZCBP_1)]*diff(c(ZCBP_1$time[2:nrow(ZCBP_1)],29))) -->
#' <!-- all_cum_sum -->
#' <!-- all_cum_sum[length(all_cum_sum)]/29 -->
#' <!-- all_cum_sum -->
#' <!-- ``` -->
#' 
#' 
#' <!-- ```{r echo=FALSE, results=FALSE} -->
#' 
#' <!-- year_intervals = cut(ZCBP_1$time[2:nrow(ZCBP_1)],seq(0,30), right = FALSE) -->
#' <!-- ZCBP_2 = cbind(all_cum_sum,year_intervals) %>% `colnames<-`(c('cum_sum','year')) %>% data.frame() -->
#' <!-- ZCBP_2 -->
#' 
#' <!-- # ind=ZCBP_2$year<2 -->
#' <!-- #  -->
#' <!-- # sum(ZCBP_2[ind,1]) -->
#' <!-- # sum(ZCBP_2[ZCBP_2$year<2]) -->
#' 
#' 
#' <!-- results = c(1:max(ZCBP_2$year)) -->
#' <!-- results -->
#' <!-- for (i in 1:max(results)) { -->
#' 
#' <!--   ind=ZCBP_2$year<=i -->
#' <!--   print(i) -->
#' <!--   results[i] = sum(ZCBP_2[ind,1])/i -->
#' <!-- } -->
#' 
#' <!-- results -->
#' 
#' <!-- # length(all_cum_sum)==length(cut(ZCBP_1$time[2:nrow(ZCBP_1)],seq(0,30), right = FALSE)) -->
#' 
#' <!-- # cbind(ZCBP_1$time,cut(tempdf$time,seq(0,30), right = FALSE)) -->
#' <!-- ``` -->
#' 
#' 
#' <!-- ```{r echo=FALSE, results=FALSE} -->
#' <!-- tempdf$time -->
#' <!-- # tempdf$time %>% cut(seq(0,30), right = FALSE) -->
#' <!-- tempdf2 = cbind(tempdf$time,cut(tempdf$time,seq(0,30), right = FALSE)) -->
#' 
#' <!-- # integrate the forward rates -->
#' <!-- ZCBP_1$price -->
#' <!-- forward.init=-log(ZCBP_1$price[1]/100) -->
#' <!-- (forward.sums= c(forward.init, forward.init + cumsum(ZCBP_1$forward_rates * diff()))) -->
#' 
#' 
#' <!-- ``` -->
#' 
#' 
#' 
#' 
#' 
#' <!-- ## Appendix -->
#' 
#' <!-- ### Question 3 -->
#' <!-- Find out why raw=True gives different coefficients estimates (link). Or instead just fit using I() instead of poly() -->
#' 
#' <!-- ```{r} -->
#' <!-- par(mfrow=c(1,1)) -->
#' <!-- fit2=lm(forward_rates~poly(time, degree = 3), data = ZCBP_1) -->
#' <!-- tempdf= data.frame(cbind(ZCBP_1$time, fit2$fitted.values)) %>%  -->
#' <!--   `colnames<-`(c('time','fitted_values')) -->
#' <!-- tempdf=tempdf[order(tempdf$time),] # create  -->
#' <!-- plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Price") -->
#' <!-- points(tempdf$time, tempdf$fitted_values, col='red', type='l') -->
#' 
#' <!-- legend(0,0.035,c("Smoothed by degree(3)"),lty=c(1), -->
#' <!--        box.lty=1,lwd=2,col=c("red"),cex=0.8) -->
#' 
#' <!-- fit2$coefficients -->
#' <!-- ``` -->
#' 
#' 
#' <!-- ```{r} -->
#' <!-- par(mfrow=c(1,1)) -->
#' <!-- fit2=lm(forward_rates~poly(time, degree = 3), data = ZCBP_1) -->
#' <!-- tempdf= data.frame(cbind(ZCBP_1$time, fit2$fitted.values)) %>%  -->
#' <!--   `colnames<-`(c('time','fitted_values')) -->
#' <!-- tempdf=tempdf[order(tempdf$time),] # create  -->
#' <!-- # plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Price") -->
#' <!-- # points(tempdf$time, tempdf$fitted_values, col='red', type='l') -->
#' <!-- #  -->
#' <!-- # legend(0,0.035,c("Smoothed by degree(3)"),lty=c(1), -->
#' <!-- #        box.lty=1,lwd=2,col=c("red"),cex=0.8) -->
#' <!-- fit2$coefficients -->
#' <!-- summary(fit2) -->
#' <!-- coefs =fit2$coefficients -->
#' 
#' 
#' <!-- plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Price") -->
#' <!-- # points(tempdf$time, tempdf$fitted_values, col='red', type='l') -->
#' <!-- chip <- function(x) coefs[1] + x*coefs[2] + (x^2)*coefs[3] + (x^3)*coefs[4] -->
#' <!-- # chip <- function(x) x*result$par[2] + result$par[3]*(x^3) + result$par[1] -->
#' <!-- curve(chip, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), add=T) -->
#' 
#' <!-- ``` -->
#' 
#' <!-- Try with poly raw = True -->
#' 
#' <!-- ```{r} -->
#' <!-- par(mfrow=c(1,1)) -->
#' 
#' <!-- # fit poly of order 2 -->
#' <!-- ?I -->
#' <!-- fit2=lm(forward_rates~poly(time, degree = 3, raw = T), data = ZCBP_1) -->
#' <!-- tempdf= data.frame(cbind(ZCBP_1$time, fit2$fitted.values)) %>%  -->
#' <!--   `colnames<-`(c('time','fitted_values')) -->
#' <!-- tempdf=tempdf[order(tempdf$time),] # create  -->
#' <!-- # plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Price") -->
#' <!-- # points(tempdf$time, tempdf$fitted_values, col='red', type='l') -->
#' <!-- #  -->
#' <!-- # legend(0,0.035,c("Smoothed by degree(3)"),lty=c(1), -->
#' <!-- #        box.lty=1,lwd=2,col=c("red"),cex=0.8) -->
#' <!-- fit2$coefficients -->
#' <!-- summary(fit2) -->
#' <!-- coefs =fit2$coefficients -->
#' 
#' 
#' <!-- plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Price") -->
#' <!-- # points(tempdf$time, tempdf$fitted_values, col='red', type='l') -->
#' <!-- chip <- function(x) coefs[1] + x*coefs[2] + (x^2)*coefs[3] + (x^3)*coefs[4] -->
#' <!-- # chip <- function(x) x*result$par[2] + result$par[3]*(x^3) + result$par[1] -->
#' <!-- curve(chip, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), add=T, col='red') -->
#' 
#' 
#' 
#' <!-- ``` -->
#' 
#' 
#' 
#' <!-- ```{r} -->
#' <!-- par(mfrow=c(1,1)) -->
#' 
#' <!-- # fit poly of order 2 -->
#' 
#' <!-- fit2=lm(forward_rates~ time +  I(time^2) + I(time^3),data = ZCBP_1) -->
#' <!-- tempdf= data.frame(cbind(ZCBP_1$time, fit2$fitted.values)) %>%  -->
#' <!--   `colnames<-`(c('time','fitted_values')) -->
#' <!-- tempdf=tempdf[order(tempdf$time),] # create  -->
#' <!-- # plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Price") -->
#' <!-- # points(tempdf$time, tempdf$fitted_values, col='red', type='l') -->
#' <!-- #  -->
#' <!-- # legend(0,0.035,c("Smoothed by degree(3)"),lty=c(1), -->
#' <!-- #        box.lty=1,lwd=2,col=c("red"),cex=0.8) -->
#' <!-- fit2$coefficients -->
#' <!-- summary(fit2) -->
#' <!-- coefs =fit2$coefficients -->
#' <!-- coefs -->
#' <!-- plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Price") -->
#' <!-- # points(tempdf$time, tempdf$fitted_values, col='red', type='l') -->
#' <!-- chip <- function(x) coefs[1] + x*coefs[2] + (x^2)*coefs[3] + (x^3)*coefs[4] -->
#' <!-- # chip <- function(x) x*result$par[2] + result$par[3]*(x^3) + result$par[1] -->
#' <!-- curve(chip, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), add=T, col='red') -->
#' 
#' 
#' 
#' <!-- ``` -->
#' 
#' 
#' <!-- Try using `optim()` as well -->
#' 
#' 
#' <!-- When using optim remember to set the lower and upper bound of the coefficients, else will give differing results. Then use curve to superimpose the smoothed curve on the original scatterplot  -->
#' 
#' <!-- ```{r} -->
#' <!-- par(mfrow=c(1,1)) -->
#' <!-- fit2=lm(forward_rates~poly(time, degree = 3), data = ZCBP_1) -->
#' <!-- # tempdf= data.frame(cbind(ZCBP_1$time, fit2$fitted.values)) %>%  -->
#' <!-- #   `colnames<-`(c('time','fitted_values')) -->
#' <!-- tempdf=tempdf[order(tempdf$time),] # create  -->
#' <!-- plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Price") -->
#' <!-- # points(tempdf$time, tempdf$fitted_values, col='red', type='l') -->
#' <!-- coef = fit2$coefficients -->
#' <!-- # plot( function(x) x*coef[2] + coef[3]*(x^2) + coef[4]*(x^3) + coef[1], min(tempdf$time), max(tempdf$time),ylim=c(0,0.08)) -->
#' <!-- # abline(h=0,v=0,lty=3) -->
#' <!-- # legend(0,0.035,c("Smoothed by degree(3)"),lty=c(1), -->
#' <!-- #        box.lty=1,lwd=2,col=c("red"),cex=0.8) -->
#' 
#' <!-- # specify SSR -->
#' <!-- sumSqMin  <- function(data, par) { -->
#' <!--   # order 2 -->
#' 
#' <!--   with(data, sum((par[1] + par[2]*(time) + par[3]*(time^2) - forward_rates)^2))   -->
#' 
#' <!-- } -->
#' <!-- sumSqMin2  <- function(par, data) { -->
#' <!--   # order 3 -->
#' <!--   with(data, sum((par[1] + par[2]*(time) + par[3]*(time^2) + par[4]*(time^3) - forward_rates)^2))   -->
#' 
#' <!-- } -->
#' <!-- sumSqMin3  <- function(par, data) { -->
#' <!--   # order 3 -->
#' <!--   with(data, sum((par[1] + par[2]*(time) + + par[3]*(time^3) - forward_rates)^2))   -->
#' 
#' <!-- } -->
#' 
#' <!-- result = optim(par = c(0,0,0), fn = sumSqMin, data=ZCBP_1,lower = c(-1,-1,-1), upper = c(1,1,1), method="L-BFGS-B") -->
#' <!-- result -->
#' <!-- plot( function(x) x*result$par[2] + result$par[3]*(x^2) + result$par[1], min(ZCBP_1$time), max(ZCBP_1$time),ylim=c(0,0.08)) -->
#' <!-- abline(h=0,v=0,lty=3) -->
#' 
#' <!-- result = optim(par = c(0,0,0,0), fn = sumSqMin2, data=ZCBP_1,lower = c(-1,-1,-1,-1), upper = c(1,1,1,1), method="L-BFGS-B") -->
#' <!-- # result = optim(par = c(0,0,0,0), fn = sumSqMin2, data=ZCBP_1) -->
#' <!-- result -->
#' <!-- plot( function(x) x*result$par[2] + result$par[3]*(x^2) + result$par[4]*(x^3) + result$par[1], min(ZCBP_1$time), max(ZCBP_1$time),ylim=c(0,0.08)) -->
#' <!-- abline(h=0,v=0,lty=3) -->
#' 
#' 
#' <!-- result = optim(par = c(0,0,0), fn = sumSqMin3, data=ZCBP_1,lower = c(-1,-1,-1), upper = c(1,1,1), method="L-BFGS-B") -->
#' <!-- # result = optim(par = c(0,0,0), fn = sumSqMin3, data=ZCBP_1) -->
#' <!-- result -->
#' <!-- plot( function(x) x*result$par[2] + result$par[3]*(x^3) + result$par[1], min(ZCBP_1$time), max(ZCBP_1$time),ylim=c(0,0.08)) -->
#' <!-- abline(h=0,v=0,lty=3) -->
#' 
#' <!-- result$par -->
#' 
#' <!-- plot(ZCBP_1$time, ZCBP_1$forward_rates, main = "Empirical forward rates against maturities", xlab="Time to maturity", ylab="Price", ylim=c(0,0.08)) -->
#' 
#' <!-- chip <- function(x) x*result$par[2] + result$par[3]*(x^3) + result$par[1] -->
#' <!-- curve(chip, min(ZCBP_1$time), max(ZCBP_1$time),n=length(ZCBP_1$time), add=T) -->
#' 
#' 
#' 
#' <!-- ``` -->
#' 
#' <!-- ```{r, echo=FALSE, results=FALSE} -->
#' <!-- par(mfrow=c(1,1)) -->
#' <!-- fit2=lm(forward_rates~poly(time, degree = 3), data = ZCBP_1) -->
#' <!-- plot(ZCBP_1$time, ZCBP_1$forward_rates) -->
#' 
#' <!-- points(ZCBP_1$time, fit2$fitted.values, col='red') -->
#' <!-- # points(ZCBP_1$time, fit2$fitted.values, col='red',type='b') -->
#' 
#' <!-- ``` -->
#' 
#' 
#' <!-- ```{r} -->
#' <!-- par(mfrow=c(1,1)) -->
#' <!-- # plot(ZCBP_1$time, ZCBP_1$spot_rates) -->
#' <!-- # line() -->
#' <!-- # help(lines) -->
#' <!-- # lines(ZCBP_1[c(1,3)]) -->
#' <!-- # plot(ZCBP_1$time, poly(ZCBP_1$spot_rates, degree = 2)) -->
#' <!-- fit1=lm(spot_rates~poly(time), data = ZCBP_1) -->
#' <!-- # poly(ZCBP_1$spot_rates, degree = 2) -->
#' <!-- plot(ZCBP_1$time, ZCBP_1$spot_rates) -->
#' <!-- points(ZCBP_1$time, fit1$fitted.values, col='red',type='l') -->
#' <!-- # plot(ZCBP_1$time, fit1$fitted.values) -->
#' <!-- # abline(fit1,col='red', lwd=2) -->
#' <!-- # cbind(ZCBP_1$spot_rates, fit1$fitted.values) -->
#' <!-- # lines(ZCBP_1$time,fit1$fitted.values[:,2], lty=1) -->
#' <!-- # fit1$fitted.values -->
#' 
#' 
#' <!-- # define function that returns the SSE -->
#' 
#' <!-- calcSSE <- function(df){ -->
#' <!--   # x is the dataframe -->
#' 
#' <!-- } -->
#' 
#' 
#' 
#' <!-- ``` -->
#' 
#' 
#' <!-- ## Question 4 -->
#' 
#' 
#' 
#' <!-- ```{r} -->
#' <!-- spot_rates_1 = c(1:max(ZCBP_2$year)) -->
#' <!-- # loop through each interval t and calculate the spot rate at time t -->
#' <!-- for (i in 1:max(spot_rates_1)) { -->
#' <!--   # get the rows corresponding to that year -->
#' <!--   ind=ZCBP_2$year<=i -->
#' <!--   temp_df = ZCBP_2[ind,] -->
#' <!--   # find the cum_sum up till time i -->
#' <!--   tempcum_sum = sum(temp_df$forward_rates*diff(c(temp_df$time,i))) -->
#' 
#' <!--   spot_rates_1[i] = tempcum_sum/i -->
#' <!--   # spot_rates[i] = (ZCBP_1$forward_rates[1]*ZCBP_1$time[2] + tempcum_sum[nrow(tempcum_sum)])/i -->
#' 
#' <!-- } -->
#' 
#' <!-- spot_rates == spot_rates_1 -->
#' <!-- spot_rates -->
#' <!-- ind = as.array(spot_rates_1) != as.array(spot_rates) -->
#' <!-- ind -->
#' <!-- cbind(spot_rates, -->
#' <!-- spot_rates_1) %>% .[ind,] -->
#' 
#' <!-- ``` -->
#' 
#' 
#' <!-- ## Question 5 -->
#' 
#' 
#' <!-- ```{r} -->
#' <!-- par(mfrow=c(1,1)) -->
#' 
#' <!-- spot_rates_df = cbind(spot_rates,seq(1:29)) %>% `colnames<-`(c('spot_rates','time')) %>% data.frame() -->
#' <!-- spot_rates_df -->
#' <!-- fit3=lm(spot_rates~poly(time, degree = 3), data = spot_rates_df) -->
#' <!-- tempdf1= data.frame(cbind(spot_rates_df$time, fit3$fitted.values)) %>%  -->
#' <!--   `colnames<-`(c('time','fitted_values')) -->
#' 
#' <!-- plot(spot_rates_df$time, spot_rates_df$spot_rates, main = "Empirical spot rates against maturities", xlab="Time to maturity", ylab="Spot rates", ylim = c(0,0.07)) -->
#' <!-- # points(tempdf1$time, tempdf1$fitted_values, col='red', type='l') -->
#' <!-- # points(tempdf1$time, tempdf1$fitted_values, col='red',type = 'l') -->
#' <!-- # line(fit3) -->
#' <!-- # prediction = predict(fit3) -->
#' <!-- # ix = sort(tempdf1$time, index.return=T)$ix -->
#' <!-- # lines(tempdf1$time[ix], prediction[ix], col=2, lwd=2) -->
#' <!-- legend(15,0.035,c("Smoothed by degree(3)"),lty=c(1), -->
#' <!--        box.lty=1,lwd=2,col=c("red"),cex=0.8) -->
#' 
#' <!-- ``` -->
#' 
#' <!-- We use the predict function to get the estimated Y value for each X value and proceed to plot it with `line()` function -->
#' 
#' 
#' <!-- ### raw = True -->
#' 
#' <!-- ```{r} -->
#' <!-- par(mfrow=c(1,1)) -->
#' 
#' <!-- spot_rates_df = cbind(spot_rates,seq(1:29)) %>% `colnames<-`(c('spot_rates','time')) %>% data.frame() -->
#' <!-- spot_rates_df -->
#' <!-- fit3=lm(spot_rates~poly(time, degree = 3, raw=T), data = spot_rates_df) -->
#' <!-- tempdf1= data.frame(cbind(spot_rates_df$time, fit3$fitted.values)) %>%  -->
#' <!--   `colnames<-`(c('time','fitted_values')) -->
#' 
#' <!-- plot(spot_rates_df$time, spot_rates_df$spot_rates, main = "Empirical spot rates against maturities", xlab="Time to maturity", ylab="Spot rates", ylim = c(0,0.07)) -->
#' <!-- # points(tempdf1$time, tempdf1$fitted_values, col='red', type='l') -->
#' <!-- # points(tempdf1$time, tempdf1$fitted_values, col='red',type = 'l') -->
#' <!-- # line(fit3) -->
#' <!-- # prediction = predict(fit3) -->
#' <!-- # ix = sort(tempdf1$time, index.return=T)$ix -->
#' <!-- # lines(tempdf1$time[ix], prediction[ix], col=2, lwd=2) -->
#' 
#' <!-- coefs = fit3$coefficients -->
#' <!-- chip <- function(x) coefs[1] + x*coefs[2] + (x^2)*coefs[3] + (x^3)*coefs[4] -->
#' <!-- # chip <- function(x) x*result$par[2] + result$par[3]*(x^3) + result$par[1] -->
#' <!-- # curve(chip, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), add=T, col='red') -->
#' <!-- #  -->
#' <!-- # legend(15,0.035,c("Smoothed by degree(3)"),lty=c(1), -->
#' <!-- #        box.lty=1,lwd=2,col=c("red"),cex=0.8) -->
#' 
#' <!-- curve(chip, min(ZCBP_1$time),max(ZCBP_1$time),n=length(ZCBP_1$time), col='red', ylim=c(0, 0.07)) -->
#' 
#' <!-- ``` -->
#' 
#' 
#' <!-- ### Try using `optim()` -->
#' 
#' <!-- ```{r} -->
#' <!-- sumSqMin  <- function(par, data) { -->
#' 
#' <!--   with(data, sum((par[1] + par[2]*(time) + par[3]*(time^2) + par[4]*(time^3) - spot_rates)^2))   -->
#' 
#' <!-- } -->
#' 
#' <!-- result = optim(par = c(0,1,1,1), fn = sumSqMin, data=spot_rates_df) -->
#' <!-- # result -->
#' <!-- temp_df2=cbind(spot_rates_df$time,spot_rates_df$time,spot_rates_df$time) -->
#' <!-- plot(spot_rates_df$time, spot_rates_df$spot_rates, main = "Empirical spot rates against maturities", xlab="Time to maturity", ylab="Spot rates", ylim = c(0,0.07)) -->
#' <!-- points(spot_rates_df$time, apply(temp_df2 *result$par[2:4] -->
#' <!--  + result$par[1],MARGIN = 1, sum), col='red', type='l') -->
#' <!-- # points(spot_rates_df$time, apply(spot_rates_df$time*result$par[2:4] + result$par[1],MARGIN = 1, sum), col='red', type='l') -->
#' 
#' <!-- legend(15,0.035,c("Smoothed by degree(3)"),lty=c(1), -->
#' <!--        box.lty=1,lwd=2,col=c("red"),cex=0.8) -->
#' 
#' 
#' <!-- ``` -->
#' 
#' # Part 2
#' 
## ----------------------------------------------------------------------------------------------------------------
# Present the data in a suitable way

library(YieldCurve)
library(xts)
library(dlookr)
library(quantmod)
library(rmgarch)
library(rugarch)
library(fGarch)
library(lattice)

# ZCBYF86 <- read.csv("ZCBYF86.csv")  # Replace with your own data import
ZCBYF86 <- read.csv("C:/Users/LWD/Desktop/SMU Files/DSA302 Financial Data Analysis/Data Sets/ZCBYF86.csv")

options(digits=5)

# Start with basic plots
Date = as.Date(ZCBYF86[,1], format="%d/%m/%Y")
ZCBData = cbind(Date, ZCBYF86[,-1])
str(ZCBData)
summary(is.na(ZCBData))  # No NA values
ZCB.xts = xts(ZCBData[,2:31], order.by = ZCBData[,1])
ZCB.ret = diff(ZCB.xts)
ZCB.ret = ZCB.ret[2:nrow(ZCB.ret),]
n=nrow(ZCBData)
ZCBData.diff = ZCBData[2:n,-1] - ZCBData[1:(n-1),-1]
ZCBData.diff = cbind(ZCBData[-1,1], ZCBData.diff)

summary(ZCBData.diff)
xyplot.ts(ZCB.xts,scales=list(y=list(relation="same")),ylab="Yield (%)")
# From this graph, it can be seen that each variable is closely correlated to each other, and there is a downward trend.
# Visually, stocks are highly correlated with each other

# Perform Principle Component Analysis to determine how useful each variable is
summary(cor(ZCBData[,-1]))
summary(prcomp(ZCBData[,-1], center = TRUE, scale. = TRUE))
# From this, can be shown that 1 year bonds explain 97.33% of the total variance, therefore showing just 1 year should suffice
# This result is expected, as all the bonds are issued by same entity

plot(ZCBData[,1], ZCBData[,2], type = c("l"), xlab = "Year", ylab = "Rate", main = "Daily Rates")
abline(lm(ZCBData[,2] ~ ZCBData[,1]), col = "blue")
# Downward trend in daily rates can be seen over the years
plot(ZCBData.diff[,1], ZCBData.diff[,2], type = c("l"), xlab = "Year", ylab = "Rate", main = "First Differenced Rates")
summary(glm(ZCBData[,2] ~ ZCBData[,1])) # General linear equation

ZCB_normalized <- normality(ZCBData)
head(ZCB_normalized)
plot_normality(ZCBData,SVENY01) # 1st column seems to be explaining most variance
plot_normality(ZCBData,SVENY02) # 2nd column explaining second most variance after SVENY02

#' 
## ----------------------------------------------------------------------------------------------------------------
# Fit the NS and NSS models to the yield data by minimizing the sum of squared error
# Compare both models using suitable diagnostics and model selection criteria

# Use YieldCurve package to solve for NS Model
ZCB.xts = xts(ZCBData[,-1], order.by = ZCBData[,1])
maturity.ZCB = c(1:30)
rate.ZCB = ZCB.xts
NSModel = Nelson.Siegel(rate = rate.ZCB, maturity = maturity.ZCB)
summary(NSModel)

# Plot NS Model Yield Curve
NS.y = NSrates(NSModel[5,], maturity.ZCB)
plot(maturity.ZCB, rate.ZCB[5,], main = "Fitting Nelson-Siegel yield curve", type="o", xlab = "Maturity", ylab = "Rate")
lines(maturity.ZCB, NS.y, col=2)
legend("bottomright",legend=c("observed yield curve","fitted yield curve"),col=c(1,2),lty=1)

# Note that the values generated from the YieldCurve package are slightly different

# Use YieldCurve package to solve for NSS Model
NSSModel = Svensson(rate = rate.ZCB, maturity = maturity.ZCB)
summary(NSSModel)
# Plot NSS Model Yield Curve
NSS.y = Srates(NSSModel, maturity.ZCB, "Spot")
plot(maturity.ZCB, rate.ZCB[5,], main = "Fitting Svensson yield curve", type="o", xlab = "Maturity", ylab = "Rate")
lines(maturity.ZCB, NSS.y[5,], col=2)
legend("bottomright",legend=c("Observed yield curve","Fitted yield curve"),col=c(1,2),lty=1)

# Note that the values generated from the YieldCurve package are slightly different

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE(NS.y, rate.ZCB)
RMSE(NSS.y, rate.ZCB)  # NSS have smaller RMSE

#' 
## ----------------------------------------------------------------------------------------------------------------
# What information can be extracted from the estimates of tetha?
# Estimate theta values using RSS
# NS Model
minSSE = function(par,data) {
  with(data,sum((par[1] + par[2]*NSModel[,1] + par[3]*NSModel[,2] +par[4]*NSModel[,3] -NSModel[,4])^2))
}

result <- optim(par=c(0,1,1,1), fn=minSSE, data=ZCB.xts, control=list(reltol=1e-10,maxit=2000))
result

# NSS Model
minSSE2 = function(par,data) {
  with(data,sum((par[1] + par[2]*NSSModel[,1] + par[3]*NSSModel[,2] +par[4]*NSSModel[,3] +par[5]*NSSModel[,4] +par[6]*NSSModel[,5] -NSSModel[,6] )^2))
}

result2 <- optim(par=c(0,1,1,1,1,1), fn=minSSE2, data=ZCB.xts, control=list(reltol=1e-10,maxit=2000))
result2

#' 
## ----------------------------------------------------------------------------------------------------------------
# What does data tell about response of the spot rates at the long end and
# wrt spot rates at the short end?

xyplot.ts(ZCB.xts,scales=list(y=list(relation="same")),ylab="Yield (%)")
# This graph shows how the bond price is less responsive to spikes as maturity time increases

QK = function(y,p1=0.025,p2=0.25)
{
  Q = quantile(y,c(p1,p2,1-p2,1-p1))
  as.numeric((Q[4]-Q[1]) / (Q[3]-Q[2]))
}
MK = function(y) mean(((y - mean(y)) / sd(y))^4)
QK(ZCBData.diff[,2])  # Quantile Kurtosis for SVENY01, leptokurtic
MK(ZCBData.diff[,2])  # Moment Kurtosis for SVENY01, leptokurtic


#' 
## ----------------------------------------------------------------------------------------------------------------
# Fit NS and NSS models on each day as a static exercise.
# Can they be used to predict future interest rate movements?

ZCB2 = cbind(ZCB.ret[,1], ZCB.ret[,2], ZCB.ret[,30])
spu=ugarchspec(variance.model=list(model="fGARCH",garchOrder=c(1,1), submodel="GARCH"),mean.model=list(armaOrder=c(0,0),include.mean=TRUE))
mspec=multispec(list(spu,spu, spu))
spm=dccspec(mspec,model="DCC",distribution="mvnorm")
fit=dccfit(spm,ZCB2)
fit

ZCB.ret=diff(log(ZCB.xts))
ZCB.ret=na.omit(ZCB.ret)
nr=nrow(ZCB.ret)
xa=1986+(1:nr)/252

# ARCH(4)
ZCB.arch1= garchFit(~1+garch(1,0),data=ZCB.ret[,1],trace=F)
summary(ZCB.arch1)

pred.va=predict(ZCB.arch1,n.ahead=100)
plot(1:100,pred.va$standardDeviation,type="l",xlab="Days",
     main="Predicted volatility over 100 days")

v.a=volatility(ZCB.arch1)
plot(xa,v.a,type="l",xlab="Year",ylab="Volatility",
     main="ARCH(4) Volatility, normal errors")
res.a=residuals(ZCB.arch1,standardize=T)
plot(xa,res.a,type="l",xlab="Year",ylab="Std Residual",
     main="ARCH(4) Std Res, normal errors")
abline(h=3,lty=2,col="red")
abline(h=-3,lty=2,col="red")


# Garch
m.g=garchFit(~1+garch(1,1),data=ZCB.ret[,1],trace=F)
summary(m.g)
par(mfrow=c(1,1))

v.g=volatility(m.g)
plot(xa,v.g,type="l",xlab="Year",ylab="Volatility",
     main="GARCH(1,1) Volatility, normal errors")
res.g=residuals(m.g,standardize=T)
plot(xa,res.g,type="l",xlab="Year",ylab="Residual",
     main="GARCH(1,1) Std Res, normal errors")
abline(h=3,lty=2,col="red")
abline(h=-3,lty=2,col="red")

qqnorm(res.g,datax=T,main="QQnorm of GARCH(1,1) std res, normal errors")
qqline(res.g,data=T,col="red")

#' 
#' 
