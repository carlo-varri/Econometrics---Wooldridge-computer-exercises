library(wooldridge)
library(fixest)
library(tidyverse)
library(plotrix)
library(car)
library(lmtest)
###### Chapter 3 exercises ####### 

####3.13####
#iii
lm_with_faminc <- feols(bwght ~ cigs + faminc, data = bwght)
lm_without_faminc <- feols(bwght ~ cigs, data = bwght)
etable(lm_with_faminc, lm_without_faminc)
#adding faminc doesn't have much of an impact on the estimated effects of 
#cigs - highly significant both times 
#This is due to the fact that cigs and faminc are not very correlated ( -0.1730449), and 
#the coefficient on faminc is practically small.

####3.14####
hprice1_regression <- lm(price ~ sqrft + bdrms, data = hprice1)
summary(hprice1_regression)

####3.15####
#i
ceo_sal_regression <- lm(log(salary) ~ log(sales) + log(mktval), data = ceosal2)
summary(ceo_sal_regression)
#ii
ceo_sal_regression_with_profits <- lm(log(salary) ~ log(sales) + log(mktval) + profits, data = ceosal2)
summary(ceo_sal_regression_with_profits)

#R2 of only 0.299 - not most of the variation

#iii
ceo_sal_regression_with_ceoten <- lm(log(salary) ~ log(sales) + log(mktval) + profits + ceoten, data = ceosal2)
summary(ceo_sal_regression_with_ceoten)
#another year increases salary by 1.2%

#iv
cor(log(ceosal2$mktval), ceosal2$profits)
#yes, highly correlated. Not surprising profits is not significant 

####3.16#####
#i

fn <- function(i) {
  print(paste("min =", min(i)))
  print(paste("max =", max(i)))
  print(paste("av = ", mean(i)))
}

fn(attend$atndrte)
fn(attend$priGPA)
fn(attend$ACT)

#ii
attend_regression <- lm(atndrte ~ priGPA + ACT, data = attend)
summary(attend_regression)

#iii
#surprising that the sign on ACT is negative. One more ACT point reduces 
#attendance rate by 1.7 pp

####3.17#### 
obtaining_residuals <- lm(educ ~ exper + tenure, data = wage1)
obtaining_B1_hat <- lm(log(wage)~ obtaining_residuals$residuals, data = wage1)
obtaining_B1_hat$coefficients
#same residual - if education increases on year, wage increases 9%

#### 3.18 ####

#i
iq_on_educ <- lm(IQ ~ educ, data = wage2)
summary(iq_on_educ)
deltahat <- iq_on_educ$coefficients[[2]]

#ii
wage_on_educ <- lm(log(wage) ~ educ, data = wage2)
summary(wage_on_educ)
B1bar <- wage_on_educ$coefficients[[2]]

#iii
mlr <-  lm(log(wage) ~ educ + IQ, data = wage2)
B1hat <- mlr$coefficients[[2]]
B2hat <- mlr$coefficients[[3]]

B1hat + B2hat*deltahat == B1bar


########## Chapter 4 exercises ###########

####4.12####

#i
#if expenditure changes by x, voteA changes by B1/100*x 

#ii 
#H0: B1 = -B2

#iii 
voteA_regression <- lm(voteA ~ log(expendA) + log(expendB) + prtystrA, data = vote1)
summary(voteA_regression)
#yes, expendA and expendB affect the outcome. 

#iv
#define a = B1 + B2
#we test H0: a = 0 
#reforming the regression to:

#y = B0 + ax1 + B2(x2 - x1) + B3x3 + u 

vote1 <- vote1 %>% mutate(diff_expend = log(expendB) - log(expendA))

reformed_regression <- lm(voteA ~ log(expendA) + diff_expend + prtystrA, data = vote1)
summary(reformed_regression)

##### 4.13 #####

#i
#H0: coeff on rank (B5) = 0

law_school_rank <- lm(log(salary) ~ LSAT + GPA + log(libvol) + 
                        log(cost) + rank, data = lawsch85)
summary(law_school_rank)
#we can reject the null: it is highly significant. If rank decreases by 
#10, salary increases by 3.3%

#ii
law_school_rank <- lm(log(salary) ~ LSAT + GPA + log(libvol) + 
                        log(cost) + rank, data = lawsch85)
linearHypothesis(law_school_rank, c("LSAT=0", "GPA=0"))
#jointly, they're highly significant 

#iii
with_class_size_and_faculty <- lm(log(salary) ~ LSAT + GPA + log(libvol) + 
                        log(cost) + rank + clsize + faculty,
                      data = lawsch85)
linearHypothesis(with_class_size_and_faculty, c("clsize=0", "faculty=0"))
#not jointly significant 

##### 4.14 #####

#i
house_prices <- lm(log(price) ~ sqrft + bdrms, data = hprice1)

theta1 <- 150*house_prices$coefficients[[2]] + house_prices$coefficients[[3]]
#price increase of 8.6%

#ii
#B2 = theta1 - 150B1 
#into regression eqn: y = B0 + B1(x1 -150x2) + thetax2 +u

hprice1 <- hprice1 %>% mutate(new_col = sqrft - 150*bdrms)

#logprice = B0 + B1(new_col) + theta(bdrms)+ u 

#iii

house_prices_two <- lm(log(price) ~ new_col + bdrms, data = hprice1)
summary(house_prices_two)
ninety_five_ci <- data.frame(upper_bound = 
                      house_pices_two$coefficients[[3]] + 
                      0.95*sqrt(diag(vcov(house_prices_two)))[[3]]/sqrt(88),
                    lower_bound =house_pices_two$coefficients[[3]] - 
                      0.95*sqrt(diag(vcov(house_prices_two)))[[3]]/sqrt(88))
ninety_five_ci
#this is wrong 


##### 4.15 #####

bwght <- lm(bwght ~ cigs + parity + faminc, data = bwght)
summary(bwght)
#R^2 = 0.0348 vs 0.0387 previously

##### 4.16 #####

#i
mlb <- lm(log(salary) ~ years + gamesyr + bavg + hrunsyr, data = mlb1)
summary(mlb)
#hrunsyr is now highly significant (was not significant before)
#the size of the coefficient has increased. If home runs increases by 
#10 per year, salary goes up 30%. 

#ii
mlb_two <- lm(log(salary) ~ years + gamesyr + bavg + hrunsyr +
                runsyr + fldperc + sbasesyr, data = mlb1)
summary(mlb_two)
#years, gamesyr, hrunsyr, runsyr are significant 

#iii
linearHypothesis(mlb_two, c("bavg=0", "fldperc=0", "sbasesyr=0"))
#F is 0.685 - not significant 

##### 4.17 #####

#i
wage <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
#H0: B2 = B3 

#ii
#a = B2 - B3 
#B2 = a + B3 into equation to get 
#y = B0 + B1x1 + ax2 + B3(x2+x3) + u 

wage1 <- wage1 %>% mutate(exper_plus_tenure = exper + tenure)

wage_two <- lm(log(wage) ~ educ + exper + exper_plus_tenure, data = wage1)
summary(wage_two)
#a is highly significant so we can reject H0.  

#this is giving the wrong answer but cant work out why

##### 4.18 #####

#i
phsrank <- 
  data.frame( smallest =
              min(twoyear$phsrank), 
            largest = 
              max(twoyear$phsrank),
            average = 
              mean(twoyear$phsrank))

#ii
colleges <- lm(log(lwage) ~ jc + totcoll + exper + phsrank, data = twoyear)
summary(colleges)
#phsrank is not s.s
#10 pp of rank is + 0.14% wage 

#iv
#id is not important to wage. Should be randomly assigned 
colleges_two <- lm(log(lwage) ~ jc + totcoll + exper + phsrank +id, data = twoyear)
summary(colleges_two)

#### 4.19 #####

#i
k401ksubs %>% filter(fsize== 1) %>% count()
k401ksubs_singles <- k401ksubs %>% filter(fsize== 1)
#ii 
#estimating single person households only
net_wealth <- lm(nettfa ~ inc + age, data = k401ksubs_singles)
summary(net_wealth)
#no slope estimate surprises 

#iii 
#intercep: someone with no income and no savings has -43$ in savings. 
#this is not interesting 

#iv
coeftest(net_wealth, hypothesis = "age = 1", alternative = "less")
#didnt get the right result - better to just do 0.84266-1/0.09202 

#v 
simple <- lm(nettfa ~ inc, data = k401ksubs_singles)
summary(simple)
#coeff is 0.8207 vs 0.79932 - not very different. Because correlation 
#between age and inc is low 



