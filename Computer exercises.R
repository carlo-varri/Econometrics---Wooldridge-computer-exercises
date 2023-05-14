library(wooldridge)
library(fixest)
library(tidyverse)
library(plotrix)
library(car)
library(lmtest)
library(huxtable)
library(AER)

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

#### C7 #### 

#i
maths_scores <- lm((math10) ~ log(expend) + lnchprg, data = meap93)
summary(maths_scores)
#yes, signs coefficients are as expected 

#ii
#setting expenditure equals to zero doesn't make sense: min is 3332 in the sample

#iii 
maths_scores_two <- lm(math10 ~ log(expend), data = meap93)
summary(maths_scores_two)
#coefficient is now much larger. likely omitted variable bias

#iv 
cor(meap93$lexpend, meap93$lnchprg)
#more expenditure correlayed with less poverty. Makes sense - richer schools in 
#richer areas

#v 
#coeff on lnch is negative. Corr between lunch and expend is also negative. 
#so, the biad in iii is positive, as expected 
#failing to account for the poverty rate leads to an overestimate of the effect of spending.



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

####C9 (from 4th edition) ####

#i
price_discrim <- lm(log(psoda) ~ prpblck + log(income) + prppov, data = discrim)
summary(price_discrim)
#prpblck is significant at the 5% level, but not the 1% level

#ii
discrim_no_na <- discrim %>% drop_na()
cor(log(discrim_no_na$income), discrim_no_na$prppov)
#strongly correlated, indicating strong multicollinearlity 
#all are significant above 

#iii
price_discrim_two <- lm(log(psoda) ~ prpblck + log(income) + prppov +
                          log(hseval), data = discrim)
summary(price_discrim_two)
#this says that if median housing value rises by 10%, price of soda will increase 
#by 1.2%
#two sides p val is close to zero 

#iv
#log income change sign and becomes insignificant 
#prppov becomes insiginficant 
#high correlation between these and house price. Their effect is captured when we 
#include house prices 

linearHypothesis(price_discrim_two, c("prppov=0", "log(income)=0"))
#jointly significant at the 5% level 

#v
#I think the final one. If we omit a significant variable (hseval), we will bias 
#our estimates of other variables of interest. 
#if prpblck increases by 1, then psode increases 9.7%


#### C10 #####
#i 
benefits <- lm(lavgsal~ bs, data = elem94_95)
summary(benefits)
#estimated slope is different from zero 
ans <- (-0.79512 +1) / 0.14965
pt(ans, (1848 - 2))
#reject at 10%, not 5% though

#ii
salary <- lm(lavgsal~ bs + lenrol + lstaff, data = elem94_95)
summary(salary)
#bs increases from -.79 to -.6, and remains highly significant 

#iii
#adding more terms increases standard error via multicolinariety 
#but reduces it via reduced error variance. 
#in this case, the second effect dominates 

#iv
#lstaff negative implies that if there are more staff then
#salary is lower. This may be because staff are willing to take a pay cut to go 
#to schools with more staff per pupils. 
#coefficient is an elasticty: as lstaff increases 1%, salary drops 0.7%

#v
salary_two <- lm(lavgsal~ bs + lenrol + lstaff + lunch, data = elem94_95)
summary(salary_two)
#coeff on lunch is negative. The more disadvatanged children there are, the lower
#the salary. This may be because these schools are in poorer areas


##### Chapter 7 Computer exercises #####
#### C1 ####
#i
gpa <- lm(colGPA ~ PC + hsGPA + ACT + mothcoll + fathcoll, data = gpa1)
summary(gpa)
#t value for PC drops very slightly, and still significant 

#ii
linearHypothesis(gpa, c("mothcoll=0", "fathcoll=0"))
#jointly insignificant so not surprising they dont change the other coeffs much 

#iii
gpa1 <- gpa1 %>% mutate(hsGPAsqr = hsGPA^2)
gpa_two <- lm(colGPA ~ PC + hsGPA + ACT + mothcoll + fathcoll +hsGPAsqr , data = gpa1)
summary(gpa_two)
#just insiginficant 
#adds a turning point to hsGPA 

#iii

#### C2 ####
#i
wge <- lm(log(wage) ~ educ + exper + tenure + married + black + south + urban, data = wage2)
summary(wge)
#salary different is 18% between black and non black, and is highly significant 

#ii
wage2 <- wage2 %>% mutate(expersqr = exper^2, tenuresqr = tenure^2)
wge_2 <- lm(log(wage) ~ educ + exper + tenure + married + black + south + urban +
            expersqr + tenuresqr, 
          data = wage2)
summary(wge_2)
linearHypothesis(wge_2, c("expersqr = 0", "tenuresqr = 0"))

#iii
wge_3 <- lm(log(wage) ~ educ*black + exper + tenure + married + black + south + urban, data = wage2)
summary(wge_3)
#return to educ doesnt depend on race 
#educ coeff now relates to non-black men 

wge_4 <- lm(log(wage) ~ educ + exper + tenure + married + married:black + 
              black + south + urban, data = wage2)
summary(wge_4)
#seem to be getting wrong coeff on interaction term for some reason 

#### C3 ####
#i
baseball <- lm(log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + 
                 fldperc + allstar +frstbase + scndbase + thrdbase + shrtstop + 
                 catcher, data = mlb1)
#H0:  B13 = 0
summary(baseball)
#true at 10%. Estimated that catchers earn 100⋅[exp(.254) – 1] ≈ 28.9% more 

#ii
#H0: β9 =0, β10 =0,..., β13 =0
linearHypothesis(baseball, c("frstbase = 0", "scndbase = 0", "thrdbase = 0",
                            "shrtstop = 0", "catcher= 0"))
#dont reject at 10% - theres no difference in salaries 


#### C4 #### 

grades <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat + female + athlete,
             data = gpa2)
summary(grades)
#ii
#athlete raises colgpa by 0.17 and is highly sign

#iii
grades_two <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + female + athlete,
                 data = gpa2)
summary(grades_two)
#athlete is no longer significant 
#this may be due to a bias is athlete and sat are negatively correlated, which they are 

#yes: this happens because we do not control for SAT scores, and athletes score lower on average than nonathletes

#iv
gpa2$male <- (1- gpa2$female)
gpa2$nonath <- (1-gpa2$athlete)
grades_gendered <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat + female:athlete + male:athlete +
                        male:nonath, data = gpa2)
summary(grades_gendered)
#baseline is female non atheletes. female atlhese 

#v
grades_gendered_two <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat + sat:female +
                            female + athlete,
                          data = gpa2)
summary(grades_gendered_two)
#no, sat:female is not significant 

#### C5 ####
ceosal1 <- ceosal1 %>% mutate(rosneg = case_when(ros < 0 ~ 1,
                                                 ros >= 0 ~ 0))
salary <- lm(log(salary)~ log(sales) + roe + rosneg, data = ceosal1)
summary(salary)

#this says: 
#a 1% increase in sales will increase expected salary by 29%
#a one pp increase in roe will increase salary by 1.7%
# if roe is negative, it will decrease salary by 23%
#all significant 

#### C6 ####
#i
sleep75_male <- sleep75 %>% filter(male == 1)
sleep75_female <- sleep75 %>% filter(male == 0)
sleep_male <- lm(sleep ~ totwrk + educ + age + I(age^2) + yngkid, data = sleep75_male)
sleep_female <- lm(sleep ~ totwrk + educ + age + I(age^2) + yngkid, data = sleep75_female)

summary(sleep_male)
summary(sleep_female)
#education is not significant in female 
#young child means less sleep for women and more for men 
#quadratic diff shape 

#practically important differences in estimates for women and men 
#do not translate into statistically significant differences. 
#We need a larger sample size to confidently determine whether there are differences in slopes.

#ii 
sleep_test <- lm(sleep ~ totwrk + totwrk:male + educ + educ:male +
                   age + age:male + I(age^2) + I(age^2):male + yngkid +
                   yngkid:male, data = sleep75)
linearHypothesis(sleep_test, c("totwrk:male = 0",
                                "male:educ = 0",
                                "male:age = 0",
                                "male:I(age^2) = 0",
                                "male:yngkid = 0"))
#rejected at 5% - there are differences in slope between male and female 
#df = n-2(k+1) df = 706- 11 = 694 DF

#iii

sleep_test_two <- lm(sleep ~ male + totwrk + totwrk:male + educ + educ:male +
                   age + age:male + I(age^2) + I(age^2):male + yngkid +
                   yngkid:male, data = sleep75)

linearHypothesis(sleep_test_two, c("male:totwrk = 0",
                               "male:educ = 0",
                               "male:age = 0",
                               "male:I(age^2) = 0",
                               "male:yngkid = 0"))
#no longer significant 
#when we allow for differneces in intercept, the slope is no longer different 

#iv 
#final model would be with all the variables and an intercept for male 


#### C7 ####
#wage for men when educ = 12.5: 
male_wage <-0.389 + 0.082*12.5
#wage for wome when educ = 12.5n
female_wage <- 0.389 - 0.227 + 0.082*12.5 - 0.0056*12.5
male_wage - female_wage

#wage for men when educ = 0: 
male_wage_two <-0.389
female_wage_two <- 0.389 - 0.227 
male_wage_two - female_wage_two
#educ impacts by 7pp 

#I'm not sure if 'male wage' is accurate... educ isnt interacted by male, so I think
#the educ coefficient is irrespective of gender

#ii
wage1 <- wage1 %>% mutate(new_educ = educ -12.5)
wage <- lm(log(wage) ~ female + educ + female:new_educ + exper + I(exper^2) +
             tenure + I(tenure^2), data = wage1)
summary(wage)
#coeff on female is the wage diff at 12.5 years of education 

#iii
#the coeff on female is highly ss. The coeff in 7.18 is not
#this one is stronger as gives a much more realistic interpretation 


#### C8 ####
#i 
#B1 would equal be greater than zero 

#ii

white <- lm(approve ~ white, data = loanapp)
summary(white)
#ss and says whites have a 20% better chance of approval - deffo significant 

#iii

lpm <- lm(approve ~ hrat + obrat + loanprc + unem +male + married + dep + sch +
            cosign + chist + pubrec + mortlat1 + mortlat2 + vr + white, data = loanapp )
summary(lpm)
#white still very sign but impact has decreased 

#iv 

lpm_two <- lm(approve ~ hrat + obrat + obrat:white +loanprc + unem +male + married + dep + sch +
            cosign + chist + pubrec + mortlat1 + mortlat2 + vr + white, data = loanapp )
summary(lpm_two)
#yes, significant at 5% 

#v 
loanapp <- loanapp %>% mutate(obrat_av = obrat -32)
lpm_three <- lm(approve ~ hrat + obrat + obrat_av:white +loanprc + unem +male + married + dep + sch +
                cosign + chist + pubrec + mortlat1 + mortlat2 + vr + white, data = loanapp )
summary(lpm_three)

#coeff on white is now the race differential when obrat = 32 
#11.3% more likely to get a loan 
# the 95% confidence interval is about .113 ± 1.96(.020) and doesnt include zero 

#### C9 ####
#i 
eligible <- k401ksubs %>% filter(e401k == 1) %>% count()
eligible/count(k401ksubs)
#39%

#ii
lpm <- lm(e401k ~ male + inc + I(inc^2) + age + I(age^2), data = k401ksubs)
summary(lpm)

#iii
#no, not independent of income or age 
#eligibility prob increases with income The quadratic is n shaped, i.e. has a maximum
#turning point 
#Same story for age - prob of eligibility rises and then falls in age 
#gender is not significant 

#iv
lpm$fitted.values 
#can see there are zero fitted values greater than one, or negative
sum(lpm$fitted.values < 0 | lpm$fitted.values > 1)

#v
k401_fitted_vals <- data.frame(fitted_vals = lpm$fitted.values) %>% 
  mutate(rounded_vals = case_when(fitted_vals >= 0.5 ~ 1,
                                            fitted_vals < 0.5 ~ 0))

k401_fitted_vals %>% count(rounded_vals==1)

#vi
k401ksubs <- k401ksubs %>% mutate(fitted_vals = k401_fitted_vals$fitted_vals,
                                  eligibil_predics = k401_fitted_vals$rounded_vals)

k401ksubs <- k401ksubs %>% mutate(comparison = e401k - eligibil_predics)
k401ksubs_not_elig <- k401ksubs %>% filter(e401k == 0) 
k401ksubs_not_elig %>% filter(comparison==0) %>% count / count(k401ksubs_not_elig)
#82% are correctly predicted

k401ksubs_elig <- k401ksubs %>% filter(e401k == 1) 
k401ksubs_elig %>% filter(comparison==0) %>% count / count(k401ksubs_elig)
#39% are correctly predicted 

#vii
#no. The model is skewed towards saying people aren't eligible

#viii
lpm_two <- lm(e401k ~ male + inc + I(inc^2) + age + I(age^2) + pira, data = k401ksubs)
summary(lpm_two)
#not significant 

#### C10 ####
#i
basketball <- lm(points ~ guard + forward + exper + I(exper^2), data = nbasal)
summary(basketball)

#ii 
#dont include all three positions as that would induce perfect multicollinearity
#(these are all the possible positions, so a + b + c =1)
#this is the dummy variable trap 

#iii 
#yes, a guard scores more than a center. THe score 2.3 more points/game, and this is 
#highly significant 

#iv 
basketball_two <- lm(points ~ guard + forward + exper + I(exper^2) + marr, data = nbasal)
summary(basketball_two)
#not significant 

#v
basketball_three <- lm(points ~ guard + forward + exper + I(exper^2) + marr +
                       marr:exper + marr:I(exper^2), data = nbasal)
summary(basketball_three)
linearHypothesis(basketball_three, c("marr = 0","exper:marr = 0", "I(exper^2):marr = 0"))
#marriage not significant

#vi 
basketball_four <- lm(assists ~ guard + forward + exper + I(exper^2) + marr, data = nbasal)
summary(basketball_four)
#exper is much less important 
huxreg(basketball_two, basketball_four)

#### C11 ####
#i
data.frame(average = mean(k401ksubs$nettfa),
           stand_dev = sd(k401ksubs$nettfa),
           min = min(k401ksubs$nettfa),
           max = max(k401ksubs$nettfa))

#ii
net <- lm(nettfa ~ e401k, data = k401ksubs)
summary(net)
#e401k means more predicted net financial assets (18.9k)

#iii
net_two <- lm(nettfa ~ e401k + inc + age + I(inc^2) + I(age^2), data = k401ksubs)
summary(net_two)
#predicts 9.7k more assets, highly significant 

#iv
k401ksubs_avage <- k401ksubs %>% mutate(avage = (age - 41))
net_three <- lm(nettfa ~ e401k + inc + age + I(inc^2) + I(age^2) +
                e401k:(avage) + e401k:I(avage^2), data = k401ksubs_avage)
summary(net_three)


#e401k:avage is significant 



#### Chapter 8 questions ####
####C1####
#i
#Var(u|male) = B0 +B1male, where female variance is B0

#ii
sleep <- lm(sleep~ totwrk + educ + age + I(age^2) + yngkid + male, data = sleep75)
residuals_sleep <- resid(sleep)^2
variance_test <- lm(residuals_sleep ~ male,data = sleep75)
summary(variance_test)

#ii
#no, the variance of not statistically significantly differnet between men and 
#women 

#### C2 ####
#i
price <- feols(price ~ lotsize + sqrft +bdrms, data = hprice1, vcov = "hetero")
summary(price)
#lotsize is no longer ss

#ii
logprice <- feols(log(price) ~ log(lotsize) + log(sqrft) +bdrms, data = hprice1, vcov = "hetero")
summary(logprice)

#iii
#using log of dependant variable can remove heteroskedacity 

#### C3 ####
#skipped - I dont think applying the full white test is worth the time 

#### C4 ####

#i
voteA <- feols(voteA ~ prtystrA + democA + log(expendA)+ log(expendB), data = vote1)
residuals <- resid(voteA)^2
hetero_test <- lm(residuals~ prtystrA + democA + log(expendA)+ log(expendB), data = vote1)
summary(hetero_test)
#R^2 = 0.03 as there is weak evidence of heterok=skadacity in 2 variables
# Remember, this is how OLS works: the estimates β are chosen to make the residuals be
#uncorrelated in the sample with each independent variable (as well as have zero sample average).

#ii

BP_test <- linearHypothesis(hetero_test, c("prtystrA = 0", "democA = 0", "log(expendA) = 0",
                                     "log(expendB) = 0"))
BP_test
#there is some evidence of heteroskadacity, but not significant at 5%

#iii
#using the special case of the white test (using fitted values)
fitted_vals <- voteA$fitted.values
fitted_vals_sqrd <- voteA$fitted.values^2

white_test <- lm(residuals~ fitted_vals + fitted_vals_sqrd)
linearHypothesis(white_test, c("fitted_vals = 0", "fitted_vals_sqrd = 0"))
#slighly less evidence of heteroskadacity now 

#### C5 ####
#i
selected <- lm(sprdcvr ~ 1, data = pntsprd)
summary(selected)
(0.51537  - 0.5)/0.02127
#0.72 is not significant at the 5%

#ii
pntsprd %>% filter(neutral == 1) %>% count
#35

#iii
spread_ols <- feols(sprdcvr~ favhome + neutral + fav25 + und25, data = pntsprd)
spread_robust <- feols(sprdcvr~ favhome + neutral + fav25 + und25, data = pntsprd, 
                       vcov = "hetero")
etable(spread_ols, spread_robust)
#neutral is most significant 

#iv 
#if all coefficients are zero, this means they do not effect the dependent variable.
#hence they do not belong in the population model. So the mean nor the variance of the 
#dependant vaeriable depends on the independant variables 

#v
linearHypothesis(spread_ols, c("favhome = 0", "neutral = 0", "fav25 = 0", "und25 = 0"))
#cant reject the null - they are jointly insignificant so there should be no heteroskedacity 
#testing with a BP test 

residuals_sqrd <- resid(spread_ols)^2

BP_test <- lm(residuals_sqrd ~ favhome + neutral + fav25 + und25, data = pntsprd)
linearHypothesis(BP_test, c("favhome = 0", "neutral = 0", "fav25 = 0", "und25 = 0"))
#there is evidence of heteroskaacity here ???

#v 
#no, jointly insignificant and explanatory power is very low 

#### C6 ####
#i
crime <- lm(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86, data = crime1)
summary(crime)
range(crime$fitted.values)

#ii
fitted_vals <- crime$fitted.values 
crime1 <- crime1 %>% mutate(est_variance = fitted_vals*(1-fitted_vals),
                            multiplier = 1/est_variance, 
                            weighted_pcnv = pcnv*multiplier)
h <- fitted_vals*(1-fitted_vals)
weights <- 1/h

WeLeSq <- lm(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86, data = crime1,
          weights = weights)
summary(WeLeSq)

#iii


#### C7 ####
#i
approve <- feols(approve ~ white + hrat + obrat + loanprc + unem + male + married + 
                   dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr,
                 data = loanapp, vcov = "hetero")
approve_OLS <- feols(approve ~ white + hrat + obrat + loanprc + unem + male + married + 
                   dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr,
                 data = loanapp)
etable(approve, approve_OLS)

tibble(robust_upper_int = 0.1288 + 1.96* 0.0259,
           robust_lower_int = 0.1288 - 1.96* 0.0259,
           OLS_upper_int = 0.1288 + 1.96* 0.0197,
           OLS_lower_int = 0.1288 - 1.96* 0.019)
#ii
range(approve_OLS$fitted.values)
#some are more than one. So would have to manipluate to ensure all are between zero 
#and one before using WLS (hi cannot be negative)

#### C8 ####
#i
#ols regression 
colgpa <- lm(colGPA ~ hsGPA +ACT + skipped + PC, data = gpa1)
summary(colgpa)
resid(colgpa)

#ii
#testing for hetrosk by regressing residuals on fitted and fitted squared (white test)
fitted <- colgpa$fitted.values
fitted_sqrd <- colgpa$fitted.values^2
resid_squared <- resid(colgpa)^2
white_test <- lm(resid_squared ~ fitted + fitted_sqrd)
linearHypothesis(white_test, c("fitted = 0", "fitted_sqrd = 0"))
#there is evidence of heteroskedacity 

#iii
#resolving heteroskedacity using weighted least squares. Making sure all fitted vals 
#are positive before using as weights 
min(white_test$fitted.values)
wghts <- 1/(white_test$fitted.values)

WLS <- lm(colGPA ~ hsGPA +ACT + skipped + PC, data = gpa1,
          weights = wghts)
summary(WLS)

#estimates of skipped and PC are very similar to OLS estimates, and both significant 
#in both cases 

#iv
#resolving heterosked with robust SEs 
WLS_robust <- feols(colGPA ~ hsGPA +ACT + skipped + PC, data = gpa1,
          weights = wghts, vcov = "hetero")
summary(WLS_robust)

#SE very similar when using robust SE vs when using weights. All same variables are
#still significant 

#### C9 ####
#i
cigs <- lm(cigs~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn, 
              data = smoke)
summary(cigs)

#ii
#first set in forming WLS is regressing logs of the squared residuals on the independent 
#variables
resid_cigs_sqrd <- resid(cigs)^2
WLS_regression_one <- lm(log(resid_cigs_sqrd) ~ log(income) + log(cigpric) + educ + age +
                     I(age^2) + restaurn, data = smoke)

summary(WLS_regression_one)

#then use the fitted values from this as weights in the original regression 
weights <- 1/exp(fitted.values(WLS_regression_one))

WLS_regression_two <- feols(cigs~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn, 
                         data = smoke, weights = weights)
summary(WLS_regression_two)

unweighted_residuals <- resid(WLS_regression_two)
unweighted_fitted <- fitted.values(WLS_regression_two)

#iii
weighted_residuals <- (unweighted_residuals/ sqrt(weights))^2
weighted_fitted <- unweighted_fitted/ sqrt(weights)

white_test <- lm(weighted_residuals ~ weighted_fitted + I(weighted_fitted^2))
summary(white_test)
linearHypothesis(white_test, c("weighted_fitted = 0", "I(weighted_fitted^2) = 0"))
#evidence of heteroskedacity in the weighted equation 

#iv 
#the weighting didnt remove the heteroskedactiy. So our WLS was misspecified 

#v 
WLS_robust <- feols(cigs~ log(income) + log(cigpric) + educ + age + I(age^2) + restaurn, 
                            data = smoke, weights = weights, vcov = "hetero")
summary(WLS_robust)
#see large differences in SE between this and WLS_regression_two, showing that weighting 
#didnt do the trick. Except restaurn all other SEs are alot bigger 

#### C10 ####
#i
OLS_401 <- feols(e401k ~ inc + I(inc^2) + age + I(age^2) + male, data = k401ksubs)
robust_401 <- feols(e401k ~ inc + I(inc^2) + age + I(age^2) + male, data = k401ksubs,
                    vcov = "hetero")
etable(OLS_401, robust_401)
#no important differences 

#ii
#expand out the condiitonal variance (where E(u^2| x) is conditional variance) and 
#see the coefficients are 0, 1 and -1. 

#iii
#performing white test for heteroskedacity
#first, regressing the errors squared on the fitted vals and fitted vals sqrd
#either OLS or robust; they are identical in fitted vals and residuals 

residuals_sqrd <- resid(OLS_401)^2
fitted_vals <- fitted.values(OLS_401)
fitted_vals_sqrd <- fitted.values(OLS_401)^2

resid_on_fitted <- lm(residuals_sqrd ~ fitted_vals + fitted_vals_sqrd)
summary(resid_on_fitted)
#coefficients are more or less as predicted  

#iv
#ensuring fitted vals are all between zero and one 
range(fitted_vals)
#now using 1/(yhat(1 - yhat)) as weights 
weights <- 1/(fitted_vals*(1-fitted_vals))

weighted_OLS <- feols(e401k ~ inc + I(inc^2) + age + I(age^2) + male, 
                                 data = k401ksubs, weights = weights)
etable(weighted_OLS, OLS_401)
#no important differences 


#### Chapter 13 ####
#C1 
#i
wage <- feols(kids~ educ + age + I(age^2) + black + east + northcen + west + 
                farm + othrural + town + smcity + y74 + y76 + y78 + y80 + y82 + y84,
              data = fertil1)
linearHypothesis(wage, c("farm = 0", "othrural = 0", "town = 0", "smcity = 0"), 
                 test = "F")

#ii
linearHypothesis(wage, c("east = 0", "northcen = 0", "west = 0"), 
                 test = "F")
#iii
residuals_sqrd <- resid(wage)^2
#regressing resid squard on years 
resid_on_years <- lm(residuals_sqrd ~ y74 + y76 + y78 + y80 + y82 + y84, data = fertil1)
#F test on regressors 
linearHypothesis(resid_on_years, c("y74 = 0", "y76 = 0", "y78 = 0", "y80 = 0", 
                                   "y82 = 0", "y84 = 0"))
#yes, evidence of heteroskedacity - we ought to compute robust statistics 

#iv 
with_interactions <- feols(kids~ educ + age + I(age^2) + black + east + northcen + west + 
                             farm + othrural + town + smcity + y74 + y76 + y78 + y80 + y82 + y84+ 
                             y74:educ + y76:educ + y78:educ + y80:educ + y82:educ + y84:educ ,
                           data = fertil1)
summary(with_interactions)
#these terms represent the impact of education of feritility changing over years 

linearHypothesis(with_interactions, c("educ:y74 = 0", "educ:y76 = 0", "educ:y78 = 0", 
                                   "educ:y80 = 0","educ:y82 = 0", "educ:y84 = 0"))
#they're not jointly significant because we're testing insignificant realtionships 
#with significant ones 

#### C2 ####
#i
#its return to education for a male with no education - the impact of gender and 
#education is captured elsewhere in the equation 

#ii
#to find % increase for a male with 12 years of education, we replace y85:educ 
#with y85:(educ-12) (centering)

cps78_85 <- cps78_85 %>% mutate(educ_centered = educ-12)
twelve_educ <- lm(lwage ~ y85 + educ + y85:educ_centered + exper + I(exper^2) + union + female + 
                    y85:female, data = cps78_85)
summary(twelve_educ)
#see the coefficient on the intercept as the impact on wage of 12 years of education
#its about 33.9% here with se of 0.034
#95% CI is given by  33.9 ± 1.96(3.4)


#iii
cps78_85 <- cps78_85 %>% mutate(wage = exp(lwage),
                                rwage = wage/1.65,
                                lrwage = log(rwage))
rwage <- lm(lrwage ~ y85 + educ + y85:educ + exper + I(exper^2) + union + female + 
                             y85:female, data = cps78_85)
summary(rwage)

#iv
#total sum of squares different 

#v
#union particip 1978 
cps78_85 %>% filter(year == 78, union == 1) %>% count()
168/cps78_85 %>% filter(year == 78) %>%  count()
#30.5%

#union particip 1985
cps78_85 %>% filter(year == 85, union == 1) %>% count()
96/cps78_85 %>% filter(year == 85) %>%  count()
#18%

#vi 
union <- lm(lwage ~ y85 + educ + y85:educ + exper + I(exper^2) + union +
         union:y85 + female + y85:female, data = cps78_85)
summary(union)
#in 78, union meant 20% higher wage
#in 85, it meant 0.39% less wage - not significant though
#the effect of being in a union didnt change between years - its about 20% in both years 

#vii 
#no, could have lower participation but wage differential not changing over time 
#perhaps if the union bargained for multiple years when it had more power 

#### C3 ####
#i
#the sign would be positive 
#if B1 is positive, then the inicinerator was built in an area of lower home vals 
#anyway 

#ii
price <- lm(log(price) ~ y81+ log(dist) + y81:log(dist), data = kielmc)
summary(price)
#the incinerator being built had no impact on the price of houses

#iii

price_two <- lm(log(price) ~ y81+ log(dist) + y81:log(dist) +
                  age + I(age^2) + rooms + baths + lintst + lland + larea, data = kielmc)
summary(price_two)
#still no impact of the incinerator 

#iii
#it was not the distance per se but the characteristics of the houses that was 
#driving the price change as distance increased 


#### C4 ####
#i
duration <- lm(log(durat)~ afchnge + highearn + afchnge:highearn +
                 male + married + head + neck + upextr + trunk + 
                 lowback + lowextr + occdis + manuf + construc, data = injury)
summary(duration)
#interaction term becomes more significant. A strong response in high earners following 
#the change in rules 

#ii
#The R^2 is low, indicating poor predictive power of the duration. But the estimates
#are by no means useless 

#iii
injury_mi <- injury %>% filter(mi == 1)

duration_mi <- lm(log(durat)~ afchnge + highearn + afchnge:highearn, 
                  data = injury_mi)
summary(duration_mi)
#michigan estimate on the interaction term is very similar, however not ss due to 
#large SE, due to the smaller sample size 


#### C5 ####

#i
rent <- lm(log(rent)~ y90 + log(pop)+ log(avginc) + pctstu, data = rental)
summary(rent)
#90 variable shows that rents are just higher in 1990, irrespective of income and 
#student concentration 
#in terms of pctstu, if the student % rises 10%, the rents rise by 5%. very ss. 

#ii
#test for hetero: 
residuals_sqrd <- resid(rent)^2

hetero_test_one <- lm(residuals_sqrd ~ y90 + log(pop)+ log(avginc) + pctstu, data = rental)
linearHypothesis(hetero_test_one, c("y90 = 0", "log(pop) = 0", "log(avginc) = 0",
                                    "pctstu = 0"))
#evidence of heteroskedacticy - so the se are not valid 

#didnt need to do this - if ai is in the error, then ommited variable bias if the 
#fixd effect is correlated with any explanatory variables 

#iii
rental_fd <- rental %>% mutate(pop_lag = log(pop) - log(lag(pop)),
                               avginc_lag = log(avginc) - log(lag(avginc)),
                               pctstu_lag = pctstu - lag(pctstu),
                               rent_lag = log(rent) - log(lag(rent)))

rent_fd = lm(rent_lag~ pop_lag+ avginc_lag + pctstu_lag, data = rental_fd)
summary(rent_fd)
#this is giving the wrong results but I cant work out why 

#### C6 ####
#i
crime3_fd <- crime3 %>% group_by(district) %>%  
  mutate(lcrime_fd = lcrime - lag(lcrime),
                               d78_fd = d78 - lag(d78),
                               clrprc1_fd = clrprc1 - lag(clrprc1),
                               clrprc2_fd = clrprc2 - lag(clrprc2),
                               avg_clr_fd = avgclr - lag(avgclr))

crime <- lm(lcrime_fd~ d78_fd + clrprc1_fd + I(clrprc2_fd+clrprc1_fd), data = crime3_fd)
summary(crime)
#as clrprc1 is not ss from zero, we cannot reject B1 = B2 

#iii
crime_adjusted <- lm(lcrime_fd ~ avg_clr_fd, data = crime3_fd)
summary(crime_adjusted)
#again, not getting correct estimate from this- fixed this - I needed to group 
#by district 


#### C7 ####
#i
gpa <- lm(trmgpa ~ spring + sat+ hsperc+ female+ black + white+ frstsem+
            tothrs+ crsgpa +  season, data = gpa3)

summary(gpa)
#season is not significant. Indicates sport season - after controlling for these 
#variables, the sport being in season doesnt make a difference 

#ii
#there will be omitted variable bias. Ability and season will be correlated

#iii 
#static variables will drop out; sat, female, hsperc, black, white
gpa3_fd <- gpa3 %>% mutate(ID = rep(1:(nrow(gpa3)/2), each = 2))
gpa3_fd<- gpa3_fd %>% group_by(ID) %>%  
                            mutate(spring_fd = spring - lag(spring),
                             frstsem_fd = frstsem - lag(frstsem),
                             tothrs_fd = tothrs - lag(tothrs),
                             crsgpa_fd = crsgpa - lag(crsgpa),
                             season_fd = season - lag(season),
                             trmgpa_fd = trmgpa - lag(trmgpa)
                             )
gpa_fd <- lm(trmgpa_fd ~ frstsem_fd + tothrs_fd+ crsgpa_fd + season_fd,
             data = gpa3_fd)

summary(gpa_fd)
#season is still not significant, but the effect has become larger 

#iv 
#anything that might affect grade that could change between a term
 #relationship for exampple, or course load 

#### C8 ####
#i 
vote2 <- vote2 %>% 
  mutate(inc_share_fd = incshr90 - incshr88,
         linexp_fd = linexp90 - linexp88,
         lchexp_fd = lchexp90 - lchexp88,
         vote_fd = vote90 - vote88)
         
                          

vote <- lm(vote_fd~ linexp_fd +lchexp_fd +  inc_share_fd, data = vote2)
summary(vote)
#only inc_share_fd is significant 

#ii 
linearHypothesis(vote, c("linexp_fd = 0", "lchexp_fd=0"))
#not jointly significant even at 20%

#iii
vote_simple <- lm(vote_fd~ inc_share_fd, data = vote2)
summary(vote_simple) 
huxreg(vote, vote_simple)
#inc share now significiant at 1%
#if the change in spending cahanges 10pp, the vote share is predicted to increase
#by 2.2% (the coeff shows impact of 1pp change)

#iv 
vote2 <- vote2 %>% filter(rptchall == 1)
vote_rptchall <- lm(vote_fd~ inc_share_fd, data = vote2)
summary(vote_rptchall)
#now, once we control for repeat challenger, inc share is not significant at any 
#sensible level 

#### C9 ####
#i
crime4 <- crime4 %>% group_by(county) %>% 
                       mutate(lwcon_fd = lwcon - lag(lwcon),
                            lwtuc_fd = lwtuc - lag(lwtuc),
                            lwtrd_fd = lwtrd - lag(lwtrd),
                            lwfir_fd = lwfir - lag(lwfir),
                            lwser_fd = lwser - lag(lwser),
                            lwmfg_fd = lwmfg - log(lwmfg),
                            lwfed_fd = lwfed - lag(lwfed),
                            lwsta_fd = lwsta - lag(lwsta),
                            lwloc_fd = lwloc - lag(lwloc),
                            lprbarr_fd = lprbarr - lag(lprbarr),
                            lprbconv_fd = lprbconv - lag(lprbconv),
                            lprbpris_fd = lprbpris - lag(lprbpris),
                            lavgsen_fd = lavgsen - lag(lavgsen),
                            lpolpc_fd = lpolpc - lag(lpolpc),
                            lcrmrte_fd = lcrmrte - lag(lcrmrte),
                            d83_fd = d83 - lag(d83),
                            d84_fd = d84 - lag(d84),
                            d85_fd = d85 - lag(d85),
                            d86_fd = d86 - lag(d86),
                            d87_fd = d87 - lag(d87))

crime_fd <- lm(lcrmrte_fd ~ 
                 d83_fd +d84_fd + d85_fd + d86_fd +d87_fd + 
                 lpolpc_fd + lavgsen_fd + lprbpris_fd + lprbconv_fd + lprbarr_fd +
                 lwloc_fd + lwsta_fd + lwfed_fd + lwmfg_fd + lwser_fd + 
                 lwfir_fd + lwtrd_fd + lwtuc_fd + lwcon_fd, data = crime4)

summary(crime_fd)

#ii
#signs on wages differ. Would expect that crime rate would fall as wages rise, 
# so signs are expected to be negative. 
#F test on wages is not s.s. 

#### C10 ####

#i
jtrain <- jtrain %>% group_by(fcode) %>% 
  mutate(d88_fd = d88 - lag(d88),
         d89_fd = d89 - lag(d89),
         grant_fd = grant - lag(grant),
         lagged_grant_fd = grant_1 - lag(grant_1),
         lemploy_fd = lemploy - lag(lemploy),
         hrsemp_fd = hrsemp - lag(hrsemp))

training <- lm(hrsemp_fd ~
                 d89_fd + grant_fd + lagged_grant_fd + lemploy_fd +
                 hrsemp_fd, data = jtrain)
summary(training)
length(training$residuals)
#running with both d88_fd and d89_fd produces errors as then we have a control for 
#1987 - 1988 and 1988 - 1989 grant - leaving one out to be represented by the 
#intercept

#there are 157 firms in all. If we had data on both 1987 - 1988 and 1988 - 1989 
#grant status for each, we would have 314 observations 

#ii
#grant says that training hours a grant means that the grant means on average 
# a firm with a grant spent 32.6 more hours training per employee.
#it is significant at the 1% level 

#iii 
#last years grant has no impact on this years training. 
#yes, that is surprising. They obviously spent it all in the first year

#iv 
#large firms do train employees more. 
#a 10 pp change in firm size means that training time increases by 0.07hrs per employee
#this is a small effect and its not significant 

#### C11 ####
#i

#change math4 = B1*change(lrexpp). change(math)/ 100 = B1/100*100*change(lrexpp) 
#which equals B1/100 * %_change(lrexpp) 
#so if %change is 10, the change in math 4 is 100 


#ii 
mathpnl <- mathpnl %>% group_by(distid) %>% mutate(math4_fd = math4 - lag(math4),
                              y94_fd = y94 - lag(y94),
                              y95_fd = y95 - lag(y95),
                              y96_fd = y96 - lag(y96),
                              y97_fd = y97 - lag(y97),
                              y98_fd = y98 - lag(y98),
                              lrexpp_fd = lrexpp - lag(lrexpp),
                              lenrol_fd = lenrol - lag(lenrol),
                              lunch_fd = lunch - lag(lunch)
                              )

#here, we dont actually difference the year dummy.
#differncing the year dummy is useful to remove any time invariant componetn 
#of the year (e.g. if there was a big political event that impacted all groups 
#in the year). Not differecing them lets us observe the direct effect of the 
#year 

math_fd <- lm(math4_fd ~ y94 + y95 + y96 + y97 + y98 + lrexpp_fd + 
                lenrol_fd + lunch_fd, data = mathpnl)
summary(math_fd)
#lrexpp_fd estimate says if spending increases by 10pp, then perc of math passed 
#decreases by 0.34 pp 

#iii 

#lagged rexpp is already included in the data as lrexpp_1.
mathpnl <- mathpnl %>% group_by(distid) %>% 
  mutate(lrexpp_1_fd = lrexpp_1 - lag(lrexpp_1))

math_fd_2 <- lm(math4_fd ~ y95 + y96 + y97 + y98 + lrexpp_fd + 
                lenrol_fd + lunch_fd + lrexpp_1_fd, data = mathpnl)
summary(math_fd_2)
#agged is highly significant and positive. 
# it says a 10pp increase in spending last year means a increase in students passing 
# of 1.1 pp 

#iv
coeftest(math_fd_2, vcov. = vcovHC, type = "HC1")
#lagged spending still significant 
#SE on both are much higher 

#v 
coeftest(math_fd_2, vcov. = vcovHAC, type = "HAC1")

#### CHAPTER 14 QUESTIONS ####

#### C1 ####

#i
rent <- lm(lrent ~ y90 + lpop + lavginc + pctstu, data = rental )
summary(rent)
#90 says 1990 is very significant (this is 1980 and 1990 data). May be inflation 
#pctstu estimate says a 10pp increase in students will increase rent by 
# 5%

#ii
#if there is correlation between a and the explanatory variables then we have some 
#ommitted variable bias here 

#iii

rental_fd <- rental %>% group_by(city) %>% mutate(pop_lag = log(pop) - log(lag(pop)),
                               avginc_lag = log(avginc) - log(lag(avginc)),
                               pctstu_lag = pctstu - lag(pctstu),
                               rent_lag = log(rent) - log(lag(rent)))

rent_fd = lm(rent_lag~ pop_lag+ avginc_lag + pctstu_lag, data = rental_fd)
summary(rent_fd)

#pctsu is still significant, but the effect has fallen alot 

#iv
rent_fe <- feols(lrent ~ y90 + lpop + lavginc + pctstu| city, data = rental)
summary(rent_fe)
#same as above 

#### C2 ####

#i
crime <- feols(lcrmrte ~ d82 +d83 + d84 + d85 + d86 + d87 + lprbarr + lprbconv + 
                 lprbpris + lavgsen + lpolpc |county, data = crime4)
summary(crime)
#nothing dramatically different to 13.9

#ii
crime_fe <- feols(lcrmrte ~ 
                 d82 + d83 +d84 + d85 + d86 +d87 + 
                 lpolpc + lavgsen + lprbpris + lprbconv + lprbarr +
                 lwloc + lwsta + lwfed + lwmfg + lwser + 
                 lwfir + lwtrd + lwtuc + lwcon| county, data = crime4)
summary(crime_fe)

etable(crime, crime_fe)
#didnt change the criminal justice variables in any meaningful way 

#iii
#would expect all wage variables to be negative, which theyre not. 
#the wage variables are jointly significant 


#### C3 ####
#i
grant <- feols(hrsemp ~ d88 + d89 + grant + grant_1 + lemploy | fcode, 
               data = jtrain)
summary(grant)
#390 obsv. 81 dropped as there is only one year of data (I think?)

#ii
#grant says that if a firm for a grant, it does 34.2 more hours of training per 
#employee in that year. Very signiifant 

#iii 
#yes, it is surprising. Firms spend all their grant money in the same year 

#iv 
#yes, large firms do less training. If a firm is 10% larger, they do 0.17 less hours.
#but this is tiny and insignificant 

#### C4 ####
#i 
#ct would be the difference between subsequent numbers, which subtract to one 
#each time 

#ii 
ezunem <- ezunem %>% group_by(city) %>% mutate(luclms_fd = luclms - lag(luclms),
                            ez_fd = ez - lag(ez))
zones <- feols(luclms_fd ~ ez_fd| city, data = ezunem, vcov = "iid")
summary(zones)
#yes, it is at the 1%

#iii
zone_year <-feols(luclms_fd ~ ez_fd +d81 +d82 + d83 + d84 + d85 +d86 +
                   d87 +d88| city, data = ezunem, vcov = "iid" )
summary(zone_year)
#ez is still significant but only at 5% now 




##### Skipping to chapter 10 (time series) ##### 
#### 10.7 ####
intdef$y79 <- ifelse(intdef$year > 1979, 1, 0)
tbill <- lm(i3 ~ inf + def + y79, data = intdef)
summary(tbill)
#there is strong evidence that the interest rate equation changed after 1979.

#### 10.8 ####
#i
dumping <- lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6 +
              t, data = barium)
summary(dumping)
#only the trend is significant

#ii
linearHypothesis(dumping, c("lchempi = 0", "lgas=0", "lrtwex=0","befile6=0",
                            "affile6=0","afdec6=0"))
#cannot reject the null here; theyre jointly insignificant 
#only the timetrend helps to explain 

#iii
dumping_2 <- lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6 +
                t + feb +mar +apr +may + jun +jul + aug + sep + oct + nov + dec,
                data = barium)
summary(dumping_2)
#no major changes here 

#10.9
puerto_rico_1 <- lm(lprepop ~ lmincov + lusgnp +t ,
                    data = prminwge)
puerto_rico_2 <- lm(lprepop ~ lmincov + lusgnp + lprgnp +t ,
                  data = prminwge)

huxreg(puerto_rico_1,puerto_rico_2)
#its significant and the coeff says that a 1% change in PR GNP changes the employ/popul ratio
# by 0.28%
#min wage effect becomes larger 

#### 10.10 ####
fertil <- lm(gfr ~ pe + pe_1 + pe_2 + ww2 + pill, data = fertil3)
summary(fertil)

fertil3 <- fertil3 %>% mutate(one_lag_diff = pe_1 - pe,
                              two_lag_diff = pe_2 - pe)

fertil_2 <- lm(gfr ~ pe + one_lag_diff + two_lag_diff + ww2 + pill, data = fertil3)
summary(fertil_2)
#see s.e. on pe for the se of LRP 

#### 10.11 ####
#i
indiana <- lm(luclms ~ year + feb  +mar +apr+ may+ jun+ jul +aug +sep +oct +nov+
              dec, data = ezanders)
summary(indiana)
#negative time trend over the period. Each year, 16.6% less claims. 
#yes evidence of seasonality 
#time trend coeff way out for some reason 

#ii
ezanders$ez <- ifelse(ezanders$year >= 1984,1,0)
indiana_2 <- lm(luclms ~ year + feb  +mar +apr+ may+ jun+ jul +aug +sep +oct +nov+
                dec + ez, data = ezanders)
summary(indiana_2)
100*(exp(-0.50803)-1)
#reduced claims 39.8%

#iii
#nothing else big happened in 1984 that mightve also been reducing the unemploy
#claims, whilst being pos. correlated with ez (no ommited variables)
# hopefully controlling for seasonablity and time trend is sufficient 

#### 10.12 #### 
#i 
fertil3 <- fertil3 %>% mutate(tsqrd = t^2)
births <- lm(gfr ~ t + tsqrd, data = fertil3)
birth_resids <- births$residuals
#this takes time trend out 

#ii
births_two <- lm(birth_resids ~ pe  + ww2 + pill + t + tsqrd,
                 data = fertil3)
summary(births_two)
#R^2 here is 0.6 vs 0.7 before. 
#still have decent amount of explanatory power after netting out trend 

#iii
fertil3 <- fertil3 %>% mutate(tcube = t^3)
birth_three <- lm(gfr ~ pe  + ww2 + pill + t + tsqrd + tcube, data = fertil3)
summary(birth_three)
#yes, very statisticaly signif. Curve fitting? 

#### 10.13 ####
#i
consumption <- lm(gc ~ gy, data = consump)
summary(consumption)
#this says that a 1 pp change in income leads to a 0.57 pp change in consumption
#income is highly s.s.

#ii
consumption_two <- lm(gc ~ gy +gy_1, data = consump)
summary(consumption_two)
#insignificant - weak evidence for lagged income change having impact on 
#consumption 

#iii
consumption_three <- lm(gc ~ gy + r3 + r3_1 + r3_2, data = consump)
summary(consumption_three)
#very weak evidence real interest rate effects consumption. But we would expect 
#it to work with lags, anyway ... or not running the regression above with the 
#lagged rates 

#### 10.14####
#i
fertil <- lm(gfr ~ pe + pe_1 + pe_2 + pe_3 + pe_4 + ww2 + pill, data = fertil3)
summary(fertil)
linearHypothesis(fertil, c("pe_3 = 0","pe_4 = 0" ))
#jointly very insignificant 

#ii
fertil3 <- fertil3 %>% mutate(pe_1LRP = pe_1- pe,
                              pe_2LRP = pe_2 - pe,
                              pe_3LRP = pe_3 - pe,
                              pe_4LRP = pe_4 - pe)
fertil3_LRP <- lm(gfr ~ pe + pe_1LRP + pe_2LRP+ pe_3LRP+ pe_4LRP + ww2+ pill, 
                  data = fertil3)
summary(fertil3_LRP)
fertil3_LRP$coefficients[2]
#LRP = 0.12419, se = 0.029572

fertil3_LRP_original <- lm(gfr ~ pe + pe_1LRP + pe_2LRP + ww2+ pill, 
                  data = fertil3)
summary(fertil3_LRP_original)
fertil3_LRP_original$coefficients[2]
#LRP = 0.1007191, se = 0.02980

#LRP is larger with more years as expected 

#iii
fertil3 <- fertil3 %>%  mutate(a = pe + pe_1 + pe_2 + pe_3 + pe_4,
                               b = pe_1 + 2*pe_2 + 3*pe_3 + 4*pe_4,
                               c = pe_1 + 4*pe_2 + 9*pe_3 + 16*pe_4)
fertil_polynomial <- lm(gfr ~ a + b + c + ww2 + pill, data = fertil3)
summary(fertil_polynomial)
#LRP = a + (a + b + c) + (a + 2b + 4c) + (a + 3b + 9c) = 0.135 


#### 10.15 ####
#i
#would expect B1 to be postive (more production should mean more earnings),
#and B2 to be negative (higher risk free rate would adjust price of risky assets 
#downward and slow the economy; however may be some reverse causaility here(rates higher
#when the economy booming) so that would indicate a higher B2)

#ii
return <- lm(rsp500 ~ pcip + i3, data = volat)
summary(return)
#pcip is positive but little evidence of an effect on returns 
# i3 is negative and significant. Say if T bill rate changes by 1 pp, return drops 
#by 1.36 pp 

#iii
#i3 is s.s. at 2%

#iv
#current 3month Tbill rate reduces the return. But this is not known in advance of 
#the period either. 

#### 10.16 ####
#i 
cor(intdef$inf, intdef$def)
#pos correlation betweeen inflation and defecit 

#ii
tbill <- lm(i3 ~ inf + inf_1 + def + def_1, data = intdef )
summary(tbill)

#iii
#LRP = 0.3426 + 0.3820 
0.3426 + 0.3820 
# = 0.72 - this is quite a lot larger than 0.61 as presented before 

#iv
linearHypothesis(tbill, c("def_1 = 0", "inf_1=0"))
#theyre jointly very significant in explaining T bill rates 

#getting quite different reults to answers here and not sure why 

#### 10.17 ####
#i
#beltlaws in 1986 Jan
#speed law was from 1997 May

#ii
accidents <- lm(ltotacc ~ t + feb + mar + apr + may + jun + jul +aug + sep +
                  oct + nov + dec, data = traffic2)
summary(accidents)
#time trends says each month, total accidents increase by 0.275% 
#evidence of seasonality and the F statistic for joint significance 
#of the monthly dummies is F = 5.15 (very significant)

#iii
accidents_two <- lm(ltotacc ~ t + feb + mar + apr + may + jun + jul +aug + sep +
                  oct + nov + dec + wkends + unem + spdlaw + beltlaw, data = traffic2)
summary(accidents_two)
#unempl says less unemployment leads to more traffic accidents 
#could mean more people in cars commuting etc. 

#iv 
#spdlaw says the spdlaw led to 5% less crashes - but the speed went up! could be 
#more caution, or a confounder. 
#and beltlaw led to 9.5% more crashes. Could be that people felt safer with belts so took more risks 

#v 
mean(traffic2$prcfat)
#0.88% led to at leads one fatality. Seems about right yes. 

#vi 
accidents_three <- lm(prcfat ~ t + feb + mar + apr + may + jun + jul +aug + sep +
                      oct + nov + dec + wkends + unem + spdlaw + beltlaw, data = traffic2)
summary(accidents_three)
#speed law now highly significant and positive. Says that when the speed went up, 
#the percentage of accidents resulting in a death went up by 0.067pp. 
#beltlaw says that when seatbelts mandated, the perpercentage of accidents 
#resulting in a death went down by 0.0295 pp 





