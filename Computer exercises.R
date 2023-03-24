library(wooldridge)
library(fixest)
library(tidyverse)
library(plotrix)
library(car)
library(lmtest)
library(huxtable)

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
