rm(list = ls()) # This line removes variables from another program 
cat("\014") # Clears the console  screen

library(plm)  # This is the package for linear panel data models
library(stargazer)

#Homework 3:
library(haven)
cell <- read_dta("ECON861_Baier/Hm_3/Cell_phones.dta")
View(cell)

#generate panel data frame:
cell.p <- pdata.frame(cell,index=c("state","year"))
pdim(cell.p) # Provides details of panel (N,T, balanced, N*T)'

deaths_bmiles <- cell.p$DeathsPerBillionMiles

# a. Pooled OLS
summary(plm(deaths_bmiles ~ cell_ban + text_ban , data=cell.p , model="pooling"))

#b. State FE that causes endogeneity?
#c.Estimate with one-way FE
summary(plm(deaths_bmiles ~ cell_ban + text_ban , data=cell.p , model="within"))

#d.Year FE that causes endogeneity ? 
#e. Estimate with two-way FE
summary(plm(deaths_bmiles ~ cell_ban + text_ban ,data=cell.p,model="within", effect = "twoways"))
summary(plm(deaths_bmiles ~ cell_ban + text_ban + factor(year), data=cell.p,model="within"))

#f. With additional controls
summary(plm(deaths_bmiles ~ cell_ban + text_ban + cell_per10thous_pop + urban_percent ,data=cell.p,model="within", effect = "twoways"))
summary(plm(deaths_bmiles ~ cell_ban + text_ban + factor(year) + cell_per10thous_pop + urban_percent ,data=cell.p,model="within", effect = "twoways"))

# Question 2:
# a. 