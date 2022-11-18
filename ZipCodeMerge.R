summary(Zip$Zip_Code)
summary(VIRegion$RegionID)

library(tidyverse)
VI_New = VI %>%
  cbind(RegionName = rownames(VIMonth)) %>% 
  pivot_longer(cols = starts_with("X"),names_to = "yr/mon",values_to = "VI") 

RI_New = RI %>% 
  cbind(RegionName = rownames(RI)) %>% 
  pivot_longer(cols = starts_with("X"),names_to = "yr/mon",values_to = "RI")


VI_RI = merge(RI_New,VI_New)

VI_RI_New = VI_RI %>% mutate(
  Year = substr(`yr/mon`, 2, 5),
  Month = substr(`yr/mon`, 7, 8),
) %>% select(-`yr/mon`)

VI_RI_New$Year = as.integer(VI_RI_New$Year)
VI_RI_New$Month = as.integer(VI_RI_New$Month)

VI_RI_Mor = VI_RI_New %>% 
  left_join(Mortgage, by=c("Year"="Year"))
names(VI_RI_Mor)[13] = "MortgageRate"
VI_RI_Mor$RegionName <- as.numeric(VI_RI_Mor$RegionName)

FinalData_Left = VI_RI_Mor %>% 
  left_join(Zip, by = c("RegionName"="Zip_Code", "Year"="Year"))

FinalData_Inner = VI_RI_Mor %>% 
  inner_join(Zip, by = c("RegionName"="Zip_Code", "Year"="Year"))


library(regclass)
summary(lm(VI~RI, data = FinalData_Inner))

hist(FinalData_Inner$RI)
hist(FinalData_Inner$VI)

#use predictive model to guess house price from other variables, maybe divide by
#other categories, take predicted value divided by actual value time 100 for
#affordability score


#Calculate monthly mortgage payment with monthly rent and compare the two

write.csv(FinalData_Inner, "DatathonData2.csv")

# Calculating NAR affordability score
FinalData_Inner$MPMT = FinalData_Inner$VI*((FinalData_Inner$MortgageRate/100/12)/
                                             (1-(1/(1+(FinalData_Inner$MortgageRate/100/12))^360)))

FinalData_Inner$Median_Income_USD = as.integer(FinalData_Inner$Median_Income_USD)
FinalData_Inner$NecMonthInc = ((FinalData_Inner$MPMT*12)/
                                 FinalData_Inner$Median_Income_USD)
class(FinalData_Inner$Median_Income_USD)
summary(FinalData_Inner$Median_Income_USD)
which(is.na(FinalData_Inner$Median_Income_USD))
FinalData_Inner[129:140,]

FinalData_Inner$LoanQualInc = FinalData_Inner$MPMT*4*12

FinalData_Inner$NARAfford = (FinalData_Inner$Median_Income_USD/
                               FinalData_Inner$LoanQualInc)*100

FinalData_Inner1 = FinalData_Inner[-c(4:5,16:18)]
write.csv(FinalData_Inner1, "DatathonNARAfford.csv")


afford = FinalData_Inner
afford$MonthInc = afford$Median_Income_USD/12

# use monthly mortgage, rent price, and income to compare with NAR
# assumptions: no down payment, no more than 25% of inc on rent/mort

# monthly income/mortgage & monthly income/rent
# Calculations: Mort/Rent and Rent/Mort (Mort/Rent might be better)
# Some of our skewed calculations for affordability are due to negligible rent
#prices (maybe those cities don't have any apartments so no rent)
afford$IncOverMort = afford$MonthInc/afford$MPMT
afford$IncOverRent = afford$MonthInc/afford$RI
afford$MortOverRent = afford$IncOverMort/afford$IncOverRent
afford$RentOverMort = afford$IncOverRent/afford$IncOverMort

MortOverRent = lm(NARAfford~MortOverRent, data = afford)
summary(MortOverRent)
visualize_model(MortOverRent)
plot(residuals(MortOverRent))
check_regression(MortOverRent)
var(afford$NARAfford,afford$MortOverRent, na.rm = T)

MortOverRentLog = lm(log(NARAfford)~log(MortOverRent), data = afford)
summary(MortOverRentLog)
plot(residuals(MortOverRentLog))
check_regression(MortOverRentLog)

RentOverMort = lm(NARAfford~RentOverMort, data = afford)
summary(RentOverMort)
visualize_model(RentOverMort)
var(afford$NARAfford,afford$RentOverMort, na.rm = T)
check_regression(RentOverMort)

RentOverMortLog = lm(log(NARAfford)~log(RentOverMort), data = afford)
summary(RentOverMortLog)
check_regression(RentOverMortLog)

summary(afford$MortOverRent)
summary(afford$RentOverMort)
summary(afford$RI)
summary(afford$MonthInc)
summary(afford$MPMT)

afford1 = afford[complete.cases(afford),]
afford_rent = afford1[which(afford1$RI>=500),]
afford_rent1 = afford_rent[,-c(3:5,17:18,24)]

write.csv(afford_rent1, "affordability2.csv")

afford2 = afford_rent1

# new calculation
afford2$MortOverInc = afford2$MPMT/afford2$MonthInc
afford2$RentOverInc = afford2$RI/afford2$MonthInc
afford2$MortOverRent1 = afford2$MortOverInc/afford2$RentOverInc

MortOverRent = lm(NARAfford~MortOverRent, data = afford2)
summary(MortOverRent)
visualize_model(MortOverRent)
plot(residuals(MortOverRent))
check_regression(MortOverRent)

MortOverRent1 = lm(NARAfford~MortOverRent1, data = afford2)
summary(MortOverRent1)
visualize_model(MortOverRent1)
check_regression(MortOverRent1)


reg1 = lm(NARAfford~MortOverInc, data=afford2)
summary(reg1)

reg2 = lm(NARAfford~IncOverMort, data=afford2)
summary(reg2)

