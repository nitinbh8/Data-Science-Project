
library(lubridate)
library(dplyr)
getwd()
project  = read.csv("C:/Users/Nitin/OneDrive/Desktop/Data Science Project/DSCI490FullDataSet.csv")

class(project$MbrStartDate)

date = as.Date(project$MbrStartDate, format = "%d/%m/%Y")
date2 = as.Date(project$MbrStartDate, format = "%d-%m-%Y")

date [is.na(date)] = date2[!is.na(date2)]
project$MbrStartDate = date
class(project$MbrStartDate)

datedate= as.Date(project$DataDate, format = "%d/%m/%Y")
project$DataDate = datedate

bdate = as.Date(project$BirthDate, format = "%d/%m/%Y")
bdate2 = as.Date(project$BirthDate, format = "%Y-%m-%d")

bdate [is.na(bdate)] = bdate2[!is.na(bdate2)]

bdate[is.na(bdate)] = bdate2[is.na(bdate)]
class(bdate)
project$BirthDate = bdate
class(project$BirthDate)

dmy[is.na(dmy)] = ymd[is.na(dmy)]

project$CreditCardStartDate = dmy

secondary = select(Data_Postal_Codes_Prospera,-Link)

sec = select(project,PostalCode)

I = merge(sec,secondary, by = "PostalCode")

project2 = project

main_data = merge(project2, secondary,by="PostalCode") %>% select(-FauxMemberNumber,-DataDate, -AreaName, -Segment, -FauxFirstName, 
                                                                  -FauxMiddleName,-FauxLastName)

str(main_data$`City/town`)

main_data$`City/town` = as.factor(main_data$`City/town`)

summary(main_data)

main_data$Gender  = ifelse(main_data$Gender=="male", 0,ifelse(main_data$Gender=="female",1,2))
main_data$MarketingConsent  = ifelse(main_data$MarketingConsent=="No", 0,ifelse(main_data$MarketingConsent=="Yes",1,2))
levels(main_data$OnlineBankingUser)
main_data$OnlineBankingUser  = ifelse(main_data$OnlineBankingUser=="No", 0,ifelse(main_data$OnlineBankingUser=="Yes",1,2))
levels(main_data$HasPayroll)
main_data$HasPayroll  = ifelse(main_data$HasPayroll=="No", 0,ifelse(main_data$HasPayroll=="Yes",1,2))
levels(main_data$StatementType)

write.csv(main_data, file = "Final_data.csv" )
Final_data = read.csv("Final_data.csv")

Final_data$Gender = as.factor(Final_data$Gender)
Final_data$MarketingConsent = as.factor(Final_data$MarketingConsent)
Final_data$OnlineBankingUser = as.factor(Final_data$OnlineBankingUser)
Final_data$HasPayroll = as.factor(Final_data$HasPayroll)
Final_data$HasCreditCard = as.factor(Final_data$HasCreditCard)
Final_data$HasMortgage = as.factor(Final_data$HasMortgage)
Final_data$HasWealthManagement = as.factor(Final_data$HasWealthManagement)
str(Final_data)

table(Final_data$Gender)
table(Final_data$HasCreditCard)
table(Final_data$HasMortgage)
table(Final_data$HasPayroll)
table(Final_data$HasWealthManagement)
write.csv(Final_data, file = "Final_data.csv" )

