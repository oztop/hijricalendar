library(tidyverse)

# Creating raw calendar

hijri <- expand.grid(days=1:30, months=1:12, years=1:1445)

# name the months

hijri$hijri.months <- case_when(hijri$months==1 ~ "Muharrem",
                                hijri$months==2 ~ "Safer",
                                hijri$months==3 ~ "Rebiülvvel",
                                hijri$months==4 ~ "Rebiülahir",
                                hijri$months==5 ~ "Cemaziyelevvel",
                                hijri$months==6 ~ "Cemaziyelahir",
                                hijri$months==7 ~ "Recep",
                                hijri$months==8 ~ "Şaban",
                                hijri$months==9 ~ "Ramazan",
                                hijri$months==10 ~ "Şevval",
                                hijri$months==11 ~ "Zilkade",
                                hijri$months==12 ~ "Zilhicce",
)



# assign the leap years and normal years

hijri$leap <- ifelse(hijri$years%%30 == 2|
                       hijri$years%%30 == 5 |
                       hijri$years%%30 == 7  |
                       hijri$years%%30 == 10 |
                       hijri$years%%30 == 13  |
                       hijri$years%%30 == 16  |
                       hijri$years%%30 == 18 | 
                       hijri$years%%30 == 21 |
                       hijri$years%%30 == 24 |
                       hijri$years%%30 == 26 |
                       hijri$years%%30 == 29, 
                     "Leap", "Normal"
)

### calibrating 29s and 30s of normal months (mind the new variable hijri.new)

hijri.new <- hijri[!grepl(2, hijri$months) &
                     !grepl(4, hijri$months) &
                     !grepl(6, hijri$months) &
                     !grepl(8, hijri$months) &
                     !grepl(10, hijri$months) |
                     
                     !grepl(30, hijri$days) | !grepl("Normal", hijri$leap) ,]    


# calirating the 29s and 30s of leap years (mind the new variable hijri.new1)

hijri.new1 <- hijri.new[       !grepl("Safer", hijri.new$hijri.months)& # because when I write 2 it removes also 12 which is zilhicce
                                 !grepl(4, hijri.new$months) & !grepl(6, hijri.new$months)&
                                 !grepl(8, hijri.new$months)&
                                 !grepl(10, hijri.new$months)
                               |
                                 !grepl(30, hijri.new$days) ,]  


# assigning name of the days


hijri.new1$weekdays <- case_when(
  hijri.new1$days%%7 == 5 ~ "Monday",
  hijri.new1$days%%7 == 6 ~ "Tuesday",
  hijri.new1$days%%7 == 0 ~ "Wednesday",
  hijri.new1$days%%7 == 1 ~ "Thursday",
  hijri.new1$days%%7 == 2 ~ "Friday",
  hijri.new1$days%%7 == 3 ~ "Saturday",
  hijri.new1$days%%7 == 4 ~ "Sunday"
) 





# adding the gregorian calendar

hijri.new1$gregorian <-  seq(as.Date("0622/07/19"), by = "day", length.out = 512060)
