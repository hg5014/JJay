#Read-in packages
install.packages("tidyverse")
library(tidyverse)
library(readxl)
install.packages("sqldf")
library(sqldf)


#Read-in and prepare F16 enrollment data
path <- file.path("~", "data/CF_Tumbleweed/20160906", "Sophomore_file_fa16_09062016.xlsx")
f16enroll <- read_excel(path, sheet = 1, col_names = TRUE)
str(f16enroll)

f16enroll[is.na(f16enroll$emplid),] #Blank record - drop

colnames(f16enroll)[29] <- "last_name"
colnames(f16enroll)[30] <- "first_name"

f16enroll2 <- subset(f16enroll, select = c(emplid, last_name, first_name, middle_name, EMAIL, Phone, address1, address2, city, state, postal
                                           , credit_current_term, acad_plan_current))

colnames(f16enroll2)[12:13] <- c("f16credatt", "f16major")

f16enroll3 <- f16enroll2[!is.na(f16enroll2$emplid),]

summary(f16enroll3)

onames <- c(names(f16enroll3[2:11]))
nnames <- paste("f16", onames, sep = "_")
colnames(f16enroll3)[2:11] <- nnames

str(f16enroll3)


#Read-in and prepare S17 enrollment data
path <- file.path("~", "data/CF_Tumbleweed/20170221", "Sophomore_file_sp17_02212017.xlsx")
s17enroll <- read_excel(path, sheet = 1, col_names = TRUE)
str(s17enroll)

s17enroll[is.na(s17enroll$emplid),] #No blanks

colnames(s17enroll)[27] <- "last_name"
colnames(s17enroll)[28] <- "first_name"

s17enroll2 <- subset(s17enroll, select = c(emplid, last_name, first_name, middle_name, Email, Phone, address1, address2, city, state, postal
                                           , credit_current_term, acad_plan_current))

colnames(s17enroll2)[12:13] <- c("s17credatt", "s17major")

summary(s17enroll2)

str(s17enroll2)

#Merge F16 and S17 data
data1617 <- merge(f16enroll3, s17enroll2, by = "emplid", all = TRUE)
str(data1617)
summary(data1617)

# Look at address/identification info
data1617[(data1617$last_name!=data1617$f16_last_name)&!is.na(data1617$last_name)&!is.na(data1617$f16_last_name)
         , c("emplid", "last_name", "f16_last_name", "first_name", "f16_first_name"
             , "Email", "f16_EMAIL", "Phone", "f16_Phone", "address1", "f16_address1")]

data1617[(data1617$first_name!=data1617$f16_first_name)&!is.na(data1617$first_name)&!is.na(data1617$f16_first_name)
          , c("emplid", "last_name", "f16_last_name", "first_name", "f16_first_name"
              , "Email", "f16_EMAIL", "Phone", "f16_Phone", "address1", "f16_address1")]

data1617[(data1617$Email!=data1617$f16_EMAIL)&!is.na(data1617$Email)&!is.na(data1617$f16_EMAIL)
          , c("emplid", "last_name", "f16_last_name", "first_name", "f16_first_name"
              , "Email", "f16_EMAIL", "Phone", "f16_Phone", "address1", "f16_address1")]

for(i in 2:11){
  data1617[,i+24] <- ifelse(is.na(data1617$last_name), data1617[,i], data1617[,i+12])
}

str(data1617)

onames <- c(names(data1617[14:23]))
nnames <- paste("s17", onames, sep = "_")
colnames(data1617)[26:35] <- nnames

data1617_2 <- subset(data1617, select = c(emplid, s17_last_name, s17_first_name, s17_middle_name, s17_Email, s17_Phone
                                          , s17_address1, s17_address2, s17_city, s17_state, s17_postal
                                          , f16credatt, f16major
                                          , s17credatt, s17major))
str(data1617_2)
summary(data1617_2)


#Read-in and prepare F17 enrollment data
path <- file.path("~", "data/CF_Tumbleweed/20170906", "AcadSuccess1179_clean.xlsx")
f17enroll <- read_excel(path, sheet = 1, col_names = TRUE)
str(f17enroll)

f17enroll[is.na(f17enroll$emplid),] #No blanks

f17enroll2 <- subset(f17enroll, select = c(emplid, last_name, first_name, middle_name, email, Phone, address1, address2, city, state, postal
                                           , credit_current_term, acad_plan_current))

colnames(f17enroll2)[12:13] <- c("f17credatt", "f17major")

summary(f17enroll2)

str(f17enroll2)

#Merge 1617 data with F17
data16f17 <- merge(data1617_2, f17enroll2, by = "emplid", all = TRUE)
str(data16f17)
summary(data16f17)

# Look at address/identification info
data16f17[(data16f17$last_name!=data16f17$s17_last_name)&!is.na(data16f17$last_name)&!is.na(data16f17$s17_last_name)
          , c("emplid", "last_name", "s17_last_name", "first_name", "s17_first_name"
              , "email", "s17_Email", "Phone", "s17_Phone", "address1", "s17_address1")]

data16f17[(data16f17$first_name!=data16f17$s17_first_name)&!is.na(data16f17$first_name)&!is.na(data16f17$s17_first_name)
          , c("emplid", "last_name", "s17_last_name", "first_name", "s17_first_name"
              , "email", "s17_Email", "Phone", "s17_Phone", "address1", "s17_address1")]

for(i in 2:11){
  data16f17[,i+26] <- ifelse(is.na(data16f17$last_name), data16f17[,i], data16f17[,i+14])
}

onames <- c(names(data16f17[16:25]))
nnames <- paste("f17", onames, sep = "_")
colnames(data16f17)[28:37] <- nnames

data16f17_2 <- subset(data16f17, select = c(emplid, f17_last_name, f17_first_name, f17_middle_name, f17_email, f17_Phone
                                            , f17_address1, f17_address2, f17_city, f17_state, f17_postal
                                            , f16credatt, f16major
                                            , s17credatt, s17major
                                            , f17credatt, f17major))

str(data16f17_2)
summary(data16f17_2)


#Read-in and prepare S18 enrollment data
path <- file.path("~", "data/CF_Tumbleweed/20180205", "SP 18 Student List 02.05.18_clean.xlsx")
s18enroll <- read_excel(path, sheet = 1, col_names = TRUE)
str(s18enroll)

s18enroll[is.na(s18enroll$emplid),] #No blanks

s18enroll2 <- subset(s18enroll, select = c(emplid, last_name, first_name, middle_name, email, Phone, address1, address2, city, state, postal
                                           , credit_current_term, tot_cred_cumulative, acad_plan_current))

colnames(s18enroll2)[12:14] <- c("s18credatt", "s18credern_cuml", "s18major")

onames <- c(names(s18enroll2[2:14]))
nnames <- paste("o", onames, sep = "_")
colnames(s18enroll2)[2:14] <- nnames

summary(s18enroll2)

str(s18enroll2)


#Read-in and prepare S18 grade data
path <- file.path("~", "data/CF_Tumbleweed/20180604", "AcadSuccess1182_clean.xlsx")
s18grade <- read_excel(path, sheet = 1, col_names = TRUE)
str(s18grade)

s18grade[is.na(s18grade$emplid),] #No blanks

colnames(s18grade)[17] <- "tot_cred_transfer_nogpa"

s18grade2 <- subset(s18grade, select = c(emplid, last_name, first_name, middle_name, email, Phone, address1, address2, city, state, postal
                                         , credit_current_term, tot_cred_transfer_nogpa, tot_cred_cumulative, current_term_GPA, current_cum_GPA, acad_plan_current))

colnames(s18grade2)[12:17] <- c("s18credatt", "s18credtrn", "s18credern_cuml", "s18gpa", "s18gpa_cuml", "s18major")

summary(s18grade2)

str(s18grade2)

#Merge S18 data
s18full <- merge(s18enroll2, s18grade2, by = "emplid", all = TRUE)
str(s18full)
summary(s18full)

# Look at address/identification info
s18full[(s18full$last_name!=s18full$o_last_name)&!is.na(s18full$last_name)&!is.na(s18full$o_last_name)
        , c("emplid", "last_name", "o_last_name", "first_name", "o_first_name"
            , "email", "o_email", "Phone", "o_Phone", "address1", "o_address1")]

s18full[(s18full$first_name!=s18full$o_first_name)&!is.na(s18full$first_name)&!is.na(s18full$o_first_name)
        , c("emplid", "last_name", "o_last_name", "first_name", "o_first_name"
            , "email", "o_email", "Phone", "o_Phone", "address1", "o_address1")]

for(i in 2:11){
  s18full[,i+29] <- ifelse(is.na(s18full$last_name), s18full[,i], s18full[,i+13])
}


onames <- c(names(s18full[15:24]))
nnames <- paste("s18", onames, sep = "_")
colnames(s18full)[31:40] <- nnames

#Calculate S18 outcomes
s18full$s18enr <- ifelse((s18full$o_s18credatt > 0)&!is.na(s18full$o_s18credatt), 1, 0)
table(s18full$o_s18credatt, s18full$s18enr, useNA = "always")

s18full$s18comp <- ifelse((s18full$s18credatt > 0)&!is.na(s18full$s18credatt), 1, 0)
table(s18full$s18credatt, s18full$s18comp, useNA = "always")

s18full$s18credern <- ifelse(s18full$s18enr == 1, s18full$s18credern_cuml - s18full$o_s18credern_cuml, 0)

s18full[sample(1:nrow(s18full), size = 50), c("emplid", "s18enr", "s18credatt","o_s18credatt", "s18credern_cuml", "o_s18credern_cuml", "s18credern")]

s18full$s18cred_latr <- (s18full$s18credern < 0|s18full$s18credern > s18full$o_s18credatt) * 1

table(s18full$s18credern, s18full$s18cred_latr)

s18full[(s18full$s18credern < 0 & s18full$o_s18credatt > 0 &! is.na(s18full$s18credern) &! is.na(s18full$o_s18credatt)), c("emplid", "s18credern", "s18credern_cuml", "o_s18credern_cuml", "s18cred_latr", "o_s18credatt")]

s18full$s18major_final <- ifelse(is.na(s18full$o_s18major), s18full$s18major, s18full$o_s18major)
s18full[s18full$s18major_final!=s18full$o_s18major&!is.na(s18full$s18major_final)&!is.na(s18full$o_s18major), ] #No such cases

s18full2 <- subset(s18full, select = c(emplid, s18_last_name, s18_first_name, s18_middle_name, s18_email, s18_Phone
                                       , s18_address1, s18_address2, s18_city, s18_state, s18_postal
                                       , s18enr, o_s18credatt, s18comp, s18credern, s18cred_latr, s18gpa
                                       , s18credtrn, s18credern_cuml, s18gpa_cuml, s18major_final))

colnames(s18full2)[13] <- c("s18credatt")
colnames(s18full2)[21] <- c("s18major")
colnames(s18full2)[2:11] <- c("last_name", "first_name", "middle_name", "email", "phone", "address1", "address2", "city", "state", "postal")

str(s18full2)
summary(s18full2)


#Merge F16-F17 data with S18
data1618 <- merge(data16f17_2, s18full2, by = "emplid", all = TRUE)
str(data1618)
summary(data1618)

# Look at address/identification info
data1618[(data1618$last_name!=data1618$f17_last_name)&!is.na(data1618$last_name)&!is.na(data1618$f17_last_name)
          , c("emplid", "last_name", "f17_last_name", "first_name", "f17_first_name"
              , "email", "f17_email", "phone", "f17_Phone", "address1", "f17_address1")]

data1618[(data1618$first_name!=data1618$f17_first_name)&!is.na(data1618$first_name)&!is.na(data1618$f17_first_name)
          , c("emplid", "last_name", "f17_last_name", "first_name", "f17_first_name"
              , "email", "f17_email", "phone", "f17_Phone", "address1", "f17_address1")]


for(i in 2:11){
  data1618[,i+36] <- ifelse(is.na(data1618$last_name), data1618[,i], data1618[,i+16])
}

str(data1618)

onames <- c(names(data1618[18:27]))
nnames <- paste("s18", onames, sep = "_")
colnames(data1618)[38:47] <- nnames

data1618_2 <- subset(data1618, select = c(emplid, s18_last_name, s18_first_name, s18_middle_name, s18_email, s18_phone
                                          , s18_address1, s18_address2, s18_city, s18_state, s18_postal
                                          , f16credatt, f16major
                                          , s17credatt, s17major
                                          , f17credatt, f17major
                                          , s18enr, s18credatt, s18comp, s18credern, s18cred_latr, s18gpa, s18credtrn, s18credern_cuml, s18gpa_cuml, s18major))

str(data1618_2)
summary(data1618_2)


#Read-in and prepare F18 enrollment data
path <- file.path("~", "data/CF_Tumbleweed/20180702", "AcadSuccess1189_clean.xlsx")
f18enroll <- read_excel(path, sheet = 1, col_names = TRUE)
str(f18enroll)

f18enroll[is.na(f18enroll$emplid),] #No blanks

f18enroll2 <- subset(f18enroll, select = c(emplid, last_name, first_name, middle_name, email, Phone, address1, address2, city, state, postal
                                           , credit_current_term, acad_plan_current))

colnames(f18enroll2)[12:13] <- c("f18credatt", "f18major")

summary(f18enroll2)

str(f18enroll2)


#Merge 1618 data with F18
data16f18 <- merge(data1618_2, f18enroll2, by = "emplid", all = TRUE)
str(data16f18)
summary(data16f18)

# Look at address/identification info
data16f18[(data16f18$last_name!=data16f18$s18_last_name)&!is.na(data16f18$last_name)&!is.na(data16f18$s18_last_name)
          , c("emplid", "last_name", "s18_last_name", "first_name", "s18_first_name"
              , "email", "s18_email", "Phone", "s18_phone", "address1", "s18_address1")]

data16f18[(data16f18$first_name!=data16f18$s18_first_name)&!is.na(data16f18$first_name)&!is.na(data16f18$s18_first_name)
          , c("emplid", "last_name", "s18_last_name", "first_name", "s18_first_name"
              , "email", "s18_email", "Phone", "s18_phone", "address1", "s18_address1")]

for(i in 2:11){
  data16f18[,i+38] <- ifelse(is.na(data16f18$last_name), data16f18[,i], data16f18[,i+26])
}

onames <- c(names(data16f18[28:37]))

data16f18_2 <- subset(data16f18, select = c(emplid, V40, V41, V42, V43, V44, V45, V46, V47, V48, V49
                                            , f16credatt, f16major
                                            , s17credatt, s17major
                                            , f17credatt, f17major
                                            , s18enr, s18credatt, s18comp, s18credern, s18cred_latr, s18gpa, s18credtrn, s18credern_cuml, s18gpa_cuml, s18major
                                            , f18credatt, f18major))


colnames(data16f18_2)[2:11] <- onames

str(data16f18_2)
summary(data16f18_2)



#Pull in additional fields and follow up data from the FY trackers for F15 F16 and F17
#Read in and prepare F15 FY tracker
path <- file.path("~", "data/SASP lists", "FRSH 15 Tracking 06.11.18 .xlsx")
f15fy <- read_excel(path, sheet = "FRSH FA 15 FINAL LIST", col_names = TRUE)
str(f15fy)
summary(f15fy)

colnames(f15fy)[1] = "emplid"
colnames(f15fy)[2] = "fy_last_name"
colnames(f15fy)[3] = "fy_first_name"
colnames(f15fy)[16] = "fys_cohort"
colnames(f15fy)[18] = "data_cohort"
colnames(f15fy)[17] = "f15major"
colnames(f15fy)[20] = "f15credatt"
colnames(f15fy)[21] = "f15credern_cuml"
colnames(f15fy)[22] = "f15gpa_cuml"
colnames(f15fy)[23] = "s16credatt"
colnames(f15fy)[24] = "s16credern_cuml"
colnames(f15fy)[25] = "s16gpa_cuml"
colnames(f15fy)[28] = "f16credern"
colnames(f15fy)[27] = "f16credern_cuml"
colnames(f15fy)[29] = "f16gpa"
colnames(f15fy)[30] = "f16gpa_cuml"
colnames(f15fy)[34] = "s17credern"
colnames(f15fy)[33] = "s17credern_cuml"
colnames(f15fy)[35] = "s17gpa"
colnames(f15fy)[36] = "s17gpa_cuml"
colnames(f15fy)[37] = "s17dismiss"
colnames(f15fy)[42] = "f17credern"
colnames(f15fy)[41] = "f17credern_cuml"
colnames(f15fy)[43] = "f17gpa"
colnames(f15fy)[44] = "f17gpa_cuml"
colnames(f15fy)[54] = "s18dismiss"
colnames(f15fy)[53] = "s18degree"

summary(f15fy)
str(f15fy)

f15fy$fys_year <- "F15"

table(f15fy$fys_year)

f15fy$f16major <- NA
f15fy$f17major <- NA

f15fy2 <- subset(f15fy, select = c(emplid, fy_last_name, fy_first_name, fys_year, fys_cohort, data_cohort
                                   , f15credatt, f15credern_cuml, f15gpa_cuml, f15major
                                   , s16credatt, s16credern_cuml, s16gpa_cuml
                                   , f16credern, f16credern_cuml, f16gpa, f16gpa_cuml, f16major
                                   , s17credern, s17credern_cuml, s17gpa, s17gpa_cuml, s17dismiss
                                   , f17credern, f17credern_cuml, f17gpa, f17gpa_cuml, f17major
                                   , s18dismiss, s18degree))

#Read in S16 FYS roster to identify SP16 FYS students
path <- file.path("~", "data/SASP lists", "FYS_Spring16_2_19_16.xls")
s16roster <- read_excel(path, sheet = 1, col_names = TRUE)
str(s16roster)
summary(s16roster)

s16roster2 <- subset(s16roster, select = c(ID))
colnames(s16roster2)[1] = "emplid"

str(s16roster2)

#Merge with  F15
s16roster2$SPFYS <- 1

f15fy3 <- merge(f15fy2, s16roster2, by = "emplid", all.x = TRUE)
str(f15fy3)
summary(f15fy3)

table(f15fy3$data_cohort)

f15fy3$n_data_cohort <- ifelse(f15fy3$data_cohort=="No FYS"&f15fy3$SPFYS==1&!is.na(f15fy3$SPFYS), "SP FYS", f15fy3$data_cohort)

table(f15fy3$n_data_cohort, f15fy3$data_cohort, f15fy3$SPFYS, useNA = "always")



### PICK UP HERE ######




#Read in and prepare F16 FY tracker
path <- file.path("~", "data/SASP lists", "FA 16 FY Cohort - 06.07.18.xlsx")
f16fy <- read_excel(path, sheet = 1, col_names = TRUE)
str(f16fy)
summary(f16fy)

colnames(f16fy)[1] = "emplid"
colnames(f16fy)[4] = "fy_last_name"
colnames(f16fy)[3] = "fy_first_name"
colnames(f16fy)[5] = "fys_cohort"
colnames(f16fy)[6] = "data_cohort"
colnames(f16fy)[7] = "f16major"
colnames(f16fy)[12] = "f16credern_cuml"
colnames(f16fy)[13] = "f16gpa"
colnames(f16fy)[14] = "f16gpa_cuml"
colnames(f16fy)[18] = "s17credern"
colnames(f16fy)[17] = "s17credern_cuml"
colnames(f16fy)[19] = "s17gpa"
colnames(f16fy)[20] = "s17gpa_cuml"
colnames(f16fy)[21] = "s17dismiss"
colnames(f16fy)[27] = "f17credern"
colnames(f16fy)[26] = "f17credern_cuml"
colnames(f16fy)[28] = "f17gpa"
colnames(f16fy)[29] = "f17gpa_cuml"
colnames(f16fy)[42] = "s18dismiss"

f16fy$fys_year <- "F16"

table(f16fy$fys_year)

f16fy$f15credatt <- 0
f16fy$f15credern_cuml <- 0
f16fy$f15gpa_cuml <- NA
f16fy$s16credatt <- 0
f16fy$s16credern_cuml <- 0
f16fy$s16gpa_cuml <- NA
f16fy$f16credern <- f16fy$f16credern_cuml
f16fy$s18degree <- NA

f16fy$f15major <- NA
f16fy$f17major <- NA

summary(f16fy)
str(f16fy)

f16fy2 <- subset(f16fy, select = c(emplid, fy_last_name, fy_first_name, fys_year, fys_cohort, data_cohort
                                   , f15credatt, f15credern_cuml, f15gpa_cuml, f15major
                                   , s16credatt, s16credern_cuml, s16gpa_cuml
                                   , f16credern, f16credern_cuml, f16gpa, f16gpa_cuml, f16major
                                   , s17credern, s17credern_cuml, s17gpa, s17gpa_cuml, s17dismiss
                                   , f17credern, f17credern_cuml, f17gpa, f17gpa_cuml, f17major
                                   , s18dismiss, s18degree))


#Read in and prepare F17 FY tracker
path <- file.path("~", "data/SASP lists", "FA 17 Cohort_6.27.18.xlsx")
f17fy <- read_excel(path, sheet = 1, col_names = TRUE)
str(f17fy)
summary(f17fy)

colnames(f17fy)[1] = "emplid"
colnames(f17fy)[3] = "fy_last_name"
colnames(f17fy)[4] = "fy_first_name"
colnames(f17fy)[5] = "fys_cohort"
colnames(f17fy)[6] = "s18cohort"
colnames(f17fy)[7] = "data_cohort"
colnames(f17fy)[8] = "f17major"
colnames(f17fy)[13] = "f17credern_cuml"
colnames(f17fy)[14] = "f17gpa"
colnames(f17fy)[15] = "f17gpa_cuml"
colnames(f17fy)[25] = "s18dismiss"

table(f17fy$data_cohort, f17fy$s18cohort, useNA = "always")

f17fy$fys_year <- ifelse(f17fy$s18cohort=="SP FYS", "S18")
f17fy$fys_year <- ifelse(is.na(f17fy$s18cohort), "F17", f17fy$fys_year)
table(f17fy$fys_year, f17fy$s18cohort, useNA = "always")
table(f17fy$data_cohort, f17fy$fys_year, useNA = "always")

f17fy$f15credatt <- 0
f17fy$f15credern_cuml <- 0
f17fy$f15gpa_cuml <- NA
f17fy$s16credatt <- 0
f17fy$s16credern_cuml <- 0
f17fy$s16gpa_cuml <- NA
f17fy$f16credern <- NA
f17fy$f16credern_cuml <- 0 
f17fy$f16gpa <- NA
f17fy$f16gpa_cuml <- NA
f17fy$s17credern <- 0
f17fy$s17credern_cuml <- 0
f17fy$s17gpa <- NA
f17fy$s17gpa_cuml <- NA
f17fy$s17dismiss <- NA
f17fy$f17credern <- f17fy$f17credern_cuml
f17fy$s18degree <- NA

f17fy$f15major <- NA
f17fy$f16major <- NA


summary(f17fy)
str(f17fy)

f17fy2 <- subset(f17fy, select = c(emplid, fy_last_name, fy_first_name, fys_year, fys_cohort, data_cohort
                                   , f15credatt, f15credern_cuml, f15gpa_cuml, f15major
                                   , s16credatt, s16credern_cuml, s16gpa_cuml
                                   , f16credern, f16credern_cuml, f16gpa, f16gpa_cuml, f16major
                                   , s17credern, s17credern_cuml, s17gpa, s17gpa_cuml, s17dismiss
                                   , f17credern, f17credern_cuml, f17gpa, f17gpa_cuml, f17major
                                   , s18dismiss, s18degree))


#Append FY datasets
FYds <- rbind(f15fy2, f16fy2, f17fy2)
str(FYds)
table(FYds$fys_year)

dupempl <- pull(FYds[duplicated(FYds$emplid)==TRUE, 1])
FYds[which(FYds$emplid %in% dupempl), ] #Looks like 3 of the same people - uncertain about 23468326

#Merge FYS data with the F16-F18 data
dupempl <- data16f18_2[duplicated(data16f18_2$emplid)==TRUE, 1]
dupempl #No duplicates in the full dataset - good, go ahead and merge with FYS

data16f18_2$infull <- 1
FYds$inFY <- 1

data16f18_wcohs <- merge(data16f18_2, FYds, by = "emplid", all = TRUE)

table(data16f18_wcohs$infull, data16f18_wcohs$inFY, useNA = "always")
str(data16f18_wcohs)


#Compare F16 and F17 major fields from FYS data and Tumbleweed files
data16f18_wcohs[data16f18_wcohs$f16major.x!=data16f18_wcohs$f16major.y&!is.na(data16f18_wcohs$f16major.x)&!is.na(data16f18_wcohs$f16major.y)
                , c("emplid", "fys_year", "f16major.x", "f16major.y")] #all match

data16f18_wcohs[data16f18_wcohs$f17major.x!=data16f18_wcohs$f17major.y&!is.na(data16f18_wcohs$f17major.x)&!is.na(data16f18_wcohs$f17major.y)
                , c("emplid", "fys_year", "f17major.x", "f17major.y")] #all match

colnames(data16f18_wcohs)[13] <- "f16major"
colnames(data16f18_wcohs)[17] <- "f17major"

#Create final variables

#Update cohorts for reporting
data16f18_wcohs$odata_cohort <- data16f18_wcohs$data_cohort

table(data16f18_wcohs$odata_cohort, data16f18_wcohs$data_cohort)

data16f18_wcohs$data_cohort <- ifelse(
  data16f18_wcohs$fys_year=="F15"&data16f18_wcohs$odata_cohort=="HONR - No FYS"
  , "HONR"
  , ifelse(data16f18_wcohs$fys_year=="F16"&data16f18_wcohs$odata_cohort %in% c("HON FYS", "HON No FYS")
           , "HONR"
           , ifelse(data16f18_wcohs$fys_year=="F16"&data16f18_wcohs$odata_cohort %in% c("PCF JUST", "R&I JUST", "SJ JUST")
                  , "Justice"
                  , data16f18_wcohs$odata_cohort 
                  )
          )
  )

table(data16f18_wcohs$odata_cohort, data16f18_wcohs$data_cohort)


#Enrollment Vars
data16f18_wcohs$f15enr <- ifelse((data16f18_wcohs$f15credatt > 0)&!is.na(data16f18_wcohs$f15credatt), 1, 0)
table(data16f18_wcohs$f15credatt, data16f18_wcohs$f15enr, useNA = "always")

data16f18_wcohs$s16enr <- ifelse((data16f18_wcohs$s16credatt > 0)&!is.na(data16f18_wcohs$s16credatt), 1, 0)
table(data16f18_wcohs$s16credatt, data16f18_wcohs$s16enr, useNA = "always")

data16f18_wcohs$f16enr <- ifelse((data16f18_wcohs$f16credatt > 0)&!is.na(data16f18_wcohs$f16credatt), 1, 0)
table(data16f18_wcohs$f16credatt, data16f18_wcohs$f16enr, useNA = "always")

data16f18_wcohs$s17enr <- ifelse((data16f18_wcohs$s17credatt > 0)&!is.na(data16f18_wcohs$s17credatt), 1, 0)
table(data16f18_wcohs$s17credatt, data16f18_wcohs$s17enr, useNA = "always")

data16f18_wcohs$f17enr <- ifelse((data16f18_wcohs$f17credatt > 0)&!is.na(data16f18_wcohs$f17credatt), 1, 0)
table(data16f18_wcohs$f17credatt, data16f18_wcohs$f17enr, useNA = "always")

data16f18_wcohs$f18enr <- ifelse((data16f18_wcohs$f18credatt > 0)&!is.na(data16f18_wcohs$f18credatt), 1, 0)
table(data16f18_wcohs$f18credatt, data16f18_wcohs$f18enr, useNA = "always")

#Credit accumulation vars 
data16f18_wcohs$f16enr_30P <- ifelse(data16f18_wcohs$f16enr==1&data16f18_wcohs$s16credern_cuml>=30&!is.na(data16f18_wcohs$f16enr)&!is.na(data16f18_wcohs$s16credern_cuml), 1, 0)
table(data16f18_wcohs$s16credern_cuml, data16f18_wcohs$f16enr, data16f18_wcohs$f16enr_30P, useNA = "always")

data16f18_wcohs$f17enr_30P <- ifelse(data16f18_wcohs$f17enr==1&data16f18_wcohs$s17credern_cuml>=30&!is.na(data16f18_wcohs$f17enr)&!is.na(data16f18_wcohs$s17credern_cuml), 1, 0)
table(data16f18_wcohs$s17credern_cuml, data16f18_wcohs$f17enr, data16f18_wcohs$f17enr_30P, useNA = "always")

data16f18_wcohs$f17enr_60P <- ifelse(data16f18_wcohs$f17enr==1&data16f18_wcohs$s17credern_cuml>=60&!is.na(data16f18_wcohs$f17enr)&!is.na(data16f18_wcohs$s17credern_cuml), 1, 0)
table(data16f18_wcohs$s17credern_cuml, data16f18_wcohs$f17enr, data16f18_wcohs$f17enr_60P, useNA = "always")

data16f18_wcohs$f18enr_30P <- ifelse(data16f18_wcohs$f18enr==1&data16f18_wcohs$s18credern_cuml>=30&!is.na(data16f18_wcohs$f18enr)&!is.na(data16f18_wcohs$s18credern_cuml), 1, 0)
table(data16f18_wcohs$s18credern_cuml, data16f18_wcohs$f18enr, data16f18_wcohs$f18enr_30P, useNA = "always")
aggregate(x = data16f18_wcohs$s18credern_cuml, by = list(paste(data16f18_wcohs$f18enr, data16f18_wcohs$f18enr_30P)), FUN = min)
aggregate(x = data16f18_wcohs$s18credern_cuml, by = list(paste(data16f18_wcohs$f18enr, data16f18_wcohs$f18enr_30P)), FUN = max)

data16f18_wcohs$f18enr_60P <- ifelse(data16f18_wcohs$f18enr==1&data16f18_wcohs$s18credern_cuml>=60&!is.na(data16f18_wcohs$f18enr)&!is.na(data16f18_wcohs$s18credern_cuml), 1, 0)
table(data16f18_wcohs$s18credern_cuml, data16f18_wcohs$f18enr, data16f18_wcohs$f18enr_60P, useNA = "always")
aggregate(x = data16f18_wcohs$s18credern_cuml, by = list(paste(data16f18_wcohs$f18enr, data16f18_wcohs$f18enr_60P)), FUN = min)
aggregate(x = data16f18_wcohs$s18credern_cuml, by = list(paste(data16f18_wcohs$f18enr, data16f18_wcohs$f18enr_60P)), FUN = max)

data16f18_wcohs$f18enr_90P <- ifelse(data16f18_wcohs$f18enr==1&data16f18_wcohs$s18credern_cuml>=90&!is.na(data16f18_wcohs$f18enr)&!is.na(data16f18_wcohs$s18credern_cuml), 1, 0)
table(data16f18_wcohs$s18credern_cuml, data16f18_wcohs$f18enr, data16f18_wcohs$f18enr_90P, useNA = "always")
aggregate(x = data16f18_wcohs$s18credern_cuml, by = list(paste(data16f18_wcohs$f18enr, data16f18_wcohs$f18enr_90P)), FUN = min)
aggregate(x = data16f18_wcohs$s18credern_cuml, by = list(paste(data16f18_wcohs$f18enr, data16f18_wcohs$f18enr_90P)), FUN = max)

#Name
data16f18_wcohs$last_name <- ifelse(is.na(data16f18_wcohs$last_name), data16f18_wcohs$fy_last_name, data16f18_wcohs$last_name)
data16f18_wcohs$first_name <- ifelse(is.na(data16f18_wcohs$first_name), data16f18_wcohs$fy_first_name, data16f18_wcohs$first_name)

#Put all major fields into a single field
data16f18_wcohs$academic_plan <- ifelse(is.na(data16f18_wcohs$f17major)
                                        , ifelse(is.na(data16f18_wcohs$f16major)
                                                 , data16f18_wcohs$f15major, data16f18_wcohs$f16major
                                                 )
                                        , data16f18_wcohs$f17major
                                        )


#Pick up cumulative credits earned and GPA from earlier sems if student not enrolled in S18
#Doing this for FY for now - eventually update this for F16-F18 using the data available
data16f18_wcohs$credits_cumulative <- ifelse(is.na(data16f18_wcohs$s18credern_cuml)
                                             , ifelse(is.na(data16f18_wcohs$f17credern_cuml)
                                                      , ifelse(is.na(data16f18_wcohs$s17credern_cuml)
                                                               , ifelse(is.na(data16f18_wcohs$f16credern_cuml)
                                                                        , ifelse(is.na(data16f18_wcohs$s16credern_cuml)
                                                                                 , ifelse(is.na(data16f18_wcohs$f15credern_cuml)
                                                                                          , 0, data16f18_wcohs$f15credern_cuml
                                                                                          )
                                                                                 , data16f18_wcohs$s16credern_cuml
                                                                                 )
                                                                        , data16f18_wcohs$f16credern_cuml
                                                                        )
                                                               , data16f18_wcohs$s17credern_cuml
                                                               )
                                                      , data16f18_wcohs$f17credern_cuml
                                                      )
                                             , data16f18_wcohs$s18credern_cuml
                                             )

data16f18_wcohs$gpa_cumulative <- ifelse(is.na(data16f18_wcohs$s18gpa_cuml)
                                         , ifelse(is.na(data16f18_wcohs$f17gpa_cuml)
                                                  , ifelse(is.na(data16f18_wcohs$s17gpa_cuml)
                                                           , ifelse(is.na(data16f18_wcohs$f16gpa_cuml)
                                                                    , ifelse(is.na(data16f18_wcohs$s16gpa_cuml)
                                                                             , ifelse(is.na(data16f18_wcohs$f15gpa_cuml)
                                                                                      , NA, data16f18_wcohs$f15gpa_cuml
                                                                                      )
                                                                             , data16f18_wcohs$s16gpa_cuml
                                                                             )
                                                                    , data16f18_wcohs$f16gpa_cuml
                                                                    )
                                                           , data16f18_wcohs$s17gpa_cuml
                                                           )
                                                  , data16f18_wcohs$f17gpa_cuml
                                                  )
                                         , data16f18_wcohs$s18gpa_cuml
                                         )

data16f18_3 <- subset(data16f18_wcohs, select = c(emplid, last_name, first_name, middle_name, email, Phone
                                              , address1, address2, city, state, postal
                                              , fys_year, fys_cohort, data_cohort, academic_plan
                                              , f15enr, f15credatt, f15credern_cuml, f15gpa_cuml
                                              , s16enr, s16credatt, s16credern_cuml, s16gpa_cuml
                                              , f16enr, f16credatt, f16enr_30P, f16credern, f16credern_cuml, f16gpa, f16gpa_cuml
                                              , s17enr, s17credatt, s17credern, s17credern_cuml, s17gpa, s17gpa_cuml, s17dismiss
                                              , f17enr, f17credatt, f17enr_30P, f17enr_60P, f17credern, f17credern_cuml, f17gpa, f17gpa_cuml
                                              , s18enr, s18credatt, s18credern, s18cred_latr, s18gpa, s18credtrn, s18credern_cuml, s18gpa_cuml, s18dismiss, s18degree
                                              , f18enr, f18credatt, f18enr_30P, f18enr_60P, f18enr_90P
                                              , credits_cumulative, gpa_cumulative))



summary(data16f18_3)

for(i in 16:62){
  if(names(data16f18_3[i])%in%c("f15gpa_cuml", "s16gpa_cuml", "f16gpa", "f16gpa_cuml"
                                , "s17gpa", "s17gpa_cuml", "s17dismiss", "f17gpa", "f17gpa_cuml"
                                , "s18gpa", "s18gpa_cuml", "s18dismiss", "s18degree")){
    
  } else{
    data16f18_3[,i] <- ifelse(is.na(data16f18_3[,i]), 0, data16f18_3[,i])
  }
}

summary(data16f18_3)


#Final checks on missing
data16f18_3[is.na(data16f18_3$last_name), ] #No cases
data16f18_3[is.na(data16f18_3$first_name), ] #No cases



#Merge in demographics data from YW
path <- file.path("~", "Data/from Ying/Demographics", "Gender_Race_FA15_17_activated.xlsx")
dem <- read_excel(path, sheet = "Gender_race_I805_06202018", col_names = TRUE)
str(dem)
table(dem$sex)
table(dem$ethnic_group, dem$reclassified_ethnic_group)

dem$indem <- 1 

s18enroll_race <- subset(s18enroll, select = c(emplid, ethnic_group, reclassified_ethnic_group))
s18enroll_race$ins18enroll <- 1

s18grade_race <- subset(s18grade, select = c(emplid, ethnic_group, reclassified_ethnic_group))
s18grade_race$ins18grade <- 1 

s18race <- merge(x = s18grade_race, y = s18enroll_race, by = "emplid", all = TRUE)
table(s18race$ins18grade , s18race$ins18enroll, useNA = "always")

s18race$reclassified_ethnic_group <- ifelse(is.na(s18race$reclassified_ethnic_group.x), s18race$reclassified_ethnic_group.y, s18race$reclassified_ethnic_group.x)
aggregate(x = s18race$emplid, by = list(paste(s18race$reclassified_ethnic_group, s18race$reclassified_ethnic_group.x, s18race$reclassified_ethnic_group.y, sep = "|")), FUN = length)

s18race$ethnic_group <- ifelse(is.na(s18race$ethnic_group.x), s18race$ethnic_group.y, s18race$ethnic_group.x)

s18race2 <- subset(s18race, select = c(emplid, ethnic_group, reclassified_ethnic_group))

s18race2$ins18 <- 1

dem2 <- merge(x = s18race2, y = dem, by = "emplid", all = TRUE)
table(dem2$ins18, dem2$indem, useNA = "always")

dem2$reclassified_ethnic_group <- ifelse(is.na(dem2$reclassified_ethnic_group.x), dem2$reclassified_ethnic_group.y, dem2$reclassified_ethnic_group.x)
dem2$ethnic_group <- ifelse(is.na(dem2$ethnic_group.x), dem2$ethnic_group.y, dem2$ethnic_group.x)

dem3 <-subset(dem2, select = c(emplid, ethnic_group, reclassified_ethnic_group, sex))
dem3$indem <- 1

path <- file.path("~", "Data/from Ying/Demographics", "SP18_stu_gender.xlsx")
s18sex <- read_excel(path, sheet = 1, col_names = TRUE)
s18sex$ins18 <- 1

dem4 <- merge(x = s18sex, y = dem3, by = "emplid", all = TRUE)
table(dem4$ins18, dem4$indem, useNA = "always")
table(dem4$sex.x, dem4$sex.y, useNA = "always")

dem4$sex <- ifelse(is.na(dem4$sex.x)|dem4$sex.x=="U", dem4$sex.y, dem4$sex.x)
dem4$sex <- ifelse(dem4$sex=="U", NA, dem4$sex)
aggregate(x = dem4$emplid, by = list(paste(dem4$sex, dem4$sex.x, dem4$sex.y, sep = "|")), FUN = length)

dem5 <- subset(dem4, select = c(emplid, ethnic_group, reclassified_ethnic_group, sex))


#Comment out the Pell IRDB data because it is causing a lot of duplicates
# dem5$indem <- 1
# 
# path <- file.path("~", "Data/from Ying/Demographics", "Gender_Race_FA15_17_activated.xlsx")
# pell <- read_excel(path, sheet = "Pell_flag_IRDB_06222018", col_names = TRUE)
# str(pell)
# 
# colnames(pell)[1] <- "emplid"
# colnames(pell)[2] <- "pell_flag"
# 
# table(pell$pell_flag, useNA = "always")
# 
# pell$inpell <- 1
# 
# dem6 <- merge(dem5, pell, by = "emplid", all = TRUE)
# table(dem6$indem, dem6$inpell, useNA = "always")
# 
# dem7 <- subset(dem6, select = c(emplid, ethnic_group, reclassified_ethnic_group, sex, pell_flag))

#Merge demographic and Pell data to full data file
dem5$indem <- 1
data16f18_3$indata <- 1

dupempl <- dem5[duplicated(dem5$emplid)==TRUE, 1]
dem5[which(dem5$emplid %in% dupempl), ] #Exact duplicates

dem6 <- unique(dem5[,1:5])

dupempl <- dem6[duplicated(dem6$emplid)==TRUE, 1]
dupempl 

data16f18_4 <- merge(data16f18_3, dem6, by = "emplid", all = TRUE)
table(data16f18_4$indata, data16f18_4$indem, useNA = "always")

data16f18_5 <- data16f18_4[, !names(data16f18_4) %in% c("indem", "indata")]
str(data16f18_5)

#Save final dataset
saveRDS(data16f18_5, file = "~/R/RData/20180703/f16f18allstudents.rds")
write.csv(data16f18_5, file = "~/R/Routput/f16f18allstudents.csv", row.names = FALSE)