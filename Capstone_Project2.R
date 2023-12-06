##################################### IMPORTANT NOTE ########################################
#   - I used doParallel package in many places, most of the time each executing thread requires between 4 to 5 GB of RAM 
#     Please update the number of execution threads wherever applicable to match your system specs and run the script step by step. 
#     using at least 10 cores CPU running the script will require around 3 hours, 
#     
#  - You skip running this file by un-commenting and run the following lines of code
#
    options(timeout = 1200) # to be able to download the file in case of big files or slow connection/server
    furl <- 'https://sameersbucket.s3.fr-par.scw.cloud/final_ckpt.RData.zip'
    fzname <- 'final_ckpt.RData.zip'
    fname <- 'final_ckpt.RData'
    if(!file.exists(fzname))
       download.file(furl,fzname)
    unzip(fzname, fname)
    load(fname)
#
############################################################################################
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(doParallel)) install.packages("doParallel", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(stringr)
library(corrplot)
library(readr)
library(gam)
library(doParallel)
library(kableExtra)


data <-  read.csv('train.csv')

org_data <- data
# missing values per columns 
na_vals <- colSums(is.na(data))
na_vals
#ID              Customer_ID                    Month                     Name                      Age                      SSN               Occupation 
#0                        0                        0                        0                        0                        0                        0 
#Annual_Income    Monthly_Inhand_Salary        Num_Bank_Accounts          Num_Credit_Card            Interest_Rate              Num_of_Loan             Type_of_Loan 
#0                    15002                        0                        0                        0                        0                        0 
#Delay_from_due_date   Num_of_Delayed_Payment     Changed_Credit_Limit     Num_Credit_Inquiries               Credit_Mix         Outstanding_Debt Credit_Utilization_Ratio 
#0                        0                        0                     1965                        0                        0                        0 
#Credit_History_Age    Payment_of_Min_Amount      Total_EMI_per_month  Amount_invested_monthly        Payment_Behaviour          Monthly_Balance             Credit_Score 
#9030                        0                        0                        0                        0                        0                        0 

## Table contains the columns and their description 
dset_cols <- tibble(Name = 'ID', 
                    Description = 'Represents a unique identification of an entry',
                    #                    Notes='100000 Unique and clean values',
                    Processing_Required='None')
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Customer ID', 
                                        Description = 'Represents a unique identification of a person',
                                        Processing_Required='None'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Month', 
                                        Description = 'Represents the month of the year',
                                        Processing_Required='Strings to integers encoding'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Name', 
                                        Description = 'Represents the name of a person',
                                        Processing_Required='None, since this column will not be used in the models'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Age', 
                                        Description = 'Represents the age of the person',
                                        Processing_Required='Removing the "_" characters, convertting to integers and outliers handling'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'SSN', 
                                        Description = 'Represents the social security number of a person',
                                        Processing_Required='None, since this column will not be used in the models'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Occupation', 
                                        Description = 'Represents the occupation of the person',
                                        Processing_Required='Handling the "------" values, then encode strings to integers'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Annual Income', 
                                        Description = 'Represents the annual income of the person',
                                        Processing_Required='Removing the "_" characters, convertting to numbers and outliers handling'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Monthly Inhand Salary', 
                                        Description = 'Represents the monthly base salary of a person',
                                        Processing_Required='NA Handling'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Num Bank Accounts', 
                                        Description = 'Represents the number of bank accounts a person holds',
                                        Processing_Required='Outliers handling'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Num Credit Card', 
                                        Description = 'Represents the number of other credit cards held by a person',
                                        Processing_Required='Outliers handling'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Interest Rate', 
                                        Description = 'Represents the interest rate on credit card',
                                        Processing_Required='Outliers handling'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Num of Loan', 
                                        Description = 'Represents the number of loans taken from the bank',
                                        Processing_Required='Removing the "_" characters, convertting to integers and outliers handling'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Type of Loan', 
                                        Description = 'Represents the types of loan taken by a person',
                                        Processing_Required='Split over "," then convert to one-hot-coding columns'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Delay from due date', 
                                        Description = 'Represents the average number of days delayed from the payment date',
                                        Processing_Required='None'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Num of Delayed Payment', 
                                        Description = 'Represents the average number of payments delayed by a person',
                                        Processing_Required='Remove the implicit missing values ("_","", and negatives), then impute them'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Changed Credit Limit', 
                                        Description = 'Represents the percentage change in credit card limit',
                                        Processing_Required='Replace the "_" with zeros'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Num Credit Inquiries', 
                                        Description = 'Represents the number of credit card inquiries',
                                        Processing_Required='NA Handling'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Credit Mix', 
                                        Description = 'Represents the classification of the mix of credits',
                                        Processing_Required='Strings to integers encoding'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Outstanding Debt', 
                                        Description = 'Represents the remaining debt to be paid (in USD)',
                                        Processing_Required='Removing the "_" characters, convertting to numbers'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Credit Utilization Ratio', 
                                        Description = 'Represents the utilization ratio of credit card',
                                        Processing_Required='None'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Credit History Age', 
                                        Description = 'Represents the age of credit history of the person',
                                        Processing_Required='Split into year and month (one-hot-coding), then NA handling'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Payment of Min Amount', 
                                        Description = 'Represents whether only the minimum amount was paid by the person',
                                        Processing_Required='Strings to integers encoding'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Total EMI per month', 
                                        Description = 'Represents the monthly EMI payments (in USD)',
                                        Processing_Required='Outliers handling'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Amount invested monthly', 
                                        Description = 'Represents the monthly amount invested by the customer (in USD)',
                                        Processing_Required='Remove the implicit missing values ("", and "__10000__"), then impute them'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Payment Behaviour', 
                                        Description = 'Represents the payment behavior of the customer (in USD)',
                                        Processing_Required='Strings to integers encoding'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Monthly Balance', 
                                        Description = 'Represents the monthly balance amount of the customer (in USD)',
                                        Processing_Required='Remove the implicit missing values (empty values), then impute them'));
dset_cols <- bind_rows(dset_cols,tibble(Name = 'Credit_Score', 
                                        Description = 'Represents the bracket of credit score (Poor, Standard, Good)',
                                        Processing_Required='Strings to integers encoding'));


## First will start by handling the NA values for Monthly_Inhand_Salary, Num_Credit_Inquiries and Credit_History_Age

## Monthly_Inhand_Salary, NA Handling
head(data%>%select(Customer_ID,Monthly_Inhand_Salary))
# for the same customer, usually this value is the same, I will take the average value as the mode value for each customer. 
# find the average for the non NA values for each customer. 
mis_mod <- data %>% filter(!is.na(Monthly_Inhand_Salary)) %>% group_by(Customer_ID) %>% summarise(mode = mean(Monthly_Inhand_Salary))
# replace the NA values with the average value for each customer 
data <- data%>%left_join(mis_mod,by='Customer_ID')%>%mutate(Monthly_Inhand_Salary=ifelse(is.na(Monthly_Inhand_Salary),mode,Monthly_Inhand_Salary))
# check again 
Monthly_Inhand_Salary_validation_sample <- head(data%>%select(Customer_ID,Monthly_Inhand_Salary,mode))
Monthly_Inhand_Salary_validation_sample
#remove the mode
data <- data %>% select(-mode)


## Num_Credit_Inquiries 
head(data%>%select(Customer_ID,Num_Credit_Inquiries),n=20)
# for the same customer, usually this value is the same, I will take the average value as the mode value for each customer. 
# find the average for the non NA values for each customer. 
mis_mod <- data %>% filter(!is.na(Num_Credit_Inquiries)) %>% group_by(Customer_ID) %>% summarise(mode = mean(Num_Credit_Inquiries))
# replace the NA values with the average value for each customer 
data <- data%>%left_join(mis_mod,by='Customer_ID')%>%mutate(Num_Credit_Inquiries=ifelse(is.na(Num_Credit_Inquiries),mode,Num_Credit_Inquiries))
# check again 
Num_Credit_Inquiries_validation_sample <- head(data%>%select(Customer_ID,Num_Credit_Inquiries,mode),n=20)
Num_Credit_Inquiries_validation_sample
#remove the mode
data <- data %>% select(-mode)


## Credit_History_Age 
head(data%>%select(Customer_ID,Credit_History_Age),n=20)
## Sample 
#1    CUS_0xd40  22 Years and 1 Months
#2    CUS_0xd40                   <NA>
#3    CUS_0xd40  22 Years and 3 Months
# More tricky than the others, because it will be 1 month than the one before, will convert it into two columns, one for year, the other for month 
# will split over " ", the first value (index 1) is the year and  the forth (index 4) is the month 
data$Credit_History_Age_Year <- sapply(data$Credit_History_Age,function(string){
  # Split over the space, then take the first value for the year 
  parts <- str_split(string," ")[[1]]
  as.integer(parts[1])
})
data$Credit_History_Age_Month <- sapply(data$Credit_History_Age,function(string){
  # Split over the space, then take the forth value for the year 
  parts <- str_split(string," ")[[1]]
  as.integer(parts[4])
})

credit_history_age_aftersplit_sample <- head(data%>%select(Customer_ID,Credit_History_Age,Credit_History_Age_Year,Credit_History_Age_Month),n=20)
## All Good, now will check if we have two more than one missing values per customer 
data%>%group_by(Customer_ID)%>%filter( is.na(Credit_History_Age_Year)) %>% summarise(missing=n()) %>% filter(missing>1) %>%summarise(total_count=n())
# A tibble: 1 × 1
#total_count
#<int>
#  1        1984
#We have 1984 customers with more than 1 missing credit history entry. 
#Those might be subsequent or not. 

#This function to handle one pass through all missing values, it checks the ones before and after every missing value
#Since we may have 3 NA or more in a row, we will call this function many time until everything is handled. 
#In case of 3 NAs in a row as example, the first call of the function will handle the first and the last NA rows, the next call will handel the one at the middle
fix_credit_history_age_1pass <- function(dta)
{
  # find the indices of the entries which has the year as NA, whenever year is NA, month will be NA as well
  nas <- which(is.na(dta$Credit_History_Age_Year))
  for(nai in unique(nas))
  {
    current <- dta[nai,] # the entry which has the NA 
    before <- dta[nai-1,] # th entry before it
    after <- dta[nai+1,] # the entry after it 
    #check if the one before belongs to same customer and not NA 
    if(current$Customer_ID==before$Customer_ID&&!is.na(before$Credit_History_Age_Year))
    {
      # if so, make the year and month of the current entry equal to the one before plus add 1 to the month 
      current$Credit_History_Age_Year = before$Credit_History_Age_Year
      current$Credit_History_Age_Month = before$Credit_History_Age_Month+1  # 1 month after
      if(current$Credit_History_Age_Month>11) # fix the month year issue 
      {
        current$Credit_History_Age_Year <- current$Credit_History_Age_Year+1 
        current$Credit_History_Age_Month <-0 
      }
    }
    #check if the one after belongs to same customer and not NA 
    else if(current$Customer_ID==after$Customer_ID&&!is.na(after$Credit_History_Age_Year))
    {
      current$Credit_History_Age_Year = after$Credit_History_Age_Year
      current$Credit_History_Age_Month = after$Credit_History_Age_Month-1 # 1 month before
      if(current$Credit_History_Age_Month>11)# fix the month year issue 
      {
        current$Credit_History_Age_Year <- current$Credit_History_Age_Year+1 
        current$Credit_History_Age_Month <-0 
      }
    }
    dta[nai,] = current;
  }
  dta # return the dataset again
}
## we will call the function many times until all NA go away, 
data%>%group_by(Customer_ID)%>%filter( is.na(Credit_History_Age_Year)) %>% summarise(missing=n()) %>% filter(missing>0) %>%summarise(total_count=n())
# We have 6650 rows with NAs
## first of the function  
data<- fix_credit_history_age_1pass(data)
#Check How many NA rows left? 
data%>%group_by(Customer_ID)%>%filter( is.na(Credit_History_Age_Year)) %>% summarise(missing=n()) %>% filter(missing>0) %>%summarise(total_count=n())
# Now only 107 missing, another pass 
data<- fix_credit_history_age_1pass(data)
data%>%group_by(Customer_ID)%>%filter( is.na(Credit_History_Age_Year)) %>% summarise(missing=n()) %>% filter(missing>0) %>%summarise(total_count=n())
# Now only 9 missing, another pass 
data<- fix_credit_history_age_1pass(data)
data%>%group_by(Customer_ID)%>%filter( is.na(Credit_History_Age_Year)) %>% summarise(missing=n()) %>% filter(missing>0) %>%summarise(total_count=n())
# Now only 1 missing, another pass 
data<- fix_credit_history_age_1pass(data)
data%>%group_by(Customer_ID)%>%filter( is.na(Credit_History_Age_Year)) %>% summarise(missing=n()) %>% filter(missing>0) %>%summarise(total_count=n())
# Now, 0 missing, all done 
# last check 
credit_history_age_validation_sample <- head(data%>%select(Customer_ID,Credit_History_Age,Credit_History_Age_Year,Credit_History_Age_Month),n=20)
credit_history_age_validation_sample
# All done, including the 12 month will go into one full year ( the last row), now dropping the original column: Credit_History_Age 
data <- data %>% select(-Credit_History_Age)



#REF_CKP: after_na_ckpt
data_after_removing_explcit_na <- data






## Handling the Implicit missing values

##Some entries have values that indicate a missing value, like - or ----, etc. In this section we will handle those values, in our dataset, those values appear for the columns that supposed to be either integers or numeric but they are strings, I will also examine the string columns, the following code will show a snapshot of our dataset: 

str(data_after_removing_explcit_na)
#Monthly_Inhand_Salary, Num_Bank_Accounts, Num_Credit_Card, Interest_Rate, Delay_from_due_date, Num_Credit_Inquiries, Credit_Utilization_Ratio, Total_EMI_per_month, Credit_History_Age_Year, and Credit_History_Age_Month are already fine and they don't have any implicit missing values, I will process the others one by one. 



## Age 
data %>% filter(str_detect(Age, "^[0-9,.,-]+$")==FALSE | Age=='') %>% select(Age) %>% unique() %>% top_n(10)
# The extra _ are the problems will remove them now 
data$Age<- str_replace_all(data$Age, "_", "")
# try to convert it to integer again 
tmp_age <- as.integer(data$Age) 
# It works!, then make it persistent 
data$Age <- as.integer(data$Age)
rm(tmp_age)

## Annual_Income 
data %>% filter(str_detect(Annual_Income, "^[0-9,.,-]+$")==FALSE| Annual_Income=='') %>% select(Annual_Income) %>% unique() %>% top_n(10)
# The extra _ are the problems will remove them now 
data$Annual_Income<- str_replace_all(data$Annual_Income, "_", "")
# try to convert it to numeric again 
tmp_Annual_Income <- as.numeric(data$Annual_Income) 
# It works!, then make it persistent 
data$Annual_Income <- as.numeric(data$Annual_Income)
rm(tmp_Annual_Income)

## Num_of_Loan 
data %>% filter(str_detect(Num_of_Loan, "^[0-9,.,-]+$")==FALSE | Num_of_Loan=='') %>% select(Num_of_Loan) %>% unique() %>% top_n(10)
# The extra _ are the problems will remove them now 
data$Num_of_Loan<- str_replace_all(data$Num_of_Loan, "_", "")
# try to convert it to numeric again 
tmp_Num_of_Loan <- as.integer(data$Num_of_Loan) 
# It works!, then make it persistent 
data$Num_of_Loan <- as.integer(data$Num_of_Loan)
rm(tmp_Num_of_Loan)

## Num_of_Delayed_Payment 
data %>% filter(str_detect(Num_of_Delayed_Payment, "^[0-9,.,-]+$") == FALSE | Num_of_Delayed_Payment=='') %>% select(Num_of_Delayed_Payment) %>% unique()
# The extra _ are the problems will remove them now, also there are empty strings and negative numbers 
data$Num_of_Delayed_Payment<- str_replace_all(data$Num_of_Delayed_Payment, "_", "")
#Will assume the empty strings and the negative values to be NA, then we will impute them with the average number per customer 
data$Num_of_Delayed_Payment<- ifelse(data$Num_of_Delayed_Payment=="",NA,data$Num_of_Delayed_Payment)
data$Num_of_Delayed_Payment <- as.integer(data$Num_of_Delayed_Payment)
data$Num_of_Delayed_Payment<- ifelse(data$Num_of_Delayed_Payment<0 ,NA,data$Num_of_Delayed_Payment)

#now find the average per customer 
mis_mod <- data %>% filter(!is.na(Num_of_Delayed_Payment)) %>% group_by(Customer_ID) %>% summarise(mode = mean(Num_of_Delayed_Payment))
data <- data%>%left_join(mis_mod,by='Customer_ID')%>%mutate(Num_of_Delayed_Payment=ifelse(is.na(Num_of_Delayed_Payment),mode,Num_of_Delayed_Payment))
head(data%>%select(Customer_ID,Num_of_Delayed_Payment,mode),n=20)
#remove the mode
data <- data %>% select(-mode)
data$Num_of_Delayed_Payment <- as.integer(data$Num_of_Delayed_Payment) #convert numeric to integers 

## Changed_Credit_Limit 
data %>% filter(str_detect(Changed_Credit_Limit, "^[0-9,.,-]+$")==FALSE | Changed_Credit_Limit=='') %>% select(Changed_Credit_Limit) %>% unique() 
# it should be numeric, the only textual value is "_" which should be denoting 0 
data <- data %>% mutate(Changed_Credit_Limit = ifelse(Changed_Credit_Limit=='_','0',Changed_Credit_Limit))
data$Changed_Credit_Limit <- as.numeric(data$Changed_Credit_Limit)

## Outstanding_Debt 
data %>% filter(str_detect(Outstanding_Debt, "^[0-9,.,-]+$")==FALSE | Outstanding_Debt=='') %>% select(Outstanding_Debt) %>% unique() 
# The extra _ are the problems will remove them now 
data$Outstanding_Debt<- str_replace_all(data$Outstanding_Debt, "_", "")
# try to convert it to numeric again 
tmp_Outstanding_Debt <- as.numeric(data$Outstanding_Debt) 
# It works!, then make it persistent 
data$Outstanding_Debt <- as.numeric(data$Outstanding_Debt)
rm(tmp_Outstanding_Debt)


## Amount_invested_monthly 
data %>% filter(str_detect(Amount_invested_monthly, "^[0-9,.,-]+$")==FALSE | Amount_invested_monthly=='') %>% select(Amount_invested_monthly) %>% unique() 
## some values are just empty strings
## the value __10000__ seems to have a special meaning, also the empty values.
## I will change them both the average value per customer. 
data$Amount_invested_monthly <- as.numeric(data$Amount_invested_monthly)
#Now those values are NAs 
mis_mod <- data %>% filter(!is.na(Amount_invested_monthly)) %>% group_by(Customer_ID) %>% summarise(mode = mean(Amount_invested_monthly))
data <- data%>%left_join(mis_mod,by='Customer_ID')%>%mutate(Amount_invested_monthly=ifelse(is.na(Amount_invested_monthly),mode,Amount_invested_monthly))
data = data%>%select(-(mode))

## Monthly_Balance 
data %>% filter(str_detect(Monthly_Balance, "^[0-9,.,-]+$")==FALSE | Monthly_Balance=='') %>% select(Monthly_Balance) %>% unique() 
# some empty values, and weird __-333333333333333333333333333__ value both will be considered as NA and will be imputed per Customer 
data$Monthly_Balance <- as.numeric(data$Monthly_Balance)
#Now those values are NAs 
mis_mod <- data %>% filter(!is.na(Monthly_Balance)) %>% group_by(Customer_ID) %>% summarise(mode = mean(Monthly_Balance))
data <- data%>%left_join(mis_mod,by='Customer_ID')%>%mutate(Monthly_Balance=ifelse(is.na(Monthly_Balance),mode,Monthly_Balance))
data = data%>%select(-(mode))








## Fixing the outliers 
#Like what I did with credit history year and month, I will use an iterative approach, but this time if the value suspected to be an outlier is greater than X*before_value or grater than X*after_value where X is 
#our threshold multiplier then this value will be the before_value or after_value 

##dta is the dataset, column_index is the index of the column to be fixed, for example Age has index of 5, X is the multiplication factor to determine if the value is in the exterme outlier 
fix_outlier_1pass <- function(dta,column_index,X)
{
  # get all the suspected outlier values for the column at column_index using the boxplot function. 
  bp <- boxplot(dta[,column_index])
  # loop through all suspected values, unique is used here in order not to process the same value twice 
  for (suspected_value in unique(bp$out))
  {
    # find the location of the suspected outlier value for the column given at column_index 
    locations = which(dta[,column_index] == suspected_value)
    for (location in locations) # Loop through all location where the suspected value found 
    {
      current <- dta[location, ] # the row for value of interest 
      before <- dta[location - 1, ] # the row before it 
      after <- dta[location + 1, ] # the row after 
      # if the previous record for the same customer and the value for this record is more than X time the previous, then this is an extreme outlier
      if (abs(current[,column_index]) > abs(before[,column_index]) * X &&
          current$Customer_ID == before$Customer_ID) # replaced with the one before 
      {
        current[,column_index] = before[,column_index]
      }
      # if the next record for the same customer and the value for this record is more than X time the next, then this is an extreme outlier
      else if (abs(current[,column_index]) > abs(after[,column_index]) * X &&
               current$Customer_ID == after$Customer_ID)
      {
        current[,column_index] = after[,column_index]# replaced with the one after 
      }
      dta[location, ] = current # update the row
    }
  }
  dta # return the updated dataset 
}



## Age 
boxplot(data$Age)
age_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,5,2)
boxplot(data$Age)
age_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,5,2)
boxplot(data$Age)
age_bp3 <- recordPlot()

data <- fix_outlier_1pass(data,5,2)
boxplot(data$Age)
age_bp4 <- recordPlot()

data <- fix_outlier_1pass(data,5,2)
boxplot(data$Age)
age_bp5 <- recordPlot()


## Annual_Income 
# Index: 8
boxplot(data$Annual_Income)
Annual_Income_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,8,2)
boxplot(data$Annual_Income)
Annual_Income_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,8,2)
boxplot(data$Annual_Income)
Annual_Income_bp3 <- recordPlot()

data <- fix_outlier_1pass(data,8,2)
boxplot(data$Annual_Income)
Annual_Income_bp4 <- recordPlot()

data <- fix_outlier_1pass(data,8,2)
boxplot(data$Annual_Income)
Annual_Income_bp5 <- recordPlot()


## Monthly_Inhand_Salary 
# Index: 9
boxplot(data$Monthly_Inhand_Salary)
Monthly_Inhand_Salary_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,9,2)
boxplot(data$Annual_Income)
Monthly_Inhand_Salary_bp2 <- recordPlot()

## Num_Bank_Accounts
# Index: 10
boxplot(data$Num_Bank_Accounts)
Num_Bank_Accounts_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,10,2)
boxplot(data$Num_Bank_Accounts)
Num_Bank_Accounts_bp2 <- recordPlot()


## Num_Credit_Card
# Index: 11
boxplot(data$Num_Credit_Card)
Num_Credit_Card_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,11,2)
boxplot(data$Num_Credit_Card)
Num_Credit_Card_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,11,2)
boxplot(data$Num_Credit_Card)
Num_Credit_Card_bp3 <- recordPlot()

data <- fix_outlier_1pass(data,11,2)
boxplot(data$Num_Credit_Card)
Num_Credit_Card_bp4 <- recordPlot()


## Interest_Rate
# Index: 12
boxplot(data$Interest_Rate)
Interest_Rate_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,12,2)
boxplot(data$Interest_Rate)
Interest_Rate_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,12,2)
boxplot(data$Interest_Rate)
Interest_Rate_bp3 <- recordPlot()

data <- fix_outlier_1pass(data,12,2)
boxplot(data$Interest_Rate)
Interest_Rate_bp4 <- recordPlot()


## Num_of_Loan
# Index: 13
boxplot(data$Num_of_Loan)
Num_of_Loan_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,13,2)
boxplot(data$Num_of_Loan)
Num_of_Loan_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,13,2)
boxplot(data$Num_of_Loan)
Num_of_Loan_bp3 <- recordPlot()

data <- fix_outlier_1pass(data,13,2)
boxplot(data$Num_of_Loan)
Num_of_Loan_bp4 <- recordPlot()


## Delay_from_due_date
# Index: 15
boxplot(data$Delay_from_due_date)
Delay_from_due_date_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,15,2)
boxplot(data$Delay_from_due_date)
Delay_from_due_date_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,15,2)
boxplot(data$Delay_from_due_date)
Delay_from_due_date_bp3 <- recordPlot()


## Num_of_Delayed_Payment
# Index: 16
boxplot(data$Num_of_Delayed_Payment)
Num_of_Delayed_Payment_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,16,2)
boxplot(data$Num_of_Delayed_Payment)
Num_of_Delayed_Payment_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,16,2)
boxplot(data$Num_of_Delayed_Payment)
Num_of_Delayed_Payment_bp3 <- recordPlot()

data <- fix_outlier_1pass(data,16,2)
boxplot(data$Num_of_Delayed_Payment)
Num_of_Delayed_Payment_bp4 <- recordPlot()

## Changed_Credit_Limit
# Index: 17
boxplot(data$Changed_Credit_Limit)
Changed_Credit_Limit_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,17,2)
boxplot(data$Changed_Credit_Limit)
Changed_Credit_Limit_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,17,2)
boxplot(data$Changed_Credit_Limit)
Changed_Credit_Limit_bp3 <- recordPlot()

data <- fix_outlier_1pass(data,17,2)
boxplot(data$Changed_Credit_Limit)
Changed_Credit_Limit_bp4 <- recordPlot()


## Num_Credit_Inquiries
# Index: 18
boxplot(data$Num_Credit_Inquiries)
Num_Credit_Inquiries_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,18,2)
boxplot(data$Num_Credit_Inquiries)
Num_Credit_Inquiries_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,18,2)
boxplot(data$Num_Credit_Inquiries)
Num_Credit_Inquiries_bp3 <- recordPlot()

data <- fix_outlier_1pass(data,18,2)
boxplot(data$Num_Credit_Inquiries)
Num_Credit_Inquiries_bp4 <- recordPlot()

data <- fix_outlier_1pass(data,18,2)
boxplot(data$Num_Credit_Inquiries)
Num_Credit_Inquiries_bp5 <- recordPlot()


## Outstanding_Debt
# Index: 20 
boxplot(data$Outstanding_Debt)
Outstanding_Debt_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,20,2)
boxplot(data$Outstanding_Debt)
Outstanding_Debt_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,20,2)
boxplot(data$Outstanding_Debt)
Outstanding_Debt_bp3 <- recordPlot()

data <- fix_outlier_1pass(data,20,2)
boxplot(data$Outstanding_Debt)
Outstanding_Debt_bp4 <- recordPlot()


## Credit_Utilization_Ratio
# Index: 21 
boxplot(data$Credit_Utilization_Ratio)
Credit_Utilization_Ratio_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,21,2)
boxplot(data$Credit_Utilization_Ratio)
Credit_Utilization_Ratio_bp2 <- recordPlot()


## Total_EMI_per_month
# Index: 23
boxplot(data$Total_EMI_per_month)
Total_EMI_per_month_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,23,2)
boxplot(data$Total_EMI_per_month)
Total_EMI_per_month_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,23,2)
boxplot(data$Total_EMI_per_month)
Total_EMI_per_month_bp3 <- recordPlot()

data <- fix_outlier_1pass(data,23,2)
boxplot(data$Total_EMI_per_month)
Total_EMI_per_month_bp4 <- recordPlot()

data <- fix_outlier_1pass(data,23,2)
boxplot(data$Total_EMI_per_month)
Total_EMI_per_month_bp5 <- recordPlot()

data <- fix_outlier_1pass(data,23,2)
boxplot(data$Total_EMI_per_month)
Total_EMI_per_month_bp6 <- recordPlot()

## Amount_invested_monthly
# Index: 24
boxplot(data$Amount_invested_monthly)
Amount_invested_monthly_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,24,2)
boxplot(data$Amount_invested_monthly)
Amount_invested_monthly_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,24,2)
boxplot(data$Amount_invested_monthly)
Amount_invested_monthly_bp3 <- recordPlot()

data <- fix_outlier_1pass(data,24,2)
boxplot(data$Amount_invested_monthly)
Amount_invested_monthly_bp4 <- recordPlot()

data <- fix_outlier_1pass(data,24,2)
boxplot(data$Amount_invested_monthly)
Amount_invested_monthly_bp5 <- recordPlot()


## Monthly_Balance
# Index: 26
boxplot(data$Monthly_Balance)
Monthly_Balance_bp1 <- recordPlot()

data <- fix_outlier_1pass(data,26,2)
boxplot(data$Monthly_Balance)
Monthly_Balance_bp2 <- recordPlot()

data <- fix_outlier_1pass(data,26,2)
boxplot(data$Monthly_Balance)
Monthly_Balance_bp3 <- recordPlot()

data <- fix_outlier_1pass(data,26,2)
boxplot(data$Monthly_Balance)
Monthly_Balance_bp4 <- recordPlot()

data <- fix_outlier_1pass(data,26,2)
boxplot(data$Monthly_Balance)
Monthly_Balance_bp5 <- recordPlot()


#REF_CKP: after_outlier_ckpt
data_after_outlier_na <- data



#### handling the strings columns 

## Month
unique(data$Month) 
#All good now encode string as integers 
#No strange values, convert the months to integers: Jan:1 Feb:2 and so on 
data <- data%>%mutate(Month=case_when(
  Month == "January"  ~1,
  Month == "February" ~2,
  Month == "March"    ~3,
  Month == "April"    ~4,
  Month == "May"      ~5,
  Month == "June"     ~6,
  Month == "July"     ~7,
  Month == "August"  ~8
))
unique(data$Month)



## Occupation 
unique(data$Occupation)
#[1] "Scientist"     "_______"       "Teacher"       "Engineer"      "Entrepreneur"  "Developer"     "Lawyer"        "Media_Manager" "Doctor"        "Journalist"    "Manager"      
#[12] "Accountant"    "Musician"      "Mechanic"      "Writer"        "Architect"    
# All good except the  "_______", let's see it in action 
head(data%>%select(Customer_ID,Occupation),n=20)
#Same as the age, occupation will not change frequently for the same customer from month to month
#The ------ value indicates a not entered value, for the same customer it will be the same as before 
# or after month, will start by converting it to NA to make the further processing easier 
data<- data%>% mutate(Occupation=ifelse(Occupation=="_______",NA,Occupation))
head(data%>%select(Customer_ID,Occupation),n=20)

fixOcuupation_1pass <- function(dta)
{
  nas <- unique(which(is.na(dta$Occupation)))
  for(nai in nas)
  {
    current <- dta[nai,]
    before <- dta[nai-1,]
    after <- dta[nai+1,]
    #check if the one before belongs to same customer and not na 
    if(current$Customer_ID==before$Customer_ID&&!is.na(before$Occupation))
    {
      current$Occupation = before$Occupation
    }
    else if(current$Customer_ID==after$Customer_ID&&!is.na(after$Occupation))
    {
      current$Occupation = after$Occupation
    }
    dta[nai,] = current;
  }
  dta
}
data %>% filter(is.na(Occupation))%>%summarise(missing=n())
#7062 cases 
data <- fixOcuupation_1pass(data)
data %>% filter(is.na(Occupation))%>%summarise(missing=n())
#Now 70 only, one more pass 
data <- fixOcuupation_1pass(data)
data %>% filter(is.na(Occupation))%>%summarise(missing=n())
#Now 5 only, one more pass 
data <- fixOcuupation_1pass(data)
data %>% filter(is.na(Occupation))%>%summarise(missing=n())
#Now 0, all done 


#Changing the occupation to integers 
data <- data%>%mutate(Occupation=case_when(
  Occupation == "Scientist" ~1,
  Occupation == "Teacher" ~2,
  Occupation == "Engineer" ~3,
  Occupation == "Entrepreneur" ~4,
  Occupation == "Developer" ~5,
  Occupation == "Lawyer" ~6,
  Occupation == "Media_Manager" ~7,
  Occupation == "Doctor" ~8,
  Occupation == "Journalist" ~9,
  Occupation == "Manager" ~10,
  Occupation == "Accountant" ~11,
  Occupation == "Musician" ~12,
  Occupation == "Mechanic" ~13,
  Occupation == "Writer" ~14,
  Occupation == "Architect" ~15    
))







## Type_of_Loan 
unique(data$Type_of_Loan)
# Looking at the loans type, I can see 
#Credit-Builder Loan,Auto Loan,Not Specified,Mortgage Loan,Student Loan,Personal Loan,Debt Consolidation Loan,Payday Loan,Home Equity Loan
#Split them as one-hot-coding 
data <- data %>% mutate(
  Type_of_Loan.Credit_Builder = str_detect(Type_of_Loan,"Credit-Builder"),
  Type_of_Loan.Auto = str_detect(Type_of_Loan,"Auto Loan"),
  Type_of_Loan.Not_Specified = str_detect(Type_of_Loan,"Not Specified"),
  Type_of_Loan.Mortgage = str_detect(Type_of_Loan,"Mortgage Loan"),
  Type_of_Loan.Student = str_detect(Type_of_Loan,"Student Loan"),
  Type_of_Loan.Personal = str_detect(Type_of_Loan,"Personal Loan"),
  Type_of_Loan.Debt_Consolidation = str_detect(Type_of_Loan,"Debt Consolidation Loan"),
  Type_of_Loan.Payday = str_detect(Type_of_Loan,"Payday Loan"),
  Type_of_Loan.Home_Equity = str_detect(Type_of_Loan,"Home Equity Loan"),
)
# Drop the Loan Type 
type_of_loan_processed_sample = head(data)
type_of_loan_processed_sample
data <- data %>% select(-Type_of_Loan)





## Credit_Mix : Represents the classification of the mix of credits
unique(data$Credit_Mix)
#What is the percentage for each 
data %>% group_by(Credit_Mix) %>% summarise(count = n()/length(data$Credit_Mix)*100)
# 20% is for the _, the only reasoning I can found for the _ is the "Uknown" which a separate class. 
#Now change the classes into numbers: -: 0, Bad: 1, Standard: 2: Good 3: 
data <- data %>% mutate(Credit_Mix = case_when(
  Credit_Mix == '_' ~ 0, 
  Credit_Mix == 'Bad' ~ 1, 
  Credit_Mix == 'Standard' ~ 2, 
  Credit_Mix == 'Good' ~ 3
))




##Payment_Behaviour 
unique(data$Payment_Behaviour)
#strange value of !@9#%8 
sample(data%>%select(Customer_ID,Payment_Behaviour))
data %>% group_by(Payment_Behaviour) %>% summarise(count = n()/length(data$Payment_Behaviour)*100)
#I couldn't find any relation, I will assume it to be a separte category 
#Convert to integers 
data <- data %>% mutate(Payment_Behaviour = case_when(
  Payment_Behaviour == '!@9#%8' ~ 0, 
  Payment_Behaviour == 'High_spent_Large_value_payments' ~ 1, 
  Payment_Behaviour == 'High_spent_Medium_value_payments' ~ 2, 
  Payment_Behaviour == 'High_spent_Small_value_payments' ~ 3, 
  Payment_Behaviour == 'Low_spent_Large_value_payments' ~ 4, 
  Payment_Behaviour == 'Low_spent_Medium_value_payments' ~ 5, 
  Payment_Behaviour == 'Low_spent_Small_value_payments' ~ 6, 
))


##Credit_Score
unique(data$Credit_Score)
data %>% group_by(Credit_Score) %>% summarise(count = n()/length(data$Credit_Score)*100)

#Replace with integer: 0:Poor, 1:Standard, 2:Good 
data <- data %>% mutate(Credit_Score = case_when(
  Credit_Score == 'Poor' ~ 0, 
  Credit_Score == 'Standard' ~ 1, 
  Credit_Score == 'Good' ~ 2
  
))



## Dataset final touches and splitting 

data_copy = data; 

data_copy$Credit_Score = as.factor(data_copy$Credit_Score) 
data_copy = data_copy%>% select(-Customer_ID)
data_copy = data_copy%>% select(-Name)
data_copy = data_copy%>% select(-SSN)
data_copy = data_copy%>% select(-ID)


data_copy <- data_copy%>%mutate(
  Month = as.factor(Month),
  Occupation = as.factor(Occupation),
  Credit_Mix = as.factor(Credit_Mix),
  Payment_of_Min_Amount = as.factor(Payment_of_Min_Amount),
  Payment_Behaviour = as.factor(Payment_Behaviour),
  Credit_History_Age_Year = as.factor(Credit_History_Age_Year),
  Credit_History_Age_Month = as.factor(Credit_History_Age_Month),
  Type_of_Loan.Credit_Builder = as.factor(Type_of_Loan.Credit_Builder),
  Type_of_Loan.Auto = as.factor(Type_of_Loan.Credit_Builder),
  Type_of_Loan.Not_Specified = as.factor(Type_of_Loan.Not_Specified),
  Type_of_Loan.Mortgage = as.factor(Type_of_Loan.Mortgage),
  Type_of_Loan.Student = as.factor(Type_of_Loan.Student),
  Type_of_Loan.Personal = as.factor(Type_of_Loan.Personal),
  Type_of_Loan.Debt_Consolidation = as.factor(Type_of_Loan.Debt_Consolidation),
  Type_of_Loan.Payday = as.factor(Type_of_Loan.Payday),
  Type_of_Loan.Home_Equity = as.factor(Type_of_Loan.Home_Equity)
)

set.seed(1, sample.kind="Rounding") # for backward compatability with old versions of R 
test_index <- createDataPartition(y = data_copy$Credit_Score, times = 1, p = 0.2, list = FALSE)
dataset_train <- data_copy[-test_index,]
dataset_test <- data_copy[test_index,]


### Features selection 

cl <- makePSOCKcluster(14)
registerDoParallel(cl)
control <- rfeControl(functions=rfFuncs, method="cv", number=5,verbose = TRUE)
results <- rfe(dataset_train[,-22], dataset_train[,22], sizes=c(1:4), rfeControl=control)
results
stopCluster(cl)


rfe.start_time <- Sys.time()
cl <- makePSOCKcluster(14) # 44GB of RAM, 10 threads (cv=10)+1 coordinating = 11*4 = 44 GB 
registerDoParallel(cl) 
control <- rfeControl(functions=rfFuncs, method="cv", number=10,verbose = TRUE)
ref_result<- rfe(dataset_train[,-22], dataset_train[,22], sizes=c(4,8,16,24,32), rfeControl=control)
rfe.end_time <- Sys.time()
ref_result
stopCluster(cl)

##Random Forest 

control <- trainControl(method='cv', 
                        number=5, 
                        search='grid')
#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(123)
#Number randomely variable selected is mtry
mtry <- c(8)
#tunegrid <- expand.grid(.mtry=mtry)
#tuneGrid <- expand.grid(.mtry=c(3:10))
# Define the grid of hyperparameters to be tuned
tuneGrid <- expand.grid(.mtry = mtry) # minimum size of terminal nodes
head(dataset_train)

sum(is.na(data$Credit_Score))
cl <- makePSOCKcluster(4)
registerDoParallel(cl)



rf_default <- train(Credit_Score~., 
                    data=dataset_train, 
                    method='rf', 
                    metric='Accuracy', 
                    trControl = control
)
print(rf_default)
stopCluster(cl)

test_predict <- predict(rf_default, dataset_test)
mean(test_predict==dataset_test$Diabetes_binary)



cl <- makePSOCKcluster(14)
registerDoParallel(cl)

knn_fit <- train(Credit_Score~., 
                 data=dataset_train, 
                 method='knn', 
                 metric='Accuracy', 
                 trControl = control
)
stopCluster(cl)

knn_fit




cl <- makePSOCKcluster(14)
registerDoParallel(cl)
rpart_fit <- train(Credit_Score~., 
                   data=dataset_train, 
                   method='rpart', 
                   metric='Accuracy', 
                   trControl = control
)
rpart_fit

stopCluster(cl)






gamLoess_fit
stopCluster(cl)

print(gamLoess_fit)

test_predict <- predict(gamLoess_fit, dataset_test)
mean(test_predict==dataset_test$Diabetes_binary)


cl <- makePSOCKcluster(4)
registerDoParallel(cl)

lm_fit <- train(Credit_Score~., 
                data=dataset_train, 
                method='gam', 
                metric='Accuracy', 
                trControl = control
)

print(lm_fit)

test_predict <- predict(lm_fit, dataset_test)
mean(test_predict==dataset_test$Diabetes_binary)




## Hyerparameter tunning, RF 
### Best Max Node 
max_acc = 0 ;
max_acc_model = '';
#Using only the best value of mtry
#tune_mtry <- expand.grid(.mtry = c(1: 7))
rf_tunning <-  tibble(ntree = 0, mtry = mtry,max_node=0,accuracy=0)
start.time <- Sys.time()

for (ntree in  seq(from = 100, to = 901, by = 200) )
{
  for (mtry in  seq(from = 2, to = 11, by = 2) ) {
    tune_mtry <- expand.grid(.mtry = mtry)
    for (mn in seq(from = 5, to = 200, by = 20) ) {
      set.seed(123)
      
      
      
      cl <- makePSOCKcluster(8)
      registerDoParallel(cl)
      
      
      model.start_time <- Sys.time()
      
      mNode <- train(Diabetes_binary~., 
                     data=dataset_train, 
                     method='rf', 
                     metric='Accuracy', 
                     tuneGrid=tune_mtry, 
                     ntree = ntree,
                     trControl=control,
                     importance = TRUE,
                     maxnodes = mn
      )
      model.end_time <- Sys.time()
      model.taken_time <- round(model.end_time - model.start_time,2)
      stopCluster(cl)
      
      current_model <- paste('ntree',toString(ntree),'mtry',toString(mtry),'maxnodes', toString(mn),'Accuracy',mNode$results['Accuracy'],'Taken Time', model.taken_time)
      print(current_model)
      if(mNode$results['Accuracy']$Accuracy>max_acc)
      {
        max_acc = mNode$results['Accuracy']$Accuracy
        max_acc_model = current_model
      }
      print(paste('Max Acc ',max_acc,' For ',max_acc_model))
      results <- tibble(ntree = ntree, mtry = mtry,max_node=mn,accuracy=mNode$results['Accuracy']$Accuracy,Taken_Time= model.taken_time)
      rf_tunning <- bind_rows(rf_tunning,results);
    }
  }
}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken



### Models building 
rfe_dataset_train = dataset_train[,ref_result[['optVariables']]]
rfe_dataset_test = dataset_test[,ref_result[['optVariables']]]
rfe_dataset_train$Credit_Score = dataset_train$Credit_Score
rfe_dataset_test$Credit_Score = dataset_test$Credit_Score

control <- trainControl(method='cv', 
                        number=5, 
                        search='grid')

test_accuracy <- function(model,dst)
{
  pred <- predict(model,dst); 
  mean(pred==dst$Credit_Score)
}

## Generalized Additive Model
cl <- makePSOCKcluster(14)
registerDoParallel(cl)
gam_rfe16.start.time <- Sys.time()
gam_rfe16 <- train(pred_exp16, 
                   data=dataset_train, 
                   method='gam', 
                   metric='Accuracy', 
                   trControl = control
)
gam_rfe16_.end.time <- Sys.time()
print(gam_rfe16)
stopCluster(cl)

plot(gam_rfe16)

gam_rfe16_test_acc <- test_accuracy(gam_rfe16,rfe_dataset_test)

##KNN

#First KNN 
cl <- makePSOCKcluster(14)
registerDoParallel(cl)
knn_rfe16.start.time <- Sys.time()

knn_rfe16 <- train(Credit_Score~., 
                            data=rfe_dataset_train, 
                            method='knn', 
                            metric='Accuracy', 
                            trControl = control
)
knn_rfe16.end.time <- Sys.time()

print(knn_rfe16)
stopCluster(cl)

plot(knn_rfe16)

# the values 5,7,9 are used for K, the best cross-validation accuracy was for K=5 
# let's try k= 3,4 as well 
cl <- makePSOCKcluster(14)
registerDoParallel(cl)
knnv2_rfe16.start.time <- Sys.time()

knnv2_rfe16 <- train(Credit_Score~., 
                   data=rfe_dataset_train, 
                   method='knn', 
                   metric='Accuracy', 
                   trControl = control,
                   tuneGrid = data.frame(k = c(3,4,5,7,9))
)
knnv2_rfe16.end.time <- Sys.time()

print(knnv2_rfe16)
stopCluster(cl)

plot(knnv2_rfe16)
#K3 is better than K=5, will this do better for testing? 



knn_rfe16_test_acc <- test_accuracy(knn_rfe16,rfe_dataset_test)
knnv2_rfe16_test_acc <- test_accuracy(knnv2_rfe16,rfe_dataset_test)



## Decision tree rpart (CART: Classification And Regression Trees)
cl <- makePSOCKcluster(14)
registerDoParallel(cl)
rpart_rfe16.start.time <- Sys.time()
rpart_rfe16 <- train(pred_exp16, 
                            data=dataset_train, 
                            method='rpart', 
                            metric='Accuracy', 
                            trControl = control
)
rpart_rfe16_.end.time <- Sys.time()
print(rpart_rfe16)
stopCluster(cl)

plot(rpart_rfe16)

#test with different values of cp 
cl <- makePSOCKcluster(14)
registerDoParallel(cl)
rpartv2_rfe16.start.time <- Sys.time()

rpartv2_rfe16 <- train(Credit_Score~., 
                     data=rfe_dataset_train, 
                     method='rpart', 
                     metric='Accuracy', 
                     trControl = control,
                     tuneGrid = data.frame(cp = c(0.0005,0.001,0.005,0.01,0.012,0.015,0.016,0.017,0.01748532,0.018,0.02,0.03,0.05,0.07,0.1,0.15696743,0.17,0.2))
)
rpartv2_rfe16.end.time <- Sys.time()

print(rpartv2_rfe16)
stopCluster(cl)

plot(rpartv2_rfe16)

rpart_rfe16_test_acc <- test_accuracy(rpart_rfe16,rfe_dataset_test)
rpartv2_rfe16_test_acc <- test_accuracy(rpartv2_rfe16,rfe_dataset_test)


## Random Forest 
cl <- makePSOCKcluster(14)
registerDoParallel(cl)
rf_rfe16.start.time <- Sys.time()
rf_rfe16 <- train(pred_exp16, 
                     data=dataset_train, 
                     method='rf', 
                     metric='Accuracy', 
                     trControl = control
)
rf_rfe16_.end.time <- Sys.time()
print(rf_rfe16)
stopCluster(cl)

plot(rf_rfe16)

# it peeks around 35, I will search few before and few after 
cl <- makePSOCKcluster(10)
registerDoParallel(cl)
rfv2_rfe16.start.time <- Sys.time()
rfv2_rfe16 <- train(pred_exp16, 
                  data=dataset_train, 
                  method='rf', 
                  metric='Accuracy', 
                  trControl = control,
                  tuneGrid = data.frame(mtry = seq(30,40,2))
                  
)
rfv2_rfe16_.end.time <- Sys.time()
print(rfv2_rfe16)
stopCluster(cl)

plot(rfv2_rfe16)


rf_rfe16_test_acc <- test_accuracy(rf_rfe16,rfe_dataset_test)
rfv2_rfe16_rfe16_test_acc <- test_accuracy(rfv2_rfe16,rfe_dataset_test)



models_info <- tibble('Model Name' = 'GAM', 
                    'CV Accuracy' = sprintf('%1.5f',gam_rfe16[["results"]][["Accuracy"]][1]),
                    'Testing Accuracy'= sprintf('%1.5f',gam_rfe16_test_acc),
                    'Tunning Parameter'=sprintf('select = %i, method = %s',gam_rfe16[["results"]][["select"]][1],gam_rfe16[["results"]][["method"]][1]))
models_info <- bind_rows(models_info,tibble('Model Name' = 'KNN', 
                                            'CV Accuracy' = sprintf('%1.5f',knnv2_rfe16[["results"]][["Accuracy"]][1]),
                                            'Testing Accuracy'= sprintf('%1.5f',knnv2_rfe16_test_acc),
                                            'Tunning Parameter'=sprintf('k = %d',knnv2_rfe16[["results"]][["k"]][1])))
models_info <- bind_rows(models_info,tibble('Model Name' = 'RPART', 
                                            'CV Accuracy' = sprintf('%1.5f',rpartv2_rfe16[["results"]][["Accuracy"]][1]),
                                            'Testing Accuracy'= sprintf('%1.5f',rpartv2_rfe16_test_acc),
                                            'Tunning Parameter'=sprintf('cp = %1.5f',rpartv2_rfe16[["results"]][["cp"]][1])))
models_info <- bind_rows(models_info,tibble('Model Name' = 'RF', 
                                            'CV Accuracy' = sprintf('%1.5f',rfv2_rfe16[["results"]][["Accuracy"]][1]),
                                            'Testing Accuracy'= sprintf('%1.5f',rfv2_rfe16_rfe16_test_acc),
                                            'Tunning Parameter'=sprintf('mtry = %d',rfv2_rfe16[["results"]][["mtry"]][1])))


## THE END
