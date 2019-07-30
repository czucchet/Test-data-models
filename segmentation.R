
### Review notes
# a) Need to take out customers in brands with rewards against non savings or PCA accounts
# b) Write up leaving date field
# c) Table doesn't have cost to serve - data is on customer, brand, product level, so @Tom Bentley ,think you should be able to calculate it from the data provided
# d) Brand for customer journeys detail table hasn't been transformed for 
# e) CJD table isn't built to customer who skip journeys, spent last night working on that and don't have a solution that doesn't require a really nested for loop and blow my cpu up. Would be good to talk to a data engineer who did that though to understand more
# f) Need to add brand to customer journeys table for applicants who didnt go through full journey 
# g) Need to join in my name of stages. Currently have step number, not step name
#ie 
#Step 1 Start     Step 1 Name(this isn't there
#1                       Application started       
#) Need to do append a last month customer base to this 
# h) Splitting out build of payment and customer - requirement to integrate at a later date
# New month doesn't factor in new product and brand movement against current month
# i) Have a revenue parameter to create within the table so that total revenue equals 1bn for instance
# j) Start in new month for new customers needs to be within that month
# k) Need to fix L1 to L4 complaints - need to link up

library(tidyverse);library(purrr);library(magrittr);library(caroline);library(tidyr);library(lubridate);library(RPostgreSQL);options(scipen=999);library(DBI)

dsol_proto = dbConnect(PostgreSQL(), user = "czucchet",
                       password = "EB%bti15ICh6NYX",
                       dbname = "dev",
                       host = "nonprod-dsol-prototyping-db.ctolc6xouppg.eu-west-1.rds.amazonaws.com", port = "5432")


########################################################
############### 0) Functions ###########################
roundUp <- function(x){round(x+4,-6)}



########################################################
############### 1) Variables ###########################
############### 1a) Customer table Variables ###########

### Bank based variables - change to adjust clients, products etc
client_name = "LBG"
cust = 500000;threshold = 100000
rev_year = 22000000000; rev_shift = 0.03;rev_amt = roundUp(rnorm(2,rev_year, (rev_shift*rev_year)))/12
profit_year = 6000000000; profit_shift = 0.03;pft_amt = roundUp(rnorm(2,profit_year, (profit_shift*profit_year)))/12
market_share = 0.18;market_var = 0.04;market_share_amt = round(rnorm(2,market_share, (market_share*market_var)),4)



brands = 3;Products = 5 # To leave as 5 for retail Product analysis
add_brands = runif(1,0.6,0.7)
complaints_multi = 7
di_base = 1200
account_multi = 4
Products_In_Scope = c("PCA", "Savings","Credit Card", "Mortgages","Loans")
Products_Fin_Split = c(0.12,0.11,0.18,0.45,0.21)
Products_Fin_Split_Var = c(0.02,0.02,0.03,0.05,0.04)

Brands_In_Scope = c("Lloyds", "Halifax","BoS")
prod_attr_rate = c(0.21,0.19,0.05,0.24,0.28)
journey_duration = c(5, 4, 12, 30, 25)
journey_devation = c(2, 0.75, 2, 5, 3)
start_hour = c(9, 13, 15, 11, 12);start_variation = c(1, 2, 1.5, 1, 1.5)
net_cust_movement = 0.02

################ Change in customer base calculations- based on parameteres above ################
cust_change = runif(1,1-net_cust_movement,1+net_cust_movement);change_prop = runif(1,0,1)+1;prior_month_custs = round(runif(1,cust-threshold,cust+threshold));cust_amount = round(prior_month_custs*cust_change);cust_change = cust_amount - prior_month_custs
leavers_t = ifelse(cust_change < 0,round(abs(cust_change)*change_prop),round(abs(cust_change)*0));freshers_t = ifelse(cust_change > 0,round(abs(cust_change)*change_prop),round(abs(cust_change)*0))
leavers = ifelse(leavers_t > 0, leavers_t,  freshers_t - abs(cust_change));freshers = ifelse(freshers_t > 0, freshers_t,  leavers_t - abs(cust_change))


PCA_Multi = 2500; Savings_Multi = 5000;Mortgage_Multi = 500000;CC_Multi = 10000;Loans_Multi = 100000
rev_multi = 4;costs_multi = 1.2
fin_change = 0.01
Branch_Multi = 1.1;Digital_Multi = 7;Telephony_Multi = 1.4;Post_Multi = 1;NextGen_Multi = 6;
inactive_multi = 0.5
Starting_Origin = "01/01/2010" 
Current_Month = as.Date(paste0("01/",month(Sys.Date()),"/",year(Sys.Date())), format = "%d/%m/%Y")
Prior_Month = as.Date(paste0("01/",month(Sys.Date())-1,"/",year(Sys.Date())), format = "%d/%m/%Y")
########################################################
##### 1b) Complaints table Variables ###################
complaints_years = 2;
Complaints_Current_Month = as.Date(paste0("01/",month(Sys.Date()),"/",year(Sys.Date())), format = "%d/%m/%Y")
Complaints_Last_Date = as.Date(paste0("01/",month(Sys.Date())+1,"/",(year(Sys.Date())-complaints_years)), format = "%d/%m/%Y")
Current_Day = as.Date(Sys.Date(), format = "%d/%m/%Y");
n_complaints_p_year = 600000;n_complaints_p_year_thresh = 20000
complaints_records = round(rnorm(1,n_complaints_p_year,n_complaints_p_year_thresh))*complaints_years
duration_multi = 60

########################################################
######## 2) Preset Variables and functions #############
if(length(Products_In_Scope) != Products){
  warning("Please note: The number of in scope products provided do not agree to the number of products to be generated in the dataset. As such, dataset will not populate as intended")
}
if(length(Brands_In_Scope) != brands){
  warning("Please note: The number of in scope brands provided do not agree to the number of brands to be generated in the dataset. As such, dataset will not populate as intended")
}
random_gen_pay <- function(n) {
  random_t <- do.call(paste0, replicate(3, sample(LETTERS, n, TRUE), FALSE))
  paste0(random_t, sprintf("%04d", sample(999, n, TRUE)), sample(LETTERS, n, TRUE))
}
random_gen <- function(n) {
  random_t <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(random_t, sprintf("%04d", sample(999999, n, TRUE)), sample(LETTERS, n, TRUE))
}
create_dist = function(data, simulations,...){
  weights_df_t = data.frame(run_1 =  runif(length(data)))
  for(i in 2:simulations){
    weights_df_t[,i] = runif(length(data))
  }
  for(j in 1:nrow(weights_df_t)){
    weights_df_t[j, simulations+1] = mean(data.matrix(weights_df_t[j, 1:simulations])) 
  }
  names(weights_df_t) = c(paste0("Sim_", 1:simulations), "Weighted_Avg" )
  weights_df = weights_df_t %>%  select(Weighted_Avg) %>% arrange(desc(Weighted_Avg))
}
########################################################
############# 3) Reference tables/objects ##############
############# a) Fresco tables #########################
segment_cd = 1:12
Segment = c("High Income Professionals","Rising Metropolitans","Asset-Rich Greys","Home-Owning Families","Road to Retirement","Working Single & Couples",
            "Older Working Families","Low Income Elderly","Still at Home","Starting Out","Poorer Parents","Mid-Life Social Renters")
segment_df = data.frame(segment_cd,Segment) %>% mutate(Segment = as.character(Segment),segment_cd = as.character(segment_cd))
Product = c("PCA", "Savings", "Mortgages", "Loans", "Credit Card", "Investments", "Pensions", "Annuities", "Wealth Management",
            "Protection")
############# b) CJD tables ############################
Stages = c("Application Initiated","Application Submitted","Application ID Check","Application Product Offered","Application Complete","Account in use")
Product_Mapping = data.frame(Product,Stage_1 = Stages[1], Stage_2 = Stages[2], Stage_3 = Stages[3], Stage_4 = Stages[4], Stage_5 = Stages[5]); 
Product_Mapping = Product_Mapping %>% mutate_if(is.factor, as.character)
channel_ref = c("Digital", "Telephony", "Next Gen", "Branch","Post","Digital","Digital")
channel_ref_probs = sort(runif(length(priority),0,1), decreasing = TRUE)
############# c) complaints tables ######################
outcome = c("DECLINED","UPHELD","NO PPI/NO CUSTOMER","NOT BANK ADVICE","NO OUTCOME DATA","HANDED OFF","Upheld - Payment Due","Upheld - No Payment","WITHDRAWN BY CUSTOMER")
what_went_wrong_L0 = c("Service","Product","Product","Product","Product","Product","Process","Service","Service","Product","Process","Product","Product","Process","Service","Product","Product","Product","Product","Product","Product","Process","Service","Service","Product","Product","Product","Service","Service","Service","Product")
what_went_wrong_L1 = c("MAKE AN ENQUIRY / CONTACT WITH THE BANK / ACCESS INTERNET BANKING","PRODUCT CLOSURES","PAYMENTS & CARD TRANSACTIONS","PRODUCT APPLICATIONS & NEW BUSINESS","PAYMENTS & CARD TRANSACTIONS","INSURANCE & INVESTMENTS (INCL. MATURITIES)","FINANCIAL DIFFICULTIES / CREDIT FILE QUERIES","MAKE AN ENQUIRY / CONTACT WITH THE BANK / ACCESS INTERNET BANKING","MANAGE BORROWING, OVERDRAFTS & CREDIT LIMITS","PRODUCT APPLICATIONS & NEW BUSINESS","FRAUD & DISPUTES","PAYMENTS & CARD TRANSACTIONS","PAYMENTS & CARD TRANSACTIONS","FRAUD & DISPUTES","CUSTOMER DETAILS & ACCOUNT SERVICING","PAYMENTS & CARD TRANSACTIONS","PAYMENTS & CARD TRANSACTIONS","PAYMENTS & CARD TRANSACTIONS","PLASTIC CARDS, PINS, BOOKS & STATEMENTS","PRODUCT CLOSURES","PAYMENTS & CARD TRANSACTIONS","FRAUD & DISPUTES","MANAGE BORROWING, OVERDRAFTS & CREDIT LIMITS","MAKE AN ENQUIRY / CONTACT WITH THE BANK / ACCESS INTERNET BANKING","PLASTIC CARDS, PINS, BOOKS & STATEMENTS","INSURANCE & INVESTMENTS (INCL. MATURITIES)","PAYMENTS & CARD TRANSACTIONS","MANAGE BORROWING, OVERDRAFTS & CREDIT LIMITS","CUSTOMER DETAILS & ACCOUNT SERVICING","MANAGE BORROWING, OVERDRAFTS & CREDIT LIMITS","PLASTIC CARDS, PINS, BOOKS & STATEMENTS")
what_went_wrong_L2 = c("Query - General enquiry or information request","Upgrade / Downgrade / Convert account (inc. trade credit card)","Standing Order - Pay, Set up, Amend or Cancel a S/O","Open or Apply for a NEW product (account, policy, loan, card, [re]mortgage)","Withdraw funds - from an account, policy, mortgage or investment","Transfer - Tfr money between internal accounts","Financial Difficulties - Discuss with bank","Access - Log on to or view self-service channel, e.g. Telephone, Internet or Mobile Banking","Overdraft - Apply for a NEW overdraft facility (not Collections)","Further Advance / Product Transfer - Apply for","Fraud - Customer reporting suspected fraud","Card transaction - Pay with a card/mobile (incl. Contactless, Applepay etc)","Regular Credits - e.g. Salary, Benefits, Pension Credit (via BACS) - [NOT SO / DD]","Customer alleges historical mis-sale","Address / Correspondence Address - Change","CHAPS - Send or Receive CHAPS","Faster Payment, Bill Payment, SIP, Pay a Contact - Send or Receive","Dispute a transaction / request a chargeback","Lost or Stolen - Report card or book missing","Close, Settle, Redeem or Cancel a product (account, policy, loan, card, mortgage)","Continuous Payment Authority / Recurring Transaction - CANCEL","Query unexpected/incorrect fees, charges or interest","Credit Card - Balance Transfer","Contact - Specific Branch or Relationship Manager","Card - Order, Receive, Activate or Unblock new/replacement card","Account / Card Benefit Scheme (e.g. AVA, Avios, Sentinel) - Use or Maintain","Direct Debit - Pay, Set up, Amend or Cancel a DD","Repayment Arrangement (Collections & Recoveries) - Agree, Set Up or Amend","Safe Custody - Use facilities","Overdraft or Credit Card Limit - Request change to limit (incl. Arrears Step Limits)","PIN - Order, Receive, Unblock a New or Replacement PIN")
what_went_wrong_df = data.frame(what_went_wrong_L0 = what_went_wrong_L0,what_went_wrong_L1 = what_went_wrong_L1,what_went_wrong_L2 =what_went_wrong_L2) %>% mutate_if(is.factor, as.character)

############# d) Financial tables ######################
############# i) Revenue ###############################
revenue_order = c("High Income Professionals","Asset-Rich Greys","Home-Owning Families","Older Working Families","Road to Retirement","Rising Metropolitans","Working Single & Couples","Mid-Life Social Renters","Poorer Parents","Low Income Elderly","Starting Out","Still at Home")
revenue_order_probs = create_dist(revenue_order, 10)
revenue_amt = revenue_order_probs/sum(revenue_order_probs)*rev_amt[1];names(revenue_amt) = "revenue_amt"
rev_df = data.frame(revenue_order = revenue_order,revenue_order_probs = revenue_order_probs,revenue_amt = revenue_amt)
rev_df_t = data.frame(revenue_order = revenue_order,revenue_order_probs = revenue_order_probs, revenue_amt_t = revenue_amt) %>% rename(Segment = revenue_order) %>% mutate(Reporting_Period = Prior_Month)%>% select(-Weighted_Avg) %>% 
  mutate(Seg_Period = paste0(Segment,"_",as.character(Reporting_Period)))
rev_df_t2 = data.frame(revenue_order = revenue_order, revenue_amt_t = data.matrix(create_dist(revenue_order, 10)/mean(data.matrix(create_dist(revenue_order, 10)))*rev_amt[2]/12),
                       Reporting_Period = rep(Current_Month,  length(revenue_order))) %>%  rename(revenue_amt =Weighted_Avg) %>% rename(Segment = revenue_order) %>%
  mutate(Seg_Period = paste0(Segment,"_",as.character(Reporting_Period)))
revenue_df = bind_rows(rev_df_t,rev_df_t2)
rm(revenue_order);rm(revenue_order_probs);rm(revenue_amt);rm(rev_df_t);rm(rev_df_t2)
############# ii) Profit ###############################
profit_order = c("High Income Professionals","Home-Owning Families","Asset-Rich Greys","Older Working Families","Road to Retirement","Rising Metropolitans","Working Single & Couples","Low Income Elderly","Poorer Parents","Starting Out","Still at Home","Mid-Life Social Renters")
profit_order_probs = create_dist(profit_order, 10)
profit_amt = profit_order_probs/sum(profit_order_probs)*pft_amt;names(profit_amt) = "profit_amt"

profit_df_t = data.frame(profit_order = profit_order,profit_order_probs = profit_order_probs, profit_amt = profit_amt) %>% rename(Segment = profit_order) %>% mutate(Reporting_Period = Prior_Month)%>% select(-Weighted_Avg) %>% 
  mutate(Seg_Period = paste0(Segment,"_",as.character(Reporting_Period)))

profit_df_t2 = data.frame(profit_order = profit_order, profit_order_probs = data.matrix(create_dist(profit_order, 10)/mean(data.matrix(create_dist(profit_order, 10)))*pft_amt[2]/12),
                          Reporting_Period = rep(Current_Month,  length(profit_order))) %>%  rename(profit_amt =Weighted_Avg,Segment = profit_order) %>% mutate(Seg_Period = paste0(Segment,"_",as.character(Reporting_Period)))

profit_df = bind_rows(profit_df_t,profit_df_t2)
rm(profit_order);rm(profit_order_probs);rm(profit_amt);rm(profit_df_t);rm(profit_df_t2)
############# iii) Market share ###############################
market_share_order = c("Working Single & Couples","Older Working Families","Home-Owning Families","Mid-Life Social Renters","Rising Metropolitans","Poorer Parents","Asset-Rich Greys","Still at Home","Starting Out","Road to Retirement","High Income Professionals","Low Income Elderly")
market_share_order_probs = create_dist(market_share_order, 50)
market_share_amount = market_share_order_probs/mean(data.matrix(market_share_order_probs));names(market_share_amount) = "market_share_amount"

market_share_df_t = data.frame(market_share_order = market_share_order,market_share_order_probs = market_share_order_probs,market_share_amount = market_share_amount*market_share_amt[1]) %>% select(-Weighted_Avg) %>%
  rename(fresco = market_share_order, market_share = market_share_amount) %>% mutate(Reporting_Period = Prior_Month)  
market_share_df_t2 = data.frame(fresco = market_share_order, market_share = data.matrix(create_dist(market_share_order, 50)/mean(data.matrix(create_dist(market_share_order, 50)))*market_share_amt[2]),
                                Reporting_Period = rep(Current_Month,  length(market_share_order))) %>%  rename(market_share =Weighted_Avg) 
market_share_df = bind_rows(market_share_df_t,market_share_df_t2)
rm(market_share_order);rm(market_share_order_probs);rm(market_share_amount);rm(market_share_df_t);rm(market_share_df_t2)

dbRemoveTable(dsol_proto, paste0(client_name,"_MARKET_SHARE_TABLE"))
dbWriteTable(dsol_proto, paste0(client_name,"_MARKET_SHARE_TABLE"), market_share_df) 

############# iv) Costs ###############################
profit_df_t = profit_df %>% select(Seg_Period,profit_amt)
cost_df = left_join(revenue_df,profit_df_t) %>%  mutate(cost_amt =  revenue_amt- profit_amt) %>% select(-revenue_amt,-profit_amt)



########################################################
########## 4) Build of tables  #########################

########## 4a) Customer table  #########################
######### Prior month Customer table ###################

set.seed(1234)
ID = paste0("Cust_", 1:prior_month_custs)

Brand_IDs = data.frame(Brand_IDs = sample(1:brands, size=length(ID), prob=runif(brands,0,1), replace=TRUE))
Brand_t2 = data.frame(Brand_IDs = as.numeric(names(sort(table(Brand_IDs), decreasing = T))), Brand = Brands_In_Scope) %>%  mutate(Brand = as.character(Brand)) 
Brand = left_join(Brand_IDs,Brand_t2)
Product_IDs = data.frame(Product_IDs = sample(1:Products, size=length(ID), prob=runif(Products,0,1), replace=TRUE))
Product_t2 = data.frame(Product_IDs = as.numeric(names(sort(table(Product_IDs), decreasing = T))), Product = Products_In_Scope) %>%  mutate(Product = as.character(Product)) 
Product = left_join(Product_IDs,Product_t2)
cs_source_t = data.frame(ID,Brand = Brand$Brand,Product = Product$Product)

########################################################

add_ID = sample(length(ID), size = round(length(ID)*add_brands), replace = T);add_ID = paste0("Cust_",add_ID)
add_brand_IDs = data.frame(add_Brand_IDs = sample(1:brands, size=length(add_ID), prob=runif(brands,0,1), replace=TRUE))
add_Brand_t2 = data.frame(add_Brand_IDs = as.numeric(names(sort(table(add_brand_IDs), decreasing = T))), Brand = Brands_In_Scope) %>%  mutate(Brand = as.character(Brand)) 
add_Brand = left_join(add_brand_IDs,add_Brand_t2)
add_Product_IDs = data.frame(add_Product_IDs = sample(1:Products, size=length(add_ID), prob=runif(Products,0,1), replace=TRUE))
add_Product_t2 = data.frame(add_Product_IDs = as.numeric(names(sort(table(add_Product_IDs), decreasing = T))), Product = Products_In_Scope) %>%  mutate(Product = as.character(Product))
add_Product = left_join(add_Product_IDs,add_Product_t2)
cs_source_t2 = data.frame(ID = add_ID,Brand = add_Brand$Brand,Product = add_Product$Product)

cs_source_t3 = rbind(cs_source_t,cs_source_t2)
cs_source_t4 = unique(cs_source_t3)
cs_source_t5 = cs_source_t4 %>%  distinct(ID,Brand)
cs_source_t6 = unique(cs_source_t4$ID)
########################################################
segment_t = data.frame(segment_cd = sample(1:length(Segment), size=length(cs_source_t6), prob=runif(length(Segment),0,1), replace=TRUE)) %>% mutate(segment_cd = as.character(segment_cd))
segment_t2 = left_join(segment_t,segment_df)
segment = data.frame(cs_source_t6,segment_t2) %>%  rename(ID = cs_source_t6)

Complaints = round(rbeta(nrow(cs_source_t4),1,10)*complaints_multi)
Distress_Payments = round(rbeta(nrow(cs_source_t4),1,8)*di_base)
Balance = ifelse(cs_source_t4$Product == "PCA", round(rbeta(length(cs_source_t4$Product[cs_source_t4$Product == "PCA"]),1,4)*PCA_Multi),
                 ifelse(Product == "Savings", round(rbeta(length(cs_source_t4$Product[cs_source_t4$Product == "Savings"]),1,5)*Savings_Multi),
                        ifelse(cs_source_t4$Product == "Credit Card", round(rbeta(length(cs_source_t4$Product[cs_source_t4$Product == "Credit Card"]),1,4)*CC_Multi),ifelse(Product == "Mortgages", round(rbeta(length(cs_source_t4$Product[cs_source_t4$Product == "Mortgages"]),1,8)*Mortgage_Multi),
                                                                                                                                                                            ifelse(cs_source_t4$Product == "Loans", round(rbeta(length(cs_source_t4$Product[cs_source_t4$Product == "Mortgages"]),1,8)*Loans_Multi),round(rbeta(1,1,8)*Loans_Multi))))))

account_sts_t = round(rbeta(nrow(cs_source_t5),1,8)*account_multi);
Account_sts_t2 = ifelse(account_sts_t == 0, "Primary Active",ifelse(account_sts_t == 1, "Primary Inactive",ifelse(account_sts_t == 2, "Secondary Active",  ifelse(account_sts_t == 3, "Primary Inactive","Other"))))
Rewards_sts = ifelse(rnorm(nrow(cs_source_t5), 0.3,0.2) > 0.5 ,1,0)

Account_sts = data.frame(cs_source_t5,Account_sts_t2,Rewards_sts) %>% rename(Account_sts = Account_sts_t2)


start_date_base = as.numeric(as.Date(Starting_Origin, format = "%d/%m/%Y", origin ="1970-01-01"));start_date_end = as.numeric(Sys.Date(), origin ="1970-01-01")
start_date = as.Date(round(runif(nrow(cs_source_t4),start_date_base,start_date_end)),origin ="1970-01-01")
has_left = ifelse(rnorm(nrow(cs_source_t4), 0.3,0.2) > 0.5 ,1,0)

cs_source_t7 = data.frame(cs_source_t4,Complaints,Distress_Payments,Balance,start_date,has_left)
cs_source_t8 = left_join(cs_source_t7,segment)

cs_source_pm = left_join(cs_source_t8,Account_sts) %>% 
  mutate(application_ID =  random_gen(n=nrow(cs_source_t8)),
         revenue = round(rbeta(nrow(cs_source_t8),1,8)*rev_multi,3),
         costs = round(rbeta(nrow(cs_source_t8),1,8)*costs_multi,3),
         profit = revenue - costs,
         Reporting_Period = Prior_Month,
         branch = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_t8),1,4.5)*Branch_Multi*inactive_multi),
                         round(rbeta(nrow(cs_source_t8),1,4.5)*Branch_Multi)),
         digital = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_t8),1,4.5)*Digital_Multi*inactive_multi),
                          round(rbeta(nrow(cs_source_t8),1,4.5)*Digital_Multi)),
         telephony = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_t8),1,4.5)*Telephony_Multi*inactive_multi),
                            round(rbeta(nrow(cs_source_t8),1,4.5)*Telephony_Multi)),
         post = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_t8),1,4.5)*Post_Multi*inactive_multi),
                       round(rbeta(nrow(cs_source_t8),1,4.5)*Post_Multi)),
         nextgen = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_t8),1,4.5)*NextGen_Multi*inactive_multi),
                          round(rbeta(nrow(cs_source_t8),1,4.5)*NextGen_Multi))
  ) %>% mutate(ID = as.character(ID))

########## Housekeeping #####################
rm(cs_source_t);rm(cs_source_t2);rm(cs_source_t3);rm(cs_source_t4);rm(cs_source_t5);rm(cs_source_t6);rm(cs_source_t7);rm(cs_source_t8)
rm(segment_t);rm(segment_t2);rm(Distress_Payments);rm(Complaints);rm(Balance);rm(account_sts_t);rm(Account_sts_t2);rm(Rewards_sts);rm(Account_sts)
rm(add_brand_IDs);rm(add_Brand_t2);rm(add_Brand);rm(add_Product_IDs);rm(add_Product_t2);rm(add_Product)
rm(Brand_IDs);rm(Brand_t2);rm(Brand);rm(Product_IDs);rm(Product_t2);rm(Product)
############################################

############# Customer - current month ###########
pm_custs = as.character(unique(cs_source_pm$ID));custs_in_scope = sample(length(pm_custs),(length(pm_custs)-leavers))

cm_custs =  data.frame(ID = pm_custs[custs_in_scope]) %>% mutate(ID = as.character(ID))
cs_source_cm_t = inner_join(cs_source_pm,cm_custs)

cs_source_cm_base = cs_source_cm_t %>% 
  mutate(Reporting_Period = Current_Month,
         branch = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_cm_t),1,4.5)*Branch_Multi*inactive_multi),
                         round(rbeta(nrow(cs_source_cm_t),1,4.5)*Branch_Multi)),
         digital = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_cm_t),1,4.5)*Digital_Multi*inactive_multi),
                          round(rbeta(nrow(cs_source_cm_t),1,4.5)*Digital_Multi)),
         telephony = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_cm_t),1,4.5)*Telephony_Multi*inactive_multi),
                            round(rbeta(nrow(cs_source_cm_t),1,4.5)*Telephony_Multi)),
         post = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_cm_t),1,4.5)*Post_Multi*inactive_multi),
                       round(rbeta(nrow(cs_source_cm_t),1,4.5)*Post_Multi)),
         nextgen = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_cm_t),1,4.5)*NextGen_Multi*inactive_multi),
                          round(rbeta(nrow(cs_source_cm_t),1,4.5)*NextGen_Multi)),
         revenue = round(revenue*(1+rnorm(nrow(cs_source_cm_t),-fin_change,fin_change)),3),
         costs = round(costs*(1+rnorm(nrow(cs_source_cm_t),-fin_change,fin_change)),3),
         profit = revenue - costs
  )

set.seed(1234)
ID = paste0("Cust_", prior_month_custs:(prior_month_custs+freshers))

Brand_IDs = data.frame(Brand_IDs = sample(1:brands, size=length(ID), prob=runif(brands,0,1), replace=TRUE))
Brand_t2 = data.frame(Brand_IDs = as.numeric(names(sort(table(Brand_IDs), decreasing = T))), Brand = Brands_In_Scope) %>%  mutate(Brand = as.character(Brand)) 
Brand = left_join(Brand_IDs,Brand_t2)
Product_IDs = data.frame(Product_IDs = sample(1:Products, size=length(ID), prob=runif(Products,0,1), replace=TRUE))
Product_t2 = data.frame(Product_IDs = as.numeric(names(sort(table(Product_IDs), decreasing = T))), Product = Products_In_Scope) %>%  mutate(Product = as.character(Product)) 
Product = left_join(Product_IDs,Product_t2)
cs_source_t = data.frame(ID,Brand = Brand$Brand,Product = Product$Product)

############################################
add_ID = sample(length(ID), size = round(length(ID)*add_brands), replace = T);add_ID = paste0("Cust_",add_ID)
add_brand_IDs = data.frame(add_Brand_IDs = sample(1:brands, size=length(add_ID), prob=runif(brands,0,1), replace=TRUE))
add_Brand_t2 = data.frame(add_Brand_IDs = as.numeric(names(sort(table(add_brand_IDs), decreasing = T))), Brand = Brands_In_Scope) %>%  mutate(Brand = as.character(Brand)) 
add_Brand = left_join(add_brand_IDs,add_Brand_t2)
add_Product_IDs = data.frame(add_Product_IDs = sample(1:Products, size=length(add_ID), prob=runif(Products,0,1), replace=TRUE))
add_Product_t2 = data.frame(add_Product_IDs = as.numeric(names(sort(table(add_Product_IDs), decreasing = T))), Product = Products_In_Scope) %>%  mutate(Product = as.character(Product))
add_Product = left_join(add_Product_IDs,add_Product_t2)
cs_source_t2 = data.frame(ID = add_ID,Brand = add_Brand$Brand,Product = add_Product$Product)

cs_source_t3 = rbind(cs_source_t,cs_source_t2)
cs_source_t4 = unique(cs_source_t3)
cs_source_t5 = cs_source_t4 %>%  distinct(ID,Brand)
cs_source_t6 = unique(cs_source_t4$ID)

#############################################

segment_t = data.frame(segment_cd = sample(1:length(Segment), size=length(cs_source_t6), prob=runif(length(Segment),0,1), replace=TRUE)) %>% mutate(segment_cd = as.character(segment_cd))
segment_t2 = left_join(segment_t,segment_df)
segment = data.frame(cs_source_t6,segment_t2) %>%  rename(ID = cs_source_t6)

Complaints = round(rbeta(nrow(cs_source_t4),1,10)*complaints_multi)
Distress_Payments = round(rbeta(nrow(cs_source_t4),1,8)*di_base)
Balance = ifelse(cs_source_t4$Product == "PCA", round(rbeta(length(cs_source_t4$Product[cs_source_t4$Product == "PCA"]),1,4)*PCA_Multi),
                 ifelse(Product == "Savings", round(rbeta(length(cs_source_t4$Product[cs_source_t4$Product == "Savings"]),1,5)*Savings_Multi),
                        ifelse(cs_source_t4$Product == "Credit Card", round(rbeta(length(cs_source_t4$Product[cs_source_t4$Product == "Credit Card"]),1,4)*CC_Multi),ifelse(Product == "Mortgages", round(rbeta(length(cs_source_t4$Product[cs_source_t4$Product == "Mortgages"]),1,8)*Mortgage_Multi),
                                                                                                                                                                            ifelse(cs_source_t4$Product == "Loans", round(rbeta(length(cs_source_t4$Product[cs_source_t4$Product == "Mortgages"]),1,8)*Loans_Multi),round(rbeta(1,1,8)*Loans_Multi))))))

account_sts_t = round(rbeta(nrow(cs_source_t5),1,8)*account_multi);
Account_sts_t2 = ifelse(account_sts_t == 0, "Primary Active",ifelse(account_sts_t == 1, "Primary Inactive",ifelse(account_sts_t == 2, "Secondary Active",  ifelse(account_sts_t == 3, "Primary Inactive","Other"))))
Rewards_sts = ifelse(rnorm(nrow(cs_source_t5), 0.3,0.2) > 0.5 ,1,0)

Account_sts = data.frame(cs_source_t5,Account_sts_t2,Rewards_sts) %>% rename(Account_sts = Account_sts_t2)

start_date_base = as.numeric(as.Date(Starting_Origin, format = "%d/%m/%Y", origin ="1970-01-01"));start_date_end = as.numeric(Sys.Date(), origin ="1970-01-01")
start_date = as.Date(round(runif(nrow(cs_source_t4),start_date_base,start_date_end)),origin ="1970-01-01")
has_left = ifelse(rnorm(nrow(cs_source_t4), 0.3,0.2) > 0.5 ,1,0)

cs_source_t7 = data.frame(cs_source_t4,Complaints,Distress_Payments,Balance,start_date,has_left)
cs_source_t8 = left_join(cs_source_t7,segment)

cs_source_cm_freshers = left_join(cs_source_t8,Account_sts) %>% 
  mutate(application_ID =  random_gen(n=nrow(cs_source_t8)),
         revenue = round(rbeta(nrow(cs_source_t8),1,8)*rev_multi,3),
         costs = round(rbeta(nrow(cs_source_t8),1,8)*costs_multi,3),
         profit = revenue - costs,
         Reporting_Period = Prior_Month,
         branch = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_t8),1,4.5)*Branch_Multi*inactive_multi),
                         round(rbeta(nrow(cs_source_t8),1,4.5)*Branch_Multi)),
         digital = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_t8),1,4.5)*Digital_Multi*inactive_multi),
                          round(rbeta(nrow(cs_source_t8),1,4.5)*Digital_Multi)),
         telephony = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_t8),1,4.5)*Telephony_Multi*inactive_multi),
                            round(rbeta(nrow(cs_source_t8),1,4.5)*Telephony_Multi)),
         post = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_t8),1,4.5)*Post_Multi*inactive_multi),
                       round(rbeta(nrow(cs_source_t8),1,4.5)*Post_Multi)),
         nextgen = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(cs_source_t8),1,4.5)*NextGen_Multi*inactive_multi),
                          round(rbeta(nrow(cs_source_t8),1,4.5)*NextGen_Multi))
  ) %>% mutate(ID = as.character(ID))

cs_source_cm = bind_rows(cs_source_cm_base,cs_source_cm_freshers);

cs_source_t9 = bind_rows(cs_source_cm,cs_source_pm) %>% 
  select(-Complaints,-Distress_Payments)

########## 4b) Complaints table  #########################
complaints_temp = cs_source_t9 %>% 
  select(ID,Brand,Product,Segment,Account_sts,start_date)  %>%
  distinct() %>% 
  mutate(days_in_scope =  as.numeric(Current_Day) - as.numeric(start_date),
         probs = days_in_scope/sum(days_in_scope),
         cust_key = paste0(ID,"_",Brand,"_",Product)
  ) 
complaints_t = sample(rep(complaints_temp$cust_key, round(complaints_records * complaints_temp$probs))) %>% data.frame() %>%
  rename(cust_key = ".") %>% mutate(cust_key = as.character(cust_key)) %>% left_join(complaints_temp) %>% 
  select(-cust_key,-days_in_scope,-probs) %>% 
  mutate(complaint_date = as.Date(round(runif(length(ID),as.numeric(Complaints_Last_Date),as.numeric(Sys.Date()))), origin = "1970-01-01")
  )
complaints =  complaints_t %>% 
  mutate(outcome = sample(outcome, nrow(complaints_t), replace = T),
         #         what_went_wrong_L0 = sample(what_went_wrong_L0, nrow(complaints_t), replace = T),
         #         what_went_wrong_L1 = sample(what_went_wrong_L1, nrow(complaints_t), replace = T),
         what_went_wrong_L2 = sample(what_went_wrong_df$what_went_wrong_L2, nrow(complaints_t), replace = T),
         channel = sample(channel_ref, nrow(complaints_t), prob = channel_ref_probs,replace = T),
         fca_flag = round(rnorm(nrow(complaints_t), 0.3,0.1)),
         cmc_flag = round(rnorm(nrow(complaints_t), 0.325,0.1)),
         has_redress = round(rnorm(nrow(complaints_t), 0.45,0.1)),
         di_payment = ifelse(has_redress == 1, round(rbeta(length(Brand),1,8)*di_base),0),
         redress_pred = ifelse(has_redress == 1,  round(rnorm(length(has_redress[has_redress == 1]), 0.65,0.1)),
                               round(rnorm(length(has_redress[has_redress == 0]), 0.30,0.1))),
         duration = round(rbeta(length(Brand),1,8)*duration_multi),
         resolve_date = as.Date(complaint_date + duration, format = "%Y-%m-%d")
  ) %>% 
  mutate(what_went_wrong_L0 = what_went_wrong_df$what_went_wrong_L0[match(what_went_wrong_L2,what_went_wrong_df$what_went_wrong_L2)],
         what_went_wrong_L1 = what_went_wrong_df$what_went_wrong_L1[match(what_went_wrong_L2,what_went_wrong_df$what_went_wrong_L2)]
  )

dbRemoveTable(dsol_proto, paste0(client_name,"_COMPLAINTS_TABLE"))
dbWriteTable(dsol_proto, paste0(client_name,"_COMPLAINTS_TABLE"),complaints)


#############################################
###### 4c) Populate complaints analysis #####
###### back into customer table #############

complaints_summ = complaints %>% 
  mutate(Reporting_Period = as.Date(paste0(substr(complaints$complaint_date,1,4),"-",substr(complaints$complaint_date,6,7),"-","01"), format = "%Y-%m-%d")
  ) %>% 
  group_by(ID,Brand,Product,Reporting_Period) %>%
  summarise(Complaints = n(),
            Distress_Payments = sum(di_payment)
  ) %>%  data.frame() %>% filter(Reporting_Period %in% c(Current_Month,Prior_Month))

cs_source = left_join(cs_source_t9,complaints_summ) 
cs_source[is.na(cs_source)] <- 0

################################################
###### 4d) Build financial numbers against #####
###### set amounts defined #####################

cs_source_temp = cs_source %>% 
  mutate(Seg_Period = paste0(Segment,"_",as.character(Reporting_Period))) %>%
  split(.$Seg_Period) 

for(i in 1:length(cs_source_temp)){
  for(j in 1:length(Products_Fin_Split)){
    cs_source_temp[[i]]$product_split_t =  round(rnorm((length(cs_source_temp[[i]]$Product ==  Products_In_Scope[[j]])),Products_Fin_Split[[j]],Products_Fin_Split_Var[[j]]),5)
    cs_source_temp[[i]]$revenue = cs_source_temp[[i]]$product_split_t/sum(cs_source_temp[[i]]$product_split_t)*revenue_df$revenue_amt[match(cs_source_temp[[i]]$Seg_Period,revenue_df$Seg_Period)]
    cs_source_temp[[i]]$costs = cs_source_temp[[i]]$product_split_t/sum(cs_source_temp[[i]]$product_split_t)*cost_df$cost_amt[match(cs_source_temp[[i]]$Seg_Period,cost_df$Seg_Period)]
    cs_source_temp[[i]]$profit = cs_source_temp[[i]]$product_split_t/sum(cs_source_temp[[i]]$product_split_t)*profit_df$profit_amt[match(cs_source_temp[[i]]$Seg_Period,profit_df$Seg_Period)]
  }
}

cs_source = bind_rows(cs_source_temp) %>% select(-product_split_t)


#############################################
##### 5) Run required summaries to populate #
###### into final tables ####################
###### Not done for payments ################
###### this table is generated after ########


cs_source_channels = gather(cs_source,key = "Channel",value = "Visits",branch:nextgen) %>% 
  group_by(Brand,Segment,Account_sts,Rewards_sts,Reporting_Period,Channel) %>% 
  summarise(Visits = sum(Visits)) %>%  data.frame()
cs_source_channels$Visits[is.na(cs_source_channels$Visits)] <- 0

cs_source_summ = cs_source %>% 
  group_by(Brand,Product,Segment,Account_sts,Rewards_sts,Reporting_Period) %>% 
  summarise(Complaints = sum(Complaints),
            Distress_Payments = sum(Distress_Payments),
            Balance = sum(Balance),revenue = sum(revenue),costs = sum(costs),
            profit = sum(profit),branch = sum(branch),
            digital = sum(digital),telephony = sum(telephony),post = sum(post),nextgen = sum(nextgen),post = sum(post),
            cust_vol = length(ID)
  ) %>% data.frame() %>% mutate(Is_Current_Month = ifelse(max(Reporting_Period) == Reporting_Period,1,0))
cs_source_summ[is.na(cs_source_summ)] <- 0

#write.delim(cs_source,"cs_source.txt", sep = "|");write.delim(cs_source_channels,"cs_source_channels.txt", sep = "|")
dbRemoveTable(dsol_proto, paste0(client_name,"_CUSTOMER_TABLE"))
dbRemoveTable(dsol_proto, paste0(client_name,"_CUSTOMER_TABLE_SUMMARY"))
dbRemoveTable(dsol_proto, paste0(client_name,"_CHANNEL_CUSTOMER_TABLE"))

dbWriteTable(dsol_proto,paste0(client_name,"_CUSTOMER_TABLE"),cs_source)
dbWriteTable(dsol_proto,paste0(client_name,"_CUSTOMER_TABLE_SUMMARY"),cs_source_summ)
dbWriteTable(dsol_proto,paste0(client_name,"_CHANNEL_CUSTOMER_TABLE"),cs_source_channels)


########## Housekeeping #####################
rm(cs_source_t);rm(cs_source_t2);rm(cs_source_t3);rm(cs_source_t4);rm(cs_source_t5);rm(cs_source_t6);rm(cs_source_t7);rm(cs_source_t8)
rm(segment_t);rm(segment_t2);rm(Complaints);rm(Distress_Payments);rm(Complaints);rm(Balance);rm(account_sts_t);rm(Account_sts_t2);rm(Rewards_sts);rm(Account_sts)
rm(add_brand_IDs);rm(add_Brand_t2);rm(add_Brand);rm(add_Product_IDs);rm(add_Product_t2);rm(add_Product)
rm(Brand_IDs);rm(Brand_t2);rm(Brand);rm(Product_IDs);rm(Product_t2);rm(Product)
rm(segment);rm(segment_t);rm(segment_t2);rm(add_ID);rm(has_left);rm(ID);rm(start_date);
rm(change_prop_t);rm(change_prop_t2);
rm(cs_source_pm);rm(cs_source_cm);

########## End: Customer table ##############

#####  Payments table Variables #####################
#####  Required to be generated after cs_source table
#####  created  #####################################
#1c i) 

amt_limit = 2000
payment_months = 1;
Current_Month = as.Date(paste0("01/",month(Sys.Date()),"/",year(Sys.Date())), format = "%d/%m/%Y")
Last_Date = as.Date(paste0("01/",month(Sys.Date())-payment_months,"/",(year(Sys.Date()))), format = "%d/%m/%Y")
Current_Day = as.Date(Sys.Date(), format = "%d/%m/%Y")
#1c ii)
avg_payments_p_month = 1
n_payments_p_month = nrow(cs_source)*avg_payments_p_month 
n_payments_p_month_thresh = 0.01
payment_records = round(rnorm(1,n_payments_p_month,n_payments_p_month*n_payments_p_month_thresh))*payment_months

payments_multi = 75
amount_multi = 500
inactive_factor = 0.03


########## 4c) Payments table  #########################

payments_t = cs_source %>% 
  select(ID,Brand,Product,Segment,Account_sts,start_date)  %>%
  distinct() %>% filter(Product %in% c("PCA", "Savings", "Credit Card"))

payments_t2 = payments_t %>% 
  mutate(cust_key = paste0(ID,"_",Brand,"_",Product),
         Avg_Payments_P_Month = ifelse(Account_sts %in% c("Primary Inactive","Secondary Inactive"),round(rbeta(nrow(payments_t[Account_sts %in% c("Primary Inactive","Secondary Inactive"),]),1,4.5)*payments_multi*inactive_factor),
                                       round(rbeta(nrow(payments_t),1,4.5)*payments_multi)),
         Days_In_Scope = ifelse(start_date>Last_Date,as.numeric(Current_Day - start_date),as.numeric(Current_Day - Last_Date)),
         probs = Avg_Payments_P_Month*Days_In_Scope/sum(Avg_Payments_P_Month*Days_In_Scope)
  )
rm(payments_t)
payments_t3 = sample(rep(payments_t2$cust_key, round(payment_records * payments_t2$probs))) %>% data.frame() ;names(payments_t3) = "cust_key"
payments = payments_t3 %>%  mutate(cust_key = as.character(cust_key)) %>% left_join(payments_t2) %>% 
  select(-cust_key,-Days_In_Scope,-probs, -start_date,-Avg_Payments_P_Month) %>% 
  mutate(payment_date = as.Date(round(runif(length(ID),as.numeric(Last_Date),as.numeric(Sys.Date()))), origin = "1970-01-01"),
         event_id = random_gen_pay(length(payment_date)),
         vendor = sample(vendors, length(payment_date), replace = T),
         trans_amt =  round(rbeta(length(payment_date),1,12)*amount_multi)
  )  %>% left_join(vendor_table) %>% 
  mutate(location = sample(location, length(payment_date), replace = T),
         is_fraud = round(rnorm(length(payment_date),0.28,0.1)),
         pred_fraud = ifelse(is_fraud == 1,round(rnorm(length(payment_date),0.65,0.1)),round(rnorm(length(payment_date),0.3,0.1))),
         bank = sample(rep(banks$Banks, round(length(payment_date) * banks$probs)))
  )

dbRemoveTable(dsol_proto, paste0(client_name,"_PAYMENTS_TABLE"))
dbWriteTable(dsol_proto,paste0(client_name,"_PAYMENTS_TABLE"),payments)

rm(payments_t2);rm(payments_t3);rm(payments)



#############################################
########## Start:Customer Journeys ##########
# temp tables
# rm(cjd_t);rm(cjd_t2);rm(cjd_t3);rm(cjd_t4);rm(cjd_t5)
complete_apps = length(cs_source$application_ID)
cjd_t = cs_source %>%  select(application_ID, ID,Brand,Product,start_date)

cjd_t2 =  length(unique(cjd_t$Product)) %>% 
  rerun(data.frame(matrix(NA, nrow(cjd_t),ncol(cjd_t)))) %>% 
  map(~set_names(.x,names(cjd_t)))  

for(i in 1:length(cjd_t2)){
  cjd_t2[[i]]$ID = as.character(cjd_t2[[i]]$ID)
  cjd_t2[[i]] = cjd_t[cjd_t$Product == Products_In_Scope[i],]
  cjd_temp_list = data.frame(matrix(NA, round(nrow(cjd_t2[[i]])/prod_attr_rate[i]),ncol(cjd_t))) 
  names(cjd_temp_list) = names(cjd_t)
  cjd_temp_list = cjd_temp_list %>% mutate(application_ID = random_gen(nrow(cjd_temp_list)),Product = Products_In_Scope[i],Brand = paste0("Brand_", sample(1:brands, size=nrow(cjd_temp_list), prob=runif(brands,0,1), replace=TRUE)),
                                           ID = as.character(ID), start_date = as.Date(start_date)
  )
  cjd_t2[[i]] = bind_rows(cjd_t2[[i]], cjd_temp_list)  
}
cjd_t3 = bind_rows(cjd_t2)

#rm(cjd_t);rm(cjd_t2);rm(cjd_t3)

#cjd_t4 = left_join(cjd_t3, Product_Mapping)

cjd_t5 = cjd_t3 %>% 
  mutate(start_date = ifelse(is.na(start_date),round(runif(nrow(cjd_t4),as.numeric(as.Date(Starting_Origin,format = "%d/%m/%Y")),as.numeric(as.Date(Sys.time(),format = "%d/%m/%Y")))),start_date),
         is_in_order = round(rnorm(nrow(cjd_t4),0.7,0.1)),
         stages = ifelse(is.na(ID), round(runif(nrow(cjd_t4), 1,4)),
                         ifelse(!is.na(ID) & is_in_order != 1, round(runif(nrow(cjd_t4), 1,4)),6)),
         start_date = as.Date(start_date, origin ="1970-01-01"),
         Stage_1_duration = NA,Stage_2_duration = NA,Stage_3_duration = NA,Stage_4_duration = NA,Stage_5_duration = NA,Stage_6_duration = NA
  )

cjd_t6 = cjd_t5 %>% split(.$Product)

#rm(cjd_t4);rm(cjd_t5);rm(cjd_t6)

for(i in 1:length(cjd_t6)){
  cjd_t6[[i]]$duration =  round(rnorm(nrow(cjd_t6[[i]]), journey_duration[i],journey_devation[i]),2)
  cjd_t6[[i]]$start_hour =  round(rnorm(nrow(cjd_t6[[i]]), start_hour[i],start_variation[i]),2)
}

cjd_staging = bind_rows(cjd_t6) %>% 
  mutate(Stage_1_duration = ifelse(stages == 4,1,ifelse(stages == 6,1,
                                                        ifelse(is_in_order == 1,1, round(rnorm(length(stages),0.7,0.1))))),
         Stage_2_duration = ifelse(is_in_order == 1 &  stages == 3,2, 
                                   ifelse(stages == 4,2,ifelse(Stage_1_duration == stages, 0,
                                                               ifelse(is_in_order == 1 &  stages == 2,2,ifelse(stages < 2 & is_in_order == 1,0, 
                                                                                                               ifelse(stages == 6,2,ifelse(is_in_order == 1 &  stages == 2,2,round(rnorm(length(stages),0.6,0.1))*2))))))),
         Stage_3_duration = ifelse(is_in_order == 1 &  stages == 3,3,
                                   ifelse(stages == 6,3,ifelse(Stage_1_duration+Stage_2_duration == stages, 0,
                                                               ifelse(stages - Stage_1_duration+Stage_2_duration >1, 3,
                                                                      ifelse(stages < 3 & is_in_order == 1,0,ifelse(stages == 5,3,
                                                                                                                    ifelse(is_in_order == 0 &  Stage_1_duration+Stage_2_duration == 1 &  stages == 2,3,round(rnorm(length(stages),0.6,0.1))*3 ))))))),
         Stage_4_duration = ifelse(stages == 6,4,   ifelse(Stage_1_duration+Stage_2_duration+Stage_3_duration == stages, 0,
                                                           ifelse(is_in_order == 1 &  stages == 4,4,ifelse(stages < 4 & is_in_order == 1,0,
                                                                                                           ifelse(stages == 5,1,ifelse(is_in_order == 1 &  stages == 4,4,
                                                                                                                                       ifelse(is_in_order == 0 &  Stage_1_duration+Stage_2_duration+Stage_3_duration <= 3 &  stages == 3,1,round(rnorm(length(stages),0.6,0.1))*4 ))))))),
         Stage_5_duration = ifelse(stages == 6,5,   ifelse(Stage_1_duration+Stage_2_duration+Stage_3_duration+Stage_4_duration == stages, 0,
                                                           ifelse(is_in_order == 1 &  stages == 5,4,ifelse(stages < 5 & is_in_order == 1,0,
                                                                                                           ifelse(stages == 6,1,ifelse(is_in_order == 1 &  stages == 5,5,
                                                                                                                                       ifelse(is_in_order == 0 &  Stage_1_duration+Stage_2_duration+Stage_3_duration+Stage_4_duration <= 4 &  stages == 4,5,round(rnorm(length(stages),0.6,0.1))*5) )))))),
         Stage_6_duration = ifelse(stages < 6,0,
                                   ifelse(stages == 6,6,ifelse(is_in_order == 1 &  stages == 6,6,
                                                               ifelse(is_in_order == 0 &  Stage_1_duration+Stage_2_duration+Stage_3_duration+Stage_4_duration+Stage_5_duration <= 4 &  stages == 5,6,round(rnorm(length(stages),0.6,0.1))))))
  )

cjd_staging_ark =  bind_rows(cjd_staging %>% mutate(Start_Year = year(start_date)) %>%group_by(Stage_1_duration,Stage_2_duration,Start_Year, Brand,Product) %>% summarise(Applications = n()) %>%  rename( Start = Stage_1_duration,End = Stage_2_duration) %>% data.frame(),
                             cjd_staging %>% mutate(Start_Year = year(start_date)) %>%group_by(Stage_2_duration,Stage_3_duration,Start_Year, Brand,Product) %>% summarise(Applications = n()) %>%  rename( Start = Stage_2_duration,End = Stage_3_duration) %>% data.frame(),
                             cjd_staging %>% mutate(Start_Year = year(start_date)) %>%group_by(Stage_3_duration,Stage_4_duration,Start_Year, Brand,Product) %>% summarise(Applications = n()) %>%  rename( Start = Stage_3_duration,End = Stage_4_duration) %>% data.frame(),
                             cjd_staging %>% mutate(Start_Year = year(start_date)) %>%group_by(Stage_4_duration,Stage_5_duration,Start_Year, Brand,Product) %>% summarise(Applications = n()) %>%  rename( Start = Stage_4_duration,End = Stage_5_duration) %>% data.frame(),
                             cjd_staging %>% mutate(Start_Year = year(start_date)) %>%group_by(Stage_5_duration,Stage_6_duration,Start_Year, Brand,Product) %>% summarise(Applications = n()) %>%  rename( Start = Stage_5_duration,End = Stage_6_duration) %>% data.frame()
) %>%  filter(Start != 0)


write.delim(cjd_staging,"cjd_detail.txt", sep = "|")
write.delim(cjd_staging_ark,"cjd_staging_ark.txt", sep = "|")

########## Start:Payments table ##########
head(cs_source)













