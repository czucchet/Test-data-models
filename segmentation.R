library(tidyverse);library(rsample);library(magrittr);library(caroline)

################# Variables #################

cust = 500000;threshold = 100000
brands = 4
Products = 5 # To leave as 5 for retail Product analysis
add_brands = runif(1,0.35,0.45)
complaints_multi = 7
di_base = 1200
account_multi = 4
PCA_Multi = 2500; Savings_Multi = 5000;Mortgage_Multi = 500000;CC_Multi = 10000;Loans_Multi = 100000
Starting_Origin = "01/01/2001"
################# Data layer info ############
segment_cd = 1:12
Segment = c("High Income Professionals","Rising Metropolitans","Asset-Rich Greys","Home-Owning Families","Road to Retirement","Working Single & Couples",
            "Older Working Families","Low Income Elderly","Still at Home","Starting Out","Poorer Parents","Mid-Life Social Renters")
segment_df = data.frame(segment_cd,Segment) %>% mutate(Segment = as.character(Segment),segment_cd = as.character(segment_cd))
#############################################

set.seed(1234)
ID = paste0("Cust_", 1:round(runif(1,cust-threshold,cust+threshold)))
Brand = paste0("Brand_", sample(1:brands, size=length(ID), prob=runif(brands,0,1), replace=TRUE))
Product = paste0("Product_", sample(1:Products, size=length(ID), prob=runif(Products,0,1), replace=TRUE))
Product = ifelse(Product == names(sort(table(Product), decreasing = T))[1], "PCA",ifelse(Product == names(sort(table(Product), decreasing = T))[2], "Savings",
                        ifelse(Product == names(sort(table(Product), decreasing = T))[3], "Credit Card",ifelse(Product == names(sort(table(Product), decreasing = T))[4], "Mortgages",
                                      ifelse(Product == names(sort(table(Product), decreasing = T))[5], "Loans", "Other")))))
cs_source_t = data.frame(ID,Brand,Product)

#############################################
add_ID = sample(length(ID), size = round(length(ID)*add_brands), replace = T);add_ID = paste0("Cust_",add_ID)
add_brand = paste0("Brand_", sample(1:brands, size=length(add_ID), prob=runif(brands,0,1), replace=TRUE))
add_Product = paste0("Product_", sample(1:Products, size=length(add_ID), prob=runif(Products,0,1), replace=TRUE))
add_Product = ifelse(add_Product == names(sort(table(add_Product), decreasing = T))[1], "PCA",ifelse(add_Product == names(sort(table(add_Product), decreasing = T))[2], "Savings",
            ifelse(add_Product == names(sort(table(add_Product), decreasing = T))[3], "Credit Card",ifelse(add_Product == names(sort(table(add_Product), decreasing = T))[4], "Mortgages",
            ifelse(add_Product == names(sort(table(add_Product), decreasing = T))[5], "Loans", "Other")))))

cs_source_t2 = data.frame(add_ID,add_brand,add_Product) %>%  rename(ID = add_ID,Brand = add_brand,Product = add_Product)

cs_source_t3 = rbind(cs_source_t,cs_source_t2)

segment_t = data.frame(segment_cd = sample(1:length(Segment), size=nrow(cs_source_t3), prob=runif(length(Segment),0,1), replace=TRUE)) %>% mutate(segment_cd = as.character(segment_cd))
segment = left_join(segment_t,segment_df) %>% select(Segment)

rewards = ifelse(rnorm(nrow(cs_source_t3), 0.3,0.2) > 0.5 ,1,0)
complaints = round(rbeta(nrow(cs_source_t3),1,10)*complaints_multi)
Distress_Payments = round(rbeta(nrow(cs_source_t3),1,8)*di_base)
Balance = ifelse(Product == "PCA", round(rbeta(length(Product[Product == "PCA"]),1,4)*PCA_Multi),ifelse(Product == "Savings", round(rbeta(length(Product[Product == "Savings"]),1,5)*Savings_Multi),
                ifelse(Product == "Credit Card", round(rbeta(length(Product[Product == "Credit Card"]),1,4)*CC_Multi),ifelse(Product == "Mortgages", round(rbeta(length(Product[Product == "Mortgages"]),1,8)*Mortgage_Multi),
                              ifelse(Product == "Loans", round(rbeta(length(Product[Product == "Mortgages"]),1,8)*Loans_Multi),round(rbeta(1,1,8)*Loans_Multi))))))

account_sts_t = round(rbeta(nrow(cs_source_t3),1,8)*account_multi);
Account_sts = ifelse(account_sts_t == 0, "Primary Active",ifelse(account_sts_t == 1, "Primary Inactive",ifelse(account_sts_t == 2, "Secondary Active",  ifelse(account_sts_t == 3, "Primary Inactive","Other"))))

start_date_base = as.numeric(as.Date(Starting_Origin, format = "%d/%m/%Y", origin ="1970-01-01"));start_date_end = as.numeric(Sys.Date(), origin ="1970-01-01")
start_date = as.Date(round(runif(nrow(cs_source_t3),start_date_base,start_date_end)),origin ="1970-01-01")

has_left = ifelse(rnorm(nrow(cs_source_t3), 0.3,0.2) > 0.5 ,1,0)

start_date




account_sts_t = round(rbeta(nrow(cs_source_t3),1,8)*account_multi)

start_date_numeric
end_date = 

  


    
########## Housekeeping #####################
rm(cs_source_t);rm(cs_source_t2)





    