library(tidyverse);library(purrr);library(magrittr);library(caroline);library(tidyr);library(lubridate);library(RPostgreSQL);options(scipen=999)

dsol_proto = dbConnect(PostgreSQL(), user = "czucchet",
                       password = "EB%bti15ICh6NYX",
                       dbname = "dev",
                       host = "nonprod-dsol-prototyping-db.ctolc6xouppg.eu-west-1.rds.amazonaws.com", port = "5432")

########################################################
############### 1) Variables ###########################
############### 1a) Project table Variables ############
client_name = "LBG"
projects_yearly = 7000;project_threshold_yearly = 200
project_yearly_budget = 2000000000;project_change = 1000000
project_years = 3
project_current_date = as.Date(Sys.Date(), format = "%d/%m/%Y")
project_start_date = as.Date(paste0("01/",month(Sys.Date())+1,"/",(year(Sys.Date())-project_years)), format = "%d/%m/%Y")
total_spend = rnorm(1,project_yearly_budget*project_years,project_change*project_years)
no_projects = round(rnorm(1,projects_yearly*project_years,project_threshold_yearly))
BUs = 3
BU_prob = runif(BUs,0,1)
Cost_Centres = 8
CC_prob = runif(Cost_Centres,0,1)
duration_max = 36
duration_prob = rbeta(1:duration_max,1,2)*runif(length(1:duration_max),0.5,1)
status_change = 0.57;status_change_var  = 0.05
audit_multi = 5

################## 1b) Functions #####################
random_gen_project <- function(n) {
  random_t <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(random_t, sprintf("%04d", sample(999999, n, TRUE)), sample(LETTERS, n, TRUE))
}
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
roundUp <- function(x){round(x+4,-2)}

months_diff <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date);sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}


################## 1c) References #####################
operating_units = c("Production","Research and Development","Purchasing","Marketing","Human Resource Management","Finance", "Strategy", "Marketing","Operations")
location = c("London, UK","Manchester, UK","Birmingham, UK","Bristol, UK","Liverpool, UK","Swindon, UK","Newcastle, UK","Brighton, UK","London, UK","London, UK","London, UK");location_probs = sort(runif(length(location),0,1), decreasing = TRUE)

project_name = c("Supplier Management System","Front End Tools Migration","Quality Evaluation Tool","IT Program",
                 "BU & Pilotage","Unified Communication Project","HR Management Tool","Identity and Access Management",
                 "Operating System Upgrade","Records Management","Public cloud evolution","Backup evolution","London, UK",
                 "London, UK","London, UK","London, UK","London, UK","London, UK","London, UK","London, UK","London, UK","London, UK");
project_probs = sort(runif(length(location),0,1), decreasing = TRUE)
priority = c("Low", "Medium", "High","Medium","High","Low", "High")
priority_probs = sort(runif(length(priority),0,1), decreasing = TRUE)
######################################################
############### 2a) Project metadata table ###########
set.seed(1234)
project_metadata = data.frame(client_name = rep(client_name,no_projects), project_id = rep(NA,no_projects)) %>% mutate_if(is.factor, as.character) %>% 
  mutate(project_id = random_gen_project(length(project_id)),
         project_start_date =as.Date(round((as.numeric(project_current_date) -as.numeric(project_start_date))*rbeta(length(project_id),250,1)*runif(length(project_id),0,1)*rbeta(length(project_id),500,1))
                                     +as.numeric(project_start_date),origin = "1970-01-01"),
         budget_t = rbeta(length(project_id), 1,4),
         budget = roundUp(budget_t/sum(budget_t)*total_spend),
         capex_t = runif(length(project_id)),opex_t = runif(length(project_id)),
         capex_t2 =capex_t/(opex_t+capex_t), opex_t2 =opex_t/(opex_t+capex_t), 
         capex = roundUp(capex_t2*budget),
         opex = budget-capex,
         bu = sample(1:BUs,length(project_id),prob = BU_prob, replace = T),
         cost_centre = sample(1:Cost_Centres,length(project_id),prob = CC_prob, replace = T),
         duration_months = sample(1:duration_max,length(project_id),prob = duration_prob, replace = T),
         end_date_t = project_start_date %m+% months(duration_months),
         est_end_date = as.Date(paste0(substring(end_date_t,1,4),"-",substring(end_date_t,6,7),"-","01"),"%Y-%m-%d"),
         location = sample(location, length(project_id), prob = location_probs,replace = T),
         completion_pcent = round(ifelse(est_end_date > Sys.time(), (duration_months - months_diff(est_end_date,Sys.time()))/duration_months, 1),4)*100,
         is_in_progress = ifelse(completion_pcent == 100, 0,1),
         was_over_budget =  ifelse(is_in_progress == 0, round(rnorm(length(project_id), 0.5,0.1),0), NA),
         was_over_time = ifelse(is_in_progress == 0 & was_over_budget == 1,round(rnorm(length(project_id), 0.45,0.1),0), 
                                ifelse(is_in_progress == 0,round(rnorm(length(project_id), 0.5,0.1),0),NA)),
         actual = ifelse(was_over_budget == 1,roundUp(((rbeta(length(project_id), 1,8)*10)+1)*budget),
                         ifelse(was_over_budget == 0 & is_in_progress == 0,budget,NA)),
         actual_time_t = ifelse(was_over_time == 1,project_start_date %m+% months(round((actual/budget)+rnorm(length(project_id),1,0.1)*duration_months,0)),
                                ifelse(was_over_time == 0 & is_in_progress == 0, est_end_date,
                                       NA)),
         actual_time = as.Date(actual_time_t,origin = "1970-01-01"),
         priority = sample(priority, length(project_id), prob = priority_probs,replace = T),
         pred_was_over_budget = ifelse(was_over_budget == 1, round(rnorm(length(project_id), 0.65,0.1),0), 
                                       ifelse(was_over_budget == 0, round(rnorm(length(project_id), 0.35,0.1),0),NA)),
         pred_was_over_time = ifelse(was_over_time == 1, round(rnorm(length(project_id), 0.635,0.1),0), 
                                     ifelse(was_over_time == 0, round(rnorm(length(project_id), 0.35,0.1),0),NA)),
         had_audit = ifelse(priority == "High" & was_over_budget == 1, round(rnorm(length(project_id), 0.33,0.1),0),
                            ifelse(priority == "High" & was_over_budget == 0, round(rnorm(length(project_id), 0.30,0.1),0),
                                   ifelse(priority == "Medium" & was_over_budget == 1, round(rnorm(length(project_id), 0.32,0.1),0),
                                          ifelse(priority == "Medium" & was_over_budget == 0, round(rnorm(length(project_id), 0.25,0.1),0),round(rnorm(length(project_id), 0.20,0.1),0))))),
         had_audit = ifelse(is.na(had_audit),0,had_audit),
         n_audits = ifelse(had_audit == 1,round(rbeta(length(project_id), 1,8)*audit_multi,0),0),
         overrun_index = ifelse(is_in_progress == 1,round(budget/budget,3), round(actual/budget,3)),
         project_picture = ifelse(overrun_index > 1.7, "High",ifelse(overrun_index > 1.2 & overrun_index <= 1.7,"Medium", "Low")),
         exp_spend = ifelse(is.na(actual),roundUp(completion_pcent/100*budget*rnorm(length(is.na(actual)),1,0.03)) , actual),
         phs_t = ifelse(project_picture == "Low", rnorm(length(project_picture[project_picture == "Low"]), 0.75, 0.05)+rnorm(length(project_picture[project_picture == "Low"]), 0, 0.01) ,
                        ifelse(project_picture == "Medium", rnorm(length(project_picture[project_picture == "Medium"]), 0.68, 0.05)+rnorm(length(project_picture[project_picture == "Medium"]), 0, 0.01) ,
                               ifelse(project_picture == "High", rnorm(length(project_picture[project_picture == "High"]), 0.61, 0.05)+rnorm(length(project_picture[project_picture == "High"]), 0, 0.01),
                                      rnorm(length(project_picture), 0.68, 0.05)+rnorm(length(project_picture), 0, 0.01)
                               )))
  )  %>% select(-budget_t,-capex_t,-opex_t,-capex_t2,-opex_t2,-end_date_t,-actual_time_t) %>% 
  filter(budget != 0) %>% filter(!is.na(client_name))


dbRemoveTable(dsol_proto, paste0(client_name,"_PROJECT_SUMMARY_TABLE"))
dbWriteTable(dsol_proto,paste0(client_name,"_PROJECT_SUMMARY_TABLE"),project_metadata)

############### 2b) Project detail table ###########

rating = c("Green","Amber","Red")

status = c("Low","Medium","High");green_probs = c(0.85,0.6,0.3);amber_probs = c(0.1,0.35,0.2);red_probs = c(0.05,0.15,0.45)
status_df = data.frame(status,green_probs,amber_probs,red_probs)
progress_v_plan = c("Low","Medium","High");green_probs = c(0.85,0.6,0.3);amber_probs = c(0.1,0.35,0.2);red_probs = c(0.05,0.15,0.45)
progress_v_plan_df = data.frame(progress_v_plan,green_probs,amber_probs,red_probs)
costs = c("Low","Medium","High");green_probs = c(0.85,0.6,0.3);amber_probs = c(0.1,0.35,0.2);red_probs = c(0.05,0.15,0.45)
costs_df = data.frame(costs,green_probs,amber_probs,red_probs)
benefits = c("Low","Medium","High");green_probs = c(0.85,0.6,0.3);amber_probs = c(0.1,0.35,0.2);red_probs = c(0.05,0.15,0.45)
benefits_df = data.frame(benefits,green_probs,amber_probs,red_probs)
risks = c("Low","Medium","High");green_probs = c(0.85,0.6,0.3);amber_probs = c(0.1,0.35,0.2);red_probs = c(0.05,0.15,0.45)
risks_df = data.frame(risks,green_probs,amber_probs,red_probs)
resources = c("Low","Medium","High");green_probs = c(0.85,0.6,0.3);amber_probs = c(0.1,0.35,0.2);red_probs = c(0.05,0.15,0.45)
resources_df = data.frame(resources,green_probs,amber_probs,red_probs)
customers = c("Low","Medium","High");green_probs = c(0.85,0.6,0.3);amber_probs = c(0.1,0.35,0.2);red_probs = c(0.05,0.15,0.45)
customers_df = data.frame(customers,green_probs,amber_probs,red_probs)


project_view = project_metadata %>% select(project_id, project_picture, project_start_date,completion_pcent,is_in_progress,phs_t)
project_detail_t = length(project_metadata$project_id) %>% 
  rerun(matrix(NA, 1,2)) 



for(i in 1:length(project_detail_t)){
  project_detail_t[[i]] = data.frame(project_detail_t[[i]])
  if(is.na(project_metadata$actual[i])){
    date_length = seq(project_metadata[i,"project_start_date"],Sys.Date(), by = 30)
  } else {
    date_length = seq(project_metadata[i,"project_start_date"],project_metadata[i,"actual_time"], by = 30)
  }
  project_detail_t[[i]] = data.frame(project_id =  rep(project_metadata[i,"project_id"] , length(date_length)), status_date = date_length)
  project_detail_t[[i]]$cost_tracking = sort(runif(length(date_length)))
  project_detail_t[[i]]$cum_spend = roundUp(project_detail_t[[i]]$cost_tracking*project_metadata$exp_spend[i]) 
  project_detail_t[[i]]$project_picture = project_view$project_picture[match(project_detail_t[[i]]$project_id,project_view$project_id)]
}

project_metadata_temp = project_metadata %>% 
  filter(!is.na(actual)) %>%
  mutate(project_id = project_id,status_date = actual_time, cost_tracking = 1, cum_spend = exp_spend, project_picture = project_picture)%>% 
  select(project_id, status_date, cost_tracking,cum_spend,project_picture) 

project_detail_t2 = bind_rows(project_detail_t)
project_detail_t3 = bind_rows(project_detail_t2,project_metadata_temp)
project_detail_t4 = project_detail_t3 %>% 
  split(.$project_picture) 

for(i in 1:length(project_detail_t4)){
  project_detail_t4[[i]]$rag_status = sample(rating, nrow(project_detail_t4[[i]]),prob =  status_df[which(unique(project_detail_t4[[i]]$project_picture) == status_df$status) ,2:4], replace = T)
  project_detail_t4[[i]]$progress_to_plan_t = round(rnorm(nrow(project_detail_t4[[i]]), status_change,status_change_var))
  project_detail_t4[[i]]$progress_to_plan = ifelse(project_detail_t4[[i]]$progress_to_plan_t == 0,  
                                                   sample(rating, length(project_detail_t4[[i]]$progress_to_plan_t == 1),prob =  progress_v_plan_df[which(unique(project_detail_t4[[i]]$project_picture) == progress_v_plan_df$progress_v_plan) ,2:4], replace = T),
                                                   project_detail_t4[[i]]$rag_status)
  project_detail_t4[[i]]$costs_t = round(rnorm(nrow(project_detail_t4[[i]]), status_change,status_change_var))
  project_detail_t4[[i]]$costs = ifelse(project_detail_t4[[i]]$costs_t == 0,  
                                        sample(rating, length(project_detail_t4[[i]]$costs_t == 1),prob =  costs_df[which(unique(project_detail_t4[[i]]$project_picture) == costs_df$costs) ,2:4], replace = T),
                                        project_detail_t4[[i]]$rag_status)
  project_detail_t4[[i]]$risks_t = round(rnorm(nrow(project_detail_t4[[i]]), status_change,status_change_var))
  project_detail_t4[[i]]$risks = ifelse(project_detail_t4[[i]]$risks_t == 0,  
                                        sample(rating, length(project_detail_t4[[i]]$risks_t == 1),prob =  risks_df[which(unique(project_detail_t4[[i]]$project_picture) == risks_df$risks) ,2:4], replace = T),
                                        project_detail_t4[[i]]$rag_status)
  project_detail_t4[[i]]$customers_t = round(rnorm(nrow(project_detail_t4[[i]]), status_change,status_change_var))
  project_detail_t4[[i]]$customers = ifelse(project_detail_t4[[i]]$customers_t == 0,
                                            sample(rating, length(project_detail_t4[[i]]$customers_t == 1),prob =  customers_df[which(unique(project_detail_t4[[i]]$project_picture) == customers_df$customers) ,2:4], replace = T),
                                            project_detail_t4[[i]]$rag_status)
  project_detail_t4[[i]]$benefits_t = round(rnorm(nrow(project_detail_t4[[i]]), status_change,status_change_var))
  project_detail_t4[[i]]$benefits = ifelse(project_detail_t4[[i]]$benefits_t == 0,
                                           sample(rating, length(project_detail_t4[[i]]$benefits_t == 1),prob =  benefits_df[which(unique(project_detail_t4[[i]]$project_picture) == benefits_df$benefits) ,2:4], replace = T),
                                           project_detail_t4[[i]]$rag_status)
  project_detail_t4[[i]]$resources_t = round(rnorm(nrow(project_detail_t4[[i]]), status_change,status_change_var))
  project_detail_t4[[i]]$resources = ifelse(project_detail_t4[[i]]$resources_t == 0,
                                            sample(rating, length(project_detail_t4[[i]]$resources_t == 1),prob =  resources_df[which(unique(project_detail_t4[[i]]$project_picture) == resources_df$resources) ,2:4], replace = T),
                                            project_detail_t4[[i]]$rag_status)
}


project_detail = bind_rows(project_detail_t4) %>%  select(-costs_t, -progress_to_plan_t,-risks_t,-customers_t,-resources_t,-benefits_t,-cost_tracking) %>% 
  arrange(project_id, status_date) %>% 
  mutate(same_project_t = ifelse(lag(project_id,1) == project_id,1,0),
         same_project = ifelse(is.na(same_project_t),0,same_project_t),
         period_spend = ifelse(same_project == 1, cum_spend - lag(cum_spend,1),cum_spend),
         time_since_update = ifelse(same_project == 1, status_date - lag(status_date,1),0)) %>% select(-same_project_t) %>% left_join(project_view) %>% 
  mutate(phs_t_var = rnorm(length(period_spend), 0,0.015)+rnorm(length(period_spend), 0,0.015),
         phs =  phs_t + phs_t_var) %>% select(-phs_t,-phs_t_var)

head(project_detail)

dbRemoveTable(dsol_proto, paste0(client_name,"_PROJECT_DETAIL"))
dbWriteTable(dsol_proto,paste0(client_name,"_PROJECT_DETAIL"),project_detail)




nrow(project_detail)
project_metadata %>% filter(project_id == "AAAJS750448I")

head(project_metadata)

