############### project prediction data layer ###############
n_lines = 5000




#############################################################

project_id = paste0("project_",1:n_lines)
division = c("Finance", "Operations", "Transformation", "Marketing", "Human Resource",  "IT")
date = 
year



year_combo = function(start,end){
  temp = rep(NA,length(start:end)*12)
  for(i in 1:length(start:end)){
    temp[i] = as.Date(paste0("01/",1:12,"/",i),format = "%d/%m/%Y") 
  }
}



budget = 
length = 

