#' @title Fitness Data Import Function
#'
#' @description Reads a fitness data file in table format and subset it to return the corrct dataframe  
#' @param dataset Fitness dataset
#' @param remove_testenvt remove all data measured on this test_environment
#' @param remove_pop remove all data of this population
#' @param remove_rate remove data where number of adults is higher than the number of eggs
#'
#' @return
#' @export
#'
#' @examples
#'data_PERF <- import_data(dataset = "DATACOMPLET_PERF.csv", remove_testenvt = c("Grape","GF"), remove_pop = "WT3", remove_rate = NA)

import_data <- function(dataset = "DATACOMPLET_PERF.csv", 
                        remove_testenvt = c("Grape","GF"), 
                        remove_pop = "WT3",
                        remove_rate = NA){
  
  ######## 
  ######## Import dataset
  ########
  ## Path using the here function so that the command line is reproducible across platforms
  data_complet <- read.table(file=here::here("data", dataset), sep=",", header=TRUE)
  
  ######## 
  ######## Remove unless variables
  ######## 
  data_complet$Generation<-as.factor(data_complet$Generation)
  data_complet$Experiment<-as.factor(data_complet$Experiment)
  data_complet$Original_environment<-as.factor(data_complet$Original_environment)
  data_complet$Test_environment <- as.factor(data_complet$Test_environment)
  data_complet$Population <- as.factor(data_complet$Population)
  data_complet$Row<-as.factor(data_complet$Row)
  data_complet$Column<-as.factor(data_complet$Column)  
  data_complet$Rack<-as.factor(data_complet$Rack)
  data_complet$Obs_O<-as.factor(data_complet$Obs_O)
  data_complet$Obs_A<-as.factor(data_complet$Obs_A)
  
  
  ######## 
  ######## Subset dataset
  ########
  data <- data_complet
  
  if(!is.na(remove_pop)){
    data <- data[data$Original_environment != remove_pop,]
    }
  
  if(!is.na(remove_testenvt[1])){
    for (i in 1:length(remove_testenvt)) {
      data <- data[data$Test_environment != remove_testenvt[i],]
    }
  }

  data <- droplevels(data)
  

  
  ######## 
  ######## Add indic and useful variables
  ######## 
  data$SA <- as.factor(ifelse(data$Original_environment == data$Test_environment, 1, 0))
  data$IndicG0 <- as.numeric(ifelse (data$Generation == "G0", 1, 0))
  data$IndicG2 <- as.numeric(ifelse(data$Generation == "G2", 1, 0))
  
  data$SAIndicG0 <- as.numeric(ifelse (data$Generation == "G0"&
                                       data$SA == "1", 1, 0))

  data$fruit_hab <- as.factor(paste(data$Original_environment, 
                                    data$Test_environment, sep = "_"))
  data$fruit_hab_ng <- as.factor(paste(data$Original_environment, 
                                       data$Test_environment, 
                                       data$Generation, sep = "_"))
  
  data$fruit_gen <- as.factor(paste(data$Original_environment, 
                                    data$Generation, sep = "_"))
  data$hab_gen <- as.factor(paste(data$Test_environment, 
                                  data$Generation, sep = "_"))
  data$pop_gen <- as.factor(paste(data$Population, 
                                  data$Generation, sep = "_"))
  
  ######## 
  ######## Add emergence rate
  ######## 
  temp_g0remv <- length(data$Nb_eggs[data$Nb_adults>data$Nb_eggs&
                                       data$Generation=="G0"])
  temp_g2remv <- length(data$Nb_eggs[data$Nb_adults>data$Nb_eggs&
                                       data$Generation=="G2"])
  temp_g0 <- length(data$Nb_eggs[data$Generation=="G0"])
  temp_g2 <- length(data$Nb_eggs[data$Generation=="G2"])
  
  if(is.na(remove_rate)){ 
        data$Rate <- data$Nb_adults / data$Nb_eggs
        print(paste0("Data where the number of adults was higher than the number of eggs were not removed (",
                     temp_g0remv, " and ", temp_g2remv, " tubes from ",temp_g0, " and ", temp_g2, 
                     " tubes for the first and thrid generation respectively)."))
  }else{ 
    if(remove_rate == TRUE){ 
        data$Rate <- data$Nb_adults / data$Nb_eggs
        print(paste0("For the first generation, ", temp_g0remv,
                     " tubes where the number of adults was higher than initial number of eggs were removed from ", 
                     temp_g0, " tubes."))
        print(paste0("For the third generation, ", temp_g2remv,
                     " tubes where the number of adults was higher than initial number of eggs were removed from ", 
                     temp_g2, " tubes."))
        data <- data[data$Nb_adults<=data$Nb_eggs,]
    }else{
      print("Error: unknown remove_rate")
    }
      
  }

  
  return(data)
}
