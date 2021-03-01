#' @title Fitness Data Import Function
#'
#' @description Reads a fitness data file in table format and subset it to return the corrct dataframe  
#' @param dataset Fitness dataset
#' @param remove_testenvt remove all data measured on this test_environment (could be a vector with several test environment)
#' @param remove_pop remove all data of this population
#' @param remove_rate if TRUE remove data where number of adults is higher than the number of eggs
#' @param trait can be only "preference" of "performance"
#' 
#'
#' @return
#' @export
#'
#' @examples
#'data_PERF <- import_data(dataset = "DATACOMPLET_PERF.csv", trait = "performance",
#' remove_testenvt = c("Grape","GF"), remove_pop = "WT3", remove_rate = NA)

import_data <- function(dataset = "DATACOMPLET_PERF.csv", trait = "performance",
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
  data_complet$Obs_O<-as.factor(data_complet$Obs_O)
  data_complet$Column<-as.factor(data_complet$Column)  
  
  if (trait == "performance") {
    data_complet$Obs_A<-as.factor(data_complet$Obs_A)
    data_complet$Row<-as.factor(data_complet$Row)
    data_complet$Rack<-as.factor(data_complet$Rack)
  }else{
    if (trait == "preference") {
      data_complet$Line<-as.factor(data_complet$Line)
      data_complet$BoxID<-as.factor(data_complet$BoxID)
      data_complet$Nb_eggs <- as.numeric(as.character(data_complet$Nb_eggs))
    }else{
      print("Error: trait unknown")
    } 
  }
  

  
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
  
  #Problem for SA when levels Test_environment is different than Original environment
  if(is.na(remove_rate)){ 
  levels(data$Original_environment) <- c(levels(data$Original_environment),levels(data$Test_environment))
  levels(data$Test_environment) <- c(levels(data$Test_environment),levels(data$Original_environment))
  }
  
  if(is.na(remove_testenvt[1])){ 
    levels(data$Original_environment) <- c(levels(data$Original_environment),levels(data$Test_environment))
    levels(data$Test_environment) <- c(levels(data$Test_environment),levels(data$Original_environment))
  }
  
  
  if (trait == "performance") {
  data$EggScore <- as.factor(ifelse(data$Nb_eggs<51, 1, 
                                    ifelse(data$Nb_eggs<101, 2, 
                                           ifelse(data$Nb_eggs<151, 3, 4))))
  data$EggScoreFive <- as.factor(ifelse(data$Nb_eggs<51, 1, 
                                        ifelse(data$Nb_eggs<101, 2,
                                               ifelse(data$Nb_eggs<151, 3, 
                                                      ifelse(data$Nb_eggs<201, 4, 5)))))
  data$EggScoreSmall <- as.factor(ifelse(data$Nb_eggs<51, 1, 
                                         ifelse(data$Nb_eggs<76, 2, 
                                                ifelse(data$Nb_eggs<101, 3, 
                                                       ifelse(data$Nb_eggs<126, 4, 
                                                              ifelse(data$Nb_eggs<151, 5, 6))))))
  }
  
  
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
  
  data <- droplevels(data)
  
  
  ######## 
  ######## Add emergence rate for performance
  ######## 
  if (trait == "performance") {
  temp_g0 <- length(data$Nb_eggs[data$Generation=="G0"])
  temp_g2 <- length(data$Nb_eggs[data$Generation=="G2"])
  
  if(remove_rate == TRUE){ 
    #Remove eggs = NA
    NA_eggs_G0 <- dim(data[is.na(data$Nb_eggs)&data$Generation=="G0",])[1]
    NA_eggs_G2 <- dim(data[is.na(data$Nb_eggs)&data$Generation=="G2",])[1]
    data <- data[!is.na(data$Nb_eggs),]
    
    #Remove adults = NA
    NA_adults <- dim(data[is.na(data$Nb_adults),])[1]
    data <- data[!is.na(data$Nb_adults),]
    
    #Remove eggs = 0
    NA_zeroeggs_G0 <- dim(data[data$Nb_eggs==0&data$Generation=="G0",])[1]
    NA_zeroeggs_G2 <- dim(data[data$Nb_eggs==0&data$Generation=="G2",])[1]
    data <- data[data$Nb_eggs!=0,]
    
    #Remove adults > eggs
    temp_g0remv <- length(data$Nb_eggs[data$Nb_adults>data$Nb_eggs&
                                         data$Generation=="G0"])
    temp_g2remv <- length(data$Nb_eggs[data$Nb_adults>data$Nb_eggs&
                                         data$Generation=="G2"])
    data <- data[data$Nb_adults<=data$Nb_eggs,]
    
    #Emegence Rate
    data$Rate <- data$Nb_adults / data$Nb_eggs
    
    print(paste0("Data (" , temp_g0, " and ", temp_g2, 
                 " tubes for the first and third generation respectively) where i) the number of eggs was NA (", 
                 NA_eggs_G0 ," and ", NA_eggs_G2,
                 " tubes for the first and third generation respectively); or ", 
                 "ii) the number of adults  was NA (", NA_adults, " and ", NA_adults,
                 " tubes for the first and third generation respectively); or ", 
                 "iii) the number of eggs was zero -Emergence rate = NaN- (",NA_zeroeggs_G0, " and ",NA_zeroeggs_G2 , 
                 " tubes for the first and third generation respectively); or ", 
                 "iv) the number of adults was higher than the number of eggs (",temp_g0remv, " and " ,temp_g2remv,
                 " tubes for the first and third generation respectively) were removed."))
    
    
  }else{ 
    if(is.na(remove_rate)){ 
      data$Rate <- data$Nb_adults / data$Nb_eggs

            print(paste0("Data (" , temp_g0, " and ", temp_g2, 
                   " tubes for the first and third generation respectively) where i) the number of eggs was NA (", 
                   NA_eggs_G0 ," and ", NA_eggs_G2,
                   " tubes for the first and third generation respectively); or ", 
                   "ii) the number of adults  was NA (", NA_adults, " and ", NA_adults,
                   " tubes for the first and third generation respectively); or ", 
                   "iii) the number of eggs was zero -Emergence rate = NaN- (",NA_zeroeggs_G0, " and ",NA_zeroeggs_G2 , 
                   " tubes for the first and third generation respectively); or ", 
                   "iv) the number of adults was higher than the initial number of eggs (",temp_g0remv, " and " ,temp_g2remv,
                   " tubes for the first and third generation respectively) were not removed."))

    }else{
      print("Error: unknown remove_rate")
    }
      
  }

  }
  
  
  
  return(data)
}
