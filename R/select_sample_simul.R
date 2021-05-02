#' Select a sample of simulations
#'
#' @description Select the same number of simul for each SA value (Avoid over-representing some SA values)
#' @param dataset_sim dataset with simulated data after index with index the add_sim_number_SA()
#' @param type_SA Select on SA genetic or SA non-genetic
#' @param nb_simul_per_SA The number of simulations kept for each SA value
#' @param minimum_SA To select or not on the minimum values of SA 
#' If "0": No select dataset with val_SA_available_MIN. The dataset will start with SA=0 (val_SA_available_MIN=0) even if less than nb_simul_per_SA for SA=0. 
#' If "select": Select dataset with val_SA_available_MIN. The lowest SA will be the lowest SA with at least nb_simul_per_SA simuls. 
#' 
#' @return the same dataset with new columns
#' @export 
#'
#' @examples select_sample_simul(dataset_sim = sim)

select_sample_simul <- function(dataset_sim = sim, type_SA = "genetic",
                                nb_simul_per_SA = 100, minimum_SA = "0") {
  
  
  if (type_SA == "genetic") {
    
  # Calculate the highest SA value with at least  nb_simul_per_SA simuls
  #####val_SA_available_MAX<-max(dataset_sim$SA_Gen[dataset_sim$index_SA_Gen==nb_simul_per_SA])
  val_rs_available_MAX<-max(dataset_sim$binnedrsqgen[dataset_sim$index_SA_Gen==nb_simul_per_SA])
  
  
  # Calculate the lowest SA value with at least nb_simul_per_SA simuls
  if (minimum_SA == "select") {
    #####val_SA_available_MIN<-min(dataset_sim$SA_Gen[dataset_sim$index_SA_Gen==nb_simul_per_SA])
    val_rs_available_MIN<-min(dataset_sim$binnedrsqgen[dataset_sim$index_SA_Gen==nb_simul_per_SA])
    
  }else{
    if (minimum_SA == "0") {
      #####val_SA_available_MIN <- 0
      val_rs_available_MIN <- 0
      
    }else{
      print("Error: minimum_SA unknown")  
      }
    }
  
  # Select simuls with the SA value with at least nb_simul_per_SA simuls
  #####select_dataset_sim<-dataset_sim[dataset_sim$SA_Gen>=val_SA_available_MIN&
  #####dataset_sim$SA_Gen<=val_SA_available_MAX,]
  select_dataset_sim<-dataset_sim[dataset_sim$binnedrsqgen>=val_rs_available_MIN&
                                    dataset_sim$binnedrsqgen<=val_rs_available_MAX,]
  
  
  # Keep only the same number of simul (=nb_simul_per_SA) per SA value
  select_dataset_sim<-select_dataset_sim[select_dataset_sim$index_SA_Gen<=nb_simul_per_SA,]
  

  }else{
    if (type_SA == "non-genetic") {
  
      # Calculate the highest SA value with at least  nb_simul_per_SA simuls
      #####val_SA_available_MAX<-max(dataset_sim$SA_NonGen[dataset_sim$index_SA_NonGen==nb_simul_per_SA])
      val_rs_available_MAX<-max(dataset_sim$binnedrsqng[dataset_sim$index_SA_NonGen==nb_simul_per_SA])
      
      
      # Calculate the lowest SA value with at least nb_simul_per_SA simuls
      if (minimum_SA == "select") {
        ##### val_SA_available_MIN<-min(dataset_sim$SA_NonGen[dataset_sim$index_SA_NonGen==nb_simul_per_SA])
        val_rs_available_MIN<-min(dataset_sim$binnedrsqng[dataset_sim$index_SA_NonGen==nb_simul_per_SA])
        
      }else{
        if (minimum_SA == "0") {
          #####val_SA_available_MIN <- 0
          val_rs_available_MIN <- 0
          
        }else{
          print("Error: minimum_SA unknown")  
        }
      }
  
      # Select simuls with the SA value with at least nb_simul_per_SA simuls
      #####select_dataset_sim<-dataset_sim[dataset_sim$SA_NonGen>=val_SA_available_MIN&
      #####                   dataset_sim$SA_NonGen<=val_SA_available_MAX,]
      
      select_dataset_sim<-dataset_sim[dataset_sim$binnedrsqng>=val_rs_available_MIN&
                                        dataset_sim$binnedrsqng<=val_rs_available_MAX,]
      
      
      # Keep only the same number of simul (=nb_simul_per_SA) per SA value
      select_dataset_sim<-select_dataset_sim[select_dataset_sim$index_SA_NonGen<=nb_simul_per_SA,]
      
    }else{
      print("Error: type_SA unknown")  
    }
    
  }
  return (select_dataset_sim)
}