#' Add indic in dataset of simulated fitness 
#'
#' @description Add indic if each simul is significant for genetic and non genetic test 
#' @param dataset_sim dataset with simulated data from the simul_fitnessdata_normal()

#' @return the same dataset with new columns
#' @export 
#'
#' @examples simul_fitnessdata_normal(dataset_sim = sim)


add_indic_sign <- function(dataset_sim = sim) { 

  dataset_sim$SA_Gen <- round(dataset_sim$SA_Gen_True,1)
  dataset_sim$SA_NonGen <- round(dataset_sim$SA_NonGen_True,1)
  dataset_sim$IndicGen <- ifelse(dataset_sim$pvalue_Gen<0.05 & dataset_sim$SAGen_Est>0, 1, 0)
  dataset_sim$IndicNonGen <- ifelse(dataset_sim$pvalue_NonGen<0.05 & dataset_sim$SANonGen_Est>0, 1, 0)
  dataset_sim$IndicGen_aov <- ifelse(dataset_sim$pvalue_Gen_aov<0.05 & dataset_sim$SAGen_Est>0, 1, 0)
  dataset_sim$IndicNonGen_aov <- ifelse(dataset_sim$pvalue_NonGen_aov<0.05 & dataset_sim$SANonGen_Est>0, 1, 0)
  
  return(dataset_sim)
}