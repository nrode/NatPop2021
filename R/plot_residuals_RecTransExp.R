#' Plot for reciprocal transplant experiment
#'
#' @description Create plot for reciprocal transplant experiment
#' @param generation the generation of dataset
#' @param trait the trait 
#' 
#' @return plot of the residuals of the trait on the three environement
#' @export 
#'
#' @examples
#'plot_RTP_residuals(trait = "Number_eggs", Generation = , )


plot_RTP_residuals <- function(distrib = "normal", design = "balanced",
                              seed = 1, nfruit = 3, nhab = 3, npop_per_fruit = 5, nrep = 3, ntrial = 20,  
                              sdbox = NA, sdpop = 0.5, sdfruithab = 1, sdfruithab_ng = 1, 
                              rho = -1/(nhab-1), rho_ng = 0, sigma = 0.5, disp_design=0.0001){
  