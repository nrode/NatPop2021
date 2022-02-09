#' Plot pairwise residuals pop effect
#'
#' @description Create a bivariate plot with residuals from common garden experiment 
#' @param dataset name of the dataset (tibble format)
#' @param formula formula of the model to use to compute the residuals
#' @param gen can be G0 or G2
#' @paran titleplot is the title of the plot
#' 
#' @importFrom dplyr vars across
#'
#' 
#' @return plot
#' @export 
#'
#' @examples
#'plot_acp(dataset = data_PERF_Rate, formula = modelformula, gen = "G2")

plot_acp <- function(dataset = data_PERF_Rate,
                     formula="asin(sqrt(Rate)) ~ Test_environment + log(Nb_eggs)",
                     gen = "G2", 
                     titleplot = "Offspring performance in G0"){
  
  # Subset dataset per generation 
  dataset <- dataset[dataset$Generation == gen,]
  
  ## Fit model
  m <- lm(formula, data=dataset)
  #summary(m)
  
  ## Compute residuals
  dataset$res <- residuals(m)
  
  ## Prepare dataset
  datameanbyEnv <- dataset %>%                         
    dplyr::group_by(Original_environment, Population, Test_environment) %>%
    dplyr::summarise_at(vars(res), list(MeanNb_adults = mean))  %>% 
    tidyr::pivot_wider(names_from=Test_environment, values_from=MeanNb_adults) %>% 
    dplyr::filter(!is.na(Cherry))

  #Remove NA
  datameanbyEnv <- datameanbyEnv[!is.na(datameanbyEnv$Blackberry),]
  datameanbyEnv <- datameanbyEnv[!is.na(datameanbyEnv$Cherry),]
  datameanbyEnv <- datameanbyEnv[!is.na(datameanbyEnv$Strawberry),]
  
  ## PCA
  pca.m0 <- ade4::dudi.pca(datameanbyEnv[3:5], scannf= F, scale=FALSE, nf=3) 
  #pca.m0
  
  ## First axis explains most of the variation
  Eig_plot <- factoextra::fviz_eig(pca.m0)
  
  Ind_plot <- factoextra::fviz_pca_ind(pca.m0,
                           repel = TRUE,
                           col.ind=datameanbyEnv$Original_environment) +
    scale_color_manual(values=c("#301934","#BC3C6D", "#3FAA96"))
  
  Var_plot <- factoextra::fviz_pca_var(pca.m0,
                           col.var = "contrib", 
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = TRUE)
  
  
  BiPlot <- factoextra::fviz_pca_biplot(pca.m0, label="var", 
                              habillage=datameanbyEnv$Original_environment, 
                              col.var = "grey30", 
                              repel = TRUE) +
    scale_color_manual(name="Fly population from:",   
                       breaks=c("Blackberry","Cherry","Strawberry"),
                       labels=c("Blackberry","Cherry","Strawberry"),
                       values=c("#301934","#BC3C6D", "#3FAA96"),
                       drop=FALSE)   +
    scale_shape_manual(name = "Fly population from:",
                     breaks=c("Blackberry","Cherry","Strawberry"),
                     labels = c("Blackberry","Cherry","Strawberry"), 
                     values =  c(15,16,17)) +
    scale_fill_manual(name="Fly population from:",   
                      breaks=c("Blackberry","Cherry","Strawberry"),
                      labels=c("Blackberry","Cherry","Strawberry"),
                      values=c("#301934","#BC3C6D", "#3FAA96"),
                      drop=FALSE) + 
    ggtitle(titleplot) +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  return(BiPlot)
}

