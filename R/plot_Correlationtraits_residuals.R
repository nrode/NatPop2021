#' Plot for relationship between rate and oviposition 
#'
#' @description Create plot for relation between two traits
#' @param gen the generation of dataset but can be: "G0", "G2" or  "Both"
#' @param fruit could be "Cherry" "Strawberry" "Blackberry" or "All"
#' @param dataset1 name of the dataset (tibble format) for the trait 1 = x-axis
#' @param dataset2 name of the dataset (tibble format) for the trait 2 = y-axis
#' @param formula1 formula of the model to use to compute the residuals of the trait 1 = x-axis
#' @param formula2 formula of the model to use to compute the residuals of the trait 2 = y-axis
#' @param axis_label_trait1 label of the x axis
#' @param axis_label_trait2 label of the y axis
#' @param printcor print correlation between the two variables
#' @param subscript subscript used when printing the correlation on the final graph
#'  
#' 
#' @return plot of the residuals of the trait on the three environement
#' @export 
#'
#' @examples
#'plot_CorrelationTraits_residuals(gen = "G2", fruit = "All", dataset1 = data_PERF_Rate, dataset2 = data_PREF_three, formula1 = "asin(sqrt(Rate)) ~ Test_environment + log(Nb_eggs)", formula2 = "log(Nb_eggs+1) ~ Test_environment + BoxID", axis_label_trait1 = "Offspring performance in XX", axis_label_trait2 = "Oviposition preference in XXX", subscript_label = "preference/performance", printcor=TRUE)

plot_CorrelationTraits_residuals <- function(gen = "G2", fruit = "All",
                                          dataset1 = data_PERF_Rate,
                                          dataset2 = data_PREF_three,
                                          formula1 = "asin(sqrt(Rate)) ~ Test_environment + Population + log(Nb_eggs)",
                                          formula2 = "log(Nb_eggs+1) ~ Test_environment+ Population + BoxID",
                                          axis_label_trait1 = "Offspring performance in XX",
                                          axis_label_trait2 = "Oviposition preference in XX",
                                          subscript_label = "preference/performance", 
                                          printcor=TRUE){
  
  #Load and subset dataset
  if (gen == "G0" | gen == "G2") {
    dataset1 <- dataset1[dataset1$Generation == gen,]
    dataset2 <- dataset2[dataset2$Generation == gen,]
  }else{
    if (gen == "Both") {
      dataset1 <- dataset1
      dataset2 <- dataset2
    }else {
      print("Error: unknown generation")
    }
  }
  
  ## Fit model
  m1 <- lm(formula1, data=dataset1)
  m2 <- lm(formula2, data=dataset2)
  
  ## Compute residuals
  dataset1$res <- residuals(m1)
  dataset2$res <- residuals(m2)
  
  ## Compute the mean of residuals for each combination of population and test environment
  groupcols <- c("Generation", "Population", "Original_environment", "Test_environment")
  datameanres1 <- dataset1 %>%                                       
    dplyr::group_by(across(groupcols)) %>%
    dplyr::summarise_at(vars(res), list(MeanResiduals1 = mean, SdResiduals = sd, SampSize1=length))
  datameanres2 <- dataset2 %>%                                       
    dplyr::group_by(across(groupcols)) %>%
    dplyr::summarise_at(vars(res), list(MeanResiduals2 = mean, SdResiduals = sd, SampSize2=length))

  ## Compute standard error
  datameanres1 <- datameanres1 %>% 
    dplyr::mutate(SeResiduals1 = SdResiduals/sqrt(SampSize1))
  datameanres2 <- datameanres2 %>% 
    dplyr::mutate(SeResiduals2 = SdResiduals/sqrt(SampSize2))
  
  ## Remove columns
  datameanres1 <- datameanres1 %>%
    dplyr::select(-SdResiduals)
  datameanres2 <- datameanres2 %>%
    dplyr::select(-SdResiduals)
  

  ## Merge in dataframe
  TEMP_SUM <- merge(as.data.frame(datameanres1), as.data.frame(datameanres2), 
                    by = groupcols)
  TEMP_SUM$N <- (TEMP_SUM$SampSize1+TEMP_SUM$SampSize2)/2
  TEMP_SUM <- TEMP_SUM[,-c(6,9)]
  
  #Subset per fruit
  if(fruit != "All"){
    TEMP_SUM <- TEMP_SUM[TEMP_SUM$Test_environment==fruit,]
  }
  
  #Color of axis: 
  if(fruit == "Cherry"){
    col <- "#BC3C6D"
  }else{
    if(fruit == "Strawberry"){
      col <- "#3FAA96"
    }else{
      if(fruit == "Blackberry"){
        col <- "#301934"
      }else{
        col <- "black"
      }
    }
  }
  

  
  ######### PLOT 
  plot_pair <- ggplot(data = TEMP_SUM,
                      aes(x = MeanResiduals1, 
                          y = MeanResiduals2)) +
    #geom_abline(intercept = 0, slope=1, linetype ="dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_errorbar(data = TEMP_SUM, aes(ymin=MeanResiduals2-SeResiduals2, 
                                       ymax=MeanResiduals2+SeResiduals2, col=Original_environment), size=0.2, alpha=0.7) +
    geom_errorbarh(data = TEMP_SUM, aes(xmin=MeanResiduals1-SeResiduals1,
                                        xmax=MeanResiduals1+SeResiduals1, col=Original_environment), size=0.2, alpha=0.7) + 
    geom_point(aes(color = Original_environment, shape = Test_environment,
                   fill = Original_environment),  size=3, alpha=0.85)  + 
    xlab(axis_label_trait1)  +
    ylab(axis_label_trait2) +
    scale_fill_manual(name="Fly population from:",   
                      breaks=c("Blackberry","Cherry","Strawberry"),
                      labels=c("Blackberry","Cherry","Strawberry"),
                      values=c("#301934","#BC3C6D", "#3FAA96"),
                      drop=FALSE) +
    scale_color_manual(name="Fly population from:",   
                       breaks=c("Blackberry","Cherry","Strawberry"),
                       labels=c("Blackberry","Cherry","Strawberry"),
                       values=c("#301934","#BC3C6D", "#3FAA96"),
                       drop=FALSE) + 
    scale_shape_manual(name = "Test fruit:",
                       labels = c("Blackberry","Cherry","Strawberry"), 
                       values =  c(15,16,17)) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_LO_sober + theme (
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_text(colour = col),
      axis.title.y = element_text(colour = col))
  
  #plot_pair
  
  
  ######### CORRELATION 
  if (gen == "G0" | gen == "G2") {
    weightedcor <- sjstats:::weighted_correlation(TEMP_SUM,
                                                  x = MeanResiduals1, 
                                                  y = MeanResiduals2, 
                                                  weights = N, 
                                                  ci.lvl = 0.95)
    rho <- as.numeric(weightedcor$estimate[1])
    eq_rho <- as.character(as.expression(substitute(~~italic(rho)[generation]~"="~weightedcor~"["~inf~";"~sup~"]",
                                                    list(generation = subscript_label,
                                                         weightedcor = format(rho, digits = 2, nsmall=2), 
                                                         inf = format(weightedcor$ci[1], digits = 2),
                                                         sup = format(weightedcor$ci[2], digits = 2)))))
    
    if(printcor) { plot_pair <- plot_pair + geom_text(x = -Inf, y = Inf, hjust = 0, vjust = 1, 
                                       label = eq_rho,
                                       parse = TRUE, 
                                       color="black", size = 3.5) }
    
    
  }else{
    if (gen == "Both") {
      weightedcor_G0 <- sjstats:::weighted_correlation(TEMP_SUM[TEMP_SUM$Generation=="G0",],
                                                    x = MeanResiduals1, 
                                                    y = MeanResiduals2, 
                                                    weights = N, 
                                                    ci.lvl = 0.95)
      rho_g0 <- as.numeric(weightedcor_G0$estimate[1])
      
      weightedcor_G2 <- sjstats:::weighted_correlation(TEMP_SUM[TEMP_SUM$Generation=="G2",],
                                                       x = MeanResiduals1, 
                                                       y = MeanResiduals2, 
                                                       weights = N, 
                                                       ci.lvl = 0.95)
      rho_g2 <- as.numeric(weightedcor_G2$estimate[1])
      eq_rho_G0 <- as.character(as.expression(substitute(~~italic(rho)[generation]~"="~weightedcorG0~"["~infg0~";"~supg0~"]",
                                                         list(generation = paste(subscript_label,"G0/G1"),
                                                              weightedcorG0 = format(rho_g0, digits = 2), 
                                                              infg0 = format(weightedcor_G0$ci[1], digits = 1),
                                                              supg0 = format(weightedcor_G0$ci[2], digits = 1)))))
      eq_rho_G2 <- as.character(as.expression(substitute(~~italic(rho)[generation]~"="~weightedcorG2~"["~infg2~";"~supg2~"]",
                                                         list(generation = paste(subscript_label,"G2/G3"),
                                                              weightedcorG2 = format(rho_g2, digits = 2), 
                                                              infg2 = format(weightedcor_G2$ci[1], digits = 1),
                                                              supg2 = format(weightedcor_G2$ci[2], digits = 1)))))
  
      
      if(printcor) { plot_pair <- plot_pair + geom_text(x = -Inf, y = Inf, hjust = 0, vjust = 1, 
                                                        label = eq_rho_G0,
                                                        parse = TRUE, 
                                                        color="black", size = 3.5)}
      
    }else {
      print("Error: unknown generation")
    }
  }
  
  
  return(plot_pair) 
}

