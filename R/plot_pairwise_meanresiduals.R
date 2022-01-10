#' Plot pairwise residuals
#'
#' @description Create a bivariate plot with residuals from common garden experiment
#' @param dataset name of the dataset (tibble format)
#' @param formula formula of the model to use to compute the residuals
#' @param test_environment name of the column including the test environment
#' @param original_environment name of the column including the environment of origin of each population 
#' @param additional_factor name of the column of an additional factor whose level will be represented with different symbols on the final graph
#' @param grp_cols vector with the names of factors that should be used to compute the mean of the trait (first/last element: name of the column with the population and test environment information)
#' @param fruit1 fruit for y-axis
#' @param fruit2 fruit for x-axis
#' @param onlyfocalpop (logical) use only populations sampled from fruit1 or fruit2  (default=TRUE)
#' @param coltest_envlevels vector of color for the different levels of the factor "test_environment"
#' @param shape_additional_factor vector for the symbols used to represent the different levels of the additional factor
#' @param xaxis_labelprint label of the x axis
#' @param yaxis_labelprint label of the y axis
#' @param xlim scale for x-axis
#' @param ylim scale for y-axis
#' @param fixedxylim (logical) should the limits for the scale of x-axis and y-axis be computed a the range of the trait across all test_environments (default=FALSE)
#' @param bisector (logical) print a bisector line through the origin (default=FALSE)
#' @param errorbars print bars for standard errors
#' @param printcor print correlation between the two variables
#' @param subscript subscript used when printing the correlation on the final graph
#' 
#' @return plot
#' @export 
#'
#' @examples
#'plot_pairwise_meanresiduals(dataset = data_PERF_Rate, formula=modelformula, fruit1 = "Cherry", fruit2 = "Blackberry", grp_cols=c("City", "Generation", "Original_environment", "Test_environment") , test_environment="Test_environment", original_environment="Original_environment", additional_factor="Generation", coltest_envlevels = c("#301934","#BC3C6D", "#3FAA96"), fixedxylim = TRUE, bisector = TRUE)

plot_pairwise_meanresiduals <- function(dataset = data_PERF_Rate,
                                        formula="asin(sqrt(Rate)) ~ Test_environment + log(Nb_eggs)",
                                        #formula="log(Nb_adults+1) ~ Test_environment + City + Nb_eggs + I(Nb_eggs^2) + I(Nb_eggs^3)",
                                        test_environment="Test_environment",
                                        original_environment="Original_environment",
                                        additional_factor=NULL,
                                        grp_cols=c("Population", "Original_environment", "Test_environment"), 
                                        fruit1 = "Cherry",
                                        fruit2 = "Blackberry",
                                        onlyfocalpop=TRUE,
                                        coltest_envlevels = NULL, 
                                        #colenvlevels = c("#301934","#BC3C6D", "#3FAA96"),
                                        shape_additional_factor=NULL,
                                        xaxis_labelprint = "Offspring performance\nin sympatry",
                                        yaxis_labelprint = "Offspring performance\nin allopatry",
                                        xlim=NULL, ylim=NULL, fixedxylim=FALSE, bisector=TRUE, 
                                        errorbars=TRUE, printcor=TRUE, subscript = NA){
  
  if(test_environment!=grp_cols[length(grp_cols)]){
    print("Error the last element of grp_cols should be equal to test_environment")
  }
  
  if(!is.null(additional_factor)){
    if(!additional_factor%in%grp_cols){
      print(paste0("Error grp_cols does not include the additional factor:", "additional_factor"))
    }
  }
  
  ## Transform test_environment into factor
  print("Converting test_environment column into a factor")
  dataset[[test_environment]] <- as.factor(dataset[[test_environment]])
  envlevels <- levels(dataset[[test_environment]])
  
  ## Transform original_environment into factor
  if(!is.null(original_environment)){
    print("Converting original_environment column into a factor")
    dataset[[original_environment]] <- as.factor(dataset[[original_environment]])
    originalenvlevels <- levels(dataset[[ original_environment]])
    
    if(length(originalenvlevels)!=length(envlevels)){
      print("The number of levels of test_environment and original_environment are different")
    }
  }
  ## Transform additional_factor into factor
  if(!is.null(additional_factor)){
    print("Converting additional_factor column into a factor")
    dataset[[additional_factor]] <- as.factor(dataset[[additional_factor]])
    additional_factorlevels <- levels( dataset[, additional_factor])
    ## Modify the default shape parameter in ggplot
    colo <- c(16, 17, 15, 18:25, 0:14)
    shape_additional_factor <- c(shape_additional_factor, colo[!colo%in%shape_additional_factor])
  }else{
    shape_additional_factor <- c(16, 17, 15, 18:25, 0:14)
  }
  
  
  if(!is.null(coltest_envlevels)&length(coltest_envlevels)!=length(envlevels)){
    print("The length of coltest_envlevels and the number of levels of test_environment are different")
  }
  
  if(!is.null(coltest_envlevels)){
    colfruit1 <- coltest_envlevels[envlevels==fruit1]
    colfruit2 <- coltest_envlevels[envlevels==fruit2]
  }else{
    colfruit1 <- scales::hue_pal()(length(envlevels))[envlevels==fruit1]
    colfruit2 <- scales::hue_pal()(length(envlevels))[envlevels==fruit2]
    print("No color provided, using default color palette")
    scales::show_col(scales::hue_pal()(length(envlevels)))
    
  }
  
  ## Fit model
  m <- lm(formula, data=dataset)
  summary(m)
  
  ## Compute residuals
  dataset$res <- residuals(m)
  
  ## Compute the mean of residuals for each combination of population and test environment
  datameanres <- dataset %>%                                       
    dplyr::group_by(across(grp_cols)) %>%
    dplyr::summarise_at(vars(res), list(MeanResiduals = mean, SdResiduals = sd, SampSize=length))
  
  ## Compute standard error
  datameanres <- datameanres %>% 
    dplyr::mutate(SeResiduals = SdResiduals/sqrt(SampSize))
  
  ## Remove columns
  datameanres <- datameanres %>%
    dplyr::select(-SdResiduals)
  
  ## Compute range of values for MeanResiduals
  if(errorbars){
    rangexy <- c(min(datameanres$MeanResiduals-datameanres$SeResiduals, na.rm=TRUE), 
                 max(datameanres$MeanResiduals+datameanres$SeResiduals, na.rm=TRUE))
  }else{
    rangexy <- range(datameanres$MeanResiduals)
  }
  
  ## Display only populations sampled from fruit1 or fruit2
  if(onlyfocalpop&!is.null(original_environment)){
    print(paste0("Displaying only populations from ", fruit1, " and ", fruit2, "."))
    datameanres <-  dplyr::filter(datameanres, 
                                  !!as.symbol(original_environment)==fruit1 |!!as.symbol(original_environment)==fruit2)
  }
  
  ## Reformat from long to wide format
  datameanreswide <- datameanres %>% 
    tidyr::pivot_wider(names_from = all_of(test_environment), values_from = c("MeanResiduals","SeResiduals", "SampSize"))
  
  ## Keep two columns with fruit 1 and fruit 2
  #data_fruit1_fruit2 <- datameanres[,c(grp_cols[-length(grp_cols)], paste0("MeanResiduals_", c(fruit1, fruit2)))]
  #data_fruit1_fruit2$N <- rowMeans(datameanres[,paste0("SampSize_", c(fruit1, fruit2))])
  #names(data_fruit1_fruit2)[length(grp_cols):(length(grp_cols)+1)] <- c("fruit1", "fruit2")
  #xaxis_labelprint <- paste0("Residuals(number of adults emerged)\n from ", fruit1)
  #yaxis_labelprint <- paste0("Residuals(number of adults emerged)\n from ", fruit2)
  
  ## Keep two columns with allopatry and sympatry
  data_fruit1_fruit2 <- datameanreswide[, c(grp_cols[-length(grp_cols)], paste0("MeanResiduals_", c(fruit1, fruit2)), paste0("SeResiduals_", c(fruit1, fruit2)))]
  
  
  
  
  ## If sympatry equal fruit1 replace with fruit1, otherwise fruit2
  data_fruit1_fruit2 <- data_fruit1_fruit2 %>% 
    dplyr::mutate(sympatry = ifelse(!!as.symbol(original_environment)==fruit1, 
                                    !!as.symbol(paste0("MeanResiduals_", fruit1)), 
                                    !!as.symbol(paste0("MeanResiduals_", fruit2))))
  data_fruit1_fruit2 <- data_fruit1_fruit2 %>% 
    dplyr::mutate(se_sympatry = ifelse(!!as.symbol(original_environment)==fruit1, 
                                       !!as.symbol(paste0("SeResiduals_", fruit1)),
                                       !!as.symbol(paste0("SeResiduals_", fruit2))))
  
  ## If sympatry equal fruit1 replace with fruit2, otherwise fruit1
  data_fruit1_fruit2 <- data_fruit1_fruit2 %>% 
    dplyr::mutate(allopatry = ifelse(!!as.symbol(original_environment)==fruit1, 
                                     !!as.symbol(paste0("MeanResiduals_", fruit2)),
                                     !!as.symbol(paste0("MeanResiduals_", fruit1))))
  
  data_fruit1_fruit2 <- data_fruit1_fruit2 %>% 
    dplyr::mutate(se_allopatry = ifelse(!!as.symbol(original_environment)==fruit1, 
                                        !!as.symbol(paste0("SeResiduals_", fruit2)),
                                        !!as.symbol(paste0("SeResiduals_", fruit1))))
  

  
  ## Compute average sample size
  data_fruit1_fruit2$N <- rowMeans(datameanreswide[, paste0("SampSize_", c(fruit1, fruit2))])
  ## Compute SA
  data_fruit1_fruit2 <- data_fruit1_fruit2 %>% 
    dplyr::mutate(SA = sympatry-allopatry)
  
  ## Remove columns
  data_fruit1_fruit2 <- data_fruit1_fruit2 %>%
    dplyr::select(-!!as.symbol(paste0("MeanResiduals_", fruit1)), -!!as.symbol(paste0("SeResiduals_", fruit1)), -!!as.symbol(paste0("MeanResiduals_", fruit2)), -!!as.symbol(paste0("SeResiduals_", fruit2)))
  
  weighted_var = function (x, w) sum(w * (x - weighted.mean(x, w, na.rm = TRUE)) ^ 2, na.rm = TRUE) / sum(w, na.rm = TRUE)
  weighted_sd = function (x, w) sqrt(weighted_var(x, w))
  
  SAstats <- data_fruit1_fruit2%>%
    #            dplyr::group_by(!!as.symbol(original_environment)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(SA_mean = weighted.mean(SA, N, na.rm = TRUE), SA_sd = weighted_sd(SA, N), Npop = length(N))
  SAstats <- SAstats %>% 
    dplyr::mutate(SA_se = SA_sd/sqrt(Npop))
  
  ## Check whether legend should be drawn
  if(length(grp_cols)>2){
    ## Check whether shape of different additional_factor should be drawn
    if(is.null(additional_factor)){
      
      plot_pair <- ggplot(data = data_fruit1_fruit2,
                          aes_q(x = as.name(names(data_fruit1_fruit2)[length(grp_cols)]), 
                                y = as.name(names(data_fruit1_fruit2)[length(grp_cols)+2]), 
                                color = as.name(names(data_fruit1_fruit2)[length(grp_cols)-1]))) +
        scale_color_manual(name="Fly population from:",   
                           breaks=originalenvlevels,
                           labels=originalenvlevels,
                           values=coltest_envlevels,
                           drop=FALSE)
      
    }else{
      plot_pair <- ggplot(data = data_fruit1_fruit2,
                          aes_q(x = as.name(names(data_fruit1_fruit2)[length(grp_cols)]), 
                                y = as.name(names(data_fruit1_fruit2)[length(grp_cols)+2]), 
                                color = as.name(names(data_fruit1_fruit2)[length(grp_cols)-1]), 
                                shape = as.name(names(data_fruit1_fruit2)[length(grp_cols)-2]))) +
        scale_color_manual(name="Fly population from:",   
                           breaks=originalenvlevels,
                           labels=originalenvlevels,
                           values=coltest_envlevels,
                           drop=FALSE) +
        scale_shape_manual(labels = additional_factorlevels, values=shape_additional_factor)
    }
    
  }else{
    
    if(is.null(additional_factor)){
      plot_pair <- ggplot(data = data_fruit1_fruit2,
                          aes_q(x = as.name(names(data_fruit1_fruit2)[length(grp_cols)]), 
                                y = as.name(names(data_fruit1_fruit2)[length(grp_cols)+2])))
    }else{
      plot_pair <- ggplot(data = data_fruit1_fruit2,
                          aes_q(x = as.name(names(data_fruit1_fruit2)[length(grp_cols)]), 
                                y = as.name(names(data_fruit1_fruit2)[length(grp_cols)+2])), 
                          shape = as.name(names(data_fruit1_fruit2)[length(grp_cols)-1])) +
        scale_shape_manual(labels = additional_factorlevels)
    }
  }
  if(bisector){
    plot_pair <- plot_pair + geom_abline(intercept = 0, slope=1, linetype ="solid", color = "grey")
  }
  
  plot_pair <- plot_pair +
    geom_point(size=3, stroke=1.3) + 
    geom_vline(xintercept = 0, linetype ="dashed", color = "grey")+
    geom_hline(yintercept = 0, linetype ="dashed", color = "grey") +
    #guides(fill = FALSE) +
    xlab(xaxis_labelprint)  +
    ylab(yaxis_labelprint)  +
    geom_point(size=3, stroke=1.3) +
    ggtitle(paste(fruit1, "vs.", fruit2)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  if(errorbars){
    plot_pair <- plot_pair +
      geom_errorbar(data = data_fruit1_fruit2, aes(ymin=allopatry-se_allopatry, ymax=allopatry+se_allopatry)) +
      geom_errorbarh(data = data_fruit1_fruit2, aes(xmin=sympatry-se_sympatry, xmax=sympatry+se_sympatry))
  }
  #theme(panel.grid.major.y = element_blank(),
  #      panel.grid.minor.y = element_blank(),
  #      axis.title.x = element_text(colour = colfruit1),
  #      axis.title.y = element_text(colour = colfruit2))
  
  if(fixedxylim){
    plot_pair <- plot_pair +
      xlim(rangexy[1],rangexy[2]) +
      ylim(rangexy[1], rangexy[2])
    
  } else{
    plot_pair <- plot_pair +
      xlim(min(layer_scales(plot_pair)$x$range$range, 
               layer_scales(plot_pair)$y$range$range),
           max(layer_scales(plot_pair)$x$range$range, 
               layer_scales(plot_pair)$y$range$range)) +
      ylim(min(layer_scales(plot_pair)$x$range$range, 
               layer_scales(plot_pair)$y$range$range), 
           max(layer_scales(plot_pair)$x$range$range, 
               layer_scales(plot_pair)$y$range$range))
  }
  if(!is.null(xlim)&!is.null(ylim)){
    plot_pair <- plot_pair +
      xlim(min(xlim[1], ylim[1]), max(xlim[2], ylim[2])) +
      ylim(min(xlim[1], ylim[1]), max(xlim[2], ylim[2]))
  }
  
  if(printcor){
    ## Compute weighted correlation
    weightedcor <- sjstats:::weighted_correlation(data_fruit1_fruit2,
                                                  x = allopatry, 
                                                  y = sympatry, 
                                                  weights = N, 
                                                  ci.lvl = 0.95)
    #weightedcor <- sjstats:::weighted_correlation(data_fruit1_fruit2,
    #                                              x = fruit1, 
    #                                              y = fruit2, 
    #                                              weights = N, 
    #                                              ci.lvl = 0.95)
    rho <- as.numeric(weightedcor$estimate[1])
    eq_rho <- as.character(as.expression(substitute(~~italic(rho)[subscript]~"="~weightedcor~"["~inf~";"~sup~"]",
                                                    list(subscript=subscript,
                                                         weightedcor = format(rho, digits = 2, nsmall=2), 
                                                         inf = format(weightedcor$ci[1], digits = 2),
                                                         sup = format(weightedcor$ci[2], digits = 2)))))
    eq_SA <- as.character(as.expression(substitute(~~italic(SA)[subscript]~"="~SAmean~"(+/-se="~se~")",
                                                   list(subscript = subscript,
                                                        SAmean = format(SAstats$SA_mean, digits = 2, nsmall=2), 
                                                        se = format(SAstats$SA_se, digits = 2)))))
    
    ## Position of the equation depends on the range of values of x and y
    if(fixedxylim){
      plot_pair <- plot_pair + geom_text(x = min(rangexy)+0.2*(max(rangexy)-min(rangexy)), y = max(rangexy), 
                                         label = eq_rho,
                                         parse = TRUE, 
                                         color="black", size = 3.5) +
        geom_text(x =  min(rangexy)+0.275*(max(rangexy)-min(rangexy)), y =  min(rangexy)+0.925*(max(rangexy)-min(rangexy)), 
                  label = eq_SA,
                  parse = TRUE, 
                  color="black", size = 3.5)
      
    }else{
      rangex <- layer_scales(plot_pair)$x$range$range
      rangey <- layer_scales(plot_pair)$y$range$range
      plot_pair <- plot_pair + geom_text(x = min(rangex)+0.2*(max(rangex)-min(rangex)), y = max(rangey), 
                                         label = eq_rho,
                                         parse = TRUE, 
                                         color="black", size = 3.5) +
        geom_text(x =  min(rangex)+0.275*(max(rangex)-min(rangex)), y =  min(rangey)+0.925*(max(rangey)-min(rangey)), 
                  label = eq_SA,
                  parse = TRUE, 
                  color="black", size = 3.5)
    }
    if(!is.null(xlim)&!is.null(ylim)){
      plot_pair <- plot_pair + geom_text(x =  min(xlim)+0.2*(max(xlim)-min(xlim)), y = max(ylim), 
                                         label = eq_rho,
                                         parse = TRUE, 
                                         color="black", size = 3.5) +
        geom_text(x =  min(xlim)+0.275*(max(xlim)-min(xlim)), y =  min(ylim)+0.925*(max(ylim)-min(ylim)), 
                  label = eq_SA,
                  parse = TRUE, 
                  color="black", size = 3.5)
    }
    
  }
  return(plot_pair)
}