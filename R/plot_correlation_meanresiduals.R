#' Plot the relationship between a trait measured using an environmental factor with two levels
#'
#' @description Create a bivariate plot showing the relationship between a trait measured using an environmental factor with two levels
#' @param dataset name of the dataset (tibble format)
#' @param formula formula of the model to use to compute the residuals
#' @param envfactor environmental factor with two levels
#' @param original_environment name of the column including the environment of origin of each population
#' @param additional_factor name of the column of an additional factor whose level will be represented with different colors or on different panel (facetwrap=TRUE) on the final graph
#' @param col_original_environment vector of color for the different levels of the factor "original_environment"
#' @param grp_cols vector with the names of factors that should be used to compute the mean of the trait (first/last element: name of the column with the population and test environment information)
#' @param printnames (logical) use the name of the first factor listed in grp_cols as labels (default=FALSE)
#' @param printsamplesize (logical) print the sample size (x, y) as labels (default=FALSE)
#' @param labelsize size of the labels if printed
#' @param facetwrap split additional factor into different panels
#' @param xlim scale for x-axis
#' @param ylim scale for y-axis
#' @param xaxis_labelprint label of the x axis
#' @param yaxis_labelprint label of the y axis
#' @param fixedxylim (logical) should the limits for the scale of x-axis and y-axis be computed a the range of the trait across all test_environments (default=FALSE)
#' 
plot_correlation_meanresiduals <- function(dataset = dataselect,
                                           formula="log(Nb_adults+1) ~",
                                           envfactor="Env",
                                           original_environment=NULL,
                                           additional_factor=NULL,
                                           col_original_environment=NULL,
                                           grp_cols=c("Pop", "Fruit", "Env"),
                                           facetwrap=FALSE,
                                           printnames=FALSE,
                                           printsamplesize=FALSE,
                                           labelsize=1,
                                           xaxis_labelprint = NULL,
                                           yaxis_labelprint =  NULL,
                                           xlim=NULL, ylim=NULL, fixedxylim = TRUE,  errorbars=TRUE){
  
  ## Transform test_environment into factor
  print("Converting envfactor column into a factor")
  dataset[[envfactor]] <- factor(dataset[[envfactor]])
  envlevels <- levels(dataset[[envfactor]])
  
  ## Transform original_environment into factor
  if(!is.null(original_environment)){
    print("Converting original_environment column into a factor")
    dataset[[original_environment]] <- as.factor(dataset[[original_environment]])
    originalenvlevels <- levels(dataset[[ original_environment]])
    
  }
  ## Transform additional_factor into factor
  if(!is.null(additional_factor)){
    print("Converting additional_factor column into a factor")
    dataset[[additional_factor]] <- factor(dataset[[additional_factor]])
    additional_factorlevels <- levels(dataset[[additional_factor]])
    
  }
  
  ## Check whether transofrmation of a column required
  if(grepl("~", as.character(formula)[1])){
    ## Fit model
    m <- lm(formula, data=dataset)
    summary(m)
    
    ## Compute residuals
    dataset$res <- residuals(m)
  }else{
    dataset <- dataset %>% 
      dplyr::mutate(res = !!as.symbol(formula))
  }
  
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
    rangexy <- c(min(datameanres$MeanResiduals-datameanres$SeResiduals, na.rm=TRUE), max(datameanres$MeanResiduals+datameanres$SeResiduals, na.rm=TRUE))
  }else{
    rangexy <- range(datameanres$MeanResiduals)
  }
  
  ## Reformat from long to wide format
  datameanreswide <- datameanres %>% 
    tidyr::pivot_wider(names_from = all_of(envfactor), values_from = c("MeanResiduals","SeResiduals", "SampSize"))
  colenvfactor <- paste0("MeanResiduals_", envlevels)
  Seenvfactor <- paste0("SeResiduals_", envlevels)
  SampSizeenvfactor <- paste0("SampSize_", envlevels)
  
  ## If no additional factor
  if(is.null(additional_factor)){
    ## If no original environment
    if(is.null(original_environment)){
      plot_pair <- ggplot(data = datameanreswide,
                          aes_q(x = as.name(colenvfactor[1]), 
                                y = as.name(colenvfactor[2])))

    }else{
      plot_pair <- ggplot(data = datameanreswide,
                          aes_q(x = as.name(colenvfactor[1]), 
                                y = as.name(colenvfactor[2]),
                                fill = as.name(original_environment)))  
    }
    
  }else{
    ## Check whether facet_wrap should be used
    if(facetwrap){
      if(is.null(original_environment)){
        plot_pair <- ggplot(data = datameanreswide,
                            aes_q(x = as.name(colenvfactor[1]), 
                                  y = as.name(colenvfactor[2]))) +
          facet_wrap(facets=additional_factor, scales = "free_y")
      }else{
        plot_pair <- ggplot(data = datameanreswide,
                            aes_q(x = as.name(colenvfactor[1]), 
                                  y = as.name(colenvfactor[2]), 
                                  color = as.name(original_environment))) +
          facet_wrap(facets=additional_factor, scales = "free_y") 

      }
      
      
    }else{
      if(is.null(original_environment)){
        plot_pair <- ggplot(data = datameanreswide,
                            aes_q(x = as.name(colenvfactor[1]), 
                                  y = as.name(colenvfactor[2]), 
                                  shape = as.name(additional_factor)))
        

      }else{
        plot_pair <- ggplot(data = datameanreswide,
                            aes_q(x = as.name(colenvfactor[1]), 
                                  y = as.name(colenvfactor[2]), 
                                  shape = as.name(additional_factor),
                                  color = as.name(original_environment))) 
      }
    }
  }
  
  

  
  
  ##Add error bars
  if(errorbars){
    plot_pair <- plot_pair  +
      geom_errorbarh(aes(xmin=datameanreswide[[colenvfactor[1]]]-datameanreswide[[Seenvfactor[1]]], xmax=datameanreswide[[colenvfactor[1]]]+datameanreswide[[Seenvfactor[1]]]), size=0.3) +
      geom_errorbar(data = datameanreswide, aes(ymin=datameanreswide[[colenvfactor[2]]]-datameanreswide[[Seenvfactor[2]]], ymax=datameanreswide[[colenvfactor[2]]]+datameanreswide[[Seenvfactor[2]]]), size=0.3)
  }
  
  ## Add data points
  plot_pair <- plot_pair +
    geom_abline(intercept = 0, slope=1, linetype ="dashed", color = "grey") +
    geom_point(size=3, stroke=1.3, col="white") + 
    geom_point(size=3, stroke=1.3, alpha = 0.9) + 
    theme(plot.title = element_text(hjust = 0.5)) +         
    xlab(xaxis_labelprint)  +
    ylab(yaxis_labelprint) +
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
      panel.grid.minor.y = element_blank()) + 
    guides(fill = guide_legend(override.aes = aes(color = NA, shape = NA)))
  
    guides(fill = guide_legend(override.aes = list(col = c("#301934","#BC3C6D", "#3FAA96"))), 
           color = guide_legend(override.aes = list(shape = c(NA,0,1)))) 
    
  #+
  #  guides(fill = guide_legend(fill = guide_legend(override.aes = list(color = c("#301934","#BC3C6D", "#3FAA96"))), color = FALSE))
  # guides(color = guide_legend(override.aes=list(shape = NA)))
  
  if(!is.null(xaxis_labelprint)){
    plot_pair <- plot_pair + xlab(xaxis_labelprint)
  }
  if(!is.null(yaxis_labelprint)){
    plot_pair <- plot_pair + ylab(yaxis_labelprint)
  }
  
  ## Add labels
  if(printnames){
    if(printsamplesize){
      plot_pair <- plot_pair +  ggrepel::geom_text_repel(aes(x=datameanreswide[[colenvfactor[1]]], 
                                                             y=datameanreswide[[colenvfactor[2]]], 
                                                             label=paste0(datameanreswide[[grp_cols[1]]], " (", datameanreswide[[SampSizeenvfactor[1]]], ",",datameanreswide[[SampSizeenvfactor[2]]], ")")),
                                                         force=1, nudge_x=0.3, nudge_y=0.3, size = labelsize)
    }else{
      plot_pair <- plot_pair +  ggrepel::geom_text_repel(aes(x=datameanreswide[[colenvfactor[1]]], 
                                                             y=datameanreswide[[colenvfactor[2]]], 
                                                             label=datameanreswide[[grp_cols[1]]]),
                                                         force=1, nudge_x=0.3, nudge_y=0.3, size = labelsize)
    }
    
  }
  

  
  ## Set x- and y-axes
  if(!is.null(xlim)&!is.null(ylim)){
    plot_pair <- plot_pair +
      xlim(xlim[1], xlim[2]) +
      ylim(ylim[1], ylim[2])
  }else{
    if(fixedxylim){
      plot_pair <- plot_pair +
        xlim(rangexy[1],rangexy[2]) +
        ylim(rangexy[1], rangexy[2])
      
    } else{
      plot_pair <- plot_pair +
        xlim(min(layer_scales(plot_pair)$x$range$range, layer_scales(plot_pair)$y$range$range), max(layer_scales(plot_pair)$x$range$range, layer_scales(plot_pair)$y$range$range)) +
        ylim(min(layer_scales(plot_pair)$x$range$range, layer_scales(plot_pair)$y$range$range), max(layer_scales(plot_pair)$x$range$range, layer_scales(plot_pair)$y$range$range))
    }
  }
}

