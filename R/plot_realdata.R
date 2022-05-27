#' Plot pairwise residuals pop effect
#'
#' @description Create a bivariate plot with residuals from common garden experiment 
#' @param dataset name of the dataset (tibble format)
#' @param formula formula of the model to use to compute the residuals
#' @param formula_Blanquart formula of the model to estimate Blanquart Local Adaptation 
#' @param gen can be G0 or G2
#' @param test_environment name of the column including the test environment
#' @param original_environment name of the column including the environment of origin of each population 
#' @param grp_cols vector with the names of factors that should be used to compute the mean of the trait (first/last element: name of the column with the population and test environment information)
#' @param fruit1 fruit for y-axis
#' @param fruit2 fruit for x-axis
#' @param onlyfocalpop (logical) use only populations sampled from fruit1 or fruit2  (default=TRUE)
#' @param coltest_envlevels vector of color for the different levels of the factor "test_environment"
#' @param xaxis_labelprint label of the x axis
#' @param yaxis_labelprint label of the y axis
#' @param xlim scale for x-axis
#' @param ylim scale for y-axis
#' @param fixedxylim (logical) should the limits for the scale of x-axis and y-axis be computed a the range of the trait across all test_environments (default=FALSE)
#' @param printcor print correlation between the two variables
#' @param printSA print SA 
#' @param subscript subscript used when printing the correlation on the final graph
#' @param test_Blanquart print the results of the Blanquart's SA test
#' 
#' @importFrom dplyr vars across
#'
#' 
#' @return plot
#' @export 
#'
#' @examples
#'plot_realdata(dataset = data_PERF_Rate, formula=modelformula, fruit1 = "Cherry", fruit2 = "Blackberry", 
#'grp_cols=c("City", "Generation", "Original_environment", "Test_environment") , test_environment="Test_environment", original_environment="Original_environment", additional_factor="Generation", coltest_envlevels = c("#301934","#BC3C6D", "#3FAA96"), fixedxylim = TRUE)

plot_realdata <- function(dataset = data_PERF_Rate,
                                                  trait="Nb_eggs",
                                                  formula_Blanquart="asin(sqrt(Rate)) ~  Test_environment + Population + SA + log(Nb_eggs) + Test_environment:Original_environment",
                                                  test_environment="Test_environment",
                                                  original_environment="Original_environment",
                                                  gen = "G2",
                                                  subscript = NA,
                                                  grp_cols=c("Population", "Original_environment", "Test_environment"), 
                                                  coltest_envlevels = c("#301934","#BC3C6D", "#3FAA96"),
                                                  xaxis_labelprint = "Offspring performance\nin sympatry",
                                                  yaxis_labelprint = "Offspring performance\nin allopatry",
                                                  xlim=NULL, ylim=NULL, fixedxylim=TRUE,  
                                                  printcor=FALSE, printSA=FALSE, test_Blanquart=TRUE){
  
  # Subset dataset per generation 
  dataset <- dataset[dataset$Generation == gen,]
  dataset <- dataset[complete.cases(dataset[,trait]), ]
  dataset$y <- dataset[,trait]
  
  
  if(test_environment!=grp_cols[length(grp_cols)]){
    print("Error the last element of grp_cols should be equal to test_environment")
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
  
  if(!is.null(coltest_envlevels)&length(coltest_envlevels)!=length(envlevels)){
    print("The length of coltest_envlevels and the number of levels of test_environment are different")
  }
  
  
  
  ## Compute the mean of residuals for each combination of population and test environment
  datameany <- dataset %>%                                       
    dplyr::group_by(across(grp_cols)) %>%
    dplyr::summarise_at(vars(y), list(MeanTrait = mean, SdTrait = sd, SampSize=length))
  
  ## Compute standard error
  datameany <- datameany %>% 
    dplyr::mutate(SeTrait = SdTrait/sqrt(SampSize))
  
  ## Remove columns
  datameany <- datameany %>%
    dplyr::select(-SdTrait)
  
  ## Compute range of values for MeanResiduals
  rangexy <- c(min(datameany$MeanTrait-datameany$SeTrait, na.rm=TRUE), 
               max(datameany$MeanTrait+datameany$SeTrait, na.rm=TRUE))
  
  
  ## Transform in dataframe
  datameany<-as.data.frame(datameany)
  
  
  #Transform dataset: symp and allop columns
  datameany$SA <- ifelse(datameany$Test_environment==datameany$Original_environment,1,0)
  
  #Remove populations without SA value
  datameany <- droplevels(datameany)
  populations_withsymp <- unique(datameany$Population[datameany$SA=="1"])
  populations_withsymp <- droplevels(populations_withsymp)
  pop_toremove <- setdiff(levels(datameany$Population), populations_withsymp)
  
  if(!identical(pop_toremove, character(0))){
    print("Populations that do not have measures in sympatry have been removed")
    datameany<-datameany[datameany$Population!=pop_toremove,]
    datameany <- droplevels(datameany)
  }
  
  
  #Create new columns for symp
  datameany$Mean_Symp <- NA
  datameany$Se_Symp <-  NA
  datameany$N_Symp<-  NA
  
  for (i in levels(datameany$Population[datameany$SA==1])) {
    meansymp <- datameany$MeanTrait[datameany$SA==1&datameany$Population==i]
    se_symp <- datameany$SeTrait[datameany$SA==1&datameany$Population==i]
    N_symp <- datameany$SampSize[datameany$SA==1&datameany$Population==i]
    datameany$Mean_Symp[datameany$Population==i] <- meansymp
    datameany$Se_Symp[datameany$Population==i] <-  se_symp
    datameany$N_Symp[datameany$Population==i] <-  N_symp
    rm(meansymp,se_symp,N_symp)
  }
  
  
  datameany$Mean_Allop <- datameany$MeanTrait
  datameany$Se_Allop <-  datameany$SeTrait
  datameany$N_Allop <-  datameany$SampSize
  
  datameany$Symp <-  datameany$Original_environment
  datameany$Allop <-  datameany$Test_environment
  
  #Remove useless row and columns
  datameany <- datameany[datameany$SA!=1,]
  datameany <- datameany[,-c(2:7)]
  
  datameany$N <- (datameany$N_Symp+datameany$N_Allop)/2
  
 
  
  ######### Blanquart test 
  ## Fit model
  lm_val <- lm(formula_Blanquart, data=dataset)
  Fratio = anova(lm_val)["SA",3]/anova(lm_val)["Test_environment:Original_environment",3]
  pvalue = 1 - pf(Fratio,anova(lm_val)["SA",1],anova(lm_val)["Test_environment:Original_environment",1])
  df1 = anova(lm_val)["SA",1]
  df2 = anova(lm_val)["Test_environment:Original_environment",1]
  
  
  #Equation
  equation_Blanquart <- as.character(as.expression(substitute(italic(F)[df1~","~df2]~"="~Fratio~";"~italic(P)~"="~pvalue,
                                                              list(Fratio = format(Fratio, digits = 2, nsmall=2),
                                                                   df1 = format(df1, digits = 2), 
                                                                   df2 = format(df2, digits = 2), 
                                                                   pvalue = format(pvalue, digits = 2)))))
  
  
  
  if(!is.null(xlim)&!is.null(ylim)){
    #Ylim 
    max_plot <- max(ylim,xlim) 
    min_plot <-  min(ylim,xlim) 
  }else{
    #Ylim 
    max_plot <- max(max(datameany$Mean_Allop+datameany$Se_Allop, na.rm = TRUE), 
                    max(datameany$Mean_Symp+datameany$Se_Symp, na.rm = TRUE)) 
    min_plot <-  min(min(datameany$Mean_Allop-datameany$Se_Allop, na.rm = TRUE), 
                     min(datameany$Mean_Symp-datameany$Se_Symp, na.rm = TRUE))  
  }
  
  
  ######### PLOT 
  plot_pair <- ggplot(data = datameany,
                      aes(x = Mean_Symp, 
                          y = Mean_Allop), 
                      color = Symp, 
                      shape = Allop) +
    geom_abline(intercept = 0, slope=1, linetype ="dashed", color = "grey") +
    geom_errorbar(data = datameany, aes(ymin=Mean_Allop-Se_Allop, ymax=Mean_Allop+Se_Allop, col=Symp), size=0.3) +
    geom_errorbarh(data = datameany, aes(xmin=Mean_Symp-Se_Symp, xmax=Mean_Symp+Se_Symp, col=Symp), size=0.3) + 
    geom_point(aes(shape = Allop, col=Symp, fill=Symp),  size=3, alpha=1)  + 
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
    scale_shape_manual(name = "Test environment:",
                       labels = c("Blackberry","Cherry","Strawberry"), 
                       values =  c(15,16,17)) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_LO_sober + theme (
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank())
  
  if(test_Blanquart){
    plot_pair <- plot_pair + annotate('text', x = min_plot, y = max_plot,
                                      label = equation_Blanquart, parse = TRUE, hjust = 0, size = 4)
  }
  
  

  
  
  ## Change range limit
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
  
  
  ## Add correlation
  if(printcor){
    ## Compute weighted correlation
    weightedcor <- sjstats:::weighted_correlation(data_fruit1_fruit2,
                                                  x = allopatry, 
                                                  y = sympatry, 
                                                  weights = N, 
                                                  ci.lvl = 0.95)
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
                                         color="black", size = 3.5) 
      if(printSA) { plot_pair <- plot_pair + geom_text(x =  min(rangexy)+0.275*(max(rangexy)-min(rangexy)), 
                                                       y =  min(rangexy)+0.925*(max(rangexy)-min(rangexy)), 
                                                       label = eq_SA,
                                                       parse = TRUE, 
                                                       color="black", size = 3.5) }
      
    }else{
      rangex <- layer_scales(plot_pair)$x$range$range
      rangey <- layer_scales(plot_pair)$y$range$range
      plot_pair <- plot_pair + geom_text(x = min(rangex)+0.2*(max(rangex)-min(rangex)), y = max(rangey), 
                                         label = eq_rho,
                                         parse = TRUE, 
                                         color="black", size = 3.5) +
        if(printSA) {  geom_text(x =  min(rangex)+0.275*(max(rangex)-min(rangex)), y =  min(rangey)+0.925*(max(rangey)-min(rangey)), 
                                 label = eq_SA,
                                 parse = TRUE, 
                                 color="black", size = 3.5) }
    }
    if(!is.null(xlim)&!is.null(ylim)){
      plot_pair <- plot_pair + geom_text(x =  min(xlim)+0.2*(max(xlim)-min(xlim)), y = max(ylim), 
                                         label = eq_rho,
                                         parse = TRUE, 
                                         color="black", size = 3.5) +
        if(printSA) {   geom_text(x =  min(xlim)+0.275*(max(xlim)-min(xlim)), 
                                  y =  min(ylim)+0.925*(max(ylim)-min(ylim)), 
                                  label = eq_SA,
                                  parse = TRUE, 
                                  color="black", size = 3.5) }
    }
    
  }
  return(plot_pair)
}

