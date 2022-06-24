#' Plot pairwise residuals pop effect
#'
#' @description Create a bivariate plot with residuals from common garden experiment 
#' @param dataset name of the dataset (tibble format)
#' @param gen can be G0 or G2
#' @param test_environment name of the column including the test environment
#' @param original_environment name of the column including the environment of origin of each population 
#' @param grp_cols vector with the names of factors that should be used to compute the mean of the trait (first/last element: name of the column with the population and test environment information)
#' @param coltest_envlevels vector of color for the different levels of the factor "test_environment"
#' @param yaxis_labelprint label of the y axis
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

plot_pop_homeaway <- function(dataset = data_PREF_three,
                                                  trait="Nb_eggs",
                                                  test_environment="Test_environment",
                                                  original_environment="Original_environment",
                                                  gen = "G2",
                                                  grp_cols=c("Population", "Original_environment", "Test_environment"), 
                                                  coltest_envlevels = c("#301934","#BC3C6D", "#3FAA96"),
                                                  yaxis_labelprint = "Offspring performance\nin allopatry"){
  
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
  datameany$Sd_Symp <-  NA
  datameany$N_Symp<-  NA
  
  for (i in levels(datameany$Population[datameany$SA==1])) {
    meansymp <- datameany$MeanTrait[datameany$SA==1&datameany$Population==i]
    sd_symp <- datameany$SdTrait[datameany$SA==1&datameany$Population==i]
    N_symp <- datameany$SampSize[datameany$SA==1&datameany$Population==i]
    datameany$Mean_Symp[datameany$Population==i] <- meansymp
    datameany$Sd_Symp[datameany$Population==i] <-  sd_symp
    datameany$N_Symp[datameany$Population==i] <-  N_symp
    rm(meansymp,sd_symp,N_symp)
  }
  
  
  datameany$Mean_Allop <- datameany$MeanTrait
  datameany$Sd_Allop <-  datameany$SdTrait
  datameany$N_Allop <-  datameany$SampSize
  
  datameany$Symp <-  datameany$Original_environment
  datameany$Allop <-  datameany$Test_environment
  
  #Remove useless row and columns
  datameany <- datameany[datameany$SA!=1,]
  datameany <- datameany[,-c(2:7)]
  
  datameany$N <- (datameany$N_Symp+datameany$N_Allop)/2
  
 #######Calcul difference between mean 
  #https://www.statology.org/confidence-interval-difference-between-means/
  #(x1–x2) +/- t*√((sp2/n1) + (sp2/n2))
  #sp2 = ((n1-1)s12 + (n2-1)s22) / (n1+n2-2)
  
  datameany$Mean_Dif <- datameany$Mean_Symp - datameany$Mean_Allop
  
  datameany$pool_var <- ((datameany$N_Symp-1)*(datameany$Sd_Symp^2) +
                           (datameany$N_Allop-1)*(datameany$Sd_Allop^2)) / (datameany$N_Symp+datameany$N_Allop-2)
  datameany$CI_int <- 2.05*(sqrt(((datameany$pool_var)/datameany$N_Symp) + 
                                ((datameany$pool_var)/datameany$N_Allop)))


  #Statistical test
  datameany$pval <- NA
  
  for (i in unique(datameany$Population)){
    for (j in unique(datameany$Allop[datameany$Population==i])){
      group1 <- dataset$y[dataset$Population==i&dataset$SA==1] 
      group2 <- dataset$y[dataset$Population==i&dataset$Test_environment==j]
      if(length(group1)>1&length(group2)>1){
        res <- t.test(group1,group2) 
        datameany$pval[datameany$Population==i&datameany$Allop==j] <- res$p.value
        rm(group1, group2, res)
      }else{
        datameany$pval[datameany$Population==i&datameany$Allop==j] <- "no-test"
        rm(group1, group2) 
      }
      
    }
  }
  
  #Corrrection multiple: Bonferroni 
  datameany$pval_corrected <- p.adjust(datameany$pval,method = "bonferroni")
  datameany$fill_point <- ifelse(datameany$pval_corrected<0.05, "*", "ns")
  datameany$fill_point <- ifelse(datameany$pval!= "no-test", datameany$fill_point,"ns")
  
  # Re-order levels of Line
  datameany$Population <- forcats::fct_reorder(datameany$Population, datameany$Mean_Symp, .desc = T)

  
 
  
  # #Plot: 
  pd <- ggplot2::position_dodge(0.3)
  
  nb_pop <- length(levels(datameany$Population))
  max_lim <- max(abs(max(datameany$Mean_Dif+datameany$CI_int, na.rm = TRUE)),
             abs(min(datameany$Mean_Dif-datameany$CI_int, na.rm = TRUE)))*1.1
  
  plot_pair <- ggplot(data = datameany,
                          aes(x = Population, y = Mean_Dif, group = Allop,
                              color = Symp, shape = Allop)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 0.75) +
    geom_errorbar(aes(ymin = Mean_Dif+CI_int,
                      ymax = Mean_Dif-CI_int),
                  width = 0.2, size = 0.5, alpha = 0.6,position = pd) +
    geom_point(fill="white", position = pd, stroke = 1.1, size =3) +
    #geom_point(aes(fill = interaction(fill_point,Symp)), position = pd, stroke = 1.1, size =3) +
    labs(shape = "Test fruit", color = "Original fruit") +
    guides(shape = guide_legend(override.aes = list(fill = c("black"))),
           fill = FALSE) +
    xlab("Populations")  +
    ylab(yaxis_labelprint) +
    # scale_fill_manual(name="Test stat",   
    #                    breaks=c("*.Blackberry","ns.Blackberry",
    #                             "*.Cherry","ns.Cherry",
    #                             "*.Strawberry","ns.Strawberry"),
    #                    labels=c("significant","non-significant","significant",
    #                             "non-significant","significant","non-significant"),
    #                    values=c("#301934","white",
    #                             "#BC3C6D","white",
    #                             "#3FAA96","white")) +
    scale_color_manual(name="Fly population from:",   
                       breaks=c("Blackberry","Cherry","Strawberry"),
                       labels=c("Blackberry","Cherry","Strawberry"),
                       values=c("#301934","#BC3C6D", "#3FAA96"),
                       drop=FALSE) + 
    scale_shape_manual(name = "Test fruit:",
                       labels = c("Blackberry","Cherry","Strawberry"), 
                       values =  c(21, 22, 24)) + 
    coord_cartesian(expand = FALSE, ylim = c(-max_lim, max_lim), xlim=c(0, nb_pop+3),clip = "off") + 
    theme_LO_sober + theme(axis.text.x  = element_blank()) +  
    annotate("text", x = nb_pop+2.5, y = 0.6*max_lim, label = 'bold("Higher on\n original")',
              size = 3,colour = "black", parse = TRUE, angle = 90) + 
    annotate("text", x = nb_pop+2.5, y = -0.6*max_lim, label = 'bold("Higher on\nalternative")',
               size = 3,colour = "black", parse = TRUE, angle = 90)  + 
    geom_segment(x = nb_pop+0.5, y = 0.4*max_lim, xend = nb_pop+0.5, yend = 0.85*max_lim, size = 0.15, 
                 arrow = arrow(length = unit(0.05, "npc")), colour = "black") + 
    geom_segment(x = nb_pop+0.5, y = -0.4*max_lim, xend =  nb_pop+0.5, yend = -0.85*max_lim, size = 0.15,
                 arrow = arrow(length = unit(0.05, "npc")),colour = "black") 
  
    plot_pair
  return(plot_pair)
}

