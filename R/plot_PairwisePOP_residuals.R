#' Plot for reciprocal transplant experiment
#'
#' @description Create plot for reciprocal transplant experiment
#' @param gen the generation of dataset but can be Both
#' @param trait the trait 
#' @param fruit1 fruit for y-axis
#' @param fruit2 fruit for x-axis
#' 
#' 
#' @return plot of the residuals of the trait on the three environement
#' @export 
#'
#' @examples
#'plot_PairwisePOP_residuals(dataset = data_PERF, trait = "Nb_eggs", gen = "Both",fruit1 = "Cherry",fruit2 = "Blackberry")


plot_PairwisePOP_residuals <- function(dataset = data_PERF_Rate, trait = "Rate", gen = "G2", 
                                       fruit1 = "Cherry", fruit2 = "Blackberry"){
  
  # Subset dataset
  if (gen == "G0" | gen == "G2") {
  data <- dataset[dataset$Generation == gen,]
  }else{
    if (gen == "Both") {
      data <- dataset
    }else {
      print("Error: unknown generation")
    }
  }
  data <- data[complete.cases(data[,trait]), ]
  
  
  # Transform variables
  if(trait == "Nb_eggs" | trait == "Nb_adults" ){
    data$y <- log(data[,trait]+1)
  }else{
    if(trait == "Rate"){
      data$y <- asin(sqrt(data[,trait]))
    }else{
      print("Error: unknown trait")
    }
  }
  
  #Extract resid 
  if (gen == "G0" | gen == "G2") {
    lm_resid <- lm(y ~ Test_environment + Population, data=data)
    data$Resid <- residuals(lm_resid)
  }else{
    if (gen == "Both") {
      lm_resid <- lm(y ~ hab_gen + pop_gen, data=data)
      data$Resid <- residuals(lm_resid)
    
      }else {
      print("Error: unknown generation")
    }
  }
  
  
  
  ##Dataset summary
  TEMP_SUM <- Rmisc::summarySE(data,
                               measurevar="Resid",
                               groupvars=c("Original_environment","Population", "Test_environment","SA", "Generation"))
  
  
  #Subset per pair of fruits
  TEMP_SUM_FRUIT <- TEMP_SUM[TEMP_SUM$Test_environment==fruit1|
                               TEMP_SUM$Test_environment==fruit2,]
  
  
  data_fruit1_fruit2<-data.table::dcast(data.table::setDT(TEMP_SUM_FRUIT), 
                                                   Population + Original_environment + Generation ~ Test_environment,
                                                   value.var  = c("Resid"))

  
  
  # Plot title and y axis title
  plot_title <- ifelse(gen == "G0", "First generation", ifelse(gen == "G2","Third generation", " "))
  
  if("Obs_A" %in% colnames(dataset)  & trait == "Nb_eggs"){
    yaxis_labelprint <- paste0("Residuals(oviposition stimulation)\n in ", fruit2)
  }else{
    if("Obs_A" %in% colnames(dataset)  & trait == "Nb_adults"){
      yaxis_labelprint <- paste0("Residuals(number of adults)\n in ", fruit2)
    }else{
      if("Rate" %in% colnames(dataset) && trait == "Rate"){
        yaxis_labelprint <- paste0("Residuals(emergence rate)\n in ", fruit2)
      }else{
        if("BoxID" %in% colnames(dataset) && trait == "Nb_eggs"){
          yaxis_labelprint <- paste0("Residuals(oviposition preference)\n in ", fruit2)
        }else{
          print("Error: unknown combinaison dataset x trait")
        }
      }
    }
  }
  
  # Plot title and x axis title
  if("Obs_A" %in% colnames(dataset)  & trait == "Nb_eggs"){
    xaxis_labelprint <- paste0("Residuals(oviposition stimulation)\n in ", fruit1)
  }else{
    if("Obs_A" %in% colnames(dataset)  & trait == "Nb_adults"){
      xaxis_labelprint <- paste0("Residuals(number of adults)\n in ", fruit1)
    }else{
      if("Rate" %in% colnames(dataset) && trait == "Rate"){
        xaxis_labelprint <- paste0("Residuals(emergence rate)\n in ", fruit1)
      }else{
        if("BoxID" %in% colnames(dataset) && trait == "Nb_eggs"){
          xaxis_labelprint <- paste0("Residuals(oviposition preference)\n in ", fruit1)
        }else{
          print("Error: unknown combinaison dataset x trait")
        }
      }
    }
  }

  
  if(colnames(data_fruit1_fruit2)[4] == fruit1){
    data_fruit1_fruit2$fruit1 <- data_fruit1_fruit2[,4]
  }else{
    data_fruit1_fruit2$fruit2 <- data_fruit1_fruit2[,4]
  }

  if(colnames(data_fruit1_fruit2)[5] == fruit1){
    data_fruit1_fruit2$fruit1 <- data_fruit1_fruit2[,5]
  }else{
    data_fruit1_fruit2$fruit2 <- data_fruit1_fruit2[,5]
  }
  
  
  
  #Color of axis: 
  if(fruit1 == "Cherry"){
    col1 <- "#BC3C6D"
  }else{
    if(fruit1 == "Strawberry"){
      col1 <- "#3FAA96"
    }else{
      if(fruit1 == "Blackberry"){
        col1 <- "#301934"
      }
    }
  }
  
  
  if(fruit2 == "Cherry"){
    col2 <- "#BC3C6D"
  }else{
    if(fruit2 == "Strawberry"){
      col2 <- "#3FAA96"
    }else{
      if(fruit2 == "Blackberry"){
        col2 <- "#301934"
      }
    }
  }
  
  
  #Plot
  if (gen == "G0" | gen == "G2") {
  plot_pair <- ggplot(data = data_fruit1_fruit2,
                      aes(x = fruit1, 
                          y = fruit2, 
                          color = Original_environment)) +
    geom_vline(xintercept = 0, linetype ="dashed", color = "grey")+
    geom_hline(yintercept = 0, linetype ="dashed", color = "grey") +
    geom_point(size=3, stroke=1.3) + 
    #guides(fill = FALSE) +
    xlab(xaxis_labelprint)  +
    ylab(yaxis_labelprint)  +
    ggtitle(plot_title) +
    scale_color_manual(name="Fly populations from:",   
                       breaks=c("Cherry", "Strawberry","Blackberry"),
                       labels=c("Cherry","Strawberry","Blackberry"),
                       values=c("#BC3C6D","#3FAA96", "#301934")) +
    theme_LO_sober + 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_text(colour = col1),
          axis.title.y = element_text(colour = col2))
  plot_pair
  
  }else{
    if (gen == "Both") {
      plot_pair <- ggplot(data = data_fruit1_fruit2,
                          aes(x = fruit1, 
                              y = fruit2, 
                              color = Original_environment, 
                              shape = Generation)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        geom_point(size=3, stroke=1.3) + 
        #guides(fill = FALSE) +
        xlab(xaxis_labelprint)  +
        ylab(yaxis_labelprint)  +
        ggtitle(plot_title) +
        scale_color_manual(name="Fly populations from:",   
                           breaks=c("Cherry", "Strawberry","Blackberry"),
                           labels=c("Cherry","Strawberry","Blackberry"),
                           values=c("#BC3C6D","#3FAA96", "#301934")) +
        scale_shape_manual(values = c(21,16)) +
        theme_LO_sober + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title.x = element_text(colour = col1),
              axis.title.y = element_text(colour = col2))
      plot_pair 
      
    }else {
      print("Error: unknown generation")
    }
  }
  
  return(plot_pair) 
}

