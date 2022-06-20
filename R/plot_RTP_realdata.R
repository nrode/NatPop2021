#' Plot for reciprocal transplant experiment with REAL DATA
#'
#' @description Create plot for reciprocal transplant experiment with real data
#' @param gen the generation of dataset
#' @param trait the trait 
#' 
#' @return plot of the real data of the trait on the three environement
#' @export 
#'
#' @examples
#'plot_RTP_residuals(dataset = data_PERF, trait = "Nb_eggs", gen = "G0")


plot_RTP_realdata <- function(dataset = data_PREF_three, trait = "Nb_eggs", gen = "G0"){
  
  # Subset dataset
  data <- dataset[dataset$Generation == gen,]
  data <- data[complete.cases(data[,trait]), ]
  data$y <- data[,trait]
  
  ##Dataset summary
  TEMP_SUM <- Rmisc::summarySE(data,
                               measurevar="y",
                               groupvars=c("Original_environment","Test_environment","SA"))
  
  pd <- position_dodge(0.6) # move them .05 to the left and right
  

  # Plot title and y axis title
  plot_title <- ifelse(gen == "G0", "G0", "G2")
  if("Obs_A" %in% colnames(dataset)  & trait == "Nb_eggs"){
    yaxis_labelprint <- paste0("Fecundity")
  }else{
    if("Obs_A" %in% colnames(dataset)  & trait == "Nb_adults"){
      yaxis_labelprint <- paste0("Number of adults")
    }else{
      if("Rate" %in% colnames(dataset) && trait == "Rate"){
        yaxis_labelprint <- paste0("Offspring performance")
      }else{
        if("BoxID" %in% colnames(dataset) && trait == "Nb_eggs"){
          yaxis_labelprint <- paste0("Oviposition preference")
        }else{
          print("Error: unknown combinaison dataset x trait")
        }
      }
    }
  }
  
  plot <- ggplot(TEMP_SUM, aes(x = Test_environment, y = y,
                               colour = Original_environment,
                               group = Original_environment,
                               fill = "white")) + 
    geom_errorbar(aes(ymin=y - (1.96*se), ymax = y + (1.96*se)), #with se=sd/sqrt(n)
                  width=.1, position = pd, size = 1) +
    geom_point(size = 4, position = pd, fill="white", shape = 21, stroke = 1.5) + 
    scale_color_manual(name="Fly populations from:",   
                       breaks=c("Cherry", "Strawberry","Blackberry"),
                       labels=c("Cherry","Strawberry","Blackberry"),
                       values=c("#BC3C6D","#3FAA96", "#301934")) +
    ylab(yaxis_labelprint)  +
    xlab("Test fruit")  +
    ggtitle(plot_title) +
    theme_LO_sober 
  
  ### Add stroke
  plot2 <- plot + 
    geom_point(aes(alpha = SA, fill = interaction(SA, Original_environment), shape = SA), 
               position = pd, size = 4) + 
    scale_alpha_manual(values = c(0,1)) + 
    scale_shape_manual(name="Test fruit:",   
                       breaks=c("1", "0"),
                       labels=c("Original","Alternative"),
                       values=c(16,21)) + 
    guides(fill = FALSE, alpha = FALSE) 
  
  
  return(plot2) 
}

