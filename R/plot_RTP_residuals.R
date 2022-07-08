#' Plot for reciprocal transplant experiment
#'
#' @description Create plot for reciprocal transplant experiment
#' @param gen the generation of dataset
#' @param trait the trait 
#' 
#' @return plot of the residuals of the trait on the three environement
#' @export 
#'
#' @examples
#'plot_RTP_residuals(dataset = data_PERF, trait = "Nb_eggs", gen = "G0")


plot_RTP_residuals <- function(dataset = data_PREF_three, trait = "Nb_eggs", gen = "G0"){
 
  # Subset dataset
  data <- dataset[dataset$Generation == gen,]
  data <- data[complete.cases(data[,trait]), ]
  
  #Remove 4 rows with Nb_eggs=NA (will not be used: neither to test local adpatation nor to extract residuals)
  if(trait == "Nb_adults"){
    data <- data[complete.cases(data[,"Nb_eggs"]), ] 
  }
  
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
  if(trait == "Rate"){
    lm_resid <- lm(y ~ Test_environment + Population + log(Nb_eggs), data=data)
  }else{
    if(trait == "Nb_adults") {
      lm_resid <- lm(y ~ Test_environment + Population + log(Nb_eggs+1), data=data)
    }else{
      if(trait == "Nb_eggs"){
        if("Obs_A" %in% colnames(dataset)){
          lm_resid <- lm(y ~ Test_environment + Population, data=data)
        }else{ 
          if("BoxID" %in% colnames(dataset)){
            lm_resid <- lm(y ~ Test_environment + Population + BoxID, data=data)
          }else{
            print("Error: unknown trait")
          } 
        }
      }else{
        print("Error: unknown trait")
      }
    }
  }
  data$Resid <- residuals(lm_resid)
  
  ##Dataset summary
  TEMP_SUM <- Rmisc::summarySE(data,
                                  measurevar="Resid",
                                  groupvars=c("Original_environment","Test_environment","SA"))
  
  pd <- position_dodge(0.6) # move them .05 to the left and right
  
  
  if(trait == "Nb_eggs"){
    if("Obs_A" %in% colnames(dataset)){
    ## Test for Local Adaptation
    lm_val = lm(y ~ Test_environment + Population + SA + Test_environment:Original_environment, 
                data = data)
    
    Fratio = anova(lm_val)[3,3]/anova(lm_val)[4,3]
    pvalue = 1 - pf(Fratio,anova(lm_val)[3,1],anova(lm_val)[4,1])
    df1 = anova(lm_val)[3,1]
    df2 = anova(lm_val)[4,1]
    }else{
      if("BoxID" %in% colnames(dataset)) {
      lm_val = lm(y ~ Test_environment + Population + SA + 
                    Test_environment:Original_environment + BoxID, 
                  data = data)
      
      Fratio = anova(lm_val)[3,3]/anova(lm_val)[5,3]
      pvalue = 1 - pf(Fratio,anova(lm_val)[3,1],anova(lm_val)[5,1])
      df1 = anova(lm_val)[3,1]
      df2 = anova(lm_val)[5,1]
      }else{
        print("Error: unknown trait")
        }
  }
  
    }else{
    if(trait == "Rate"){
      lm_val = lm(y ~ Test_environment + Population + SA + log(Nb_eggs) +
                    Test_environment:Original_environment, 
                  data = data)
      
      Fratio = anova(lm_val)[3,3]/anova(lm_val)[5,3]
      pvalue = 1 - pf(Fratio,anova(lm_val)[3,1],anova(lm_val)[5,1])
      df1 = anova(lm_val)[3,1]
      df2 = anova(lm_val)[5,1]
      }else{
        if (trait == "Nb_adults"){
          lm_val = lm(y ~ Test_environment + Population + SA + log(Nb_eggs+1) +
                        Test_environment:Original_environment, 
                      data = data)
          
          Fratio = anova(lm_val)[3,3]/anova(lm_val)[5,3]
          pvalue = 1 - pf(Fratio,anova(lm_val)[3,1],anova(lm_val)[5,1])
          df1 = anova(lm_val)[3,1]
          df2 = anova(lm_val)[5,1]
        }else{
          print("Error: unknown trait")
        }
    }
  }

  
  #Equation
  equation <- as.character(as.expression(substitute(italic(F)[df1~","~df2]~"="~Fratio~";"~italic(P)~"="~pvalue,
                                                list(Fratio = format(Fratio, digits = 2, nsmall=2),
                                                     df1 = format(df1, digits = 2), 
                                                     df2 = format(df2, digits = 2), 
                                                     pvalue = format(pvalue, digits = 2)))))
  

  #Ylim 
  max_plot <- 1.1 * max(TEMP_SUM$Resid+TEMP_SUM$ci)
  
  
  # Plot title and y axis title
  plot_title <- ifelse(gen == "G0", "G0", "G2")
  if("Obs_A" %in% colnames(dataset)  & trait == "Nb_eggs"){
    yaxis_labelprint <- paste0("Residuals(fecundity)")
  }else{
    if("Obs_A" %in% colnames(dataset)  & trait == "Nb_adults"){
      yaxis_labelprint <- paste0("Residuals(number of adults)")
    }else{
      if("Rate" %in% colnames(dataset) && trait == "Rate"){
        yaxis_labelprint <- paste0("Residuals(offspring performance)")
      }else{
        if("BoxID" %in% colnames(dataset) && trait == "Nb_eggs"){
          yaxis_labelprint <- paste0("Residuals(oviposition preference)")
        }else{
        print("Error: unknown combinaison dataset x trait")
        }
      }
    }
  }
  
    plot <- ggplot(TEMP_SUM, aes(x = Test_environment, y = Resid,
                               colour = Original_environment,
                               group = Original_environment,
                               fill = "white")) + 
    geom_errorbar(aes(ymin=Resid-(1.96*se), ymax = Resid+(1.96*se)), #with se=sd/sqrt(n)
                  width=.1, position = pd, size = 1) +
    annotate('text', x = 3.5, y = max_plot, label = equation, parse = TRUE, hjust = 1, size = 4) + 
    geom_point(size = 4, position = pd, fill="white", shape = 21, stroke = 1.5) + 
    scale_color_manual(name="Fly populations from:",   
                       breaks=c("Cherry", "Strawberry","Blackberry"),
                       labels=c("Cherry","Strawberry","Blackberry"),
                       values=c("#BC3C6D","#3FAA96", "#301934")) +
    ylab(yaxis_labelprint)  +
    xlab("Test fruit")  +
    ggtitle(plot_title) +
    theme_LO_sober +
      guides(color = guide_legend(override.aes=list(shape = 3, stroke = 1.3)))
    
    ### Add stroke
    plot2 <- plot + 
      geom_point(aes(alpha = SA, fill = interaction(SA, Original_environment), shape = SA), 
                 position = pd, size = 4) + 
      scale_alpha_manual(values = c(0,1)) + 
      scale_shape_manual(name="Test fruit:",   
                        breaks=c("1", "0"),
                        labels=c("Original","Alternative"),
                        values=c(16,21)) + 
      guides(alpha = FALSE, fill=FALSE) 

    plot2
  return(plot2) 
}
  
  