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
  
  
  ######### MODELS
  #Extract resid 
  if (gen == "G0" | gen == "G2") {
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
  }else{
    if (gen == "Both") {
      if(trait == "Rate"){
        lm_resid <- lm(y ~ Test_environment:Generation + Population:Generation + log(Nb_eggs), data=data)
      }else{
        if(trait == "Nb_adults") {
          lm_resid <- lm(y ~ Test_environment:Generation + Population:Generation + log(Nb_eggs+1), data=data)
        }else{
          if(trait == "Nb_eggs"){
            if("Obs_A" %in% colnames(dataset)){
              lm_resid <- lm(y ~ Test_environment:Generation + Population:Generation, data=data)
            }else{ 
              if("BoxID" %in% colnames(dataset)){
                lm_resid <- lm(y ~ Test_environment:Generation + Population:Generation + BoxID, data=data)
              }else{
                print("Error: unknown trait")
              } 
            }
          }else{
            print("Error: unknown trait")
          }
        }
      }
      
      }else {
      print("Error: unknown generation")
    }
  }
  
  data$Resid <- residuals(lm_resid)
  
  ######### Dataset summary
  TEMP_SUM <- Rmisc::summarySE(data,
                               measurevar="Resid",
                               groupvars=c("Original_environment","Population", 
                                           "Test_environment","SA", "Generation"))
  
  
  #Subset per pair of fruits
  TEMP_SUM_FRUIT <- TEMP_SUM[TEMP_SUM$Test_environment==fruit1|
                               TEMP_SUM$Test_environment==fruit2,]
  
  ######### TRANSFORM DATASET
  data_fruit1_fruit2<-data.table::dcast(data.table::setDT(TEMP_SUM_FRUIT), 
                                                   Population + Original_environment +  Generation ~ Test_environment,
                                                   value.var  = c("Resid"))

  #Add vector with the mean of the number of tubes for weighted correlation
  data_fruit1_fruit2$N <- NA
  for(i in levels(data_fruit1_fruit2$Population)){
    for(j in levels(data_fruit1_fruit2$Generation))
    data_fruit1_fruit2$N[data_fruit1_fruit2$Population==i&
                           data_fruit1_fruit2$Generation==j] <- sum(TEMP_SUM_FRUIT$N[TEMP_SUM_FRUIT$Test_environment==fruit1&
                                                                                      TEMP_SUM_FRUIT$Population==i&
                                                                                       TEMP_SUM_FRUIT$Generation==j],
                                                                   TEMP_SUM_FRUIT$N[TEMP_SUM_FRUIT$Test_environment==fruit2&
                                                                                      TEMP_SUM_FRUIT$Population==i&
                                                                                      TEMP_SUM_FRUIT$Generation==j])/2
  }
  
  
  ######### PLOT
  # Plot title and y axis title
  plot_title <- ifelse(gen == "G0", "First generation", ifelse(gen == "G2","Third generation", " "))
  
  if("Obs_A" %in% colnames(dataset)  & trait == "Nb_eggs"){
    yaxis_labelprint <- paste0("Residuals(oviposition stimulation)\n in ", fruit2)
  }else{
    if("Obs_A" %in% colnames(dataset)  & trait == "Nb_adults"){
      yaxis_labelprint <- paste0("Residuals(number of adults)\n in ", fruit2)
    }else{
      if("Rate" %in% colnames(dataset) && trait == "Rate"){
        yaxis_labelprint <- paste0("Residuals(offspring performance)\n in ", fruit2)
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
        xaxis_labelprint <- paste0("Residuals(offspring performance)\n in ", fruit1)
      }else{
        if("BoxID" %in% colnames(dataset) && trait == "Nb_eggs"){
          xaxis_labelprint <- paste0("Residuals(oviposition preference)\n in ", fruit1)
        }else{
          print("Error: unknown combinaison dataset x trait")
        }
      }
    }
  }

  
  ## Add columns fruit1 and fruit2
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
  
  
  ######### CORRELATION 
  if (gen == "G0" | gen == "G2") {
    weightedcor <- sjstats:::weighted_correlation(data_fruit1_fruit2,
                                                  x = fruit2, 
                                                  y = fruit1, 
                                                  weights = N, 
                                                  ci.lvl = 0.95)
    rho <- as.numeric(weightedcor$estimate[1])
    eq_rho <- as.character(as.expression(substitute(~~italic(rho)[generation]~"="~weightedcor~"["~inf~";"~sup~"]",
                   list(generation = ifelse(gen=="G0", "G0","G2"),
                        weightedcor = format(rho, digits = 2, nsmall=2), 
                        inf = format(weightedcor$ci[1], digits = 2),
                        sup = format(weightedcor$ci[2], digits = 2)))))
    
  }else{
    if (gen == "Both") {
      weightedcor_G0 <- sjstats:::weighted_correlation(data_fruit1_fruit2[data_fruit1_fruit2$Generation=="G0",],
                                                    x = fruit2, 
                                                    y = fruit1, 
                                                    weights = N, 
                                                    ci.lvl = 0.95)
      rho_g0 <- as.numeric(weightedcor_G0$estimate[1])
      
      weightedcor_G2 <- sjstats:::weighted_correlation(data_fruit1_fruit2[data_fruit1_fruit2$Generation=="G2",],
                                                       x = fruit2, 
                                                       y = fruit1, 
                                                       weights = N, 
                                                       ci.lvl = 0.95)
      rho_g2 <- as.numeric(weightedcor_G2$estimate[1])
      if("Rate" %in% colnames(dataset) && trait == "Rate"){
        eq_rho_G0 <- as.character(as.expression(substitute(~~italic(rho)[generation]~"="~weightedcorG0~"["~infg0~";"~supg0~"]",
                                                           list(generation = "G1",
                                                                weightedcorG0 = format(rho_g0, digits = 2), 
                                                                infg0 = format(weightedcor_G0$ci[1], digits = 1),
                                                                supg0 = format(weightedcor_G0$ci[2], digits = 1)))))
        eq_rho_G2 <- as.character(as.expression(substitute(~~italic(rho)[generation]~"="~weightedcorG2~"["~infg2~";"~supg2~"]",
                                                           list(generation = "G3",
                                                                weightedcorG2 = format(rho_g2, digits = 2), 
                                                                infg2 = format(weightedcor_G2$ci[1], digits = 1),
                                                                supg2 = format(weightedcor_G2$ci[2], digits = 1)))))
        
      }else{
      eq_rho_G0 <- as.character(as.expression(substitute(~~italic(rho)[generation]~"="~weightedcorG0~"["~infg0~";"~supg0~"]",
                                                      list(generation = "G0",
                                                           weightedcorG0 = format(rho_g0, digits = 2), 
                                                           infg0 = format(weightedcor_G0$ci[1], digits = 1),
                                                           supg0 = format(weightedcor_G0$ci[2], digits = 1)))))
      eq_rho_G2 <- as.character(as.expression(substitute(~~italic(rho)[generation]~"="~weightedcorG2~"["~infg2~";"~supg2~"]",
                                                         list(generation = "G2",
                                                              weightedcorG2 = format(rho_g2, digits = 2), 
                                                              infg2 = format(weightedcor_G2$ci[1], digits = 1),
                                                              supg2 = format(weightedcor_G2$ci[2], digits = 1)))))
      }
      
    }else {
      print("Error: unknown generation")
    }
  }
  
  #coordinates of equation 
  if("Obs_A" %in% colnames(dataset)  & trait == "Nb_eggs"){
    if(fruit1=="Cherry") {x_lim = 0}else{
      if(fruit1=="Strawberry") {x_lim = -0.4}else{
        if(fruit1=="Blackberry"){x_lim = -0.45}}}
    
    if(fruit1=="Cherry") {y_lim = 0.82
    y_lim2 = 0.67}else{
      if(fruit1=="Strawberry") {y_lim = 1.12
    y_lim2 = 0.99}else{
      if(fruit1=="Blackberry") {y_lim =  0.78
    y_lim2 = 0.62}}}

    
      }else{
    if("Obs_A" %in% colnames(dataset)  & trait == "Nb_adults"){
      dif<-0.05*(max(c(data_fruit1_fruit2$fruit2,
                       data_fruit1_fruit2$fruit1), na.rm = TRUE)-min(c(data_fruit1_fruit2$fruit2,
                                                                       data_fruit1_fruit2$fruit1), na.rm = TRUE))
      y_lim <- 1.05*max(data_fruit1_fruit2$fruit2, na.rm = TRUE)
      y_lim2 <- (1.05-dif)*max(data_fruit1_fruit2$fruit2, na.rm = TRUE)
      x_lim <- 0.3*max(data_fruit1_fruit2$fruit1, na.rm = TRUE)
      
      
          }else{
      if("Rate" %in% colnames(dataset) && trait == "Rate"){
        if(fruit1=="Cherry") {x_lim = -0.3}else{
          if(fruit1=="Strawberry") {x_lim = -0.09}else{
            if(fruit1=="Blackberry"){x_lim = -0.22}}}
        
        
        if(fruit1=="Cherry") {y_lim = 0.58
          y_lim2 = 0.49}else{
            if(fruit1=="Strawberry") {y_lim = 0.30
            y_lim2 = 0.24
            }else{
              if(fruit1=="Blackberry") {y_lim = 0.85
            y_lim2 = 0.74}}}
 
        
        
            }else{
        if("BoxID" %in% colnames(dataset) && trait == "Nb_eggs"){
          dif<-0.05*(max(c(data_fruit1_fruit2$fruit2,
                           data_fruit1_fruit2$fruit1), na.rm = TRUE)-
                       min(c(data_fruit1_fruit2$fruit2,
                             data_fruit1_fruit2$fruit1), na.rm = TRUE))
          
    
          if(fruit1=="Cherry") {x_lim = 0}else{
            if(fruit1=="Strawberry") {x_lim = -0.16}else{
              if(fruit1=="Blackberry"){x_lim = -1.2}}}
   
          
          if(fruit1=="Cherry") {y_lim = 0.8
          y_lim2 = 0.56}else{
            if(fruit1=="Strawberry") {y_lim = 1.29
          y_lim2 =  1.13}else{
            if(fruit1=="Blackberry") {y_lim =  1.4
          y_lim2 = 1.23}}}
          
          
            }else{
          print("Error: unknown combinaison dataset x trait")
        }
      }
    }
  }
  
  
  
  #Plot
  if (gen == "G0" | gen == "G2") {
  plot_pair <- ggplot(data = data_fruit1_fruit2,
                      aes(x = fruit1, 
                          y = fruit2, 
                          color = Original_environment)) +
    geom_point(size=3, stroke=1.3) + 
    geom_vline(xintercept = 0, linetype ="dashed", color = "grey")+
    geom_hline(yintercept = 0, linetype ="dashed", color = "grey") +
    #guides(fill = FALSE) +
    xlab(xaxis_labelprint)  +
    ylab(yaxis_labelprint)  +
    geom_text(x = x_lim, y = y_lim, 
              label = eq_rho,
              parse = TRUE, 
              color="black", size = 3.5) +
    geom_point(size=3, stroke=1.3) + 
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
  #plot_pair
  
  }else{
    if (gen == "Both") {
      plot_pair <- ggplot(data = data_fruit1_fruit2,
                          aes(x = fruit1, 
                              y = fruit2, 
                              color = Original_environment, 
                              shape = Generation)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        geom_label(x = x_lim, y = y_lim, 
                  label = eq_rho_G0, fill="white",label.size = NA,
                  parse = TRUE, 
                  color="black", size = 3.5, hjust = 0) +
        geom_label(x = x_lim, y = y_lim2, 
                  label = eq_rho_G2,fill="white", label.size = NA,
                  parse = TRUE, 
                  color="black", size = 3.5, hjust = 0) +
        geom_point(size=3, stroke=1.3) + 
        xlab(xaxis_labelprint)  +
        ylab(yaxis_labelprint)  +
        ggtitle(plot_title) +
        scale_color_manual(name="Fly populations from:",   
                           breaks=c("Cherry", "Strawberry","Blackberry"),
                           labels=c("Cherry","Strawberry","Blackberry"),
                           values=c("#BC3C6D","#3FAA96", "#301934")) +
        scale_shape_manual(labels = c("G0/G1", "G2/G3"), values = c(21,16)) +
        theme_LO_sober + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title.x = element_text(colour = col1),
              axis.title.y = element_text(colour = col2))
      #plot_pair 
      
    }else {
      print("Error: unknown generation")
    }
  }
  
  return(plot_pair) 
}

