#' Plot for relationship between rate and oviposition 
#'
#' @description Create plot for relation between egg to adult and oviposition stimulation 
#' @param gen the generation of dataset but can be: "G0", "G2" or  "Both"
#' @param trait2 Could be "Stimulation" or "Preference" (trait 1 is Egg-to-adult viabilty)
#' @param fruit could be "Cherry" "Strawberry" or "Blackberry"
#' 
#' 
#' @return plot of the residuals of the trait on the three environement
#' @export 
#'
#' @examples
#'plot_PairwisePOP_residuals(dataset = data_PERF, trait = "Nb_eggs", gen = "Both",fruit1 = "Cherry",fruit2 = "Blackberry")


plot_RelationTraits_residuals <- function(gen = "G2", fruit = "Blackberry", trait2 = "Preference"){
  
  #Load and subset dataset
  if (gen == "G0" | gen == "G2") {
    data <- data_PERF_Rate[data_PERF_Rate$Generation == gen,]
    data2 <- data_PREF_three[data_PREF_three$Generation == gen,]
  }else{
    if (gen == "Both") {
      data <- data_PERF_Rate
      data2 <- data_PREF_three
    }else {
      print("Error: unknown generation")
    }
  }
  
  
  
  ######### MODELS
  #Extract resid 
  if (gen == "G0" | gen == "G2") {
    lm_resid <- lm(asin(sqrt(data$Rate)) ~ Test_environment + Population + log(Nb_eggs), data = data)
    data$Resid_rate <- residuals(lm_resid)
    lm_resid <- lm(log(Nb_eggs+1) ~ Test_environment + Population, data = data)
    data$Resid_eggs <- residuals(lm_resid)
    lm_resid <- lm(log(Nb_eggs+1) ~ Test_environment + Population + BoxID, data = data2)
    data2$Resid_eggs <- residuals(lm_resid)
  }else{
    if (gen == "Both") {
      lm_resid <- lm(asin(sqrt(data$Rate)) ~ Test_environment:Generation + Population:Generation + log(Nb_eggs), data = data)
      data$Resid_rate <- residuals(lm_resid)
      lm_resid <- lm(log(Nb_eggs+1) ~ Test_environment:Generation + Population:Generation, data = data)
      data$Resid_eggs <- residuals(lm_resid)
      lm_resid <- lm(log(Nb_eggs+1) ~ Test_environment:Generation + Population:Generation + BoxID, data = data2)
      data2$Resid_eggs <- residuals(lm_resid)
    }else {
      print("Error: unknown generation")
    }
  }
  
  
  
  ######### Dataset summary
  TEMP_SUM_Rate <- Rmisc::summarySE(data,
                               measurevar="Resid_rate",
                               groupvars=c("Original_environment","Population", 
                                           "Test_environment","SA", "Generation"))
  if (trait2 == "Stimulation") {
  TEMP_SUM_Eggs <- Rmisc::summarySE(data,
                                    measurevar="Resid_eggs",
                                    groupvars=c("Original_environment","Population", 
                                                "Test_environment","SA", "Generation"))
  }else{
    if (trait2 == "Preference") {
      TEMP_SUM_Eggs <- Rmisc::summarySE(data2,
                                        measurevar="Resid_eggs",
                                        groupvars=c("Original_environment","Population", 
                                                    "Test_environment","SA", "Generation"))     
    }else {
      print("Error: unknown trait2")
    }
  }
  
  #Merge Rate and preference
  TEMP_SUM <- merge(TEMP_SUM_Eggs, TEMP_SUM_Rate, 
                    by = c("Generation","Original_environment","Test_environment","Population","SA"))
  TEMP_SUM$N <- (TEMP_SUM$N.x+TEMP_SUM$N.y)/2
  TEMP_SUM <- TEMP_SUM[,c("Generation","Original_environment","Test_environment","Population","SA","Resid_eggs","Resid_rate","N")]
  
  #Subset per fruit
  TEMP_SUM_FRUIT <- TEMP_SUM[TEMP_SUM$Test_environment==fruit,]
  
  
  
  ######### PLOT
  # Plot title and x/y axis title
  plot_title <- ifelse(gen == "G0", "First generation", ifelse(gen == "G2","Third generation", " "))
  xaxis_labelprint <- paste0("Residuals(offspring performance)\n in ", fruit)
  if (trait2 == "Stimulation") {
    yaxis_labelprint <- paste0("Residuals(oviposition stimulation)\n in ", fruit)
  }else{
    if (trait2 == "Preference") {
      yaxis_labelprint <- paste0("Residuals(oviposition preference)\n in ", fruit)
    }else {
      print("Error: unknown trait")
    }
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
      }
    }
  }

  
  
  ######### CORRELATION 
  if (gen == "G0" | gen == "G2") {
    weightedcor <- sjstats:::weighted_correlation(TEMP_SUM_FRUIT,
                                                  x = Resid_eggs, 
                                                  y = Resid_rate, 
                                                  weights = N, 
                                                  ci.lvl = 0.95)
    rho <- as.numeric(weightedcor$estimate[1])
    eq_rho <- as.character(as.expression(substitute(~~italic(rho)[generation]~"="~weightedcor~"["~inf~";"~sup~"]",
                                                    list(generation = ifelse(gen=="G0", "G0/G1","G2/G3"),
                                                         weightedcor = format(rho, digits = 2, nsmall=2), 
                                                         inf = format(weightedcor$ci[1], digits = 2),
                                                         sup = format(weightedcor$ci[2], digits = 2)))))
    
  }else{
    if (gen == "Both") {
      weightedcor_G0 <- sjstats:::weighted_correlation(TEMP_SUM_FRUIT[TEMP_SUM_FRUIT$Generation=="G0",],
                                                    x = Resid_eggs, 
                                                    y = Resid_rate, 
                                                    weights = N, 
                                                    ci.lvl = 0.95)
      
      
      rho_g0 <- as.numeric(weightedcor_G0$estimate[1])
      
      weightedcor_G2 <- sjstats:::weighted_correlation(TEMP_SUM_FRUIT[TEMP_SUM_FRUIT$Generation=="G2",],
                                                       x = Resid_eggs, 
                                                       y = Resid_rate, 
                                                       weights = N, 
                                                       ci.lvl = 0.95)
      rho_g2 <- as.numeric(weightedcor_G2$estimate[1])
     eq_rho_G0 <- as.character(as.expression(substitute(~~italic(rho)[generation]~"="~weightedcorG0~"["~infg0~";"~supg0~"]",
                                                         list(generation = "G0/G1",
                                                              weightedcorG0 = format(rho_g0, digits = 2), 
                                                              infg0 = format(weightedcor_G0$ci[1], digits = 1),
                                                              supg0 = format(weightedcor_G0$ci[2], digits = 1)))))
      eq_rho_G2 <- as.character(as.expression(substitute(~~italic(rho)[generation]~"="~weightedcorG2~"["~infg2~";"~supg2~"]",
                                                         list(generation = "G2/G3",
                                                              weightedcorG2 = format(rho_g2, digits = 2), 
                                                              infg2 = format(weightedcor_G2$ci[1], digits = 1),
                                                              supg2 = format(weightedcor_G2$ci[2], digits = 1)))))
  
    }else {
      print("Error: unknown generation")
    }
  }
  
  #coordinates of equation 
  if (gen == "G0") {
    if (trait2 == "Preference"){
      if(fruit=="Cherry") {
        ymax <- max(TEMP_SUM_FRUIT$Resid_rate)*0.83
        }else{
        if(fruit=="Strawberry") {        
        ymax <- max(TEMP_SUM_FRUIT$Resid_rate)*0.83
        }else{
          if(fruit=="Blackberry"){
            ymax <- max(TEMP_SUM_FRUIT$Resid_rate)*0.83}}}
    }else{
      if(fruit=="Cherry") {
        ymax <- max(TEMP_SUM_FRUIT$Resid_rate)*0.83
      }else{
        if(fruit=="Strawberry") {        
          ymax <- max(TEMP_SUM_FRUIT$Resid_rate)*0.83
        }else{
          if(fruit=="Blackberry"){
            ymax <- max(TEMP_SUM_FRUIT$Resid_rate)*0.83}}}
    }
  }else{
    if (gen == "G2") {
      if (trait2 == "Preference"){      
        if(fruit=="Cherry") {
          ymax <- max(TEMP_SUM_FRUIT$Resid_rate)*0.88
        }else{
          if(fruit=="Strawberry") {        
            ymax <- -0.08
          }else{
            if(fruit=="Blackberry"){
              ymax <- max(TEMP_SUM_FRUIT$Resid_rate)*0.88}}}
      }else{
        if(fruit=="Cherry") {
          ymax <- max(TEMP_SUM_FRUIT$Resid_rate)*0.88
          }else{
          if(fruit=="Strawberry") {        
            ymax <- -0.08
          }else{
            if(fruit=="Blackberry"){
              ymax <- max(TEMP_SUM_FRUIT$Resid_rate)*0.88}}}
      }
  }else{
    if (gen == "Both") {
      ymax <- max(TEMP_SUM_FRUIT$Resid_rate)*0.9
      ymax2 <- max(TEMP_SUM_FRUIT$Resid_rate)*0.9
      xmax2 <- min(TEMP_SUM_FRUIT$Resid_eggs)*1.1
    }else{
      print("Error: unknown generation")
    }
  }
  }
  
  xmax <- min(TEMP_SUM_FRUIT$Resid_eggs)*0.8
  
  #Plot
  if (gen == "G0") {
    plot_pair <- ggplot(data = TEMP_SUM_FRUIT,
                        aes(x = Resid_eggs, 
                            y = Resid_rate, 
                            color = Original_environment)) +
      geom_vline(xintercept = 0, linetype ="dashed", color = "grey")+
      geom_hline(yintercept = 0, linetype ="dashed", color = "grey") +
      geom_point(size=2.8, stroke=1.3, shape = 21) + 
      #guides(fill = FALSE) +
      xlab(xaxis_labelprint)  +
      ylab(yaxis_labelprint)  +
      geom_text(x = xmax, y = ymax,
                label = eq_rho,
                parse = TRUE,
                color="black", size = 3.5, hjust = 0) +
      ggtitle(plot_title) +
      scale_color_manual(name="Fly populations from:",   
                         breaks=c("Cherry", "Strawberry","Blackberry"),
                         labels=c("Cherry","Strawberry","Blackberry"),
                         values=c("#BC3C6D","#3FAA96", "#301934")) +
      theme_LO_sober + 
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title.x = element_text(colour = col),
            axis.title.y = element_text(colour = col))
    #plot_pair
    
  }else{
    if (gen == "G2") {
      plot_pair <- ggplot(data = TEMP_SUM_FRUIT,
                          aes(x = Resid_eggs, 
                              y = Resid_rate, 
                              color = Original_environment)) +
        geom_vline(xintercept = 0, linetype ="dashed", color = "grey")+
        geom_hline(yintercept = 0, linetype ="dashed", color = "grey") +
        geom_point(size=2.8, stroke=1.3, shape = 16) + 
        #guides(fill = FALSE) +
        xlab(xaxis_labelprint)  +
        ylab(yaxis_labelprint)  +
        geom_text(x = xmax, y = ymax,
                  label = eq_rho,
                  parse = TRUE,
                  color="black", size = 3.5, hjust = 0) +
        ggtitle(plot_title) +
        scale_color_manual(name="Fly populations from:",   
                           breaks=c("Cherry", "Strawberry","Blackberry"),
                           labels=c("Cherry","Strawberry","Blackberry"),
                           values=c("#BC3C6D","#3FAA96", "#301934")) +
        theme_LO_sober + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title.x = element_text(colour = col),
              axis.title.y = element_text(colour = col))
      #plot_pair
      
    }else{
    if (gen == "Both") {
      plot_pair <- ggplot(data = TEMP_SUM_FRUIT,
                          aes(y = Resid_eggs, 
                              x = Resid_rate, 
                              color = Original_environment, 
                              shape = Generation)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        geom_point(size=2.5, stroke=1.3) + 
        #guides(fill = FALSE) +
        geom_text(x = xmax, y = ymax,
                  label = eq_rho_G0,
                  parse = TRUE,
                  color="black", size = 3.5) +
        geom_text(x = xmax2, y = ymax2,
                  label = eq_rho_G2,
                  parse = TRUE,
                  color="black", size = 3.5) +
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
              axis.title.x = element_text(colour = col),
              axis.title.y = element_text(colour = col))
      #plot_pair 
      
    }else {
      print("Error: unknown generation")
    }
    }
  }
  
  
  return(plot_pair) 
}

