#' Plot for reciprocal transplant experiment
#'
#' @description Create plot for reciprocal transplant experiment
#' @param effect can be "Genetic" or "Non-genetic"
#' @param trait the trait 
#' 
#' @return plot of the residuals of the trait on the three environement
#' @export 
#'
#' @examples
#'plot_RTP_residuals(dataset = data_PERF, trait = "Nb_eggs", effect = "Genetic")


plot_Genetic_Nongenetic_residuals <- function(dataset = data_PERF_Rate, trait = "Rate", effect = "Genetic"){
  # Subset dataset
  data <- dataset
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
  
  data$Resid <- residuals(lm_resid)

  ##Dataset summary
  TEMP_SUM <- Rmisc::summarySE(data,
                               measurevar="Resid",
                               groupvars=c("Generation","Original_environment","Test_environment","SA"))
  #tapply(data$Resid,list(data$Original_environment,data$Test_environment,data$Generation),mean)
  #tapply(data$Resid,list(data$Original_environment,data$Test_environment,data$Generation),sd)
  
  
  ##Transform in genetic vs non genetic dataset
  SUM_Genetic_NonGenetic <- TEMP_SUM
  SUM_Genetic_NonGenetic$Effect <- ifelse(SUM_Genetic_NonGenetic$Generation=="G0","Non-genetic","Genetic")
  #Calcul mean and sd as:
    #mean(genetic)=mean(G2)
    #sd(genetic)=sd(G2)
    #mean(nnon-genetic)=mean(G0)-mean(G2)
    #sd(genetic)=sqrt(var(G0)-var(G2))
  
  #Genetic effect
  SUM_Genetic_NonGenetic$mean_effect <-  ifelse(SUM_Genetic_NonGenetic$Effect=="Genetic",SUM_Genetic_NonGenetic$Resid,NA)
  SUM_Genetic_NonGenetic$sd_effect <-  ifelse(SUM_Genetic_NonGenetic$Effect=="Genetic",SUM_Genetic_NonGenetic$sd,NA)
  
  #Nongenetic effect
  for (i in levels(SUM_Genetic_NonGenetic$Original_environment)) {
      for (j in levels(SUM_Genetic_NonGenetic$Test_environment)) {
        SUM_Genetic_NonGenetic$mean_effect[SUM_Genetic_NonGenetic$Original_environment==i&
                                             SUM_Genetic_NonGenetic$Test_environment==j&
                                             SUM_Genetic_NonGenetic$Generation=="G0"] <-
          SUM_Genetic_NonGenetic$Resid[SUM_Genetic_NonGenetic$Original_environment==i&
                                         SUM_Genetic_NonGenetic$Test_environment==j&
                                         SUM_Genetic_NonGenetic$Generation=="G0"] - 
          SUM_Genetic_NonGenetic$Resid[SUM_Genetic_NonGenetic$Original_environment==i&
                                         SUM_Genetic_NonGenetic$Test_environment==j&
                                         SUM_Genetic_NonGenetic$Generation=="G2"]
        
        SUM_Genetic_NonGenetic$sd_effect[SUM_Genetic_NonGenetic$Original_environment==i&
                                             SUM_Genetic_NonGenetic$Test_environment==j&
                                             SUM_Genetic_NonGenetic$Generation=="G0"] <-
          sqrt((SUM_Genetic_NonGenetic$sd[SUM_Genetic_NonGenetic$Original_environment==i&
                                         SUM_Genetic_NonGenetic$Test_environment==j&
                                         SUM_Genetic_NonGenetic$Generation=="G0"])^2-
                 (SUM_Genetic_NonGenetic$sd[SUM_Genetic_NonGenetic$Original_environment==i&
                                         SUM_Genetic_NonGenetic$Test_environment==j&
                                         SUM_Genetic_NonGenetic$Generation=="G2"])^2)
     }
  }
  
  
  
  #Perform se:
  SUM_Genetic_NonGenetic$se_effect <- SUM_Genetic_NonGenetic$sd_effect/sqrt(SUM_Genetic_NonGenetic$N)
      
  
  SUM_Genetic_NonGenetic <- SUM_Genetic_NonGenetic[,c("Effect","Original_environment","Test_environment",
                                                      "SA","mean_effect","sd_effect","se_effect")]

  #TEST LOCAL ADAPTATION GENETIC vs NON-GENETIC
  if(trait == "Nb_eggs"){
    if("Obs_A" %in% colnames(dataset)){
      
      lm_val <- aov(y ~ pop_gen + hab_gen + SA:IndicG0 + SA +
                  Original_environment:Test_environment + 
                  Original_environment:Test_environment:IndicG0, 
                data = data)
      
      ## F test for SA
      (Fratio_Gen <- (anova(lm_val)[3,2]/anova(lm_val)[5,2])/(1/anova(lm_val)[5, 1]))
      (pvalue_Gen <- 1 - pf(Fratio_Gen, 1, anova(lm_val)[5, 1]) )
      df1_Gen = 1
      df2_Gen = anova(lm_val)[5, 1]
      
      ## F test for SA
      (Fratio_NonGen <- (anova(lm_val)[4,2]/anova(lm_val)[6,2])/(1/anova(lm_val)[6, 1]))
      (pvalue_NonGen <- 1 - pf(Fratio_NonGen, 1, anova(lm_val)[6, 1]) )
      df1_NonGen = 1
      df2_NonGen = anova(lm_val)[6, 1]
      
    }else{
      if("BoxID" %in% colnames(dataset)) {
        lm_val <- aov(y ~ pop_gen + hab_gen + SA:IndicG0 + SA +
                    Original_environment:Test_environment + 
                    Original_environment:Test_environment:IndicG0 +
                    BoxID, 
                  data = data)
        
        ## F test for SA
        (Fratio_Gen <- (anova(lm_val)[3,2]/anova(lm_val)[6,2])/(1/anova(lm_val)[6, 1]))
        (pvalue_Gen <- 1 - pf(Fratio_Gen, 1, anova(lm_val)[6, 1]) )
        df1_Gen = 1
        df2_Gen = anova(lm_val)[6, 1]
        
        ## F test for SA
        (Fratio_NonGen <- (anova(lm_val)[5,2]/anova(lm_val)[7,2])/(1/anova(lm_val)[7, 1]))
        (pvalue_NonGen <- 1 - pf(Fratio_NonGen, 1, anova(lm_val)[7, 1]) )
        df1_NonGen = 1
        df2_NonGen = anova(lm_val)[7, 1]
        
      }else{
        print("Error: unknown trait")
      }
    }
    
  }else{
    if(trait == "Rate"){
      
      
      lm_val <- aov(y ~ pop_gen + hab_gen + SA:IndicG0 + SA +
                  Original_environment:Test_environment + 
                  Original_environment:Test_environment:IndicG0 +
                  log(Nb_eggs), 
                data = data)
      
      
      ## F test for SA
      Fratio_Gen <- (anova(lm_val)[3,2]/anova(lm_val)[6,2])/(1/anova(lm_val)[6, 1])
      pvalue_Gen <- 1 - pf(Fratio_Gen, 1, anova(lm_val)[6, 1]) 
      df1_Gen = 1
      df2_Gen = anova(lm_val)[6, 1]
      
      ## F test for SA
      Fratio_NonGen <- (anova(lm_val)[5,2]/anova(lm_val)[7,2])/(1/anova(lm_val)[7, 1])
      pvalue_NonGen <- 1 - pf(Fratio_NonGen, 1, anova(lm_val)[7, 1]) 
      df1_NonGen = 1
      df2_NonGen = anova(lm_val)[7, 1]

    }else{
      if (trait == "Nb_adults"){
        lm_val <- aov(y ~ pop_gen + hab_gen + SA:IndicG0 + SA +
                        Original_environment:Test_environment + 
                        Original_environment:Test_environment:IndicG0 +
                        log(Nb_eggs+1), 
                      data = data)
        
        ## F test for SA
        Fratio_Gen <- (anova(lm_val)[3,2]/anova(lm_val)[6,2])/(1/anova(lm_val)[6, 1])
        pvalue_Gen <- 1 - pf(Fratio_Gen, 1, anova(lm_val)[6, 1]) 
        df1_Gen = 1
        df2_Gen = anova(lm_val)[6, 1]
        
        ## F test for SA
        Fratio_NonGen <- (anova(lm_val)[5,2]/anova(lm_val)[7,2])/(1/anova(lm_val)[7, 1])
        pvalue_NonGen <- 1 - pf(Fratio_NonGen, 1, anova(lm_val)[7, 1]) 
        df1_NonGen = 1
        df2_NonGen = anova(lm_val)[7, 1]
        
      }else{
        print("Error: unknown trait")
      }
    }
  }
  
  if(effect == "Genetic"){
    Fratio = Fratio_Gen
    pvalue = pvalue_Gen
    df1 = df1_Gen
    df2 = df2_Gen
    effect_name_test = "Genetic"  }else{
    if(effect == "Non-genetic"){
      Fratio = Fratio_NonGen
      pvalue = pvalue_NonGen
      df1 = df1_NonGen
      df2 = df2_NonGen  
      effect_name_test = "Plastic"}else{
      print("Error: effect tested unknown")
    }
  }
  

  
  
  
  #Equation
  equation <- as.character(as.expression(substitute(italic(F)[effect_name~"-"~df1~","~df2]~"="~Fratio~";"~italic(P)~"="~pvalue,
                                                    list(effect_name = effect_name_test,
                                                         Fratio = format(Fratio, digits = 2, nsmall=2),
                                                         df1 = format(df1, digits = 2), 
                                                         df2 = format(df2, digits = 2), 
                                                         pvalue = format(pvalue, digits = 2)))))
  equation
  
  
  
  ##### Subset with the generation
  #Dataset with the mean
  SUM_Genetic_NonGenetic <- SUM_Genetic_NonGenetic[SUM_Genetic_NonGenetic$Effect == effect,]
  # #Dataset for the analysis
  # if(effect == "Genetic"){
  #   data <- data[data$Generation == "G0",]
  # }else{
  #   if(effect == "Non-genetic"){
  #     data <- data[data$Generation == "G2",]
  #   }else{
  #     print("Error: generation trait")
  #   }
  # }
  
  #Ylim 
  if("BoxID" %in% colnames(dataset) && trait == "Nb_eggs" && effect == "Non-genetic"){
    max_plot <- 0.34
    }else{
      max_plot <- 1.1 * max(SUM_Genetic_NonGenetic$mean_effect+1.96*SUM_Genetic_NonGenetic$se_effect)
      }

  
  
  # Plot title and y axis title
  plot_title <- effect
 
  if("Obs_A" %in% colnames(dataset)  & trait == "Nb_eggs"){
    yaxis_labelprint <- paste0("Residuals(oviposition stimulation)")
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
  pd <- position_dodge(0.6) # move them .05 to the left and right
  
  plot <- ggplot(SUM_Genetic_NonGenetic, aes(x = Test_environment, y = mean_effect,
                               colour = Original_environment,
                               group = Original_environment,
                               fill = "white")) +

    geom_errorbar(aes(ymin=mean_effect-1.96*se_effect, ymax = mean_effect+1.96*se_effect),
                      width=.1, position=pd, size = 1) +
    geom_point(size = 4, position=pd, fill="white", shape = 21, stroke = 1.5) + 
    scale_color_manual(name="Fly populations from:",   
                       breaks=c("Cherry", "Strawberry","Blackberry"),
                       labels=c("Cherry","Strawberry","Blackberry"),
                       values=c("#BC3C6D","#3FAA96", "#301934")) +
    annotate(geom="label", x = 0.8,
             y = max_plot, label = equation, parse = TRUE, 
             hjust = 0, size = 4, fill="white", label.size = NA) +
    geom_errorbar(aes(ymin=mean_effect-1.96*se_effect, ymax = mean_effect+1.96*se_effect),
                  width=.1, position=pd, size = 1) +
    geom_point(size = 4, position=pd, fill="white", shape = 21, stroke = 1.5) +
    ylab(yaxis_labelprint)  +
    xlab("Test environment")  +
    ggtitle(plot_title) +
    theme_LO_sober 
  
  ### Add stroke
  plot2 <- plot + 
    geom_point(aes(alpha = SA, fill = interaction(SA, Original_environment)), 
               position = pd, size = 4) + 
    scale_alpha_manual(values = c(0,1)) + 
    guides(fill = FALSE, alpha = FALSE) 
  

  return(plot2) 

}

  