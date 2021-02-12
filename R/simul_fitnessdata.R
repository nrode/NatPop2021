#' Simulate fitness and calculate SA 
#'
#' @description Simulate fitness data
#' @param distrib Distribution of data: "normal", "poisson" or "binomial"
#' @param design If design = "unbalanced" -> sample dataset
#' @param seed Seed used for simulation
#' @param nfruit Number of selective fruits 
#' @param npop_per_fruit Number of populations per fruit
#' @param nhab Number of habitat
#' @param nrep Number of replicates per populationxhabitat
#' @param ntrial Number of trial of Bernouilli (only for Logit distribution)
#' @param sdpop Standard error for the variance across populations
#' @param sdfruithab Standard error for the variance of the genetic interaction PopxHabitat 
#' @param sdfruithab_ng Standard error for the variance of the non-genetic interaction PopxHabitat 
#' @param sigma Residual variance across replicates
#' @param rho Genetic covariance (from which emerges SA genetic)
#' @param rho_ng Non genetic covariance (from which emerges SA non genetic)
#' 
#' @return vector seed, SA_Gen_True, SA_NonGen_True, SAcoef, rho, rho_ng, Fratio_Gen, 
#' pvalue_Gen, Fratio_NonGen, pvalue_NonGen, Fratio_Gen_aov, pvalue_Gen_aov, Fratio_NonGen_aov, pvalue_NonGen_aov
#' @export 
#'
#' @examples
#'simul_fitnessdata_normal(distrib = "normal", design = "balanced", seed = 1, nfruit = 3, nhab = 3, npop_per_fruit = 3,
#' nrep = 1, sdpop = 0,  sdfruithab = 1, sdfruithab_ng = 1, rho=-1/(nhab-1), rho_ng=-1/(nhab-1)


simul_fitnessdata <- function(distrib = "normal", design = "balanced",
                              seed = 1, nfruit = 3, nhab = 3, npop_per_fruit = 5, nrep = 10, ntrial = 20,  
                              sdbox = NA, sdpop = 0.5, sdfruithab = 1, sdfruithab_ng = 1, 
                              rho = -1/(nhab-1), rho_ng = 0, sigma = 0.5){
  
  ########################################################
  ########           Create dataframe             ########
  ########################################################
  data <- expand.grid(Ind = as.character(1:nrep),
                      Hab = as.character(1:nhab), 
                      Fruit = as.character(1:nfruit),
                      Pop_fruit = as.character(1:npop_per_fruit),
                      Gen = c("G0", "G2"))
  data$Pop <- as.factor(paste(data$Fruit,data$Pop_fruit,sep = "_"))
  data$SA<-as.factor(ifelse(data$Fruit == data$Hab, 1, 0))
  
  
  ########################################################
  ########     Balanced vs Unbalanced dataset     ########
  ########################################################
  if (design == "unbalanced") {

    #Number of row in the balanced design 
    nb_row_balanced <- nrep * nfruit * nhab * npop_per_fruit * 2 #for two generations
    #Number of row that there would be if there was one less replicate per combination
    nb_row_unbalanced <- (nrep-1) * nfruit * nhab * npop_per_fruit * 2
    
    #Select number of row for unbalanced design in balanced dataset 
    row_sample <- sample(1:nb_row_balanced,nb_row_unbalanced, replace = FALSE)
    #Sample number of row for balanced design in unbalanced dataset 
      #Like that: the number of data is the same im balanced and unbalanced 
                  # but some combinations are more or less repeated. 
    row_unbalanced <- sample(row_sample,nb_row_balanced, replace = TRUE)
    
    #Select row in balanced dataset corresponsdong to the unbalanced sample
    data_unbalanced <- data[row_unbalanced,]
    
    #Check: 
    # dim(data)
    # dim(data_unbalanced)
    # tapply(data$Ind,list(data$Pop_fruit,data$Fruit,data$Hab,data$Gen),length)
    # tapply(data_unbalanced$Ind,list(data_unbalanced$Pop_fruit,data_unbalanced$Fruit,
    #                                 data_unbalanced$Hab,data_unbalanced$Gen),length)
    
    data <- data_unbalanced
  }
  
  
  ########################################################
  ########           Add indic variables          ########
  ########################################################
  data$IndicG0<-as.numeric(ifelse (as.character(data$Gen) == "G0", 1, 0))
  data$IndicG2<-as.numeric(ifelse (as.character(data$Gen) == "G2", 1, 0))
  
  data$SAIndicG0<-as.numeric(ifelse (as.character(data$Gen) == "G0"&
                                       (as.character(data$SA) == "1"), 1, 0))
  
  
  data$fruit_hab <- as.factor(paste(data$Fruit, data$Hab, sep = "_"))
  data$fruit_hab_ng <- as.factor(paste(data$Fruit, data$Hab, data$Gen, sep = "_"))
  
  data$fruit_gen <- as.factor(paste(data$Fruit, data$Gen, sep = "_"))
  data$hab_gen <- as.factor(paste(data$Hab, data$Gen, sep = "_"))
  data$pop_gen <- as.factor(paste(data$Pop, data$Gen, sep = "_"))

  
  
  ########################################################
  ########        Matrix: Genetic effect          ########
  ########################################################
  ## Sample genetic interaction Fruit x habitat
  set.seed(seed)
  mat_fruithab <- matrix(rho*sdfruithab*sdfruithab, nfruit, nhab)
  diag(mat_fruithab) <- sdfruithab*sdfruithab
  ## Variance-covariance matrix between genetic fruit x hab effects
  FruitHabEff <- MASS::mvrnorm(n=nhab, mu = rep(0, nhab), Sigma = mat_fruithab)
  
  if (rho<0&sdfruithab != 0) {
    for (i in 1:nfruit){
      maxFruitHab_Eff <- max(FruitHabEff[i,])
      FruitHabEff[i,-i] <- FruitHabEff[i,][FruitHabEff[i,] != maxFruitHab_Eff]
      FruitHabEff[i,i] <- maxFruitHab_Eff
      
    }
  }
  
  Symp <- diag(FruitHabEff)
  Allo <- FruitHabEff[row(FruitHabEff) != col(FruitHabEff)]
  SA_Gen_True <- mean(Symp) - mean(Allo)
  
  ## Melt dataset
  fruithab <- reshape::melt(FruitHabEff)
  names(fruithab) <- c("fruit", "hab", "GenEff")
  fruithab$fruit_hab <- as.factor(paste(fruithab$fruit, fruithab$hab, sep = "_"))
  
  data <- merge(x = data, y = fruithab, by = "fruit_hab")
  
  
  
  ########################################################
  ########    Matrix: Non genetic effect          ########
  ########################################################
  ## Sample non genetic interaction Fruit x habitat
  mat_fruithab_ng <- matrix(rho_ng*sdfruithab_ng*sdfruithab_ng, nfruit, nhab)
  diag(mat_fruithab_ng) <- sdfruithab_ng*sdfruithab_ng
  ## Variance-covariance matrix between non genetic fruit x hab effects
  FruitHab_ngEff <- MASS::mvrnorm(n=nhab, mu = rep(0, nhab), Sigma = mat_fruithab_ng)
  
  if (rho_ng<0&sdfruithab_ng!=0) {
    for (i in 1:nfruit){
      maxFruitHab_ngEff <- max(FruitHab_ngEff[i,])
      FruitHab_ngEff[i,-i] <- FruitHab_ngEff[i,][FruitHab_ngEff[i,]!= maxFruitHab_ngEff]
      FruitHab_ngEff[i,i] <- maxFruitHab_ngEff
      
    }
  }
  Symp <- diag(FruitHab_ngEff)
  Allo <- FruitHab_ngEff[row(FruitHab_ngEff)!=col(FruitHab_ngEff)]
  SA_NonGen_True <- mean(Symp)-mean(Allo)
  
  ## Melt dataset
  fruithab_ng <- reshape::melt(FruitHab_ngEff)
  names(fruithab_ng) <- c("fruit", "hab", "NonGenEff")
  fruithab_ng$fruit_hab_ng <- as.factor(paste(fruithab_ng$fruit, fruithab_ng$hab, "G0", sep = "_"))
  
  
  data <- merge(x = data, y =fruithab_ng[,3:4], by = "fruit_hab_ng", all.x =TRUE)
  ## When generation G2, replace NA by 0
  data$NonGenEff <- ifelse(is.na(data$NonGenEff), 0, data$NonGenEff)
  
  # Add variation sdpop
  data$PopEff <-  data$Pop
  levels(data$PopEff) <- rnorm(nlevels(data$PopEff), 0, sd = sdpop)
  data$PopEff <- as.numeric(as.character(data$PopEff))

  
  
  ########################################################
  #########    Distribution: calculate fitness    ########
  ########################################################
  if (distrib == "normal") {
    data$fitness <- data$GenEff + data$NonGenEff + data$PopEff + rnorm(n=nrow(data), 0, sigma) 
  }else{
    if (distrib == "poisson") {
      if(is.na(sdbox)){
        data$fitness_count <- rpois(n=nrow(data), lambda = exp(data$GenEff + data$NonGenEff + data$PopEff ))
        data$fitness <- log(data$fitness_count+1)
      }else{
        ## We consider that each "Ind" associated with nhab environments in the dataset correspond to the measure of the same population in a given generation in a "preference box" including the nhab environments
        data$BoxID <- as.factor(paste(data$Pop, data$Ind, data$Gen, sep="_"))

        # Add variation among boxes
        data$BoxEff <- data$BoxID
        levels(data$BoxEff) <- rnorm(nlevels(data$BoxEff), 0, sd = sdbox)
        data$BoxEff <- as.numeric(as.character(data$BoxEff))
        data$fitness_count <- rpois(n=nrow(data), lambda = exp(data$GenEff + data$NonGenEff + data$PopEff + data$BoxEff))
        data$fitness <- log(data$fitness_count+1)
      }

    }else{
      if (distrib == "binomial") {
      data$fitness_logit <- rbinom(n=nrow(data),
                             size = ntrial, 
                             prob = (1/(1+exp(-(data$GenEff + data$NonGenEff + data$PopEff ))))) / ntrial
      data$fitness <- asin(sqrt(data$fitness_logit))
      }else{
      print("Error: unknown distribution")  
        }
    } 
    }
  

  
  
  
  ########################################################
  ########               Box or not               ########
  ########################################################
  
    
  if(is.na(sdbox)){
  

  
  
  
  
  #######################################################
  ## Analysis of genetic effects lm   ###
  #######################################################
  
  m1 <- aov(fitness ~ pop_gen + hab_gen + SA + Fruit:Hab, 
            contrasts = list(Fruit = "contr.sum", Hab = "contr.sum"), data = data)
  
  ## F test for SA
  Fratio = (anova(m1)[3,2]/anova(m1)[4,2])/(1/anova(m1)[4, 1])
  pvalue = 1 - pf(Fratio, 1, anova(m1)[4, 1]) 
  
  
  #######################################################
  ## Analysis of genetic effects lmer ###
  #######################################################
  ## Model to get the df of the interaction
  m00 <- lme4::lmer(fitness ~ Pop + Hab + SA + (1|Fruit:Hab), data = data)
  
  indic <- grep("SA",names(lme4::fixef(m00)))
  
  Fratio_Gen = as.numeric(lme4::fixef(m00)[indic[1]]^2/vcov(m00)[indic[1],indic[1]])
  pvalue_Gen = 1 - pf(Fratio_Gen, 1, anova(m1)[4, 1])
  
  
  
  #######################################################
  ## Analysis of genetic and non-genetic effects lm   ###
  #######################################################
  m2 <- aov(fitness ~ pop_gen + hab_gen + SA:IndicG0 + SA +
              Fruit:Hab + Fruit:Hab:IndicG0, data = data)
  
  ## F test for SA
  Fratio_Gen_aov = (anova(m2)[3,2]/anova(m2)[5,2])/(1/anova(m2)[5, 1])
  pvalue_Gen_aov = 1 - pf(Fratio_Gen_aov, 1, anova(m2)[5, 1]) 
  
  
  ## F test for SA
  Fratio_NonGen_aov = (anova(m2)[4,2]/anova(m2)[6,2])/(1/anova(m2)[6, 1])
  pvalue_NonGen_aov = 1 - pf(Fratio_NonGen_aov, 1, anova(m2)[6, 1]) 
  
  
  
  
  #######################################################
  ## Analysis of genetic and non-genetic effects lmer ###
  #######################################################
  m0 <- lme4::lmer(fitness ~ pop_gen + hab_gen + SA + SAIndicG0 + (1|Fruit:Hab)+
                     (0+lme4::dummy(Gen, "G0")|Fruit:Hab), data = data)
  
  indic <- grep("SA",names(lme4::fixef(m0)))
  
  Fratio_NonGen = as.numeric(lme4::fixef(m0)[indic[2]]^2/vcov(m0)[indic[2],indic[2]])
  pvalue_NonGen = 1 - pf(Fratio_NonGen, 1, anova(m2)[6, 1])
  
  
  
  
  
  
  
  
  ##################################################
  ## Estimate genetic and non-genetic SA ###
  ##################################################
  
  m00 <- lm(fitness ~ pop_gen + hab_gen + SA + SAIndicG0, data = data)
  indic <- grep("SA",names(coef(m00)))
  
  SAcoef <- coef(m00)[indic]
  names(SAcoef) <- c("SAGen_Est", "SANonGen_Est")
  
  
  
  
  
}else{
  #... TO DO add models with boxID effect
}

  
  
  
  
  
  
  
  return(c(seed=seed, SA_Gen_True = SA_Gen_True, SA_NonGen_True=SA_NonGen_True,
           SAcoef, rho = rho, rho_ng = rho_ng, 
           Fratio_Gen = Fratio_Gen, pvalue_Gen = pvalue_Gen,
           Fratio_NonGen = Fratio_NonGen, pvalue_NonGen = pvalue_NonGen, 
           Fratio_Gen_aov = Fratio_Gen_aov, pvalue_Gen_aov = pvalue_Gen_aov, 
           Fratio_NonGen_aov = Fratio_NonGen_aov, pvalue_NonGen_aov = pvalue_NonGen_aov))
  
}

