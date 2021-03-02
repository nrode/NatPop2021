#' Power test non genetic local adaptation 
#'
#' @description Simulation to estimate the power of detection non genetic local adaptation 
#' @param seed Seed used for simulation
#' @param SAng Estimate of the  non genetic SA effect
#' 
#' @return vector: seed, SAng, SAGen, SANonGen, SAGen_Est, SANonGen_Est, Fratio_Gen, 
#' pvalue_Gen, Fratio_NonGen, pvalue_NonGen, IndicGen, IndicNonGen
#' 
#' @export 
#'
#' @examples
#'simul_powertest_nongenet(seed = 1, SAng = 0.32)


simul_powertest_nongenet <- function(seed = 1, SAng = 0.32){
  
  set.seed(seed)
  
  ##### MODEL 
  ## Model to extract value of our dataset
  m <- lme4::lmer(asin(sqrt(Rate)) ~ SA + SA*IndicG0 + Test_environment:Generation  +
                    (1|Population:Test_environment) + 
                    (0 + lme4::dummy(Generation, "G0")|Population:Test_environment),
                  data = data_PERF_Rate)
  #summary(m)
  
  #Extract SA value for later
  indic <- grep("SA",names(lme4::fixef(m)))
  SAcoef <- lme4::fixef(m)[indic]
  names(SAcoef) <- c("SAGen", "SANonGen")
  
  #Extract mean and variance effects
  fixef_m <- lme4::fixef(m)  #Mean with the fixed effects
  random_m <- data.frame(lme4::VarCorr(m))$vcov #var with the random effect
  
  ## Replace non genetic SA
  fixef_m[names(fixef_m)=="SA1:IndicG0"] <- SAng 
  
  
  ##### SIMULATED DATASTE 
  ## New dataset for the simulated data
  sim_data <- data_PERF_Rate
  
  
  ## Design matrix for the simulated data
  M1 <- model.matrix(~ SA + SA*IndicG0 + Test_environment:Generation,
                     data = sim_data)
  # dim(M1)
  # dim(sim_data)
  # head(M1)
  # head(sim_data)
  
  ##### FIXED EFFECT
  ## Compute the breeding value of each individual in the new dataset
  #For 2 columns dropping problem:
  M1 <- M1[, colnames(M1)%in%names(fixef_m)] 
  # dim(M1)
  # head(M1)
  
  #Extract fixef(m)
  sim_data$FixEff <- M1%*%fixef_m
  
  #Check: correspond to do a predict without considered ramdom effects: 
  #data.frame(M1%*%fixef_m,predict(m, re.form = NA))
  
  
  
  
  ##### RANDOM EFFECT
  # Add variation Population:Test_environment
  sim_data$RandomEff1 <-  as.factor(paste(sim_data$Population, 
                                          sim_data$Test_environment, sep = "_"))
  levels(sim_data$RandomEff1) <- rnorm(nlevels(sim_data$RandomEff1), 0, sd = sqrt(random_m[1]))
  sim_data$RandomEff1 <- as.numeric(as.character(sim_data$RandomEff1))
  
  
  # Add variation Population:Test_environment:G0
  sim_data$RandomEff2 <-  as.factor(paste(sim_data$Population, 
                                          sim_data$Test_environment,
                                          sim_data$Generation, sep = "_"))
  levels(sim_data$RandomEff2) <- rnorm(nlevels(sim_data$RandomEff2), 0, sd = sqrt(random_m[2]))
  sim_data$RandomEff2 <- as.numeric(as.character(sim_data$RandomEff2))
  ## When generation G2, replace RandomEff2 by 0
  sim_data$RandomEff2 <- ifelse(sim_data$Generation == "G2", 0, sim_data$RandomEff2)
  
  
  
  ##### ERROR TERM
  ## Compute the random environmental error
  varres <- sigma(m)
  sim_data$ResEff <- rnorm(n=nrow(sim_data), mean = 0, sd = varres)
  
  
  ##### FITNESS
  ## Compute the fitness total
  sim_data$y <- sim_data$FixEff + sim_data$RandomEff1 + sim_data$RandomEff2 + sim_data$ResEff
  
  
  ##### MODEL WITH SIMULATED DATA
  msim <- lme4::lmer(y ~ SA + SA*IndicG0 + Test_environment:Generation  +
                       (1|Population:Test_environment) +
                       (0 + lme4::dummy(Generation, "G0")|Population:Test_environment),
                     data = sim_data)
  
  # summary(msim) 
  # summary(m) 
  
  indic <- grep("SA",names(lme4::fixef(msim)))
  SAcoef_Est <- lme4::fixef(msim)[indic]
  names(SAcoef_Est) <- c("SAGen_Est", "SANonGen_Est")
  
  
  ##### TEST LOCAL ADATPATION
  m2 <- aov(y ~ pop_gen + hab_gen + SA:IndicG0 + SA +
              Original_environment:Test_environment + 
              Original_environment:Test_environment:IndicG0, 
            data = sim_data)
  
  ## F test for SA
  Fratio_Gen <- (anova(m2)[3,2]/anova(m2)[5,2])/(1/anova(m2)[5, 1])
  pvalue_Gen <- 1 - pf(Fratio_Gen, 1, anova(m2)[5, 1]) 
  
  
  ## F test for SA
  Fratio_NonGen <- (anova(m2)[4,2]/anova(m2)[6,2])/(1/anova(m2)[6, 1])
  pvalue_NonGen <- 1 - pf(Fratio_NonGen, 1, anova(m2)[6, 1]) 
  
  
  # Add indic pvalue_NonGen and pvalue_Gen sign
  IndicGen <- ifelse(pvalue_Gen<0.05, 1, 0)
  IndicNonGen <- ifelse(pvalue_NonGen<0.05, 1, 0)
  
  
  
  return(c(seed=seed, SAng = SAng, 
    SAcoef["SAGen"], SAcoef["SANonGen"],
    SAcoef_Est["SAGen_Est"], SAcoef_Est["SANonGen_Est"], 
    Fratio_Gen = Fratio_Gen, pvalue_Gen = pvalue_Gen,
    Fratio_NonGen = Fratio_NonGen, pvalue_NonGen = pvalue_NonGen, 
    IndicGen = IndicGen, IndicNonGen = IndicNonGen))
  
}

  