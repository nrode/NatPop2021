#' Simulate fitness and calculate SA with poisson distribution 
#'
#' @description Simulate fitness data: poisson distribution 
#' @param seed Seed used for simulation
#' @param npop Number of populations
#' @param nhab Number of habitat
#' @param nrep Number of replicates per populationxhabitat
#' @param sdpophab Standard error for the variance of the genetic interaction PopxHabitat 
#' @param sdpophab_ng Standard error for the variance of the non-genetic interaction PopxHabitat 
#' @param sigma Residual variance across replicates
#' @param rho Genetic covariance (from which emerges SA genetic)
#' @param rho_ng Non genetic covariance (from which emerges SA non genetic)
#' @param constant Should we add a constant factor? 
#' 
#' @return vector seed, SA_Gen_True, SA_NonGen_True, SAcoef, rho, rho_ng, Fratio_Gen, 
#' pvalue_Gen, Fratio_NonGen, pvalue_NonGen, Fratio_Gen_aov, pvalue_Gen_aov, Fratio_NonGen_aov, pvalue_NonGen_aov
#' @export 
#'
#' @examples
#'simul_fitnessdata_poisson(seed = 1, npop = 3, nhab = 3, nrep = 5, sdpophab = 1, sdpophab_ng = 1, 
#'sigma = 0.5, rho=-1/(nhab-1), rho_ng=-1/(nhab-1), sdpophab_ng = 1, sigma = 0.5, constant = 0)


simul_fitnessdata_poisson <- function(seed = 1, npop = 3, nhab = 3, nrep = 5, sdpophab = 1, 
                                     rho = -1/(nhab-1), rho_ng = -1/(nhab-1), sdpophab_ng = 1, 
                                     sigma = 0.5, constant = 0){
  
  
  ## Dataset
  data <- expand.grid(Ind=as.character(1:nrep),
                      Hab=as.character(1:nhab), 
                      Pop=as.character(1:npop), 
                      Gen=c("G0", "G2"))
  data$SA<-as.factor(ifelse(data$Pop==data$Hab, 1, 0))
  data$IndicG0<-as.numeric(ifelse (as.character(data$Gen)=="G0",1,0))
  data$IndicG2<-as.numeric(ifelse (as.character(data$Gen)=="G2",1,0))
  
  data$SAIndicG0<-as.numeric(ifelse (as.character(data$Gen)=="G0"&
                                       (as.character(data$SA)=="1"),1,0))
  
  
  data$pop_hab <- as.factor(paste(data$Pop, data$Hab, sep="_"))
  data$pop_hab_ng <- as.factor(paste(data$Pop, data$Hab, data$Gen, sep="_"))
  
  data$pop_gen <- as.factor(paste(data$Pop, data$Gen, sep="_"))
  data$hab_gen <- as.factor(paste(data$Hab, data$Gen, sep="_"))
  
  ## Sample genetic interaction Pop x habitat
  set.seed(seed)
  mat_pophab <- matrix(rho*sdpophab*sdpophab, npop, nhab)
  diag(mat_pophab) <- sdpophab*sdpophab
  ## Variance-covariance matrix between genetic pop x hab effects
  PopHabEff <- MASS::mvrnorm(n=nhab, mu=rep(0, nhab), Sigma=mat_pophab)
  
  if (rho<0&sdpophab!=0) {
    for (i in 1:npop){
      maxPopHab_Eff <- max(PopHabEff[i,])
      PopHabEff[i,-i] <- PopHabEff[i,][PopHabEff[i,]!=maxPopHab_Eff]
      PopHabEff[i,i] <- maxPopHab_Eff
      
    }
  }
  
  Symp <- diag(PopHabEff)
  Allo <- PopHabEff[row(PopHabEff)!=col(PopHabEff)]
  SA_Gen_True <- mean(Symp)-mean(Allo)
  
  ## Melt dataset
  pophab <- reshape::melt(PopHabEff)
  names(pophab) <- c("pop", "hab", "GenEff")
  pophab$pop_hab <- as.factor(paste(pophab$pop, pophab$hab, sep="_"))
  
  data <- merge(x=data, y=pophab, by="pop_hab")
  
  ## Sample non genetic interaction Pop x habitat
  mat_pophab_ng <- matrix(rho_ng*sdpophab_ng*sdpophab_ng, npop, nhab)
  diag(mat_pophab_ng) <- sdpophab_ng*sdpophab_ng
  ## Variance-covariance matrix between non genetic pop x hab effects
  PopHab_ngEff <- MASS::mvrnorm(n=nhab, mu=rep(0, nhab), Sigma=mat_pophab_ng)
  
  if (rho_ng<0&sdpophab_ng!=0) {
    for (i in 1:npop){
      maxPopHab_ngEff <- max(PopHab_ngEff[i,])
      PopHab_ngEff[i,-i] <- PopHab_ngEff[i,][PopHab_ngEff[i,]!=maxPopHab_ngEff]
      PopHab_ngEff[i,i] <- maxPopHab_ngEff
      
    }
  }
  Symp <- diag(PopHab_ngEff)
  Allo <- PopHab_ngEff[row(PopHab_ngEff)!=col(PopHab_ngEff)]
  SA_NonGen_True <- mean(Symp)-mean(Allo)
  
  ## Melt dataset
  pophab_ng <- reshape::melt(PopHab_ngEff)
  names(pophab_ng) <- c("pop", "hab", "NonGenEff")
  pophab_ng$pop_hab_ng <- as.factor(paste(pophab_ng$pop, pophab_ng$hab, "G0", sep="_"))
  
  
  data <- merge(x=data, y=pophab_ng[,3:4], by="pop_hab_ng", all.x=TRUE)
  ## When generation G2, replace NA by 0
  data$NonGenEff <- ifelse(is.na(data$NonGenEff), 0, data$NonGenEff)
  
  data$fitness <- rpois(n=nrow(data), lambda = exp(constant + data$GenEff + data$NonGenEff))
  #tapply(data$fitness, list(data$Pop, data$Hab, data$Gen), mean)
  
  ##################################################
  ## Estimate genetic and non-genetic SA ###
  ##################################################
  
  m00 <- lm(log(fitness +1)~pop_gen+hab_gen+SA+SAIndicG0, data=data)
  indic <- grep("SA",names(coef(m00)))
  
  SAcoef <- coef(m00)[indic]
  names(SAcoef) <- c("SAGen_Est", "SANonGen_Est")
  
  #######################################################
  ## Analysis of genetic effects lmer ###
  #######################################################
  ## Model to get the df of the interaction
  m_perGen <- lm(log(fitness +1)~pop_gen+hab_gen+Pop:Hab:IndicG0+Pop:Hab:IndicG2+SA:IndicG0+SA, data=data)
  
  m00 <- lme4::lmer(log(fitness +1)~Pop+Hab+SA+(1|Pop:Hab), data=data)
  
  
  indic <- grep("SA",names(lme4::fixef(m00)))
  
  Fratio_Gen = as.numeric(lme4::fixef(m00)[indic[1]]^2/vcov(m00)[indic[1],indic[1]])
  pvalue_Gen = 1 - pf(Fratio_Gen, 1, anova(m_perGen)[5, 1])
  
  #######################################################
  ## Analysis of genetic and non-genetic effects lmer ###
  #######################################################
  ## Model to get the df of the interaction
  m_perGen <- lm(log(fitness +1)~pop_gen+hab_gen+Pop:Hab:IndicG0+Pop:Hab+SA:IndicG0+SA, data=data)
  
  m0 <- lme4::lmer(log(fitness +1)~pop_gen+hab_gen+SA+SAIndicG0+(1|Pop:Hab)+
                     (0+lme4::dummy(Gen, "G0")|Pop:Hab), data=data)
  
  indic <- grep("SA",names(lme4::fixef(m0)))
  
  Fratio_Gen = as.numeric(lme4::fixef(m0)[indic[1]]^2/vcov(m0)[indic[1],indic[1]])
  pvalue_Gen = 1 - pf(Fratio_Gen, 1, anova(m_perGen)[4, 1])
  
  Fratio_NonGen = as.numeric(lme4::fixef(m0)[indic[2]]^2/vcov(m0)[indic[2],indic[2]])
  pvalue_NonGen = 1 - pf(Fratio_NonGen, 1, anova(m_perGen)[6, 1])
  
  #######################################################
  ## Analysis of genetic effects lm   ###
  #######################################################
  
  m1 <- aov(log(fitness +1)~pop_gen+hab_gen+SA+Pop:Hab, contrasts=list(Pop="contr.sum", Hab="contr.sum"), data=data)
  
  ## F test for SA
  Fratio = (anova(m1)[3,2]/anova(m1)[4,2])/(1/anova(m1)[4,1])
  pvalue = 1 - pf(Fratio, 1, anova(m1)[4,1]) #the correct test (see equation D7 in Appendix D of the paper)
  
  #######################################################
  ## Analysis of genetic and non-genetic effects lm   ###
  #######################################################
  data$pop_hab_G0 <- as.factor(ifelse(data$Gen=="G0", paste(data$Pop, data$Hab, sep="_"), 0))
  m2 <- aov(log(fitness +1)~pop_gen+hab_gen+SA:IndicG0+SA+Pop:Hab+Pop:Hab:IndicG0, data=data)
  
  ## F test for SA
  Fratio_Gen_aov = (anova(m2)[3,2]/anova(m2)[5,2])/(1/anova(m2)[5,1])
  pvalue_Gen_aov = 1 - pf(Fratio_Gen_aov, 1, anova(m2)[5,1]) #the correct test (see equation D7 in Appendix D of the paper)
  
  ## F test for SA
  Fratio_NonGen_aov = (anova(m2)[4,2]/anova(m2)[6,2])/(1/anova(m2)[6,1])
  pvalue_NonGen_aov = 1 - pf(Fratio_NonGen_aov, 1, anova(m2)[6,1]) #the correct test (see equation D7 in Appendix D of the paper)
  
  return(c(seed=seed, SA_Gen_True=SA_Gen_True, SA_NonGen_True=SA_NonGen_True, 
           SAcoef, rho=rho, rho_ng=rho_ng, Fratio_Gen=Fratio_Gen, pvalue_Gen=pvalue_Gen,
           Fratio_NonGen=Fratio_NonGen, pvalue_NonGen=pvalue_NonGen, Fratio_Gen_aov=Fratio_Gen_aov,
           pvalue_Gen_aov=pvalue_Gen_aov, Fratio_NonGen_aov=Fratio_NonGen_aov, 
           pvalue_NonGen_aov=pvalue_NonGen_aov))
  
}