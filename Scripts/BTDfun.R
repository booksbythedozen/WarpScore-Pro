BTDfun <- function(TestScore, type)  # TestScore = name of test score of interest; type = left- or right-tailed test
{
  # Create dataframe of normative data for test score of interest
  NormData <- subset(dsNorms, Score==TestScore)
  
  # Identify patient score
  if (!(TestScore %in% names(PatientData))) {
    message("BTD scoring function not executed: test score '",
            TestScore, "' is absent from patient data.")
    return(invisible(NULL))
  }
  
  CaseScore <- PatientData[[TestScore]]
  
  # Verify that the score is not missing
  if (is.na(CaseScore)) {
    message("BTD scoring function not executed due to missing patient data: ",
            TestScore)
    return(invisible(NULL))
  }
  
  Norm_mean <- NormData$Nmean
  Norm_sd   <- NormData$Nsd
  Norm_n    <- NormData$Nn
  
  # Assign Bayesian TD htest list object
  BTD.htest <- BTD(case = CaseScore, 
                   controls = Norm_mean,
                   sd = Norm_sd,
                   sample_size = Norm_n,
                   alternative = type, 
                   int_level = 0.95, 
                   iter = 10000)
  
  df <- BTD.htest[[1]]        # degrees of freedom for t-test (normative sample size minus number of covariates)
  p.value <- BTD.htest[[2]]*100   # point-estimate of posterior probability
  L.PI <- BTD.htest[[5]][[4]] # lower-bound of 95% percentile interval for posterior probability estimate
  H.PI <- BTD.htest[[5]][[5]] # upper-bound of 95% percentile interval for posterior probability estimate
  zscore <- (CaseScore - Norm_mean) / Norm_sd
  # Reverse the direction when higher raw scores indicate poorer performance
  if (type == "greater") {
    zscore <- -zscore
  }
  PR <- pnorm(zscore)*100
  Input <- BTD.htest[[9]]
  BTDagg <- data.frame(TestScore, # Aggregate into a dataframe and assign to global environment object with "<<-"
                       df,
                       p.value,
                       L.PI,
                       H.PI,
                       zscore,
                       PR,
                       Input)
  BTDdf <<- rbind(BTDdf, BTDagg) # appends output data to compile results table
  # example of use: BTDfun("TrailsA", "greater") # where "greater" means higher raw score represents worse performance
}