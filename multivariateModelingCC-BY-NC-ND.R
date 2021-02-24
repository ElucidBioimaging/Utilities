# Copyright (c) Elucid Bioimaging
# This work is licensed under a Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
# <a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/">Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License</a>.
lib_loc <- '~/R/lib64/R/library'
if (!require('readr')) install.packages('readr', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('tidyr')) install.packages('tidyr', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('rstudioapi')) install.packages('rstudioapi', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('withr')) install.packages('withr', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('doBy')) install.packages('doBy', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('ggplot2')) install.packages('ggplot2', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('xtable')) install.packages('xtable', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('tidyverse')) install.packages('tidyverse', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('caret')) install.packages('caret', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('recipes')) install.packages('recipes', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('broom')) install.packages('broom', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('ComplexHeatmap')) install.packages('BiocManager', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('ComplexHeatmap')) BiocManager::install("ComplexHeatmap", lib=lib_loc)
if (!require('C50')) install.packages('C50', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('elasticnet')) install.packages('elasticnet', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('e1071')) install.packages('e1071', repos="https://cran.r-project.org", lib=lib_loc)
if (!require('pROC')) install.packages('pROC', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('plotROC')) install.packages('plotROC', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('glmnet')) install.packages('glmnet', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('dplyr')) install.packages('dplyr', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('yardstick')) install.packages('yardstick', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('RColorBrewer')) install.packages('RColorBrewer', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('lubridate')) install.packages('lubridate', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('MASS')) install.packages('MASS', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('ModelMetrics')) install.packages('ModelMetrics', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('dendextend')) install.packages('dendextend', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)
if (!require('pls')) install.packages('pls', repos="http://cran.r-project.org", lib=lib_loc, dependencies=TRUE)

library(readr,lib=lib_loc)
library(tidyr,lib=lib_loc)
library(rstudioapi,lib=lib_loc)
library(withr,lib=lib_loc)
library(doBy,lib=lib_loc)
library(ggplot2,lib=lib_loc)
library(xtable,lib=lib_loc)
library(tidyverse,lib=lib_loc)
library(caret,lib=lib_loc)
library(recipes,lib=lib_loc)
library(broom,lib=lib_loc)
library(ComplexHeatmap,lib=lib_loc)
library(C50,lib=lib_loc)
library(elasticnet,lib=lib_loc)
library(e1071,lib=lib_loc)
library(pROC,lib=lib_loc)
library(plotROC,lib=lib_loc)
library(glmnet,lib=lib_loc)
library(dplyr,lib=lib_loc)
library(yardstick,lib=lib_loc)
library(RColorBrewer,lib=lib_loc)
library(lubridate,lib=lib_loc)
library(MASS,lib=lib_loc)
library(ModelMetrics,lib=lib_loc)
library(dendextend,lib=lib_loc)
library(pls,lib=lib_loc)

theme_set(theme_gray() + theme(legend.position="top",
                               axis.title.x=element_text(size=10), axis.text.x=element_text(size=10),
                               axis.title.y=element_text(size=10), axis.text.y=element_text(size=10)))

#### utility functions
pct_chg <- function(x) { 100*((x-lag(x))/lag(x)) }
baseline <- function(x) { lag(x) }
follow_up <- function(x) { x }
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
medianWithoutNA <- function(x) { median(x[which(!is.na(x))]) }
format_p.value_str <- function(p.value) { ifelse(p.value > 0.05, paste0("P=", format(round(p.value, 2), nsmall=2)), 
                                                 ifelse(p.value <= 0.05 & p.value >= 0.01, paste0("*P=", format(round(p.value, 2), nsmall=2)), 
                                                        ifelse(p.value < 0.01 & p.value >= 0.001, "**P<0.01", 
                                                               ifelse(p.value < 0.001 & p.value >= 0.0001, "***P<0.001", "****P<0.0001")))) }

custom_summary <- function(data, lev=NULL, model=NULL) {
  a <- defaultSummary(data, lev, model)
  CCCobj <- yardstick::ccc_vec(data[, "pred"], data[, "obs"])
  out <- c(a, CCCobj)
  names(out)[4] <- c("CCC")
  out
}

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

save_tuning_and_varImp <- function(resp_mod, processedSet, modeledSet, modelType)
{
  params <- resp_mod$modelInfo$parameters$parameter
  paramData <- resp_mod$modelInfo$parameters
  plotIt <- "yes" # default
  if (all(params == "parameter")) {
    plotIt <- "There are no tuning parameters for this model."
  } else {
    dat <- resp_mod$results
    
    ## Check to see which tuning parameters were varied
    #       params is a factor, so just using params does not work properly when model metric is not the first column in dat
    #           e.g. oob resampling
    paramValues <- apply(dat[,as.character(params),drop = FALSE], 2, function(x) length(unique(x)))
    if (any(paramValues <= 1)) {
       plotIt <- "There are no tuning parameters with more than 1 value."
     }
  }

  if (plotIt == "yes") {
    ggplot(resp_mod) +
      ggtitle(paste0(processedSet$response, " by ", modeledSet$setLabel)) + theme(plot.title=element_text(size=12, face="bold", hjust=0.5))
    ggsave(file.path(modeledSet$outLoc, paste(processedSet$response, "_", modeledSet$setLabel, "_", modelType, "_tuning.png", sep='')),  width=4, height=4)
  }

  varImp_ <- try(varImp(resp_mod), silent=TRUE)
  if (class(varImp_)[1] != "try-error") {
    ggplot(varImp_, top=10) +
      ggtitle(paste0(processedSet$response, " by ", modeledSet$setLabel)) + theme(plot.title=element_text(size=12, face="bold", hjust=0.5))
    ggsave(file.path(modeledSet$outLoc, paste(processedSet$response, "_", modeledSet$setLabel, "_", modelType, "_varImp.png", sep='')),  width=4, height=4)
    return(TRUE)
  }
  return(FALSE)
}

save_roc_plot <- function(dataSet, ROC, Sens, Spec, Kappa, modelTypeStr, titleText, filePath)
{
  ggplot(data=dataSet, aes(d=obs, m=Y)) +
    geom_roc(labels=FALSE, size.point=0, color="darkblue") +
    annotate("text", x=0.65, y=0.35,
             label=(paste0("AUC=", round(ROC, 2), "\nSensitivity=", round(Sens, 2), "\nSpecificity=", round(Spec, 2), "\nKappa=", round(Kappa, 2)
                          )), color="#707070", hjust=0, vjust=1) + 
    annotate("text", x=0.03, y=0.95, label=modelTypeStr, color="#707070", fontface="italic", hjust=0, vjust=0) +
    geom_abline(intercept=0, slope=1, col="grey", lty=2) +
    xlab("1 - Specificity") + ylab("Sensitivity") +
    theme(legend.position="none") +
    ggtitle(titleText) + theme(plot.title=element_text(size=12, face="bold", hjust=0.5))
  ggsave(filePath,  width=4, height=4)
  
}

save_xy_plot <- function(dataSet, RMSE_pt_estimate, Rsquared_pt_estimate, CCC_pt_estimate,
                         bias_pt_estimate, slope_pt_estimate, intercept_pt_estimate, 
                         plot_range, modelTypeStr, titleText, filePath)
{
  ggplot(data=dataSet, aes(x=obs, y=pred)) + geom_point(color="darkblue") + xlim(plot_range) + ylim(plot_range) + 
    coord_fixed(ratio=1) + geom_abline(intercept=0, slope=1, color="darkgrey", linetype="dashed") + stat_smooth(method=lm, se=FALSE) + 
    annotate("text", x=plot_range[1], y=plot_range[2],
             label=(paste0("RMSE=", round(RMSE_pt_estimate, 2), "\nR^2=", round(Rsquared_pt_estimate, 2), "\nCCC=", round(CCC_pt_estimate, 2), 
                           "\nbias=", round(bias_pt_estimate, 2), "\nslope=", round(slope_pt_estimate, 2), "\nintercept=", round(intercept_pt_estimate, 2)
                          )), color="#707070", hjust=0, vjust=1) + 
    annotate("text", x=plot_range[2], y=plot_range[1], label=modelTypeStr, color="#707070", fontface="italic", hjust=1, vjust=0) +
    xlab("Observed") + ylab("Predicted") +
    ggtitle(titleText) + theme(plot.title=element_text(size=12, face="bold", hjust=0.5))
  ggsave(filePath, width=4, height=4)
}

toOrig <- function(unscaledEst, postScaling, postOffset) { return((unscaledEst*postScaling)+postOffset) }

####
# post-processing for models estimating categoric response variables
####
multivariateModeling_catPost <- function(cat_resp_mod, processedSet, modeledSet, modelType, metricForOptimization, maximizeForOptimization, bestTuning, 
                                         postScaling, postOffset, preProc_recipe_for_subset=NULL)
{
  modelTypeStr <- ifelse(length(c(processedSet$predictors_cat, processedSet$predictors_cont)) <= 1, "", modelType)

  # first get FullKey into bestTuning
  full_keys_w_index <- processedSet$FullKeys %>% dplyr::mutate(rowIndex=row_number()) %>% dplyr::select(FullKey, rowIndex)
  bestTuning <- full_join(bestTuning, full_keys_w_index, by="rowIndex") %>% dplyr::select(-rowIndex)
  
  # plot observed vs. predicted
  modeledSet$outputs[[modelType]]$predictions <- bestTuning %>% dplyr::select(pred, obs, FullKey, N, Y)
  modeledSet$outputs[[modelType]]$predictions <- full_join(modeledSet$outputs[[modelType]]$predictions, processedSet$FullKeys, by=c("FullKey"))
  modeledSet$forBadSampleAnalysis_cat <- list_merge(modeledSet$forBadSampleAnalysis_cat, modelType)
  
  # get performance summary for best feature subset
  num_resamples <- nrow(cat_resp_mod$resample) # counting how many resamples there are for calculation of the CI
  modeledSet$outputs[[modelType]]$perf_summary <- 
    as_tibble(cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]) %>%
    dplyr::mutate(outLoc=modeledSet$outLoc) %>%
    dplyr::mutate(response=processedSet$response) %>%
    dplyr::mutate(predictorSet=processedSet$setLabel) %>%
    dplyr::mutate(modelType=modelType) %>%

    dplyr::mutate(ROCwidth=abs(qt(0.025, num_resamples-1))*(cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$ROCSD)/
                           sqrt(num_resamples)) %>% 
    dplyr::mutate(ci_lb_ROC=cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$ROC - ROCwidth) %>%
    dplyr::mutate(ci_lb_ROC=ifelse(ci_lb_ROC < 0, 0, ci_lb_ROC)) %>%
    dplyr::mutate(ci_ub_ROC=cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$ROC + ROCwidth) %>%
    dplyr::mutate(ci_ub_ROC=ifelse(ci_ub_ROC > 1, 1, ci_ub_ROC)) %>%
    dplyr::mutate(ci_ROC=paste0("[",round(ci_lb_ROC,2),",",round(ci_ub_ROC,2),"]")) %>%
    
    dplyr::mutate(Senswidth=abs(qt(0.025, num_resamples-1))*(cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$SensSD)/
                            sqrt(num_resamples)) %>% 
    dplyr::mutate(ci_lb_Sens=cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$Sens - Senswidth) %>%
    dplyr::mutate(ci_lb_Sens=ifelse(ci_lb_Sens < 0, 0, ci_lb_Sens)) %>%
    dplyr::mutate(ci_ub_Sens=cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$Sens + Senswidth) %>%
    dplyr::mutate(ci_ub_Sens=ifelse(ci_ub_Sens > 1, 1, ci_ub_Sens)) %>%
    dplyr::mutate(ci_Sens=paste0("[",round(ci_lb_Sens,2),",",round(ci_ub_Sens,2),"]")) %>%

    dplyr::mutate(Specwidth=abs(qt(0.025, num_resamples-1))*(cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$SpecSD)/
                            sqrt(num_resamples)) %>% 
    dplyr::mutate(ci_lb_Spec=cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$Spec - Specwidth) %>%
    dplyr::mutate(ci_lb_Spec=ifelse(ci_lb_Spec < 0, 0, ci_lb_Spec)) %>%
    dplyr::mutate(ci_ub_Spec=cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$Spec + Specwidth) %>%
    dplyr::mutate(ci_ub_Spec=ifelse(ci_ub_Spec > 1, 1, ci_ub_Spec)) %>%
    dplyr::mutate(ci_Spec=paste0("[",round(ci_lb_Spec,2),",",round(ci_ub_Spec,2),"]")) %>%

    dplyr::mutate(Kappawidth=abs(qt(0.025, num_resamples-1))*(cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$KappaSD)/
                             sqrt(num_resamples)) %>% 
    dplyr::mutate(ci_lb_Kappa=cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$Kappa - Kappawidth) %>%
    dplyr::mutate(ci_lb_Kappa=ifelse(ci_lb_Kappa < 0, 0, ci_lb_Kappa)) %>%
    dplyr::mutate(ci_ub_Kappa=cat_resp_mod$results[best(cat_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$Kappa + Kappawidth) %>%
    dplyr::mutate(ci_ub_Kappa=ifelse(ci_ub_Kappa > 1, 1, ci_ub_Kappa)) %>%
    dplyr::mutate(ci_Kappa=paste0("[",round(ci_lb_Kappa,2),",",round(ci_ub_Kappa,2),"]")) %>%

    dplyr::select(outLoc, response, predictorSet, modelType, ROC, ci_ROC, Sens, ci_Sens, Spec, ci_Spec, Kappa, ci_Kappa)

  prediction_subset <- modeledSet$outputs[[modelType]]$predictions %>% dplyr::select(obs, pred)
  
  ROC_pt_estimate <- modeledSet$outputs[[modelType]]$perf_summary$ROC        
  Sens_pt_estimate <- modeledSet$outputs[[modelType]]$perf_summary$Sens        
  Spec_pt_estimate <- modeledSet$outputs[[modelType]]$perf_summary$Spec        
  Kappa_pt_estimate <- modeledSet$outputs[[modelType]]$perf_summary$Kappa        
  save_roc_plot(modeledSet$outputs[[modelType]]$predictions, 
                ROC_pt_estimate, Sens_pt_estimate, Spec_pt_estimate, Kappa_pt_estimate,
                modelTypeStr,
                paste0(processedSet$response, " by ", modeledSet$setLabel),
                file.path(modeledSet$outLoc, paste(processedSet$response, "_", modeledSet$setLabel, "_", modelType, "_roc.png", sep='')))
  
  # also save the points themselves
  write_csv(cat_resp_mod$pred, path=file.path(modeledSet$outLoc, paste(processedSet$response, "_", modeledSet$setLabel, "_", modelType, "_DEV.csv", sep='')))

  modeledSet$outputs[[modelType]]$perf_summary$ROC <- paste(round(ROC_pt_estimate, 2), modeledSet$outputs[[modelType]]$perf_summary$ci_ROC)
  modeledSet$outputs[[modelType]]$perf_summary$ci_ROC <- NULL

  modeledSet$outputs[[modelType]]$perf_summary$Sens <- paste(round(Sens_pt_estimate, 2), modeledSet$outputs[[modelType]]$perf_summary$ci_Sens)
  modeledSet$outputs[[modelType]]$perf_summary$ci_Sens <- NULL

  modeledSet$outputs[[modelType]]$perf_summary$Spec <- paste(round(Spec_pt_estimate, 2), modeledSet$outputs[[modelType]]$perf_summary$ci_Spec)
  modeledSet$outputs[[modelType]]$perf_summary$ci_Spec <- NULL

  modeledSet$outputs[[modelType]]$perf_summary$Kappa <- paste(round(Kappa_pt_estimate, 2), modeledSet$outputs[[modelType]]$perf_summary$ci_Kappa)
  modeledSet$outputs[[modelType]]$perf_summary$ci_Kappa <- NULL

  write_csv(modeledSet$outputs[[modelType]]$perf_summary, path=file.path(modeledSet$outLoc, paste(processedSet$response, "_", modeledSet$setLabel, "_", modelType, "_perf.csv", sep='')))

  # now see if this model improved the prior best, and if so, update the data structure accordingly
  if (maximizeForOptimization) {
    if (ROC_pt_estimate > modeledSet$best_$currentBest) {
      modeledSet$best_$model <- cat_resp_mod
      modeledSet$best_$currentBest <- ROC_pt_estimate
      modeledSet$best_$predictions <- modeledSet$outputs[[modelType]]$predictions
      modeledSet$best_$modelType <- modelType
      modeledSet$best_$modelTypeStr <- modelTypeStr
      modeledSet$best_$postScaling <- postScaling
      modeledSet$best_$postOffset <- postOffset
      modeledSet$best_$preProc_recipe_for_subset <- preProc_recipe_for_subset # only used for rfeLR
    }
  } else {
    #THIS BRANCH IS NOT FULLY OPERATIONAL NOW (the per summary is now a string, because it is adorned with the CI. Need to have a pt estimate, but for a flexible metric.)
    if (modeledSet$outputs[[modelType]]$perf_summary[[metricForOptimization]] < modeledSet$best_$currentBest) {
      modeledSet$best_$model <- cat_resp_mod
      modeledSet$best_$currentBest <- modeledSet$outputs[[modelType]]$perf_summary[[metricForOptimization]]
      modeledSet$best_$predictions <- modeledSet$outputs[[modelType]]$predictions
      modeledSet$best_$modelType <- modelType
      modeledSet$best_$modelTypeStr <- modelTypeStr
      modeledSet$best_$postScaling <- postScaling
      modeledSet$best_$postOffset <- postOffset
      modeledSet$best_$preProc_recipe_for_subset <- preProc_recipe_for_subset # only used for rfeLR
    }
  }
  
  return(modeledSet)
}

####
# post-processing for models estimating continuous response variables.
####
multivariateModeling_contPost <- function(cont_resp_mod, processedSet, modeledSet, modelType, metricForOptimization, maximizeForOptimization, bestTuning, 
                                          postScaling, postOffset, preProc_recipe_for_subset=NULL)
{
  modelTypeStr <- ifelse(length(c(processedSet$predictors_cat, processedSet$predictors_cont)) <= 1, "", modelType)

  # first get FullKey into bestTuning
  full_keys_w_index <- processedSet$FullKeys %>% dplyr::mutate(rowIndex=row_number()) %>% dplyr::select(FullKey, rowIndex)
  bestTuning <- full_join(bestTuning, full_keys_w_index, by="rowIndex") %>% dplyr::select(-rowIndex)
  
  # plot observed vs. predicted
  modeledSet$outputs[[modelType]]$predictions <- bestTuning %>%
                                                 dplyr::group_by(obs, FullKey) %>%
                                                 dplyr::summarize(pred=mean(pred, na.rm=TRUE)) %>%
                                                 dplyr::select(pred, obs, FullKey)
  modeledSet$outputs[[modelType]]$predictions <- full_join(modeledSet$outputs[[modelType]]$predictions, processedSet$FullKeys, by=c("FullKey"))
  modeledSet$forBadSampleAnalysis_cont <- list_merge(modeledSet$forBadSampleAnalysis_cont, modelType)
  for_continuous_summary <- modeledSet$outputs[[modelType]]$predictions %>% dplyr::mutate(bias=pred - obs)
  mean_bias <- mean(for_continuous_summary$bias, na.rm=TRUE)
  mean_bias_resample <- bestTuning %>%
                        dplyr::mutate(bias=pred-obs) %>%
                        dplyr::group_by(Resample) %>%
                        dplyr::summarize(mean_bias=mean(bias, na.rm=TRUE))
  sd_bias_resample <- sd(mean_bias_resample$mean_bias, na.rm=TRUE)
  lm_summary <- lm(pred ~ obs, data=for_continuous_summary)
  lm_ci <- confint(lm_summary)
  
  # get performance summary for best feature subset
  num_resamples <- nrow(cont_resp_mod$resample) # counting how many resamples there are for calculation of the CI
  modeledSet$outputs[[modelType]]$perf_summary <- 
    as_tibble(cont_resp_mod$results[best(cont_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]) %>%
    dplyr::mutate(outLoc=modeledSet$outLoc) %>%
    dplyr::mutate(response=processedSet$response) %>%
    dplyr::mutate(predictorSet=processedSet$setLabel) %>%
    dplyr::mutate(modelType=modelType) %>%

    dplyr::mutate(RMSEwidth=abs(qt(0.025, num_resamples-1))*toOrig(cont_resp_mod$results[best(cont_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$RMSESD, postScaling, postOffset)/
                            sqrt(num_resamples)) %>%
    dplyr::mutate(ci_lb_RMSE=cont_resp_mod$results[best(cont_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$RMSE - RMSEwidth) %>%
    dplyr::mutate(ci_lb_RMSE=ifelse(ci_lb_RMSE < 0, 0, ci_lb_RMSE)) %>%
    dplyr::mutate(ci_ub_RMSE=cont_resp_mod$results[best(cont_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$RMSE + RMSEwidth) %>%
    #dplyr::mutate(ci_ub_RMSE= ifelse(ci_ub_RMSE > 1, 1, ci_ub_RMSE)) %>%
    dplyr::mutate(ci_RMSE=paste0("[",round(ci_lb_RMSE,2),",",round(ci_ub_RMSE,2),"]")) %>%

    dplyr::mutate(Rsquaredwidth=abs(qt(0.025, num_resamples-1))*(cont_resp_mod$results[best(cont_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$RsquaredSD)/
                                sqrt(num_resamples)) %>%
    dplyr::mutate(ci_lb_Rsquared=cont_resp_mod$results[best(cont_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$Rsquared - Rsquaredwidth) %>%
    dplyr::mutate(ci_lb_Rsquared=ifelse(ci_lb_Rsquared < 0, 0, ci_lb_Rsquared)) %>%
    dplyr::mutate(ci_ub_Rsquared=cont_resp_mod$results[best(cont_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$Rsquared + Rsquaredwidth) %>%
    dplyr::mutate(ci_ub_Rsquared=ifelse(ci_ub_Rsquared > 1, 1, ci_ub_Rsquared)) %>%
    dplyr::mutate(ci_Rsquared=paste0("[",round(ci_lb_Rsquared,2),",",round(ci_ub_Rsquared,2),"]")) %>%

    dplyr::mutate(CCCwidth=abs(qt(0.025, num_resamples-1))*(cont_resp_mod$results[best(cont_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$CCCSD)/
                           sqrt(num_resamples)) %>%
    dplyr::mutate(ci_lb_CCC=cont_resp_mod$results[best(cont_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$CCC - CCCwidth) %>%
    dplyr::mutate(ci_lb_CCC=ifelse(ci_lb_CCC < 0, 0, ci_lb_CCC)) %>%
    dplyr::mutate(ci_ub_CCC=cont_resp_mod$results[best(cont_resp_mod$results, metric=metricForOptimization, maximize=maximizeForOptimization), ]$CCC + CCCwidth) %>%
    dplyr::mutate(ci_ub_CCC=ifelse(ci_ub_CCC > 1, 1, ci_ub_CCC)) %>%
    dplyr::mutate(ci_CCC=paste0("[",round(ci_lb_CCC,2),",",round(ci_ub_CCC,2),"]")) %>%

    dplyr::select(outLoc, response, predictorSet, modelType, RMSE, ci_RMSE, Rsquared, ci_Rsquared, CCC, ci_CCC)
  
  modeledSet$outputs[[modelType]]$perf_summary <- modeledSet$outputs[[modelType]]$perf_summary %>%
                                                  dplyr::mutate(bias=toOrig(mean_bias, postScaling, postOffset),
                                                                biaswidth=abs(qt(0.025, num_resamples-1))*toOrig(sd_bias_resample, postScaling, postOffset)/sqrt(num_resamples),
                                                                ci_lb_bias=bias-biaswidth,
                                                                ci_ub_bias=bias+biaswidth,
                                                                ci_bias=paste0("[",round(ci_lb_bias,2),",",round(ci_ub_bias,2),"]"),
                                                                slope=as.numeric(lm_summary$coefficients[2]),
                                                                ci_lb_slope=as.numeric(lm_ci[2,1]),
                                                                ci_ub_slope=as.numeric(lm_ci[2,2]),
                                                                ci_slope=paste0("[",round(ci_lb_slope,2),",",round(ci_ub_slope,2),"]"),
                                                                intercept=toOrig(as.numeric(lm_summary$coefficients[1]), postScaling, postOffset),
                                                                ci_lb_intercept=toOrig(as.numeric(lm_ci[1,1]), postScaling, postOffset),
                                                                ci_ub_intercept=toOrig(as.numeric(lm_ci[1,2]), postScaling, postOffset),
                                                                ci_intercept=paste0("[",round(ci_lb_intercept,2),",",round(ci_ub_intercept,2),"]"))
                                                              
  prediction_subset <- modeledSet$outputs[[modelType]]$predictions %>% dplyr::select(obs, pred)
  plot_range <- extendrange(prediction_subset)

  RMSE_pt_estimate <- toOrig(modeledSet$outputs[[modelType]]$perf_summary$RMSE, postScaling, postOffset)     
  Rsquared_pt_estimate <- modeledSet$outputs[[modelType]]$perf_summary$Rsquared        
  CCC_pt_estimate <- modeledSet$outputs[[modelType]]$perf_summary$CCC        
  bias_pt_estimate <- modeledSet$outputs[[modelType]]$perf_summary$bias        
  slope_pt_estimate <- modeledSet$outputs[[modelType]]$perf_summary$slope        
  intercept_pt_estimate <- modeledSet$outputs[[modelType]]$perf_summary$intercept        
  save_xy_plot(modeledSet$outputs[[modelType]]$predictions,
               RMSE_pt_estimate, Rsquared_pt_estimate, CCC_pt_estimate,
               bias_pt_estimate, slope_pt_estimate, intercept_pt_estimate,
               plot_range, modelTypeStr,  
               paste0(processedSet$response, " by ", modeledSet$setLabel),
               file.path(modeledSet$outLoc, paste(processedSet$response, "_", modeledSet$setLabel, "_", modelType, "_xy.png", sep='')))

  # also save the points themselves to an html table
  write_csv(cont_resp_mod$pred, path=file.path(modeledSet$outLoc, paste(processedSet$response, "_", modeledSet$setLabel, "_", modelType, "_DEV.csv", sep='')))

  modeledSet$outputs[[modelType]]$perf_summary$RMSE <- paste(round(RMSE_pt_estimate, 2), modeledSet$outputs[[modelType]]$perf_summary$ci_RMSE)
  modeledSet$outputs[[modelType]]$perf_summary$ci_RMSE <- NULL

  modeledSet$outputs[[modelType]]$perf_summary$Rsquared <- paste(round(Rsquared_pt_estimate, 2), modeledSet$outputs[[modelType]]$perf_summary$ci_Rsquared)
  modeledSet$outputs[[modelType]]$perf_summary$ci_Rsquared <- NULL

  modeledSet$outputs[[modelType]]$perf_summary$CCC <- paste(round(CCC_pt_estimate, 2), modeledSet$outputs[[modelType]]$perf_summary$ci_CCC)
  modeledSet$outputs[[modelType]]$perf_summary$ci_CCC <- NULL

  modeledSet$outputs[[modelType]]$perf_summary$bias <- paste(round(bias_pt_estimate, 2), modeledSet$outputs[[modelType]]$perf_summary$ci_bias)
  modeledSet$outputs[[modelType]]$perf_summary$biaswidth <- NULL
  modeledSet$outputs[[modelType]]$perf_summary$ci_bias <- NULL
  modeledSet$outputs[[modelType]]$perf_summary$ci_lb_bias <- NULL
  modeledSet$outputs[[modelType]]$perf_summary$ci_ub_bias <- NULL

  modeledSet$outputs[[modelType]]$perf_summary$slope <- paste(round(slope_pt_estimate, 2), modeledSet$outputs[[modelType]]$perf_summary$ci_slope)
  modeledSet$outputs[[modelType]]$perf_summary$ci_slope <- NULL
  modeledSet$outputs[[modelType]]$perf_summary$ci_lb_slope <- NULL
  modeledSet$outputs[[modelType]]$perf_summary$ci_ub_slope <- NULL

  modeledSet$outputs[[modelType]]$perf_summary$intercept <- paste(round(intercept_pt_estimate, 2), modeledSet$outputs[[modelType]]$perf_summary$ci_intercept)
  modeledSet$outputs[[modelType]]$perf_summary$ci_intercept <- NULL
  modeledSet$outputs[[modelType]]$perf_summary$ci_lb_intercept <- NULL
  modeledSet$outputs[[modelType]]$perf_summary$ci_ub_intercept <- NULL

  write_csv(modeledSet$outputs[[modelType]]$perf_summary, path=file.path(modeledSet$outLoc, paste(processedSet$response, "_", modeledSet$setLabel, "_", modelType, "_perf.csv", sep='')))
  
  # now see if this model improved the prior best, and if so, update the data structure accordingly
  if (maximizeForOptimization) {
    if (CCC_pt_estimate > modeledSet$best_$currentBest) {
      modeledSet$best_$model <- cont_resp_mod
      modeledSet$best_$currentBest <- CCC_pt_estimate 
      modeledSet$best_$predictions <- modeledSet$outputs[[modelType]]$predictions
      modeledSet$best_$modelType <- modelType
      modeledSet$best_$modelTypeStr <- modelTypeStr
      modeledSet$best_$postScaling <- postScaling
      modeledSet$best_$postOffset <- postOffset
      modeledSet$best_$preProc_recipe_for_subset <- preProc_recipe_for_subset # only used for rfeLR
    }
  } else {
    #THIS BRANCH IS NOT FULLY OPERATIONAL NOW (the per summary is now a string, because it is adorned with the CI. Need to have a pt estimate, but for a flexible metric.)
    if (modeledSet$outputs[[modelType]]$perf_summary[[metricForOptimization]] < modeledSet$best_$currentBest) {
      modeledSet$best_$model <- cont_resp_mod
      modeledSet$best_$currentBest <- modeledSet$outputs[[modelType]]$perf_summary[[metricForOptimization]]
      modeledSet$best_$predictions <- modeledSet$outputs[[modelType]]$predictions
      modeledSet$best_$modelType <- modelType
      modeledSet$best_$modelTypeStr <- modelTypeStr
      modeledSet$best_$postScaling <- postScaling
      modeledSet$best_$postOffset <- postOffset
      modeledSet$best_$preProc_recipe_for_subset <- preProc_recipe_for_subset # only used for rfeLR
    }
  }

  return(modeledSet)
}

####
# multivariateModeling: The primary input here is a list of results from the initial processing performed on the predictors
####
multivariateModeling <- function(processedSet)
{
  # set up a list for the results to be added to, which will contain the summarizations from the various
  # applied modeling methods
  modeledSet=list()
  modeledSet$outLoc <- processedSet$outLoc
  modeledSet$setLabel <- processedSet$setLabel
  cat(paste0("Preparing to model ", processedSet$response, " using ", modeledSet$setLabel, "...\n"))
  modeledSet$response <- processedSet$response
  modeledSet$is_response_cat <- processedSet$is_response_cat
  modeledSet$performanceSummary_cat <- data.frame()
  modeledSet$performanceSummary_cont <- data.frame()
  modeledSet$outputs <- list()
  modeledSet$forBadSampleAnalysis_cat <- list()
  modeledSet$forBadSampleAnalysis_cont <- list()
  modeledSet$ID <- processedSet$ID
  modeledSet$FullKeys <- processedSet$FullKeys

  if (modeledSet$is_response_cat) {
    metricForOptimization="ROC"
    maximizeForOptimization=TRUE
    if (maximizeForOptimization) {
      modeledSet$best_$currentBest <- -Inf
    } else {
      modeledSet$best_$currentBest <- Inf
    }
  } else {
    metricForOptimization="CCC"
    maximizeForOptimization=TRUE
    if (maximizeForOptimization) {
      modeledSet$best_$currentBest <- -Inf
    } else {
      modeledSet$best_$currentBest <- Inf
    }
  }

  num_observations <- nrow(processedSet$dataForModeling)
  num_predictors <- ncol(processedSet$dataForModeling) - 1 # one column is the response

  cat(paste0("...starting to model ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
  num_repeats <- 4 # default, also the correct value if response is cont
  use_downsampling <- FALSE # default
  postScalingForOriginal <- 1 # default
  postOffsetForOriginal <- 0 # default
  if (processedSet$is_response_cat) {
    # we use more repeats when there is a relatively large imbalance between true classes given for a cat response
    majority_count <- max(table(processedSet$dataForModeling[,processedSet$response]))
    minority_count <- min(table(processedSet$dataForModeling[,processedSet$response]))
    ratio_of_majority_to_minority <- majority_count / minority_count
    use_downsampling <- ((ratio_of_majority_to_minority < 7) & (minority_count > 30))
    if (use_downsampling) {
      num_repeats <- ifelse(ratio_of_majority_to_minority >= 90/10, 250, 
                     ifelse(ratio_of_majority_to_minority < 90/10 & ratio_of_majority_to_minority >= 80/20, 150,
                     ifelse(ratio_of_majority_to_minority < 80/20 & ratio_of_majority_to_minority >= 70/30, 75, 
                     ifelse(ratio_of_majority_to_minority < 70/30 & ratio_of_majority_to_minority >= 60/40, 35, 15))))
      cat(paste0("  with downsampling (because there is enough minority to work with), resulting in ", num_repeats, 
                 " repeats due to ratio_of_majority_to_minority=", round(ratio_of_majority_to_minority,2),
                 " (", minority_count, " examples of minority class).\n"))
    } else {
      cat(paste0("  without downsampling due to ratio_of_majority_to_minority=", round(ratio_of_majority_to_minority,2),
                 " (", minority_count, " examples of minority class).\n  resulting in the use of ", num_repeats, " repeats.\n"))
      postOffsetForOriginal <- -(0.5 - (minority_count/(majority_count+minority_count))) # needed to shift the predicted probabilities down if minorty isn't half of total
    }
  }
  
  if (use_downsampling) {
    recipe_start_when_resp_is_cat_and_at_least_one_cat_pred <- recipe(processedSet$recipe_formula, data=processedSet$dataForModeling) %>%
                                                               step_downsample(seed=as.numeric(Sys.time()), all_outcomes()) %>%
                                                               step_dummy(all_predictors(), -all_numeric()) %>%
                                                               step_nzv(all_predictors()) %>%
                                                               step_center(all_predictors()) %>%
                                                               step_scale(all_predictors()) %>%
                                                               step_YeoJohnson(all_predictors())
                                       
    recipe_start_when_resp_is_cat_and_no_cat_preds <- recipe(processedSet$recipe_formula, data=processedSet$dataForModeling) %>%
                                                      step_downsample(seed=as.numeric(Sys.time()), all_outcomes()) %>%
                                                      step_nzv(all_predictors()) %>%
                                                      step_center(all_predictors()) %>%
                                                      step_scale(all_predictors()) %>%
                                                      step_YeoJohnson(all_predictors())
  } else {
    recipe_start_when_resp_is_cat_and_at_least_one_cat_pred <- recipe(processedSet$recipe_formula, data=processedSet$dataForModeling) %>%
                                                               step_dummy(all_predictors(), -all_numeric()) %>%
                                                               step_nzv(all_predictors()) %>%
                                                               step_center(all_predictors()) %>%
                                                               step_scale(all_predictors()) %>%
                                                               step_YeoJohnson(all_predictors())
                                       
    recipe_start_when_resp_is_cat_and_no_cat_preds <- recipe(processedSet$recipe_formula, data=processedSet$dataForModeling) %>%
                                                      step_nzv(all_predictors()) %>%
                                                      step_center(all_predictors()) %>%
                                                      step_scale(all_predictors()) %>%
                                                      step_YeoJohnson(all_predictors())
  }

  recipe_start_when_resp_is_cont_and_at_least_one_cat_pred <- recipe(processedSet$recipe_formula, data=processedSet$dataForModeling)  %>%
                                                              step_dummy(all_predictors(), -all_numeric()) %>%
                                                              step_nzv(all_predictors()) %>%
                                                              step_center(all_predictors()) %>%
                                                              step_scale(all_predictors()) %>%
                                                              step_YeoJohnson(all_predictors())

  recipe_start_when_resp_is_cont_and_no_cat_preds <- recipe(processedSet$recipe_formula, data=processedSet$dataForModeling) %>%
                                                     step_nzv(all_predictors()) %>%
                                                     step_center(all_predictors()) %>%
                                                     step_scale(all_predictors()) %>%
                                                     step_YeoJohnson(all_predictors())

  ###
  # Most of the model types all use a common structure, whch we set up here (rfeLR doens't use it, but it will just be harmlessly ignored by that modelType)
  if (processedSet$is_response_cat) {
    if (processedSet$num_cat_preds > 0) {
      preProc_recipe <- recipe_start_when_resp_is_cat_and_at_least_one_cat_pred %>% step_corr(all_predictors(), threshold=.9) 
    } else {
      preProc_recipe <- recipe_start_when_resp_is_cat_and_no_cat_preds %>% step_corr(all_predictors(), threshold=.9) 
    }
  } else {
    if (processedSet$num_cat_preds > 0) {
      preProc_recipe <- recipe_start_when_resp_is_cont_and_at_least_one_cat_pred %>% step_corr(all_predictors(), threshold=.9) 
    } else {
      preProc_recipe <- recipe_start_when_resp_is_cont_and_no_cat_preds %>% step_corr(all_predictors(), threshold=.9) 
    }
  }
  if (modeledSet$is_response_cat) {
    int_ctrl <- trainControl(method="repeatedcv", repeats=num_repeats, classProbs=TRUE, summaryFunction=fiveStats, savePredictions=TRUE)              
  } else {
    int_ctrl <- trainControl(method="repeatedcv", repeats=num_repeats, summaryFunction=custom_summary, savePredictions=TRUE)
  }

  if (!is.list(processedSet$response)) {
    ##### LINEAR MODELING METHODS
    # Recursive feature elimination: set up code for rfe.  The recipes package cannot yet be directly used with rfe, so the code is 
    # different than the code for the other model types.
    if (num_predictors > 1) { # rfe in particular isn't well defined unless there are at least two predictors. So skip the reduction step
                              # (step_corr) if not enough predictors (essentially resulting in it just being lr, not lr with rfe).
      cor_threshold <- 0.9
      repeat {
        # general case where there are more predictors than only 2
        if (processedSet$is_response_cat) {
          if (processedSet$num_cat_preds > 0) {
            preProc_recipe_for_subset <- recipe_start_when_resp_is_cat_and_at_least_one_cat_pred %>%
                                         step_corr(all_predictors(), threshold=cor_threshold) %>%
                                         prep(training=processedSet$dataForModeling, retain=TRUE)
          } else {
            preProc_recipe_for_subset <- recipe_start_when_resp_is_cat_and_no_cat_preds %>%
                                         step_corr(all_predictors(), threshold=cor_threshold) %>%
                                         prep(training=processedSet$dataForModeling, retain=TRUE)
          }
        } else {
          if (processedSet$num_cat_preds > 0) {
            preProc_recipe_for_subset <- recipe_start_when_resp_is_cont_and_at_least_one_cat_pred %>%
                                         step_corr(all_predictors(), threshold=cor_threshold) %>%
                                         prep(training=processedSet$dataForModeling, retain=TRUE)
          } else {
            preProc_recipe_for_subset <- recipe_start_when_resp_is_cont_and_no_cat_preds %>%
                                         step_corr(all_predictors(), threshold=cor_threshold) %>%
                                         prep(training=processedSet$dataForModeling, retain=TRUE)
          }
        }
        data_for_subset <- bake(preProc_recipe_for_subset, new_data=processedSet$dataForModeling)
        num_observations <- nrow(data_for_subset)
        num_predictors <- ncol(data_for_subset) - 1 # one column is the the response
        if (num_predictors < 0.8*num_observations) {
          # now we have a reasonable number of predictors for the number of observations we have, so move ahead
          break
        } else {
          # notch down the correlation coefficent and re-run the recipe so as to reduce excessive number of predictors given the
          # number of observations
          cor_threshold <- cor_threshold - 0.02
        }
      }
    } else {
      # special case where there is only one predictor
      if (processedSet$is_response_cat) {
        # in this special case we can't do nzv (otherwise the recipes are the same)
        if (processedSet$num_cat_preds > 0) {
          if (use_downsampling) {
            preProc_recipe_for_subset <- recipe(processedSet$recipe_formula, data=processedSet$dataForModeling) %>%
                                         step_downsample(seed=as.numeric(Sys.time()), all_outcomes()) %>%
                                         step_dummy(all_predictors(), -all_numeric()) %>%
                                         step_center(all_predictors()) %>%
                                         step_scale(all_predictors()) %>%
                                         step_YeoJohnson(all_predictors()) %>%
                                         prep(training=processedSet$dataForModeling, retain=TRUE)
          } else {
            preProc_recipe_for_subset <- recipe(processedSet$recipe_formula, data=processedSet$dataForModeling) %>%
                                         step_dummy(all_predictors(), -all_numeric()) %>%
                                         step_center(all_predictors()) %>%
                                         step_scale(all_predictors()) %>%
                                         step_YeoJohnson(all_predictors()) %>%
                                         prep(training=processedSet$dataForModeling, retain=TRUE)            
          }
        } else {
          if (use_downsampling) {
            preProc_recipe_for_subset <- recipe(processedSet$recipe_formula, data=processedSet$dataForModeling) %>%
                                         step_downsample(seed=as.numeric(Sys.time()), all_outcomes()) %>%
                                         step_center(all_predictors()) %>%
                                         step_scale(all_predictors()) %>%
                                         step_YeoJohnson(all_predictors()) %>%
                                         prep(training=processedSet$dataForModeling, retain=TRUE)
          } else {
            preProc_recipe_for_subset <- recipe(processedSet$recipe_formula, data=processedSet$dataForModeling) %>%
                                         step_center(all_predictors()) %>%
                                         step_scale(all_predictors()) %>%
                                         step_YeoJohnson(all_predictors()) %>%
                                         prep(training=processedSet$dataForModeling, retain=TRUE)            
          }
        }
      } else {
        if (processedSet$num_cat_preds > 0) {
          preProc_recipe_for_subset <- recipe(processedSet$recipe_formula, data=processedSet$dataForModeling)  %>%
                                       step_dummy(all_predictors(), -all_numeric()) %>%
                                       step_center(all_predictors()) %>%
                                       step_scale(all_predictors()) %>%
                                       step_YeoJohnson(all_predictors()) %>%
                                       prep(training=processedSet$dataForModeling, retain=TRUE)
        } else {
          preProc_recipe_for_subset <- recipe(processedSet$recipe_formula, data=processedSet$dataForModeling) %>%
                                       step_center(all_predictors()) %>%
                                       step_scale(all_predictors()) %>%
                                       step_YeoJohnson(all_predictors()) %>%
                                       prep(training=processedSet$dataForModeling, retain=TRUE)
        }
      }
      data_for_subset <- bake(preProc_recipe_for_subset, new_data=processedSet$dataForModeling)
    }

    # LOGISTIC / LINEAR REGRESSION: set up RFE control structure
    if (modeledSet$is_response_cat) {
      #set.seed(63331)
      internal_ctrl=trainControl(method="none", classProbs=TRUE, allowParallel=FALSE)
      rfeLRFuncsNew <- caretFuncs
      rfeLRFuncsNew$summary <- fiveStats
      rfeLRFuncsNew$rank <- function (object, x, y) {
        coefs <- abs(coef(object$finalModel))
        coefs <- coefs[names(coefs)  !=   "(Intercept)"]
        coefs[is.na(coefs)] <- 0
        if (length(coefs) < ncol(x)) {
          missing_cols <- colnames(x)[!(colnames(x) %in% names(coefs))]
          missing_coef <- rep(0, length(missing_cols))
          names(missing_coef) <- missing_cols
          coefs <- c(coefs, missing_coef)
        }
        vimp <- data.frame(Overall=unname(coefs), var=names(coefs))
        vimp <- vimp[order(vimp$Overall, decreasing=TRUE), , drop=FALSE]
        vimp
      }
    } else {
      #set.seed(63331)
      internal_ctrl=trainControl(method="none", allowParallel=FALSE)
      rfeLRFuncsNew <- caretFuncs
      rfeLRFuncsNew$summary <- custom_summary
    }
    #set.seed(63331)
    rfeCtrl <- rfeControl(functions=rfeLRFuncsNew, method="repeatedcv", repeats=num_repeats, rerank=TRUE, returnResamp="all", saveDetails=TRUE, verbose=FALSE)
    
    # split for_analysis data set into the response and the predictors
    for_analysis_X <- data_for_subset %>% dplyr::select(-c(processedSet$response))
    for_analysis_Y <- data_for_subset %>% pull(c(processedSet$response))
    seq_by_val <- ifelse(ncol(for_analysis_X)/3 < 1, 1, round(ncol(for_analysis_X)/3, 0))
    if (ncol(for_analysis_X) > 0) {
      rfe_sequence <- seq(1, ncol(for_analysis_X), by=seq_by_val)
      #set.seed(63331)
      rfeLR_mod <- rfe(x=for_analysis_X, 
                       y=for_analysis_Y, 
                       sizes=rfe_sequence, 
                       rfeControl=rfeCtrl, 
                       metric=metricForOptimization, 
                       method="glm", 
                       trControl=internal_ctrl)

      if (length(c(processedSet$predictors_cat, processedSet$predictors_cont)) > 1) {
        # no reason to do this if only one predictor, as occurs for example in the baseline, and doing so creates an ouput at the console that we don't need to see
        ggplot(rfeLR_mod) +
          ggtitle(paste0(processedSet$response, " by ", modeledSet$setLabel)) + theme(plot.title=element_text(size=12, face="bold", hjust=0.5))
        ggsave(file.path(modeledSet$outLoc, paste(processedSet$response, "_", modeledSet$setLabel, "_rfeLR_tuning.png", sep='')),  width=4, height=4)
      }

      vi_rfeLR <- varImp(rfeLR_mod) %>% 
                  dplyr::mutate(Importance=100*Overall/max(Overall), Predictor=rownames(varImp(rfeLR_mod))) %>%
                  dplyr::arrange(desc(Importance)) %>%
                  dplyr::slice(1:10) %>%
                  dplyr::mutate(Predictor=factor(Predictor, levels=Predictor))
      ggplot(vi_rfeLR, aes(x=Predictor, y=Importance)) +
       geom_col() +
       scale_x_discrete(limits=rev(levels(vi_rfeLR$Predictor))) +
       coord_flip() +
       ggtitle(paste0(processedSet$response, " by ", modeledSet$setLabel)) + theme(plot.title=element_text(size=12, face="bold", hjust=0.5))
      ggsave(file.path(modeledSet$outLoc, paste(processedSet$response, "_", modeledSet$setLabel, "_rfeLR_varImp.png", sep='')),  width=4, height=4)
      
      # create tibble containing the logistic regression estimates, odds ratios, 95% confidence intervals, and p-values  
      rfeLR_summary <- summary(rfeLR_mod$fit)
      if (modeledSet$is_response_cat) {
        modeledSet$outputs$rfeLR$coefficient_table <- tibble(Feature=rownames(rfeLR_summary$coefficients), 
                                                             Estimate=data.frame(rfeLR_summary$coefficients)$Estimate, 
                                                             OR=exp(coef(rfeLR_mod$fit$finalModel)), 
                                                             Lower=exp(confint.default(rfeLR_mod$fit$finalModel, level=0.95))[,1], 
                                                             Upper=exp(confint.default(rfeLR_mod$fit$finalModel, level=0.95))[,2], 
                                                             PValue=rfeLR_summary$coefficients[,"Pr(>|z|)"])
      } else {
        modeledSet$outputs$rfeLR$coefficient_table <- tibble(Feature=rownames(rfeLR_summary$coefficients),
                                                             Estimate=data.frame(rfeLR_summary$coefficients)$Estimate,
                                                             Lower=confint.default(rfeLR_mod$fit$finalModel, level=0.95)[,1],
                                                             Upper=confint.default(rfeLR_mod$fit$finalModel, level=0.95)[,2],
                                                             PValue=rfeLR_summary$coefficients[,"Pr(>|t|)"])
      }

      print(xtable(modeledSet$outputs$rfeLR$coefficient_table), type="html", 
            file=file.path(modeledSet$outLoc, paste(processedSet$response, "_", modeledSet$setLabel, "_rfeLR_coefficient_table.html", sep='')), include.rownames=FALSE)
      
      # now do the post as most other model types do it
      if (modeledSet$is_response_cat) {
        modeledSet <- multivariateModeling_catPost(rfeLR_mod, processedSet, modeledSet, "rfeLR", metricForOptimization, maximizeForOptimization, 
                                                   rfeLR_mod$pred %>% dplyr::filter(Variables == rfeLR_mod$optsize),
                                                   postScalingForOriginal, postOffsetForOriginal, preProc_recipe_for_subset)
      } else {
        modeledSet <- multivariateModeling_contPost(rfeLR_mod, processedSet, modeledSet, "rfeLR", metricForOptimization, maximizeForOptimization, 
                                                    rfeLR_mod$pred %>% dplyr::filter(Variables == rfeLR_mod$optsize),
                                                    postScalingForOriginal, postOffsetForOriginal, preProc_recipe_for_subset)
      }
    }
    cat(paste0("...finished rfeLR for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))

    ##
    # Penalized models (as long as there is at least one continuous predictor, the penalized models are ill-defined with only categoric predictors)
    if (processedSet$predictors_cont[1] != "") {
      if (modeledSet$is_response_cat) {
        # categoric response variables use glmnet modeling
        glmnetGrid <- expand.grid(alpha=c(0.01, 0.03, 0.1, 0.2, 0.3), lambda=c(0.01, 0.03, 0.1, 0.15, 0.2))
        #set.seed(63331)
        glmnet_mod <- try(train(x=preProc_recipe, 
                           data=processedSet$dataForModeling, 
                           metric=metricForOptimization, 
                           method="glmnet", 
                           tuneGrid=glmnetGrid, 
                           trControl=int_ctrl), 
                           silent=TRUE)
        if (class(glmnet_mod)[1] != "try-error") {
          modeledSet <- multivariateModeling_catPost(glmnet_mod, processedSet, modeledSet, "glmnet", metricForOptimization, maximizeForOptimization, 
                                                     glmnet_mod$pred %>% dplyr::filter(alpha==glmnet_mod$bestTune$alpha & lambda==glmnet_mod$bestTune$lambda),
                                                     postScalingForOriginal, postOffsetForOriginal)
          cat(paste0("...finished glmnet model for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
        } else {
          cat(paste0("...glmnet model crashed for ", processedSet$response, " using ", modeledSet$setLabel, " (no model could be built)\n"))
        }
      } else {
        # continuous response variables use ridge modeling
        ridgeGrid <- data.frame(lambda=seq(0, 0.7, length=15))
        #set.seed(63331)
        ridge_mod <- try(train(x=preProc_recipe, 
                           data=processedSet$dataForModeling, 
                           metric=metricForOptimization, 
                           method="ridge", 
                           tuneGrid=ridgeGrid, 
                           trControl=int_ctrl), 
                           silent=TRUE)
        if (class(ridge_mod)[1] != "try-error") {
          if (save_tuning_and_varImp(ridge_mod, processedSet, modeledSet, "ridge")) {
            modeledSet <- multivariateModeling_contPost(ridge_mod, processedSet, modeledSet, "ridge", metricForOptimization, maximizeForOptimization, 
                                                        ridge_mod$pred %>% dplyr::filter(lambda==ridge_mod$bestTune$lambda),
                                                        postScalingForOriginal, postOffsetForOriginal)
            cat(paste0("...finished ridge model for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
          } else {
            cat(paste0("...ridge model built, but varImp crashed for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
          }
        } else {
          cat(paste0("...ridge model crashed for ", processedSet$response, " using ", modeledSet$setLabel, " (no model could be built)\n"))
        }
      }
    }

    ##### NON-LINEAR MODELING METHODS
    ###
    # Support Vector Machines
    # svmRadial (works for both categoric and continuous response variables with only modest differences)
    #set.seed(63331)
    svmRadial_mod <- try(train(x=preProc_recipe, 
                               data=processedSet$dataForModeling, 
                               metric=metricForOptimization, 
                               method="svmRadial", 
                               trace=FALSE, verboseIter=TRUE, verbose=FALSE, silent=TRUE, maxiter=-1, # to suppress the extraneous convergence messages
                               tuneGrid=expand.grid(C=c(0.01, 0.03, 0.1, 0.2, 0.3, 0.4, 0.5, 1.0, 1.25, 1.5, 2.0, 3.0), sigma=c(0.0001, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3)), 
                               trControl=int_ctrl), 
                               silent=TRUE)
    if (class(svmRadial_mod)[1] != "try-error") {
      
      # get performance summary for best feature subset
      if (save_tuning_and_varImp(svmRadial_mod, processedSet, modeledSet, "svmRadial")) {
        if (modeledSet$is_response_cat) {
          modeledSet <- multivariateModeling_catPost(svmRadial_mod, processedSet, modeledSet, "svmRadial", metricForOptimization, maximizeForOptimization, 
                                                     svmRadial_mod$pred %>% dplyr::filter(C==svmRadial_mod$bestTune$C & sigma==svmRadial_mod$bestTune$sigma),
                                                     postScalingForOriginal, postOffsetForOriginal)
        } else {
          modeledSet <- multivariateModeling_contPost(svmRadial_mod, processedSet, modeledSet, "svmRadial", metricForOptimization, maximizeForOptimization,
                                                      svmRadial_mod$pred %>% dplyr::filter(C==svmRadial_mod$bestTune$C & sigma==svmRadial_mod$bestTune$sigma),
                                                      postScalingForOriginal, postOffsetForOriginal)
        }
        cat(paste0("...finished svmRadial model for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
      } else {
        cat(paste0("...svmRadial model built, but varImp crashed for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
      }
    } else {
      cat(paste0("...svmRadial model crashed for ", processedSet$response, " using ", modeledSet$setLabel, " (no model could be built)\n"))
    }

    ###
    # Tree-based models
    if (modeledSet$is_response_cat) {
      # categoric response variables use C5.0 modeling
      #set.seed(63331)
      c50_mod <- try(train(x=preProc_recipe, 
                      data=processedSet$dataForModeling, 
                      metric=metricForOptimization, 
                      method="C5.0", 
                      tuneGrid=expand.grid(trials=c(1:10, 20, 30, 40, 50, 60), winnow=FALSE, model="tree"), 
                      trControl=int_ctrl), 
                      silent=TRUE)
      if (class(c50_mod)[1] != "try-error") {
        if (save_tuning_and_varImp(c50_mod, processedSet, modeledSet, "c50")) {
          modeledSet <- multivariateModeling_catPost(c50_mod, processedSet, modeledSet, "c50", metricForOptimization, maximizeForOptimization, 
                                                     c50_mod$pred %>% dplyr::filter(model==as.character(c50_mod$bestTune$model) & 
                                                                                    winnow==as.character(c50_mod$bestTune$winnow) & 
                                                                                    trials==c50_mod$bestTune$trials),
                                                     postScalingForOriginal, postOffsetForOriginal)
          cat(paste0("...finished c50 model for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
        } else {
          cat(paste0("...c50 model built, but varImp crashed for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
        }
      } else {
        cat(paste0("...c50 model crashed for ", processedSet$response, " using ", modeledSet$setLabel, " (no model could be built)\n"))
      }
    } else {
      # continuous response variables use cubist modeling
      #set.seed(63331)
      cubist_mod <- try(train(x=preProc_recipe, 
                          data=processedSet$dataForModeling, 
                          metric=metricForOptimization, 
                          method="cubist", 
                          tuneGrid=expand.grid(committees=c(1:10, 20, 30, 40, 50, 60), neighbors=c(1, 3, 5, 7, 9)), # note, 9 neighbors is the most that can be used
                          trControl=int_ctrl), 
                          silent=TRUE)
      if (class(cubist_mod)[1] != "try-error") {
        if (save_tuning_and_varImp(cubist_mod, processedSet, modeledSet, "cubist")) {
          modeledSet <- multivariateModeling_contPost(cubist_mod, processedSet, modeledSet, "cubist", metricForOptimization, maximizeForOptimization,
                                                      cubist_mod$pred %>%dplyr::filter(committees==cubist_mod$bestTune$committees & neighbors==cubist_mod$bestTune$neighbors),
                                                      postScalingForOriginal, postOffsetForOriginal)
          cat(paste0("...finished cubist model for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
        } else {
          cat(paste0("...cubist model built, but varImp crashed for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
        }
      } else {
        cat(paste0("...cubist model crashed for ", processedSet$response, " using ", modeledSet$setLabel, " (no model could be built)\n"))
      }
    }
  
    ###
    # Partial Least Squares
    # pls (works for both categoric and continuous response variables, and also univariate /multivariate response, with only modest differences)
    #set.seed(63331)
    minimally_viable_latent_variables <- 2
    max_num_latent_variables_to_be_searched <- 20
    if (!is.list(processedSet$response)) {
      # number of latent variables needs to be less than or equal to number of predictors when the response is univariate
      num_predictors <- ncol(processedSet$dataForModeling) - 1 # one column is the response
      pls_can_run <- (num_predictors >= minimally_viable_latent_variables)
      if (pls_can_run) {
        if (num_predictors > max_num_latent_variables_to_be_searched) {
          plsGrid <- data.frame(ncomp=seq(1, max_num_latent_variables_to_be_searched, by=1))
        } else {
          plsGrid <- data.frame(ncomp=seq(1, num_predictors-1, by=1))
        }
      }
    } else {
      # number of latent variables needs to be less than or equal to the min number of response or predictors when the response is multivariate
      max_latent_variables_in_input <- min(ncol(processedSet$dataForModeling)-length(response), length(response))
      pls_can_run <- (max_latent_variables_in_input >= minimally_viable_latent_variables)
      if (pls_can_run) {
        if (max_latent_variables_in_input > max_num_latent_variables_to_be_searched) {
          plsGrid <- data.frame(ncomp=seq(1, max_num_latent_variables_to_be_searched, by=1))
        } else {
          plsGrid <- data.frame(ncomp=seq(1, max_latent_variables_in_input-1, by=1))
        }
      }
    }
    if (pls_can_run) {  
      pls_mod <- try(train(x=preProc_recipe, 
                           data=processedSet$dataForModeling, 
                           metric=metricForOptimization, 
                           method="pls", 
                           tuneGrid=plsGrid, 
                           trControl=int_ctrl), 
                           silent=TRUE)
      if (class(pls_mod)[1] != "try-error") {
        # get performance summary for best feature subset
        if (save_tuning_and_varImp(pls_mod, processedSet, modeledSet, "pls")) {
          if (modeledSet$is_response_cat) {
            modeledSet <- multivariateModeling_catPost(pls_mod, processedSet, modeledSet, "pls", metricForOptimization, maximizeForOptimization, 
                                                       pls_mod$pred %>% dplyr::filter(ncomp==pls_mod$bestTune$ncomp),
                                                       postScalingForOriginal, postOffsetForOriginal)
          } else {
            modeledSet <- multivariateModeling_contPost(pls_mod, processedSet, modeledSet, "pls", metricForOptimization, maximizeForOptimization,
                                                        pls_mod$pred %>% dplyr::filter(ncomp==pls_mod$bestTune$ncomp),
                                                        postScalingForOriginal, postOffsetForOriginal)
          }
          cat(paste0("...finished pls model for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
        } else {
          cat(paste0("...pls model built, but varImp crashed for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
        }
      } else {
        cat(paste0("...pls model crashed for ", processedSet$response, " using ", modeledSet$setLabel, " (no model could be built)\n"))
      }
    }
    
    ###
    # Neural Networks
    # avNNet modeling 
    if (modeledSet$is_response_cat) {
      #set.seed(63331)
      avNNet_mod <- try(train(x=preProc_recipe, 
                              data=processedSet$dataForModeling, 
                              metric=metricForOptimization, 
                              method="avNNet", 
                              trace=FALSE, 
                              tuneGrid=expand.grid(size=c(1, 6, 12, 18, 24, 30, 36), decay=c(0, 0.005, 0.01, 0.025, 0.04, 0.055, 0.07), bag=c(FALSE, TRUE)), 
                              trControl=int_ctrl), 
                              silent=TRUE)
      if (class(avNNet_mod)[1] != "try-error") {
          if (save_tuning_and_varImp(avNNet_mod, processedSet, modeledSet, "avNNet")) {
          modeledSet <- multivariateModeling_catPost(avNNet_mod, processedSet, modeledSet, "avNNet", metricForOptimization, maximizeForOptimization, 
                                                     avNNet_mod$pred %>% dplyr::filter(size==avNNet_mod$bestTune$size & decay==avNNet_mod$bestTune$decay & bag==avNNet_mod$bestTune$bag),
                                                     postScalingForOriginal, postOffsetForOriginal)
          cat(paste0("...finished avNNet model for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
        } else {
          cat(paste0("...avNNet model built, but varImp crashed for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
        }
      } else {
        cat(paste0("...avNNet model crashed for ", processedSet$response, " using ", modeledSet$setLabel, " (no model could be built)\n"))
      }
    } else {
      # continuous response variable avNNet needs continuous valued responses  to be in the interval [0,1]
      if (processedSet$response == "FFR") {
        postScalingForOriginal <- 100
        postOffsetForOriginal <- 0
      } else {
        max_response <- max(processedSet$dataForModeling[,processedSet$response])
        min_response <- min(processedSet$dataForModeling[,processedSet$response])
        postScalingForOriginal <- (max_response - min_response)
        postOffsetForOriginal <- min_response
      }
      
      current_data <- processedSet$dataForModeling
      current_data[,processedSet$response] <- (current_data[,processedSet$response] - postOffsetForOriginal)/postScalingForOriginal
      
      if (processedSet$num_cat_preds > 0) {
        current_recipe <- recipe(processedSet$recipe_formula, data=current_data) %>%
                          step_dummy(all_predictors(), -all_numeric()) %>%
                          step_nzv(all_predictors()) %>%
                          step_corr(all_predictors(), threshold=.9) %>%
                          step_center(all_predictors()) %>%
                          step_scale(all_predictors()) %>%
                          step_YeoJohnson(all_predictors())
      } else {
        current_recipe <- recipe(processedSet$recipe_formula, data=current_data) %>%
                          step_nzv(all_predictors()) %>%
                          step_corr(all_predictors(), threshold=.9) %>%
                          step_center(all_predictors()) %>%
                          step_scale(all_predictors()) %>%
                          step_YeoJohnson(all_predictors())
      }

      #set.seed(63331)
      avNNet_mod <- try(train(x=current_recipe, 
                              data=current_data,
                              metric=metricForOptimization, 
                              method="avNNet", 
                              trace=FALSE, 
                              tuneGrid=expand.grid(size=c(1, 6, 12, 18, 24, 30), decay=c(0, 0.005, 0.01, 0.025, 0.04), bag=c(FALSE, TRUE)), 
                              trControl=int_ctrl), 
                              silent=TRUE)
      if (class(avNNet_mod)[1] != "try-error") {
        if (save_tuning_and_varImp(avNNet_mod, processedSet, modeledSet, "avNNet")) {
          modeledSet <- multivariateModeling_contPost(avNNet_mod, processedSet, modeledSet, "avNNet", metricForOptimization, maximizeForOptimization,
                                                      avNNet_mod$pred %>% dplyr::filter(size==avNNet_mod$bestTune$size & decay==avNNet_mod$bestTune$decay & bag==avNNet_mod$bestTune$bag),
                                                      postScalingForOriginal, postOffsetForOriginal)
          cat(paste0("...finished avNNet model for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
        } else {
          cat(paste0("...avNNet model built, but varImp crashed for ", processedSet$response, " using ", modeledSet$setLabel, "\n"))
        }
      } else {
        cat(paste0("...avNNet model crashed for ", processedSet$response, " using ", modeledSet$setLabel, " (no model could be built)\n"))
      }
    }

  } else {
    # multi-variate response
    multi_rec <- recipe(as.formula(paste0(paste(unlist(response), collapse="+"),"~.")), data = dev_data) %>%
                 step_center(everything()) %>%
                 step_scale(everything())

    #set.seed(814)
    folds <- vfold_cv(dev_data, v=10, repeats=num_repeats)
    folds <- folds %>% mutate(recipes=map(splits, prepper, recipe=multi_rec, retain=TRUE))

    get_var_explained <- function(recipe, ...) {
      # Extract the predictors and outcomes into their own matrices
      y_mat <- juice(recipe, composition="matrix", all_outcomes())
      x_mat <- juice(recipe, composition="matrix", all_predictors())
      
      # The pls package prefers the data in a data frame where the outcome and predictors are in _matrices_. 
      # To make sure this is formatted properly, use the `I` function to inhibit `data.frame` from making
      # all the individual columns.
      pls_format <- data.frame(endpoints=I(y_mat), measurements=I(x_mat))
      # Fit the model
      current_mod <- plsr(endpoints ~ measurements, data=pls_format)
      
      # Get the proportion of the predictor variance that is explained by the model for different number of
      # components. 
      xve <- explvar(current_mod)/100 
      
      # To do the same for the outcome, it is more complex. This code 
      # was extracted from pls:::summary.mvr. 
      explained <- drop(pls::R2(current_mod, estimate="train", intercept=FALSE)$val) %>% 
                   # transpose so that components are in rows
                   t() %>% 
                   as.data.frame() %>%
                   # Add the predictor proportions
                   mutate(predictors=cumsum(xve) %>% as.vector(),
                          components=seq_along(xve)) %>%
                   # Put into a tidy format that is tall
                   gather(source, proportion, -components)
    }

    folds <- folds %>% mutate(var=map(recipes, get_var_explained))
    num_components <- min(max(length(response), length(c(predictors_cat, predictors_cont))), 25) # maximimum viable number of components
    variance_data <- bind_rows(folds[["var"]]) %>%
                     filter(components <= num_components) %>%
                     group_by(components, source) %>%
                     summarize(proportion=mean(proportion))

    # tuning parameter profile
    if (length(c(processedSet$predictors_cat,processedSet$predictors_cat)) >= 2) {
      ggplot(variance_data, aes(x=components, y=proportion, col=source)) + 
        geom_line() + geom_point() + theme(legend.position = "top") + 
        ggtitle(paste0(processedSet$response, " by ", modeledSet$setLabel)) + theme(plot.title=element_text(size=12, face="bold", hjust=0.5))
      ggsave(file.path(modeledSet$outLoc, paste(processedSet$response, "_", modeledSet$setLabel, "_", modelType, "_tuning.png", sep='')),  width=4, height=4)
    }

    ## Build model using optimal number of latent variables on all training data
    y_mat_dev <- juice(prep(multi_rec, training=dev_data), composition="matrix", all_outcomes())
    x_mat_dev <- juice(prep(multi_rec, training=dev_data), composition="matrix", all_predictors())

    pls_format_dev <- data.frame(endpoints=I(y_mat_dev), measurements=I(x_mat_dev))
    # Fit the model
    optimal_number_of_components <- variance_data %>%
                                    dplyr::filter(source != "predictors") %>%
                                    ungroup() %>%
                                    dplyr::filter(proportion == max(proportion)) %>%
                                    dplyr::select(components) %>%
                                    as.numeric()
    mod_dev <- plsr(endpoints ~ measurements, data=pls_format_dev, ncomp=optimal_number_of_components)

    ##Use trained model to predict development data
    y_mat_dev <- juice(prep(multi_rec, training=dev_data), composition = "matrix", all_outcomes())
    x_mat_dev <- juice(prep(multi_rec, training=dev_data), composition = "matrix", all_predictors())

    pls_format_dev <- data.frame(endpoints=I(y_mat_dev), measurements = I(x_mat_dev)
    )

    dev_preds <- data.frame(predict(mod_dev, pls_format_dev, type=c("response"), ncomp=optimal_number_of_components))
    colnames(dev_preds) <- unlist(response)
    dev_preds <- 
     dev_preds %>%
     mutate(id = row_number())

    dev_preds_long <- gather(dev_preds, key="Response", value="Pred", -id)

    dev_obs_long <- y_mat_dev %>%
                    as.data.frame() %>%
                    dplyr::select(unlist(response)) %>%
                    mutate(id = row_number()) %>%
                    gather(key="Response", value="Obs", -id)

    dev_obs_vs_pred_response <- full_join(dev_obs_long, dev_preds_long, by=c("id", "Response"))

    ggplot(data=dev_obs_vs_pred_response, aes(x=Obs, y=Pred)) + geom_point(color="darkblue") + xlim(plot_range) + ylim(plot_range) + 
      facet_wrap(~Response) +
      coord_fixed(ratio=1) + geom_abline(intercept=0, slope=1, color="darkgrey", linetype="dashed") + stat_smooth(method=lm, se=FALSE) + 
      xlab("Observed") + ylab("Predicted") +
      ggtitle("celebrate that we have a multi-resp model") + theme(plot.title=element_text(size=12, face="bold", hjust=0.5))
    ggsave(filePath, width=4, height=4)
  }

  #####
  # Return the results
  return(modeledSet)
}
