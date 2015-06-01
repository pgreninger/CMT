rm(list = ls());

tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self"))
{
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

toc <- function()
{
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc - tic)
  invisible(toc)
}

# setwd("C:/Users/Pat/Documents/_Projects/Engelman/DrugScreen")
# source("CurveFit.R")

library(drc)
library(pracma) # for AUC

## Dose response curve may fail to converge. Try fitting in order
## 1) 4-parameter curve
## 2) 5-parameter curve
## 3) linear model
## 
## Input is data frame with first column containing the fraction of control
## and second column containing the corresponding dose in uM.
##
## Returns:
##    m   - the model that was fit
##    fit - a text string indicating the type of model ("LL4", "LL5" or "LINEAR")

test.fit<-function(dataFr){
  x<-try(drm(response ~ dose, data = dataFr, fct = (LL.4(names = c("hs", "bottom", "top", "ec50"))), lowerl=(c(-Inf, 0, 0, -Inf))), silent=TRUE)
  if (class(x) != "try-error"){
    return( list(m = x, fit="LL4") )
  }
  
  x<-try(drm(response ~ dose, data = dataFr, fct = (LL.5(names = c("hs", "bottom", "top", "ec50", "exponent"))), lowerl=(c(-Inf, 0, 0, -Inf, 1))), silent=TRUE)
  if (class(x) != "try-error"){
    return( list(m = x, fit="LL5") )
  }
  
  m <- lm(response ~ dose)
  return( list(m = m, fit="LINEAR") )
}


## Returns:
##    FIT_SLOPE_SIGN - "POSITIVE" or "NEGATIVE"
##    IC50           - IC50 concentration or NaN if IC50 could not be determined
##    LABEL_IC50     - "INSIDE_TESTED_RANGE" if IC50 could be determined from dose respnse,
##                     "OUTSIDE_TESTED_RANGE" if IC50 is extrapolated,
##                     "NO_VALUE" if linear model
##    EC50           - model coefficient "ec50:(Intercept)" for drc
##                     for lm, ((first predicted value + last predicted value)/2 - intercept coefficient)/dose coefficient
##    TOP            - model coefficient "top:(Intercept)" for drc, NaN for lm
##    BOTTOM         - model coefficient "bottom:(Intercept)" for drc, NaN for lm
##    HS             - model coefficient "hs:(Intercept)" for drc, dose coefficient for lm
##    AUC            - normalized AUC calculated using trapz(newdata, modelCurve)/(highest dose - lowest dose)
getModelParams<-function(model, predictions=10000){
  require(drc)
  require(pracma)
  if (class(model) == "lm")
  {
    dose<- model$model[,2]
    newdata = data.frame(independent_variable = seq(dose[1], dose[length(dose)], length.out=length(dose)))
    return(getLMParams(model, newdata))
  }
  else
  {
    dose<-model$data[,1]
    newdata = data.frame(independent_variable = seq(dose[1], dose[length(dose)], length.out=predictions))
    return(getDRCParams(model, newdata))
  }
}

getLMParams<-function(model, newdata)
{
  modelCurve<-predict(model, newdata)
  dose<- model$model[,2]
  
  FIT_SLOPE_SIGN <- ifelse(model$coefficients["dose"] > 0, "POSITIVE", "NEGATIVE") 
  # solve y = ax + b for x when y = 0.5
  IC_50<- (0.5 - model$coefficients["(Intercept)"])/model$coefficients["dose"]
  LABEL_IC50 <- ifelse( (min(modelCurve) < 0.5 && max(modelCurve) > 0.5), "INSIDE_TESTED_RANGE", "OUTSIDE_TESTED_RANGE")
  IC50_CAPPED <- ifelse( (min(modelCurve) < 0.5 && max(modelCurve) > 0.5), IC_50, dose[length(dose)])
  AUC <- trapz(newdata[,1], modelCurve) / (dose[length(dose)]-dose[1])
  EC50 <- ((modelCurve[1] + modelCurve[length(modelCurve)])/2 - model$coefficients["(Intercept)"]) / model$coefficients["dose"]
  TOP <- NaN
  BOTTOM <- NaN
  HS <- model$coefficients["dose"]
  return (list("slope_sign" = FIT_SLOPE_SIGN, "ic50"=IC_50, "ic50_capped"=IC50_CAPPED, "ic50_label"=LABEL_IC50, "auc"=AUC, "ec50"=EC50, "top"=TOP, "bottom"=BOTTOM, "hs"=HS))
}

getDRCParams<-function(model, newdata)
{
  modelCurve<-predict(model, newdata)
  dose<-model$data[,1]
  FIT_SLOPE_SIGN<-ifelse(model$coefficients["hs:(Intercept)"] < 0, "POSITIVE", "NEGATIVE")
  
  # IC50 within tested range
  if (min(modelCurve) <= 0.5 && max(modelCurve) >= 0.5)
  {
    # get index of result that is closest to 0.5
    ic50_result<-which.min(abs(0.5 - modelCurve))
    # get dose that corresponds to result closest to IC50
    IC50<-newdata[ic50_result,]
    LABEL_IC50<-"INSIDE_TESTED_RANGE"
    IC50_CAPPED = IC50;
  }
  else
  {
    ## what if top intercept is < 0.5? SOLVED
    if ((model$coefficients["bottom:(Intercept)"] > 0.5) || (model$coefficients["top:(Intercept)"] < 0.5))
    {
      IC50<-NaN
      LABEL_IC50<-"NO_VALUE"
      IC50_CAPPED = dose[length(dose)];
    }
    else  # model$coefficients["bottom:(Intercept)"] <= 0.5
    {
      # extrapolate IC50?
      if (modelCurve[length(modelCurve)] > modelCurve[1])
      {
        if (modelCurve[1] > 0.5) 
        {
          independent_variable<-seq(0, dose[1], length.out=nrow(newdata))
        }
        else
        {
          independent_variable<-seq(dose[length(dose)], nrow(newdata)*dose[length(dose)], length.out=nrow(newdata)) 
        }
      }
      else
      {
        if (modelCurve[1] < 0.5)
        {
          independent_variable<-seq(0, dose[1], length.out=nrow(newdata))
        }
        else
        {
          independent_variable<-seq(dose[length(dose)], nrow(newdata)*dose[length(dose)], length.out=nrow(newdata))  
        }
      }
      extrapolated_IC50 = predict(model, newdata = data.frame(independent_variable)) 
      ic50_result<-which.min(abs(0.5 - extrapolated_IC50))
      IC50<-independent_variable[ic50_result]
      LABEL_IC50 = 'OUTSIDE_TESTED_RANGE';
      IC50_CAPPED = dose[length(dose)];
    }
  }
  AUC<-trapz(newdata[,1], modelCurve) / (dose[length(dose)]-dose[1])
  EC50<-model$coefficients["ec50:(Intercept)"]
  TOP<-model$coefficients["top:(Intercept)"]
  BOTTOM<-model$coefficients["bottom:(Intercept)"]
  HS<-model$coefficients["hs:(Intercept)"]
  return (list("slope_sign" = FIT_SLOPE_SIGN, "ic50"=IC50, "ic50_capped"=IC50_CAPPED, "ic50_label"=LABEL_IC50, "auc"=AUC, "ec50"=EC50, "top"=TOP, "bottom"=BOTTOM, "hs"=HS))
}

###################################################################################################################################################################
## END OF DEFINE SOME FUNCTIONS

library(ggplot2) # for plots
library(scales)
library(directlabels)
library(gridExtra);
require(gplots);
library(reshape);

## ADD A LIST OF IMAGES (PAIRS OF CELL-LINES AND DRUGS) THAT YOU WISH TO PLOT:
source("/Work_September/ENGELMAN_CURVE_FITTING_IULIAN/"); 
setwd("/Work_September/ENGELMAN_CURVE_FITTING_IULIAN/"); # working directory!
list1 <- list.files(pattern = "refit_EngelmanRawData_thru_20150106.txt"); # it reads any .txt files (they supposedly contain drug response data)
data1 = read.table(list1, sep = "\t", header = TRUE);
# Lapatinib CompoundNo = 119;
FINAL_LIST_RANDOM_LAPATINIB = which(data1$CompoundNo == 119);
rm(list1, data1);

NUM = length(FINAL_LIST_RANDOM_LAPATINIB); #number of plots to create!

source("/Work_September/ENGELMAN_CURVE_FITTING_IULIAN"); 
setwd("/Work_September/ENGELMAN_CURVE_FITTING_IULIAN"); # working directory!
list1 <- list.files(pattern = "EngelmanRawData"); # it reads any .txt files (they supposedly contain drug response data)
data1 = read.table(list1[1], sep = "\t", header = TRUE);
data_temp = rev(data1[,7:15]); data1[,7:15] = data_temp; rm(data_temp); # set response data in decreasing order (as seen on a curve fitting plot)
TEMP = data1[,7:15];
TEMP_NORM = c();
for (i in 1:dim(TEMP)[1])
{
  TEMP_NORM = rbind(TEMP_NORM, TEMP[i,]/mean(as.numeric(data1[i,16]))); # compute the mean value from the positive 'pos' controls
}
rm(i);
data2 = cbind(data1[,1:6], TEMP_NORM, data1[,16:19]);
rm(data1); data1 = data2; rm(data2); rm(TEMP_NORM); rm(TEMP);
colnames(data1)[7:15] = c("RESPONSE1","RESPONSE2","RESPONSE3","RESPONSE4","RESPONSE5","RESPONSE6","RESPONSE7","RESPONSE8","RESPONSE9"); # compound concentration (in micro M)

## ADD DRUG CONCENTRATIONS!
data2 = read.table('DRUG_CONCENTRATIONS.txt', sep = "\t", header = TRUE);
data_temp = c();
for (cl in 1:dim(data1)[1])     # for each drug combination - cell-line pair:
{
  cat(cl,"\n");
  text1 = data1$CompoundNo[cl];
  row = which(data2$CompoundNo %in% text1);
  if(length(row) == 9)
  {
    data_temp = rbind(data_temp, rev(as.numeric(t(data2$CompoundConc[row]))));  
  } else {
    data_temp = rbind(data_temp, rev(rep(0, 9)));  
  }
  rm(text1);
  rm(row);
}
data1 = cbind(data1, data_temp);
colnames(data1)[20:28] = c("CONC1","CONC2","CONC3","CONC4","CONC5","CONC6","CONC7","CONC8","CONC9"); # compound concentration (in micro M)
rm(data_temp);
rm(data2);
rm(cl);

# ACTUAL ANALYSIS!
OLD_DIMENSION = dim(data1)[2];
data1$fit<-NA
data1$slope_sign<-NA
data1$ic50<-NA
data1$ic50_capped<-NA
data1$ic50_label<-NA
data1$auc<-NA
data1$ec50<-NA
data1$top<-NA
data1$bottom<-NA
data1$hs<-NA
data1$overall_residual<-NA

NEW_DIMENSION = dim(data1)[2];
data1$index_rem<-NA
data1$residual_rem<-NA
data1$fit_rem<-NA
data1$slope_sign_rem<-NA
data1$ic50_rem<-NA
data1$ic50_capped_rem<-NA
data1$ic50_label_rem<-NA
data1$auc_rem<-NA
data1$ec50_rem<-NA
data1$top_rem<-NA
data1$bottom_rem<-NA
data1$hs_rem<-NA
data1$overall_residual_rem<-NA
data_fit_plot = data1[FINAL_LIST_RANDOM_LAPATINIB,];

#SORT THE DATA FOR EACH PLOTS:
data_fit_plot_final = c();
unique_celllines = unique(data_fit_plot$Cell.Line.ID);
for (cl in 1:length(unique_celllines))
{
 data_temp = which(data_fit_plot$Cell.Line.ID == unique_celllines[cl]); 
 data_temp2 = data_fit_plot$AnchorID[data_temp];
 data_temp3 = sort(unique(data_temp2));
 if(length(data_temp3) == 2)
 {
  for  (cl2 in 1: length(data_temp3))
  {
   data_fit_plot_final = rbind(data_fit_plot_final, data_fit_plot[data_temp[which(data_temp2 == data_temp3[cl2])[1]],]);
  }
 }
}
rm(data_temp, data_temp2, data_temp3);


for (cl in seq(1, dim(data_fit_plot_final)[1], by = 2))     # for each drug combination - cell-line pair:
{
 #A PLOT THE CURVE OF THE LIBRARY DRUG ALONE, NO ANCHOR!  
 cat(cl,"\n");
 dose = as.numeric(data_fit_plot_final[cl,20:28]); # drug concentration
 response_drug_alone = as.numeric(data_fit_plot_final[cl,7:15]); # drug response (percent control viability)
 dataFr<-data.frame(response=response_drug_alone, dose=dose);
 response = response_drug_alone;
 model_drug_alone <- test.fit(dataFr);
 results <- getModelParams(model_drug_alone$m, predictions=10000);
 data_fit_plot_final[cl,OLD_DIMENSION+1] <- model_drug_alone$fit;
 data_fit_plot_final[cl,(OLD_DIMENSION+2):(OLD_DIMENSION+10)] <- unlist(results);
 data_fit_plot_final[cl, OLD_DIMENSION+11] <- sum(abs(residuals(model_drug_alone$m)));
 IC50_DRUG_ALONE = as.numeric(data_fit_plot_final$ic50_capped[cl]);
 EC50_DRUG_ALONE = as.numeric(data_fit_plot_final$ec50[cl]);
  
 if (class(model_drug_alone$m) == "lm")
 {
  dose<- model_drug_alone$m$model[,2];
  newdata = data.frame(independent_variable = seq(dose[1], dose[length(dose)], length.out=length(dose)));
  #text000 = strcat(c("Cell-line: ", as.character(data1$Cell.Line.ID[cl]), " | Compound_No: ", as.character(data1$CompoundNo[cl]), " | Anchor_ID: ", as.character(data1$AnchorID[cl])));
  text000 = strcat(c("Cell-line: ", as.character(data_fit_plot_final$Cell.Line.ID[cl+1]), " | Compound_No: ", as.character(data_fit_plot_final$CompoundNo[cl+1]), " | Anchor_ID: ", as.character(data_fit_plot_final$AnchorID[cl+1])));
  dataFr_FINAL = c();
  for (clcl in 1:(72/dim(dataFr)[1])) 
  {
   dataFr_FINAL = rbind(dataFr, dataFr_FINAL);
  }
  colnames(dataFr_FINAL)[1:2] = c("y","x");
  p1 <-  ggplot(dataFr_FINAL, aes(x=x, y=y)) + geom_point(shape=1, colour = "black") +
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  #geom_smooth(method = "lm", formula = y ~ poly(x, 4), se=FALSE) + 
  #theme(legend.text=element_text(size=10)) + 
  theme(legend.position = "none") + 
  xlab("Concentration") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size=10)) +
  #scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9")) + 
  #scale_y_continuous(breaks = c(0.2, 0.25, 0.4, 0.5, 0.6, 0.8, 1), limits = c(0, 1.3)) +          
  scale_x_continuous(limits = c(dose[1]-0.1, dose[9]+0.1), trans = 'log2', 
                     breaks = trans_breaks('log2', function(x) 2^x), 
                     labels = trans_format('log2', math_format(2^.x))) +
  annotation_logticks(sides = "tb") +
  theme(axis.text=element_text(size=9,face = "bold"),
        axis.title=element_text(size=10,face = "bold")) +
  ylab("Viability") + 
  #ylim(0, 2) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size=10)) +
  ggtitle(text000) + 
  theme(plot.title = element_text(size = 12,face = "bold", colour="black"))
  p1 <- p1 + geom_abline(intercept = as.numeric(model_drug_alone$m$coefficients["(Intercept)"]), slope = as.numeric(model_drug_alone$m$coefficients["dose"]), colour = "black");
  #p1 <- p1 + geom_point(aes(x = as.numeric(EC50_DRUG_ALONE), y = as.numeric(model_drug_alone$m$coefficients["dose"])*as.numeric(EC50_DRUG_ALONE) + as.numeric(model_drug_alone$m$coefficients["(Intercept)"])), colour = "black", size = 5, shape = 23, bg = "green");
  #p1 <- p1 + geom_point(aes(x = as.numeric(IC50_DRUG_ALONE), y = as.numeric(model_drug_alone$m$coefficients["dose"])*as.numeric(IC50_DRUG_ALONE) + as.numeric(model_drug_alone$m$coefficients["(Intercept)"])), colour = "black", size = 5, shape = 23, bg = "red"); 
 } else {
         dose<-model_drug_alone$m$data[,1];
         newdata = data.frame(independent_variable = seq(dose[1], dose[length(dose)], length.out=10000));
         #text000 = strcat(c("Cell-line: ", as.character(data1$CellID[cl]), " | Compound_No: ", as.character(data1$Compound[cl]), " | Anchor_ID: ", as.character(data1$AnchorID[cl])));
         text000 = strcat(c("Cell-line: ", as.character(data_fit_plot_final$Cell.Line.ID[cl+1]), " | Compound_No: ", as.character(data_fit_plot_final$CompoundNo[cl+1]), " | Anchor_ID: ", as.character(data_fit_plot_final$AnchorID[cl+1])));
         dataFr_FINAL = c();
         for (clcl in 1:(72/dim(dataFr)[1])) 
         {
          dataFr_FINAL = rbind(dataFr, dataFr_FINAL);
         }
         colnames(dataFr_FINAL)[1:2] = c("y","x");
         p1 <-  ggplot(dataFr_FINAL, aes(x=x, y=y)) + geom_point(shape=1, colour = "black") +
         #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
         #geom_smooth(method = "lm", formula = y ~ poly(x, 4), se=FALSE) + 
         #theme(legend.text=element_text(size=10)) + 
         theme(legend.position = "none") + 
         xlab("Concentration") + 
         theme(axis.text.x = element_text(angle = 0, hjust = 1, size=10)) +
         #scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9")) + 
         #scale_y_continuous(breaks = c(0.2, 0.25, 0.4, 0.5, 0.6, 0.8, 1), limits = c(0, 1.3)) +          
         scale_x_continuous(limits = c(dose[1]-0.1, dose[9]+0.1), trans = 'log2', 
                            breaks = trans_breaks('log2', function(x) 2^x), 
                            labels = trans_format('log2', math_format(2^.x))) +
         annotation_logticks(sides = "tb") +
         theme(axis.text=element_text(size=9,face = "bold"),
               axis.title=element_text(size=10,face = "bold")) +
         ylab("Viability") + 
         #ylim(0, 2) + 
         theme(axis.text.y = element_text(angle = 0, hjust = 1, size=10)) +
         ggtitle(text000) + 
         theme(plot.title = element_text(size = 12,face = "bold", colour="black"))
         p1 <- p1 + geom_line(data = data.frame(x = seq(dose[1], dose[9], length.out=72), y = predict(model_drug_alone$m, newdata = data.frame(independent_variable = seq(dose[1], dose[9], length.out=72)))), size = 1, colour = "black");
         #p1 <- p1 + geom_point(aes(x = as.numeric(EC50_DRUG_ALONE), y = predict(model_drug_alone$m, newdata = data.frame(independent_variable = as.numeric(EC50_DRUG_ALONE)))), colour = "black", size = 5, shape = 23, bg = "green");
         #p1 <- p1 + geom_point(aes(x = as.numeric(IC50_DRUG_ALONE), y = predict(model_drug_alone$m, newdata = data.frame(independent_variable = as.numeric(IC50_DRUG_ALONE)))), colour = "black", size = 5, shape = 23, bg = "red");
        }
 
 #AA REMOVE LARGEST RESIDUAL/POSSIBLE OUTLIER:
 index_outlier_drug_alone = which.max(abs(residuals(model_drug_alone$m)));
 dataFr_rem <- dataFr[-index_outlier_drug_alone,];
 model_drug_alone_rem <- test.fit(dataFr_rem);
 results_rem <- getModelParams(model_drug_alone_rem$m, predictions=10000);
 
 data_fit_plot_final[cl,(NEW_DIMENSION+1):(NEW_DIMENSION+3)] <- c(index_outlier_drug_alone, residuals(model_drug_alone_rem$m)[index_outlier_drug_alone], model_drug_alone_rem$fit);
 if(results_rem[4] != "INSIDE_TESTED_RANGE")
 {
   results_rem[3] = results[3];
 }
 data_fit_plot_final[cl,(NEW_DIMENSION+4):(NEW_DIMENSION+12)] <- unlist(results_rem);
 data_fit_plot_final[cl, NEW_DIMENSION+13] <- sum(abs(residuals(model_drug_alone_rem$m)));
 IC50_DRUG_ALONE_REM = as.numeric(data_fit_plot_final$ic50_capped_rem[cl]);
 EC50_DRUG_ALONE_REM = as.numeric(data_fit_plot_final$ec50_rem[cl]);
 
 if (class(model_drug_alone_rem$m) == "lm")
 {
   dose_rem<- model_drug_alone_rem$m$model[,2];
   newdata_rem = data.frame(independent_variable = seq(dose_rem[1], dose_rem[length(dose_rem)], length.out=length(dose_rem)));
   #text000 = strcat(c("Cell-line: ", as.character(data_fit_plot_final$Cell.Line.ID[cl]), " | Compound_No: ", as.character(data_fit_plot_final$CompoundNo[cl]), " | Anchor_ID: ", as.character(data_fit_plot_final$AnchorID[cl])));
   dataFr_FINAL_REM = c();
   for (clcl in 1:(72/dim(dataFr_rem)[1])) 
   {
     dataFr_FINAL_REM = rbind(dataFr_rem, dataFr_FINAL_REM);
   }
   colnames(dataFr_FINAL_REM)[1:2] = c("y","x");
   #p1 <- p1 + geom_point(aes(x = dataFr_FINAL_REM$x, y = dataFr_FINAL_REM$y), shape = 1, colour = "black");
   #p1 <- p1 + geom_point(aes(x = as.numeric(dose[index_outlier_drug_alone]), y = as.numeric(response[index_outlier_drug_alone])), colour = "black", shape = 2, size = 5); # mark the outlier
   p1 <- p1 + geom_abline(intercept = as.numeric(model_drug_anchor$m$coefficients["(Intercept)"]), slope = as.numeric(model_drug_anchor$m$coefficients["dose"]), colour = "black", linetype = "dotdash"); 
   #p1 <- p1 + geom_point(aes(x = as.numeric(EC50_REM), y = as.numeric(model_rem$m$coefficients["dose"])*as.numeric(EC50_REM) + as.numeric(model_rem$m$coefficients["(Intercept)"])), colour = "red", size = 4, shape = 10);
   #p1 <- p1 + geom_point(aes(x = as.numeric(IC50_REM), y = as.numeric(model_rem$m$coefficients["dose"])*as.numeric(IC50_REM) + as.numeric(model_rem$m$coefficients["(Intercept)"])), colour = "green", size = 4, shape = 10); 
 } else {
         dose_rem<-model_drug_alone_rem$m$data[,1];
         newdata_rem = data.frame(independent_variable = seq(dose_rem[1], dose_rem[length(dose_rem)], length.out=10000));
         #text000 = strcat(c("Cell-line: ", as.character(data_fit_plot_final$CellID[cl]), " | Compound_No: ", as.character(data_fit_plot_final$Compound[cl]), " | Anchor_ID: ", as.character(data_fit_plot_final$AnchorID[cl])));
         dataFr_FINAL_REM = c();
         for (clcl in 1:(72/dim(dataFr_rem)[1])) 
         {
          dataFr_FINAL_REM = rbind(dataFr_rem, dataFr_FINAL_REM);
         }
         colnames(dataFr_FINAL_REM)[1:2] = c("y","x");
         #p1 <- p1 + geom_point(aes(x = dataFr_FINAL_REM$x, y = dataFr_FINAL_REM$y), shape = 1, colour = "black");
         #p1 <- p1 + geom_point(aes(x = as.numeric(dose[index_outlier_drug_alone]), y = as.numeric(response[index_outlier_drug_alone])), colour = "black", shape = 2, size = 5); # mark the outlier
         p1 <- p1 + geom_line(data = data.frame(x = seq(dose_rem[1], dose_rem[8], length.out=72), y = predict(model_drug_alone_rem$m, newdata = data.frame(independent_variable = seq(dose_rem[1], dose_rem[8], length.out=72)))), size = 1, linetype = "dotdash");
         #p1 <- p1 + geom_point(aes(x = as.numeric(EC50_REM), y = predict(model_rem$m, newdata = data.frame(independent_variable = as.numeric(EC50_REM)))), colour = "red", size = 4, shape = 10);
         #p1 <- p1 + geom_point(aes(x = as.numeric(IC50_REM), y = predict(model_rem$m, newdata = data.frame(independent_variable = as.numeric(IC50_REM)))), colour = "green", size = 4, shape = 10);
        }
 
 
 
 #B PLOT THE CURVE OF THE LIBRARY DRUG AND THE ANCHOR! 
 cl = cl + 1;
 cat(cl,"\n");
 dose = as.numeric(data_fit_plot_final[cl,20:28]); # drug concentration
 response_drug_anchor = as.numeric(data_fit_plot_final[cl,7:15]); # drug response (percent control viability)
 dataFr<-data.frame(response=response_drug_anchor, dose=dose);
 response = response_drug_anchor;
 model_drug_anchor <- test.fit(dataFr);
 results <- getModelParams(model_drug_anchor$m, predictions=10000);
 data_fit_plot_final[cl,OLD_DIMENSION+1] <- model_drug_anchor$fit;
 data_fit_plot_final[cl,(OLD_DIMENSION+2):(OLD_DIMENSION+10)] <- unlist(results);
 data_fit_plot_final[cl, OLD_DIMENSION+11] <- sum(abs(residuals(model_drug_anchor$m)));
 IC50_DRUG_ANCHOR = as.numeric(data_fit_plot_final$ic50_capped[cl]);
 EC50_DRUG_ANCHOR = as.numeric(data_fit_plot_final$ec50[cl]);
 
 if (class(model_drug_anchor$m) == "lm")
 {
  dose<- model_drug_anchor$m$model[,2];
  newdata = data.frame(independent_variable = seq(dose[1], dose[length(dose)], length.out=length(dose)));
  #text000 = strcat(c("Cell-line: ", as.character(data1$Cell.Line.ID[cl]), " | Compound_No: ", as.character(data1$CompoundNo[cl]), " | Anchor_ID: ", as.character(data1$AnchorID[cl])));
  #text000 = strcat(c("Cell-line: ", as.character(data_fit_plot_final$Cell.Line.ID[cl]), " | Compound_No: ", as.character(data_fit_plot_final$CompoundNo[cl]), " | Anchor_ID: ", as.character(data_fit_plot_final$AnchorID[cl])));
  dataFr_FINAL = c();
  for (clcl in 1:(72/dim(dataFr)[1])) 
  {
   dataFr_FINAL = rbind(dataFr, dataFr_FINAL);
  }
  colnames(dataFr_FINAL)[1:2] = c("y","x");
  p1 <- p1 + geom_point(aes(x = dataFr_FINAL$x, y = dataFr_FINAL$y), shape = 1, colour = "orange");
  p1 <- p1 + geom_abline(intercept = as.numeric(model_drug_anchor$m$coefficients["(Intercept)"]), slope = as.numeric(model_drug_anchor$m$coefficients["dose"]), colour = "orange");

  #p1 <-  ggplot(dataFr_FINAL, aes(x=x, y=y)) + geom_point(shape=1, colour = "yellow") +
  ##scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  ##geom_smooth(method = "lm", formula = y ~ poly(x, 4), se=FALSE) + 
  ##theme(legend.text=element_text(size=10)) + 
  #theme(legend.position = "none") + 
  #xlab("Concentration") + 
  #theme(axis.text.x = element_text(angle = 0, hjust = 1, size=10)) +
  ##scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9")) + 
  ##scale_y_continuous(breaks = c(0.2, 0.25, 0.4, 0.5, 0.6, 0.8, 1), limits = c(0, 1.3)) +          
  #scale_x_continuous(limits = c(dose[1]-0.1, dose[9]+0.1), trans = 'log2', 
  #                   breaks = trans_breaks('log2', function(x) 2^x), 
  #                   labels = trans_format('log2', math_format(2^.x))) +
  #annotation_logticks(sides = "tb") +
  #theme(axis.text=element_text(size=9,face = "bold"),
  #      axis.title=element_text(size=10,face = "bold")) +
  #ylab("Viability") + 
  ##ylim(0, 2) +
  #theme(axis.text.y = element_text(angle = 0, hjust = 1, size=10)) +
  #ggtitle(text000) + 
  #theme(plot.title = element_text(size = 12,face = "bold", colour="yellow"))
  #p1 <- p1 + geom_abline(intercept = as.numeric(model$m$coefficients["(Intercept)"]), slope = as.numeric(model$m$coefficients["dose"]), colour = "yellow");
  
  #p1 <- p1 + geom_point(aes(x = as.numeric(EC50_DRUG_ANCHOR), y = as.numeric(model_drug_anchor$m$coefficients["dose"])*as.numeric(EC50_DRUG_ANCHOR) + as.numeric(model_drug_anchor$m$coefficients["(Intercept)"])), colour = "yellow", size = 5, shape = 23, bg = "green");
  #p1 <- p1 + geom_point(aes(x = as.numeric(IC50_DRUG_ANCHOR), y = as.numeric(model_drug_anchor$m$coefficients["dose"])*as.numeric(IC50_DRUG_ANCHOR) + as.numeric(model_drug_anchor$m$coefficients["(Intercept)"])), colour = "yellow", size = 5, shape = 23, bg = "red"); 
 } else {
  dose<-model_drug_anchor$m$data[,1];
  newdata = data.frame(independent_variable = seq(dose[1], dose[length(dose)], length.out=10000));
  #text000 = strcat(c("Cell-line: ", as.character(data1$CellID[cl]), " | Compound_No: ", as.character(data1$Compound[cl]), " | Anchor_ID: ", as.character(data1$AnchorID[cl])));
  #text000 = strcat(c("Cell-line: ", as.character(data_fit_plot_final$Cell.Line.ID[cl]), " | Compound_No: ", as.character(data_fit_plot_final$CompoundNo[cl]), " | Anchor_ID: ", as.character(data_fit_plot_final$AnchorID[cl])));
  dataFr_FINAL = c();
  for (clcl in 1:(72/dim(dataFr)[1])) 
  {
   dataFr_FINAL = rbind(dataFr, dataFr_FINAL);
  }
  colnames(dataFr_FINAL)[1:2] = c("y","x");
  p1 <- p1 + geom_point(aes(x = dataFr_FINAL$x, y = dataFr_FINAL$y), shape = 1, colour = "orange");
  p1 <- p1 + geom_line(data = data.frame(x = seq(dose[1], dose[9], length.out=72), y = predict(model_drug_anchor$m, newdata = data.frame(independent_variable = seq(dose[1], dose[9], length.out=72)))), size = 1, colour = "orange");
  
  #p1 <-  ggplot(dataFr_FINAL, aes(x=x, y=y)) + geom_point(shape=1, colour = "yellow") +
  ##scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  ##geom_smooth(method = "lm", formula = y ~ poly(x, 4), se=FALSE) + 
  ##theme(legend.text=element_text(size=10)) + 
  #theme(legend.position = "none") + 
  #xlab("Concentration") + 
  #theme(axis.text.x = element_text(angle = 0, hjust = 1, size=10)) +
  ##scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9")) + 
  ##scale_y_continuous(breaks = c(0.2, 0.25, 0.4, 0.5, 0.6, 0.8, 1), limits = c(0, 1.3)) +          
  #scale_x_continuous(limits = c(dose[1]-0.1, dose[9]+0.1), trans = 'log2', 
  #                   breaks = trans_breaks('log2', function(x) 2^x), 
  #                   labels = trans_format('log2', math_format(2^.x))) +
  #annotation_logticks(sides = "tb") +
  #theme(axis.text=element_text(size=9,face = "bold"),
  #      axis.title=element_text(size=10,face = "bold")) +
  #ylab("Viability") + 
  ##ylim(0, 2) + 
  #theme(axis.text.y = element_text(angle = 0, hjust = 1, size=10)) +
  #ggtitle(text000) + 
  #theme(plot.title = element_text(size = 12,face = "bold", colour="black"))
  #p1 <- p1 + geom_line(data = data.frame(x = seq(dose[1], dose[9], length.out=72), y = predict(model$m, newdata = data.frame(independent_variable = seq(dose[1], dose[9], length.out=72)))), size = 1, colour = "yellow");
  
  #p1 <- p1 + geom_point(aes(x = as.numeric(EC50_DRUG_ANCHOR), y = predict(model_drug_anchor$m, newdata = data.frame(independent_variable = as.numeric(EC50_DRUG_ANCHOR)))), colour = "yellow", size = 5, shape = 23, bg = "green");
  #p1 <- p1 + geom_point(aes(x = as.numeric(IC50_DRUG_ANCHOR), y = predict(model_drug_anchor$m, newdata = data.frame(independent_variable = as.numeric(IC50_DRUG_ANCHOR)))), colour = "yellow", size = 5, shape = 23, bg = "red");
 } 
 
 
 #BB REMOVE LARGEST RESIDUAL/POSSIBLE OUTLIER:
 index_outlier_drug_anchor = which.max(abs(residuals(model_drug_anchor$m)));
 dataFr_rem <- dataFr[-index_outlier_drug_anchor,];
 model_drug_anchor_rem <- test.fit(dataFr_rem);
 results_rem <- getModelParams(model_drug_anchor_rem$m, predictions=10000);
 
 data_fit_plot_final[cl,(NEW_DIMENSION+1):(NEW_DIMENSION+3)] <- c(index_outlier_drug_anchor, residuals(model_drug_anchor_rem$m)[index_outlier_drug_anchor], model_drug_anchor_rem$fit);
 if(results_rem[4] != "INSIDE_TESTED_RANGE")
 {
   results_rem[3] = results[3];
 }
 data_fit_plot_final[cl,(NEW_DIMENSION+4):(NEW_DIMENSION+12)] <- unlist(results_rem);
 data_fit_plot_final[cl, NEW_DIMENSION+13] <- sum(abs(residuals(model_drug_anchor_rem$m)));
 IC50_DRUG_ANCHOR_REM = as.numeric(data_fit_plot_final$ic50_capped_rem[cl]);
 EC50_DRUG_ANCHOR_REM = as.numeric(data_fit_plot_final$ec50_rem[cl]);
 
 if (class(model_drug_anchor_rem$m) == "lm")
 {
   dose_rem<- model_drug_anchor_rem$m$model[,2];
   newdata_rem = data.frame(independent_variable = seq(dose_rem[1], dose_rem[length(dose_rem)], length.out=length(dose_rem)));
   #text000 = strcat(c("Cell-line: ", as.character(data_fit_plot_final$Cell.Line.ID[cl]), " | Compound_No: ", as.character(data_fit_plot_final$CompoundNo[cl]), " | Anchor_ID: ", as.character(data_fit_plot_final$AnchorID[cl])));
   dataFr_FINAL_REM = c();
   for (clcl in 1:(72/dim(dataFr_rem)[1])) 
   {
     dataFr_FINAL_REM = rbind(dataFr_rem, dataFr_FINAL_REM);
   }
   colnames(dataFr_FINAL_REM)[1:2] = c("y","x");
   #p1 <- p1 + geom_point(aes(x = dataFr_FINAL_REM$x, y = dataFr_FINAL_REM$y), shape = 1, colour = "black");
   p1 <- p1 + geom_point(aes(x = as.numeric(dose[index_outlier_drug_anchor]), y = as.numeric(response_drug_anchor[index_outlier_drug_anchor])), colour = "orange", shape = 2, size = 5); # mark the outlier
   p1 <- p1 + geom_abline(intercept = as.numeric(model_drug_anchor_rem$m$coefficients["(Intercept)"]), slope = as.numeric(model_drug_anchor_rem$m$coefficients["dose"]), colour = "orange", linetype = "dotdash"); 
   p1 <- p1 + geom_point(aes(x = as.numeric(EC50_DRUG_ANCHOR_REM), y = as.numeric(model_drug_anchor_rem$m$coefficients["dose"])*as.numeric(EC50_DRUG_ANCHOR_REM) + as.numeric(model_drug_anchor_rem$m$coefficients["(Intercept)"])), colour = "orange", size = 4, shape = 23, bg = "green");
   p1 <- p1 + geom_point(aes(x = as.numeric(IC50_DRUG_ANCHOR_REM), y = as.numeric(model_drug_anchor_rem$m$coefficients["dose"])*as.numeric(IC50_DRUG_ANCHOR_REM) + as.numeric(model_drug_anchor_rem$m$coefficients["(Intercept)"])), colour = "orange", size = 4, shape = 23, bg = "red"); 
 } else {
   dose_rem<-model_drug_anchor_rem$m$data[,1];
   newdata_rem = data.frame(independent_variable = seq(dose_rem[1], dose_rem[length(dose_rem)], length.out=10000));
   #text000 = strcat(c("Cell-line: ", as.character(data_fit_plot_final$Cell.Line.ID[cl]), " | Compound_No: ", as.character(data_fit_plot_final$CompoundNo[cl]), " | Anchor_ID: ", as.character(data_fit_plot_final$AnchorID[cl])));
   dataFr_FINAL_REM = c();
   for (clcl in 1:(72/dim(dataFr_rem)[1])) 
   {
     dataFr_FINAL_REM = rbind(dataFr_rem, dataFr_FINAL_REM);
   }
   colnames(dataFr_FINAL_REM)[1:2] = c("y","x");
   #p1 <- p1 + geom_point(aes(x = dataFr_FINAL_REM$x, y = dataFr_FINAL_REM$y), shape = 1, colour = "black");
   
   p1 <- p1 + geom_point(aes(x = as.numeric(dose[index_outlier_drug_anchor]), y = as.numeric(response_drug_anchor[index_outlier_drug_anchor])), colour = "orange", shape = 2, size = 5); # mark the outlier
   p1 <- p1 + geom_line(data = data.frame(x = seq(dose_rem[1], dose_rem[8], length.out=72), y = predict(model_drug_anchor_rem$m, newdata = data.frame(independent_variable = seq(dose_rem[1], dose_rem[8], length.out=72)))), size = 1, colour = "orange", linetype = "dotdash");
   p1 <- p1 + geom_point(aes(x = as.numeric(EC50_DRUG_ANCHOR_REM), y = predict(model_drug_anchor_rem$m, newdata = data.frame(independent_variable = as.numeric(EC50_DRUG_ANCHOR_REM)))), colour = "orange", size = 4, shape = 23, bg = "green");
   p1 <- p1 + geom_point(aes(x = as.numeric(IC50_DRUG_ANCHOR_REM), y = predict(model_drug_anchor_rem$m, newdata = data.frame(independent_variable = as.numeric(IC50_DRUG_ANCHOR_REM)))), colour = "orange", size = 4, shape = 23, bg = "red");
 }
 
 
 #REPLOT THE IC50 AND EC50 VALUES (ORIGINAL DRUG_ALONE VALUES) THAT SOMEHOW GOT SHIFTED AFTER OTHER PLOTS:
 if (class(model_drug_alone$m) == "lm")
 {
  p1 <- p1 + geom_point(aes(x = as.numeric(EC50_DRUG_ALONE), y = as.numeric(model_drug_alone$m$coefficients["dose"])*as.numeric(EC50_DRUG_ALONE) + as.numeric(model_drug_alone$m$coefficients["(Intercept)"])), colour = "black", size = 6, shape = 23, bg = "green");
  p1 <- p1 + geom_point(aes(x = as.numeric(IC50_DRUG_ALONE), y = as.numeric(model_drug_alone$m$coefficients["dose"])*as.numeric(IC50_DRUG_ALONE) + as.numeric(model_drug_alone$m$coefficients["(Intercept)"])), colour = "black", size = 6, shape = 23, bg = "red");  
 } else {
         p1 <- p1 + geom_point(aes(x = as.numeric(EC50_DRUG_ALONE), y = predict(model_drug_alone$m, newdata = data.frame(independent_variable = as.numeric(EC50_DRUG_ALONE)))), colour = "black", size = 6, shape = 23, bg = "green");
         p1 <- p1 + geom_point(aes(x = as.numeric(IC50_DRUG_ALONE), y = predict(model_drug_alone$m, newdata = data.frame(independent_variable = as.numeric(IC50_DRUG_ALONE)))), colour = "black", size = 6, shape = 23, bg = "red");
        }
 
 #REPLOT THE IC50 AND EC50 VALUES (ORIGINAL DRUG_ALONE VALUES AFTER OUTLIER REMOVAL) THAT SOMEHOW GOT SHIFTED AFTER OTHER PLOTS:
 if (class(model_drug_alone_rem$m) == "lm")
 {
   p1 <- p1 + geom_point(aes(x = as.numeric(dose[index_outlier_drug_alone]), y = as.numeric(response_drug_alone[index_outlier_drug_alone])), colour = "black", shape = 2, size = 5); # mark the outlier
   p1 <- p1 + geom_point(aes(x = as.numeric(EC50_DRUG_ALONE_REM), y = as.numeric(model_drug_alone_rem$m$coefficients["dose"])*as.numeric(EC50_DRUG_ALONE_REM) + as.numeric(model_drug_alone_rem$m$coefficients["(Intercept)"])), colour = "black", size = 4, shape = 23, bg = "green");
   p1 <- p1 + geom_point(aes(x = as.numeric(IC50_DRUG_ALONE_REM), y = as.numeric(model_drug_alone_rem$m$coefficients["dose"])*as.numeric(IC50_DRUG_ALONE_REM) + as.numeric(model_drug_alone_rem$m$coefficients["(Intercept)"])), colour = "black", size = 4, shape = 23, bg = "red");  
 } else {
         p1 <- p1 + geom_point(aes(x = as.numeric(dose[index_outlier_drug_alone]), y = as.numeric(response_drug_alone[index_outlier_drug_alone])), colour = "black", shape = 2, size = 5); # mark the outlier
         p1 <- p1 + geom_point(aes(x = as.numeric(EC50_DRUG_ALONE_REM), y = predict(model_drug_alone_rem$m, newdata = data.frame(independent_variable = as.numeric(EC50_DRUG_ALONE_REM)))), colour = "black", size = 4, shape = 23, bg = "green");
         p1 <- p1 + geom_point(aes(x = as.numeric(IC50_DRUG_ALONE_REM), y = predict(model_drug_alone_rem$m, newdata = data.frame(independent_variable = as.numeric(IC50_DRUG_ALONE_REM)))), colour = "black", size = 4, shape = 23, bg = "red");
 }
 
 #REPLOT THE IC50 AND EC50 VALUES (ORIGINAL DRUG AND ANCHOR VALUES) THAT SOMEHOW GOT SHIFTED AFTER OTHER PLOTS:
 if (class(model_drug_anchor$m) == "lm")
 {
   p1 <- p1 + geom_point(aes(x = as.numeric(EC50_DRUG_ANCHOR), y = as.numeric(model_drug_anchor$m$coefficients["dose"])*as.numeric(EC50_DRUG_ANCHOR) + as.numeric(model_drug_anchor$m$coefficients["(Intercept)"])), colour = "yellow", size = 6, shape = 23, bg = "green");
   p1 <- p1 + geom_point(aes(x = as.numeric(IC50_DRUG_ANCHOR), y = as.numeric(model_drug_anchor$m$coefficients["dose"])*as.numeric(IC50_DRUG_ANCHOR) + as.numeric(model_drug_anchor$m$coefficients["(Intercept)"])), colour = "yellow", size = 6, shape = 23, bg = "red");  
 } else {
   p1 <- p1 + geom_point(aes(x = as.numeric(EC50_DRUG_ANCHOR), y = predict(model_drug_anchor$m, newdata = data.frame(independent_variable = as.numeric(EC50_DRUG_ANCHOR)))), colour = "yellow", size = 6, shape = 23, bg = "green");
   p1 <- p1 + geom_point(aes(x = as.numeric(IC50_DRUG_ANCHOR), y = predict(model_drug_anchor$m, newdata = data.frame(independent_variable = as.numeric(IC50_DRUG_ANCHOR)))), colour = "yellow", size = 6, shape = 23, bg = "red");
 }
 
 
 if(exists('p1'))
 {
  setwd("/Work_September/ENGELMAN_CURVE_FITTING_IULIAN/PLOTS_LAPATINIB"); # working directory!
  text000 = strcat(c("Cell-line: ", as.character(data_fit_plot_final$Cell.Line.ID[cl]), " | Compound_No: ", as.character(data_fit_plot_final$CompoundNo[cl]), " | Anchor_ID: ", as.character(data_fit_plot_final$AnchorID[cl])));
  text0000 = strcat(text000, ".jpg");
  jpeg(text0000, res = 250, width = 3000, height = 2000);
  grid.arrange(p1, ncol=1);
  dev.off()
  setwd("/Work_September/ENGELMAN_CURVE_FITTING_IULIAN/"); # working directory!
 }
}
#Rearrange the columns in the output file:
data_fit_plot_final_rearranged = data_fit_plot_final[,c(1:4, 29, 30, 33, 40, 41, 39, 42, 43, 46, 52, 5:28, 31, 32, 34:38, 44, 45, 47:51)];
setwd("/Work_September/ENGELMAN_CURVE_FITTING_IULIAN/PLOTS_LAPATINIB"); # working directory!
write.table(data_fit_plot_final_rearranged, file=paste("plot_LAPATINIB_refit_",list1[1], sep=""), sep = "\t", row.names = FALSE, quote = FALSE, col.names = TRUE);
setwd("/Work_September/ENGELMAN_CURVE_FITTING_IULIAN/"); # working directory!
toc();