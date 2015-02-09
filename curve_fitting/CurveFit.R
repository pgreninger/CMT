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
## 1) 4-parameter curve (DRC)
## 2) 5-parameter curve (DRC)
## 3) linear model (LM)
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
##    FIT_SLOPE_SIGN      - "POSITIVE" or "NEGATIVE"
##    IC50                - IC50 concentration or NaN if IC50 could not be determined
##    LABEL_IC50          - "INSIDE_TESTED_RANGE" if IC50 could be determined from dose respnse,
##                          "OUTSIDE_TESTED_RANGE" if IC50 is extrapolated,
##                          "NO_VALUE" if linear model
##    EC50                - model coefficient "ec50:(Intercept)" for drc
##                          for lm, ((first predicted value + last predicted value)/2 - intercept coefficient)/dose coefficient
##    TOP                 - model coefficient "top:(Intercept)" for drc, NaN for lm
##    BOTTOM              - model coefficient "bottom:(Intercept)" for drc, NaN for lm
##    HS                  - model coefficient "hs:(Intercept)" for drc, dose coefficient for lm
##    AUC                 - normalized AUC calculated using trapz(newdata, modelCurve)/(highest dose - lowest dose)
##    AUC_ORIGINAL_CONC   - normalized AUC calculated using trapz(dose, response)/(highest dose - lowest dose), ON THE ORIGINAL DRUG CONCENTRATIONS (ORIGINAL CURVE)
##    IC50_CAPPED         - IC50 capped values to either the maximum tested concentration or the minimum tested concentration.
getModelParams<-function(model, predictions=10000, seeding_density)
{
 require(drc);
 require(pracma);
 if (class(model) == "lm")
 {
  dose<- model$model[,2];
  newdata = data.frame(independent_variable = seq(dose[1], dose[length(dose)], length.out=length(dose)));
  return(getLMParams(model, newdata, seeding_density))
 } else {
         dose<-model$data[,1];
         newdata = data.frame(independent_variable = seq(dose[1], dose[length(dose)], length.out=predictions));
         return(getDRCParams(model, newdata, seeding_density))
        }
}

getLMParams<-function(model, newdata, seeding_density)
{
 modelCurve<-predict(model, newdata);
 dose<- model$model[,2];
 response<-model$model[,1];
  
 FIT_SLOPE_SIGN <- ifelse(model$coefficients["dose"] > 0, "POSITIVE", "NEGATIVE");
 # solve y = ax + b for x when y = 0.5
 IC_50<- (0.5 - model$coefficients["(Intercept)"])/model$coefficients["dose"];
 IC_LETHAL<- (seeding_density - model$coefficients["(Intercept)"])/model$coefficients["dose"];
 LABEL_IC50 <- ifelse( (min(modelCurve) < 0.5 && max(modelCurve) > 0.5), "INSIDE_TESTED_RANGE", "OUTSIDE_TESTED_RANGE");
 if(min(modelCurve) < 0.5 && max(modelCurve) > 0.5)
 {
  IC50_CAPPED = IC_50;
 } else if((min(modelCurve) > 0.5 && model$coefficients["dose"] > 0) || (min(modelCurve) < 0.5 && model$coefficients["dose"] < 0))  
   {
    IC50_CAPPED = dose[1];
   } else {
           IC50_CAPPED = dose[length(dose)];
          }
 AUC <- trapz(newdata[,1], modelCurve) / (dose[length(dose)]-dose[1]);
 AUC_ORIGINAL_CONC<-trapz(dose, response) / (dose[length(dose)]-dose[1]);
 EC50 <- ((modelCurve[1] + modelCurve[length(modelCurve)])/2 - model$coefficients["(Intercept)"]) / model$coefficients["dose"];
 TOP <- NaN;
 BOTTOM <- NaN;
 HS <- model$coefficients["dose"];
 return (list("slope_sign" = FIT_SLOPE_SIGN, "ic50"=IC_50, "ic50_capped"=IC50_CAPPED, "ic_lethal"=IC_LETHAL, "ic50_label"=LABEL_IC50, "auc"=AUC, "auc_original_conc" = AUC_ORIGINAL_CONC, "ec50"=EC50, "top"=TOP, "bottom"=BOTTOM, "hs"=HS))
}

getDRCParams<-function(model, newdata, seeding_density)
{
 modelCurve<-predict(model, newdata);
 dose<-model$data[,1];
 response<-model$data[,2];
 FIT_SLOPE_SIGN<-ifelse(model$coefficients["hs:(Intercept)"] < 0, "POSITIVE", "NEGATIVE");
  
 # IC50 within tested range
 if (min(modelCurve) <= 0.5 && max(modelCurve) >= 0.5)
 {
  # get index of result that is closest to 0.5
  ic50_result<-which.min(abs(0.5 - modelCurve));
    
  temp1<-seq(0, nrow(newdata)*dose[length(dose)], length.out=nrow(newdata));
  temp2<-predict(model, newdata = data.frame(temp1));
  temp3<-which.min(abs(seeding_density - temp2));
  independent_variable_lethal<-seq(temp1[temp3]/100, 100*temp1[temp3], length.out = nrow(newdata)); # 10 = FOLD CHANGE WITHING THE RANGE OF THE DRUG CONCENTRATIONS!
  extrapolated_IC_lethal = predict(model, newdata = data.frame(independent_variable_lethal));
  ic_lethal_result<-which.min(abs(seeding_density - extrapolated_IC_lethal));
  IC_LETHAL<-independent_variable_lethal[ic_lethal_result];
    
  # get dose that corresponds to result closest to IC50
  IC50<-newdata[ic50_result,]
  LABEL_IC50<-"INSIDE_TESTED_RANGE"
  IC50_CAPPED = IC50;
 }
 else {
       if ((model$coefficients["bottom:(Intercept)"] > 0.5) || (model$coefficients["top:(Intercept)"] < 0.5))
       {
        IC50<-NaN;
        IC_LETHAL<-NaN;
        LABEL_IC50<-"NO_VALUE";
        if(((model$coefficients["bottom:(Intercept)"] > 0.5) && (model$coefficients["hs:(Intercept)"] < 0)) || ((model$coefficients["top:(Intercept)"] < 0.5) && (model$coefficients["hs:(Intercept)"] > 0)))
        {
         IC50_CAPPED = dose[1];
        } else {
                IC50_CAPPED = dose[length(dose)];
               }
       }
       else { 
             # extrapolate IC50:
             if (modelCurve[length(modelCurve)] > modelCurve[1])
             {
              if (modelCurve[1] > 0.5) 
              {
               independent_variable<-seq(0, dose[1], length.out=nrow(newdata));
               indicator = 1;
              }
              else {
                    independent_variable<-seq(dose[length(dose)], nrow(newdata)*dose[length(dose)], length.out=nrow(newdata)); 
                    indicator = 2;
                   }
             }
             else {
                   if (modelCurve[1] < 0.5)
                   {
                    independent_variable<-seq(0, dose[1], length.out=nrow(newdata));
                    indicator = 1;
                   }
                   else {
                         independent_variable<-seq(dose[length(dose)], nrow(newdata)*dose[length(dose)], length.out=nrow(newdata));
                         indicator = 2;
                        }
                   }
            extrapolated_IC50 = predict(model, newdata = data.frame(independent_variable)); 
            ic50_result<-which.min(abs(0.5 - extrapolated_IC50));
            ic_lethal_result<-which.min(abs(seeding_density - extrapolated_IC50));
            IC50<-independent_variable[ic50_result];
            IC_LETHAL<-independent_variable[ic_lethal_result];
            LABEL_IC50 = 'OUTSIDE_TESTED_RANGE';
            if(indicator == 1)
            {
             IC50_CAPPED = dose[1];
            } else {
                    IC50_CAPPED = dose[length(dose)];
                   }
           }
      }
 AUC<-trapz(newdata[,1], modelCurve) / (dose[length(dose)]-dose[1])
 AUC_ORIGINAL_CONC<-trapz(dose, response) / (dose[length(dose)]-dose[1])
 EC50<-model$coefficients["ec50:(Intercept)"]
 TOP<-model$coefficients["top:(Intercept)"]
 BOTTOM<-model$coefficients["bottom:(Intercept)"]
 HS<-model$coefficients["hs:(Intercept)"]
 return (list("slope_sign" = FIT_SLOPE_SIGN, "ic50"=IC50, "ic50_capped"=IC50_CAPPED, "ic_lethal"=IC_LETHAL, "ic50_label"=LABEL_IC50, "auc"=AUC, "auc_original_conc" = AUC_ORIGINAL_CONC, "ec50"=EC50, "top"=TOP, "bottom"=BOTTOM, "hs"=HS))
}

###################################################################################################################################################################
## END OF DEFINE SOME FUNCTIONS
