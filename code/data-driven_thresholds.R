
library(PresenceAbsence)

DATA <- data.frame(
  ID = 1:nrow(data),
  presence = data$presence, 
  RF_pred = preds_RF, 
  BRT_pred = preds_BRT, 
  MAX_pred = preds_MAX, 
  GLM_pred = preds_GLM, 
  GAM_pred = preds_GAM
  )

head(DATA)

model_evaluation_table <- presence.absence.accuracy(DATA)  # I do this with my extract_metrics() function
model_evaluation_table

# Add Ensemble prediction
DATA$ENS_pred <- preds_ENS
presence_absence_thresholds <- optimal.thresholds(DATA)
presence_absence_thresholds

# Save as csv
write.csv(
  presence_absence_thresholds,
  file.path(dir_tables, paste0("binary_model_optimal_thresholds_R.csv")),
  row.names = FALSE
)



# Selecting method to optimise:
# 1	  Default	threshold=0.5
# 2	  Sens=Spec	sensitivity=specificity (balancing true positive and true negative rates)
# 3	  MaxSens+Spec	maximizes (sensitivity+specificity)/2
# 4	  MaxKappa	maximizes Kappa
# 5   MaxPCC	maximizes PCC (percent correctly classified)
# 6	  PredPrev=Obs	predicted prevalence=observed prevalence
# 7	  ObsPrev	threshold=observed prevalence
# 8	  MeanProb	mean predicted probability
# 9	  MinROCdist	minimizes distance between ROC plot and (0,1)
# 10	ReqSens	user defined required sensitivity
# 11	ReqSpec	user defined required specificity
# 12	Cost	user defined relative costs ratio

# Chooses the threshold that maximizes the average of sensitivity and specificity, also known as balanced accuracy.
tht <- as.numeric(presence_absence_thresholds[3,2:7])
presence.absence.accuracy(DATA, threshold=tht)

metrics <- rbind(
  extract_metrics(data$presence, preds_full[["RF"]], thr = tht[1]),
  extract_metrics(data$presence, preds_full[["BRT"]], thr = tht[2]),
  extract_metrics(data$presence, preds_full[["MAX"]], thr = tht[3]),
  extract_metrics(data$presence, preds_full[["GLM"]], thr = tht[4]),
  extract_metrics(data$presence, preds_full[["GAM"]], thr = tht[5]),
  extract_metrics(data$presence, preds_full[["ENS"]], thr = tht[6])
)
metrics


presence.absence.accuracy(DATA, threshold=0.20)

summary(presence_absence_thresholds[-1, ])
