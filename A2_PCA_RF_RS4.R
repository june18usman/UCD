#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Load Necessary Libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(caret)      # For model training and evaluation
library(randomForest) # For Random Forest modeling
library(ggplot2)    # For visualizations
library(tidyverse)   # For data manipulation
library(pROC)
library(PresenceAbsence)
library(tidyverse)
library(writexl)
library(readxl)


# Set random seed for reproducibility
set.seed(123)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load and Prepare the Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the working directory
setwd("C:/Users/musman/Documents/R Files for Final Results/FINAL CODE")

# Load datasets
AfterPCA_AllCowsX <- read.csv("AfterPCA_AllCowsX.csv", header = TRUE) 
CombConorAll <- read.csv("CombConorAll.csv", header = TRUE)

# Combine the dataset with target variable 'High' (lameness status)
AllCowsData_NRX <- cbind(AfterPCA_AllCowsX, CombConorAll$High)
colnames(AllCowsData_NRX)[ncol(AllCowsData_NRX)] <- "High"

# Convert the target variable to a factor
AllCowsData_NRX$High <- factor(AllCowsData_NRX$High)

# Create 5-fold cross-validation
folds <- createFolds(AllCowsData_NRX$High, k = 5, returnTrain = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Set up Cross-Validation Strategy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
train_control <- trainControl(
  method = "cv",                  # Cross-validation method
  number = 5,                     # 5-fold cross-validation
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize Empty Lists for Metrics and Results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_predicted_probabilities <- list()
all_observed_outcomes <- list()
accuracy_list <- c()
sensitivity_list <- c()
specificity_list <- c()
pos_pred_value_list <- c()
neg_pred_value_list <- c()
auc_list <- c()
balanced_accuracy_list <- c()
calibration_error_list <- c()


# Helper function for splitting features and labels
split_features_labels <- function(data) {
  features <- data[, -ncol(data)]
  labels <- data$High
  return(list(features = features, labels = labels))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Perform Cross-Validation and Model Training
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (i in seq_along(folds)) {
  train_indices <- folds[[i]]  # Training indices for the current fold
  train_data <- AllCowsData_NRX[train_indices, ]
  test_data <- AllCowsData_NRX[-train_indices, ]
  
  train_features <- train_data[, -ncol(train_data)]
  train_labels <- train_data$High
  test_features <- test_data[, -ncol(test_data)]
  test_labels <- test_data$High
  
  train_split <- split_features_labels(train_data)
  test_split <- split_features_labels(test_data)
  
  
  # Train the Random Forest model using the training data
  model <- randomForest(
    x = train_features, 
    y = train_labels, 
    #ntree = 100,          # Use the number of trees as specified
    #mtry = 3,             # Use the number of variables for each split (can be tuned)
    #nodesize = 5,          # Set the minimum size of nodes (can be tuned)
    ntree = 250,          # Use the number of trees as specified
    mtry = 10,             # Use the number of variables for each split (can be tuned)
    nodesize = 5         # Set the minimum size of nodes (can be tuned)
    
  )
  
  # Predict probabilities and convert them to classes
  predicted_probabilities <- predict(model, newdata = test_data, type = "prob")[, "1"]
  predicted_classes <- ifelse(predicted_probabilities > 0.25, 1, 0)
  
  # Confusion matrix to evaluate the model performance
  conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$High), positive = "1")
  
  
  # Store metrics for each fold
  accuracy_list <- c(accuracy_list, conf_matrix$overall["Accuracy"])
  sensitivity_list <- c(sensitivity_list, conf_matrix$byClass["Sensitivity"])
  specificity_list <- c(specificity_list, conf_matrix$byClass["Specificity"])
  pos_pred_value_list <- c(pos_pred_value_list, conf_matrix$byClass["Pos Pred Value"])
  neg_pred_value_list <- c(neg_pred_value_list, conf_matrix$byClass["Neg Pred Value"])
  
    # AUC calculation: Skip if the test set is single-class
  true_labels <- as.numeric(as.character(test_split$labels))
  
  if (length(unique(true_labels)) > 1) {
    roc_curve <- pROC::roc(response = true_labels, predictor = predicted_probabilities)
    auc_value <- pROC::auc(roc_curve)
    auc_list <- c(auc_list, auc_value)  # Append to AUC list
  } else {
    print("Skipping AUC calculation for this fold due to single-class data.")
  }
  
  balanced_accuracy <- (conf_matrix$byClass["Sensitivity"] + conf_matrix$byClass["Specificity"]) / 2
  balanced_accuracy_list <- c(balanced_accuracy_list, balanced_accuracy)
  
  abs_calibration_error <- mean(abs(predicted_probabilities - as.numeric(as.character(test_split$labels))), na.rm = TRUE)
  calibration_error_list <- c(calibration_error_list, abs_calibration_error)
  
  
  # Store probabilities and observed outcomes for calibration plot
  all_predicted_probabilities[[i]] <- predicted_probabilities
  all_observed_outcomes[[i]] <- as.numeric(as.character(test_labels))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summarize Metrics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
format_result <- function(values) {
  avg <- round(mean(values, na.rm = TRUE) * 100, 2)
  std <- round(sd(values, na.rm = TRUE) * 100, 2)
  return(paste0(avg, "% (", std, "%)"))
}

cat("Accuracy:", format_result(accuracy_list), "\n")
cat("Sensitivity:", format_result(sensitivity_list), "\n")
cat("Specificity:", format_result(specificity_list), "\n")
cat("Pos Pred Value:", format_result(pos_pred_value_list), "\n")
cat("Neg Pred Value:", format_result(neg_pred_value_list), "\n")
cat("AUC:", format_result(auc_list), "\n")
cat("Balanced Accuracy:", format_result(balanced_accuracy_list), "\n")
cat("Mean Absolute Calibration Error (MACE):", format_result(calibration_error_list), "\n")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WRITING MODEL OUTPUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pred_prob_LR <- unlist(all_predicted_probabilities)
obs_outcome <- unlist(all_observed_outcomes)

# Combine the vectors into a data frame
df <- data.frame(Predicted_Probabilities = pred_prob_LR, Observed_Outcome = obs_outcome)

# Write the data frame to an Excel file
write_xlsx(df, "A2_PCA_RF_RS4.xlsx")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the calibration plot function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print_calibration_plot = function(calibration_prediction_data, bins = 15) {
  calibration_data  = data.frame(
    name = 0,
    real = calibration_prediction_data$event,
    predicted = calibration_prediction_data$predicted
  )
  calibration_data$name = rownames(calibration_data)
  
  # Generate calibration plot data
  calibration_plot_data = calibration.plot(
    calibration_data,
    which.model = 1,
    na.rm = TRUE,
    N.bins = bins
  )
  
  # Print the calibration plot data to inspect it
  print(calibration_plot_data)
  
  # Adjust confidence intervals
  calibration_plot_data$CI_lower = pmax(0, calibration_plot_data$BinObsCIlower - calibration_plot_data$BinObs + calibration_plot_data$BinPred)
  calibration_plot_data$CI_upper = pmin(1, calibration_plot_data$BinObsCIupper - calibration_plot_data$BinObs + calibration_plot_data$BinPred)
  
  # Filter out bins with fewer than 5 samples
  calibration_plot_data = subset(calibration_plot_data, NBin >= 5)
  
  # Create the calibration plot
  calibration_plot = ggplot(data = calibration_plot_data, aes(x = BinPred, y = BinObs)) +
    geom_point() +
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
    xlab("Predicted") +
    ylab("Observed") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = paste(seq(0, 100, 10), "%", sep = "")) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = paste(seq(0, 100, 10), "%", sep = "")) +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          axis.text.x = element_text(color = "black", size = 14),
          axis.text.y = element_text(color = "black", size = 14),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          plot.title = element_text(face = "bold", size = 16),
          panel.grid.major = element_line(colour = "gray60", size = 0.5, linetype = "dashed"),
          panel.grid.major.x = element_blank()) +
    geom_abline(intercept = 0, slope = 1)
  
  return(calibration_plot)
}

# Load Model Output
CR_A2_PCA_RF_RS <- read_excel("A2_PCA_RF_RS4.xlsx")  # CR_A2_PCA_RF_RS: Callibration Read- Apprach 1-no reduction- random forest- Farm Split
CR_A2_PCA_RF_RS$Observed_Outcome <- as.numeric(CR_A2_PCA_RF_RS$Observed_Outcome)
CR_A2_PCA_RF_RS <- CR_A2_PCA_RF_RS %>% select(Observed_Outcome, Predicted_Probabilities)
names(CR_A2_PCA_RF_RS) <- c("event", "predicted")

# Open a PNG device to save the plot
png(filename = "C_A2_PCA_RF_RS.png", width = 800, height = 600)
# Generate and display the calibration plot
calibration_plot <- print_calibration_plot(CR_A2_PCA_RF_RS, bins = 15)
#print(calibration_plot)
# Close the graphical device
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End of Code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
