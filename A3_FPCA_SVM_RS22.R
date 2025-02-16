#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Necessary Libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(caret)      # For model training and evaluation
library(ggplot2)    # For visualizations
library(tidyverse)  # For data manipulation
library(dplyr)


set.seed(123)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load and Prepare the Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the working directory and load the data
setwd("C:/Users/musman/Documents/R Files for Final Results/FINAL CODE") # Modify this path if necessary

# Load datasets
AfterFPCA_AllCowsX <- read.csv("AfterFPCA_AllCowsX.csv", header = TRUE) 
CombConorAll <- read.csv("CombConorAll.csv", header = TRUE)

# Combine the dataset with target variable 'High' (lameness status)
AllCowsData_NRX <- cbind(AfterFPCA_AllCowsX, CombConorAll$High)
colnames(AllCowsData_NRX)[ncol(AllCowsData_NRX)] <- "High"

# Convert the target variable to a factor with valid names ("No" and "Yes" instead of "0" and "1")
AllCowsData_NRX$High <- factor(AllCowsData_NRX$High, levels = c(0, 1), labels = c("No", "Yes"))

# Create 5-fold cross-validation
folds <- createFolds(AllCowsData_NRX$High, k = 5, returnTrain = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up Cross-Validation Strategy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
train_control <- trainControl(
  method = "cv",                  # Cross-validation method
  number = 5,                     # 5-fold cross-validation
  classProbs = TRUE               # Enable probability predictions
)

# Define a tuning grid for the SVM
tune_grid <- expand.grid(sigma = 0.01, C = 1)  # Adjust values of sigma and C as needed

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Initialize Empty Lists for Metrics and Results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
accuracy_list <- c()
sensitivity_list <- c()
specificity_list <- c()
pos_pred_value_list <- c()
neg_pred_value_list <- c()
all_predicted_probabilities <- list()
all_observed_outcomes <- list()

# Initialize empty lists for additional metrics
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
  
  
  # Train the SVM model with RBF kernel
  model <- train(
    High ~ .,                      # Use all features to predict 'High'
    data = train_data,             # Training data
    method = "svmRadial",          # SVM with RBF kernel
    trControl = train_control,     # Cross-validation settings
    tuneGrid = tune_grid           # Parameter tuning grid
  )

  
  # Predict probabilities and convert them to classes with threshold 0.2
  predicted_probabilities <- predict(model, newdata = test_data, type = "prob")[, "Yes"]
  predicted_classes <- ifelse(predicted_probabilities > 0.25, "Yes", "No")
  
  # Confusion matrix to evaluate the model performance
  conf_matrix <- confusionMatrix(as.factor(predicted_classes), test_data$High, positive = "Yes")
  
  # Convert factor levels to numeric (1 for Yes, 0 for No)
  test_labels_numeric <- ifelse(test_labels == "Yes", 1, 0)
  
  # Now use test_labels_numeric as the true labels for evaluation
  true_labels <- test_labels_numeric  # Use the correct numeric labels
  
  # Store metrics for each fold
  accuracy_list <- c(accuracy_list, conf_matrix$overall["Accuracy"])
  sensitivity_list <- c(sensitivity_list, conf_matrix$byClass["Sensitivity"])
  specificity_list <- c(specificity_list, conf_matrix$byClass["Specificity"])
  pos_pred_value_list <- c(pos_pred_value_list, conf_matrix$byClass["Pos Pred Value"])
  neg_pred_value_list <- c(neg_pred_value_list, conf_matrix$byClass["Neg Pred Value"])
  
  
  print(length( true_labels))
  print(length( predicted_probabilities))
  
  if (length(unique(true_labels)) > 1) {
    roc_curve <- pROC::roc(response = true_labels, predictor = predicted_probabilities)
    auc_value <- pROC::auc(roc_curve)
    auc_list <- c(auc_list, auc_value)  # Append to AUC list
  } else {
    print("Skipping AUC calculation for this fold due to single-class data.")
  }
  balanced_accuracy <- (conf_matrix$byClass["Sensitivity"] + conf_matrix$byClass["Specificity"]) / 2
  balanced_accuracy_list <- c(balanced_accuracy_list, balanced_accuracy)
  
  #abs_calibration_error <- mean(abs(predicted_probabilities - as.numeric(as.character(test_split$labels))), na.rm = TRUE)
  abs_calibration_error <- mean(abs(predicted_probabilities - as.numeric(as.character(true_labels))), na.rm = TRUE)
  calibration_error_list <- c(calibration_error_list, abs_calibration_error)
  
  # Store probabilities and observed outcomes for calibration plot
  all_predicted_probabilities[[i]] <- predicted_probabilities
  all_observed_outcomes[[i]] <- ifelse(test_data$High == "Yes", 1, 0)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Summarize Metrics
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
write_xlsx(df, "A3_FPCA_SVM_RS22.xlsx")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the calibration plot function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print_calibration_plot = function(calibration_prediction_data, bins = 25) {
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calibration Plot Section (unchanged)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Random Forest Model Output
CR_A3_FPCA_SVM_RS <- read_excel("A3_FPCA_SVM_RS22.xlsx")
CR_A3_FPCA_SVM_RS$Observed_Outcome <- as.numeric(CR_A3_FPCA_SVM_RS$Observed_Outcome)
CR_A3_FPCA_SVM_RS <- CR_A3_FPCA_SVM_RS %>% select(Observed_Outcome, Predicted_Probabilities)
names(CR_A3_FPCA_SVM_RS) <- c("event", "predicted")

# Open a PNG device to save the plot
png(filename = "C_A3_FPCA_SVM_RS.png", width = 800, height = 600)

# Generate and display the calibration plot
calibration_plot <- print_calibration_plot(CR_A3_FPCA_SVM_RS, bins = 25)

# Close the graphical device
dev.off()

