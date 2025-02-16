This code was developed for the results presented in the research paper "Comparison of Machine Learning and Validation Methods for High-Dimensional Accelerometer Data to Detect Foot Lesions in Dairy Cattle." These codes and data files can be used to reproduce all the presented results in the paper.

The data files (.csv and .xlsx) are described as follows:

AllCowsDataX: Contains raw accelerometer data for 383 cows in the Z direction, without dimensionality reduction (20,000 features).
AfterPCA_AllCowsX: Contains the principal components of the accelerometer data for 383 cows in the Z direction, representing the reduced dataset after PCA.
AfterFPCA_AllCowsX: Contains the functional principal components of the accelerometer data for 383 cows in the Z direction, representing the reduced dataset after FPCA.

CombConorAll: Contains the binary lameness classification outcome (0/1) based on clinical assessments of each cow.


The R code File files are described as follows:

A1_NR_RF_FS1:      Approach 1 _ No Reduction _ Random Forest _ Farm Split(Farm Fold) _ Code 1
A1_NR_RF_RS2:      Approach 1 _ No Reduction _ Random Forest _ Random Split(N Fold) _ Code 2
A2_PCA_RF_FS3:     Approach 2 _ Principal Component Analysis _ Random Forest _ Farm Split(Farm Fold) _ Code 3
A2_PCA_RF_RS4:     Approach 2 _ Principal Component Analysis _ Random Forest _ Random Split(N Fold) _ Code 4
A2_PCA_LR_FS5:     Approach 2 _ Principal Component Analysis _ Logistic Regression _ Farm Split(Farm Fold) _ Code 5
A2_PCA_LR_RS6:     Approach 2 _ Principal Component Analysis _ Logistic Regression _ Random Split(N Fold) _ Code 6
A2_PCA_KNN_FS7:    Approach 2 _ Principal Component Analysis _ K-Nearest Neighbors  _ Farm Split(Farm Fold) _ Code 7
A2_PCA_KNN_RS8:    Approach 2 _ Principal Component Analysis _ K-Nearest Neighbors  _ Random Split(N Fold) _ Code 8
A2_PCA_NB_FS9:     Approach 2 _ Principal Component Analysis _ Naïve Bayes _ Farm Split(Farm Fold) _ Code 9
A2_PCA_NB_RS10:    Approach 2 _ Principal Component Analysis _ Naïve Bayes _ Random Split(N Fold) _ Code 10
A2_PCA_SVM_FS11:   Approach 2 _ Principal Component Analysis _ Support Vector Machines _ Farm Split(Farm Fold) _ Code 11
A2_PCA_SVM_RS12:   Approach 2 _ Principal Component Analysis _ Support Vector Machines _ Random Split(N Fold) _ Code 12
A3_FPCA_RF_FS13:   Approach 3 _ Functional Principal Component Analysis _ Random Forest _ Farm Split(Farm Fold) _ Code 13
A3_FPCA_RF_RS14:   Approach 3 _ Functional Principal Component Analysis _ Random Forest _ Random Split(N Fold) _ Code 14
A3_FPCA_LR_FS15:   Approach 3 _ Functional Principal Component Analysis _ Logistic Regression _ Farm Split(Farm Fold) _ Code 15
A3_FPCA_LR_RS16:   Approach 3 _ Functional Principal Component Analysis _ Logistic Regression _ Random Split(N Fold) _ Code 16
A3_FPCA_KNN_FS17:  Approach 3 _ Functional Principal Component Analysis _ K-Nearest Neighbors  _ Farm Split(Farm Fold) _ Code 17
A3_FPCA_KNN_RS18:  Approach 3 _ Functional Principal Component Analysis _ K-Nearest Neighbors  _ Random Split(N Fold) _ Code 18
A3_FPCA_NB_FS19:   Approach 3 _ Functional Principal Component Analysis _ Naïve Bayes _ Farm Split(Farm Fold) _ Code 19
A3_FPCA_NB_RS20:   Approach 3 _ Functional Principal Component Analysis _ Naïve Bayes _ Random Split(N Fold) _ Code 20
A3_FPCA_SVM_FS21:  Approach 3 _ Functional Principal Component Analysis _ Support Vector Machines _ Farm Split(Farm Fold) _ Code 21
A3_FPCA_SVM_RS22:  Approach 3 _ Functional Principal Component Analysis _ Support Vector Machines _ Random Split(N Fold) _ Code 22
