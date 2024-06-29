#Lung cancer prediction
Predicting lung cancer using machine Learning(KNN)
### Project Statement

This project aims to develop and evaluate a predictive model using the K-Nearest Neighbors (KNN) algorithm to classify cancer patients based on their symptoms and various risk factors. The primary objective is to ensure the model's accuracy and reliability in predicting the symptom levels of cancer patients.

#### Objectives

1. **Data Pre-processing:**
   - Clean the dataset by identifying and handling missing values.
   - Filter rows containing zeros and visualize the distribution of such rows.

2. **Feature Selection and Engineering:**
   - Select relevant predictor variables for model building.
   - Create a subset of the data for correlation analysis and identify highly correlated variables to be removed.

3. **Exploratory Data Analysis (EDA):**
   - Conduct visual analyses to understand the distribution and relationships of features.
     - Generate bar plots, histograms, density plots, and box plots to visualize data distributions.

4. **Data Normalization:**
   - Normalize the data to ensure all features contribute equally to the model.
   
5. **Model Building and Evaluation:**
   - Split the dataset into training and testing sets (70% training, 30% testing).
   - Implement the KNN algorithm with varying values of k (k = 26, 27, and 1-30) to find the optimal number of neighbors.
   - Calculate the proportion of correct classifications and create confusion matrices to evaluate model performance.

6. **Model Optimization:**
   - Use cross-validation to fine-tune the KNN model, ensuring robust performance.
   - Train the KNN model using caret's `trainControl` function with repeated cross-validation.
   - Explore a grid of k values (1-30) to find the optimal k using caret's `train` function.
   - Evaluate the model's performance on the testing set and visualize the results.

7. **Accuracy Assessment:**
   - Compare the accuracy of the KNN model built manually and the model optimized using the caret package.
   - Print and interpret the accuracy results for both models.

By following these objectives, this project aims to develop a reliable and accurate predictive model to classify cancer patients based on their symptoms and risk factors, thus aiding in early diagnosis and appropriate treatment planning.
