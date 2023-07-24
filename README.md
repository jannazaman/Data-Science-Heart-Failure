# Data-Science-Heart-Failure

#### Goals for the Analysis of the Dataset:
The main goal is to accurately predict the target variable given the predictors of age, sex, exercise angina, chest pain type, resting blood pressure, cholesterol, fasting blood sugar, resting electrocardiographic results, maximum heart rate achieved of the patient and so forth. The target variable will include a binary value that represents 0 as having a lower chance of having a heart attack and 1 as having a higher chance of having a heart attack. Essentially, 1 equates to heart disease and 0 is normal. I believe it is important to be able to accurately predict whether someone is prone to having a heart attack, as it can allow patients to adjust their lifestyle (earlier on) accordingly for heart attack prevention, and potentially not result in heart failure. 

### Research Question:
Could we accurately predict an individual’s chance of getting a heart disease (heart failure) based on the predictor variables in the Heart Failure dataset? If so, which model amongst KNN or SVM provides a more accurate result? 

__________________________________________________________________________________________________________________

### Description of the Dataset:
The data set is titled “Heart Failure Prediction Dataset” and can be found on Kaggle; it was originally posted by Fedesoriano. The Heart data frame comprises 918 observations with 12 variables; 11 of them being the features used to make predictions. The data frame contains the following predictors:

1) Age: age of the patient [years] Quantitative
2) Sex: sex of the patient [M: Male, F: Female] Qualitative
3) ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic] Qualitative
4) RestingBP: resting blood pressure [mm Hg] Quantitative
5) Cholesterol: serum cholesterol [mm/dl] Quantitative
6) FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise] Quantitative
7) RestingECG: resting electrocardiogram results [Normal: Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria] Qualitative
8) MaxHR: maximum heart rate achieved [Numeric value between 60 and 202] Quantitative
9) ExerciseAngina: exercise-induced angina [Y: Yes, N: No] Qualitative
10) Oldpeak: oldpeak = ST [Numeric value measured in depression] Quantitative
11) ST_Slope: the slope of the peak exercise ST segment [Up: upsloping, Flat: flat, Down: downsloping] Quantitative
12) HeartDisease: output class [1: heart disease, 0: Normal] Quantitative 

## Notes:
- ***All of the code is in "Heart Project.R" file.***
- ***You can ignore the warnings on "Heart Failure.ipynb" file.***
- ***Dataset used is included - just download and adjust the data import section.***

