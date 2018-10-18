
# coding: utf-8

# In[1]:


# Importing the necessary libraries
import pandas as pd
from imblearn.combine import SMOTEENN
from numpy import loadtxt
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from xgboost import XGBClassifier


# read training data 
# Insert relevant path
df = pd.read_csv("Meds_AnonymisedTrainingData_Delirium.csv")
dataset = loadtxt('Meds_AnonymisedTrainingData_Delirium.csv', delimiter=","
                  , skiprows=1)
# Obtaining input and target
X = dataset[:, 1:47]
Y = dataset[:, 47]
# Handling imbalance
sm = SMOTEENN()
X_resampled, y_resampled = sm.fit_sample(X, Y)
# split data into train and test sets
seed = 1329
test_size = 0.33
X_train, X_test, y_train, y_test = train_test_split(X_resampled, y_resampled
                                                    , test_size=test_size
                                                    , random_state=seed)
# fit model no training data
model = XGBClassifier()
model.fit(X_train, y_train)

# make predictions for test data
pred_y = model.predict(X_test)
predictions = [round(value) for value in pred_y]

# evaluate prediction metrics
# for additional metrics, view notebook
accuracy = accuracy_score(y_test, predictions)


# Validating the model with new data
# id_validation 
df_val = loadtxt('Meds_AnonymisedValidationData_Delirium.csv', delimiter=","
                 , skiprows=1)

X = df_val[:, 1:47]
Y = df_val[:, 47]

# make predictions for test data
y_predict = model.predict(X)
predictions_1 = [round(value) for value in y_predict]

# evaluate predictions
accuracy = accuracy_score(Y, predictions_1)