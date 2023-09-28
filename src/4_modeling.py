### 3_modeling ###

# Libraries ----
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import pickle
from statistics import mean
from sklearn.model_selection import train_test_split, KFold, RandomizedSearchCV, GridSearchCV
from sklearn.preprocessing import MinMaxScaler
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, classification_report
from pprint import pprint

# Read ----
indicadores_fs_raw = pd.read_csv("./data/3_final/indicadores_fs.csv")

# Prepare dataset

# Delimit to year 2019
indicadores_fs_clean = indicadores_fs_raw[indicadores_fs_raw.anio == 2019]

# Remove measure of corruption from other years
indicadores_fs_clean = indicadores_fs_clean.loc[:, ~indicadores_fs_clean.columns.str.endswith("17")]
indicadores_fs_clean = indicadores_fs_clean.loc[:, ~indicadores_fs_clean.columns.str.endswith("21")]

# Remove non-relevant columns
indicadores_fs_clean = indicadores_fs_clean.drop(["nom_ent", "nom_mun", "mun_inegi", "anio", "gm_2020", "prop_corrup_per19", "prop_corrup5_inc19", "ovsae", "ovpt", "po2sm"], axis = 1)

# Remove municipalities that were not captured by ENCIG survey
indicadores_fs_clean = indicadores_fs_clean.dropna(subset = ["prop_corrup_inc19"])

# Check NAs
na_cols = []
for col in indicadores_fs_clean.columns:
  na_count = indicadores_fs_clean[col].isna().sum()
  if na_count > 0:
    print(f'{col} - {na_count}')
    na_cols.append(col) 

# Fill NA with median
indicadores_fs_clean = indicadores_fs_clean.fillna(value = {"cs_propety_tax_total_revs": indicadores_fs_clean.cs_propety_tax_total_revs.median(),
                                                    "lrs_direct_long_term_debt_pobtot": indicadores_fs_clean.lrs_direct_long_term_debt_pobtot.median(),
                                                    "lrs_debt_service_total_rev": indicadores_fs_clean.lrs_debt_service_total_rev.median()})

# Convert to dummies var `mun_tipo`
indicadores_fs_clean = pd.get_dummies(indicadores_fs_clean, columns = ["mun_tipo"])
# indicadores_fs_clean = indicadores_fs_clean.drop(["mun_tipo_metropolitano"], axis = 1) # Metropolitano will be the base case of th dataset

# Add feature variable 
indicadores_fs_clean["corrup"] = np.where(indicadores_fs_clean.prop_corrup_inc19 > indicadores_fs_clean.prop_corrup_inc19.mean(), 1, 0)
indicadores_fs_clean = indicadores_fs_clean.drop(["prop_corrup_inc19"], axis = 1)

# Distribution of classes
indicadores_fs_clean.corrup.value_counts()

# Split features from target
X = np.array(indicadores_fs_clean.drop(["corrup"], axis = 1))
y = np.array(indicadores_fs_clean["corrup"])

# Normalize features
scaler = MinMaxScaler()
X = scaler.fit_transform(X)

# Split Train, test datasets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 42)

print("X_train:", X_train.shape)
print("X_test:", X_test.shape)

print("y_train:", y_train.shape)
print("y_test:", y_test.shape)

# Compare size of classes in sets
class_train, counts_train = np.unique(y_train, return_counts = True)
class_test, counts_test = np.unique(y_test, return_counts = True)

print("Y train")
print(np.asarray((class_train, counts_train)).T)
print("Y test")
print(np.asarray((class_test, counts_test)).T)

# Cross Validation

## Random Search

# Number of trees in random forest
n_estimators = [int(x) for x in np.linspace(start = 50, stop = 500, num = 10)]
# Number of features to consider at every split
max_features = ['auto', 'sqrt']
# Maximum number of levels in tree
max_depth = [int(x) for x in np.linspace(10, 100, num = 11)]
max_depth.append(None)
# Minimum number of samples required to split a node
min_samples_split = [2, 5, 10, 20]
# Minimum number of samples required at each leaf node
min_samples_leaf = [1, 2, 4, 6, 8]
# Method of selecting samples for training each tree
bootstrap = [True, False]

# Create the random grid
random_grid = {'n_estimators': n_estimators,
               'max_features': max_features,
               'max_depth': max_depth,
               'min_samples_split': min_samples_split,
               'min_samples_leaf': min_samples_leaf,
               'bootstrap': bootstrap}

pprint(random_grid)

# Use the random grid to search for best hyperparameters
# First create the base model to tune
rf = RandomForestClassifier(random_state = 42)

print("Parameters of RF:\n")
pprint(rf.get_params())
# Random search of parameters, using 3 fold cross validation, 
# search across 100 different combinations, and use all available cores
rf_random = RandomizedSearchCV(estimator=rf, param_distributions=random_grid,
                              n_iter = 100, scoring='neg_mean_absolute_error', 
                              cv = 3, verbose=2, random_state=42, n_jobs=-1,
                              return_train_score=True)

# Fit the random search model
rf_random.fit(X_train, y_train);

print(rf_random.best_params_)

def evaluate(model, test_features, test_labels):
    predictions = model.predict(test_features)
    accuracy = accuracy_score(y_true = test_labels, y_pred = predictions)
    print('Model Performance')
    print('Accuracy = {:0.2f}%.'.format(accuracy*100))
    
    return accuracy

# Evaluate base model
base_model = RandomForestClassifier(n_estimators = 10, random_state = 42)
base_model.fit(X_train, y_train)
base_accuracy = evaluate(base_model, X_test, y_test)

# Evaluate random search model
best_random = rf_random.best_estimator_
random_accuracy = evaluate(best_random, X_test, y_test)

print('Improvement of {:0.2f}%.'.format( 100 * (random_accuracy - base_accuracy) / base_accuracy))

# Validacion cruzada

kf = KFold(n_splits = 5)
CLF_rf = []
f1_rf = []
for train_idx, val_idx in kf.split(X_train, y_train):

    # Conjuntos de entrenamiento y validacion
    X_train_kf, y_train_kf = X[train_idx], y[train_idx]
    X_val, y_val = X[val_idx], y[val_idx]

    # Random Forest
    clf = RandomForestClassifier(n_estimators = 450, 
                                 min_samples_split = 5, 
                                 min_samples_leaf = 4, 
                                 max_features = "sqrt", 
                                 max_depth = None, 
                                 bootstrap = True, 
                                 random_state = 42).fit(X_train_kf, y_train_kf)

    # Predicciones
    y_pred = clf.predict(X_val)

    CLF_rf.append(clf)

    print("-"*50)
    print("Resultados del entrenamiento:", len(CLF_rf)-1)
    report = classification_report(y_val, y_pred, digits = 4, output_dict = True)
    f1 = report["macro avg"]["f1-score"]
    f1_rf.append(f1)
    print("Macro avg - F1-score:", round(f1, 4))

# Media total 
print("F1 medio: ", mean(f1_rf))

# Evaluacion final
y_pred_final = CLF_rf[0].predict(X_test)
print("RANDOM FOREST EVALUATION\n")
print(classification_report(y_test, y_pred_final))

# Use sklearn to export the tree 
from sklearn.tree import export_graphviz

# Write the decision tree as a dot file
visual_tree = CLF_rf[0].estimators_[12]
export_graphviz(visual_tree, out_file = './figs/best_tree.dot', feature_names = indicadores_fs_clean.drop(["corrup"], axis = 1).columns, 
                precision = 2, filled = True, rounded = True, max_depth = None)

# Use pydot for converting to an image file
import pydot

# Import the dot file to a graph and then convert to a png
(graph, ) = pydot.graph_from_dot_file('./figs/best_tree.dot')
graph.write_png('./figs/best_tree.png')

sorted_idx = CLF_rf[0].feature_importances_.argsort()
plt.barh(indicadores_fs_clean.drop(["corrup"], axis = 1).columns[sorted_idx], CLF_rf[0].feature_importances_[sorted_idx])
plt.xlabel("Random Forest Feature Importance")
plt.show()
