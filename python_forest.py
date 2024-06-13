# code adapted from https://github.com/microsoft/EconML
import matplotlib.pyplot as plt
import shap
from econml.dml import CausalForestDML
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from random import randrange, seed
import subprocess
from scipy.stats.mstats import winsorize
import numpy as np
import shap

seed(14)

# Load in data
subprocess.call ("Rscript data_setup.R", shell=True)
X = pd.read_csv('X_id.csv')
Y = pd.read_csv('Y.csv')
# Y.income = winsorize(Y.income, limits=(0, 0.02))
W = pd.read_csv('W.csv')

print(X.head())

# fit causal forest with default parameters 
causal_forest = CausalForestDML(n_estimators = 100)
causal_forest.fit(Y, W, X=X)
# X = X.rename(columns=dict(zip(labs["name"], labs["label"])))

# Fit SHAP values using work around because of issue 445 on EconML Github
background = shap.maskers.Independent(X, max_samples=200)
explainer = shap.Explainer(causal_forest.model_cate.estimators_[0], background)
shap_values = explainer(X, check_additivity=False)
shap.plots.beeswarm(shap_values)
shap.plots.waterfall(shap_values[0])
shap.plots.waterfall(shap_values[100])
shap.plots.waterfall(shap_values[200])
shap.plots.waterfall(shap_values[300])

# This code below should work but doesn't due to an EconML bug.
# # calculate shap values of causal forest model 
# shap_values = causal_forest.shap_values(np.array(X))
# # plot shap values 
# shap.summary_plot(shap_values['income']['treat'], show = False)
# plt.savefig("figures/overall_waterfall.png", bbox_inches='tight', dpi=300)
# plt.show()
# 
# # Waterfall
# for i in range(5):
#     shap.plots.waterfall(shap_values['income']['treat'][randrange(len(shap_values['income']['treat']))], max_display=20, show = False)
#     plt.savefig(f"figures/idwaterfall_{i}.png", bbox_inches='tight', dpi=300)
#     plt.show()
# 
# # estimates
# ate = causal_forest.ate_inference(X=X)
# inference = causal_forest.effect_inference(X)
