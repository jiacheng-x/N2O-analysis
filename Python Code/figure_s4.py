import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.ensemble import IsolationForest
import copy
import seaborn as sns
import scipy.stats as stats

n2o = pd.read_excel("N2O_Fluxes.xlsx")
n2o['Date'] = pd.to_datetime(n2o['Date'])

n2o = n2o.drop(n2o[n2o['Chamber']=='Chamber 5'].index).sort_values(by=['Node', 'Chamber'])

#Early growing season
n2o_first = copy.copy(n2o[(n2o['Date'] >= "2022-05-13") & (n2o['Date'] <= "2022-07-07")])

chamber_ids = n2o_first['Chamber ID'].unique()
threshold = np.zeros_like(chamber_ids)
data = {'Chamber ID': chamber_ids, 'Threshold_IQR': threshold, 'Threshold_iForest': threshold, 'Threshold_SD': threshold,}
chamber_thresh = pd.DataFrame(data)

model = IsolationForest(n_estimators=100, max_samples='auto', contamination='auto',random_state=40)

for id in chamber_ids:
    n2o_sub = copy.copy(n2o_first.loc[n2o_first['Chamber ID'] == id])
    hot_iqr = 1.5*stats.iqr(n2o_sub['N2O'])+np.percentile(n2o_sub['N2O'],75)
    hot_sd = 4*np.std(n2o_sub['N2O']) + np.mean(n2o_sub['N2O'])
    model.fit(n2o_sub[['N2O']])
    n2o_sub['scores'] = model.decision_function(n2o_sub[['N2O']])
    n2o_sub['anomaly'] = model.predict(n2o_sub[['N2O']])
    hot_iForest = n2o_sub.loc[((n2o_sub['anomaly']==-1) & (n2o_sub['N2O']>0)), 'N2O' ].min()
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_IQR'] = hot_iqr
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_SD'] = hot_sd
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_iForest'] = hot_iForest

chamber_thresh["Threshold_IQR"] = pd.to_numeric(chamber_thresh["Threshold_IQR"], errors='coerce')
chamber_thresh["Threshold_SD"] = pd.to_numeric(chamber_thresh["Threshold_SD"], errors='coerce')
chamber_thresh["Threshold_iForest"] = pd.to_numeric(chamber_thresh["Threshold_iForest"], errors='coerce')

plt.figure(figsize=(15, 5))  # Adjust the figure size to fit the 1x3 layout

# Plot Threshold_IQR
plt.subplot(1, 3, 1)
sns.heatmap(chamber_thresh.set_index("Chamber ID")[["Threshold_IQR"]].sort_index(),
            cmap="YlGnBu", annot=True, fmt=".4f", linewidths=.5, cbar_kws={'label': '1.5 x IQR'}, xticklabels=False)
plt.title("1.5 x IQR Threshold Values for Each Chamber")

# Plot Threshold_iForest
plt.subplot(1, 3, 2)
sns.heatmap(chamber_thresh.set_index("Chamber ID")[["Threshold_iForest"]].sort_index(),
            cmap="YlGnBu", annot=True, fmt=".4f", linewidths=.5, cbar_kws={'label': 'Isolation Forest'}, xticklabels=False)
plt.title("Isolation Forest Threshold Values for Each Chamber")

# Plot Threshold_SD
plt.subplot(1, 3, 3)
sns.heatmap(chamber_thresh.set_index("Chamber ID")[["Threshold_SD"]].sort_index(),
            cmap="YlGnBu", annot=True, fmt=".4f", linewidths=.5, cbar_kws={'label': '4 SD'}, xticklabels=False)
plt.title("4 SD Threshold Values for Each Chamber")

plt.tight_layout()
#plt.savefig('Early_season_heat_map_legends.pdf',format='pdf', dpi=300)
plt.show()

#Late growing season
n2o_second = copy.copy(n2o[(n2o['Date'] >= "2022-07-08") & (n2o['Date'] <= "2022-10-31")])

threshold = np.zeros_like(chamber_ids)
data = {'Chamber ID': chamber_ids, 'Threshold_IQR': threshold, 'Threshold_iForest': threshold, 'Threshold_SD': threshold,}
chamber_thresh = pd.DataFrame(data)

model = IsolationForest(n_estimators=100, max_samples='auto', contamination='auto',random_state=40)

for id in chamber_ids:
    n2o_sub = copy.copy(n2o_second.loc[n2o_second['Chamber ID'] == id])
    hot_iqr = 1.5*stats.iqr(n2o_sub['N2O'])+np.percentile(n2o_sub['N2O'],75)
    hot_sd = 4*np.std(n2o_sub['N2O']) + np.mean(n2o_sub['N2O'])
    model.fit(n2o_sub[['N2O']])
    n2o_sub['scores'] = model.decision_function(n2o_sub[['N2O']])
    n2o_sub['anomaly'] = model.predict(n2o_sub[['N2O']])
    hot_iForest = n2o_sub.loc[((n2o_sub['anomaly']==-1) & (n2o_sub['N2O']>0)), 'N2O' ].min()
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_IQR'] = hot_iqr
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_SD'] = hot_sd
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_iForest'] = hot_iForest

chamber_thresh["Threshold_IQR"] = pd.to_numeric(chamber_thresh["Threshold_IQR"], errors='coerce')
chamber_thresh["Threshold_SD"] = pd.to_numeric(chamber_thresh["Threshold_SD"], errors='coerce')
chamber_thresh["Threshold_iForest"] = pd.to_numeric(chamber_thresh["Threshold_iForest"], errors='coerce')

plt.figure(figsize=(15, 5))  # Adjust the figure size to fit the 1x3 layout

# Plot Threshold_IQR
plt.subplot(1, 3, 1)
sns.heatmap(chamber_thresh.set_index("Chamber ID")[["Threshold_IQR"]].sort_index(),
            cmap="YlGnBu", annot=True, fmt=".4f", linewidths=.5, cbar_kws={'label': '1.5 x IQR'}, xticklabels=False)
plt.title("1.5 x IQR Threshold Values for Each Chamber")

# Plot Threshold_iForest
plt.subplot(1, 3, 2)
sns.heatmap(chamber_thresh.set_index("Chamber ID")[["Threshold_iForest"]].sort_index(),
            cmap="YlGnBu", annot=True, fmt=".4f", linewidths=.5, cbar_kws={'label': 'Isolation Forest'}, xticklabels=False)
plt.title("Isolation Forest Threshold Values for Each Chamber")

# Plot Threshold_SD
plt.subplot(1, 3, 3)
sns.heatmap(chamber_thresh.set_index("Chamber ID")[["Threshold_SD"]].sort_index(),
            cmap="YlGnBu", annot=True, fmt=".4f", linewidths=.5, cbar_kws={'label': '4 SD'}, xticklabels=False)
plt.title("4 SD Threshold Values for Each Chamber")

plt.tight_layout()
#plt.savefig('Late_season_heat_map_legends.pdf',format='pdf', dpi=300)
plt.show()

#Post-harvest season
n2o_third = copy.copy(n2o[(n2o['Date'] >= "2022-11-01")])

threshold = np.zeros_like(chamber_ids)
data = {'Chamber ID': chamber_ids, 'Threshold_IQR': threshold, 'Threshold_iForest': threshold, 'Threshold_SD': threshold,}
chamber_thresh = pd.DataFrame(data)

model = IsolationForest(n_estimators=100, max_samples='auto', contamination='auto',random_state=40)

for id in chamber_ids:
    n2o_sub = copy.copy(n2o_third.loc[n2o_third['Chamber ID'] == id])
    hot_iqr = 1.5*stats.iqr(n2o_sub['N2O'])+np.percentile(n2o_sub['N2O'],75)
    hot_sd = 4*np.std(n2o_sub['N2O']) + np.mean(n2o_sub['N2O'])
    model.fit(n2o_sub[['N2O']])
    n2o_sub['scores'] = model.decision_function(n2o_sub[['N2O']])
    n2o_sub['anomaly'] = model.predict(n2o_sub[['N2O']])
    hot_iForest = n2o_sub.loc[((n2o_sub['anomaly']==-1) & (n2o_sub['N2O']>0)), 'N2O' ].min()
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_IQR'] = hot_iqr
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_SD'] = hot_sd
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_iForest'] = hot_iForest

chamber_thresh["Threshold_IQR"] = pd.to_numeric(chamber_thresh["Threshold_IQR"], errors='coerce')
chamber_thresh["Threshold_SD"] = pd.to_numeric(chamber_thresh["Threshold_SD"], errors='coerce')
chamber_thresh["Threshold_iForest"] = pd.to_numeric(chamber_thresh["Threshold_iForest"], errors='coerce')

plt.figure(figsize=(15, 5))  # Adjust the figure size to fit the 1x3 layout

# Plot Threshold_IQR
plt.subplot(1, 3, 1)
sns.heatmap(chamber_thresh.set_index("Chamber ID")[["Threshold_IQR"]].sort_index(),
            cmap="YlGnBu", annot=True, fmt=".4f", linewidths=.5, cbar_kws={'label': '1.5 x IQR'}, xticklabels=False)
plt.title("1.5 x IQR Threshold Values for Each Chamber")

# Plot Threshold_iForest
plt.subplot(1, 3, 2)
sns.heatmap(chamber_thresh.set_index("Chamber ID")[["Threshold_iForest"]].sort_index(),
            cmap="YlGnBu", annot=True, fmt=".4f", linewidths=.5, cbar_kws={'label': 'Isolation Forest'}, xticklabels=False)
plt.title("Isolation Forest Threshold Values for Each Chamber")

# Plot Threshold_SD
plt.subplot(1, 3, 3)
sns.heatmap(chamber_thresh.set_index("Chamber ID")[["Threshold_SD"]].sort_index(),
            cmap="YlGnBu", annot=True, fmt=".4f", linewidths=.5, cbar_kws={'label': '4 SD'}, xticklabels=False)
plt.title("4 SD Threshold Values for Each Chamber")

plt.tight_layout()
#plt.savefig('Post_season_heat_map_legends.pdf',format='pdf', dpi=300)
plt.show()