import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.ensemble import IsolationForest
import copy
import scipy.stats as stats
import matplotlib.patches as mpatches

def format_percentage(value):
    # Round the value
    rounded_value = round(value)

    # Check if the original value has one decimal place
    if value == round(value * 10) / 10:
        return f"{value:.1f}%"
    elif value == 100:
        return "100%"
    else:
        if rounded_value == 100:
            return "100%"
        else:
            return f"{rounded_value}%"

n2o = pd.read_excel("N2O_Fluxes.xlsx")
n2o['Date'] = pd.to_datetime(n2o['Date'])

n2o = n2o.drop(n2o[n2o['Chamber']=='Chamber 5'].index).sort_values(by=['Node', 'Chamber'])

#Early growing season
n2o_first = copy.copy(n2o[(n2o['Date'] >= "2022-05-13") & (n2o['Date'] <= "2022-07-07")])
#Late growing season
n2o_second = copy.copy(n2o[(n2o['Date'] >= "2022-07-08") & (n2o['Date'] <= "2022-10-31")])
#Post-harvest season
n2o_third = copy.copy(n2o[(n2o['Date'] >= "2022-11-01")])

chamber_ids = n2o_first['Chamber ID'].unique()
threshold = np.zeros_like(chamber_ids)
data = {'Chamber ID': chamber_ids, 'Threshold_IQR': threshold, 'Threshold_iForest': threshold, 'Threshold_4SD': threshold}
chamber_thresh_first = pd.DataFrame(data)
chamber_thresh_second = pd.DataFrame(data)
chamber_thresh_third = pd.DataFrame(data)

# Initialize dictionaries to hold the cumulative hot moments N2O flux for each method and season
hot_moments_flux = {'Threshold_IQR': {}, 'Threshold_iForest': {}, 'Threshold_4SD': {}}
season_names = ['Early-Growing', 'Late-Growing', 'Post-Harvest']

# Loop through each season and its corresponding threshold DataFrame
for season_data, season_name, chamber_thresh in [(n2o_first, 'Early-Growing', chamber_thresh_first),
                                                 (n2o_second, 'Late-Growing', chamber_thresh_second),
                                                 (n2o_third, 'Post-Harvest', chamber_thresh_third)]:

    model = IsolationForest(n_estimators=100, max_samples='auto', contamination='auto', random_state=40)

    for id in chamber_ids:
        n2o_sub = copy.copy(season_data.loc[season_data['Chamber ID'] == id])
        hot_iqr = 1.5 * stats.iqr(n2o_sub['N2O'])+np.percentile(n2o_sub['N2O'],75)
        hot_4sd = 4 * np.std(n2o_sub['N2O']) + np.mean(n2o_sub['N2O'])

        model.fit(n2o_sub[['N2O']])
        n2o_sub['scores'] = model.decision_function(n2o_sub[['N2O']])
        n2o_sub['anomaly'] = model.predict(n2o_sub[['N2O']])
        hot_iForest = n2o_sub.loc[((n2o_sub['anomaly'] == -1) & (n2o_sub['N2O'] > 0)), 'N2O'].min()

        chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_IQR'] = hot_iqr
        chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_4SD'] = hot_4sd
        chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_iForest'] = hot_iForest

    for method in ['Threshold_IQR', 'Threshold_iForest', 'Threshold_4SD']:
        hot_moments_flux[method][season_name] = 0  # Initialize

        for id in chamber_ids:
            threshold_value = chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, method].iloc[0]
            hot_moments_flux_value = season_data.loc[(season_data['Chamber ID'] == id) & (season_data['N2O'] > threshold_value), 'N2O'].sum()
            hot_moments_flux[method][season_name] += hot_moments_flux_value

# Calculate the total N2O flux for the whole year for each method.
# This is the sum of the hot moments flux for all seasons.
total_flux = {method: sum(flux.values()) for method, flux in hot_moments_flux.items()}

# Calculate the total N2O flux for the whole year (all seasons, chambers, and nodes)
total_year_flux = n2o['N2O'].sum()

# Calculate the total N2O flux for each season
total_season_flux = {'Early-Growing': n2o_first['N2O'].sum(),
                     'Late-Growing': n2o_second['N2O'].sum(),
                     'Post-Harvest': n2o_third['N2O'].sum()}

methods = list(hot_moments_flux.keys())
season_names = ['Early-Growing', 'Late-Growing', 'Post-Harvest']

chamber_ids = n2o['Chamber ID'].unique()
data = {'Chamber ID': chamber_ids, 'Threshold_IQR': np.zeros_like(chamber_ids),
        'Threshold_iForest': np.zeros_like(chamber_ids), 'Threshold_4SD': np.zeros_like(chamber_ids)}

model = IsolationForest(n_estimators=100, max_samples='auto', contamination='auto', random_state=40)

# Yearly threshold calculation
chamber_thresh_yearly = pd.DataFrame(data)
for id in chamber_ids:
    n2o_sub = copy.deepcopy(n2o[n2o['Chamber ID'] == id])
    hot_iqr_yearly = 1.5 * stats.iqr(n2o_sub['N2O'])+np.percentile(n2o_sub['N2O'],75)
    hot_4sd_yearly = 4 * np.std(n2o_sub['N2O']) + np.mean(n2o_sub['N2O'])
    model.fit(n2o_sub[['N2O']])
    n2o_sub['scores'] = model.decision_function(n2o_sub[['N2O']])
    n2o_sub['anomaly'] = model.predict(n2o_sub[['N2O']])
    hot_iForest_yearly = n2o_sub.loc[(n2o_sub['anomaly'] == -1) & (n2o_sub['N2O'] > 0), 'N2O'].min()
    chamber_thresh_yearly.loc[chamber_thresh_yearly['Chamber ID'] == id, 'Threshold_IQR'] = hot_iqr_yearly
    chamber_thresh_yearly.loc[chamber_thresh_yearly['Chamber ID'] == id, 'Threshold_4SD'] = hot_4sd_yearly
    chamber_thresh_yearly.loc[chamber_thresh_yearly['Chamber ID'] == id, 'Threshold_iForest'] = hot_iForest_yearly


# Calculate cumulative flux and hot moments for each chamber using seasonal and yearly thresholds
cumulative_flux = {}
hot_moments_flux_seasonal = {}
for chamber_id in chamber_ids:
    cumulative_flux[chamber_id] = n2o[n2o['Chamber ID'] == chamber_id]['N2O'].sum()
    hot_moments_flux_seasonal[chamber_id] = {'Threshold_IQR': 0, 'Threshold_iForest': 0, 'Threshold_4SD': 0}
    for season_data, season_thresh in [(n2o_first, chamber_thresh_first), (n2o_second, chamber_thresh_second), (n2o_third, chamber_thresh_third)]:
        season_data_chamber = season_data[season_data['Chamber ID'] == chamber_id]
        for method in ['Threshold_IQR', 'Threshold_iForest', 'Threshold_4SD']:
            threshold = season_thresh.loc[season_thresh['Chamber ID'] == chamber_id, method].iloc[0]
            hot_moments_flux_seasonal[chamber_id][method] += season_data_chamber[season_data_chamber['N2O'] > threshold]['N2O'].sum()

hot_moments_flux_yearly = {chamber_id: {method: n2o[(n2o['Chamber ID'] == chamber_id) &
                                                    (n2o['N2O'] > chamber_thresh_yearly.loc[chamber_thresh_yearly['Chamber ID'] == chamber_id, method].iloc[0])]['N2O'].sum()
                                        for method in ['Threshold_IQR', 'Threshold_iForest', 'Threshold_4SD']}
                           for chamber_id in chamber_ids}

plt.rcParams['font.family'] = 'Arial'

# Define your functions and data (assuming n2o, n2o_first, n2o_second, n2o_third, chamber_thresh_first, chamber_thresh_second, chamber_thresh_third are already defined)

# Seasonal threshold calculation
chamber_ids = n2o['Chamber ID'].unique()
data = {'Chamber ID': chamber_ids, 'Threshold_IQR': np.zeros_like(chamber_ids),
        'Threshold_iForest': np.zeros_like(chamber_ids), 'Threshold_4SD': np.zeros_like(chamber_ids)}
chamber_thresh = pd.DataFrame(data)  # This will be used as 'chamber_thresh_seasonal'

model = IsolationForest(n_estimators=100, max_samples='auto', contamination='auto', random_state=40)

for id in chamber_ids:
    n2o_sub = copy.deepcopy(n2o[n2o['Chamber ID'] == id])
    hot_iqr = 1.5 * stats.iqr(n2o_sub['N2O'])+np.percentile(n2o_sub['N2O'],75)
    hot_sd = 4 * np.std(n2o_sub['N2O']) + np.mean(n2o_sub['N2O'])
    model.fit(n2o_sub[['N2O']])
    n2o_sub['scores'] = model.decision_function(n2o_sub[['N2O']])
    n2o_sub['anomaly'] = model.predict(n2o_sub[['N2O']])
    hot_iForest = n2o_sub.loc[(n2o_sub['anomaly'] == -1) & (n2o_sub['N2O'] > 0), 'N2O'].min()
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_IQR'] = hot_iqr
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_4SD'] = hot_sd
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_iForest'] = hot_iForest

# Yearly threshold calculation
chamber_thresh_yearly = pd.DataFrame(data)
for id in chamber_ids:
    n2o_sub = copy.deepcopy(n2o[n2o['Chamber ID'] == id])
    hot_iqr_yearly = 1.5 * stats.iqr(n2o_sub['N2O'])+np.percentile(n2o_sub['N2O'],75)
    hot_sd_yearly = 4 * np.std(n2o_sub['N2O']) + np.mean(n2o_sub['N2O'])
    model.fit(n2o_sub[['N2O']])
    n2o_sub['scores'] = model.decision_function(n2o_sub[['N2O']])
    n2o_sub['anomaly'] = model.predict(n2o_sub[['N2O']])
    hot_iForest_yearly = n2o_sub.loc[(n2o_sub['anomaly'] == -1) & (n2o_sub['N2O'] > 0), 'N2O'].min()
    chamber_thresh_yearly.loc[chamber_thresh_yearly['Chamber ID'] == id, 'Threshold_IQR'] = hot_iqr_yearly
    chamber_thresh_yearly.loc[chamber_thresh_yearly['Chamber ID'] == id, 'Threshold_4SD'] = hot_sd_yearly
    chamber_thresh_yearly.loc[chamber_thresh_yearly['Chamber ID'] == id, 'Threshold_iForest'] = hot_iForest_yearly

# Calculate cumulative flux and hot moments for each chamber using seasonal and yearly thresholds
cumulative_flux = {}
hot_moments_flux_seasonal = {}
for chamber_id in chamber_ids:
    cumulative_flux[chamber_id] = n2o[n2o['Chamber ID'] == chamber_id]['N2O'].sum()
    hot_moments_flux_seasonal[chamber_id] = {'Threshold_IQR': 0, 'Threshold_iForest': 0, 'Threshold_4SD': 0}
    for season_data, season_thresh in [(n2o_first, chamber_thresh_first), (n2o_second, chamber_thresh_second), (n2o_third, chamber_thresh_third)]:
        season_data_chamber = season_data[season_data['Chamber ID'] == chamber_id]
        for method in ['Threshold_IQR', 'Threshold_iForest', 'Threshold_4SD']:
            threshold = season_thresh.loc[season_thresh['Chamber ID'] == chamber_id, method].iloc[0]
            hot_moments_flux_seasonal[chamber_id][method] += season_data_chamber[season_data_chamber['N2O'] > threshold]['N2O'].sum()

hot_moments_flux_yearly = {chamber_id: {method: n2o[(n2o['Chamber ID'] == chamber_id) &
                                                    (n2o['N2O'] > chamber_thresh_yearly.loc[chamber_thresh_yearly['Chamber ID'] == chamber_id, method].iloc[0])]['N2O'].sum()
                                        for method in ['Threshold_IQR', 'Threshold_iForest', 'Threshold_4SD']}
                           for chamber_id in chamber_ids}

# Define colors for seasonal and yearly calculations
seasonal_color = '#56B4E9'
yearly_color = '#D55E00'

# Initialize the plot with no subplot borders
fig, axes = plt.subplots(4, 4, figsize=(25, 25), subplot_kw=dict(frame_on=False))
axes = axes.flatten()

width = 0.9  # Width of the bars
gap = 0.3  # Gap between different methods

# Adjust the positions for bars
for i, chamber_id in enumerate(chamber_ids):
    ax = axes[i]

    ind = np.arange(3)  # Three methods

    # Calculate positions for the bars
    pos1 = ind * (2 * width + gap)  # Position for seasonal bars
    pos2 = pos1 + width  # Position for yearly bars

    # Plot bars for total cumulative flux (seasonal and yearly side by side)
    total_heights = [cumulative_flux[chamber_id]] * 3  # Same height for all methods
    ax.bar(pos1, total_heights, width, label='Total Cumulative N2O Flux', color=seasonal_color)
    ax.bar(pos2, total_heights, width, label='Total Cumulative N2O Flux - Yearly', color=yearly_color)

    # Add shaded areas for hot moments flux (seasonal and yearly)
    for j, method in enumerate(['Threshold_IQR', 'Threshold_iForest', 'Threshold_4SD']):
        # Seasonal calculation
        hot_moments_height_seasonal = hot_moments_flux_seasonal[chamber_id][method]
        ax.bar(pos1[j], hot_moments_height_seasonal, width, color='none', edgecolor='black', hatch='//')
        percentage_seasonal = (hot_moments_height_seasonal / total_heights[j]) * 100
        ax.annotate(format_percentage(percentage_seasonal), (pos1[j], hot_moments_height_seasonal), ha='center', fontsize=22, fontweight='bold')

        # Yearly calculation
        hot_moments_height_yearly = hot_moments_flux_yearly[chamber_id][method]
        ax.bar(pos2[j], hot_moments_height_yearly, width, color='none', edgecolor='black', hatch='//')
        percentage_yearly = (hot_moments_height_yearly / total_heights[j]) * 100
        ax.annotate(format_percentage(percentage_yearly), (pos2[j], hot_moments_height_yearly), ha='center', fontsize=22, fontweight='bold')

    ax.set_xticks(pos1 + width / 2)  # Set x-ticks between the seasonal and yearly bars
    ax.set_xticklabels(["1.5 x\nIQR", "Isolation\nForest", r"4 SD"], fontsize=18, ha='center', va='top')
    ax.tick_params(axis='y', labelsize=22)


# Only show x-axis labels on the last row of the plots
for ax in axes[:-4]:
    ax.set_xticklabels([])

# Removing the borders from the subplots but keeping the axes
for ax in axes:
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['bottom'].set_visible(True)
    ax.spines['left'].set_visible(True)

# Titles for each column and row
column_titles = ["Chamber 1", "Chamber 2", "Chamber 3", "Chamber 4"]
for i, title in enumerate(column_titles):
    fig.text(0.19 + i * 0.205, 0.89, title, ha='center', va='bottom', fontsize=26)

row_titles = ["Node 1", "Node 2", "Node 3", "Node 4"]
for i, title in enumerate(row_titles):
    fig.text(0.87, 0.82 - i * 0.21, title, ha='left', va='center', fontsize=26, rotation=-90)

# Add overall title to the left and bottom
fig.text(0.05, 0.5, r"Cumulative N$_2$O flux (nmol m$^{-2}$ s$^{-1}$)", va='center', rotation='vertical', fontsize=30)
fig.text(0.5, 0.04, 'Threshold determination method', ha='center', va='center', fontsize=30)

# Create custom legend
legend_title = " "
patch_handles = [mpatches.Patch(color=seasonal_color, label='Seasonal Calculation'),
                 mpatches.Patch(color=yearly_color, label='Yearly Calculation')]
patch_handles.append(mpatches.Patch(facecolor='none', edgecolor='black', hatch='//', label='Hot moments'))
lgd = fig.legend(handles=patch_handles, title=legend_title, title_fontsize='24', loc='upper right', bbox_to_anchor=(1.1, 0.5), handlelength=1.5, handleheight=1.5, frameon=False, fontsize=20)
lgd._legend_box.align = "left"
lgd.get_title().set_fontweight('bold')

# Adjust layout
plt.subplots_adjust(right=0.85, hspace=0.1, wspace=0.2)
#plt.savefig('cumulative_season_vs_whole_year_unit_adjusted_yr.pdf',format='pdf')
plt.show()