import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.ensemble import IsolationForest
import copy
import scipy.stats as stats
import matplotlib.patches as mpatches

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

plt.rcParams['font.family'] = 'Arial'
# Initialize dictionaries to hold the cumulative hot moments N2O flux and total season flux for each chamber
hot_moments_flux_chamber = {}
total_season_flux_chamber = {}

# Loop through each chamber
for chamber_id in chamber_ids:

    # Initialize sub-dictionaries for each chamber
    hot_moments_flux_chamber[chamber_id] = {'Early-Growing': {}, 'Late-Growing': {}, 'Post-Harvest': {}}
    total_season_flux_chamber[chamber_id] = {'Early-Growing': 0, 'Late-Growing': 0, 'Post-Harvest': 0}

    # Loop through each season
    for season_data, season_name in [(n2o_first, 'Early-Growing'),
                                     (n2o_second, 'Late-Growing'),
                                     (n2o_third, 'Post-Harvest')]:

        # Filter by chamber
        season_data_chamber = season_data[season_data['Chamber ID'] == chamber_id]

        # Calculate the total N2O flux for this chamber and season
        total_season_flux_chamber[chamber_id][season_name] = season_data_chamber['N2O'].sum()

        # Loop through each method
        for method in ['Threshold_IQR', 'Threshold_iForest', 'Threshold_4SD']:

            # Initialize the hot moments flux to 0
            hot_moments_flux_chamber[chamber_id][season_name][method] = 0

            # Get the threshold for this chamber and method

            threshold_value = chamber_thresh.loc[chamber_thresh['Chamber ID'] == chamber_id, method].iloc[0]

            # Calculate the hot moments flux for this chamber, season, and method
            hot_moments_flux_value = season_data_chamber.loc[season_data_chamber['N2O'] > threshold_value, 'N2O'].sum()

            # Store the calculated value
            hot_moments_flux_chamber[chamber_id][season_name][method] = hot_moments_flux_value

methods = list(hot_moments_flux.keys())
season_names = ['Early-Growing', 'Late-Growing', 'Post-Harvest']
chamber_ids = list(hot_moments_flux_chamber.keys())

# Initialize the plot
fig, axes = plt.subplots(4, 4, figsize=(25, 25), subplot_kw=dict(frame_on=False))

# Flatten the axes array
axes = axes.flatten()

# Colors for each season
colors = ['#E5AB02', '#A5761C', '#666666']

# Adjust space between subplots
plt.subplots_adjust(hspace=0.1, wspace=0.2)

# Loop through each chamber to create the bars
for k, chamber_id in enumerate(chamber_ids):
    ax = axes[k]

    # X-axis positions for the bars
    ind = np.arange(len(methods))

    # Width of the bars
    width = 0.9

    # Loop through each method
    for i, method in enumerate(methods):
        # The bottom of the next segment in the bar
        bottom_value = 0

        # Loop through each season to create the segments of each bar
        for j, season in enumerate(season_names):
            ax.bar(ind[i], total_season_flux_chamber[chamber_id][season], width, color=colors[j], bottom=bottom_value, label=f'{season}' if i == 0 and k == 0 else "")

            # Option 1 and 2: Use less dense hatch lines and lighter color
            ax.bar(ind[i], hot_moments_flux_chamber[chamber_id][season][method], width, color='none', edgecolor='black', hatch='/', bottom=bottom_value)

            # Calculate the percentage of hot moments flux
            percentage = (hot_moments_flux_chamber[chamber_id][season][method] / total_season_flux_chamber[chamber_id][season]) * 100

            # Option 3 and 4: Annotate with a background box and different text color
            ax.annotate(format_percentage(percentage), (ind[i], bottom_value + hot_moments_flux_chamber[chamber_id][season][method] / 2),
                        textcoords="offset points", xytext=(0,10), ha='center',
                        bbox=dict(boxstyle="round,pad=0.3", edgecolor="none", facecolor="none", alpha=0.7),
                        fontsize=18, color='black', fontweight='bold')

            # Update the bottom_value for the next segment
            bottom_value += total_season_flux_chamber[chamber_id][season]

        # Add labels
        ax.set_xticks(ind)
        ax.set_xticklabels(["1.5 x\nIQR", "Isolation\nForest", r"4 SD"], fontsize=14, ha='center', va='top')
        # Set y-tick label size
        ax.tick_params(axis='y', labelsize=18)

# Show only x-axis labels on the last row of the plots
for ax in axes[:-4]:  # Assuming there are 16 subplots in a 4x4 grid
    ax.set_xticklabels([])  # Hide x-axis labels for all but last row

# Removing the borders from the subplots but keeping the axes
for ax in axes:
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['bottom'].set_visible(True)  # Keep bottom spine for x-axis
    ax.spines['left'].set_visible(True)   # Keep left spine for y-axis


# Titles for each column at the top
column_titles = ["Chamber 1", "Chamber 2", "Chamber 3", "Chamber 4"]
for i, title in enumerate(column_titles):
    fig.text(0.19 + i * 0.2, 0.89, title, ha='center', va='bottom', fontsize=20, fontweight='normal')

# Titles for each row at the right
row_titles = ["Node 1", "Node 2", "Node 3", "Node 4"]
for i, title in enumerate(row_titles):
    fig.text(0.87, 0.82 - i * 0.22, title, ha='left', va='center', fontsize=20, rotation=-90, fontweight='normal')

# Add overall title to the left
fig.text(0.05, 0.5, r"Cumulative N$_2$O flux (nmol m$^{-2}$ s$^{-1}$)", va='center', rotation='vertical', fontsize=28, fontweight='bold')
fig.text(0.5, 0.04, 'Threshold determination method', ha='center', va='center', fontsize=28, fontweight='bold')


# Create custom legend handles with "Season" title, bold and larger font size.
legend_title = "Season"
patch_handles = [mpatches.Patch(color=colors[i], label=season) for i, season in enumerate(["Early growing season", "Late growing season", "Post harvest & wintertime"])]
patch_handles.append(mpatches.Patch(facecolor='none', edgecolor='black', hatch='//', label='Hot moments'))
lgd = plt.legend(handles=patch_handles, title=legend_title, title_fontsize='20', loc='right', bbox_to_anchor=(2.1, 2.1), handlelength=1.5, handleheight=1.5, frameon=False, fontsize=18)
lgd._legend_box.align = "left"  # Align the legend title to the left
lgd.get_title().set_fontweight('bold')


# Make room for the legend to the right of the plot
plt.subplots_adjust(right=0.85)
#plt.savefig('chamber_cumulative_unit_adjusted_yr.pdf',format='pdf', dpi=300)
# Show the plot
plt.show()