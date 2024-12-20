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


#####Percentage of time that is hot moments by season#########
chamber_ids = n2o_first['Chamber ID'].unique()
threshold = np.zeros_like(chamber_ids)
data = {'Chamber ID': chamber_ids, 'Threshold_IQR': threshold, 'Threshold_iForest': threshold, 'Threshold_2SD': threshold, 'Threshold_4SD': threshold}
chamber_thresh_first = pd.DataFrame(data)
chamber_thresh_second = pd.DataFrame(data)
chamber_thresh_third = pd.DataFrame(data)

# Initialize dictionaries to hold the cumulative hot moments N2O flux for each method and season
hot_moments_flux = {'Threshold_IQR': {}, 'Threshold_iForest': {}, 'Threshold_2SD': {}, 'Threshold_4SD': {}}
season_names = ['Early-Growing', 'Late-Growing', 'Post-Harvest']

# Loop through each season and its corresponding threshold DataFrame
for season_data, season_name, chamber_thresh in [(n2o_first, 'Early-Growing', chamber_thresh_first),
                                                 (n2o_second, 'Late-Growing', chamber_thresh_second),
                                                 (n2o_third, 'Post-Harvest', chamber_thresh_third)]:

    model = IsolationForest(n_estimators=100, max_samples='auto', contamination='auto', random_state=40)

    for id in chamber_ids:
        n2o_sub = copy.copy(season_data.loc[season_data['Chamber ID'] == id])
        hot_iqr = 1.5 * stats.iqr(n2o_sub['N2O'])+np.percentile(n2o_sub['N2O'],75)
        hot_2sd = 2 * np.std(n2o_sub['N2O']) + np.mean(n2o_sub['N2O'])
        hot_4sd = 4 * np.std(n2o_sub['N2O']) + np.mean(n2o_sub['N2O'])

        model.fit(n2o_sub[['N2O']])
        n2o_sub['scores'] = model.decision_function(n2o_sub[['N2O']])
        n2o_sub['anomaly'] = model.predict(n2o_sub[['N2O']])
        hot_iForest = n2o_sub.loc[((n2o_sub['anomaly'] == -1) & (n2o_sub['N2O'] > 0)), 'N2O'].min()

        chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_IQR'] = hot_iqr
        chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_2SD'] = hot_2sd
        chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_4SD'] = hot_4sd
        chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_iForest'] = hot_iForest

    for method in ['Threshold_IQR', 'Threshold_iForest', 'Threshold_2SD', 'Threshold_4SD']:
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

# Initialize dictionaries
hot_moments_flux_chamber = {}
total_season_flux_chamber = {}

# DataFrame to store results
result_df = pd.DataFrame(columns=['Chamber ID', 'Method', 'Season', 'Percentage'])

# Loop through each chamber
for chamber_id in chamber_ids:
    hot_moments_flux_chamber[chamber_id] = {'Early-Growing': {}, 'Late-Growing': {}, 'Post-Harvest': {}}
    total_season_flux_chamber[chamber_id] = {'Early-Growing': 0, 'Late-Growing': 0, 'Post-Harvest': 0}

    # Loop through each season
    for season_data, season_name in [(n2o_first, 'Early-Growing'),
                                     (n2o_second, 'Late-Growing'),
                                     (n2o_third, 'Post-Harvest')]:
        season_data_chamber = season_data[season_data['Chamber ID'] == chamber_id]
        total_season_flux_chamber[chamber_id][season_name] = season_data_chamber['N2O'].sum()

        # Loop through each method
        for method in ['Threshold_IQR', 'Threshold_iForest', 'Threshold_2SD', 'Threshold_4SD']:
            hot_moments_flux_chamber[chamber_id][season_name][method] = 0
            threshold_value = chamber_thresh.loc[chamber_thresh['Chamber ID'] == chamber_id, method].iloc[0]
            hot_moments_flux_value = season_data_chamber.loc[season_data_chamber['N2O'] > threshold_value, 'N2O'].sum()
            hot_moments_flux_chamber[chamber_id][season_name][method] = hot_moments_flux_value

            # Calculate the percentage
            percentage = (hot_moments_flux_value / total_season_flux_chamber[chamber_id][season_name]) * 100

            # Append to result DataFrame
            new_row = {'Chamber ID': chamber_id, 'Method': method, 'Season': season_name, 'Percentage': percentage}
            result_df = pd.concat([result_df, pd.DataFrame([new_row])], ignore_index=True)


# Save the DataFrame to an Excel file
#result_df.to_csv("percentage_cumulative_flux_by_season_2408.csv", index=False)

# Initialize a dictionary to hold the percentage of hot moments time for each method and season
hot_moments_time_percentage = {'Threshold_IQR': {}, 'Threshold_iForest': {}, 'Threshold_2SD': {}, 'Threshold_4SD': {}}
season_names = ['Early-Growing', 'Late-Growing', 'Post-Harvest']

# Loop through each season and its corresponding threshold DataFrame
for season_data, season_name, chamber_thresh in [(n2o_first, 'Early-Growing', chamber_thresh_first),
                                                 (n2o_second, 'Late-Growing', chamber_thresh_second),
                                                 (n2o_third, 'Post-Harvest', chamber_thresh_third)]:

    for method in ['Threshold_IQR', 'Threshold_iForest', 'Threshold_2SD', 'Threshold_4SD']:
        hot_moments_time_percentage[method][season_name] = {}

        for id in chamber_ids:
            threshold_value = chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, method].iloc[0]
            total_observations = len(season_data.loc[season_data['Chamber ID'] == id])
            hot_moments_observations = len(season_data.loc[(season_data['Chamber ID'] == id) & (season_data['N2O'] > threshold_value)])

            if total_observations > 0:  # To avoid division by zero
                percentage = (hot_moments_observations / total_observations) * 100
            else:
                percentage = 0

            hot_moments_time_percentage[method][season_name][id] = percentage

percentage_list = []

# Loop through each method, season, and chamber ID to populate the list
for method in hot_moments_time_percentage.keys():
    for season in hot_moments_time_percentage[method].keys():
        for chamber_id, percentage in hot_moments_time_percentage[method][season].items():
            percentage_list.append([chamber_id,method, season,  percentage])

# Convert the list to a DataFrame
percentage_df = pd.DataFrame(percentage_list, columns=['Chamber ID', 'Method','Season','Percentage'])

# Export to CSV
#percentage_df.to_csv('percentage_time_by_season_2408.csv', index=False)

# Add a 'Season' column to each DataFrame
chamber_thresh_first['Season'] = 'Early-Growing'
chamber_thresh_second['Season'] = 'Late-Growing'
chamber_thresh_third['Season'] = 'Post-Harvest'

# Concatenate the DataFrames
all_thresholds = pd.concat([chamber_thresh_first, chamber_thresh_second, chamber_thresh_third])

# Export to CSV
#all_thresholds.to_csv('chamber_thresholds_by_season_2408.csv', index=False)



##########Percentage of time that is hot moments for whole year#########

# Calculate thresholds for each chamber
chamber_ids = n2o['Chamber ID'].unique()
threshold = np.zeros_like(chamber_ids)
data = {'Chamber ID': chamber_ids, 'Threshold_IQR': threshold, 'Threshold_iForest': threshold, 'Threshold_2SD': threshold, 'Threshold_4SD': threshold}
chamber_thresh = pd.DataFrame(data)

model = IsolationForest(n_estimators=100, max_samples='auto', contamination='auto', random_state=40)

for id in chamber_ids:
    n2o_sub = copy.deepcopy(n2o.loc[n2o['Chamber ID'] == id])
    hot_iqr = 1.5 * stats.iqr(n2o_sub['N2O']) + np.percentile(n2o_sub['N2O'],75)
    hot_2sd = 2 * np.std(n2o_sub['N2O']) + np.mean(n2o_sub['N2O'])
    hot_4sd = 4 * np.std(n2o_sub['N2O']) + np.mean(n2o_sub['N2O'])
    model.fit(n2o_sub[['N2O']])
    n2o_sub['scores'] = model.decision_function(n2o_sub[['N2O']])
    n2o_sub['anomaly'] = model.predict(n2o_sub[['N2O']])
    hot_iForest = n2o_sub.loc[(n2o_sub['anomaly'] == -1) & (n2o_sub['N2O'] > 0), 'N2O'].min()
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_IQR'] = hot_iqr
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_2SD'] = hot_2sd
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_4SD'] = hot_4sd
    chamber_thresh.loc[chamber_thresh['Chamber ID'] == id, 'Threshold_iForest'] = hot_iForest

# Initialize a dictionary for the whole year
hot_moments_time_percentage_year = {'Threshold_IQR': {}, 'Threshold_iForest': {}, 'Threshold_2SD': {}, 'Threshold_4SD': {}}

# Loop through each method and chamber
for chamber_id in chamber_ids:
    n2o_sub = n2o[n2o['Chamber ID'] == chamber_id]
    total_observations = len(n2o_sub)

    for method in ['Threshold_IQR', 'Threshold_iForest', 'Threshold_2SD', 'Threshold_4SD']:
        threshold_value = chamber_thresh.loc[chamber_thresh['Chamber ID'] == chamber_id, method].iloc[0]
        hot_moments_observations = len(n2o_sub[n2o_sub['N2O'] > threshold_value])

        if total_observations > 0:  # To avoid division by zero
            percentage = (hot_moments_observations / total_observations) * 100
        else:
            percentage = 0

        hot_moments_time_percentage_year[method][chamber_id] = percentage

# Flatten the dictionary to a list of tuples for the DataFrame
data = []
for method, chambers in hot_moments_time_percentage_year.items():
    for chamber_id, percentage in chambers.items():
        data.append((method, chamber_id, percentage))

# Create a DataFrame for the percentages
percentage_df_year = pd.DataFrame(data, columns=['Method', 'Chamber ID', 'Percentage'])

# Export to CSV
#percentage_df_year.to_csv('percentage_time_by_year_2408.csv', index=False)

# Initialize dictionaries
total_year_flux_chamber = {}

# DataFrame to store results
percentage_year_flux_df = pd.DataFrame(columns=['Chamber ID', 'Method', 'Percentage'])

# Loop through each chamber
for chamber_id in chamber_ids:

    chamber_data = n2o[n2o['Chamber ID'] == chamber_id]
    total_year_flux_chamber[chamber_id] = chamber_data['N2O'].sum()

    # Loop through each method
    for method in ['Threshold_IQR', 'Threshold_iForest', 'Threshold_2SD', 'Threshold_4SD']:
        threshold_value = chamber_thresh.loc[chamber_thresh['Chamber ID'] == chamber_id, method].iloc[0]
        hot_moments_flux_value = chamber_data.loc[chamber_data['N2O'] > threshold_value, 'N2O'].sum()

        # Calculate the percentage
        percentage = (hot_moments_flux_value / total_year_flux_chamber[chamber_id]) * 100

        # Append to result DataFrame
        new_row = {'Chamber ID': chamber_id, 'Method': method, 'Percentage': percentage}
        percentage_year_flux_df = pd.concat([percentage_year_flux_df, pd.DataFrame([new_row])], ignore_index=True)

percentage_year_flux_df
# Save the DataFrame to an Excel file
#percentage_year_flux_df.to_csv("percentage_cumulative_flux_by_year_2408.csv", index=False)

#chamber_thresh.to_csv("threshold_by_year_2408.csv", index=False)