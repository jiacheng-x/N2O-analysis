import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


n2o = pd.read_excel("N2O_Fluxes.xlsx")
n2o['Date'] = pd.to_datetime(n2o['Date'])

n2o = n2o.drop(n2o[n2o['Chamber']=='Chamber 5'].index).sort_values(by=['Node', 'Chamber'])

chamber_ids = n2o['Chamber ID'].unique()

# Set global font to Arial
plt.rcParams['font.family'] = 'Arial'

fig, axes = plt.subplots(4, 4, figsize=(35, 35))

for index, chamber_id in enumerate(chamber_ids):
    plt.subplot(4, 4, index + 1)
    g = sns.scatterplot(data=n2o.loc[n2o['Chamber ID'] == chamber_id], x="Date", y='N2O', s=300, edgecolor='none')  # Enlarge scatter plot points
    g.set_xlabel('', labelpad=0)
    g.set_ylabel('', labelpad=0)

    for item in g.get_xticklabels():
        item.set_rotation(45)
        item.set_fontsize(31)  # Font size for x-axis labels

    for item in g.get_yticklabels():
        item.set_fontsize(31)  # Font size for y-axis labels


column_titles = ["Chamber 1", "Chamber 2", "Chamber 3", "Chamber 4"]
for i, title in enumerate(column_titles):
    # Adjust x position based on the subplot's position and width
    x_position = (axes[0, i].get_position().x0 + axes[0, i].get_position().x1) / 2
    if i == 1:
        adjustment_factor = -0.01  # Adjust this value as needed
        x_position += adjustment_factor
    elif i == 2:
        adjustment_factor = -0.03  # Adjust this value as needed
        x_position += adjustment_factor
    elif i == 3:
        adjustment_factor = -0.04  # Adjust this value as needed
        x_position += adjustment_factor
    else: x_position += 0
    fig.text(x_position, 0.89, title, ha='center', va='center', fontsize=50, fontweight='normal')

row_titles = ["Node 1", "Node 2", "Node 3", "Node 4"]
for i, title in enumerate(row_titles):
    # Adjust y position based on the subplot's position and height
    y_position = (axes[i, 0].get_position().y0 + axes[i, 0].get_position().y1) / 2
    fig.text(0.86, y_position, title, ha='left', va='center', fontsize=50, rotation=-90, fontweight='normal')


# Common Y-axis title
plt.figtext(0.06, 0.5, r"Cumulative N$_2$O flux (nmol m$^{-2 -s}$ )", va='center', rotation='vertical', fontsize=60, fontweight='bold')

# Common X-axis title
plt.figtext(0.5, 0.04, 'Date', ha='center', fontsize=60, fontweight='bold')

plt.subplots_adjust(right=0.85, wspace=0.3, hspace=0.3)

#plt.savefig('all_year_original.pdf',format='pdf')
plt.show()