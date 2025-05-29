import numpy as np
import pandas as pd
from openpyxl import Workbook
import os
import matplotlib.pyplot as plt

# Load once and use globally
df = pd.read_excel("Data\League Wide Cap Hits\LWCH_202425.xlsx", sheet_name=0)


def gini(team):
    """
    Calculate the raw Gini coefficient for a given team.

    Parameters:
    team (list): A list of integers representing the scores of team members.

    Returns:
    float: The Gini coefficient, a measure of inequality.
    """
    n = len(team)
    if n == 0 or np.sum(team) == 0:
        return 0.0
    index = np.arange(1, n + 1)
    return np.sum((2 * index - n - 1) * team) / (n * np.sum(team))

def compute_and_write_team_gini(team_name, output_path="teamGinis.xlsx"):
    team_data = df[df["Team"] == team_name]
    if team_data.empty:
        print(f"No data found for team: {team_name}")
        return

    year = team_data["Year"].iloc[0]
    salaries = np.sort(team_data["Cap Hit"].values)
    raw_gini = gini(salaries)
    adjusted_gini = (len(salaries) + 1) / (len(salaries) - 1) * raw_gini if len(salaries) > 1 else raw_gini

    result_df = pd.DataFrame([{
        "Team": team_name,
        "Year": year,
        "Raw Gini": round(raw_gini, 4),
        "Adjusted Gini": round(adjusted_gini, 4),
        "Roster Size": len(salaries)
    }])

    #Checking if output file exists and add the info if it does
    if os.path.exists(output_path):
        existing = pd.read_excel(output_path)
        combined = pd.concat([existing, result_df], ignore_index=True)
    else:
        combined = result_df

    combined.to_excel(output_path, index=False)
    return result_df

def plot_all_teams_gini():
    """
    Plot the Gini coefficient for all teams over the seasons.
    """
    if not os.path.exists("teamGinis.xlsx"):
        print("Gini data file does not exist. Please compute Gini coefficients first.")
        return

    # Load the Gini data

    gini = pd.read_excel("teamGinis.xlsx")
    gini['Year'] = pd.to_numeric(gini['Year'], errors='coerce')  

    plt.figure(figsize=(12, 6))

# Group and plot
for team in gini['Team'].unique():
    team_data = gini[gini['Team'] == team]
    plt.scatter(team_data['Year'], team_data['Raw Gini'], label=team, alpha=0.6)

plt.title("Raw Gini Coefficient by Team per Season")
plt.xlabel("Season")
plt.ylabel("Raw Gini Coefficient")
plt.xticks(sorted(gini['Year'].dropna().unique().astype(int)))  # Ensure correct X labels
plt.grid(True)
plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left', ncol=1, fontsize='small')
plt.tight_layout()
plt.show()

def plot_gini_over_time(team, filepath = 'teamGinis.xlsx'):
    """
    Plot the Gini coefficient over time for a specific team.

    Parameters:
    team (str): The name of the team to plot.
    filepath (str): Path to the Excel file containing Gini data.
    """
    gini = pd.read_excel("teamGinis.xlsx")
    gini['Year'] = pd.to_numeric(gini['Year'], errors='coerce') 
    team_data = gini[gini['Team'] == team]
    if team_data.empty:
        print(f"No Gini data found for team: {team}")
        return
    
    plt.figure(figsize=(10, 5))
    plt.scatter(team_data['Year'], team_data['Raw Gini'], color='green')
    plt.plot(team_data['Year'], team_data['Raw Gini'], linestyle='--', alpha=0.6)
    for _, row in team_data.iterrows():
        plt.text(row['Year'], row['Raw Gini'] + 0.002, f"{row['Raw Gini']:.3f}", fontsize=8, ha='center')

    plt.title(f"Raw Gini Coefficient for {team} by Season")
    plt.xlabel("Season")
    plt.ylabel("Raw Gini Coefficient")
    plt.xticks(sorted(team_data['Year'].astype(int)))
    plt.grid(True)
    plt.tight_layout()
    plt.show()

plot_gini_over_time('NYI')








