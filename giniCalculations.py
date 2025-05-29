import numpy as np
import pandas as pd
from openpyxl import Workbook
import os

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

compute_and_write_team_gini('ANA')
compute_and_write_team_gini('ARI')
compute_and_write_team_gini('BOS')
compute_and_write_team_gini('BUF')
compute_and_write_team_gini('CAR')
compute_and_write_team_gini('CBJ')
compute_and_write_team_gini('CGY')
compute_and_write_team_gini('CHI')
compute_and_write_team_gini('COL')
compute_and_write_team_gini('DAL')
compute_and_write_team_gini('DET')
compute_and_write_team_gini('EDM')
compute_and_write_team_gini('FLA')
compute_and_write_team_gini('LAK')
compute_and_write_team_gini('MIN')
compute_and_write_team_gini('MTL')
compute_and_write_team_gini('NJD')
compute_and_write_team_gini('NSH')
compute_and_write_team_gini('NYI')
compute_and_write_team_gini('NYR')
compute_and_write_team_gini('OTT')
compute_and_write_team_gini('PHI')
compute_and_write_team_gini('PIT')
compute_and_write_team_gini('SEA')
compute_and_write_team_gini('SJS') 
compute_and_write_team_gini('STL')
compute_and_write_team_gini('TBL')
compute_and_write_team_gini('TOR')
compute_and_write_team_gini('VAN')
compute_and_write_team_gini('VGK')
compute_and_write_team_gini('WAS')
compute_and_write_team_gini('WPG')
