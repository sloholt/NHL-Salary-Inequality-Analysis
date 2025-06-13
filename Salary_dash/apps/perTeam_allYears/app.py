from dash import Dash, html, dcc, callback, Output, Input
import plotly.express as px
import pandas as pd

df = pd.read_csv('CompleteTeamData.csv')
app = Dash()
app.layout = html.Div([
    html.H1('Team Salary Comparison', style={'textAlign': 'center'}),
     dcc.Dropdown(
        options=[{'label': team, 'value': team} for team in df['Team'].unique()],
        value='ANA',
        id='dropdown-selection'
    ),
    dcc.Graph(id='graph-content')
])

@callback(
    Output('graph-content', 'figure'),
    Input('dropdown-selection', 'value')
)

def update_graph(selected_team): 
    filtered_df = df[df['Team'] == selected_team]
    fig = px.line(filtered_df, x='Year', y='RawGini', title=f'{selected_team} Gini Coefficient Over Time')
    fig.update_traces(mode='lines+markers')
    return fig

if __name__ == '__main__':
    app.run(debug=True) 

    