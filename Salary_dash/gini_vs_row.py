from dash import Dash, html, dcc, callback, Output, Input
import plotly.express as px
import pandas as pd

df = pd.read_csv('CompleteTeamData.csv')
app = Dash()

app.layout = html.Div([
    html.H1('Gini Coefficient vs ROW per Team', 
            style={'textAlign': 'center'}),
    dcc.Dropdown(
        options=[{'label': str(year), 
                  'value': year} for year in sorted(df['Year'].unique())],
                  id='dropdown-selection', 
                  placeholder='Select a year'
    ),
    dcc.Graph(id='graph-content')
])

@callback(
    Output('graph-content', 'figure'),
    Input('dropdown-selection', 'value')
)

def update_graph(selected_year):
    filtered_df = df[df['Year'] == selected_year]
    fig = px.scatter(
        filtered_df,
        x='RawGini',
        y='ROW',
        text='Team',
        title=f'Gini Coefficient vs ROW in {selected_year}',
        labels={
            'RawGini': 'Gini Coefficient',
            'ROW': 'Regulation + Overtime Wins'
        }
    )
    return fig

if __name__ == '__main__':
    app.run(debug=True)
