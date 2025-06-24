from dash import Dash, html, dcc, callback, Output, Input
import plotly.express as px
import pandas as pd
import webbrowser


df = pd.read_csv('CompleteTeamData.csv')
app = Dash()

app.layout = html.Div([
    html.H1('Team Gini Coefficient by Year', style={'textAlign': 'center'}),
    dcc.Dropdown(
        options=[{'label': str(year), 'value': year} for year in sorted(df['Year'].unique())],
        id='dropdown-selection'
    ), 
    dcc.Graph(id='graph-content')
])

@callback(
    Output('graph-content', 'figure'), 
    Input('dropdown-selection', 'value')
)

def update_graph(selected_year):
    filtered_df = df[df['Year'] == selected_year]
    fig = px.bar(
        filtered_df, 
        x='Team', 
        y='RawGini', 
        title=f'Raw Gini Coefficient by Team in {selected_year}',
        labels={'RawGini': 'Raw Gini Coefficient'}
    )
    return fig
if __name__ == '__main__':
    app.run(debug=True)

"""
fig = px.density_heatmap(df, x='Year', y='RawGini', marginal_x="histogram", marginal_y="violin")
fig.show()
fig.write_html("test_plot.html")
webbrowser.open("test_plot.html")
"""