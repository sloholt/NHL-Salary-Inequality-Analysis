from dash import Dash, html, dcc, callback, Output, Input
import plotly.express as px
import pandas as pd
import json

with open("Team_Logos.json", "r") as f: 
    logo_map = json.load(f)

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
        hover_name='Team',
        title=f'Gini Coefficient vs ROW in {selected_year}',
        labels={
            'RawGini': 'Gini Coefficient',
            'ROW': 'Regulation + Overtime Wins'
        }
    )
    for _, row in filtered_df.iterrows():
        abbr = row["Team"]
        logo_url = logo_map.get(abbr)
        if logo_url: 
            logo_url = logo_url.replace("github.com", "raw.githubusercontent.com").replace("/blob/", "/")
            fig.add_layout_image(
                dict(
                    source = logo_url, 
                    x=row["RawGini"],
                    y=row["ROW"],
                    xref='x',
                    yref="y",
                    xanchor="center",
                    yanchor="middle",
                    sizex=0.01,
                    sizey=1.5, 
                    sizing="contain",
                    opacity = 1,
                    layer = "above"
                )
            )
    fig.update_traces(marker_opacity=0)
    fig.update_layout(
        title=f'Gini Coefficient vs ROW in {selected_year}',
        hoverlabel=dict(bgcolor="white", font=dict(size=13,family="Arial")),
        plot_bgcolor="#FFFFFF",
        height=600,
        margin=dict(l=20, r=20, t=40, b=20)
    )
    return fig

if __name__ == '__main__':
    app.run(debug=True)
