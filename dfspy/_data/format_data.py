import pandas as pd


csv_path_offense = '2016-10-02/$20.00entry_NFLMILLIONAIREMAKERWK4/offensive_players.csv'
csv_path_defense = '2016-10-02/$20.00entry_NFLMILLIONAIREMAKERWK4/defenses.csv'
df_offense = pd.read_csv(csv_path_offense)
df_defense = pd.read_csv(csv_path_defense)
df = pd.concat([df_offense, df_defense])
df['Team'] = map(lambda x: x.upper(), df['Team'])
df['Opponent'] = map(lambda x: x.upper(), df['Opponent'])
df.to_csv('2016-10-02.csv', index=False)
