import pandas

class Database(object):

    def __init__(self, dataframe=None, csv_path=None):
        """Must supply either dataframe or csv_path."""
        super(Database, self).__init__()
        self.df = dataframe if dataframe is not None else pandas.read_csv(csv_path)

    def teams(self):
        """Return all teams."""
        return tuple(sorted(set(self.df.Team)))

    def pids(self):
        """Return all player ids."""
        return tuple(sorted(set(self.df.ID)))

    def matchups(self):
        """Return all matchups."""
        return set(map(tuple, map(sorted, zip(self.df.Team, self.df.Opponent))))

    def opponent(self, team):
        """Return opponent given a team."""
        return  list(self.df[self.df.Team == team].Opponent)[0]

    def pid_positions(self, position):
        """All player ids for given position."""
        return set(self.df[self.df.Position == position].ID)

    def pid_teams(self, team):
        """All GIDs for given team."""
        return set(self.df[self.df.Team == team].ID)

    def name(self, pid):
        return list(self.df[self.df.ID == pid].Name)[0]

    def team(self, pid):
        return list(self.df[self.df.ID == pid].Team)[0]

    def position(self, pid):
        return list(self.df[self.df.ID == pid].Position)[0]

    def salary(self, pid):
        return list(self.df[self.df.ID == pid].Salary)[0]

    def projection(self, pid):
        return list(self.df[self.df.ID == pid].Projection_dfn)[0]

    def actual(self, pid):
        return list(self.df[self.df.ID == pid].Actual)[0]


if __name__=='__main__':

    csv_name = 'data/2016-09-18.csv'
    db = Database(csv_path = csv_name)
    print db.opponent('DEN')

