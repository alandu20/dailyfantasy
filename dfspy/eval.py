from roster import *
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import cPickle as pickle

# NOTES: week 12 has no player ids, week 14 has no defense actual fp's.


class Evaluation(object):
    """Evaluation class for a generated roseter set."""

    def __init__(self, db, payout_df= None, payout_path=None, 
                 standings_df= None, standings_path=None,
                 roster_set=None):
        super(Evaluation, self).__init__()
        if roster_set == None:
            roster_set = RosterSet()
        self.roster_set = roster_set

        self.db = db

        # Dataframes to store evaluation metrics.
        self.payout = payout_df if payout_df is not None else pandas.read_csv(payout_path)
        self.standings = standings_df if standings_df is not None else pandas.read_csv(standings_path)
        
        # Map roster to actual fantasy points earned.
        self.actual_fp = {}
        for roster in self.roster_set.rosters:
            actual_roster_fp = 0
            for pid in roster.pids:
                actual_roster_fp += self.db.actual(pid)
            self.actual_fp[roster] = actual_roster_fp

        # Map roster standings.
        self.roster_standings = {}
        for roster in self.roster_set.rosters:
            rankings = list(self.standings[self.standings.Points > 
                self.actual_fp[roster]].Rank)
            rank = 1 if len(rankings) == 0 else rankings[-1]-1
            self.roster_standings[roster] = rank

        # TODO: Map payout.
        self.roster_payouts = {}


    def money_earned(self):
        """Payout for current roster set."""
        return 


    def percent_itm(self):
        """Percentage of rosters in the money."""
        return


    def average_standing(self):
        """Returns the average roster standing."""
        return np.mean(self.roster_standings.values())


    def plot_standings(self):
        """Plot the roster sets standings distrbution."""
        bins = list(self.payout.Place_lo)
        plt.xscale('log')
        plt.hist(self.roster_standings.values(),bins=bins) 
        plt.show()


    def plot_actual_fp(self, bin_width=5):
        """Plot the distribution of realized fantasy points."""
        bins = range(int(min(self.actual_fp.values())), 
                     int(max(self.actual_fp.values()))+bin_width, bin_width)
        plt.hist(self.actual_fp.values())
        plt.show()


if __name__ == '__main__':

    week = 3

    csv_name = '_data/2016-%s/records.csv' %week
    db = Database(csv_path = csv_name)

    with open('_experiments/formulation_4/2016-%s.pickle' %week, 'rb') as input:
        roster_set = pickle.load(input)

    standings_path = '_data/2016-%s/contest_standings.csv' %week
    payout_path = '_data/2016-%s/payout_structure.csv' %week

    ev = Evaluation(db=db,
                    payout_path=payout_path,
                    standings_path=standings_path,
                    roster_set=roster_set)
    print roster_set.print_dk_format(db)
    ev.plot_standings()
    ev.plot_actual_fp()