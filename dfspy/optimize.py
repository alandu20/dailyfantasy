from position import Positions
from database import Database
import pulp
import roster as rost


SALARY_CAP = 50000
MIN_PARTICIPATING_TEAMS = 2
MIN_PARTICIPATING_MATCHUPS = 2

NUM_PLAYERS = 9
OVERLAP_CEILING = 5
QB_WR_CLUSTERS = 1
QB_WR_CLUSTER_SIZE = 3

class OptimizationProblem(object):
    """Optimization problem definition"""

    def __init__(self, pulp_problem, player_variables):
        super(OptimizationProblem, self).__init__()
        self.pulp_problem = pulp_problem
        self.player_variables = player_variables

    def best_pids(self):
        self.pulp_problem.solve()
        if self.pulp_problem.status <= 0:
            raise Exception("Infeasible Solution.")
        return {pid for pid, variable 
            in self.player_variables.iteritems()
            if variable.varValue}



def optimization_problem(db, roster_set=None):

    if roster_set is None:
        roster_set = rost.RosterSet()

    # Establish variables.
    player_vars = {pid: pulp.LpVariable(name="player-%s-%s-%s-%s" % 
        (db.position(pid), db.team(pid), db.name(pid), pid), cat='Binary')
        for pid in db.pids()}

    team_vars = {team: pulp.LpVariable(name="team-%s" % team, cat='Binary')
      for team in db.teams()}

    matchup_vars = {matchup: pulp.LpVariable(name="matchup-%s" % '-'.join(matchup), 
        cat='Binary') for matchup in db.matchups()}

    # Objective function and optimization problem.
    objective = sum([player_vars[pid] * projections 
        for pid, projections in zip(db.df.ID, db.df.Projection_dfn)])
    prob = pulp.LpProblem("DK Optimization", pulp.LpMaximize)
    prob += objective, "maximize expectied fantasy points"

    # Feasibility constraints.
    prob += (sum(player_vars.values()) == NUM_PLAYERS,
        "%s players required" % NUM_PLAYERS)
    for position in Positions.all():
        # Number of active players for position.
        active_in_position = sum(player_vars[pid] for pid 
            in db.pid_positions(position))
        required_in_position = Positions.num_required(position)
        
        # Bounds on the active player per position.
        if Positions.is_flex(position):
            prob += (active_in_position >= required_in_position,
                "%s requires at LEAST %s players" % (position, required_in_position))
            prob += (active_in_position <= required_in_position+1,
                "%s requires at MOST %s players" % (position, required_in_position+1))
        else:
            prob += (active_in_position == required_in_position,
                "%s requires at MOST %s players" % (position, required_in_position))
    
    # The total money spent on active players must be <= than salary cap.
    prob += (sum(SALARY_CAP >= player_vars[pid] * salary
        for pid, salary in zip(db.df.ID, db.df.Salary)),
        "Must have salary below %s" % SALARY_CAP)
    
    # Overlap constraint.
    for i, roster in enumerate(roster_set.rosters):
        overlap = 0.0
        for pid in roster.pids:
            overlap += player_vars[pid]
        prob += (overlap <= OVERLAP_CEILING,
            "Roster %d overlap must not exceed %s" % (i, OVERLAP_CEILING))


    return OptimizationProblem(prob, player_vars)


def best_pids(db, roster_set):
    return optimization_problem(db, roster_set).best_pids()


def fill_roster_set(db, roster_size):
    roster_set = rost.RosterSet()
    for _ in range(roster_size):
        roster_set.add(best_pids(db, roster_set))
    return roster_set


if __name__ == '__main__':
    csv_name = 'data/2016-09-18.csv'
    db = Database(csv_path = csv_name)

    roster_set = fill_roster_set(db, 150)
    print roster_set.to_string(db)











