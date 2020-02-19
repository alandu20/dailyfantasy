from database import *
from position import Positions

class Roster(object):
    """Holds a single roster."""

    def __init__(self, pids):
        super(Roster, self).__init__()
        self.pids = pids

    def to_string(self, db):
        strings = []
        projected_roster_fp = 0
        actual_roster_fp = 0
        for pid in self.pids:
            string = "%s %s %s" % (db.position(pid), 
                db.team(pid), db.name(pid))
            strings.append(string)
            projected_roster_fp += db.projection(pid)
            actual_roster_fp += db.actual(pid)

        # Add the projected and actual points of roster.
        strings.insert(0, "Proj %s" %(projected_roster_fp))
        strings.insert(0, "Actual %s" %(actual_roster_fp))
        return '|'.join(strings)

    def dk_player_order(self, db, pid):
        position_order = {'QB':1, 'RB': 2, 'WR':3, 'TE':4, 'DST':5}
        return position_order[db.position(pid)]

    def print_dk_format(self, db):
        strings = []
        flex = []
        num_required = {'QB':1, 'RB': 2, 'WR':3, 'TE':1, 'DST':1}
        for i, pid in enumerate(sorted(self.pids, key=lambda pid: self.dk_player_order(db, pid))):
            num_required[db.position(pid)] -= 1
            if num_required[db.position(pid)] < 0:
                flex = "%s (%s)" % (db.name(pid), pid)
            else:
                string = "%s (%s)" % (db.name(pid), pid)
                strings.append(string)
            if i == 7:
                strings.append(flex)
        return ', '.join(strings)



class RosterSet(object):
    """Holds sequence of rosters."""

    def __init__(self, rosters=None):
        super(RosterSet, self).__init__()
        rosters = rosters or []
        self.rosters = rosters

    def add(self, pids):
        self.rosters.append(Roster(pids))

    def to_string(self, db):
        return '\n'.join([
          roster.to_string(db)
          for roster in self.rosters
        ])

    def print_dk_format(self, db):
        return '\n'.join([
          roster.print_dk_format(db)
          for roster in self.rosters
        ])



if __name__=='__main__':
    
    csv_name = 'data/2016-09-18.csv'
    db = Database(csv_path = csv_name)
    
    roster1 = Roster([7395497,7395498])
    roster2 = Roster([7395553,7395608])

    roster_set = RosterSet([roster1, roster2])

    print roster_set.to_string(db)

