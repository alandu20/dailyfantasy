from optimizer import *
from database import *
from roster import *
import cPickle as pickle
import pulp


def formulation_1(db, roster_set_size=150, solver=pulp.PULP_CBC_CMD()):
    op = OptimizationProblem(db)
    op.add_objective()
    op.add_feasibility_constraint()
    op.add_overlap_constraint()
    op.add_qb_stack_constraint(num_wrs=1, num_tes=0, num_rbs=0)
    op.add_opp_dst_constraint(no_qb=True, no_wr=True, 
                              no_rb=True, no_te=True)
    op.solve(roster_set_size, solver=solver)
    return op.roster_set, str(op)

def formulation_4(db, roster_set_size=150, solver=pulp.PULP_CBC_CMD()):
    op = OptimizationProblem(db)
    op.add_objective()
    op.add_feasibility_constraint()
    op.add_overlap_constraint()
    op.add_qb_stack_constraint(num_wrs=1, num_tes=0, num_rbs=0)
    op.add_opp_dst_constraint(no_qb=True, no_wr=True, 
                              no_rb=True, no_te=True)
    op.add_no_flex_constraint(no_te=True)
    op.add_exposure_constraint(qb_exp=0.4, wr_exp=0.4, 
                               rb_exp=0.4, te_exp=0.4, def_exp=0.4)
    op.solve(roster_set_size, solver=solver)
    return op.roster_set, str(op)

def formulation_14(db, roster_set_size=150, solver=pulp.PULP_CBC_CMD()):
    op = OptimizationProblem(db)
    op.add_objective()
    op.add_feasibility_constraint()
    op.add_overlap_constraint(overlap_ceiling=4)
    op.add_qb_stack_constraint(num_wrs=1, num_tes=0, num_rbs=0)
    op.add_opp_dst_constraint(no_qb=True, no_wr=True, 
                              no_rb=True, no_te=True)
    op.add_exposure_constraint()
    op.solve(roster_set_size, solver=solver)
    return op.roster_set, str(op)




if __name__ == '__main__':

    for week in xrange(3, 4):

        print '------------------------BEGINNING WEEK %s------------------------' %week
        csv_name = '_data/2016-%s/records.csv' %(week)
        db = Database(csv_path=csv_name)
        roster_set, summary = formulation_4(db, 150, solver = pulp.GUROBI(msg=False))

        with open('_experiments/formulation_4/2016-%s.pickle' %week, 'wb') as output:
            pickle.dump(roster_set, output, protocol=pickle.HIGHEST_PROTOCOL)

    with open("_experiments/formulation_4/readme.txt", "w") as output:
        output.write(summary)

