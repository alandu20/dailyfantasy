module formulations 

# To install DataFrames, simply run Pkg.add("DataFrames")
using DataFrames

#=
GLPK is an open-source solver, and additionally Cbc is an open-source solver. This code uses GLPK
because we found that it was slightly faster than Cbc in practice. For those that want to build
very sophisticated models, they can buy Gurobi. To install GLPKMathProgInterface, simply run
Pkg.add("GLPKMathProgInterface")
=#
#using GLPKMathProgInterface
using Gurobi

# Once again, to install run Pkg.add("JuMP")
using JuMP

export one_lineup_no_stacking, one_lineup_Type_1, one_lineup_Type_2, one_lineup_Type_3,
one_lineup_Type_4, one_lineup_Type_5, one_lineup_Type_6, one_lineup_Type_7, one_lineup_Type_8, one_lineup_Type_9, one_lineup_Type_10, one_lineup_Type_11,one_lineup_Type_12, one_lineup_Type_13, one_lineup_Type_14


############################  Lineup Generator Functions  ############################

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Only Feasibility Constraints 
function one_lineup_no_stacking(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())
    # Variable for skaters in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for goalie in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # between 1 and 2 TE (Because of FLEX player)
    @constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)
    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)
    # Objective
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
function one_lineup_Type_1(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # between 1 and 2 TE (Because of FLEX player)
    @constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)

    # Objective
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end


    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
function one_lineup_Type_2(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # between 1 and 2 TE (Because of FLEX player)
    @constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @variable(m, QBWR_stack[i=1:num_pairs], Bin)
    @constraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)

    # Objective
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
# - QB-oppWR
function one_lineup_Type_3(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    # #=
    # DraftKings Fantasy Contests require the following lineup:
    #     - 1xQB
    #     - 2xRB
    #     - 3xWR 
    #     - 1xTE
    #     - 1xFLEX (RB/WR/TE)
    #     - 1xDST
    # Whose salaries sum to less than $55,000
    # =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # between 1 and 2 TE (Because of FLEX player)
    @constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 2 different teams represented between the 8 offensive players (constructed w/o defenses_lineup b/c will effectively never have an entire offense from the same team so DK rule will be satisfied)
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @variable(m, QBWR_stack[i=1:num_pairs], Bin)
    @constraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)
    

    # Must have a QB/opp-WR Pair
    @variable(m, QBoppWR_stack[i=1:num_pairs_QBoppWR], Bin)
    @constraint(m, constr[i=1:num_pairs_QBoppWR], 10*QBoppWR_stack[i] <= sum{team_pairs_QBoppWR[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBoppWR_stack[i], i=1:num_pairs_QBoppWR} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)
    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)
    # Objective
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
# - no TE for flex
function one_lineup_Type_4(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @variable(m, QBWR_stack[i=1:num_pairs], Bin)
    @constraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)
    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)
    # Objective
    # @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn_perturbed")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn_perturbed]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
# - no TE or RB for flex (must be WR)
function one_lineup_Type_5(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # 2 RB (no RB FLEX player)
    @constraint(m, 2 == sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    # @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    # @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @variable(m, QBWR_stack[i=1:num_pairs], Bin)
    @constraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)
    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)
    # Objective
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
# - no TE for flex
# - QB-oppWR Stack
function one_lineup_Type_6(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)
    
    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @variable(m, QBWR_stack[i=1:num_pairs], Bin)
    @constraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)
    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)
    # Objective
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
# - no TE for flex
# - QB-oppWR Stack
# - Must have a Value RB (RB worth less than 5000)
function one_lineup_Type_7(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)
    
    # Must have 1 Value RB 
    @constraint(m, sum{cheapRunningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @variable(m, QBWR_stack[i=1:num_pairs], Bin)
    @constraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Must have a QB/opp-WR Pair
    @variable(m, QBoppWR_stack[i=1:num_pairs_QBoppWR], Bin)
    @constraint(m, constr[i=1:num_pairs_QBoppWR], 10*QBoppWR_stack[i] <= sum{team_pairs_QBoppWR[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBoppWR_stack[i], i=1:num_pairs_QBoppWR} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)
    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)
    # Objective
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
# - no TE for flex
# - QB-oppWR Stack
# - Must have a Value RB (RB worth less than 5000)
# - RB can not be from the same team as a WR or TE. 
function one_lineup_Type_8(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source,team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)
    
    # Must have 1 Value RB 
    @constraint(m, sum{cheapRunningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @variable(m, QBWR_stack[i=1:num_pairs], Bin)
    @constraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Must have a QB/opp-WR Pair
    @variable(m, QBoppWR_stack[i=1:num_pairs_QBoppWR], Bin)
    @constraint(m, constr[i=1:num_pairs_QBoppWR], 10*QBoppWR_stack[i] <= sum{team_pairs_QBoppWR[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBoppWR_stack[i], i=1:num_pairs_QBoppWR} >= 1)

    # Can not have a WR/RB from the same team
    @variable(m, RBWR_stack[i=1:num_pairs_RBWR], Bin)
    @constraint(m, constr[i=1:num_pairs_RBWR], 10*RBWR_stack[i] <= sum{team_pairs_RBWR[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{RBWR_stack[i], i=1:num_pairs_RBWR} >= 3)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)
    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)
    # Objective
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-Top Reciever Stack (If you have a QB then also include a Top Reciever from the same team)
#   - WR must be #1/#2 in terms of Targets Rank 
# - no TE for flex
# - Must have a Value RB (RB worth less than 5000)
function one_lineup_Type_9(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)
    
    # Must have 1 Value RB 
    @constraint(m, sum{cheapRunningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB-Top Target Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum == 10 there must be 
    # at least a QB/Reciever Pair
    @variable(m, targets_stack[i=1:num_pairs_targets], Bin)
    @constraint(m, constr[i=1:num_pairs_targets], 11*targets_stack[i] == sum{team_pairs_targets[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{targets_stack[i], i=1:num_pairs_targets} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)
    # Objective
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn_perturbed")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn_perturbed]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-Top Reciever Target Stack (If you have a QB then also include a WR from the same team)
# - Can not have 3 players from the same team
# - no TE for flex
# - This is just Formulation 4 + the Top Reciever instead of Random WR Stack + no 3 players from same team. 
function one_lineup_Type_10(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $50,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # Can't have 3 offensive players from the same team
    for team in 1:num_teams
        @constraint(m, sum{offensive_players_teams[t, team]*offensive_players_lineup[t], t=1:num_offensive_players} <= 2)
    end

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB-Top Target Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum == 10 there must be 
    # at least a QB/Reciever Pair
    @variable(m, targets_stack[i=1:num_pairs_targets], Bin)
    @constraint(m, constr[i=1:num_pairs_targets], 11*targets_stack[i] == sum{team_pairs_targets[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{targets_stack[i], i=1:num_pairs_targets} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)

    # Objective
    # @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn_perturbed")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn_perturbed]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-Top Reciever Target Stack (If you have a QB then also include a WR from the same team)
# - Can not have more than 3 players from the same team
# - no TE for flex
# - Must have a Value RB (RB worth less than 5000)
# - This is just Formulation 7 + the Top Reciever instead of Random WR Stack + + no 3 players from same team 
function one_lineup_Type_11(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $50,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # Can't have 3 offensive players from the same team
    for team in 1:num_teams
        @constraint(m, sum{offensive_players_teams[t, team]*offensive_players_lineup[t], t=1:num_offensive_players} <= 2)
    end

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB-Top Target Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum == 10 there must be 
    # at least a QB/Reciever Pair
    @variable(m, targets_stack[i=1:num_pairs_targets], Bin)
    @constraint(m, constr[i=1:num_pairs_targets], 11*targets_stack[i] == sum{team_pairs_targets[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{targets_stack[i], i=1:num_pairs_targets} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)

    # Objective
    # @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn_perturbed")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn_perturbed]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-Top Reciever Target Stack (If you have a QB then also include a WR from the same team)
# - no TE for flex
# - This is just Formulation 10 but allows for 3 players on the same team instead of 2

function one_lineup_Type_12(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $50,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # # Can't have 3 offensive players from the same team
    for team in 1:num_teams
        @constraint(m, sum{offensive_players_teams[t, team]*offensive_players_lineup[t], t=1:num_offensive_players} <= 3)
    end

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB-Top Target Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum == 10 there must be 
    # at least a QB/Reciever Pair
    @variable(m, targets_stack[i=1:num_pairs_targets], Bin)
    @constraint(m, constr[i=1:num_pairs_targets], 11*targets_stack[i] == sum{team_pairs_targets[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{targets_stack[i], i=1:num_pairs_targets} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)


    # Objective
    # @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn_perturbed")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn_perturbed]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end


# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-Top Reciever Target Stack (If you have a QB then also include a WR from the same team)
# - Can not have more than 3 players from the same team
# - no TE for flex
# - at least 3 receivers must be in the top 3 Target Leaders for their team (If Flex is WR, does not have to follow this rule)
function one_lineup_Type_13(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $50,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    #Add Top WR Constraint
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i]*topWideReciever[i], i=1:num_offensive_players})

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # Can't have 3 offensive players from the same team
    for team in 1:num_teams
        @constraint(m, sum{offensive_players_teams[t, team]*offensive_players_lineup[t], t=1:num_offensive_players} <= 2)
    end

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB-Top Target Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum == 10 there must be 
    # at least a QB/Reciever Pair
    @variable(m, targets_stack[i=1:num_pairs_targets], Bin)
    @constraint(m, constr[i=1:num_pairs_targets], 11*targets_stack[i] == sum{team_pairs_targets[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{targets_stack[i], i=1:num_pairs_targets} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)

    # Objective
    # @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn_perturbed")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn_perturbed]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
# - no TE for flex
# - Same as Formulation 4 with Position Exposure Constraints
function one_lineup_Type_14(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum(defenses_lineup[i] for i=1:num_defenses) == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum(quarterBack[i]*offensive_players_lineup[i] for i=1:num_offensive_players) +
                   sum(runningBack[i]*offensive_players_lineup[i] for i=1:num_offensive_players) + 
                   sum(wideReciever[i]*offensive_players_lineup[i] for i=1:num_offensive_players) +
                   sum(tightEnd[i]*offensive_players_lineup[i] for i=1:num_offensive_players) == 8)

    # One QB constraint
    @constraint(m, sum(quarterBack[i]*offensive_players_lineup[i] for i=1:num_offensive_players) == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum(runningBack[i]*offensive_players_lineup[i] for i=1:num_offensive_players))
    @constraint(m, sum(runningBack[i]*offensive_players_lineup[i] for i=1:num_offensive_players) <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum(wideReciever[i]*offensive_players_lineup[i] for i=1:num_offensive_players))
    @constraint(m, sum(wideReciever[i]*offensive_players_lineup[i] for i=1:num_offensive_players) <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum(tightEnd[i]*offensive_players_lineup[i] for i=1:num_offensive_players))
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum(offensive_players[i,:Salary]*offensive_players_lineup[i] for i=1:num_offensive_players) + sum(defenses[i,:Salary]*defenses_lineup[i] for i=1:num_defenses) <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum(offensive_players_teams[t, i]*offensive_players_lineup[t] for t=1:num_offensive_players))
    @constraint(m, sum(used_team[i] for i=1:num_teams) >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum(defenses_opponents[k, i]*offensive_players_lineup[k] for k=1:num_offensive_players)<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @variable(m, QBWR_stack[i=1:num_pairs], Bin)
    @constraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum(team_pairs[k,i]*offensive_players_lineup[k] for k=1:num_offensive_players))
    @constraint(m, sum(QBWR_stack[i] for i=1:num_pairs) >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum(lineups[j,i]*offensive_players_lineup[j] for j=1:num_offensive_players) + sum(lineups[num_offensive_players+j,i]*defenses_lineup[j] for j=1:num_defenses) <= num_overlap)

    # Can't have 3 offensive players from the same team
    for team in 1:num_teams
        @constraint(m, sum{offensive_players_teams[t, team]*offensive_players_lineup[t], t=1:num_offensive_players} <= 2)
    end

    # Exposure Constraint
    #QB 
    # @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j]*quarterBack[j] <= num_lineups * exposure_qb)
    # #RB
    # @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j]*runningBack[j] <= num_lineups * exposure_rb)
    # #WR 
    # @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j]*wideReciever[j] <= num_lineups * exposure_wr)
    # #TE 
    # @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j]*tightEnd[j] <= num_lineups * exposure_te)
    for i in 1:num_offensive_players
        if (quarterBack[i] == 1)
            @constraint(m, sum(lineups[i,j] for j=1:size(lineups)[2]) + offensive_players_lineup[i] <= num_lineups * exposure_qb)
        elseif (runningBack[i] == 1)
            @constraint(m, sum(lineups[i,j] for j=1:size(lineups)[2]) + offensive_players_lineup[i] <= num_lineups * exposure_rb)
        elseif (wideReciever[i] == 1)
            @constraint(m, sum(lineups[i,j] for j=1:size(lineups)[2]) + offensive_players_lineup[i] <= num_lineups * exposure_wr)
        elseif (tightEnd[i] == 1)
            @constraint(m, sum(lineups[i,j] for j=1:size(lineups)[2]) + offensive_players_lineup[i] <= num_lineups * exposure_te)
        end
    end

    # Defense
    @constraint(m, constr[j=1:num_defenses], sum(lineups[num_offensive_players + j,i] for i=1:size(lineups)[2]) + defenses_lineup[j] <= num_lineups * exposure_defense)
    # Objective
    # @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    if (projections_source == "Projection")
        @objective(m, Max, sum(offensive_players[i,:Projection]*offensive_players_lineup[i] for i=1:num_offensive_players) + sum(defenses[i,:Projection]*defenses_lineup[i] for i=1:num_defenses))
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum(offensive_players[i,:Projection_dfn]*offensive_players_lineup[i] for i=1:num_offensive_players) + sum(defenses[i,:Projection_dfn]*defenses_lineup[i] for i=1:num_defenses))
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum(offensive_players[i,:Projection_fc]*offensive_players_lineup[i] for i=1:num_offensive_players) + sum(defenses[i,:Projection_fc]*defenses_lineup[i] for i=1:num_defenses))
    elseif (projections_source == "Projection_dfn_perturbed")
        @objective(m, Max, sum(offensive_players[i,:Projection_dfn_perturbed]*offensive_players_lineup[i] for i=1:num_offensive_players) + sum(defenses[i,:Projection_dfn]*defenses_lineup[i] for i=1:num_defenses))
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum(offensive_players[i,:Projection_reg]*offensive_players_lineup[i] for i=1:num_offensive_players) + sum(defenses[i,:Projection_dfn]*defenses_lineup[i] for i=1:num_defenses))
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum(offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i] for i=1:num_offensive_players) + sum(defenses[i,:Projection_dfn]*defenses_lineup[i] for i=1:num_defenses))
    elseif (projections_source == "Actual")
        @objective(m, Max, sum(offensive_players[i,:Actual]*offensive_players_lineup[i] for i=1:num_offensive_players) + sum(defenses[i,:Actual]*defenses_lineup[i] for i=1:num_defenses))
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Same as Formulation 14 with two additional constraints:
#   - exposure_valuewr constraint: must have one value wr
#   - DST Salary >= 2700 and <= 3100
function one_lineup_Type_15(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Defense Salary Constraint
    @constraint(m, sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} >= 2700)
    @constraint(m, sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 3100)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # must have 1 ValueWR
    @constraint(m, 1 == sum{valueWideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @variable(m, QBWR_stack[i=1:num_pairs], Bin)
    @constraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    #QB 
    # @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j]*quarterBack[j] <= num_lineups * exposure_qb)
    # #RB
    # @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j]*runningBack[j] <= num_lineups * exposure_rb)
    # #WR 
    # @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j]*wideReciever[j] <= num_lineups * exposure_wr)
    # #TE 
    # @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j]*tightEnd[j] <= num_lineups * exposure_te)
    for i in 1:num_offensive_players
        if (quarterBack[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_qb)
        elseif (runningBack[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_rb)
        elseif (wideReciever[i] == 1)
            if (valueWideReciever[i] == 1) 
                @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_valueWideReciever)
            else 
                @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_wr)
            end 
        elseif (tightEnd[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_te)
        end
    end

    # Defense
    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure_defense)
    # Objective
    # @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn_perturbed")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn_perturbed]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-Top Reciever Target Stack (If you have a QB then also include a WR from the same team)
# - Can not have more than 3 players from the same team
# - no TE for flex
# - at least 3 receivers must be in the top 3 Target Leaders for their team (If Flex is WR, does not have to follow this rule)
function one_lineup_Type_16(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $50,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Defense Salary Constraint
    @constraint(m, sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} >= 2700)
    @constraint(m, sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 3100)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # must have 1 ValueWR
    @constraint(m, 1 == sum{valueWideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})

    #Add Top WR Constraint
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i]*topWideReciever[i], i=1:num_offensive_players})

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # Can't have 3 offensive players from the same team
    for team in 1:num_teams
        @constraint(m, sum{offensive_players_teams[t, team]*offensive_players_lineup[t], t=1:num_offensive_players} <= 2)
    end

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB-Top Target Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum == 10 there must be 
    # at least a QB/Reciever Pair
    @variable(m, targets_stack[i=1:num_pairs_targets], Bin)
    @constraint(m, constr[i=1:num_pairs_targets], 11*targets_stack[i] == sum{team_pairs_targets[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{targets_stack[i], i=1:num_pairs_targets} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    for i in 1:num_offensive_players
        if (quarterBack[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_qb)
        elseif (runningBack[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_rb)
        elseif (wideReciever[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_wr)
        elseif (tightEnd[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_te)
        end
    end

    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure)

    # Objective
    # @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn_perturbed")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn_perturbed]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
# - no TE for flex
# - Same as Formulation 4 with Position Exposure Constraints
# - DST Salary >= 2700 and <= 3100
function one_lineup_Type_17(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Defense Salary Constraint
    @constraint(m, sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} >= 2700)
    @constraint(m, sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 3100)
    
    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses-2], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @variable(m, QBWR_stack[i=1:num_pairs], Bin)
    @constraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    #QB 
    # @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j]*quarterBack[j] <= num_lineups * exposure_qb)
    # #RB
    # @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j]*runningBack[j] <= num_lineups * exposure_rb)
    # #WR 
    # @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j]*wideReciever[j] <= num_lineups * exposure_wr)
    # #TE 
    # @constraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j]*tightEnd[j] <= num_lineups * exposure_te)
    for i in 1:num_offensive_players
        if (quarterBack[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_qb)
        elseif (runningBack[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_rb)
        elseif (wideReciever[i] == 1)
            if (valueWideReciever[i] == 1) 
                @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_valueWideReciever)
            else 
                @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_wr)
            end 
        elseif (tightEnd[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_te)
        end
    end

    # Defense
    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure_defense)
    # Objective
    # @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn_perturbed")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn_perturbed]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Same as Formulation 14 with two additional constraints:
#   - exposure_valuewr constraint: must have one value wr
#   - DST Salary >= 2700 and <= 3100
#   - Only allow 1 WR with salary < 5000

function one_lineup_Type_18(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Defense Salary Constraint
    @constraint(m, sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} >= 2700)
    @constraint(m, sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 3100)

    # Eight Offensive_Players constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} + 
                   sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} +
                   sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @constraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # must have 1 ValueWR
    @constraint(m, 1 == sum{valueWideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})

    # must have 1 CheapWR
    @constraint(m, 1 == sum{cheapWideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @variable(m, QBWR_stack[i=1:num_pairs], Bin)
    @constraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    for i in 1:num_offensive_players
        if (quarterBack[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_qb)
        elseif (runningBack[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_rb)
        elseif (wideReciever[i] == 1)
            if (valueWideReciever[i] == 1) 
                @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_valueWideReciever)
            else 
                @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_wr)
            end 
        elseif (tightEnd[i] == 1)
            @constraint(m, sum{lineups[i,j], j=1:size(lineups)[2]} + offensive_players_lineup[i] <= num_lineups * exposure_te)
        end
    end

    # Defense
    @constraint(m, constr[j=1:num_defenses], sum{lineups[num_offensive_players + j,i], i=1:size(lineups)[2]} + defenses_lineup[j] <= num_lineups * exposure_defense)
    # Objective
    # @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    if (projections_source == "Projection")
        @objective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_fc")
        @objective(m, Max, sum{offensive_players[i,:Projection_fc]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_fc]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_dfn_perturbed")
        @objective(m, Max, sum{offensive_players[i,:Projection_dfn_perturbed]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Projection_reg_split")
        @objective(m, Max, sum{offensive_players[i,:Projection_reg_split]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})
    elseif (projections_source == "Actual")
        @objective(m, Max, sum{offensive_players[i,:Actual]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Actual]*defenses_lineup[i], i=1:num_defenses})
    end

    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end
############################  Setting Formation  ############################

function create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, path_to_output, projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valueWideReciever)
    #=
    num_lineups is an integer that is the number of lineups (Line 28)
    num_overlap is an integer that gives the overlap between each lineup (Line 31)
    path_offensive_players is a string that gives the path to the Offensive_Players csv file (Line 34)
    path_defenses is a string that gives the path to the Defenses csv file (Line 37)
    formulation is the type of formulation you would like to use (Line 48)
    path_to_output is a string where the final csv file with your lineups will be (Line 40)
    =#

    # Load information for offensive_players table
    offensive_players = readtable(path_offensive_players)

    # Load information for defenses table
    defenses = readtable(path_defenses)

    # Number of offensive_players
    num_offensive_players = size(offensive_players)[1]

    # Number of defenses
    num_defenses = size(defenses)[1]

    # quarterBack stores the information on which players are quarterBack
    quarterBack = Array(Int64, 0)

    # runningBack stores the information on which players are runningBack
    runningBack = Array(Int64, 0)

    # cheapRunningBack stores the information on which players are runningBack that is less than 5000
    cheapRunningBack = Array(Int64, 0)

    # cheapWideReciever stores the information on which players are WideReciever that is less than 5000
    cheapWideReciever = Array(Int64, 0)

    # wideReciever stores the information on which players are wideReciever
    wideReciever = Array(Int64, 0)
    topWideReciever = Array(Int64, 0)
    valueWideReciever = Array(Int64, 0)
    # tightEnd stores the information on which players are tightEnd
    tightEnd = Array(Int64, 0)

    if(use_Freq_Ind)
        for i =1:num_offensive_players
            if offensive_players[i,:Position] == "QB" 
                quarterBack=vcat(quarterBack,fill(1,1))
                runningBack=vcat(runningBack,fill(0,1))
                wideReciever=vcat(wideReciever,fill(0,1))
                tightEnd=vcat(tightEnd,fill(0,1))
                cheapRunningBack=vcat(cheapRunningBack,fill(0,1))
                topWideReciever=vcat(topWideReciever,fill(0,1))
                cheapWideReciever=vcat(cheapWideReciever,fill(0,1))
            elseif (offensive_players[i,:Position] == "RB" && offensive_players[i,:FreqInd] == 1)
                quarterBack=vcat(quarterBack,fill(0,1))
                runningBack=vcat(runningBack,fill(1,1))
                wideReciever=vcat(wideReciever,fill(0,1))
                tightEnd=vcat(tightEnd,fill(0,1))
                topWideReciever=vcat(topWideReciever,fill(0,1))
                cheapWideReciever=vcat(cheapWideReciever,fill(0,1))
                if offensive_players[i,:Salary] < 5000
                    cheapRunningBack=vcat(cheapRunningBack,fill(1,1))
                else 
                    cheapRunningBack=vcat(cheapRunningBack,fill(0,1))
                end
            elseif (offensive_players[i,:Position] == "WR" && offensive_players[i,:FreqInd] == 1)
                quarterBack=vcat(quarterBack,fill(0,1))
                runningBack=vcat(runningBack,fill(0,1))
                wideReciever=vcat(wideReciever,fill(1,1))
                tightEnd=vcat(tightEnd,fill(0,1))
                cheapRunningBack=vcat(cheapRunningBack,fill(0,1))
                if offensive_players[i,:RankTargets] < 4
                    topWideReciever=vcat(topWideReciever,fill(1,1))
                else
                    topWideReciever=vcat(topWideReciever,fill(0,1))
                end
                if offensive_players[i,:Salary] < 5000
                    cheapWideReciever=vcat(cheapWideReciever,fill(1,1))
                else
                    cheapWideReciever=vcat(cheapWideReciever,fill(0,1))
                end
            elseif offensive_players[i,:Position] == "TE" && offensive_players[i,:FreqInd] == 1
                quarterBack=vcat(quarterBack,fill(0,1))
                runningBack=vcat(runningBack,fill(0,1))
                wideReciever=vcat(wideReciever,fill(0,1))
                tightEnd=vcat(tightEnd,fill(1,1))
                cheapRunningBack=vcat(cheapRunningBack,fill(0,1))
                topWideReciever=vcat(topWideReciever,fill(0,1))
                cheapWideReciever=vcat(cheapWideReciever,fill(0,1))
            else
                quarterBack=vcat(quarterBack,fill(0,1))
                runningBack=vcat(runningBack,fill(0,1))
                wideReciever=vcat(wideReciever,fill(0,1))
                tightEnd=vcat(tightEnd,fill(0,1))
                cheapRunningBack=vcat(cheapRunningBack,fill(0,1))
                topWideReciever=vcat(topWideReciever,fill(0,1))
                cheapWideReciever=vcat(cheapWideReciever,fill(0,1))
            end
        end
    else
        for i =1:num_offensive_players
            if offensive_players[i,:Position] == "QB" 
                quarterBack=vcat(quarterBack,fill(1,1))
                runningBack=vcat(runningBack,fill(0,1))
                wideReciever=vcat(wideReciever,fill(0,1))
                tightEnd=vcat(tightEnd,fill(0,1))
                cheapRunningBack=vcat(cheapRunningBack,fill(0,1))
                topWideReciever=vcat(topWideReciever,fill(0,1))
                valueWideReciever=vcat(valueWideReciever,fill(0,1))
            elseif offensive_players[i,:Position] == "RB"
                quarterBack=vcat(quarterBack,fill(0,1))
                runningBack=vcat(runningBack,fill(1,1))
                wideReciever=vcat(wideReciever,fill(0,1))
                tightEnd=vcat(tightEnd,fill(0,1))
                topWideReciever=vcat(topWideReciever,fill(0,1))
                valueWideReciever=vcat(valueWideReciever,fill(0,1))
                if offensive_players[i,:Salary] < 5000
                    cheapRunningBack=vcat(cheapRunningBack,fill(1,1))
                else 
                    cheapRunningBack=vcat(cheapRunningBack,fill(0,1))
                end
            elseif offensive_players[i,:Position] == "WR"
                quarterBack=vcat(quarterBack,fill(0,1))
                runningBack=vcat(runningBack,fill(0,1))
                wideReciever=vcat(wideReciever,fill(1,1))
                tightEnd=vcat(tightEnd,fill(0,1))
                cheapRunningBack=vcat(cheapRunningBack,fill(0,1))
                if offensive_players[i,:RankTargets] < 4
                    topWideReciever=vcat(topWideReciever,fill(1,1))
                else 
                    topWideReciever=vcat(topWideReciever,fill(0,1))
                end
                if offensive_players[i,:ValueWR] == 1
                    valueWideReciever=vcat(valueWideReciever,fill(1,1))
                else 
                    valueWideReciever=vcat(valueWideReciever,fill(0,1))
                end
            else
                quarterBack=vcat(quarterBack,fill(0,1))
                runningBack=vcat(runningBack,fill(0,1))
                wideReciever=vcat(wideReciever,fill(0,1))
                tightEnd=vcat(tightEnd,fill(1,1))
                cheapRunningBack=vcat(cheapRunningBack,fill(0,1))
                topWideReciever=vcat(topWideReciever,fill(0,1))
                valueWideReciever=vcat(valueWideReciever,fill(0,1))
            end
        end
    end


    #=
    Process the position information in the skaters file to populate the 
    skill_positions (QB, RB, WR, TE) with the corresponding correct information
    =#
    


    # Create team indicators from the information in the offensive_players file
    teams = unique(offensive_players[:Team])

    # Total number of teams
    num_teams = size(teams)[1]

    # player_info stores information on which team each player is on
    player_info = zeros(Int, size(teams)[1])

    # Populate player_info with the corresponding information
    for j=1:size(teams)[1]
        if offensive_players[1, :Team] == teams[j]
            player_info[j] =1
        end
    end
    offensive_players_teams = player_info'

    for i=2:num_offensive_players
        player_info = zeros(Int, size(teams)[1])
        for j=1:size(teams)[1]
            if offensive_players[i, :Team] == teams[j]
                player_info[j] =1
            end
        end
        offensive_players_teams = vcat(offensive_players_teams, player_info')
    end



    # Create defense identifiers so you know who they are playing
    opponents = defenses[:Opponent]
    defenses_teams = defenses[:Team]
    defenses_opponents=[]
    for num = 1:size(teams)[1]
        if opponents[1] == teams[num]
            defenses_opponents = offensive_players_teams[:, num]
        end
    end
    for num = 2:size(opponents)[1]
        for num_2 = 1:size(teams)[1]
            if opponents[num] == teams[num_2]
                defenses_opponents = hcat(defenses_opponents, offensive_players_teams[:,num_2])
            end
        end
    end

     # Create WR/QB Lines to know which QB-WR Pairs are on the same team
    pair_info = zeros(Int, num_offensive_players)
    for num=1:size(offensive_players)[1]
        if offensive_players[:Team][num] == teams[1]
            if offensive_players[:Position][num] == "QB"
                pair_info[num] = 9
            elseif offensive_players[:Position][num] == "WR"
               pair_info[num] = 1
            end
        end
    end
    team_pairs = hcat(pair_info)

    #Weighting so that we can force a QB to exist in the stack of QB/WR's 
    for num2 = 2:size(teams)[1]
        pair_info = zeros(Int, num_offensive_players)
        for num=1:size(offensive_players)[1]
            if offensive_players[:Team][num] == teams[num2]
                if offensive_players[:Position][num] == "QB"
                    pair_info[num] = 9
                elseif offensive_players[:Position][num] == "WR"
                   pair_info[num] = 1
                end
            end
        end
        team_pairs = hcat(team_pairs, pair_info)
    end
    num_pairs = size(team_pairs)[2]

    # Create top Receiving Targets Stack 
    pair_info_targets = zeros(Int, num_offensive_players)
    for num=1:size(offensive_players)[1]
        if offensive_players[:Team][num] == teams[1]
            if offensive_players[:Position][num] == "QB"
                pair_info_targets[num] = 10
            elseif offensive_players[:RankTargets][num] <= 2
                pair_info_targets[num] = 1
            end
        end
    end
    team_pairs_targets = hcat(pair_info_targets)

    #Weighting so that we can force a QB to exist in the stack of QB/WR's 
    for num2 = 2:size(teams)[1]
        pair_info_targets = zeros(Int, num_offensive_players)
        for num=1:size(offensive_players)[1]
            if offensive_players[:Team][num] == teams[num2]
                if offensive_players[:Position][num] == "QB"
                    pair_info_targets[num] = 10
                elseif offensive_players[:RankTargets][num] <= 2
                    pair_info_targets[num] = 1
                end
            end
        end
        team_pairs_targets = hcat(team_pairs_targets, pair_info_targets)
    end
    num_pairs_targets = size(team_pairs_targets)[2]

     # Create RB/WR pairs to make anti-stacks
    pair_info_RBWR = zeros(Int, num_offensive_players)
    for num=1:size(offensive_players)[1]
        if offensive_players[:Team][num] == teams[1]
            if offensive_players[:Position][num] == "RB"
                pair_info_RBWR[num] = 10
            elseif offensive_players[:Position][num] == "WR"
                pair_info_RBWR[num] = 1
            end
        end
    end
    team_pairs_RBWR = hcat(pair_info_RBWR)

    #Weighting so that we can differentiate a RB from a WR
    for num2 = 2:size(teams)[1]
        pair_info_RBWR = zeros(Int, num_offensive_players)
        for num=1:size(offensive_players)[1]
            if offensive_players[:Team][num] == teams[num2]
                if offensive_players[:Position][num] == "RB"
                    pair_info_RBWR[num] = 10
                elseif offensive_players[:Position][num] == "WR"
                    pair_info_RBWR[num] = 1
                end
            end
        end
        team_pairs_RBWR = hcat(team_pairs_RBWR, pair_info_RBWR)
    end
    num_pairs_RBWR = size(team_pairs_RBWR)[2]

    # for QB-oppWR stack
    pair_info_QBoppWR = zeros(Int, num_offensive_players)
    for num=1:size(offensive_players)[1]
        if offensive_players[:Team][num] == teams[1]
            if offensive_players[:Position][num] == "QB"
                pair_info_QBoppWR[num] = 9
            end
        elseif offensive_players[:Opponent][num] == teams[1]
            if offensive_players[:Position][num] == "WR"
                pair_info_QBoppWR[num] = 1
            end
        end
    end
    team_pairs_QBoppWR = hcat(pair_info_QBoppWR)

    #Weighting so that we can force a QB to exist in the stack of QB/WR's
    for num2 = 2:size(teams)[1]
        pair_info_QBoppWR = zeros(Int, num_offensive_players)
        for num=1:size(offensive_players)[1]
            if offensive_players[:Team][num] == teams[num2]
                if offensive_players[:Position][num] == "QB"
                    pair_info_QBoppWR[num] = 9
                end
            elseif offensive_players[:Opponent][num] == teams[num2]
                if offensive_players[:Position][num] == "WR"
                    pair_info_QBoppWR[num] = 1
                end
            end
        end
        team_pairs_QBoppWR = hcat(team_pairs_QBoppWR, pair_info_QBoppWR)
    end
    num_pairs_QBoppWR = size(team_pairs_QBoppWR)[2]

    # for QB-opp Target stack
    pair_info_QBoppTarget = zeros(Int, num_offensive_players)
    for num=1:size(offensive_players)[1]
        if offensive_players[:Team][num] == teams[1]
            if offensive_players[:Position][num] == "QB"
                pair_info_QBoppTarget[num] = 10
            end
        elseif offensive_players[:Opponent][num] == teams[1]
            if offensive_players[:RankTargets][num] <= 2
                pair_info_QBoppTarget[num] = 1
            end
        end
    end
    team_pairs_QBoppTarget = hcat(pair_info_QBoppTarget)

    #Weighting so that we can force a QB to exist in the stack of QB/WR's
    for num2 = 2:size(teams)[1]
        pair_info_QBoppTarget = zeros(Int, num_offensive_players)
        for num=1:size(offensive_players)[1]
            if offensive_players[:Team][num] == teams[num2]
                if offensive_players[:Position][num] == "QB"
                    pair_info_QBoppTarget[num] = 10
                end
            elseif offensive_players[:Opponent][num] == teams[num2]
                if offensive_players[:RankTargets][num] <= 2
                    pair_info_QBoppTarget[num] = 1
            end
            end
        end
        team_pairs_QBoppTarget = hcat(team_pairs_QBoppTarget, pair_info_QBoppTarget)
    end
    num_pairs_QBoppTarget = size(team_pairs_QBoppTarget)[2]

    # Lineups using formulation as the stacking type
    the_lineup= formulation(offensive_players, defenses, hcat(zeros(Int, num_offensive_players + num_defenses), zeros(Int, num_offensive_players + num_defenses)), num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source,team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)

    the_lineup2 = formulation(offensive_players, defenses, hcat(the_lineup, zeros(Int, num_offensive_players + num_defenses)), num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source,team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
    tracer = hcat(the_lineup, the_lineup2)
    for i=1:(num_lineups-2)
        println(string("**************************** LINEUP NUMBER: ", i+2, " ****************************"))
        try
            thelineup=formulation(offensive_players, defenses, tracer, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack, num_lineups, projections_source, team_pairs_RBWR, num_pairs_RBWR, team_pairs_targets, num_pairs_targets, team_pairs_QBoppTarget, num_pairs_QBoppTarget, topWideReciever, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, valueWideReciever, exposure_valueWideReciever, cheapWideReciever)
            tracer = hcat(tracer,thelineup)
        catch
            break
        end
    end

    # FOR REAL FILES WITH PLAYER IDs
    # Create the output csv file FOR DRAFTKINGS INPUT
    # Write File in the following order:
    # Names of the QB, RB1, RB2, WR1, WR2, WR3, TE, FLEX (RB/WR/TE), and DST
    lineup2 = ""
    header = "QB,RB,RB,WR,WR,WR,TE,FLEX,DST"
    header = string(header, """

    """)
    for j = 1:size(tracer)[2]
        lineup = ["" "" "" "" "" "" "" "" ""]
        for i =1:num_offensive_players
            if tracer[i,j] == 1
                if quarterBack[i]==1
                    lineup[1] = string(offensive_players[i,2])
                elseif runningBack[i] == 1
                    if lineup[2] == ""
                        lineup[2] = string(offensive_players[i,2])
                    elseif lineup[3] == ""
                        lineup[3] = string(offensive_players[i,2])
                    elseif lineup[8] == ""
                        lineup[8] = string(offensive_players[i,2])
                    end
                elseif wideReciever[i]==1
                    if lineup[4] == ""
                        lineup[4] = string(offensive_players[i,2])
                    elseif lineup[5] ==""
                        lineup[5] = string(offensive_players[i,2])
                    elseif lineup[6] == ""
                        lineup[6] = string(offensive_players[i,2])
                    elseif lineup[8] == ""
                        lineup[8] = string(offensive_players[i,2])
                    end
                elseif tightEnd[i]==1
                    if lineup[7] == ""
                        lineup[7] = string(offensive_players[i,2])
                    elseif lineup[8] ==""
                        lineup[8] = string(offensive_players[i,2])
                    end
                end
            end
        end
        for i =1:num_defenses
            if tracer[num_offensive_players+i,j] == 1
                lineup[9] = string(defenses[i,2])
            end
        end
        for name in lineup
            lineup2 = string(lineup2, name, ",")
        end
        lineup2 = chop(lineup2)
        lineup2 = string(lineup2, """

        """)
    end
    outfile = open(path_to_output, "w")
    write(outfile, header)
    write(outfile, lineup2)
    close(outfile)
end

end
