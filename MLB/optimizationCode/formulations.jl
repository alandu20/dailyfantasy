module formulations

#This code implements the formulations for  baseball lineups

# To install DataFrames, simply run Pkg.add("DataFrames")
using DataFrames

# To install MathProgBase, simply run Pkg.add("MathProgBase")
using MathProgBase
# Once again, to install run Pkg.add("JuMP")
using JuMP


#=
GLPK is an open-source solver.  For those that want to build
very sophisticated models, they can buy Gurobi. To install GLPKMathProgInterface, simply run
Pkg.add("GLPKMathProgInterface")
=#
using GLPKMathProgInterface

#uncomment this line only if you installed Gurobi, which costs money :(), but is super fast :)
using Gurobi  




#####################################################################################################################
#####################################################################################################################
# This is a function that creates one lineup using the  Stacking Type 3 formulation
# No pitcher opposite batter
# Batters with consecutive batting order
#only keep 4th order and earlier batters, cuz they make more points
function formulation_feasibility(players, old_lineups, num_overlap,stack_size, P,B1,B2,B3,C,SS,OF, players_teams, players_opp, players_games,players_stacks, covar_matrix, num_pitchers, covar_lambda, exposure, num_lineups, exposure_P,exposure_B1,exposure_B2,exposure_B3,exposure_C,exposure_SS,exposure_OF, min_pitcher_exposure)
    

    #################################################################################################
    #define the model formulation which will be solved using GLPK
    #m = Model(solver=GLPKSolverMIP())

    #uncomment this line only if you are using Gurobi, which costs money :(), but is super fast :)
    m = Model(solver=GurobiSolver(OutputFlag=0))
   
   #number of players playing today
    num_players = size(players)[1]
    
    #number of games today
    games = unique(players[:Game])
    num_games = size(games)[1]
    
    #number of teams playing today
    teams = unique(players[:teamAbbrev])
    num_teams = size(teams)[1]

    #number of stacks per team (this is 9)
    num_stacks = 9;  

    # Variable for players in lineup.
    @variable(m, players_lineup[i=1:num_players], Bin)
    
    #OBJECTIVE
    @objective(m, Max, sum(players[i,:Proj_dfn] * players_lineup[i] for i = 1:num_players))


    #NUMBER OF PLAYERS: 10 players constraint
    @constraint(m, sum(players_lineup[i] for i = 1:num_players) == 10)
    
    #SALARY: Financial Constraint - salary less than $50,000
    @constraint(m, sum(players[i,:Salary] * players_lineup[i] for i = 1:num_players) <= 50000)

    #POSITION
    #  2 P constraint
    @constraint(m, sum(P[i] * players_lineup[i] for i = 1:num_players) == 2)
    # one B1 constraint
    @constraint(m, sum(B1[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B2 constraint
    @constraint(m, sum(B2[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B3 constraint
    @constraint(m, sum(B3[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one C constraint
    @constraint(m, sum(C[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one SS constraint
    @constraint(m, sum(SS[i] * players_lineup[i] for i = 1:num_players) == 1)
    # 3 OF constraint
    @constraint(m, sum(OF[i] * players_lineup[i] for i = 1:num_players) == 3)
  
   
  
    #GAMES: at least 2 different games for the 10 players constraints
    #variable for each game used in lineup, equals 1 if game used
    @variable(m, used_game[i=1:num_games], Bin)
    #constraint satisfied by each game
    @constraint(m, constr[i=1:num_games], used_game[i] <= sum(players_games[t,i] * players_lineup[t] for t = 1:num_players))
    #constraint makes sure at least 2 used_game variables equal 1
    @constraint(m, sum(used_game[i] for i = 1:num_games) >= 2)


    #HITTERS at most 5 hitters from one team constraint
    @constraint(m, constr[i=1:num_teams], sum(players_teams[t,i] * (1 - P[t]) * players_lineup[t] for t = 1:num_players) <= 5)
    

    #OVERLAP Constraint
    @constraint(m, constr[i=1:size(old_lineups)[2]], sum(old_lineups[j,i] * players_lineup[j] for j = 1:num_players) <= num_overlap)

 
    #NO PITCHER VS BATTER no pitcher vs batter constraint
    @constraint(m, hitter_pitcher[g=1:num_teams], 
                   8*sum(P[k] * players_lineup[k] * players_teams[k,g] for k = 1:num_players) + 
                    sum((1 - P[k]) * players_lineup[k] * players_opp[k,g] for k = 1:num_players) <= 8)


    #STACK: at least stack_size hitters from at least 1 team, consecutive hitting order
   #define a variable for each stack on each team.  This variable =1 if the stack on the team is used
    @variable(m, used_stack_batters[i=1:num_teams,j=1:num_stacks], Bin)
    
    #constraint for each stack, used or not
    @constraint(m, constraint_stack[i=1:num_teams,j=1:num_stacks], stack_size*used_stack_batters[i,j] <= 
                   sum(players_teams[t,i] * players_stacks[t,j] * (1 - P[t]) * players_lineup[t] for t = 1:num_players))  
    
    #make sure at least one stack is used
    @constraint(m, sum(used_stack_batters[i,j] for i = 1:num_teams for j = 1:num_stacks) >= 1)  
   
   
    # Exposure Constraint
    @constraint(m, constr[j=1:num_players], sum(old_lineups[j,i] for i = 1:(size(old_lineups))[2]) + players_lineup[j] <= num_lineups * exposure)


   ########################################################################################################
    # Solve the integer programming problem  
    tic()
    status = solve(m);
   

    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        players_lineup_copy = Array(Int64, 0)
        for i=1:num_players
            if getvalue(players_lineup[i]) >= 0.9 && getvalue(players_lineup[i]) <= 1.1
                players_lineup_copy = vcat(players_lineup_copy, fill(1,1))
            else
                players_lineup_copy = vcat(players_lineup_copy, fill(0,1))
            end
        end

        return(players_lineup_copy)
    end
end



#####################################################################################################################
#####################################################################################################################
# This is a function that creates one lineup using the  Stacking Type 3 formulation
# No pitcher opposite batter
# Batters with consecutive batting order
#only keep 4th order and earlier batters, cuz they make more points
function formulation0_covar(players, old_lineups, num_overlap,stack_size, P,B1,B2,B3,C,SS,OF, players_teams, players_opp, players_games,players_stacks, covar_matrix, num_pitchers, covar_lambda, exposure, num_lineups, exposure_P,exposure_B1,exposure_B2,exposure_B3,exposure_C,exposure_SS,exposure_OF, min_pitcher_exposure)
    

    #################################################################################################
    #define the model formulation which will be solved using GLPK
    #m = Model(solver=GLPKSolverMIP())

    #uncomment this line only if you are using Gurobi, which costs money :(), but is super fast :)
    m = Model(solver=GurobiSolver(OutputFlag=0))
   
   #number of players playing today
    num_players = size(players)[1]
    
    #number of games today
    games = unique(players[:Game])
    num_games = size(games)[1]
    
    #number of teams playing today
    teams = unique(players[:teamAbbrev])
    num_teams = size(teams)[1]

    #number of stacks per team (this is 9)
    num_stacks = 9;  

    # Variable for players in lineup.
    @variable(m, players_lineup[i=1:num_players], Bin)
    
    #OBJECTIVE
    @objective(m, Max, sum(players[i,:Proj_dfn] * players_lineup[i] for i = 1:num_players) + covar_lambda * sum(covar_matrix[i-num_pitchers,j - num_pitchers] * players_lineup[i] * players_lineup[j] for i = num_pitchers + 1 :num_players for j = num_pitchers + 1:num_players) )


    #NUMBER OF PLAYERS: 10 players constraint
    @constraint(m, sum(players_lineup[i] for i = 1:num_players) == 10)
    
    #SALARY: Financial Constraint - salary less than $50,000
    @constraint(m, sum(players[i,:Salary] * players_lineup[i] for i = 1:num_players) <= 50000)

    #POSITION
    #  2 P constraint
    @constraint(m, sum(P[i] * players_lineup[i] for i = 1:num_players) == 2)
    # one B1 constraint
    @constraint(m, sum(B1[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B2 constraint
    @constraint(m, sum(B2[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B3 constraint
    @constraint(m, sum(B3[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one C constraint
    @constraint(m, sum(C[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one SS constraint
    @constraint(m, sum(SS[i] * players_lineup[i] for i = 1:num_players) == 1)
    # 3 OF constraint
    @constraint(m, sum(OF[i] * players_lineup[i] for i = 1:num_players) == 3)
  
   
  
    #GAMES: at least 2 different games for the 10 players constraints
    #variable for each game used in lineup, equals 1 if game used
    @variable(m, used_game[i=1:num_games], Bin)
    #constraint satisfied by each game
    @constraint(m, constr[i=1:num_games], used_game[i] <= sum(players_games[t,i] * players_lineup[t] for t = 1:num_players))
    #constraint makes sure at least 2 used_game variables equal 1
    @constraint(m, sum(used_game[i] for i = 1:num_games) >= 2)


    #HITTERS at most 5 hitters from one team constraint
    @constraint(m, constr[i=1:num_teams], sum(players_teams[t,i] * (1 - P[t]) * players_lineup[t] for t = 1:num_players) <= 5)
    

    #OVERLAP Constraint
    @constraint(m, constr[i=1:size(old_lineups)[2]], sum(old_lineups[j,i] * players_lineup[j] for j = 1:num_players) <= num_overlap)

 
    #NO PITCHER VS BATTER no pitcher vs batter constraint
    @constraint(m, hitter_pitcher[g=1:num_teams], 
                   8*sum(P[k] * players_lineup[k] * players_teams[k,g] for k = 1:num_players) + 
                    sum((1 - P[k]) * players_lineup[k] * players_opp[k,g] for k = 1:num_players) <= 8)


    #STACK: at least stack_size hitters from at least 1 team, consecutive hitting order
   #define a variable for each stack on each team.  This variable =1 if the stack on the team is used
    @variable(m, used_stack_batters[i=1:num_teams,j=1:num_stacks], Bin)
    
    #constraint for each stack, used or not
    @constraint(m, constraint_stack[i=1:num_teams,j=1:num_stacks], stack_size*used_stack_batters[i,j] <= 
                   sum(players_teams[t,i] * players_stacks[t,j] * (1 - P[t]) * players_lineup[t] for t = 1:num_players))  
    
    #make sure at least one stack is used
    @constraint(m, sum(used_stack_batters[i,j] for i = 1:num_teams for j = 1:num_stacks) >= 1)  
   
   


   ########################################################################################################
    # Solve the integer programming problem  
    tic()
    status = solve(m);
   

    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        players_lineup_copy = Array(Int64, 0)
        for i=1:num_players
            if getvalue(players_lineup[i]) >= 0.9 && getvalue(players_lineup[i]) <= 1.1
                players_lineup_copy = vcat(players_lineup_copy, fill(1,1))
            else
                players_lineup_copy = vcat(players_lineup_copy, fill(0,1))
            end
        end

        return(players_lineup_copy)
    end
end


#####################################################################################################################
#####################################################################################################################
# This is a function that creates one lineup using the  Stacking Type 3 formulation
# No pitcher opposite batter
# Batters with consecutive batting order
#only keep 4th order and earlier batters, cuz they make more points
function formulation1_covar(players, old_lineups, num_overlap,stack_size, P,B1,B2,B3,C,SS,OF, players_teams, players_opp, players_games,players_stacks, covar_matrix, num_pitchers, covar_lambda, exposure, num_lineups, exposure_P,exposure_B1,exposure_B2,exposure_B3,exposure_C,exposure_SS,exposure_OF, min_pitcher_exposure)
    

    #################################################################################################
    #define the model formulation which will be solved using GLPK
    #m = Model(solver=GLPKSolverMIP())

    #uncomment this line only if you are using Gurobi, which costs money :(), but is super fast :)
    m = Model(solver=GurobiSolver(OutputFlag=0))
   
   #number of players playing today
    num_players = size(players)[1]
    
    #number of games today
    games = unique(players[:Game])
    num_games = size(games)[1]
    
    #number of teams playing today
    teams = unique(players[:teamAbbrev])
    num_teams = size(teams)[1]

    #number of stacks per team (this is 9)
    num_stacks = 9;  

    # Variable for players in lineup.
    @variable(m, players_lineup[i=1:num_players], Bin)
    
    #OBJECTIVE
    @objective(m, Max, sum(players[i,:Proj_dfn] * players_lineup[i] for i = 1:num_players) + covar_lambda * sum(covar_matrix[i-num_pitchers,j - num_pitchers] * players_lineup[i] * players_lineup[j] for i = num_pitchers + 1 :num_players for j = num_pitchers + 1:num_players) )


    #NUMBER OF PLAYERS: 10 players constraint
    @constraint(m, sum(players_lineup[i] for i = 1:num_players) == 10)
    
    #SALARY: Financial Constraint - salary less than $50,000
    @constraint(m, sum(players[i,:Salary] * players_lineup[i] for i = 1:num_players) <= 50000)

    #POSITION
    #  2 P constraint
    @constraint(m, sum(P[i] * players_lineup[i] for i = 1:num_players) == 2)
    # one B1 constraint
    @constraint(m, sum(B1[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B2 constraint
    @constraint(m, sum(B2[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B3 constraint
    @constraint(m, sum(B3[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one C constraint
    @constraint(m, sum(C[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one SS constraint
    @constraint(m, sum(SS[i] * players_lineup[i] for i = 1:num_players) == 1)
    # 3 OF constraint
    @constraint(m, sum(OF[i] * players_lineup[i] for i = 1:num_players) == 3)
  
   
  
    #GAMES: at least 2 different games for the 10 players constraints
    #variable for each game used in lineup, equals 1 if game used
    @variable(m, used_game[i=1:num_games], Bin)
    #constraint satisfied by each game
    @constraint(m, constr[i=1:num_games], used_game[i] <= sum(players_games[t,i] * players_lineup[t] for t = 1:num_players))
    #constraint makes sure at least 2 used_game variables equal 1
    @constraint(m, sum(used_game[i] for i = 1:num_games) >= 2)


    #HITTERS at most 5 hitters from one team constraint
    @constraint(m, constr[i=1:num_teams], sum(players_teams[t,i] * (1 - P[t]) * players_lineup[t] for t = 1:num_players) <= 5)
    

    #OVERLAP Constraint
    @constraint(m, constr[i=1:size(old_lineups)[2]], sum(old_lineups[j,i] * players_lineup[j] for j = 1:num_players) <= num_overlap)

 
    #NO PITCHER VS BATTER no pitcher vs batter constraint
    @constraint(m, hitter_pitcher[g=1:num_teams], 
                   8*sum(P[k] * players_lineup[k] * players_teams[k,g] for k = 1:num_players) + 
                    sum((1 - P[k]) * players_lineup[k] * players_opp[k,g] for k = 1:num_players) <= 8)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_players], sum(old_lineups[j,i] for i = 1:(size(old_lineups))[2]) + players_lineup[j] <= num_lineups * exposure)
   


   ########################################################################################################
    # Solve the integer programming problem  
    tic()
    status = solve(m);
   

    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        players_lineup_copy = Array(Int64, 0)
        for i=1:num_players
            if getvalue(players_lineup[i]) >= 0.9 && getvalue(players_lineup[i]) <= 1.1
                players_lineup_copy = vcat(players_lineup_copy, fill(1,1))
            else
                players_lineup_copy = vcat(players_lineup_copy, fill(0,1))
            end
        end

        return(players_lineup_copy)
    end
end


#####################################################################################################################
#####################################################################################################################
# This is a function that creates one lineup using the  Stacking Type 3 formulation
    #- Optimization is max(projections + (lambda)*covariance)
    #- General Team Stacking 
    #- Exposure Constraints 
function formulation2_covar(players, old_lineups, num_overlap,stack_size, P,B1,B2,B3,C,SS,OF, players_teams, players_opp, players_games,players_stacks, covar_matrix, num_pitchers, covar_lambda, exposure, num_lineups, exposure_P,exposure_B1,exposure_B2,exposure_B3,exposure_C,exposure_SS,exposure_OF, min_pitcher_exposure)
    

    #################################################################################################
    #define the model formulation which will be solved using GLPK
    #m = Model(solver=GLPKSolverMIP())

    #uncomment this line only if you are using Gurobi, which costs money :(), but is super fast :)
    m = Model(solver=GurobiSolver(OutputFlag=0))
   
   #number of players playing today
    num_players = size(players)[1]
    
    #number of games today
    games = unique(players[:Game])
    num_games = size(games)[1]
    
    #number of teams playing today
    teams = unique(players[:teamAbbrev])
    num_teams = size(teams)[1]

    #number of stacks per team (this is 9)
    num_stacks = 9;  

    # Variable for players in lineup.
    @variable(m, players_lineup[i=1:num_players], Bin)
    
    #OBJECTIVE
    @objective(m, Max, sum(players[i,:Proj_dfn] * players_lineup[i] for i = 1:num_players) + covar_lambda * sum(covar_matrix[i-num_pitchers,j - num_pitchers] * players_lineup[i] * players_lineup[j] for i = num_pitchers + 1 :num_players for j = num_pitchers + 1:num_players) )


    #NUMBER OF PLAYERS: 10 players constraint
    @constraint(m, sum(players_lineup[i] for i = 1:num_players) == 10)
    
    #SALARY: Financial Constraint - salary less than $50,000
    @constraint(m, sum(players[i,:Salary] * players_lineup[i] for i = 1:num_players) <= 50000)

    #POSITION
    #  2 P constraint
    @constraint(m, sum(P[i] * players_lineup[i] for i = 1:num_players) == 2)
    # one B1 constraint
    @constraint(m, sum(B1[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B2 constraint
    @constraint(m, sum(B2[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B3 constraint
    @constraint(m, sum(B3[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one C constraint
    @constraint(m, sum(C[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one SS constraint
    @constraint(m, sum(SS[i] * players_lineup[i] for i = 1:num_players) == 1)
    # 3 OF constraint
    @constraint(m, sum(OF[i] * players_lineup[i] for i = 1:num_players) == 3)
  
   
  
    #GAMES: at least 2 different games for the 10 players constraints
    #variable for each game used in lineup, equals 1 if game used
    @variable(m, used_game[i=1:num_games], Bin)
    #constraint satisfied by each game
    @constraint(m, constr[i=1:num_games], used_game[i] <= sum(players_games[t,i] * players_lineup[t] for t = 1:num_players))
    #constraint makes sure at least 2 used_game variables equal 1
    @constraint(m, sum(used_game[i] for i = 1:num_games) >= 2)


    #HITTERS at most 5 hitters from one team constraint
    @constraint(m, constr[i=1:num_teams], sum(players_teams[t,i] * (1 - P[t]) * players_lineup[t] for t = 1:num_players) <= 5)
    

    #OVERLAP Constraint
    @constraint(m, constr[i=1:size(old_lineups)[2]], sum(old_lineups[j,i] * players_lineup[j] for j = 1:num_players) <= num_overlap)

 
    #NO PITCHER VS BATTER no pitcher vs batter constraint
    @constraint(m, hitter_pitcher[g=1:num_teams], 
                   8*sum(P[k] * players_lineup[k] * players_teams[k,g] for k = 1:num_players) + 
                    sum((1 - P[k]) * players_lineup[k] * players_opp[k,g] for k = 1:num_players) <= 8)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_players], sum(old_lineups[j,i] for i = 1:(size(old_lineups))[2]) + players_lineup[j] <= num_lineups * exposure)
   
   
    #STACK: [Stack_Size] number of players from a single team must be used
    @variable(m, used_stack_team[i=1:num_teams], Bin)
    
    #constraint for each stack, used or not
    @constraint(m, constraint_stack[i=1:num_teams], stack_size*used_stack_team[i] <= 
                   sum(players_teams[t,i] * players_lineup[t] for t = num_pitchers + 1:num_players))  
    
    #make sure at least one stack is used
    @constraint(m, sum(used_stack_team[i] for i = 1:num_teams) >= 1)  


   ########################################################################################################
    # Solve the integer programming problem  
    tic()
    status = solve(m);
   

    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        players_lineup_copy = Array(Int64, 0)
        for i=1:num_players
            if getvalue(players_lineup[i]) >= 0.9 && getvalue(players_lineup[i]) <= 1.1
                players_lineup_copy = vcat(players_lineup_copy, fill(1,1))
            else
                players_lineup_copy = vcat(players_lineup_copy, fill(0,1))
            end
        end

        return(players_lineup_copy)
    end
end


#####################################################################################################################
#####################################################################################################################
# This is a function that creates one lineup using the  Stacking Type 3 formulation
# No pitcher opposite batter
# Batters with consecutive batting order
function formulation3_covar(players, old_lineups, num_overlap,stack_size, P,B1,B2,B3,C,SS,OF, players_teams, players_opp, players_games,players_stacks, covar_matrix, num_pitchers, covar_lambda, exposure, num_lineups, exposure_P,exposure_B1,exposure_B2,exposure_B3,exposure_C,exposure_SS,exposure_OF, min_pitcher_exposure)
    

    #################################################################################################
    #define the model formulation which will be solved using GLPK
    #m = Model(solver=GLPKSolverMIP())

    #uncomment this line only if you are using Gurobi, which costs money :(), but is super fast :)
    m = Model(solver=GurobiSolver(OutputFlag=0))
   
   #number of players playing today
    num_players = size(players)[1]
    
    #number of games today
    games = unique(players[:Game])
    num_games = size(games)[1]
    
    #number of teams playing today
    teams = unique(players[:teamAbbrev])
    num_teams = size(teams)[1]

    #number of stacks per team (this is 9)
    num_stacks = 9;  

    # Variable for players in lineup.
    @variable(m, players_lineup[i=1:num_players], Bin)
    
    #OBJECTIVE
    @objective(m, Max, sum(players[i,:Proj_dfn] * players_lineup[i] for i = 1:num_players) + covar_lambda * sum(covar_matrix[i-num_pitchers,j - num_pitchers] * players_lineup[i] * players_lineup[j] for i = num_pitchers + 1 :num_players for j = num_pitchers + 1:num_players) )


    #NUMBER OF PLAYERS: 10 players constraint
    @constraint(m, sum(players_lineup[i] for i = 1:num_players) == 10)
    
    #SALARY: Financial Constraint - salary less than $50,000
    @constraint(m, sum(players[i,:Salary] * players_lineup[i] for i = 1:num_players) <= 50000)

    #POSITION
    #  2 P constraint
    @constraint(m, sum(P[i] * players_lineup[i] for i = 1:num_players) == 2)
    # one B1 constraint
    @constraint(m, sum(B1[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B2 constraint
    @constraint(m, sum(B2[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B3 constraint
    @constraint(m, sum(B3[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one C constraint
    @constraint(m, sum(C[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one SS constraint
    @constraint(m, sum(SS[i] * players_lineup[i] for i = 1:num_players) == 1)
    # 3 OF constraint
    @constraint(m, sum(OF[i] * players_lineup[i] for i = 1:num_players) == 3)
  
   
  
    #GAMES: at least 2 different games for the 10 players constraints
    #variable for each game used in lineup, equals 1 if game used
    @variable(m, used_game[i=1:num_games], Bin)
    #constraint satisfied by each game
    @constraint(m, constr[i=1:num_games], used_game[i] <= sum(players_games[t,i] * players_lineup[t] for t = 1:num_players))
    #constraint makes sure at least 2 used_game variables equal 1
    @constraint(m, sum(used_game[i] for i = 1:num_games) >= 2)


    #HITTERS at most 5 hitters from one team constraint
    @constraint(m, constr[i=1:num_teams], sum(players_teams[t,i] * (1 - P[t]) * players_lineup[t] for t = 1:num_players) <= 5)
    

    #OVERLAP Constraint
    @constraint(m, constr[i=1:size(old_lineups)[2]], sum(old_lineups[j,i] * players_lineup[j] for j = 1:num_players) <= num_overlap)

 
    #NO PITCHER VS BATTER no pitcher vs batter constraint
    @constraint(m, hitter_pitcher[g=1:num_teams], 
                   8*sum(P[k] * players_lineup[k] * players_teams[k,g] for k = 1:num_players) + 
                    sum((1 - P[k]) * players_lineup[k] * players_opp[k,g] for k = 1:num_players) <= 8)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_players], sum(old_lineups[j,i] for i = 1:(size(old_lineups))[2]) + players_lineup[j] <= num_lineups * exposure)
   
   
    #STACK: at least stack_size hitters from at least 1 team, consecutive hitting order
   #define a variable for each stack on each team.  This variable =1 if the stack on the team is used
    @variable(m, used_stack_batters[i=1:num_teams,j=1:num_stacks], Bin)
    
    #constraint for each stack, used or not
    @constraint(m, constraint_stack[i=1:num_teams,j=1:num_stacks], stack_size*used_stack_batters[i,j] <= 
                   sum(players_teams[t,i] * players_stacks[t,j] * players_lineup[t] for t = num_pitchers + 1:num_players))  
    
    #make sure at least one stack is used
    @constraint(m, sum(used_stack_batters[i,j] for i = 1:num_teams for j = 1:num_stacks) >= 1)  


   ########################################################################################################
    # Solve the integer programming problem  
    tic()
    status = solve(m);
   

    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        players_lineup_copy = Array(Int64, 0)
        for i=1:num_players
            if getvalue(players_lineup[i]) >= 0.9 && getvalue(players_lineup[i]) <= 1.1
                players_lineup_copy = vcat(players_lineup_copy, fill(1,1))
            else
                players_lineup_copy = vcat(players_lineup_copy, fill(0,1))
            end
        end

        return(players_lineup_copy)
    end
end

#####################################################################################################################
#####################################################################################################################
# This is a function that creates one lineup using the  Stacking Type 3 formulation
    #- Optimization is max(projections + (lambda)*covariance)
    #- General Team Stacking 
        #- Team + Pitcher Stacking
    #- Exposure Constraints 
function formulation4_covar(players, old_lineups, num_overlap,stack_size, P,B1,B2,B3,C,SS,OF, players_teams, players_opp, players_games,players_stacks, covar_matrix, num_pitchers, covar_lambda, exposure, num_lineups, exposure_P,exposure_B1,exposure_B2,exposure_B3,exposure_C,exposure_SS,exposure_OF, min_pitcher_exposure)
    

    #################################################################################################
    #define the model formulation which will be solved using GLPK
    #m = Model(solver=GLPKSolverMIP())

    #uncomment this line only if you are using Gurobi, which costs money :(), but is super fast :)
    m = Model(solver=GurobiSolver(OutputFlag=0))
   
   #number of players playing today
    num_players = size(players)[1]
    
    #number of games today
    games = unique(players[:Game])
    num_games = size(games)[1]
    
    #number of teams playing today
    teams = unique(players[:teamAbbrev])
    num_teams = size(teams)[1]

    #number of stacks per team (this is 9)
    num_stacks = 9;  

    # Variable for players in lineup.
    @variable(m, players_lineup[i=1:num_players], Bin)
    
    #OBJECTIVE
    @objective(m, Max, sum(players[i,:Proj_dfn] * players_lineup[i] for i = 1:num_players) + covar_lambda * sum(covar_matrix[i-num_pitchers,j - num_pitchers] * players_lineup[i] * players_lineup[j] for i = num_pitchers + 1 :num_players for j = num_pitchers + 1:num_players) )


    #NUMBER OF PLAYERS: 10 players constraint
    @constraint(m, sum(players_lineup[i] for i = 1:num_players) == 10)
    
    #SALARY: Financial Constraint - salary less than $50,000
    @constraint(m, sum(players[i,:Salary] * players_lineup[i] for i = 1:num_players) <= 50000)

    #POSITION
    #  2 P constraint
    @constraint(m, sum(P[i] * players_lineup[i] for i = 1:num_players) == 2)
    # one B1 constraint
    @constraint(m, sum(B1[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B2 constraint
    @constraint(m, sum(B2[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B3 constraint
    @constraint(m, sum(B3[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one C constraint
    @constraint(m, sum(C[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one SS constraint
    @constraint(m, sum(SS[i] * players_lineup[i] for i = 1:num_players) == 1)
    # 3 OF constraint
    @constraint(m, sum(OF[i] * players_lineup[i] for i = 1:num_players) == 3)
  
   
  
    #GAMES: at least 2 different games for the 10 players constraints
    #variable for each game used in lineup, equals 1 if game used
    @variable(m, used_game[i=1:num_games], Bin)
    #constraint satisfied by each game
    @constraint(m, constr[i=1:num_games], used_game[i] <= sum(players_games[t,i] * players_lineup[t] for t = 1:num_players))
    #constraint makes sure at least 2 used_game variables equal 1
    @constraint(m, sum(used_game[i] for i = 1:num_games) >= 2)


    #HITTERS at most 5 hitters from one team constraint
    @constraint(m, constr[i=1:num_teams], sum(players_teams[t,i] * (1 - P[t]) * players_lineup[t] for t = 1:num_players) <= 5)
    

    #OVERLAP Constraint
    @constraint(m, constr[i=1:size(old_lineups)[2]], sum(old_lineups[j,i] * players_lineup[j] for j = 1:num_players) <= num_overlap)

 
    #NO PITCHER VS BATTER no pitcher vs batter constraint
    @constraint(m, hitter_pitcher[g=1:num_teams], 
                   8*sum(P[k] * players_lineup[k] * players_teams[k,g] for k = 1:num_players) + 
                    sum((1 - P[k]) * players_lineup[k] * players_opp[k,g] for k = 1:num_players) <= 8)

    # Exposure Constraint
    @constraint(m, constr[j=1:num_players], sum(old_lineups[j,i] for i = 1:(size(old_lineups))[2]) + players_lineup[j] <= num_lineups * exposure)
   
   

    #STACK: at least stack_size hitters from at least 1 team, consecutive hitting order
   #define a variable for each stack on each team.  This variable =1 if the stack on the team is used
    @variable(m, used_stack_batters[i=1:num_teams,j=1:num_stacks], Bin)
    
    #constraint for each stack, used or not
    @constraint(m, constraint_stack[i=1:num_teams,j=1:num_stacks], stack_size*used_stack_batters[i,j] + 1 <= 
                   sum(players_teams[t,i] * players_stacks[t,j] * players_lineup[t] for t = 1:num_players))  
    
    #make sure at least one stack is used
    @constraint(m, sum(used_stack_batters[i,j] for i = 1:num_teams for j = 1:num_stacks) >= 1)  



   ########################################################################################################
    # Solve the integer programming problem  
    tic()
    status = solve(m);
   

    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        players_lineup_copy = Array(Int64, 0)
        for i=1:num_players
            if getvalue(players_lineup[i]) >= 0.9 && getvalue(players_lineup[i]) <= 1.1
                players_lineup_copy = vcat(players_lineup_copy, fill(1,1))
            else
                players_lineup_copy = vcat(players_lineup_copy, fill(0,1))
            end
        end

        return(players_lineup_copy)
    end
end

#####################################################################################################################
#####################################################################################################################
# This is a function that creates one lineup using the  Stacking Type 3 formulation
    #- Optimization is max(projections + (lambda)*covariance)
    #- General Team Stacking 
    #- Positional Exposure Constraints
    #- consecutive stacking like formulation 3
function formulation5_covar(players, old_lineups, num_overlap,stack_size, P,B1,B2,B3,C,SS,OF, players_teams, players_opp, players_games,players_stacks, covar_matrix, num_pitchers, covar_lambda, exposure, num_lineups, exposure_P,exposure_B1,exposure_B2,exposure_B3,exposure_C,exposure_SS,exposure_OF, min_pitcher_exposure)
    

    #################################################################################################
    #define the model formulation which will be solved using GLPK
    #m = Model(solver=GLPKSolverMIP())

    #uncomment this line only if you are using Gurobi, which costs money :(), but is super fast :)
    m = Model(solver=GurobiSolver(OutputFlag=0))
   
   #number of players playing today
    num_players = size(players)[1]
    
    #number of games today
    games = unique(players[:Game])
    num_games = size(games)[1]
    
    #number of teams playing today
    teams = unique(players[:teamAbbrev])
    num_teams = size(teams)[1]

    #number of stacks per team (this is 9)
    num_stacks = 9;  

    # Variable for players in lineup.
    @variable(m, players_lineup[i=1:num_players], Bin)
    
    #OBJECTIVE
    @objective(m, Max, sum(players[i,:Proj_dfn] * players_lineup[i] for i = 1:num_players) + covar_lambda * sum(covar_matrix[i-num_pitchers,j - num_pitchers] * players_lineup[i] * players_lineup[j] for i = num_pitchers + 1 :num_players for j = num_pitchers + 1:num_players) )


    #NUMBER OF PLAYERS: 10 players constraint
    @constraint(m, sum(players_lineup[i] for i = 1:num_players) == 10)
    
    #SALARY: Financial Constraint - salary less than $50,000
    @constraint(m, sum(players[i,:Salary] * players_lineup[i] for i = 1:num_players) <= 50000)

    #POSITION
    #  2 P constraint
    @constraint(m, sum(P[i] * players_lineup[i] for i = 1:num_players) == 2)
    # one B1 constraint
    @constraint(m, sum(B1[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B2 constraint
    @constraint(m, sum(B2[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B3 constraint
    @constraint(m, sum(B3[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one C constraint
    @constraint(m, sum(C[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one SS constraint
    @constraint(m, sum(SS[i] * players_lineup[i] for i = 1:num_players) == 1)
    # 3 OF constraint
    @constraint(m, sum(OF[i] * players_lineup[i] for i = 1:num_players) == 3)
  
   
  
    #GAMES: at least 2 different games for the 10 players constraints
    #variable for each game used in lineup, equals 1 if game used
    @variable(m, used_game[i=1:num_games], Bin)
    #constraint satisfied by each game
    @constraint(m, constr[i=1:num_games], used_game[i] <= sum(players_games[t,i] * players_lineup[t] for t = 1:num_players))
    #constraint makes sure at least 2 used_game variables equal 1
    @constraint(m, sum(used_game[i] for i = 1:num_games) >= 2)


    #HITTERS at most 5 hitters from one team constraint
    @constraint(m, constr[i=1:num_teams], sum(players_teams[t,i] * (1 - P[t]) * players_lineup[t] for t = 1:num_players) <= 5)
    

    #OVERLAP Constraint
    @constraint(m, constr[i=1:size(old_lineups)[2]], sum(old_lineups[j,i] * players_lineup[j] for j = 1:num_players) <= num_overlap)

 
    #NO PITCHER VS BATTER no pitcher vs batter constraint
    @constraint(m, hitter_pitcher[g=1:num_teams], 
                   8*sum(P[k] * players_lineup[k] * players_teams[k,g] for k = 1:num_players) + 
                    sum((1 - P[k]) * players_lineup[k] * players_opp[k,g] for k = 1:num_players) <= 8)


   
   
    #STACK: at least stack_size hitters from at least 1 team, consecutive hitting order
   #define a variable for each stack on each team.  This variable =1 if the stack on the team is used
    @variable(m, used_stack_batters[i=1:num_teams,j=1:num_stacks], Bin)
    
    #constraint for each stack, used or not
    @constraint(m, constraint_stack[i=1:num_teams,j=1:num_stacks], stack_size*used_stack_batters[i,j] <= 
                   sum(players_teams[t,i] * players_stacks[t,j] * players_lineup[t] for t = num_pitchers + 1:num_players))  
    
    #make sure at least one stack is used
    @constraint(m, sum(used_stack_batters[i,j] for i = 1:num_teams for j = 1:num_stacks) >= 1)  

    # Exposure Constraint P,B1,B2,B3,C,SS,OF
    for i in 1:num_players
        if (P[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_P)
        end
        if (B1[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B1)
        end
        if (B2[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B2)
        end
        if (B3[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B3)
        end
        if (C[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_C)
        end
        if (SS[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_SS)
        end
        if (OF[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_OF)
        end
    end

   ########################################################################################################
    # Solve the integer programming problem  
    tic()
    status = solve(m);
   

    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        players_lineup_copy = Array(Int64, 0)
        for i=1:num_players
            if getvalue(players_lineup[i]) >= 0.9 && getvalue(players_lineup[i]) <= 1.1
                players_lineup_copy = vcat(players_lineup_copy, fill(1,1))
            else
                players_lineup_copy = vcat(players_lineup_copy, fill(0,1))
            end
        end

        return(players_lineup_copy)
    end
end

#####################################################################################################################
#####################################################################################################################
# This is a function that creates one lineup using the  Stacking Type 3 formulation
    #- Optimization is max(projections + (lambda)*covariance)
    #- General Team Stacking 
    #- Position Exposure Constraints 
function formulation6_covar(players, old_lineups, num_overlap,stack_size, P,B1,B2,B3,C,SS,OF, players_teams, players_opp, players_games,players_stacks, covar_matrix, num_pitchers, covar_lambda, exposure, num_lineups, exposure_P,exposure_B1,exposure_B2,exposure_B3,exposure_C,exposure_SS,exposure_OF, min_pitcher_exposure)
    

    #################################################################################################
    #define the model formulation which will be solved using GLPK
    #m = Model(solver=GLPKSolverMIP())

    #uncomment this line only if you are using Gurobi, which costs money :(), but is super fast :)
    m = Model(solver=GurobiSolver(OutputFlag=0))
   
   #number of players playing today
    num_players = size(players)[1]
    
    #number of games today
    games = unique(players[:Game])
    num_games = size(games)[1]
    
    #number of teams playing today
    teams = unique(players[:teamAbbrev])
    num_teams = size(teams)[1]

    #number of stacks per team (this is 9)
    num_stacks = 9;  

    # Variable for players in lineup.
    @variable(m, players_lineup[i=1:num_players], Bin)
    
    #OBJECTIVE
    @objective(m, Max, sum(players[i,:Proj_dfn] * players_lineup[i] for i = 1:num_players) + covar_lambda * sum(covar_matrix[i-num_pitchers,j - num_pitchers] * players_lineup[i] * players_lineup[j] for i = num_pitchers + 1 :num_players for j = num_pitchers + 1:num_players) )


    #NUMBER OF PLAYERS: 10 players constraint
    @constraint(m, sum(players_lineup[i] for i = 1:num_players) == 10)
    
    #SALARY: Financial Constraint - salary less than $50,000
    @constraint(m, sum(players[i,:Salary] * players_lineup[i] for i = 1:num_players) <= 50000)

    #POSITION
    #  2 P constraint
    @constraint(m, sum(P[i] * players_lineup[i] for i = 1:num_players) == 2)
    # one B1 constraint
    @constraint(m, sum(B1[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B2 constraint
    @constraint(m, sum(B2[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B3 constraint
    @constraint(m, sum(B3[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one C constraint
    @constraint(m, sum(C[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one SS constraint
    @constraint(m, sum(SS[i] * players_lineup[i] for i = 1:num_players) == 1)
    # 3 OF constraint
    @constraint(m, sum(OF[i] * players_lineup[i] for i = 1:num_players) == 3)
  
   
  
    #GAMES: at least 2 different games for the 10 players constraints
    #variable for each game used in lineup, equals 1 if game used
    @variable(m, used_game[i=1:num_games], Bin)
    #constraint satisfied by each game
    @constraint(m, constr[i=1:num_games], used_game[i] <= sum(players_games[t,i] * players_lineup[t] for t = 1:num_players))
    #constraint makes sure at least 2 used_game variables equal 1
    @constraint(m, sum(used_game[i] for i = 1:num_games) >= 2)


    #HITTERS at most 5 hitters from one team constraint
    @constraint(m, constr[i=1:num_teams], sum(players_teams[t,i] * (1 - P[t]) * players_lineup[t] for t = 1:num_players) <= 5)
    

    #OVERLAP Constraint
    @constraint(m, constr[i=1:size(old_lineups)[2]], sum(old_lineups[j,i] * players_lineup[j] for j = 1:num_players) <= num_overlap)

 
    #NO PITCHER VS BATTER no pitcher vs batter constraint
    @constraint(m, hitter_pitcher[g=1:num_teams], 
                   8*sum(P[k] * players_lineup[k] * players_teams[k,g] for k = 1:num_players) + 
                    sum((1 - P[k]) * players_lineup[k] * players_opp[k,g] for k = 1:num_players) <= 8)

    
    # Exposure Constraint P,B1,B2,B3,C,SS,OF
    for i in 1:num_players
        if (P[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_P)
        end
        if (B1[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B1)
        end
        if (B2[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B2)
        end
        if (B3[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B3)
        end
        if (C[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_C)
        end
        if (SS[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_SS)
        end
        if (OF[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_OF)
        end
    end
   
    #STACK: [Stack_Size] number of players from a single team must be used
    @variable(m, used_stack_team[i=1:num_teams], Bin)
    
    #constraint for each stack, used or not
    @constraint(m, constraint_stack[i=1:num_teams], stack_size*used_stack_team[i] <= 
                   sum(players_teams[t,i] * players_lineup[t] for t = num_pitchers + 1:num_players))  
    
    #make sure at least one stack is used
    @constraint(m, sum(used_stack_team[i] for i = 1:num_teams) >= 1)  


   ########################################################################################################
    # Solve the integer programming problem  
    tic()
    status = solve(m);
   

    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        players_lineup_copy = Array(Int64, 0)
        for i=1:num_players
            if getvalue(players_lineup[i]) >= 0.9 && getvalue(players_lineup[i]) <= 1.1
                players_lineup_copy = vcat(players_lineup_copy, fill(1,1))
            else
                players_lineup_copy = vcat(players_lineup_copy, fill(0,1))
            end
        end

        return(players_lineup_copy)
    end
end
   
#####################################################################################################################
#####################################################################################################################
# This is a function that creates one lineup using the  Stacking Type 3 formulation
    #- Optimization is max(projections + (lambda)*covariance)
    #- General Team Stacking 
    #- Positional Exposure Constraints
    #- consecutive stacking like formulation 3
    #- Min Exposure Constraint for Pitcher
function formulation7_covar(players, old_lineups, num_overlap,stack_size, P,B1,B2,B3,C,SS,OF, players_teams, players_opp, players_games,players_stacks, covar_matrix, num_pitchers, covar_lambda, exposure, num_lineups, exposure_P,exposure_B1,exposure_B2,exposure_B3,exposure_C,exposure_SS,exposure_OF, min_pitcher_exposure)
    

    #################################################################################################
    #define the model formulation which will be solved using GLPK
    #m = Model(solver=GLPKSolverMIP())

    #uncomment this line only if you are using Gurobi, which costs money :(), but is super fast :)
    m = Model(solver=GurobiSolver(OutputFlag=0))
   
   #number of players playing today
    num_players = size(players)[1]
    
    #number of games today
    games = unique(players[:Game])
    num_games = size(games)[1]
    
    #number of teams playing today
    teams = unique(players[:teamAbbrev])
    num_teams = size(teams)[1]

    #number of stacks per team (this is 9)
    num_stacks = 9;  

    # Variable for players in lineup.
    @variable(m, players_lineup[i=1:num_players], Bin)
    
    #OBJECTIVE
    @objective(m, Max, sum(players[i,:Proj_dfn] * players_lineup[i] for i = 1:num_players) + covar_lambda * sum(covar_matrix[i-num_pitchers,j - num_pitchers] * players_lineup[i] * players_lineup[j] for i = num_pitchers + 1 :num_players for j = num_pitchers + 1:num_players) )


    #NUMBER OF PLAYERS: 10 players constraint
    @constraint(m, sum(players_lineup[i] for i = 1:num_players) == 10)
    
    #SALARY: Financial Constraint - salary less than $50,000
    @constraint(m, sum(players[i,:Salary] * players_lineup[i] for i = 1:num_players) <= 50000)

    #POSITION
    #  2 P constraint
    @constraint(m, sum(P[i] * players_lineup[i] for i = 1:num_players) == 2)
    # one B1 constraint
    @constraint(m, sum(B1[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B2 constraint
    @constraint(m, sum(B2[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B3 constraint
    @constraint(m, sum(B3[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one C constraint
    @constraint(m, sum(C[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one SS constraint
    @constraint(m, sum(SS[i] * players_lineup[i] for i = 1:num_players) == 1)
    # 3 OF constraint
    @constraint(m, sum(OF[i] * players_lineup[i] for i = 1:num_players) == 3)
  
   
  
    #GAMES: at least 2 different games for the 10 players constraints
    #variable for each game used in lineup, equals 1 if game used
    @variable(m, used_game[i=1:num_games], Bin)
    #constraint satisfied by each game
    @constraint(m, constr[i=1:num_games], used_game[i] <= sum(players_games[t,i] * players_lineup[t] for t = 1:num_players))
    #constraint makes sure at least 2 used_game variables equal 1
    @constraint(m, sum(used_game[i] for i = 1:num_games) >= 2)


    #HITTERS at most 5 hitters from one team constraint
    @constraint(m, constr[i=1:num_teams], sum(players_teams[t,i] * (1 - P[t]) * players_lineup[t] for t = 1:num_players) <= 5)
    

    # #OVERLAP Constraint
    @constraint(m, constr[i=1:size(old_lineups)[2]], sum(old_lineups[j,i] * players_lineup[j] for j = 1:num_players) <= num_overlap)

 
    # #NO PITCHER VS BATTER no pitcher vs batter constraint
    @constraint(m, hitter_pitcher[g=1:num_teams], 
                   8*sum(P[k] * players_lineup[k] * players_teams[k,g] for k = 1:num_players) + 
                    sum((1 - P[k]) * players_lineup[k] * players_opp[k,g] for k = 1:num_players) <= 8)


   
   
    #STACK: at least stack_size hitters from at least 1 team, consecutive hitting order
   #define a variable for each stack on each team.  This variable =1 if the stack on the team is used
    @variable(m, used_stack_batters[i=1:num_teams,j=1:num_stacks], Bin)
    
    #constraint for each stack, used or not
    @constraint(m, constraint_stack[i=1:num_teams,j=1:num_stacks], stack_size*used_stack_batters[i,j] <= 
                   sum(players_teams[t,i] * players_stacks[t,j] * players_lineup[t] for t = num_pitchers + 1:num_players))  
    
    #make sure at least one stack is used
    @constraint(m, sum(used_stack_batters[i,j] for i = 1:num_teams for j = 1:num_stacks) >= 1)  

    # Min of Max Exposure Constraint 
    @variable(m, pitcher_count[i=1:num_players], Bin)

    # Exposure Constraint P,B1,B2,B3,C,SS,OF
    for i in 1:num_players
        if (P[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_P)

            #Min Exposure Constraint
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] >= min_pitcher_exposure*(size(old_lineups)[2])*pitcher_count[i])
        end
        if (B1[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B1)
        end
        if (B2[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B2)
        end
        if (B3[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B3)
        end
        if (C[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_C)
        end
        if (SS[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_SS)
        end
        if (OF[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_OF)
        end
    end

    #make sure at least one pitcher is above the exposure constraint
    @constraint(m, sum(pitcher_count[i]*P[i] for i = 1:num_players) >= 1)  
    

   ########################################################################################################
    # Solve the integer programming problem  
    tic()
    status = solve(m);
   

    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        players_lineup_copy = Array(Int64, 0)
        for i=1:num_players
            if getvalue(players_lineup[i]) >= 0.9 && getvalue(players_lineup[i]) <= 1.1
                players_lineup_copy = vcat(players_lineup_copy, fill(1,1))
            else
                players_lineup_copy = vcat(players_lineup_copy, fill(0,1))
            end
        end

        return(players_lineup_copy)
    end
end

#####################################################################################################################
#####################################################################################################################
# This is a function that creates one lineup using the  Stacking Type 3 formulation
    #- Optimization is max(projections + (lambda)*covariance)
    #- General Team Stacking 
    #- Position Exposure Constraints 
    #- Min Exposure Constraint for Pitcher
function formulation8_covar(players, old_lineups, num_overlap,stack_size, P,B1,B2,B3,C,SS,OF, players_teams, players_opp, players_games,players_stacks, covar_matrix, num_pitchers, covar_lambda, exposure, num_lineups, exposure_P,exposure_B1,exposure_B2,exposure_B3,exposure_C,exposure_SS,exposure_OF, min_pitcher_exposure)
    

    #################################################################################################
    #define the model formulation which will be solved using GLPK
    #m = Model(solver=GLPKSolverMIP())

    #uncomment this line only if you are using Gurobi, which costs money :(), but is super fast :)
    m = Model(solver=GurobiSolver(OutputFlag=0))
   
   #number of players playing today
    num_players = size(players)[1]
    
    #number of games today
    games = unique(players[:Game])
    num_games = size(games)[1]
    
    #number of teams playing today
    teams = unique(players[:teamAbbrev])
    num_teams = size(teams)[1]

    #number of stacks per team (this is 9)
    num_stacks = 9;  

    # Variable for players in lineup.
    @variable(m, players_lineup[i=1:num_players], Bin)
    
    #OBJECTIVE
    @objective(m, Max, sum(players[i,:Proj_dfn] * players_lineup[i] for i = 1:num_players) + covar_lambda * sum(covar_matrix[i-num_pitchers,j - num_pitchers] * players_lineup[i] * players_lineup[j] for i = num_pitchers + 1 :num_players for j = num_pitchers + 1:num_players) )


    #NUMBER OF PLAYERS: 10 players constraint
    @constraint(m, sum(players_lineup[i] for i = 1:num_players) == 10)
    
    #SALARY: Financial Constraint - salary less than $50,000
    @constraint(m, sum(players[i,:Salary] * players_lineup[i] for i = 1:num_players) <= 50000)

    #POSITION
    #  2 P constraint
    @constraint(m, sum(P[i] * players_lineup[i] for i = 1:num_players) == 2)
    # one B1 constraint
    @constraint(m, sum(B1[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B2 constraint
    @constraint(m, sum(B2[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B3 constraint
    @constraint(m, sum(B3[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one C constraint
    @constraint(m, sum(C[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one SS constraint
    @constraint(m, sum(SS[i] * players_lineup[i] for i = 1:num_players) == 1)
    # 3 OF constraint
    @constraint(m, sum(OF[i] * players_lineup[i] for i = 1:num_players) == 3)
  
   
  
    #GAMES: at least 2 different games for the 10 players constraints
    #variable for each game used in lineup, equals 1 if game used
    @variable(m, used_game[i=1:num_games], Bin)
    #constraint satisfied by each game
    @constraint(m, constr[i=1:num_games], used_game[i] <= sum(players_games[t,i] * players_lineup[t] for t = 1:num_players))
    #constraint makes sure at least 2 used_game variables equal 1
    @constraint(m, sum(used_game[i] for i = 1:num_games) >= 2)


    #HITTERS at most 5 hitters from one team constraint
    @constraint(m, constr[i=1:num_teams], sum(players_teams[t,i] * (1 - P[t]) * players_lineup[t] for t = 1:num_players) <= 5)
    

    #OVERLAP Constraint
    @constraint(m, constr[i=1:size(old_lineups)[2]], sum(old_lineups[j,i] * players_lineup[j] for j = 1:num_players) <= num_overlap)

 
    #NO PITCHER VS BATTER no pitcher vs batter constraint
    @constraint(m, hitter_pitcher[g=1:num_teams], 
                   8*sum(P[k] * players_lineup[k] * players_teams[k,g] for k = 1:num_players) + 
                    sum((1 - P[k]) * players_lineup[k] * players_opp[k,g] for k = 1:num_players) <= 8)

    
    # Min of Max Exposure Constraint 
    @variable(m, pitcher_count[i=1:num_players], Bin)
    
    # Exposure Constraint P,B1,B2,B3,C,SS,OF
    for i in 1:num_players
        if (P[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_P)

            #Min Exposure Constraint
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] >= min_pitcher_exposure*(size(old_lineups)[2])*pitcher_count[i])
        end
        if (B1[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B1)
        end
        if (B2[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B2)
        end
        if (B3[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B3)
        end
        if (C[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_C)
        end
        if (SS[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_SS)
        end
        if (OF[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_OF)
        end
    end
   
    #STACK: [Stack_Size] number of players from a single team must be used
    @variable(m, used_stack_team[i=1:num_teams], Bin)
    
    #constraint for each stack, used or not
    @constraint(m, constraint_stack[i=1:num_teams], stack_size*used_stack_team[i] <= 
                   sum(players_teams[t,i] * players_lineup[t] for t = num_pitchers + 1:num_players))  
    
    #make sure at least one stack is used
    @constraint(m, sum(used_stack_team[i] for i = 1:num_teams) >= 1)  

    #make sure at least one pitcher is above the exposure constraint
    @constraint(m, sum(pitcher_count[i]*P[i] for i = 1:num_players) >= 1)  

   ########################################################################################################
    # Solve the integer programming problem  
    tic()
    status = solve(m);
   

    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        players_lineup_copy = Array(Int64, 0)
        for i=1:num_players
            if getvalue(players_lineup[i]) >= 0.9 && getvalue(players_lineup[i]) <= 1.1
                players_lineup_copy = vcat(players_lineup_copy, fill(1,1))
            else
                players_lineup_copy = vcat(players_lineup_copy, fill(0,1))
            end
        end

        return(players_lineup_copy)
    end
end


#####################################################################################################################
#####################################################################################################################
# This is a function that creates one lineup using the  Stacking Type 3 formulation
    #- Optimization is max(projections + (lambda)*covariance)
    #- General Team Stacking 
    #- Positional Exposure Constraints
    #- consecutive stacking like formulation 3
    #- Min Exposure Constraint for Pitcher
    #- C salary <= 3200
function formulation9_covar(players, old_lineups, num_overlap,stack_size, P,B1,B2,B3,C,SS,OF, players_teams, players_opp, players_games,players_stacks, covar_matrix, num_pitchers, covar_lambda, exposure, num_lineups, exposure_P,exposure_B1,exposure_B2,exposure_B3,exposure_C,exposure_SS,exposure_OF, min_pitcher_exposure)
    

    #################################################################################################
    #define the model formulation which will be solved using GLPK
    #m = Model(solver=GLPKSolverMIP())

    #uncomment this line only if you are using Gurobi, which costs money :(), but is super fast :)
    m = Model(solver=GurobiSolver(OutputFlag=0))
   
   #number of players playing today
    num_players = size(players)[1]
    
    #number of games today
    games = unique(players[:Game])
    num_games = size(games)[1]
    
    #number of teams playing today
    teams = unique(players[:teamAbbrev])
    num_teams = size(teams)[1]

    #number of stacks per team (this is 9)
    num_stacks = 9;  

    # Variable for players in lineup.
    @variable(m, players_lineup[i=1:num_players], Bin)
    
    #OBJECTIVE
    @objective(m, Max, sum(players[i,:Proj_dfn] * players_lineup[i] for i = 1:num_players) + covar_lambda * sum(covar_matrix[i-num_pitchers,j - num_pitchers] * players_lineup[i] * players_lineup[j] for i = num_pitchers + 1 :num_players for j = num_pitchers + 1:num_players) )


    #NUMBER OF PLAYERS: 10 players constraint
    @constraint(m, sum(players_lineup[i] for i = 1:num_players) == 10)
    
    #SALARY: Financial Constraint - salary less than $50,000
    @constraint(m, sum(players[i,:Salary] * players_lineup[i] for i = 1:num_players) <= 50000)

    #POSITION
    #  2 P constraint
    @constraint(m, sum(P[i] * players_lineup[i] for i = 1:num_players) == 2)
    # one B1 constraint
    @constraint(m, sum(B1[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B2 constraint
    @constraint(m, sum(B2[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one B3 constraint
    @constraint(m, sum(B3[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one C constraint
    @constraint(m, sum(C[i] * players_lineup[i] for i = 1:num_players) == 1)
    # one SS constraint
    @constraint(m, sum(SS[i] * players_lineup[i] for i = 1:num_players) == 1)
    # 3 OF constraint
    @constraint(m, sum(OF[i] * players_lineup[i] for i = 1:num_players) == 3)
  
    # All catchers salary 3200 or less
    @constraint(m, sum(players[i,:Salary] * C[i] * players_lineup[i] for i=1:num_players) <= 3200)
   
  
    #GAMES: at least 2 different games for the 10 players constraints
    #variable for each game used in lineup, equals 1 if game used
    @variable(m, used_game[i=1:num_games], Bin)
    #constraint satisfied by each game
    @constraint(m, constr[i=1:num_games], used_game[i] <= sum(players_games[t,i] * players_lineup[t] for t = 1:num_players))
    #constraint makes sure at least 2 used_game variables equal 1
    @constraint(m, sum(used_game[i] for i = 1:num_games) >= 2)


    #HITTERS at most 5 hitters from one team constraint
    @constraint(m, constr[i=1:num_teams], sum(players_teams[t,i] * (1 - P[t]) * players_lineup[t] for t = 1:num_players) <= 5)
    

    #OVERLAP Constraint
    @constraint(m, constr[i=1:size(old_lineups)[2]], sum(old_lineups[j,i] * players_lineup[j] for j = 1:num_players) <= num_overlap)

 
    #NO PITCHER VS BATTER no pitcher vs batter constraint
    @constraint(m, hitter_pitcher[g=1:num_teams], 
                   8*sum(P[k] * players_lineup[k] * players_teams[k,g] for k = 1:num_players) + 
                    sum((1 - P[k]) * players_lineup[k] * players_opp[k,g] for k = 1:num_players) <= 8)


   
   
    #STACK: at least stack_size hitters from at least 1 team, consecutive hitting order
   #define a variable for each stack on each team.  This variable =1 if the stack on the team is used
    @variable(m, used_stack_batters[i=1:num_teams,j=1:num_stacks], Bin)
    
    #constraint for each stack, used or not
    @constraint(m, constraint_stack[i=1:num_teams,j=1:num_stacks], stack_size*used_stack_batters[i,j] <= 
                   sum(players_teams[t,i] * players_stacks[t,j] * players_lineup[t] for t = num_pitchers + 1:num_players))  
    
    #make sure at least one stack is used
    @constraint(m, sum(used_stack_batters[i,j] for i = 1:num_teams for j = 1:num_stacks) >= 1)  

    # Exposure Constraint P,B1,B2,B3,C,SS,OF
    for i in 1:num_players
        if (P[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_P)
        end
        if (B1[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B1)
        end
        if (B2[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B2)
        end
        if (B3[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_B3)
        end
        if (C[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_C)
        end
        if (SS[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_SS)
        end
        if (OF[i] == 1)
            @constraint(m, sum(old_lineups[i,j] for j = 1:(size(old_lineups))[2]) + players_lineup[i] <= num_lineups * exposure_OF)
        end
    end

   ########################################################################################################
    # Solve the integer programming problem  
    tic()
    status = solve(m);
   

    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        players_lineup_copy = Array(Int64, 0)
        for i=1:num_players
            if getvalue(players_lineup[i]) >= 0.9 && getvalue(players_lineup[i]) <= 1.1
                players_lineup_copy = vcat(players_lineup_copy, fill(1,1))
            else
                players_lineup_copy = vcat(players_lineup_copy, fill(0,1))
            end
        end

        return(players_lineup_copy)
    end
end
   

end