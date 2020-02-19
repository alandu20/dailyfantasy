#=
This code is our (Michael Chiang and Alan Du) adaptation of the techniques used 
in the paper, Winning Daily Fantasy Hockey Contests Using Integer Programming by 
Hunter, Vielma, and Zaman. 

=#

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

# Running
# exec '/Applications/Julia-0.5.app/Contents/Resources/julia/bin/julia'
# include("generator.jl")

include("formulations.jl")

################################################################################################################
# Contest information

contest_date = "2017-12-24";
contest_name = "\$8.00entry_NFL \$1.5M Santa’s Eight Reindeer Special [\$100K to 1st]";

############################  Setting Variables  ############################

#=
Variables for solving the problem (change these)
=#
# num_lineups is the total number of lineups
num_lineups = 75

# num_overlap is the maximum overlap of players between the lineups that you create
num_overlap = 4

# exposure is a number from 0-1 that gives the total % of lineups that a single player can be in
exposure = 0.4

#Only used for Formulation 14
exposure_defense = 0.25
exposure_wr = 0.40
exposure_rb = 0.75
exposure_te = 0.75
exposure_qb = 0.5
exposure_valuewr = 0.1

###########################  Setting Formation  ############################

use_Freq_Ind = false


############################  Setting Formation  ############################

#=
formulation is the type of formulation that you would like to use. 
    Available Options: 
        - 0 <- one_lineup_no_stacking
        - 1 <- one_lineup_Type_1 (form 0 + defense-offense constraint)
        - 2 <- one_lineup_Type_2 (form 1 + QB-WR stack)
        - 3 <- one_lineup_Type_3 (form 2 + QB-oppWR stack)
        - 4 <- one_lineup_Type_4 (form 2 + no TE for flex)
        - 5 <- one_lineup_Type_5 (form 2 + no TE or RB for flex)
        - 6 <- one_lineup_Type_6 (form 4 + QB-oppWR stack)
        - 7 <- one_lineup_Type_7 (form 6 + RB Salary < 5000)
        - 8 <- one_lineup_Type_8 In Progress (form 7 + RB can not be from the same team as WR or TE)
        - 9 <- one_lineup_Type_9 (form 4 + QB-Top Receiver (#1/#2 in RankTargets) stack)
        - 10 <- one_lineup_Type_10 (form 9 + no 3 players from same team)
        - 11 <- one_lineup_Type_11 (form 10 + RB Salary < 5000)
        - 12 <- one_lineup_Type_12 (form 9 + no 4 players from same team)
        - 13 <- one_lineup_Type_13 (form 10 + all 3 WR (excluding flex) in top 3 in RankTargets)
        - 14 <- one_lineup_Type_14 (form 4 with player exposure constraints)
        - 15 <- one_lineup_Type_15 (form 14 + must have 1 value wr + 2700 <= DST Salary <= 3100)
        - 16 <- one_lineup_Type_16 (form 13 + player exposure + must have 1 value wr + 2700 <= DST Salary <= 3100)
        - 17 <- one_lineup_Type_17 (form 14 + 2700 <= DST Salary <= 3100)
=#
formulation_type = 14


############################  Setting Projections Source  ############################

#=
projections_source tells which Projections we're using for this generation
    Available Options: 
        - "Projection" <- From rotogrinders 
        - "Projection_dfn"
        - "Projection_fc"
        - "Projection_dfn_perturbed"
        - "Projection_reg"
        - "Projection_reg_split"
        - "Actual" (historical)
=#
projections_source = "Projection_dfn"

############################  Create Paths to data  ############################
contest_directory_path = string("../data_warehouse/", contest_date, "/", contest_name, "/");

#path to the csv file with the players information
path_defenses = string(contest_directory_path, "defenses.csv"); 
path_offensive_players = string(contest_directory_path, "offensive_players.csv"); 
path_to_output = "output.csv"

if (formulation_type == 1) 
    formulation = formulations.one_lineup_Type_1
elseif (formulation_type == 2) 
    formulation = formulations.one_lineup_Type_2
elseif (formulation_type == 3)
    formulation = formulations.one_lineup_Type_3
elseif (formulation_type == 4) 
    formulation = formulations.one_lineup_Type_4
elseif (formulation_type == 5)
    formulation = formulations.one_lineup_Type_5
elseif (formulation_type == 6)
    formulation = formulations.one_lineup_Type_6
elseif (formulation_type == 7) 
    formulation = formulations.one_lineup_Type_7  
elseif (formulation_type == 8) 
    formulation = formulations.one_lineup_Type_8  
elseif (formulation_type == 9) 
    formulation = formulations.one_lineup_Type_9  
elseif (formulation_type == 10) 
    formulation = formulations.one_lineup_Type_10  
elseif (formulation_type == 11) 
    formulation = formulations.one_lineup_Type_11
elseif (formulation_type == 12) 
    formulation = formulations.one_lineup_Type_12
elseif (formulation_type == 13) 
    formulation = formulations.one_lineup_Type_13
elseif (formulation_type == 14) 
    formulation = formulations.one_lineup_Type_14
elseif (formulation_type == 15) 
    formulation = formulations.one_lineup_Type_15
elseif (formulation_type == 16) 
    formulation = formulations.one_lineup_Type_16
elseif (formulation_type == 17) 
    formulation = formulations.one_lineup_Type_17
elseif (formulation_type == 18) 
    formulation = formulations.one_lineup_Type_18
else
    formulation = formulations.one_lineup_no_stacking 
end

########### Running the code ###########


# Normal (Live) Output
formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, path_to_output, projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)


