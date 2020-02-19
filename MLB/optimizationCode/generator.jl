# This code solves for multiple baseball lineups

include("data_cleaning.jl")
include("formulations.jl")  #this code has all the different formualations


################################################################################################################
# Contest information

contest_date = "2017-06-15";
contest_name = "\$33.00entry_MLB\$150KFastball";


################################################################################################################

#INPUT PARAMS
# num_lineups is the total number of lineups
num_lineups = 150; 

# num_overlap is the maximum overlap of players between the lineups 
num_overlap = 5;

#number of hitters in the stack (number of consecutive hitters in the hitting order)
stack_size = 5; 

#FORMULATION:  formulation is the type of formulation that you would like to use. 
formulation = formulations.formulation9_covar

# Covariance term 
lambda_var = 0.001

# Exposure Constraints
exposure = 0.6

exposure_P = 0.8
exposure_B1 = 0.3
exposure_B2 = 0.4
exposure_B3 = 0.6
exposure_C = 0.3
exposure_SS = 0.3
exposure_OF = 0.6

min_pitcher_exposure = 0.4

################################################################################################################

contest_directory_path = string("../data_warehouse/", contest_date, "/", contest_name, "/");

#path to the csv file with the players information (pitchers and hitters);
path_pitchers = string(contest_directory_path, "pitchers.csv"); 
path_hitters = string(contest_directory_path, "hitters.csv"); 
path_covar_matrix = string(contest_directory_path, "covariance_mat_chg75p_exp(spike).csv"); 
# path_to_output is a string  that gives the path to the csv file that will give the outputted results

path_to_output= string(contest_directory_path, "lineups/",
     string(formulation), "_stacksize_", stack_size,"_overlap_", num_overlap,"_lineups_", num_lineups,"_lambda_", lambda_var,
     "_exposure_P", exposure_P,"_exposure_B1", exposure_B1,"_exposure_B2", exposure_B2,"_exposure_B3", exposure_B3,"_exposure_C", exposure_C,"_exposure_SS", exposure_SS,"_exposure_OF", exposure_OF,"_min_pitcher_exposure", min_pitcher_exposure,"_debug.csv"); 


start_time = time_ns()

data_cleaning.create_lineups(num_lineups, num_overlap, stack_size,formulation, path_pitchers,path_hitters, path_covar_matrix, lambda_var, exposure, exposure_P,exposure_B1,exposure_B2,exposure_B3,exposure_C,exposure_SS,exposure_OF, min_pitcher_exposure, path_to_output);


println("##############################")
println("###### Finished Lineups ######")
println("##############################")

println("\nCalculated DraftKings baseball lineups.\n\tNumber of lineups = ", num_lineups, " \n\tStack size = ",stack_size,
"\n\tOverlap = ", num_overlap,"\n" )
end_time = time_ns()
println("Took ", (end_time - start_time)/60e10, " minutes to calculate ", num_lineups, " lineups")

println("Saving data to file ",path_to_output)

# for z in 3:9 
#     for k in 2:5 #stack
#         for i in 1:9 #temp line
#             for j in 3:9 # time line
#                 num_overlap = z

#                 stack_size = k

#                 # Covariance term 
#                 lambda_var = 0.001 * i

#                 # Exposure Constraints
#                 exposure = 0.1 * j

#                 path_to_output= string(contest_directory_path, "/lineups/",
#                                string(formulation), "_stacksize_", stack_size,"_overlap_", num_overlap,"_lineups_", num_lineups,"_lambda_", lambda_var,"_exposure_", exposure,"_test_stacks.csv"); 

#                 start_time = time_ns()

#                 data_cleaning.create_lineups(num_lineups, num_overlap, stack_size,formulation, path_pitchers,path_hitters, path_covar_matrix, lambda_var, exposure,  path_to_output);


#                 println("##############################")
#                 println("###### Finished Lineups ######")
#                 println("##############################")

#                 println("\nCalculated DraftKings baseball lineups.\n\tNumber of lineups = ", num_lineups, " \n\tStack size = ",stack_size,
#                 "\n\tOverlap = ", num_overlap,"\n" )
#                 end_time = time_ns()
#                 println("Took ", (end_time - start_time)/60e10, " minutes to calculate ", num_lineups, " lineups")

#                 println("Saving data to file ",path_to_output)
#             end
#         end
#     end
# end

#save the projected and actual points for the lineups
#lineup_points_proj(path_to_output,path_hitters,path_pitchers,path_to_output_proj);
#lineup_points_actual(path_to_output,path_hitters,path_pitchers,path_to_output_actual);

