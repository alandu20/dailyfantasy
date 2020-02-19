# This code solves for multiple baseball lineups
using DataFrames
include("data_cleaning.jl")
include("formulations.jl")  #this code has all the different formualations

################################################################################################################
# Choose Formulation

#FORMULATION:  formulation is the type of formulation that you would like to use. 
formulation = formulations.formulation5_covar
# formulation_feasibility
# formulation0_covar
# formulation1_covar - no stacking
# formulation2_covar - stacking

#########################################################################
# Running the code

num_lineups = 150; 


baseline_contest_data = readtable("baseline_contests_temp.csv");

for contest_info_index in 1:size(baseline_contest_data)[1]
    for stack in 5:5 #stack
        for overlap in 5:5
            for lambda in [5] # time line
              for exposure_P in [0.8] # time line
                for exposure_B1 in [0.5] # time line
                  for exposure_B2 in [0.4] # time line
                    for exposure_B3 in [0.6] # time line
                      for exposure_C in [0.5] # time line
                        for exposure_SS in [0.5] # time line
                          for exposure_OF in [0.6] # time line
                            for min_pitcher_exposure in [0.6]

                              num_overlap = overlap

                              stack_size = stack

                              # Covariance term 
                              lambda_var = 0.001 * lambda

                              # Exposure Constraints
                              exposure = 0.6

                              contest_date = baseline_contest_data[contest_info_index,:Date] #Hard Coded 'Date'
                              contest_name = baseline_contest_data[contest_info_index,:Contest_names] # Hard Coded 'Contest_name'

                             contest_directory_path = string("../data_warehouse/", contest_date, "/", contest_name, "/");

                             #path to the csv file with the players information (pitchers and hitters);
                             path_pitchers = string(contest_directory_path, "pitchers.csv"); 
                             path_hitters = string(contest_directory_path, "hitters.csv"); 
                             path_covar_matrix = string(contest_directory_path, "covariance_mat_update.csv"); 
                             # path_to_output is a string  that gives the path to the csv file that will give the outputted results

                              if formulation == formulations.formulation7_covar || formulation == formulations.formulation8_covar
                                path_to_output= string(contest_directory_path, "lineups/",
                                     string(formulation), "_stacksize_", stack_size,"_overlap_", num_overlap,"_lineups_", num_lineups,"_lambda_", lambda_var,
                                     "_exposure_P", exposure_P,"_exposure_B1", exposure_B1,"_exposure_B2", exposure_B2,"_exposure_B3", exposure_B3,"_exposure_C", exposure_C,"_exposure_SS", exposure_SS,"_exposure_OF", exposure_OF,"_min_pitcher_exposure", min_pitcher_exposure,"_covar_update.csv"); 
                              else
                                path_to_output= string(contest_directory_path, "lineups/",
                                     string(formulation), "_stacksize_", stack_size,"_overlap_", num_overlap,"_lineups_", num_lineups,"_lambda_", lambda_var,
                                     "_exposure_P", exposure_P,"_exposure_B1", exposure_B1,"_exposure_B2", exposure_B2,"_exposure_B3", exposure_B3,"_exposure_C", exposure_C,"_exposure_SS", exposure_SS,"_exposure_OF", exposure_OF,"_covar_update.csv"); 
                              end


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
                            end
                          end
                        end
                      end
                    end
                  end
                end
              end
            end
        end
    end
end

