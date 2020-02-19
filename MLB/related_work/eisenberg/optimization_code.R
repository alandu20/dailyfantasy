#function that returns the optimal lineup
#specifically it returns a list where the first element is the data from that day
#and the second element is the output of the Gurobi Optimzation command
#takes a season (integer), date (4 character string), risk tolerance (lambda),
#vector of predictions, constraint matrices, and the covariance matrix
opt_lineup <-function(season, date, lambda, Team_mat, Salary_mat, Position_mat, Cov_mat, C_pred)
  #extracting data from the desired day
  daily_data = opt_final[opt_final$season==season & opt_final$date == date, ]
  #treat all OF the same
  daily_data$field = ifelse(daily_data$field>6, 7, daily_data$field)
  #N is the number of players to choose from
  N = nrow(daily_data)
  #rename the quadratic constraint matrix
  linear_constraint = as.matrix(rbind(Team_mat, Salary_mat, Position_mat))
  A = linear_constraint
  #the objective vector is projected points
  C = C_pred
  #############
  #building the Gurobi model
  model = list()
  #defining the objective function
  model$modelsense = "max"
  model$obj = C
  #including the risk parameter lambda
  model$Q = lambda*Cov_mat
  
  #setting the linear constraint
  model$A = A
  #quick way to define equality signs and RHS constraints for
  #an arbitrary number of teams
  relation = c()
  right_side = c()
  for(i in 1:nrow(A)) {
    #positions
    if(i <= 6) {
      relation = append(relation, "=")
      right_side = append(right_side, 1)
    }
    #you get 3 OF
    if(i==7) {
      relation = append(relation, "=")
      right_side = append(right_side, 3)
    }
    #for teams
    if(i > 7 & i < nrow(A)) {
      relation = append(relation, "<=")
      right_side = append(right_side, 4)
    }
    #salary
    if(i==nrow(A)) {
      relation = append(relation, "<=")
      right_side = append(right_side, 35000)
    }
  }
  #setting the right hand side of the constraints
  model$sense = relation
  model$rhs = right_side
  #all variables are binary
  model$vtype = rep("B", ncol(A))
  #the result of the model
  model_result = gurobi(model, params = NULL)
  output = list(daily_data, model_result)
  #return the data and the optimization result
  return(output)
  }