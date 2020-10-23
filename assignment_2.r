library(WheresCroc)

myFunction = function (moveInfo, readings, positions, edges, probs){
  checkIfInTop = 3
  # reset state if new game
  if((moveInfo$mem$status >= 0)) { # status new game has started
    #reset states
    dim = length(probs$salinity[,1])
    
    moveInfo$mem$state = rep(1/dim, dim)
    if ((moveInfo$mem$status == 0)) { # status first game has started
      connections = diag(1, nrow = dim, ncol = dim)
      
      # set connections in matrix
      connections[edges] = 1
      connections[edges[, c(2,1)]] = 1
      
      # calculate transition_matrix based on connections
      divide_by = colSums(connections)
      moveInfo$mem$connections = connections
      moveInfo$mem$transition_matrix = sweep(connections, 2, divide_by, FUN = '/')
      moveInfo$mem$checkIfInTop = 22
    }
    
    moveInfo$mem$status = -1
  }
  
  connections = moveInfo$mem$connections
  state = moveInfo$mem$state
  transition_matrix  = moveInfo$mem$transition_matrix
  player_pos = positions[3]
  
  # update sate based on backpackers
  if (is.na(positions[1]) == FALSE) {
    if (positions[1] < 0) { # gets eaten
      state[] = 0
      state[positions[1]] = 1
    } else {
      state[positions[1]] = 0
    }
  }
  if (is.na(positions[2]) == FALSE) {
    if (positions[2] < 0) { # gets eaten
      state[] = 0
      state[positions[2]] = 1
    } else {
      state[positions[2]] = 0
    }
  }
  
  probs_salinity = dnorm(readings[1],  mean = probs$salinity[,1], sd = probs$salinity[,2])
  probs_phosphate = dnorm(readings[2], mean = probs$phosphate[,1], sd = probs$phosphate[,2])
  probs_nitrogen = dnorm(readings[3],  mean = probs$nitrogen[,1], sd = probs$nitrogen[,2])
  
  # calculating the emission matrix
  emission_matrix = diag(probs_salinity * probs_phosphate * probs_nitrogen)
  
  # probabilityies of crocs position next turn 
  state = emission_matrix %*% state
  
  # normalise state
  state = state / sum(state)
  
  node_to_move_to = which.max(state) # most likely next round
  path = c(0)
  if (node_to_move_to != player_pos) {
    path = findPath(player_pos, node_to_move_to, connections)
  } else {
    order = order(state, decreasing = TRUE)
    node_to_move_to_plan_b = order[2]
    path = append(node_to_move_to, findPath(player_pos, node_to_move_to_plan_b, connections))
  }
  ranks = rank(state)
  if((ranks[player_pos] > (length(ranks) - checkIfInTop)) & (ranks[path[1]] < ranks[player_pos] )) {
    moveInfo$moves = c(0, path[1])
    state[player_pos] = 0
  } else if(ranks[path[1]] > (length(ranks) - checkIfInTop)){
    moveInfo$moves = c(path[1], 0)
    state[path[1]] = 0
  } else {
    moveInfo$moves = c(path[1], path[2])
  }
  
  state = transition_matrix %*% state

  moveInfo$mem$state = state
  return(moveInfo)
}

findPath <- function(start, goal, matrix) {
  nodesNumber = length(matrix[1,])
  q = c(start)
  visited = rep(F, nodesNumber)
  visited[start] = T
  prev = rep(NULL, nodesNumber)
  
  while(length(q) > 0) {
    current = q[1]
    q = q[-1]
    
    if (current == goal) {
      path = c(goal)
      node = prev[goal]
      while (!is.na(prev[node])) {
        path = c(node, path)
        node = prev[node]
      }
      return(path)
    }
    
    neighbours = which(matrix[current,] > 0)
    
    for (neighbour in neighbours) {
      if (!visited[neighbour]) {
        q = c(q, neighbour)
        visited[neighbour] = T
        prev[neighbour] = current
      }
    }
  }
  return(NA)
}

#runWheresCroc(myFunction, showCroc = TRUE)
testWC(myFunction, verbose = 1)
#runWheresCroc(WheresCroc::manualWC)
