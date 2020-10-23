library(DeliveryMan)

create_node <- function(posX, posY, cost = .Machine$integer.max) {
  return(list(x = posX, y = posY, cost = cost))
}

distance <- function (x1, y1, x2, y2){
  return(abs(x1 - x2) + abs(y1 - y2))
}

myFunction <- function(roads, car, packages) {
  # roads-> list of two matrixes for traffic conditions hroads and vroads (coordintes bottom left to top right )
  # coordinates stat <1,1> and go to <dim,dim>
  # list of information about your car -> load -> mem -> nextMove 
  # matrix about packages
  car$nextMove = 5
  
  destX = 1
  destY = 1
  if(car$load == 0) {
    # closest package
    distances = distance(car$x, car$y, packages[,1], packages[,2])
    # set packages we already collected to infinite distance
    distances[packages[, 5] != 0] = .Machine$integer.max 
    minDistanceIndex = which.min(distances)
    destX = packages[minDistanceIndex, 1]
    destY = packages[minDistanceIndex, 2]
    
  } else {
    destX = packages[car$load, 3]
    destY = packages[car$load, 4]   
  }
  path = find_path(car$x, car$y, destX, destY, roads)
  nextPoint = path[[1]]
  car$nextMove = next_move(car$x, car$y, nextPoint$x, nextPoint$y)
  return(car)
}

get_neighbour <- function(posX, posY, cameFromX, cameFromY, goalX, goalY, costToNode) {
  totalCost = costToNode + distance(posX, posY, goalX, goalY)
  toVisit = TRUE
  return(create_node_info_element(posX, posY, cameFromX, cameFromY, costToNode, totalCost, toVisit =TRUE))
}

get_neighbours <- function (posX, posY, goalX, goalY, costToParentNode, roads) {
  maxY = length(roads$hroads[1,])
  maxX = length(roads$vroads[,1])
  
  n1 = NULL
  n2 = NULL
  n3 = NULL
  n4 = NULL
  
  if (posX > 1) {
    cost = roads$hroads[posX-1, posY] + costToParentNode
    n1 = create_node(posX-1,posY, cost)
  }
  if (posX < maxX) {
    cost = roads$hroads[posX, posY] + costToParentNode
    n2 = create_node(posX + 1, posY, cost)
  }
  if (posY > 1) {
    cost = roads$vroads[posX, posY - 1] + costToParentNode
    n3 = create_node(posX,posY - 1, cost)
  }
  if (posY < maxY) {   
    cost = roads$vroads[posX, posY] + costToParentNode
    n4 = create_node(posX,posY + 1, cost)
  }
  return(list(n1, n2, n3, n4))
}

find_path <- function (startX, startY, goalX, goalY, roads) {
  pq_cost = c()
  pq_list = list()
  
  maxX = length(roads$vroads[,1])
  maxY = length(roads$hroads[1,])
  cameFromX <<- matrix(0, nrow = maxX, ncol = maxY)
  cameFromY <<- matrix(0, nrow = maxX, ncol = maxY)
  costToNode <<- matrix(.Machine$double.xmax, nrow = maxX, ncol = maxY)
  visited <<- matrix(FALSE, nrow = maxX, ncol = maxY)
  
  maxY = length(roads$hroads[1,])
  maxX = length(roads$vroads[,1])

  cost = 0
  node = create_node(startX, startY, 0)
  # priority queue insert
  insert.order <- findInterval(cost, pq_cost)
  pq_cost <- append(pq_cost, cost, insert.order)
  pq_list <- append(pq_list, list(node), insert.order)

  
  costToNode[startX, startY] = 0
  cameFromX[startX, startY] = startX
  cameFromY[startX, startY] = startY
  
  current = NULL
  while (length(pq_cost) != 0) {
    # pop queue
    current <- pq_list[[1]]
    pq_cost <- pq_cost[-1]
    pq_list <- pq_list[-1]
    
    if (current$x == goalX & current$y == goalY) {
      path_list = NULL
      # walk backwards until starting element is reached
      while (current$x != startX | current$y != startY) {
        # get next element
        path_list = append(list(current), path_list)
        current = create_node(cameFromX[current$x, current$y], cameFromY[current$x, current$y], costToNode[current$x, current$y])
      }
      return(path_list)
    }
    
    neighbours = get_neighbours(current$x, current$y, goalX, goalY, costToNode[current$x, current$y], roads)
    
    for(n in neighbours) {
      if(is.null(n) == FALSE) {
        if(n$cost < costToNode[n$x, n$y]) {
          cameFromX[n$x,n$y] = current$x
          cameFromY[n$x,n$y] = current$y
          
          costToNode[n$x, n$y] = n$cost
          
          totalCost = n$cost + distance(n$x, n$y, goalX, goalY)
          
          # priority queue insert
          insert.order <- findInterval(totalCost, pq_cost)
          pq_cost <- append(pq_cost, totalCost, insert.order)
          pq_list <- append(pq_list, list(n), insert.order)
        }
      }
    }
    
  }
  #Failed to find path
  return(NULL)
}

next_move <- function(carX, carY, dirX, dirY) {
  if(!is.numeric(dirX) || !is.numeric(dirY)) {
    return(0)
  }
  xDir = carX - dirX
  yDir = carY - dirY
  
  if (xDir + yDir < 0) {
    if (xDir == 0) {
      # up
      return(8)
    } else {
      # right
      return(6)
    }
  } else if (xDir + yDir > 0){
    if (xDir == 0) {
      # down
      return(2)
    } else {
      # left
      return(4)
    }
  }
  return(0)
}

startTime = Sys.time()
print(testDM(myFunction, timeLimit = 250))
endTime = Sys.time()
print(endTime - startTime)
