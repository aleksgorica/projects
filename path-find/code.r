library(GA)
maze3 = c("##########",
          "#S.......#",
          "########.#",
          "#........#",
          "#.########",
          "#........#",
          "########.#",
          "#........#",
          "#.########",
          "#E########")
testMaze = c("#######",
             "#RRRLL#",
             "#U#####",
             "#U#####",
             "#E#####",
             "#######",
             "#######")


convertDirToBits = function(dir){
  if (dir == "R") return(c(0,0))
  if (dir == "U") return(c(0,1))
  if (dir == "L") return(c(1,0))
  if (dir == "D") return(c(1,1))
}

getDir = function(bit1, bit2)
{
  first = 2 * bit1
  second = bit2
  dec = first + second
  if (dec == 0) return("R")
  if (dec == 1) return("U")
  if (dec == 2) return("L")
  if (dec == 3) return("D")
}



  printMap <- function(map, height, width){
  for (i in 1:height){
    print(paste(map[[i]], collapse=""))
  }
}
  


visited = function(point, li)
{
  if (length(li) > 0){
    for (i in 1:length(li)){
      
      if ((li[[i]]$i == point$i) && (li[[i]]$j == point$j)){
        return(T)
      }
    }
  }
  return(F)
}

getPosToBitIndex <- function(map, height, width, endPos){
  count <- 1
  m <- matrix(0, height, width)
  for (i in 1:height){
    for (j in 1:width){
      c <- map[[i]][j]
      if (c == "." || c == "S"){
        m[i,j] <- count
        count <- count + 2
      }
      if (c == "T" || c == "E"){
        if (!(endPos$i == i && endPos$j == j)){
          m[i,j] <- count
          count <- count + 2
        }
      }
      
    }
  }
  return(m)
}



  # startPos: list(i,j)
  # goalPos: list(i,j)
  # binmap: c(list(dir, dir, ...), ..., list(dir, dir))








solve = function(maze){
  BIG_NUM <- 100000
  SAMPLE_SIZE = 2
  mazeli = list()
  NBITS = 2*(1 + sum(lengths(regmatches(maze, gregexpr("\\.", maze)))) + sum(lengths(regmatches(maze, gregexpr("T", maze)))))
  for (i in 1:length(maze)){
    mazeli[[i]] <- c(unlist(strsplit(maze[i], "")))
  }
  width = length(mazeli[[1]])
  height = length(mazeli)
  N = width * height * 2
  
  #finding start, end and treasure
  start_pos <- 0
  end_pos <- 0
  treasures <- list()
  for (i in 1:height){
    for (j in 1:width){
      if (mazeli[[i]][j] == "S"){
        start_pos <- list(i=i, j=j)
      }
      if (mazeli[[i]][j] == "E"){
        end_pos <- list(i=i, j=j)
      }
      if (mazeli[[i]][j] == "T"){
        treasures[[length(treasures) + 1]] <- list(i=i, j=j)
      }
    }
  }

  
  findValidDir <- function(pos, map)
  {
    valid = c()
    i <- pos$i+1
    j <- pos$j
    if (!(j < 1 || j > width || i < 1 || i > height || (map[[i]][j] == "#"))){
      valid <- append(valid, "D")
    } 
    i <- pos$i
    j <- pos$j + 1
    if (!(j < 1 || j > width || i < 1 || i > height || (map[[i]][j] == "#"))){
      valid <- append(valid, "R")
    } 
    i <- pos$i - 1
    j <- pos$j
    if (!(j < 1 || j > width || i < 1 || i > height || (map[[i]][j] == "#"))){
      valid <- append(valid, "U")
    } 
    i <- pos$i
    j <- pos$j - 1
    if (!(j < 1 || j > width || i < 1 || i > height || (map[[i]][j] == "#"))){
      valid <- append(valid, "L")
    } 
    return(valid)
  }
  
  
  changeOrientation = function(position, dirmap){
    pos = position
    changes = list()
    count = 1
    
    while (TRUE){
      currentDir <- dirmap[[pos$i]][pos$j]
      validDir <- findValidDir(pos, dirmap)
      dir <- ""
      found <- FALSE
      while (TRUE){
        if (length(validDir) == 0) break
        dir <- sample(validDir, size=1)[1]
        if (dir != currentDir) {
          prevPos <- move(pos, dir, dirmap)
          prevPosNext <- move(prevPos, dirmap[[prevPos$i]][prevPos$j],dirmap)
          if (prevPosNext$i == pos$i && prevPosNext$j == pos$j) {
            found <- TRUE
            changes[[count]] = list(i=pos$i,j=pos$j, dir=dir)
            count <- count + 1
            pos <- move(pos, dir, dirmap)
            break
          } else {
            validDir <- validDir[!validDir == dir]
          } 
        } else {
          validDir <- validDir[!validDir == dir]
        } 
        
      }
      newValid <- findValidDir(pos, dirmap)
      if (length(newValid) == 1) break
      if (!found) {
        newDir <- sample(newValid[!newValid == currentDir], size=1)[1]
        changes[[count]] <- list(i=pos$i, j=pos$j, dir=newDir)
        
        break
      }
      
      
    }
    return(changes)
  }
  
  evalutateCase = function(startPos, goalPos, map)
  {
    dist <- 0
    pos = list(i=startPos$i, j=startPos$j)
    visited = list()
    steps = 1
    DIST_KOEF = 100
    
    while (TRUE){
      dir = map[[pos$i]][pos$j]
      visited[[(steps+1)/2]] = list(i=pos$i, j=pos$j)

      if (dir == "R") {
        pos$j <- pos$j + 1
      }
      if (dir == "D") {
        pos$i <- pos$i + 1
      }
      if (dir == "L") {
        pos$j <- pos$j - 1
      }
      if (dir == "U") {
        pos$i <- pos$i - 1
      }
      #ce si sel ven iz okvirjev
      if (pos$j < 1 || pos$j > width || pos$i < 1 || pos$i > height || map[[pos$i]][pos$j] == "#"){
        return(steps - BIG_NUM)
      }
      
      #smo prisli do konca
      if (pos$i == goalPos$i && pos$j == goalPos$j){
        return(BIG_NUM - steps)
      }
      
      #smo ze bili tu
      if (visited(pos, visited)){
        return(steps - BIG_NUM)
      }
      steps <- steps + 1
    }
    
    return (steps - BIG_NUM)
  }
  
  move <- function(startPos, dir, map){
    pos = list(i=startPos$i, j=startPos$j)
    if (dir == "R") {
      pos$j <- pos$j + 1
    }
    if (dir == "D") {
      pos$i <- pos$i + 1
    }
    if (dir == "L") {
      pos$j <- pos$j - 1
    }
    if (dir == "U") {
      pos$i <- pos$i - 1
    }
    return(pos)
  }
  
  convertToOrders <- function(directionMap, startPos, endPos)
  {
    pos = list(i=startPos$i, j=startPos$j)
    instructions = c()
    #print(tour)
    count = 1
    while(TRUE){
      dir = directionMap[[pos$i]][pos$j]
      if (dir == "R") {
        pos$j <- pos$j + 1
        instructions[count] <- "R"
      }
      if (dir == "D") {
        pos$i <- pos$i + 1
        instructions[count] <- "D"
      }
      if (dir == "L") {
        pos$j <- pos$j - 1
        instructions[count] <- "L"
      }
      if (dir == "U") {
        pos$i <- pos$i - 1
        instructions[count] <- "U"
      }
      
      if (pos$j < 1 || pos$j > width || pos$i < 1 || pos$i > height || mazeli[[pos$i]][pos$j] == "#"){
        instructions <- append(instructions, "I")
        return(instructions)
      }
      
      if (pos$i == endPos$i && pos$j == endPos$j){
        instructions <- append(instructions, "V")
        return(instructions)
      }
      count <- count + 1
      if (count > width * height) break
    }
    instructions <- append(instructions, "N")
    return (instructions)
  }
  
  
  
  getValidPos = function(goalPos){
    valid <- list()
    count = 1
    for (i in 1:height){
      for (j in 1:width){
        if (mazeli[[i]][j] == "T"){
          if (!(goalPos$i == i && goalPos$j == j)){
            valid[[count]] <- list(i=i, j=j)
            count <- count + 1
          }
        }
        if (mazeli[[i]][j] == "."){
          valid[[count]] <- list(i=i, j=j)
          count <- count + 1
        }
      }
    }
    return(valid)
  }
  
  convertBinMap = function(bin_directions, goalPos)
  {
    directionMap <- list()
    dir_count = 1
    for (i in 1:height){
      row = c()
      for (j in 1:width){
        if (mazeli[[i]][j] == "#"){
          row[j] = "#"
        }
        if (mazeli[[i]][j] == "S"){
          row[j] = getDir(bin_directions[dir_count], bin_directions[dir_count+1])
          dir_count <- dir_count + 2
        }
        
        if (mazeli[[i]][j] == "T" || mazeli[[i]][j] == "E"){
          if (goalPos$i == i && goalPos$j == j){
            row[j] = "T"
          } else {
            row[j] = getDir(bin_directions[dir_count], bin_directions[dir_count+1])
            dir_count <- dir_count + 2
          }
        }
        if (mazeli[[i]][j] == "."){
          row[j] = getDir(bin_directions[dir_count], bin_directions[dir_count+1])
          dir_count <- dir_count + 2
        }
      }
      directionMap[[i]] = row
    }
    return(directionMap)
    
  }
  findError <- function(dirmap)
  {
    wrongPoint <- list()
    nWrong <- 1
    counters <- list()
    nCounters <- 1
    
    for (i in 1:height){
      for (j in 1:width){
        order <- dirmap[[i]][j]
        if (order == "U"){
          if (i == 1){
            wrongPoint[[nWrong]] <- list(i=i, j=j)
            nWrong <- nWrong + 1
          } else if (dirmap[[i-1]][j] == "#"){
            wrongPoint[[nWrong]] <- list(i=i, j=j)
            nWrong <- nWrong + 1
          }
        }
        if (order == "D"){
          if (i < height && dirmap[[i+1]][j] == "U"){
            counters[[nCounters]] <- list(list(i=i,j=j), list(i=i+1, j=j))
            nCounters <- nCounters + 1
          }
          if (i == height){
            wrongPoint[[nWrong]] <- list(i=i, j=j)
            nWrong <- nWrong + 1
          } else if (dirmap[[i+1]][j] == "#"){
            wrongPoint[[nWrong]] <- list(i=i, j=j)
            nWrong <- nWrong + 1
          }
        }
        if (order == "R"){
          if (j < width && dirmap[[i]][j+1] == "L"){
            counters[[nCounters]] <- list(list(i=i, j=j), list(i=i, j=j+1))
            nCounters <- nCounters + 1
          }
          if (j == width){
            wrongPoint[[nWrong]] <- list(i=i, j=j)
            nWrong <- nWrong + 1
          } else if (dirmap[[i]][j+1] == "#"){
            wrongPoint[[nWrong]] <- list(i=i, j=j)
            nWrong <- nWrong + 1
          }
        }
        if (order == "L")
        {
          if (j == 1){
            wrongPoint[[nWrong]] <- list(i=i, j=j)
            nWrong <- nWrong + 1
          } else if (dirmap[[i]][j-1] == "#"){
            wrongPoint[[nWrong]] <- list(i=i, j=j)
            nWrong <- nWrong + 1
          }
        }
      }
    }
    return(list(counters=counters, wrongPoints=wrongPoint))
  }
  
  MAPS <- list()
  solveSmall = function(endPos){
    mutate = function(object, parent){
      mutate <- parent <- as.vector(object@population[parent,])
      
      RANDOM_THRESH = 0.6

            randomMutation <- runif(1)
      if (randomMutation < RANDOM_THRESH){
        n <- length(parent)
        change <- sample(1:n, size = 1)
        for (i in 1:length(change)){
          mutate[change[j]] <- abs(mutate[change[j]]-1)
        
        }
      }
      
      map <- convertBinMap(mutate, endPos)
      errors <- findError(map)
      counters <- errors$counters
      wrongPoints <- errors$wrongPoints
      if (length(wrongPoints) != 0) {
        for (i in 1:length(wrongPoints)){
          pos <- wrongPoints[[i]]
          valid <- findValidDir(pos, map)
          randomDir <- sample(valid, size=1)[1]
          bits <- convertDirToBits(randomDir)
          ind <- posToBitIndex[pos$i,pos$j]
          mutate[ind] <- bits[1]
          mutate[ind+1] <- bits[2]
        }
      }
      if (length(counters) != 0){
        counterPair = sample(counters, size=1)[[1]]
        counterElement = sample(counterPair, size=1)[[1]]
        toChangeOrient = changeOrientation(counterElement, map)
        if (length(toChangeOrient) == 0) return(mutate)
        for (i in 1:length(toChangeOrient)){
          pos <- toChangeOrient[[i]]
          bits <- convertDirToBits(pos$dir)
          ind <- posToBitIndex[pos$i, pos$j]
          mutate[ind] <- bits[1]
          mutate[ind+1] <- bits[2]
        }
      } 
      return(mutate)
    }
    
    fitness = function(bindirmap)
    {
      dirMap = convertBinMap(bindirmap, endPos)
      errors <- findError(dirMap)
      counters <- errors$counters
      wrongPoints <- errors$wrongPoints
      result = 0
      if (length(treasures) != 0){
        for (i in 1:length(treasures)){
          if (treasures[[i]]$i != endPos$i || treasures[[i]]$j != endPos$j){
            result <- result + evalutateCase(treasures[[i]], endPos, dirMap)
          }
        }
      }
      for (i in 1:length(samplePos)){
        result <- result + 100 * evalutateCase(samplePos[[i]], endPos, dirMap)
      }
      result <- result + 100 * evalutateCase(start_pos, endPos, dirMap)
      result <- result - 100 * length(counters)
      result <- result - 100 * length(wrongPoints)
      return(result)
    }
    
    potPos = getValidPos(endPos)
    samplePos = sample(potPos, size=SAMPLE_SIZE)
    posToBitIndex <- getPosToBitIndex(mazeli, height, width, endPos)
    GA3 <- ga(type = "binary", fitness = fitness, nBits = NBITS, maxiter = 1000,  run = 100, popSize = 40, mutation=mutate, pmutation=0.8)
    sol = GA3@solution
    convert = convertBinMap(sol[1,], endPos)
    print(sol[1,])
    print(posToBitIndex)
    printMap(convert, height, width)
    return(convert)
  }
  
  

  MAPS[[1]] <- solveSmall(end_pos)
  if (length(treasures) != 0){
    for (i in 1:length(treasures)){
      print(treasures[[i]])
      MAPS[[1+i]] <- solveSmall(treasures[[i]])
    }
  }
  return(list(maps=MAPS, end=end_pos, treasures=treasures, start=start_pos, origMaze=mazeli))
  
}

solMaze = solve(maze4_T)

dist2points = function(startPos, goalPos, map)
{
  width = length(map[[1]])
  height = length(map)
  dist <- 0
  pos = list(i=startPos$i, j=startPos$j)
  visited = list()
  steps = 1

  while (TRUE){
    dir = map[[pos$i]][pos$j]
    
    visited[[(steps+1)/2]] = list(i=pos$i, j=pos$j)
    if (dir == "R") {
      pos$j <- pos$j + 1
    }
  
    if (dir == "D") {
      
      pos$i <- pos$i + 1
    }
    if (dir == "L") {
      pos$j <- pos$j - 1
    }
    if (dir == "U") {
      pos$i <- pos$i - 1
    }
    #ce si sel ven iz okvirjev
    
    if (pos$j < 1 || pos$j > width || pos$i < 1 || pos$i > height || map[[pos$i]][pos$j] == "#"){
      return(Inf)
    }
    
    #smo prisli do konca
    
    if (pos$i == goalPos$i && pos$j == goalPos$j){
      return(steps)
    }
    
    #smo ze bili tu
    if (visited(pos, visited)){
      return(Inf)
    }
    steps <- steps + 1
  }
  return (Inf)
}
calculateDists = function(MAZES, start, end, treasures){
  dists = list()
  distances = matrix(0, length(treasures) + 1, length(treasures) + 2)
  
  if (length(treasures) > 0) {
    distances[length(treasures)+1,length(treasures)+2] = dist2points(start, end, MAZES[[1]])
    for(j in 1:length(treasures)){
      distances[j, length(treasures)+1] = dist2points(end, treasures[[j]], MAZES[[j+1]])
      distances[length(treasures)+1,j] = dist2points(treasures[[j]], end, MAZES[[1]])
    }
    for(i in 1:length(treasures)){
      distances[i, length(treasures)+2] = dist2points(start, treasures[[i]], MAZES[[i+1]])
      for(j in 1:length(treasures)){
        if (i == j) next
        distances[i, j] = dist2points(treasures[[j]], treasures[[i]], MAZES[[i+1]])
      }
    }
    return(distances)
  }
}

minimizeDistanceMatrix = function(distances, n){
  mindist = distances
  i = 1
  while (i <= n+1){
    j = i+1
    while (j<= n+1){
      m = min(distances[i, j], distances[j,i])
      mindist[i,j]= m
      mindist[j,i]= m
      j = j + 1
    }
    i = i + 1
    
  }
  return(mindist)
}

distances = calculateDists(solMaze$maps, solMaze$start, solMaze$end, solMaze$treasures)
min_distances = minimizeDistanceMatrix(distances, length(solMaze$treasures))


findMinDist <- function(perm, farness) {
  minDist = Inf
  permIndex = 2
  if (length(perm) > 0) {
    for (i in 1:length(perm)){
      dist = 0
      N = length(perm[[1]])
      currPerm = perm[[i]]
      dist <- dist + farness[currPerm[1], N+2]
      for (j in 2:N){
        from = currPerm[j-1]
        to = currPerm[j]
        dist <- dist + farness[to, from]
      }
      dist <- dist + farness[N+1, currPerm[N]]
  
      
      cat(permIndex, minDist, i, dist)
      
      if (dist < minDist){
        permIndex = i
        minDist = dist
      }
    }
  }
  return(perm[[permIndex]])
  
}

#from
# end - length(treasures) + 1
# start - length(treasures) + 2
# treasures - treasure_index

#to
#end - 1
#treasures - treasure_index



TSP = function(tour){
  tourLength <- function(tour)
  {
    N <- length(tour)
    for (i in 1:length(tour)){
      dist = 0
      dist <- dist + min_distances[tour[1], N+2]
      for (j in 2:N){
        from = tour[j-1]
        to = tour[j]
        dist <- dist + min_distances[to, from]
      }
      dist <- dist + min_distances[N+1, tour[N]]
    }
    return(dist)
  }
    
    
    
   
  return(1/tourLength(tour))
}

if (length(sol$treasures) >= 4){
GA <- ga(type = "permutation", fitness = TSP, lower = 1, upper = length(solMaze$treasures), popSize = factorial(length(solMaze$treasures)+1), maxiter = 100, run = 50, pmutation = 0.2)
}
printResult = function(origMaze, MAZES, sequence, starts, ends){
  for (i in 1:length(sequence)){
    maze = MAZES[[abs(sequence[i])]]
    start = starts[[i]]
    reverse=FALSE
    if (sequence[i] < 0){
      reverse=TRUE
    }
    #clean maze
    for (y in 1:length(origMaze)){
      for (x in 1:length(origMaze[[y]])){
        c = origMaze[[y]][x]
        if (c != "#"){
          origMaze[[y]][x] = " "
        } 
      }
    }
    pos = start
    while (TRUE){
      dir = maze[[pos$i]][pos$j]
      if (dir == "R") {
        origMaze[[pos$i]][pos$j] = "R"
        
        pos$j <- pos$j + 1
      }
      if (dir == "D") {
        origMaze[[pos$i]][pos$j] = "D"
        
        pos$i <- pos$i + 1
      }
      if (dir == "L") {
        origMaze[[pos$i]][pos$j] = "L"
        pos$j <- pos$j - 1
      }
      if (dir == "U") {
        origMaze[[pos$i]][pos$j] = "U"
        
        pos$i <- pos$i - 1
      }
      if (pos$i == ends[[i]]$i && pos$j == ends[[i]]$j){
        if (reverse == FALSE){
          origMaze[[pos$i]][pos$j] = "X"
          printMap(origMaze, length(origMaze), length(origMaze[[1]]))
        }
        print(" ")
        break
      }
    }
    visited = list()
    steps = 1
    if (reverse == TRUE){
      while (TRUE){
        visited[[steps]] = list(i=pos$i, j=pos$j)
        nex = list()
        
        if (origMaze[[pos$i]][pos$j+1] != "#" && origMaze[[pos$i]][pos$j+1] != " "){
          t = list(i=pos$i, j=pos$j+1)
          if (!visited(t, visited)){
            origMaze[[pos$i]][pos$j] = "R"
            nex = t

          }
        }
        if (origMaze[[pos$i]][pos$j-1] != "#" && origMaze[[pos$i]][pos$j-1] != " "){
          t = list(i=pos$i, j=pos$j-1)
          if (!visited(t, visited)){
            origMaze[[pos$i]][pos$j] = "L"
            nex = t
          }
        }
        
        if (origMaze[[pos$i+1]][pos$j] != "#" && origMaze[[pos$i+1]][pos$j] != " "){
          t = list(i=pos$i+1, j=pos$j)
          if (!visited(t, visited)){
            origMaze[[pos$i]][pos$j] = "D"
            nex = t
          }
        }
        if (origMaze[[pos$i-1]][pos$j] != "#" && origMaze[[pos$i-1]][pos$j] != " "){
          t = list(i=pos$i-1, j=pos$j)
          if (!visited(t, visited)){
            origMaze[[pos$i]][pos$j] = "U"
            nex = t
          }
        }
        steps <- steps + 1
        #print(visited)
        pos <- list(i=nex$i, j=nex$j)
        if (pos$i == starts[[i]]$i && pos$j == starts[[i]]$j){
          origMaze[[pos$i]][pos$j] = "X"
          printMap(origMaze, length(origMaze), length(origMaze[[1]]))
          print(" ")
          break
        }
      }
      
    }
  }
}

simulatePath <- function(origMaze, MAZES, perm, distances, min_distances, treasures, startPos, endPos){
  N <- length(perm)
  sequence <- c()
  start <- list()
  end <- list()
  start[[1]] <- startPos
  end[[1]] <- treasures[[perm[1]]]
  sequence[1] <- perm[1]+1
  for(i in 2:N){
    to = perm[i]
    from = perm[i-1]
    if (distances[to, from] > min_distances[to, from]){
      sequence[i] = -1 * (from + 1)
      start[[i]] = treasures[[to]]
      end[[i]] = treasures[[from]]
    } else {
      sequence[i] = to + 1
      end[[i]] = treasures[[to]]
      start[[i]] = treasures[[from]]
    }
  }
  if (distances[N+1, perm[N]] > min_distances[N+1, perm[N]]){
    sequence[N+1] = -1 * (N + 1)
    start[[N+1]] = endPos
    end[[N+1]] = treasures[[perm[N]]]
  } else {
    sequence[N+1] = 1
    start[[N+1]] = treasures[[perm[N]]]
    end[[N+1]] = endPos
  }

  printResult(solMaze$origMaze, MAZES, sequence, start, end)
}

finalSol = function(solMaze){
  if (length(solMaze$treasures) == 0){
    print("no treassures")
    printResult(solMaze$origMaze, solMaze$maps, c(1), list(solMaze$start), list(solMaze$end))
  } else {
    print("treasures")
    perm = permn(length(solMaze$treasures))
    mD = findMinDist(perm, min_distances)
    simulatePath(solMaze$origMaze, solMaze$maps, mD, distances, min_distances, solMaze$treasures, solMaze$start, solMaze$end)
  }
}
finalSol(solMaze)
