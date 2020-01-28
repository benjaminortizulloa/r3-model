#Initialize all node/actors in system 
#outputs a list of node lists
#npeople : number of people in system
#nRehab : number of rehabs in system
#nPostRehab : number of locations people can go to after rehab
#rateRehab : if not NULL, probability of recovery (0-1), must be a vector of size nRehab
#ratePostRehab : if not NULL, probability of relapse (0-1), must be a vector of size nPostRehab
initializeNodes <- function(nPeople, nRehab, nPostRehab, rateRehab = NULL, ratePostRehab = NULL){
  #create random recovery/stabilizing rate for rehab facility
  if(is.null(rateRehab)){
    rateRehab <- runif(nRehab)
  }
  
  #create random relapse rate for postRehab facilities
  if(is.null(ratePostRehab)){
    ratePostRehab <- runif(nPostRehab)
  }
  
  people <- tibble::tibble(
    id = paste0('person_', 1:nPeople), #person id
    stable = sample(c(T, F), nPeople, T), #are they currently using drugs
    type = 'person'
  )
  
  rehab <- tibble::tibble(
    id = paste0('rehab_', 1:nRehab), #rehab's id
    rate = rateRehab, #chance of stabilizing patients
    type = 'rehab'
  )
  
  #create non-rehab facilities
  postRehab <- tibble::tibble(
    id = paste0('postRehab_', 1:nPostRehab),
    rate = ratePostRehab, #chance of relapse
    type = 'postRehab'
  )
  
  list(
    people = people,
    rehab = rehab,
    postRehab = postRehab
  )
}

#Initialize the edges (Where are people currently located)
#outputs an edgelist
#nodeList : output of `initializeNodes`
initializeEdges <- function(nodeList){
  el <- tibble::tibble(
    from = nodeList$people$id
  )
  
  #randomly assign a postRehab location for patients to start at
  assignedPR <- dplyr::sample_n(nodeList$postRehab, nrow(el), T)
  assignedPR <- dplyr::rename(assignedPR, to = id, toType = type)
  
  el <- cbind(el, assignedPR)
  el$fromStabilized <- nodeList$people$stable
  
  return(el)
}

#During iteration, potentially relocate people if they are outside of rehab
#people will go to rehab if they relapse
#postRecoveryEdges : edges filtered down to `toType == postRehab`
#rehabNodes : rehab dataframe found in the output of `initializeNodes`
#postRehabNodes : postRehab dataframe found in the output of `initializeNodes`
postRecoveryTurn <- function(postRecoveryEdges, rehabNodes, postRehabNodes){
  if(nrow(postRecoveryEdges) == 0) return(NULL)
  
  #stochastic element to determine how patient will react
  randomNumbers <- runif(nrow(postRecoveryEdges))
  
  #if patient will relapse, select a random  rehab
  randomRehab <- sample(rehabNodes$id, nrow(postRecoveryEdges), T)
  
  #boolean to determine whether or not patient will relapse / return to rehab
  changeNew <- randomNumbers < postRecoveryEdges$rate | !postRecoveryEdges$fromStabilized
  
  #if changeNew == true, then chose the randomRehab of the same index, if not, then stay in same postRecovery location
  to <- ifelse(changeNew, randomRehab, postRecoveryEdges$to)
  
  #combine rehabs and postrehab
  possibleLocations <- rbind(rehabNodes, postRehabNodes)
  
  #get future location info
  futureInfo <- tibble::tibble(id = to)
  futureInfo <- dplyr::left_join(futureInfo, possibleLocations, by = 'id')
  
  tibble::tibble(
    from = postRecoveryEdges$from,
    to = to,
    fromStabilized = !changeNew,
    toType = futureInfo$type,
    rate = futureInfo$rate
  )
}

#During iteration, relocate people if they are in rehab
#people always leave rehab - regardless if recovered/stable or not
#rehabEdges : edges filtered down to `toType == rehab`
#postRehabNoes : postRehab dataframe found in the output of `initializeNodes`
rehabTurn <- function(rehabEdges, postRehabNodes){
  if(nrow(rehabEdges) == 0) return(NULL)
  
  #stochastic element to determine how patient will react
  randomNumbers <- runif(nrow(rehabEdges))
  
  #will the patient be stabilized in the next turn
  fromStabilized = randomNumbers < rehabEdges$rate
  
  #select random postRehab node to allocate patient
  futureInfo <- dplyr::sample_n(postRehabNodes, nrow(rehabEdges), T)
  
  tibble::tibble(
    from = rehabEdges$from,
    to = futureInfo$id,
    fromStabilized = fromStabilized,
    toType = futureInfo$type,
    rate = futureInfo$rate
  )
}

#single turn of the model - rewires people to new locations
#ouputs an updated edgelist
#el : edge list. First iteration should be the output of `initializeEdges`
#rehabNodes : rehab dataframe found in the output of `initializeNodes`
#postRehabNodes : postRehab dataframe found in the output of `initializeNodes`
addictionSystem <- function(el, rehabNodes, postRehabNodes){ 
  rehabEdges <- dplyr::filter(el, toType == 'rehab')
  postRecoveryEdges <- dplyr::filter(el, toType == 'postRehab')
  
  prTurn <- postRecoveryTurn(postRecoveryEdges, rehabNodes, postRehabNodes)
  rTurn <- rehabTurn(rehabEdges, postRehabNodes)
  
  rbind(prTurn, rTurn)
}

#provide simple summary statistics for a simulation turn
#outputs a dataframe with simple statistics, including a representative graph
#el : edge list. First iteration should be theoutput of `initializeEdges` 
#nodeList : output of `initializeNodes`
summaryTable <- function(el, nodeList){
  #create a single dataframe to represent nodelist of network
  nl <- dplyr::bind_rows(nodeList)
  
  #create a graph representation of system
  g <- igraph::graph_from_data_frame(el, T, nl)
  
  tibble::tibble(
    nStable = sum(el$fromStabilized),
    nTotal = nrow(el),
    pStable = nStable/nTotal,
    pPostRecovery = sum(el$toType == 'postRehab')/nTotal,
    pRehab = sum(el$toType == 'rehab')/nTotal,
    g = list(g)
  )
}

#a simulation where people go in and out of rehab
#nothing changes with regards to the locations
#meant to be used to compare with simulations with interventions
#outputs a dataframe with each result stored in its respective row
#edgeList : output of `initializeEdges`
#nodeList : output of `initializeNodes`
#nTime : how many turns are in the simulation
noIntervention <- function(edgeList, nodeList, nTime){
  initialResult <- summaryTable(edgeList, nodeList)

  results <- list(initialResult)

  for(i in 2:nTime){
    #use the graph from the previous turn
    g <- results[[i-1]]$g[[1]]
    
    #obtain the edge list from the previous graph
    el <- igraph::as_data_frame(g)
    
    #run a simulation turn on the edgelist
    el <- addictionSystem(el, nodeList$rehab, nodeList$postRehab)
      
    #obtain summary stats and new graph
    result <- summaryTable(el, nodeList)
    
    #push results in a list we will use later
    results[[i]] <- result
  }
  
  #convert the list into a data frame
  results <- tibble::enframe(results, name = 'time')
  results <- tidyr::unnest(results, value)
  
  return(results)
}

#a simulation where people go in and out of rehab
#worst rehab is replaced with a new one at a given frequency
#outputs a dataframe with each result stored in its respective row
#edgeList : output of `initializeEdges`
#nodeList : output of `initializeNodes`
#nTime : how many turns are in the simulation
#interventionFrequency : an integer `n`. The worst rehab will be replaced on every n turn
replacementIntervention <- function(edgeList, nodeList, nTime, interventionFreq){
  initialResult <- summaryTable(edgeList, nodeList)
  
  results <- list(initialResult)
  
  for(i in 2:nTime){
    #use the graph from the previous turn
    g <- results[[i -1]]$g[[1]]
    
    #obtain the edge list from the previous graph
    el <- igraph::as_data_frame(g)
    
    if(!i %% interventionFreq){
      #identify the worst recovery rate
      minRehabRate <- min(nodeList$rehab$rate)
      
      #identify the bet recovery rate
      maxRehabRate <- max(nodeList$rehab$rate)
      
      #identify the id of rehab with worst recovery rate
      worstRehab <- dplyr::filter(nodeList$rehab, rate == minRehabRate)$id
      worstRehab <- sample(worstRehab, 1)
      
      #replace worst rehab with a new rehab with a random recovery rate between min and max rates
      nodeList$rehab[nodeList$rehab$id == worstRehab, 'rate'] <- runif(1, minRehabRate, maxRehabRate)
    }
    
    #run a simulation turn on the edgelist
    el <- addictionSystem(el, nodeList$rehab, nodeList$postRehab)
    
    result <- summaryTable(el, nodeList)
    results[[i]] <- result
  }
  
  #convert result into a dataframe
  results <- tibble::enframe(results, name = 'time')
  results <- tidyr::unnest(results, value)
  
  return(results)
}

#iterate over simulations to get multiple results
#outputs a data frame combining multiple iterations of a simulation
#nIter : number of iterations to run
#fun : a simulation in the form of a function 'ie. function(){replacementIntervention(yourEdgeList, yourNodeList, 100, 5)}'
iterateSim <- function(nIter, fun){
  results <- list()
  
  for(i in 1:nIter){
    results[[i]] <- fun() 
  }
  
  results <- tibble::enframe(results, name = 'iteration')
  results <- tidyr::unnest(results, value)
  return(results)
}



people2people <- function(g){
  g <- igraph::bipartite_projection(g,
                                    types = igraph::V(g)$type == "person",
                                    which = 'true')
}

removeRandomEdges <- function(g, toRemove = .75){
  edgeCount <- igraph::ecount(g)

  g <- g - igraph::E(g)[sample(1:edgeCount, edgeCount * toRemove)]
}

observeCommunities <- function(g){
  wc <- igraph::cluster_walktrap(g)
  igraph::V(g)$membership <- wc$membership
  purrr::map(1:length(wc), function(i){
    g - igraph::V(g)[membership != i]
  })
}




