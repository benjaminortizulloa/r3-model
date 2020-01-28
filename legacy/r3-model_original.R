library(tidyverse)
library(igraph)

addictionSystem <- function(el, nl){ 
  purrr::pmap_df(el, function(from, to, fromStabilized, toType, rate){
    randomNum <- runif(1)
    
    #relaps rate
    if(toType == 'postRecovery'){
      return(tibble::tibble(
        from = from,
        to = ifelse(randomNum < rate | !fromStabilized, sample(nl[nl$type=='rehab',]$id, 1), to),
        fromStabilized = !(randomNum < rate | !fromStabilized),
        toType = nl[nl$id == to, ]$type,
        rate = nl[nl$id == to, ]$rate
      ))
    }
    
    #stabilizing rate
    if(toType == 'rehab'){
      return(tibble::tibble(
        from = from,
        to = sample(nl[nl$type == 'postRecovery',]$id, 1),
        fromStabilized = randomNum < rate,
        toType = nl[nl$id == to,]$type,
        rate = nl[nl$id == to,]$rate
      ))
    }
  })}

set.seed(4321)
people <- tibble::tibble(
  id = paste0('person_', 1:1000),
  stable = F,
  type = 'person'
)

rehab <- tibble::tibble(
  id = paste0('rehab_', 1:20),
  rate = runif(20, .2, .8), #chance of stabilizing
  type = 'rehab'
)

#create non-rehab facilities
outRehab <- tibble::tibble(
  id = c('nothing', 'sober house'),
  rate = c(.8, .2), #chance of relapse
  type = 'postRecovery'
)

nl <- dplyr::bind_rows(people, rehab, outRehab)

el <- tibble::tibble(
  from = people$id,
  to = 'nothing',
  fromStabilized = F,
  toType = 'postRecovery',
  rate = outRehab[outRehab$id == 'nothing',]$rate
)


#null model
set.seed(4321)
noIntervention <- purrr::map(1:2, function(i){
  g <<- igraph::graph_from_data_frame(el, T, nl)
  purrr::map(setNames(0:10, 0:10), function(j){
    el2 <- addictionSystem(as_data_frame(g), nl)
    g <<- igraph::graph_from_data_frame(el2, T, nl)
    tibble(
      nStable = sum(el2$fromStabilized),
      nTotal = nrow(el2),
      pStable = nStable/nTotal,
      pPostRecovery = sum(el2$toType == 'postRecovery')/nTotal,
      pRehab = sum(el2$toType == 'rehab')/nTotal,
      pSoberHouse = sum(el2$to == 'sober house')/nTotal,
      pNothing = sum(el2$to == 'nothing')/nTotal,
      g = list(g)
    )
  }) %>%
    tibble::enframe(name = 'time') %>%
    tidyr::unnest() %>%
    dplyr::mutate(iteration = i)
}) %>%
  tibble::enframe(name = 'iteration') %>% 
  tidyr::unnest()

write_rds(noIntervention, 'noRehabIntervention.rds')

#removal intervention
set.seed(43211)
withIntervention <- map(1:20, function(i){
  g <<- graph_from_data_frame(el, T, nl)
  nl2 <<- nl
  
  map(setNames(0:100, 0:100), function(j){
    el1 <<- as_data_frame(g) 
    
    if(!(j %% 10 | j == 0)){
      minRehabRate <<- min(nl2[nl2$type == "rehab",]$rate)
      worstRehab <<- nl2[nl2$type == 'rehab' & nl2$rate == minRehabRate,]$id
      nl2 <<-  nl2[nl2$id != worstRehab,]
      tempPeople <<- el1[el1$to == worstRehab,]$from
      if(length(tempPeople) > 0){
        el1[el1$to == worstRehab,]$toType <<- 'postRecovery'
        el1[el1$to == worstRehab,]$rate <<- nl[nl$id == 'nothing',]$rate
        el1[el1$to == worstRehab,]$to <<- 'nothing'
      }
    }
    
    el2 <<- addictionSystem(as_data_frame(g), nl2)
    g <<- graph_from_data_frame(el2, T, nl2)
    
    tibble(
      nStable = sum(el2$fromStabilized),
      nTotal = nrow(el2),
      pStable = nStable/nTotal,
      pPostRecovery = sum(el2$toType == 'postRecovery')/nTotal,
      pRehab = sum(el2$toType == 'rehab')/nTotal,
      pSoberHouse = sum(el2$to == 'sober house')/nTotal,
      pNothing = sum(el2$to == 'nothing')/nTotal,
      g = list(g)
    )
  }) %>%
    enframe(name = 'time') %>%
    unnest()
}) %>%
  enframe(name = 'iteration') %>% 
  unnest()

write_rds(withIntervention, 'withRehabIntervention.rds')

#replacement game
set.seed(4321)
withReplacement <- map(1:20, function(i){
  g <<- graph_from_data_frame(el, T, nl)
  nl2 <<- nl
  
  map(setNames(0:100, 0:100), function(j){
    el1 <<- as_data_frame(g) 
    
    if(!(j %% 10 | j == 0)){
      minRehabRate <<- min(nl2[nl2$type == "rehab",]$rate)
      maxRehabRate <<- max(nl2[nl2$type == "rehab",]$rate)
      worstRehab <<- nl2[nl2$type == 'rehab' & nl2$rate == minRehabRate,]$id
      
      nl2 <<-  nl2[nl2$id != worstRehab,]
      nl2 <<- rbind(nl2, tibble(
        id = paste0('rehab_t_', j),
        stable = NA,
        type = 'rehab',
        rate = runif(1, minRehabRate, maxRehabRate)
      ))
      
      tempPeople <<- el1[el1$to == worstRehab,]$from
      if(length(tempPeople) > 0){
        el1[el1$to == worstRehab,]$toType <<- 'postRecovery'
        el1[el1$to == worstRehab,]$rate <<- nl[nl$id == 'nothing',]$rate
        el1[el1$to == worstRehab,]$to <<- 'nothing'
      }
    }
    
    el2 <<- addictionSystem(as_data_frame(g), nl2)
    g <<- graph_from_data_frame(el2, T, nl2)
    
    tibble(
      nStable = sum(el2$fromStabilized),
      nTotal = nrow(el2),
      pStable = nStable/nTotal,
      pPostRecovery = sum(el2$toType == 'postRecovery')/nTotal,
      pRehab = sum(el2$toType == 'rehab')/nTotal,
      pSoberHouse = sum(el2$to == 'sober house')/nTotal,
      pNothing = sum(el2$to == 'nothing')/nTotal,
      g = list(g)
    )
  }) %>%
    enframe(name = 'time') %>%
    unnest()
}) %>%
  enframe(name = 'iteration') %>% 
  unnest()

write_rds(withReplacement, 'withRehabReplacement.rds')

together <- bind_rows(list(
  noIntervention %>%
    mutate(type = 'No Changes'),
  withIntervention %>%
    mutate(type = "Removing Worst Rehab Every 10 Steps"),
  withReplacement %>%
    mutate(type = 'Replacing Worst Rehab Every 10 Steps')
)) %>%
  mutate(time = as.numeric(time))

#image 1
together %>%
  select(time, pStable, iteration, pSoberHouse, type) %>%
  gather(measure, percent, - time, -iteration, -type) %>%
  mutate(iteration = paste0(iteration, measure))%>%
  ggplot(aes(time, percent, color = measure)) +
  geom_line(aes(group = iteration), alpha = .1) + 
  scale_y_continuous(limits = c(0, .7)) +
  facet_wrap(~type, ncol = 1)+
  theme_bw() +
  labs(
    title = "Readmission-Relapse Cycle",
    subtitle = expression(symbol(N)[Agents] * " = 1000, " * symbol(N)[Iterations] * " = 20, " * symbol(N)[Rehabs] * " = 20, " * alpha[Rehabs] * ' = 20%-80%, ' * beta[Nothing] *' = 80%, ' * beta[SoberHouse] *' = 20%'),
    y = '%',
    x = expression(tau),
    color = '',
    alpha = ''
  )  +
  scale_color_manual(values = c('pSoberHouse' = 'blue', 'pStable' = "black"), labels = c('In Sober House', 'Stable (not-using)'))+
  guides(color = guide_legend(override.aes = list(size = 5, alpha = .5)))
dev.off()

#image 2
png('r3_cycle_initial_results.png', 1100, 800)
together %>%
  select(time, pStable, iteration, pSoberHouse, type) %>%
  gather(measure, percent, - time, -iteration, -type) %>%
  mutate(iteration = paste0(iteration, measure)) %>%
  mutate(measure = map_chr(measure, function(x){
    if(x == 'pSoberHouse') return("% Population with Continued Support System")
    if(x == 'pStable') return("% Population in Stable Condition (not using opiods)")
  })) %>%
  group_by(type, measure, time) %>%
  summarize(mean = mean(percent)) %>%
  ggplot(aes(time, mean, color = type)) +
  geom_line(alpha = .4, size = 1.1) +
  facet_wrap(~measure, ncol = 1) +
  scale_color_manual(values = c("Replacing Worst Rehab Every 10 Steps" = "#1b9e77",
                                "Removing Worst Rehab Every 10 Steps" = '#d95f02',
                                "No Changes" = '#e7298a'))  +
  guides(color = guide_legend(override.aes = list(size = 5, alpha = .5))) +
  theme_bw() +
  labs(
    title = "Readmission-Relapse Cycle (Mean)",
    subtitle = expression(symbol(N)[Agents] * " = 1000, " * symbol(N)[Iterations] * " = 20, " * symbol(N)[Rehabs] * " = 20, " * alpha[Rehabs] * ' = 20%-80%, ' * beta[Nothing] *' = 80%, ' * beta[SoberHouse] *' = 20%'),
    y = 'Mean %',
    x = expression(tau),
    color = '',
    alpha = ''
  ) 
dev.off()
