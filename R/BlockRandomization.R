#' TODO: this version is attempting to approach randomization by 
#' randomizing and then checking against things, re-randomizing as
#' necessary.  BUT, what if we did a model where we do a grouping
#' (with group_by) for some arbitrary set of characteristics, then 
#' randomize on those subgroups?  That's the obvious solution for 
#' factor variables, but if a factor is correlated with some more 
#' continuous characteristic (race and SES), then you're creating
#' a challenge down the road for yourself.
#' 
#' Assuming that isn't the case, though, you can work with a scenario
#' where you factor block-randomize, then within-group check for 
#' balance on any number of other characteristics.  
#' 
#' What you could then do is collect a distribution of the imbalances
#' of particular randomizations, to see whether getting a random
#' distribution in this way reflects a strong constraint on assingment
#' (i.e., how nonrandom does your "random" assignment have to be in order to work)
#' 
#' 
#' 


# BlockRand <- function(dat, 
#                       fact.groups = NULL,
#                       cont.groups = NULL,
#                       cont.cutoffs = NULL,
#                       balance = NULL){
#   # Checks the classes of these
#   
#   if (!is.null(fact.groups)){
#     
#     fg.classes <- sapply(dat[fact.groups], class)
#     blockdat <- dat[fact.groups]
#     
#     blockdat[fg.classes != "factor"] <- 
#       lapply(blockdat[fg.classes != "factor"], factor)
#   }
#   
#   if (!is.null(cont.groups)){
#     
#     if (is.null(cont.cutoffs)) {
#         cont.cutoffs <- rep(NA, length(cont.groups))
#     }
#     
#     blockdat[cont.groups] <- 
#       lapply(seq_along(cont.groups), function(x){
#       
#       workdat <- dat[cont.groups[x]]
#       
#       
#       if (!is.na(cont.cutoffs[x])) {
#         # If cutoff provided, use that:
#         workdat$group <- 
#           factor(
#             ifelse(
#               workdat >= cond.cutoffs[x],
#               "high",
#               "low"
#             ))
#       } else {
#         # Else divide by high or low groups
#         workdat$group <- 
#           factor(
#             ifelse(
#               as.numeric(scale(workdat)) >= 0,
#               "high",
#               "low"
#             ))
#       }
#       
#       workdat$group
#       })
#   }
#   
# blockdat
# 
#   
# }              

# 
# debugonce(BlockRand)
# BlockRand(
#   dat = iris, 
#   fact.groups = "Species", 
#   cont.groups = c("Petal.Length", "Sepal.Length"),
#   cont.cutoffs = NULL
#   )        
# 

