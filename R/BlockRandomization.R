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

