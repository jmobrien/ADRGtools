# lmperm - Permutation tests for a variable within lm() 

require(broom)
require(tidyverse)


lmperm <- function(model, dat, perm.var, perms = 10000){
  
  # Run main model:
  mainmod <- 
    lm(model, dat)
  # Get tidy output:
  mainmodtidy <- 
    tidy(mainmod)
  # Permuted models:
  perm.test <- 
    replicate(perms, {
      permdat <- dat
      permdat[perm.var] <- 
        sample(permdat[[perm.var]])
      
      lm(model, permdat) %>% tidy
    },
    simplify = FALSE)
  
  # Get estimate distributions:
  dists <-
    perm.test %>% 
    map_dfr(
      ~{.x$estimate %>% 
          as.list %>% 
          setNames(.x$term) %>% 
          as_tibble
      })


  # Names of things:
  perm.quantile.names <-
    c("perm.CI.95.lower", "perm.median", "perm.CI.95.upper")
  
  perm.quantiles <-
    c(.025, .5, .975)
  
  
  
  # Obtain quantiles of estimate from each element:
  quantiles <-
    map2_dfr(
      dists,
      mainmodtidy$estimate,
      
      ~{bind_cols(
        quantile(x = .x, probs = perm.quantiles, names = FALSE) %>% 
          as.list %>% 
          setNames(perm.quantile.names) %>% 
          as_tibble,
        tibble(perm.p.value = mean(abs(.x) >= abs(.y)))
      )
      })
  
  # Full Model:
  model.extend <-         
    bind_cols(
      mainmodtidy,
      quantiles
    )
  # Output (includes things for plotting):
  list(
    dists = dists,
    model = model.extend
  )
  
} 

plot.lmperm <- function(lmperm.obj){
  lmperm.obj$dists %>% 
    gather(key = term,
           value = dist
    ) %>% 
    ggplot(aes(x = dist)) + geom_density(fill = "blue", alpha = .5) +
    # Real-Data estimated value:
    geom_vline(data = lmperm.obj$model, aes(xintercept = estimate), color = "red") +
    # Two-tailed 95% boundaries from permuted null distribution of estimates:
    geom_vline(data = lmperm.obj$model, aes(xintercept = perm.CI.95.lower), linetype = 2, color = "gray4") +
    geom_vline(data = lmperm.obj$model, aes(xintercept = perm.CI.95.upper), linetype = 2, color = "gray4") +
    # Median estimate of permuted null distribution:
    geom_vline(data = lmperm.obj$model, aes(xintercept = perm.median), linetype = 3, color = "gray4") +
    facet_grid(. ~ term, scales = "free", shrink = TRUE)
}


