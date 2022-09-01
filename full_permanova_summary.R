full_permanova_summary <- function(y,env,model,a,b,alpha = 0.05,...){
  require(vegan)
  require(dplyr)
  # y is community table (species as columns!)
  # model is the model in formula nomenclature, see: help(formula)
  # a is the first variable used in the model (must be a column name of env)
  # b is the second variable used in the model (must be a column name of env)
  # alpha is the significance level for p-value
  
  # Make model
  model <- as.formula(model)
  
  # Permanova for model
  permanova <- 
    adonis2(model, data=env)
  permanova_clean <- 
    permanova %>% 
    as.data.frame() %>% 
    mutate(Test = "PERMANOVA",Term = rownames(permanova))
  
  # Betadisper helper function
  full_betadisper <- function(var){
    bd <- permutest(
      betadisper(
        vegdist(y),
        group = pull(select(env,all_of(var))),
        type = "centroid"),
      permutations = 999)
    #
    bd[[1]] %>% 
      as.data.frame() %>% 
      mutate(Term = all_of(var),Test = "Betadisper",Term2 = c("Groups","Residuals"))
  }
  
  # Betadisper for a
  betadisper_a <- full_betadisper(a)
  # Betadisper for b
  betadisper_b <- full_betadisper(b)
  
  # Merge in a single table and add more information
  permanova_clean %>% 
    full_join(betadisper_a) %>% 
    full_join(betadisper_b) %>% 
    mutate(Significance = ifelse(`Pr(>F)` < alpha,
                                 paste0("Significant (p<",alpha,")"),
                                 paste0("Not significant (p>=",alpha,")")))
}
