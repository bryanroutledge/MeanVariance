# Mean Variance Lib
# This is the KEY function that caculates w1 and wR
# via inverting V

Find.wstar <- function(
    Economy # The thing coming out of Estimate.Mean.Variance
){
  ones <- ones <- matrix(1, nrow = nrow(Economy$V), ncol = 1)
  V_inv <- solve(Economy$V)  # that means "invert" in R
  
  wR <- (V_inv %*% Economy$Rbar) / as.numeric(t(ones) %*% V_inv %*% Economy$Rbar)
  w1 <- (V_inv %*% ones) / as.numeric(t(ones) %*% V_inv %*% ones) 
  
  # Info FYI 
  Rbar.wR = Calc.Rbar(wR,Economy$Rbar)  
  Rbar.w1 = Calc.Rbar(w1,Economy$Rbar)  
  
  sigma.wR = Calc.Sigma(wR,Economy$V) 
  sigma.w1 = Calc.Sigma(w1,Economy$V) 
  
  # Storing the weights and moments in seperate tables makes things later simpler
  Economy$portfolio.weights <- tibble(
    Name = rownames(wR),
    wR =  wR %>% as.numeric(),
    w1 =  w1 %>% as.numeric(), 
  )
  
  
  Economy$portfolio.moments <- tibble(
    Name = c("wR", "w1"),
    Rbar = c(Rbar.wR, Rbar.w1),
    RSigma = c(sigma.wR, sigma.w1)
  )
  
  return(Economy)
}


Find.wstar.many <- function(
    E,    # maybe just portfolio
    mu   # vector of target returns 
){
  
  Rbar.wR <- E$portfolio.moments %>% filter(Name=="wR") %>% pull(Rbar) %>% as.numeric()
  Rbar.w1 <- E$portfolio.moments %>% filter(Name=="w1") %>% pull(Rbar) %>% as.numeric()
  
  # now write a loop over mu. No need to be pretty
  for (m in mu){
    w.name=paste0("w.mu.",sprintf("%.4f", m))
    alpha = (m-Rbar.w1)/(Rbar.wR-Rbar.w1)
    E$portfolio.weights<- E$portfolio.weights %>% 
      mutate(
        !!sym(w.name) := (1-alpha)*w1 + alpha*wR   # w1 and wR should already be there
      )
    w <- E$portfolio.weights %>% select(!!w.name) %>% as.matrix(ncol=1)
    E$portfolio.moments <- E$portfolio.moments %>% 
      add_row(
        Name=w.name,
        Rbar = Calc.Rbar(w,E$Rbar),
        RSigma =Calc.Sigma(w,E$V)
      )
  }
  
  # To get a nice line plot we need the Rbar and RSigma ordered nicely
  # Slope is positive above GMV and negative below GMV
  RbarGMV = E$portfolio.moments %>% Get.GMV.Rbar
  E$portfolio.moments <- E$portfolio.moments %>%
    mutate(
      Efficient = ifelse(Rbar>=RbarGMV,TRUE,FALSE)
    ) 
  
  return(E)
}
