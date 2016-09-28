# The function takes "threshold" portion of the data
# (considering lowerest threshold/2 and highest threshold/2 points as outliers)
# and fits gamma distribution to the data
#
# x is a numeric vector of all measuremnets
# threshold is a portion of the data to consider as outliers;
fit<- function(x,threshold=5e-4) {
  # 1. filtering out outliers
  q_levels <- c(threshold*0.5,1.0-threshold*0.5);
  q_values <- quantile(x,probs = q_levels)
  outliers_mask <- x<q_values[1] | x> q_values[2];
  x_filtered <- x[!outliers_mask]
  
  # 2. Getting characteristics of clean data
  mu <- mean(x_filtered);
  sigma2 <- var(x_filtered)
  
  # 3. Coverting them to gamma distribution parameters
  rate = mu/sigma2
  shape = rate*mu
  
  # 4. plotting (usful for debugging)
  f <- function(x) {return(dgamma(x,shape=shape,rate=rate));}
  hist(x_filtered,breaks = 50,freq=F)
  lines(density(x_filtered),col='red')
  curve(f,from = min(x_filtered), to = max(x_filtered),n = 200 , add = T, col='blue')
  
  # 5. Evaluating fitted model
  q_values.model <- qgamma(q_levels,shape = shape,rate = rate)
  outliers_mask.model <- x<q_values.model[1] | x> q_values.model[2];
  
  tp <- sum(outliers_mask & outliers_mask.model);
  fp <- sum(!outliers_mask & outliers_mask.model);
  fn <- sum(outliers_mask & !outliers_mask.model);
  ppv <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  
  result = list(
    q_levels = q_levels,
    q_values = q_values,
    q_values.model = q_values.model,
    obs_count = length(x),
    outliers_count = sum(outliers_mask),
    rate=rate,
    shape=shape,
    quality.in_sample=list(
      precision = ppv,
      sensitivity = recall,
      f1 = 2*ppv*recall/(ppv+recall)
    )
  )
  return(result);
}