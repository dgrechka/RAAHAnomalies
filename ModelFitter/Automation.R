updateAllModels <- function(host,username,password,dbName,version) {
  
  t1 <- data.frame(
    sensorId=numeric(),
    obsCount=numeric(),
    outlierCount=numeric(),
    rate=numeric(),
    shape=numeric(),
    ppv=numeric(),
    sensitivity=numeric(),
    f1=numeric());
  print("Getting sensor list");
  ids <- getSensorIDs(host,username,password,dbName);
  N <- nrow(ids)
  counter <- 0;
  for(id in ids$ID) {
    counter <- counter +1;
    print(paste0("Processing sensor with ID ",id," (",counter," of ",N,"). Fetching quality controlled sensor samples"));
    samples <- getSamples(host,username,password,dbName,id);
    if(length(samples)<2 | sd(samples)==0) {
      print(paste0("Fetched ",length(samples)," samples. Skipping as too few good samples"));
      next;
    }
    print(paste0("Fetched ",length(samples)," samples. Fitting model"));
    m <- fit(samples);
    t1 <- rbind(t1,c(id,m$obs_count,m$outliers_count,m$rate,m$shape,
                     m$quality.in_sample$precision,m$quality.in_sample$sensitivity,
                     m$quality.in_sample$f1));
    print("Fitted. Persiting to DB");
    persistModel(host,username,password,dbName,id,m,version)
  }
  names(t1) <- c("sensorId","obsCount",
                 "outlierCount","rate",
                 "shape","ppv",
                 "sensitivity","f1");
  return(t1);
}