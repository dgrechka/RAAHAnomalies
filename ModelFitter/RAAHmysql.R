require('RMySQL')

#Connects to DB, extracts the set of sensors, fits a model for each of them
#puts fitted parameters back to DB
getSensorIDs  <- function(
  host,
  username,
  password,
  dbName,
  minMeasurementCount=10000){ #if one measurement lasts 4 min then default is ~27.7 days of measurements
  con <- dbConnect(RMySQL::MySQL(), host = host, dbname=dbName,
                 user = username, password = password);
  q <- dbGetQuery(con, paste0("select se.id ID,count(*) measurements from Sensors se inner join Samples sp on se.id=sp.sensorID
                  group by sp.sensorID
                  having measurements>",minMeasurementCount));
  dbDisconnect(con);
return(q);
}


getSamples <- function(host,username,password,dbName,sensorID){
  con <- dbConnect(RMySQL::MySQL(), host = host, dbname=dbName,
                   user = username, password = password);
  q <- dbGetQuery(con, paste0("select doseRate(pulsesCount,measureTimespanMin) doseRate
                  from Samples
                  where sensorID=",sensorID,
                  " AND quality=0 AND !isExperiment;"));
  dbDisconnect(con);
  return(q$doseRate);
}

safeDouble <- function(x) {
  if((x == -Inf) || (x == Inf)) {
    return('null');
  }
  else {
    return(x);
  }
}

persistModel <- function(host,username,password,dbName,sensorID,model,version){
  con <- dbConnect(RMySQL::MySQL(), host = host, dbname=dbName,
                   user = username, password = password);
  q <- paste0("insert into
                         GammaProperties
                         (sensorId,creationTimeUTC,GammaShape,GammaRate,totalMeasurementCount,outlierCount,version,AIC,BIC,logLikelihood)
                         values (",sensorID,",UTC_TIMESTAMP(),",
              model$shape,",",
              model$rate,",",
              model$obs_count,",",
              model$outliers_count,",'",
              version,"',",
              safeDouble(model$quality.in_sample$AIC),",",
              safeDouble(model$quality.in_sample$BIC),",",
              safeDouble(model$quality.in_sample$lglk),")");
  #print(q);
  dbGetQuery(con, q);
  dbDisconnect(con);
  return();
}