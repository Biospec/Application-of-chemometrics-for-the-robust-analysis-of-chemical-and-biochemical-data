n = the number of features
##### Vast scaling ######
  train = train[,2:189]
  for (v in 1: length(train)){
    train[,v] = (train[,v]-mean.train[v])/ std.train[v]*(mean.train[v]/std.train[v])
    }
  test =  test[,2:189]
    for (p in 1:length(test)){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]*(mean.train[p]/std.train[p])
      }
      
###### Pareto scaling #######

  train = train[,2:189]
  for (v in 1: length(train)){
    train[,v] = (train[,v]-mean.train[v])/ sqrt(std.train[v])
    }
  test =  test[,2:189]
    for (p in 1:length(test)){
      test[,p] = (test[,p]-mean.train[p])/sqrt(std.train[p])
      }
      
###### level scaling #########

  train = train[,2:189]
  for (v in 1: length(train)){
    train[,v] = (train[,v]-mean.train[v])/ mean.train[v]
    }
  test =  test[,2:189]
    for (p in 1:length(test)){
      test[,p] = (test[,p]-mean.train[p])/mean.train[p]
      }
 ###### autoscaling #######
 
 train = train[,2:189]
  for (v in 1: length(train)){
    train[,v] = (train[,v]-mean.train[v])/ std.train[v]
    }
  test =  test[,2:189]
    for (p in 1:length(test)){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]
      }
      
##### range scaling #######

 min.train = sapply(train[,2:189],min)
 max.train = sapply(train[,2:189],max)
 train = train[,2:189]
  for (v in 1: length(train)){
    train[,v] = (train[,v]-mean.train[v])/ (max.train[v]-min.train[v])
    }
  test =  test[,2:189]
    for (p in 1:length(test)){
      test[,p] = (test[,p]-mean.train[p])/(max.train[p]-min.train[p])
      }