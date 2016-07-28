#replace every data with the name of the data frame that you want to modify
rapply(data, f = function(x) ifelse(is.nan(x),0,x),how="replace")
#you don't have to do first line which is other way to performed the same thing but later works better
is.nan.data.frame = function(x)
do.call(cbind, lapply(x, is.nan))
data[is.nan(data)] = 0