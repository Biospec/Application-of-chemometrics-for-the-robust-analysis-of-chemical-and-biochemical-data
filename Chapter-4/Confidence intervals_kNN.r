mean_kNN = mean(Accuracy_kNN)
sd_kNN = apply(Accuracy_kNN,2,sd)
n = length(Accuracy_kNN)
error = qnorm(0.975)*sd_kNN/sqrt(n)
left = mean_kNN-error
right = mean_kNN+error
print(mean_kNN)
print(left)
print(right)