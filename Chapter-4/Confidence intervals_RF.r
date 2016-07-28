mean_RF = mean(Accuracy_RF)
sd_RF = apply(Accuracy_RF,2,sd)
n = length(Accuracy_RF)
error = qnorm(0.975)*sd_RF/sqrt(n)
left = mean_RF-error
right = mean_RF+error
print(mean_RF)
print(left)
print(right)