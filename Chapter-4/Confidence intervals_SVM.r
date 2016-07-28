mean_SVM = mean(Accuracy_SVM)
sd_SVM = apply(Accuracy_SVM,2,sd)
n = length(Accuracy_SVM)
error = qnorm(0.975)*sd_SVM/sqrt(n)
left = mean_SVM-error
right = mean_SVM+error
print(mean_SVM)
print(left)
print(right)