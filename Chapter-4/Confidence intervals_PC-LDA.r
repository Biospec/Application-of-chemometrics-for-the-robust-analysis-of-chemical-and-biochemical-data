mean_LDA = mean(Accuracy_LDA)
sd_LDA = apply(Accuracy_LDA,2,sd)
n = length(Accuracy_LDA)
error = qnorm(0.975)*sd_LDA/sqrt(n)
left = mean_LDA-error
right = mean_LDA+error
print(mean_LDA)
print(left)
print(right)