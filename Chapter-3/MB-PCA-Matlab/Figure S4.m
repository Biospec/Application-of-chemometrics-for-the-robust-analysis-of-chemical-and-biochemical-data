load test.txt
Xtest=[test(1:90,:) test(91:180,:) test(181:end,:)];
Xtest=Xtest-ones(90,1)*mean(Xtest);
Xtest=Xtest./(ones(90,1)*std(Xtest));
Xin = [1 52; 53 52*2; 52*2+1 52*3] 
[Tb,Pb,Wt,Tt,ssq,Rbo,Rbv,Lbo,Lbv,Lto] = cpca(Xtest,Xin,[3 3 3]);
figure
subplot(1,3,1)
plot(Tb(1:30,1),Tb(1:30,4),'ko')
hold on
plot(Tb(31:60,1),Tb(31:60,4),'r^')
plot(Tb(61:90,1),Tb(61:90,4),'g+')
hold off
subplot(1,3,2)
plot(Tb(1:30,2),Tb(1:30,5),'ko')
hold on
plot(Tb(31:60,2),Tb(31:60,5),'r^')
plot(Tb(61:90,2),Tb(61:90,5),'g+')
hold off
subplot(1,3,3)
plot(Tb(1:30,3),Tb(1:30,6),'ko')
hold on
plot(Tb(31:60,3),Tb(31:60,6),'r^')
plot(Tb(61:90,3),Tb(61:90,6),'g+')
hold off