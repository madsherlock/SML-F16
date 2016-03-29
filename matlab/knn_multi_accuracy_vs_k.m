%multi_person
M=csvread('../data/knnMulti-1-1-2-1-3-1-4-1-5-1-6-1-7-1-8-1-10-1-11-1-dpi100-sig1.5-rep3-results.csv',1);
k=M(:,1);
acc=M(:,2);
accSD=M(:,4);
nrep=3;
figure(1);
%Standard mean error SME is SD/sqrt(N).
%N is 3 repetitions of cross validation???
%95% confidence intervals are found at 1.96 SME.
accErr5 = ((accSD)*1.96)*(1/sqrt(nrep));
bar(k,acc,'FaceColor',[0.5 0.5 0.5]);
hold on;
errorbar(k,acc,accErr5,'k.')
grid on; grid minor;
ylabel('Accuracy');
xlabel('k');
title('Accuracy at 100 DPI, \sigma=1.5, MIX, with 95% CI');

pos=get(gcf,'Position');
pos(3)=2*pos(3);
set(gcf,'Position',pos)

%%

%multi_person
M=csvread('../data/knnMultiLOO-1-1-2-1-3-1-4-1-5-1-6-1-7-1-8-1-10-1-11-1-dpi100-sig1.5-rep3-results.csv',1);
k=M(:,1);
acc=M(:,2);
accSD=M(:,4);
nrep=3;
figure(2);
%Standard mean error SME is SD/sqrt(N).
%N is 3 repetitions of cross validation???
%95% confidence intervals are found at 1.96 SME.
accErr5 = ((accSD)*1.96)*(1/sqrt(nrep));
bar(k,acc,'FaceColor',[0.5 0.5 0.5]);
hold on;
errorbar(k,acc,accErr5,'k.')
grid on; grid minor;
ylabel('Accuracy');
xlabel('k');
title('Accuracy at 100 DPI, \sigma=1.5, LOO, with 95% CI');

pos=get(gcf,'Position');
pos(3)=2*pos(3);
set(gcf,'Position',pos)
