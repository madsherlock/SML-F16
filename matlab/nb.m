A=csvread('../data/nbPDresults.csv',1,1);
thr=A(:,1);
fL=A(:,2);
usekernel=A(:,3);
Accuracy=A(:,4);
AccuracySD=A(:,6);
clear A;

idxNoLaplaceNoKernel=(1-fL)&(1-usekernel);
idxLaplaceNoKernel=(fL)&(1-usekernel);
idxNoLaplaceKernel=(1-fL)&usekernel;
idxLaplaceKernel=fL&usekernel;

idx=[idxNoLaplaceNoKernel,idxLaplaceNoKernel,idxNoLaplaceKernel,idxLaplaceKernel];

thresh=thr(idxNoLaplaceNoKernel);

figure(1);
bars = [...
    Accuracy(idxNoLaplaceNoKernel),...
    Accuracy(idxLaplaceNoKernel),...
    Accuracy(idxNoLaplaceKernel),...
    Accuracy(idxLaplaceKernel)];
h=bar([thresh,thresh,thresh,thresh],bars)
set(h,'BarWidth',1);
set(gca,'YGrid','on');
xlabel('PCA proportion of cumulative variance explained');
ylabel('Accuracy');
xlim([0,1.05]);
legend('No Laplace, Normal','Laplace, Normal','No Laplace, KDE','Laplace, KDE',...
    'Location','NorthWest');
title({'Classification accuracy vs. PCA threshold and Naïve Bayes method,',...
    '10-fold cross validation'});
set(gca,'FontSize',9);
print('nbPD-accuracy-vs-pca-vs-nb.eps','-depsc'); 