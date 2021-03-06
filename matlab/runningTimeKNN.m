%% read csv
M{1}=csvread('../data/runningTimekNN-2-2-sig1.5-DPI100.csv',1,0);
M{2}=csvread('../data/runningTimekNN-2-2-sig2.5-DPI200.csv',1,0);
M{3}=csvread('../data/runningTimekNN-2-2-sig3.5-DPI300.csv',1,0);
sigs=[1.5 2.5 3.5];

%%
for(it=1:3)
    figure(it);
    DPI=100*it;
    [X,Y] = meshgrid(...
    linspace(min(M{it}(:,1)),max(M{it}(:,1)),length(M{it})),...
    linspace(min(M{it}(:,2)),max(M{it}(:,2)),length(M{it})));
    contourf(X,Y,griddata(M{it}(:,1),M{it}(:,2),M{it}(:,3),X,Y));
    xlabel('N');
    ylabel('k');
    grid on;
    colorbar;
    load('myColors.mat');
    colormap(myColors);
    caxis([min(M{1}(:,3)) max(M{3}(:,3))]);
    hold on;scatter(M{it}(:,1),M{it}(:,2),'mx');hold off
%    title(sprintf('Running time [s] vs. N and k, %d DPI, \\sigma=%0.1f',[DPI sigs(it)]));
    title(sprintf('Running time [s] of N/10 predictions vs. N and k, %d DPI',DPI));
        
end
figure(4)
loglog(M{1}(:,1),M{1}(:,3)./(M{1}(:,1)./10),'bx'); hold on;
loglog(M{2}(:,1),M{2}(:,3)./(M{1}(:,1)./10),'gx');
loglog(M{3}(:,1),M{3}(:,3)./(M{1}(:,1)./10),'rx');
%loglog(M{3}(:,1),1/100*(M{3}(:,1)),'ro');
%loglog(M{3}(:,1),1/100*(M{3}(:,1)).*log(M{3}(:,1)),'r.');
loglog(M{3}(:,1),1/200000*(M{3}(:,1)),'m-');
grid on;
hold off;
xlabel('N');
ylabel('Running time per classification [s]');
title('k-NN classification time vs. N at various DPI');
legend('100 DPI','200 DPI','300 DPI','5\cdot{}10^{-6}\cdot{}N reference','Location','NorthWest');