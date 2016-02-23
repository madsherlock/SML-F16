%% initialize
DPI_list = [100 200 300];
sigma_list = [0.2 0.3 0.6 1 1.5 2 2.5 3 3.5];
k_list = [1 2 3 4 5 6 7 8 9 10 11];
nk = length(k_list);


%% Load data
fnpre = '../data/cv-2-2-100-';
fn=horzcat(fnpre,num2str(sigma_list(1)),'-10.csv');
[k100,Accuracy100,Kappa100,AccuracySD100,KappaSD100] = importcv(fn);
sigma100=ones(nk,1)*sigma_list(1);
DPI100=ones(nk*length(sigma_list),1)*DPI_list(1);

for(i=2:length(sigma_list))
    s=sigma_list(i);
    sigma100 = horzcat(sigma100,ones(nk,1)*s);
    fn = horzcat(fnpre,num2str(s),'-10.csv');
    [tk,tAccuracy,tKappa,tAccuracySD,tKappaSD] = importcv(fn);
    k100=horzcat(k100,tk);
    Accuracy100=horzcat(Accuracy100,tAccuracy);
    Kappa100=horzcat(Kappa100,tKappa);
    AccuracySD100=horzcat(AccuracySD100,tAccuracySD);
    KappaSD100=horzcat(KappaSD100,tKappaSD);
end

fnpre = '../data/cv-2-2-200-';
fn=horzcat(fnpre,num2str(sigma_list(1)),'-3.csv');
[k200,Accuracy200,Kappa200,AccuracySD200,KappaSD200] = importcv(fn);
sigma200=ones(nk,1)*sigma_list(1);
DPI200=ones(nk*length(sigma_list),1)*DPI_list(2);

for(i=2:length(sigma_list))
    s=sigma_list(i);
    sigma200 = horzcat(sigma200,ones(nk,1)*s);
    fn = horzcat(fnpre,num2str(s),'-3.csv');
    [tk,tAccuracy,tKappa,tAccuracySD,tKappaSD] = importcv(fn);
    k200=horzcat(k200,tk);
    Accuracy200=horzcat(Accuracy200,tAccuracy);
    Kappa200=horzcat(Kappa200,tKappa);
    AccuracySD200=horzcat(AccuracySD200,tAccuracySD);
    KappaSD200=horzcat(KappaSD200,tKappaSD);
end

fnpre = '../data/cv-2-2-300-';
fn=horzcat(fnpre,num2str(sigma_list(1)),'-3.csv');
[k300,Accuracy300,Kappa300,AccuracySD300,KappaSD300] = importcv(fn);
sigma300=ones(nk,1)*sigma_list(1);
DPI300=ones(nk*length(sigma_list),1)*DPI_list(3);

for(i=2:length(sigma_list))
    s=sigma_list(i);
    sigma300 = horzcat(sigma300,ones(nk,1)*s);
    fn = horzcat(fnpre,num2str(s),'-3.csv');
    [tk,tAccuracy,tKappa,tAccuracySD,tKappaSD] = importcv(fn);
    k300=horzcat(k300,tk);
    Accuracy300=horzcat(Accuracy300,tAccuracy);
    Kappa300=horzcat(Kappa300,tKappa);
    AccuracySD300=horzcat(AccuracySD300,tAccuracySD);
    KappaSD300=horzcat(KappaSD300,tKappaSD);
end



fnpre = '../data/cv-2-1-100-';
fn=horzcat(fnpre,num2str(sigma_list(1)),'-10.csv');
[k100kiddi,Accuracy100kiddi,Kappa100kiddi,AccuracySD100kiddi,KappaSD100kiddi] = importcv(fn);
sigma100kiddi=ones(nk,1)*sigma_list(1);
DPI100kiddi=ones(nk*length(sigma_list),1)*DPI_list(1);

for(i=2:length(sigma_list))
    s=sigma_list(i);
    sigma100kiddi = horzcat(sigma100kiddi,ones(nk,1)*s);
    fn = horzcat(fnpre,num2str(s),'-10.csv');
    [tk,tAccuracy,tKappa,tAccuracySD,tKappaSD] = importcv(fn);
    k100kiddi=horzcat(k100kiddi,tk);
    Accuracy100kiddi=horzcat(Accuracy100kiddi,tAccuracy);
    Kappa100kiddi=horzcat(Kappa100kiddi,tKappa);
    AccuracySD100kiddi=horzcat(AccuracySD100kiddi,tAccuracySD);
    KappaSD100kiddi=horzcat(KappaSD100kiddi,tKappaSD);
end

clear tk tAccuracy tKappa tAccuracySD tKappaSD fn s fnpre i


%% Plot 100 DPI
figure(1);
contourf(sigma_list,k_list,Accuracy100);
colormap('jet');
title('Accuracy at 100 DPI, one person');
xlabel('\sigma')
ylabel('k')
grid on;
colorbar;
caxis([0.4 1]);

%% Plot 200 DPI
figure(2);
contourf(sigma_list,k_list,Accuracy200);
colormap('jet');
title('Accuracy at 200 DPI, one person');
xlabel('\sigma')
ylabel('k')
grid on;
colorbar;
caxis([0.4 1]);

%% Plot 300 DPI
figure(3);
contourf(sigma_list,k_list,Accuracy300);
colormap('jet');
title('Accuracy at 300 DPI, one person');
xlabel('\sigma')
ylabel('k')
grid on;
colorbar;
caxis([0.4 1]);

%% Plot 100 DPI Kiddi
figure(4);
contourf(sigma_list,k_list,Accuracy100kiddi);
colormap('jet');
title('Accuracy at 100 DPI, another person');
xlabel('\sigma')
ylabel('k')
grid on;
colorbar;
caxis([0.4 1]);

%% Plot 100 DPI, k = 1, bar plot with error bars
figure(5);
%Standard mean error SME is SD/sqrt(N).
%At 100 DPI, N is 10 repetitions of cross validation.
%At 200 and 300 DPI, N is 3.
%95% confidence intervals are found at 1.96 SME.
Accuracy100Error5 = ((AccuracySD100)*1.96)*(1/sqrt(10));
bar(sigma_list,Accuracy100(1,:),'FaceColor',[0.5 0.5 0.5]);
hold on;
errorbar(sigma_list,Accuracy100(1,:),Accuracy100Error5(1,:),'k.')
grid on; grid minor;
ylabel('Accuracy');
xlabel('\sigma');
title('Accuracy at 100 DPI, k = 1, one person, with 95% CI');

%% Plot 200 DPI, k = 1, bar plot with error bars
figure(6);
Accuracy200Error5 = ((AccuracySD200)*1.96)*(1/sqrt(3));
bar(sigma_list,Accuracy200(1,:),'FaceColor',[0.5 0.5 0.5]);
hold on;
errorbar(sigma_list,Accuracy200(1,:),Accuracy200Error5(1,:),'k.')
grid on; grid minor;
ylabel('Accuracy');
xlabel('\sigma');
title('Accuracy at 200 DPI, k = 1, one person, with 95% CI');

%% Plot 300 DPI, k = 3, bar plot with error bars
figure(7);
Accuracy300Error5 = ((AccuracySD300)*1.96)*(1/sqrt(3));
bar(sigma_list,Accuracy300(3,:),'FaceColor',[0.5 0.5 0.5]);
hold on;
errorbar(sigma_list,Accuracy300(3,:),Accuracy300Error5(3,:),'k.')
grid on; grid minor;
ylabel('Accuracy');
xlabel('\sigma');
title('Accuracy at 300 DPI, k = 3, one person, with 95% CI');

%% Plot Kiddi 100 DPI, k = 1, bar plot with error bars
figure(8);
Accuracy100Error5kiddi = ((AccuracySD100kiddi)*1.96)*(1/sqrt(10));
bar(sigma_list,Accuracy100kiddi(1,:),'FaceColor',[0.5 0.5 0.5]);
hold on;
errorbar(sigma_list,Accuracy100kiddi(1,:),Accuracy100Error5kiddi(1,:),'k.')
grid on; grid minor;
ylabel('Accuracy');
xlabel('\sigma');
title('Accuracy at 100 DPI, k = 1, another person, with 95% CI');

