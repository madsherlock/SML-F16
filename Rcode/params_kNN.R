#k-NN parameters
#General iterations:
DPI_list = c(100,200,300);
sigma_list = c(0.2, 0.3, 0.6, 1.0, 1.5, 2.0, 2.5, 3, 3.5); #added 3 and 3.5 for 200 DPI setting.
k_list = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
#Cross validation setup
n_folds = 10 # 90/10 % split
n_repeats = 3 # 10 times 90/10 % split
