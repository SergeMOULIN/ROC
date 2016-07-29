# as.double marche sur les vecteur (chouette)
#num = as.double(res[1,2])
#a = c('5', '6')
#b = as.double(a)


l = list()
l = c(l,list(1))
l = c(l,list(2))
l = c(l,list(3))


a = 1
m = n = 8 
nom = 'ROC/data_cyril_metaux_sol.txt'
X = read.table(nom,header = TRUE)
x = as.double(X[1,(a+1):(m+n+a)])
y  = c(rep(0,8),rep(1,8))

x2 = c()
x2_cat0 = c()
x2_cat1 = c()
for (j in 1:length(x))
    if (y[j] == 0)
        {
        x2 = c(x2, x[j])
        x2_cat0 = c(x2_cat0,x[j])
        }
    if (y[j] == 1)
        {
        x2 = c(x2, x[j])
        x2_cat1 = c(x2_cat1,x[j])
        }

Wilcoxon = wilcox.test(x2_cat0,x2_cat1)  
Wilcoxon = wilcox.test(x2_cat0,x2_cat1,exact = TRUE)  # pas la même p-value que sur python !!!


x2_cat0 = 1:9
x2_cat1 = 11:19
Wilcoxon = wilcox.test(x2_cat0,x2_cat1)
