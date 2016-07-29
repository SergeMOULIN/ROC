#' ROC_ligne
#'
#' Perform a ROC analysis for a given quantitative variable and a given binary variable.
#'
#' @param x: Vector that contains the value of the subjects for the given quantitative variable.
#' @param m: The number of subjects from the first category.
#' @param n: The number of subjects from the second category.
#' @param cat0 : Name of the first category.
#' @param cat1: Name of the second category.
#' @param y: Vector that contains the value of the subjects for the given binary variable. 
#' y is defined as follows: y[i] = 0 if the subject i is in the first category
#' and y[i] = 1 if the subject i is in the second category.
#' @return "threshold_inf" and "threshold_sup": The two consecutive values of the quantitative variable such as any cut-off taken in the interval
#'  [threshold_inf" , "threshold_sup] maximize the sum between sensitivity and specificity.
#' @return threshold: Mean between threshold_inf" and "threshold_sup.  
#' @return Preference: The category considered as positive in the analysis. In other terms, the category that is 
#' mostly above the threshold. 
#' @return True positive rate: The proportion of elements of the postitive category that are above the threshold.
#' @return True negative rate: The proportion of elements of the negative category that are below the threshold.
#' @return Sum true positive and true negative: Sum of true positive rate and true negative rate.
#' @return Well classified subjects: well classified subjects
#' @return AUC: Area under the curve.
#' @return delta_norm: Difference between threshold_inf and threshold_sup after normalization of x.   
ROC_ligne = function(x,m,n,cat0,cat1,y)
    {
    Norm = scale(x) #  Normalization. 
    vrai_pos = n # The proportion of elements of the positive category that are above the threshold.
    vrai_neg = 0 # The proportion of elements of the negative category that are below the threshold.
    faux_pos = m # The proportion of elements of the negative category that are above the threshold.
    faux_neg = 0 # The proportion of elements of the positive category that are below the threshold.
    indice = order(x)
    for (i in 1:(m+n))
        {
        if (y[indice[i]] == 0)  
            {
            vrai_pos  = c(vrai_pos, vrai_pos[i])
            vrai_neg =  c(vrai_neg, vrai_neg[i]+1)
            faux_pos =  c(faux_pos, faux_pos[i]-1)
            faux_neg =  c(faux_neg, faux_neg[i])
            }
        if (y[indice[i]] == 1) 
            {
            vrai_pos = c(vrai_pos, vrai_pos[i]-1)
            vrai_neg = c(vrai_neg, vrai_neg[i])
            faux_pos = c(faux_pos, faux_pos[i])
            faux_neg = c(faux_neg, faux_neg[i]+1)
            }
        if (i > 1)
            {
            k = 1
            while (x[indice[(i-k)]] == x[indice[i]]) # & (i-k) >= 1) 
                {
                vrai_pos[i+1-k] = vrai_pos[i+1]
                vrai_neg[i+1-k] = vrai_neg[i+1]
                faux_pos[i+1-k] = faux_pos[i+1]
                faux_neg[i+1-k] = faux_neg[i+1]
                k = k+1
                if ((i-k) == 0) break
                }
            }
        }
    AUC = 0.
    for (i in 1:(m+n))
        {
        AUC = AUC + (faux_pos[i]-faux_pos[i+1])*(vrai_pos[i+1]+vrai_pos[i])
        }
    AUC = AUC / (m*n*2)
    tot = c()
    for (i in 1:(m+n+1))
        {
        vrai_pos[i] = vrai_pos[i]/n
        vrai_neg[i] = vrai_neg[i]/m
        faux_pos[i] = faux_pos[i]/m
        faux_neg[i] = faux_neg[i]/n
        tot =  c(tot, (vrai_pos[i] + vrai_neg[i]))
        }

    tot_seuil = max(tot)
    Min = min(tot)

    if (AUC >= 0.5 & tot_seuil != 1)
        {
        pref = cat1
        k = which.max(tot)

        l = k+1
        while ((tot[k] == tot[l]) & (l < length(tot)))
            l= l+1

        seuil_inf = x[indice[k-1]]
        seuil_inf_norm = Norm[indice[k-1]]
        seuil_sup = x[indice[l-1]]
        seuil_sup_norm = Norm[indice[l-1]]
        delta_norm = seuil_sup_norm - seuil_inf_norm
        seuil = (seuil_sup + seuil_inf)/2
        vrai_pos_seuil = vrai_pos[k]
        vrai_neg_seuil = vrai_neg[k]
        bien_classe = vrai_neg[k] * m + vrai_pos[k] * n
        }

    if (AUC < 0.5)
        {
        pref = cat0
        AUC = 1 - AUC
        tot_seuil = 1 - min(tot)
        k = which.min(tot)

        l = k+1
        while ((tot[k] == tot[l]) & (l+1 < length(tot)))
            {l = l+1}

        seuil_inf = x[indice[k-1]]
        seuil_inf_norm = Norm[indice[k-1]]
        seuil_sup = x[indice[l-1]]
        seuil_sup_norm = Norm[indice[l-1]]
        delta_norm = seuil_sup_norm - seuil_inf_norm
        seuil = (seuil_sup + seuil_inf)/2

        for (i in 1:(m+n+1))
            {
            b = vrai_pos[i]
            vrai_pos[i] = faux_pos[i]
            faux_pos[i] = b
            c = vrai_neg[i]
            vrai_neg[i] = faux_neg[i]
            faux_neg[i] = c
            }
        vrai_pos_seuil = vrai_pos[k]
        vrai_neg_seuil = vrai_neg[k]
        bien_classe = vrai_neg[k] * n + vrai_pos[k] * m
        tot_seuil = vrai_neg_seuil + vrai_pos_seuil
         }
    if (AUC == 0.5 & tot_seuil == 1 & Min == 1)
        {
        pref = "neutre"
        seuil_inf = seuil_sup = seuil = delta_norm = vrai_pos_seuil = 0
        vrai_neg_seuil = 1
        bien_classe = n
        }
    return (c(seuil,pref,vrai_pos_seuil,vrai_neg_seuil,tot_seuil,bien_classe,AUC,delta_norm,seuil_inf,seuil_sup))
    }


non_nul = function(X,y)
  {
  N = dim(X)[1]
  res =  matrix (rep(0, 2*N), N, 2)
  for (i in 1:N)
    {
    x = X[i,]
    for (j in 1:length(x))
      {
      if (y[j] == 0 & x[j] != 0)
        {res[i,1] = res[i,1] + 1}
      if (y[j] == 1 & x[j] != 0)
        {res[i,2] = res[i,2] + 1}
      }
    }
  return(res)
  }


ROC_base = function(X,a,m,n,cat0,cat1,y,nb_of_nonzero)
    {
    if (y[1] == 'sorted')
         {y = c(rep(0,m),rep(1,n))}
    y2 = c()
    for (j in 1:length(y))
        {if (y[j] == 0)
            {y2 = c(y2,0)}
        if (y[j] == 1)
            {y2= c(y2,1)}}

    for (i in 1:(dim(X)[1]))
        {
        x = as.double(X[i,(a+1):(m+n+a)])
        x2 = c()
        x2_cat0 = c()
        x2_cat1 = c()
        for (j in 1:length(x))
            {
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
            }
        Wilcoxon = wilcox.test(x2_cat0,x2_cat1,exact = FALSE)
        description = c()
        for (j in 1:a)
          {description = c(description,as.character(X[i,j]))}
        if (i == 1)
            {res = c(description,ROC_ligne(x2,m,n,cat0,cat1,y=y2),Wilcoxon$p.value)}
        if (i > 1)
          {res = rbind(res,c(description,ROC_ligne(x2,m,n,cat0,cat1,y=y2),Wilcoxon$p.value))}
    }
    if (nb_of_nonzero == TRUE)
      {res = cbind(res,non_nul(X[,((a+1):dim(X)[2])],y2))}
    or = order(as.double(res[,a+7]),as.double(res[,a+8]),decreasing=T)
    res2 = res[or,]
    return (res2)
    }


#' rast:ROC analysis sorted table
#'
#' Import your data. Compute a ROC analysis for each quantitative variable of the database. Meet all these results in the same table. 
#' Sort this table to show the most discriminating variables and export this table in excel.  
#'
#' @param name_data : The name of your database (input).
#' @param a : The number of descriptives collumns. (see exemple)
#' @param m : The number of subjects from the first categorie.
#' @param n : The number of subjects from the second categorie.
#' @param cat0 : Name of the first categorie.
#' @param cat1 : Name of the second categorie.
#' @param y: Optional vector indicating which column corresponds to category 0 and which column corresponds to category 1. 
#' By default, if no value is provided for y, it is assumed that the columns between (a + 1) and (a + m) correspond to category 0 
#' and the n following columns correspond to category 1.
#' @param name_output : Optional parameter. The name of the excel table returned. By default, name_output = "ROC_table.xls"
#' @param nb_of_nonzero : Optional parameter. Write nb_of_nonzero = TRUE if you want to get the number of nonzero subjets 
#' in each categrorie for every quantitative variables.
#' @return "threshold_inf" and "threshold_sup": The two consecutive values of the quantitative variable such as any cut-off taken in the interval
#'  [threshold_inf" , "threshold_sup] maximize the sum between sensitivity and specificity.
#' @return threshold: Mean between threshold_inf" and "threshold_sup.  
#' @return Preference: The category considered as positive in the analysis. In other terms, the category that is 
#' mostly above the threshold. 
#' @return True positive rate: The proportion of elements of the postitive category that are above the threshold.
#' @return True negative rate: The proportion of elements of the negative category that are below the threshold.
#' @return Sum true positive and true negative: Sum of true positive rate and true negative rate.
#' @return Well classified subjects: well classified subjects
#' @return AUC: Area under the curve.
#' @return delta_norm: Difference between threshold_inf and threshold_sup after normalization of x.   
#' @return Wilcoxon p-value: Wilcoxon p-value
#' @example tests/testRAST.R
#' @export
rast = function(name_data,a,m,n,cat0,cat1,y='sorted', name_output = "ROC_table.xls",nb_of_nonzero = FALSE)
    {
    X = read.table(name_data,header = TRUE)
    res = ROC_base(X,a,m,n,cat0,cat1,y,nb_of_nonzero)
    E = colnames(X)[1:a]
    collumn_names = c(E,'threshold','Preference','True positive rate','True negative rate','Sum true positive and true negative',
                      'well classified subjects','AUC','Delta_norm','threshold inf','threshold sup','Wilcoxon p-value')
    if (nb_of_nonzero == TRUE)
    {collumn_names = c(collumn_names, paste('Number of nonzero subjects in ' , cat0), paste('Number of nonzero subjects in ' , cat1))}
    colnames(res)  = collumn_names
    write.table(res,file=name_output,sep="\t",row.names=FALSE)
    return(res)
    }


# Example 1:
# In this first example, the database used as input is the followig (without #).
#
# Id	caractere	setosa1	setosa2	setosa3	setosa4	setosa5	setosa6	setosa7	setosa8	versicolor1	versicolor2	versicolor3	versicolor4	versicolor5	versicolor6	versicolor7	versicolor8
# 1	Sepal_Length	5.1	4.9	4.7	4.6	5	5.4	4.6	5	7	6.4	6.9	5.5	6.5	5.7	6.3	4.9
# 2	Sepal_Width	3.5	3	3.2	3.1	3.6	3.9	3.4	3.4	3.2	3.2	3.1	2.3	2.8	2.8	3.3	2.4
# 3	Petal_Length	1.4	1.4	1.3	1.5	1.4	1.7	1.4	1.5	4.7	4.5	4.9	4	4.6	4.5	4.7	3.3
# 4	Petal_Width	0.2	0.2	0.2	0.2	0.2	0.4	0.3	0.2	1.4	1.5	1.5	1.3	1.5	1.3	1.6	1
#
# This database, named "Exeample_1" is a part of the classical "Iris" database.
# The columns are not correcty sorted here (every setosa then evey versicolor). Thus it is not necessary to defined an input "y".
#
#
res = rast('example1.txt',2,8,8,'setosa','versicolor')
#
# The output of the fonction is the table:
#
# Id	caractere	threshold	Preference	True positive rate	True negative rate	Sum true positive and true negative	well classified subjects	#AUC	Delta_norm	threshold inf	threshold sup	Wilcoxon p-value
# 3	Petal_Length	2.5	  versicolor	1	    1	2	    16	1	          1.02192446713797	1.7	3.3	0.000837046362390583
# 4	Petal_Width	  0.7	  versicolor	1	    1	2	    16	1	          0.983959199337784	0.4	1	  0.000649688280521811
# 1	Sepal_Length	5.45	versicolor	0.875	1	1.875	15	0.9296875	  0.120476827067682	5.4	5.5	0.00448535921842047
# 2	Sepal_Width	  3.35	setosa	    0.625	1	1.625	13	0.8515625	  0.240307791085116	3.3	3.4	0.0202088881490031
#
#
#
# Example 2:
#
# In this exemple, the database used as input is the same that in example 1, exepted that the columns are not correcty sorted (every setosa then evey versicolor) as in example 1.
# This implies that the input "y" as to be defined here.
#
# Id	caractere	setosa1	setosa2	setosa3	setosa4	versicolor1	versicolor2	versicolor3	versicolor4	setosa5	setosa6	setosa7	setosa8	versicolor5	versicolor6	versicolor7	versicolor8
# 1	Sepal.Length	5,1	4,9	4,7	4,6	7	6,4	6,9	5,5	5	5,4	4,6	5	6,5	5,7	6,3	4,9
# 2	Sepal.Width	3,5	3	3,2	3,1	3,2	3,2	3,1	2,3	3,6	3,9	3,4	3,4	2,8	2,8	3,3	2,4
# 3	Petal.Length	1,4	1,4	1,3	1,5	4,7	4,5	4,9	4	1,4	1,7	1,4	1,5	4,6	4,5	4,7	3,3
# 4	Petal.Width	0,2	0,2	0,2	0,2	1,4	1,5	1,5	1,3	0,2	0,4	0,3	0,2	1,5	1,3	1,6	1
#
y  = c(rep(0,4),rep(1,4),rep(0,4),rep(1,4))
res = rast('example2.txt',2,8,8,'setosa','versicolor',y, name_output = "ROC_table_2.xls",nb_of_nonzero = TRUE)

