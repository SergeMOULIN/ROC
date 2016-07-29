#' ROC_ligne
#'
#' Perform a ROC analysis for a given quantitative variable and a given binary variable.
#'
#' @param x: Vector that contains the value of the subjects for the given quantitative variable.
#' @param m: The number of subjects from the first category.
#' @param n: The number of subjects from the second category.
#' @param  cat0 : Name of the first category.
#' @param cat1: Name of the second category.
#' @param y: Vector that contains the value of the subjects for the given binary variable. y is defined as follows: y[i] = 0 if the subject i is in the first category
#' and y[i] = 1 if the subject i is in the second category.
#' @return "threshold_inf" and "threshold_sup": The two consecutive values of the quantitative variable such as any cut-off taken in the interval
#'  [threshold_inf" , "threshold_sup] maximize the sum between sensitivity and specificity.
#' @return threshold: Mean between threshold_inf" and "threshold_sup.  
#' @return Preference: The catégorie considered as positive in the analyse. #In other terms, the categorie that is
#' @return True positive rate: Proportion of
#' @return True negative rate:
#' @return Sum true positive and true negative: Sum true positive and true negative
#' @return Well classified subjects: well classified subjects
#' @return AUC: Area under the curve.
#' @return delta_norm: Difference  
ROC_ligne = function(x,m,n,cat0,cat1,y)
    {
    Norm = scale(x) #  Normalization. 
    vrai_pos = n # La proportion d'éléments de la categorie 1 au dessus du seuil (c'est bien).
    vrai_neg = 0 # La proportion d'éléments de la catégorie 0 au dessous du seuil (c'est bien).
    faux_pos = m # La proportion d'éléments de la categorie 0 au dessus du seuil (pas bien).
    faux_neg = 0 # La proportion d'éléments élément de la catégorie 1 en dessous du seuil (pas bien).

    indice = order(x)
    for (i in 1:(m+n))
        {
        if (y[indice[i]] == 0)  # C'est bien.
            {
            vrai_pos  = c(vrai_pos, vrai_pos[i])
            vrai_neg =  c(vrai_neg, vrai_neg[i]+1)
            faux_pos =  c(faux_pos, faux_pos[i]-1)
            faux_neg =  c(faux_neg, faux_neg[i])
            }
        if (y[indice[i]] == 1) # C'est pas bien
            {
            vrai_pos = c(vrai_pos, vrai_pos[i]-1)
            vrai_neg = c(vrai_neg, vrai_neg[i])
            faux_pos = c(faux_pos, faux_pos[i])
            faux_neg = c(faux_neg, faux_neg[i]+1)
            }
        if (i > 1)
            {
            k = 1
            while (x[indice[(i-k)]] == x[indice[i]]) # & (i-k) >= 1) # point interessant ici... Il faut executer la boucle si i-k = 1 mais il ne faut pas verifier la condition si i-k = 0.
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
        #return [vrai_pos,vrai_neg,faux_pos,tot,AUC,delta_norm,seuil,pref,vrai_pos_seuil,vrai_neg_seuil,tot_seuil,bien_classe,seuil_inf,seuil_sup]
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
  n = dim(X)[1]
  res =  matrix (rep(0, 2*n), n, 2)
  for (i in 1:n)
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
#' Import your data,  provied a table
#'
#' @param mane_data : The name of your database (input).
#' @param a : The number of descriptives collumns. (conf)
#' @param m : The number of subjects from the first categorie.
#' @param n : The number of subjects from the second categorie.
#' @param cat0 : Name of the first categorie.
#' @param cat1 : Name of the second categorie.
#' @param y: Optional vector indicating which column corresponds to category 0 and which column corresponds to category 1. 
#' By default, if no value is provided for y, it is assumed that the columns between (a + 1) and (a + m) correspond to category 0 
#' and the n following columns correspond to category 1.
#' @param nom_output : nom_output = "ROC_table.xls"
#' @param nb_of_nonzero : Optional parameter. Write nb_of_nonzero = TRUE if you want to get the number of nonzero subjets in each categrorie for every quantitative variables.
#' @return "threshold_inf" and "threshold_sup": The two consecutive values of the quantitative variable such as any cut-off taken in the interval
#'  [threshold_inf" , "threshold_sup] maximize the sum between sensitivity and specificity.
#' @return threshold:
#' @return Preference: The catégorie considered as positive in the analyse. #In other terms, the categorie that is
#' @return True positive rate: Proportion of
#' @return True negative rate:
#' @return Sum true positive and true negative:
#' @return well classified subjects:
#' @return AUC:
#' @return delta_norm:
#' @return Wilcoxon p-value:
#' @example tests/testRAST.R
#' @export
rast = function(name_data,a,m,n,cat0,cat1,y='sorted', nom_output = "ROC_table.xls",nb_of_nonzero = FALSE)
    {
    X = read.table(name_data,header = TRUE)
    res = ROC_base(X,a,m,n,cat0,cat1,y,nb_of_nonzero)
    E = colnames(X)[1:a]
    collumn_names = c(E,'threshold','Preference','True positive rate','True negative rate','Sum true positive and true negative',
                      'well classified subjects','AUC','Delta_norm','threshold inf','threshold sup','Wilcoxon p-value')
    if (nb_of_nonzero == TRUE)
    {collumn_names = c(collumn_names, paste('Number of nonzero subjects in ' , cat0), paste('Number of nonzero subjects in ' , cat1))}
    colnames(res)  = collumn_names
    write.table(res,file=nom_output,sep="\t",row.names=FALSE)
    return(res)
    }
