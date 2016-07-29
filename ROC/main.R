#nom = 'ROC/data_cyril_metaux.txt'
#X = read.table(nom,header = TRUE,row.names = NULL)
#a = 1
#m = n = 8 
#x = as.double(X[1,(a+1):(m+n+a)])


ROC_ligne = function(x,m,n,cat0,cat1,y)
    {
    Norm = scale(x) # La normalisation. Voir quoi faire si les valeurs de x sont toute égales. Peut-être que je peut juste laisse comme ça...
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
    return (list(vrai_pos,vrai_neg,faux_pos,tot,AUC,delta_norm,seuil,pref,vrai_pos_seuil,vrai_neg_seuil,tot_seuil,bien_classe,seuil_inf,seuil_sup))
    }



ROC_base = function(X,a,m,n,cat0,cat1,y)
    {
    if (y == 'sorted')
    #if (y == c())
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
        Wilcoxon = wilcox.test(x2_cat0,x2_cat1)
        if (i == 1)
            {res = c(ROC_ligne(x2,m,n,cat0,cat1,y=y2)[5:14],as.character(X[i,1:a]),Wilcoxon$p.value)}
        if (i > 1)
            {res = rbind(res,c(ROC_ligne(x2,m,n,cat0,cat1,y=y2)[5:14],as.character(X[i,1:a]),Wilcoxon$p.value))}
        }    
    or = order(as.double(res[,1]),as.double(res[,2]),decreasing=T)
    res2 = res[or,]
    return (res2)
    }



rast = function(nom,a,m,n,cat0,cat1,y='sorted', nom_output = "ROC_table.xls")
    {  # rast : ROC analysis sorted table
    X = read.table(nom,header = TRUE)
    res0 = ROC_base(X,a,m,n,cat0,cat1,y)
    res1 = res0[,11:(10+a)]
    res2 = res0[,3:8]
    res3 = res0[,1:2]
    res4 = res0[,9:10]
    res5 = res0[,(11+a)] 
    res = cbind(res1,res2,res3,res4,res5)
    E = colnames(X)[1:a]
    colnames(res) = c(E,'threshold','Preference','True positive rate','True negative rate','Sum true positive and true negative','well classified subjects','AUC',
          'Delta_norm','threshold inf','threshold sup','Wilcoxon p-value')
          #, 'Number of nonzero subjects in ' + cat0, 'Number of nonzero subjects in ' + cat1]
    write.table(res,file=nom_output,sep="\t",row.names=FALSE)
    return(res)
    }




##############################################################


#a = 1
#m = n = 8 
#x = as.double(X[1,(a+1):(m+n+a)])
#y  = c(rep(0,4),rep(1,4),rep(0,4),rep(1,4))
#res = ROC_ligne (x,m,n,'lag','nat',y)

y  = c(rep(0,4),rep(1,4),rep(0,4),rep(1,4))
res = rast('ROC/data_cyril_metaux.txt',1,8,8,'lag','nat',y)
#res = rast('ROC/data_cyril_metaux.txt',1,8,8,'lag','nat')


# Pourquoi la normalisation de R via Norm = scale(x) ne donne pas la même chose qu'en python???? 
# Déjà np.std(x) sous pyton ne donne pas la même chose que sd(x) sous R!! 
# Penser à prévenir pour mon bug de division (pyton)