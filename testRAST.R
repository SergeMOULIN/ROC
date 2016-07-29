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
res = rast('data/example1.txt',2,8,8,'setosa','versicolor')
res = rast('data/example1.txt',2,8,8,'setosa','versicolor',nb_of_nonzero = TRUE)
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
res = rast('data/example2.txt',1,8,8,'setosa','versicolor',y, nom_output = "ROC_table_2.xls")
