
#reading files

annotations= read.table(file= "C:/Users/Aakanksha Choudhary/OneDrive/Desktop/R and stats report/Annotations.csv", header= TRUE, row.names= 1, sep= "\t")
de_gout_v_hc= read.table(file= "C:/Users/Aakanksha Choudhary/OneDrive/Desktop/R and stats report/DE_Gout_vs_HC.csv", header= TRUE, row.names= 1, sep= "\t")
de_sa_v_hc= read.table(file= "C:/Users/Aakanksha Choudhary/OneDrive/Desktop/R and stats report/DE_SA_vs_HC.csv", header= TRUE, row.names= 1, sep= "\t")
exp_table= read.table(file= "C:/Users/Aakanksha Choudhary/OneDrive/Desktop/R and stats report/Expression_Table.csv", header= TRUE, row.names=1, sep="\t")
sample_sheet= read.table(file= "C:/Users/Aakanksha Choudhary/OneDrive/Desktop/R and stats report/Sample_Information.csv", header= TRUE, row.names= 1, sep="\t")

row.names(sample_sheet)= names(exp_table)


#DIVIDING THE VARIABLE COLUMNS BASED ON GROUPS INTO VECTORS
neutrophils_H = sample_sheet$NEUTROPHILS[sample_sheet$GROUP == "HEALTHY"]
neutrophils_G = sample_sheet$NEUTROPHILS[sample_sheet$GROUP == "GOUT"]
neutrophils_S = sample_sheet$NEUTROPHILS[sample_sheet$GROUP == "SA"]

monocytes_H = sample_sheet$MONOCYTES[sample_sheet$GROUP == "HEALTHY"]
monocytes_G = sample_sheet$MONOCYTES[sample_sheet$GROUP == "GOUT"]
monocytes_S = sample_sheet$MONOCYTES[sample_sheet$GROUP == "SA"]

age_H = sample_sheet$AGE[sample_sheet$GROUP == "HEALTHY"]
age_G = sample_sheet$AGE[sample_sheet$GROUP == "GOUT"]
age_S = sample_sheet$AGE[sample_sheet$GROUP == "SA"]

#GETTING THE SUMMARY STATS FOR EACH VARIABLE
summary_neutro_H= summary(neutrophils_H)
summary_neutro_G= summary(neutrophils_G)
summary_neutro_S= summary(neutrophils_S)

summary_mono_H= summary(monocytes_H)
summary_mono_G= summary(monocytes_G)
summary_mono_S= summary(monocytes_S)

summary_age_H= summary(age_H)
summary_age_G= summary(age_G)
summary_age_S= summary(age_S)


#CONVERTING THE SUMMARY STATS OF EACH VARIABLE INTO A DATA FRAME
summary_stats_NEUTROPHILS= data.frame(Group = c("HEALTHY", "GOUT", "SA"),
Neutrophils_Mean = c(summary_neutro_H[["Mean"]], summary_neutro_G[["Mean"]], summary_neutro_S[["Mean"]]),
Neutrophils_Min = c(summary_neutro_H[["Min."]], summary_neutro_G[["Min."]], summary_neutro_S[["Min."]]),
Neutrophils_Max = c(summary_neutro_H[["Max."]], summary_neutro_G[["Max."]], summary_neutro_S[["Max."]]),
Neutrophils_Q1 = c(summary_neutro_H[["1st Qu."]], summary_neutro_G[["1st Qu."]], summary_neutro_S[["1st Qu."]]),
Neutrophils_Q3 = c(summary_neutro_H[["3rd Qu."]], summary_neutro_G[["3rd Qu."]], summary_neutro_S[["3rd Qu."]]),
Neutrophils_Median = c(summary_neutro_H[["Median"]], summary_neutro_G[["Median"]], summary_neutro_S[["Median"]]))

write.csv(summary_stats_NEUTROPHILS, file = "summary_neutro.csv", row.names = FALSE)

summary_stats_MONOCYTES=data.frame(Group = c("HEALTHY", "GOUT", "SA"),
Monocytes_Mean = c(summary_mono_H[["Mean"]], summary_mono_G[["Mean"]], summary_mono_S[["Mean"]]),
Monocytes_Min = c(summary_mono_H[["Min."]], summary_mono_G[["Min."]], summary_mono_S[["Min."]]),
Monocytes_Max = c(summary_mono_H[["Max."]], summary_mono_G[["Max."]], summary_mono_S[["Max."]]),
Monocytes_Q1 = c(summary_mono_H[["1st Qu."]], summary_mono_G[["1st Qu."]], summary_mono_S[["1st Qu."]]),
Monocytes_Q3 = c(summary_mono_H[["3rd Qu."]], summary_mono_G[["3rd Qu."]], summary_mono_S[["3rd Qu."]]),
Monocytes_Median = c(summary_mono_H[["Median"]], summary_mono_G[["Median"]], summary_mono_S[["Median"]]))

write.csv(summary_stats_MONOCYTES, file = "summary_mono.csv", row.names = FALSE)

summary_stats_AGE=data.frame(Group = c("HEALTHY", "GOUT", "SA"),
Age_Mean = c(summary_age_H[["Mean"]], summary_age_G[["Mean"]], summary_age_S[["Mean"]]),
Age_Min = c(summary_age_H[["Min."]], summary_age_G[["Min."]], summary_age_S[["Min."]]),
Age_Max = c(summary_age_H[["Max."]], summary_age_G[["Max."]], summary_age_S[["Max."]]),
Age_Q1 = c(summary_age_H[["1st Qu."]], summary_age_G[["1st Qu."]], summary_age_S[["1st Qu."]]),
Age_Q3 = c(summary_age_H[["3rd Qu."]], summary_age_G[["3rd Qu."]], summary_age_S[["3rd Qu."]]),
Age_Median = c(summary_age_H[["Median"]], summary_age_G[["Median"]], summary_age_S[["Median"]]))

write.csv(summary_stats_AGE, file = "summary_age.csv", row.names = FALSE)


summary(sample_sheet)

#DENSITY PLOT FOR NEUTROPHIL LEVELS BETWEEN 3 GROUPS
neutro_density= ggplot(sample_sheet, aes(x= NEUTROPHILS))+ geom_density()+
  labs(x= "Neutrophil levels", y= "Density of Samples", title= "Variation of Neutrophils")+
  geom_density(colour="white", fill="pink", size=2, alpha=0.5)
print(neutro_density)

#ANOVA FOR NEUTROPHIL LEVELS BETWEEN 3 GROUPS
anova_neutro= aov(NEUTROPHILS~GROUP, data = sample_sheet)
summary(anova_neutro)

#MAKING A BOXPLOT FOR NEUTROPHILS LEVELS BETWEEN 3 GROUPS
neutro_box= ggplot(sample_sheet, aes(x= GROUP, y= NEUTROPHILS, fill= "GROUP"))+ geom_boxplot()+
  geom_jitter(aes(color = GROUP), width = 0.2, alpha = 0.5) +  
  labs(x = "Group", y = "Neutrophil levels", title = "Neutrophil variation")
print(neutro_box)

#DENSITY PLOT FOR MONOCYTE LEVELS BETWEEN 3 GROUPS
mono_density= ggplot(sample_sheet, aes(x= MONOCYTES))+ geom_density()+
  labs(x= "Monocyte levels", y= "Density of Samples", title= "Variation of Monocytes")+
  geom_density(colour="white", fill="pink", size=2, alpha=0.5)
print(mono_density)

#ANOVA FOR MONOCYTE LEVELS BETWEEN 3 GROUPS
anova_mono= aov(MONOCYTES~GROUP, data = sample_sheet)
summary(anova_mono)

#MAKING A BOXPLOT FOR MONOCYTE LEVEL LEVELS BETWEEN 3 GROUPS
mono_box= ggplot(sample_sheet, aes(x= GROUP, y= MONOCYTES, fill= "GROUP"))+ geom_boxplot()+
  geom_jitter(aes(color = GROUP), width = 0.2, alpha = 0.5) +  
  labs(x = "Group", y = "Monocyte levels", title = "Monocyte variation")
print(mono_box)

#DENSITY PLOT FOR AGE LEVELS BETWEEN 3 GROUPS
AGE_density= ggplot(sample_sheet, aes(x= AGE))+ geom_density()+
  labs(x= "samples", y= "AGE", x= "variation of AGE")+
  geom_density(colour="white", fill="pink", size=2, alpha=0.5)
print(AGE_density)

#ANOVA FOR AGE LEVELS BETWEEN 3 GROUPS
anova_age= aov(AGE~GROUP, data = sample_sheet)
summary(anova_age)

#MAKING A BOXPLOT FOR AGE LEVEL LEVELS BETWEEN 3 GROUPS
age_box= ggplot(sample_sheet, aes(x= GROUP, y= AGE, fill= "GROUP"))+ geom_boxplot()+
  geom_jitter(aes(color = GROUP), width = 0.2, alpha = 0.5) +  
  labs(x = "Group", y = "Age", title = "Age Variation")
print(age_box)


#MAKING A BARPLOT FOR SEX BETWEEN 3 GROUPS
sex_bar = ggplot(sample_sheet, aes(x = GROUP, fill = SEX)) +
  geom_bar(position = "dodge") +
  labs(x = "Group", y = "Count", title = "Grouped Bar Chart by Sex") +
  theme_minimal()

print(sex_bar)


#merging annotations and de tables to get the symbols
de_gout_annotated= merge(annotations, de_gout_v_hc, by.x=0, by.y=0)
de_sa_annotated= merge(annotations, de_sa_v_hc, by.x=0, by.y=0)

#changing the row IDs to gene ID

row.names(de_gout_annotated)= de_gout_annotated[,1]
de_gout_annotated= de_gout_annotated[,-1]
row.names(de_sa_annotated)= de_sa_annotated[,1]
de_sa_annotated= de_sa_annotated[,-1]

#making de tables with only significant genes

de_gout_sig= subset(de_gout_annotated, p.adj<0.05)
de_sa_sig= subset(de_sa_annotated, p.adj<0.05)

#making an expression table with symbols

exp_table_annotated_symbol= merge(annotations, exp_table, by.x=0, by.y=0)
row.names(exp_table_annotated_symbol)= exp_table_annotated_symbol[,1]
exp_table_annotated_symbol= exp_table_annotated_symbol[,-1]
exp_table_annotated_symbol= exp_table_annotated_symbol[,-c(2:5)]

#sorting the order of the sig tables so it contains the gene with the lowest p value at the top

sorted_order_gout= order(de_gout_sig[,"p.adj"], decreasing= FALSE)
de_gout_sig= de_gout_sig[sorted_order_gout,]
sorted_order_sa= order(de_sa_sig[,"p.adj"], decreasing= FALSE)
de_sa_sig= de_sa_sig[sorted_order_sa,]

#merging both differential tables
de_merged= merge(de_gout_v_hc, de_sa_v_hc, by.x=0, by.y=0)
row.names(de_merged)= de_merged[,1]
de_merged = de_merged[-1]
names(de_merged)= c("log2fold_gout", "p_gout", "p.adj_gout", "log2fold_sa", "p_sa", "p.adj_sa")


#getting the overlapping genes in de tables for gout and sa

de_sa_gout= merge(de_gout_sig, de_sa_sig, by.x=0, by.y=0)
row.names(de_sa_gout)= de_sa_gout[,1]
de_sa_gout = de_sa_gout[-1]
de_sa_gout = de_sa_gout[,-c(2:5)]
de_sa_gout = de_sa_gout[,-c(5:9)]
names(de_sa_gout)= c("symbol", "log2fold_gout", "p_gout", "p.adj_gout", "log2fold_sa", "p_sa", "p.adj_sa")

#making an expression table with significant genes common to both gout and sa

exp_symbol_sig= merge(de_sa_gout, exp_table, by.x=0, by.y=0)
row.names(exp_symbol_sig)= exp_symbol_sig[,1]
exp_symbol_sig= exp_symbol_sig[,-1]
exp_symbol_sig= exp_symbol_sig[,-c(2:7)]
exp_sig= exp_symbol_sig[,-1]

##PCA FOR GENE EXPRESSION
install.packages("ggplot2")
library(ggplot2)


PCA = prcomp(t(exp_table))
pca_coordinates = data.frame(PCA$x)
pca_plot= ggplot(pca_coordinates, aes(x=PC1, y= PC2, colour= sample_sheet$GROUP))+ geom_point()
print(pca_plot)

variance_pca = (PCA$sdev^2) / sum(PCA$sdev^2)

# Calculate the proportion of variance explained by the first two PCs
variance_pca_first_two = sum(variance_pca[1:2])*100
print(variance_pca_first_two)

PCA_sig = prcomp(t(exp_sig))
pca_coordinates_sig = data.frame(PCA_sig$x)
pca_sig_plot= ggplot(pca_coordinates_sig, aes(x=PC1, y= PC2, colour= sample_sheet$GROUP))+ geom_point()
print(pca_sig_plot)

variance_pca_sig = (PCA_sig$sdev^2) / sum(PCA_sig$sdev^2)
variance_pcasig_first_two = sum(variance_pca_sig[1:2])*100
print(variance_pcasig_first_two)

#getting up and down regulated genes for gout from sig table
up_gout= subset(de_gout_sig, log2fold > 1)
sorted_gout_up= order(up_gout[,"log2fold"], decreasing= TRUE)
up_gout= up_gout[sorted_gout_up,]
down_gout= subset(de_gout_sig, log2fold < -1)
sorted_gout_down= order(down_gout[,"log2fold"], decreasing= FALSE)
down_gout= down_gout[sorted_gout_down,]

top_5_gout_up = subset(up_gout[1:5,])
write.csv(top_5_gout_up, file = "top5gout_up.csv", row.names = FALSE)

top_5_gout_down = subset(down_gout[1:5,])
write.csv(top_5_gout_down, file = "top5gout_down.csv", row.names = FALSE)




reg_plot_gout = ggplot(de_gout_v_hc, aes(x=log2fold, y=-log(p,10))) +
  geom_point(colour = "grey") + 
  geom_point(data=up_gout, colour = "pink") + 
  geom_point(data = down_gout, colour = "lightblue")+
  geom_vline(xintercept=-1,linetype="dashed") +
  geom_vline(xintercept=1,linetype="dashed") +
  geom_hline(yintercept=-log10(0.05),linetype="dashed")+
print(reg_plot_gout)


#getting the up and down regulated genes for sa from sig tables
up_sa= subset(de_sa_sig, log2fold > 1)
sorted_sa_up= order(up_sa[,"log2fold"], decreasing= TRUE)
up_sa= up_sa[sorted_sa_up,]
down_sa= subset(de_sa_sig, log2fold < -1)
sorted_sa_down= order(down_sa[,"log2fold"], decreasing= FALSE)
down_sa= down_sa[sorted_gout_down,]

top_5_sa_up = subset(up_sa[1:5,])
write.csv(top_5_sa_up, file = "top5sa_up.csv", row.names = FALSE)

top_5_sa_down = subset(down_sa[1:5,])
write.csv(top_5_sa_down, file = "top5sa_down.csv", row.names = FALSE)


reg_plot_sa = ggplot(de_sa_v_hc, aes(x=log2fold, y=-log(p,10))) + 
  geom_point(colour = "grey") + 
  geom_point(data=up_sa, colour = "pink") + 
  geom_point(data = down_sa, colour = "lightblue")+
  geom_vline(xintercept=-1,linetype="dashed") +
  geom_vline(xintercept=1,linetype="dashed") +
  geom_hline(yintercept=-log10(0.05),linetype="dashed")
print(reg_plot_sa)

##GENES OF INTEREST
#GENE-1, SYMBOL- HP, ID-ENSG00000257017

gene_HP = exp_table["ENSG00000257017",]
gene_HP = data.frame(t(gene_HP))
names(gene_HP)[1]= "HP"
gene_HP$GROUP= sample_sheet$GROUP
gene_HP$NEUTROPHILS= sample_sheet$NEUTROPHILS
gene_HP$MONOCYTES= sample_sheet$MONOCYTES
gene_HP$SEX= sample_sheet$SEX

#making a density plot for expression

HP_density= ggplot(gene_HP, aes(x= log10(HP))) + geom_density()+
  labs(x= "Expression", y="Density of Samples", title= "Expression for HP")+
  geom_density(colour= "white", fill="pink", size=2, alpha=0.5)
print(HP_density)

#making a boxplot against groups
HP_box= ggplot(gene_HP, aes(x= GROUP, y= log10(HP), fill= "GROUP")) + geom_boxplot()+ 
  geom_jitter(aes(color = GROUP), width = 0.2, alpha = 0.5) +  
  labs(x = "Group", y = "HP Expression", title = "Group v/s HP expression")
print(HP_box)


#making a scatterplot against neutrophils
HP_neutro_scatter= ggplot(gene_HP, aes(x = log10(HP), y = NEUTROPHILS, colour= GROUP))+ geom_point()+
  labs(x = "expression", y= "NEUTROPHILS", title= "NEUTROPHILS v/s exp")+
  scale_color_manual(values = c("pink", "blue", "grey")) + 
  guides(color = guide_legend(title = "Groups"))  
print(HP_neutro_scatter)

#making a scatterplot against monocytes
HP_mono_scatter= ggplot(gene_HP, aes(x = log10(HP), y = MONOCYTES, colour= GROUP))+ geom_point()+
  labs(x = "expression", y= "MONOCYTES", title= "MONOCYTES v/s exp")+
  scale_color_manual(values = c("pink", "blue", "grey")) + 
  guides(color = guide_legend(title = "Groups"))
print(HP_mono_scatter)

transformed_HP= log10(gene_HP[,1])
gene_HP$transformed_HP= transformed_HP


#linear regression of HP gene expression against neutrophils


HP_neutro_line = ggplot(gene_HP, aes(x=transformed_HP, y=NEUTROPHILS)) + geom_point() + 
  labs(x="HP Expression", y="Neutrophil levels", title= "HP V/S NEUTROPHILS") +
  geom_smooth(method = "lm", se = FALSE)+
  geom_text(x = max(gene_HP$transformed_HP)-0.5, y = min(gene_HP$NEUTROPHILS)-5,
            label = paste("Linear Regression: y =", round(coef(HP_lm)[2], 2), "x +", round(coef(HP_lm)[1], 2)))
print(HP_neutro_line)

HP_lm= lm(gene_HP$transformed_HP~gene_HP$NEUTROPHILS)
summary(HP_lm)
anova(HP_lm)

HP_eq = sprintf("y = %.2fx + %.2f (R^2 = %.2f)", coef(HP_lm)[2], coef(HP_lm)[1], summary(HP_lm)$r.squared)

autoplot(HP_lm)


hist(rstandard(HP_lm))


residuals_HP_lm = residuals(HP_lm)
shapiro_residuals_HP= shapiro.test(residuals_HP_lm)
print(shapiro_residuals_HP)


#spearman's correlation for neutrophils and hp expression as not normally distributed residuals
HP_cor= cor.test(gene_HP$HP, gene_HP$NEUTROPHILS, method = "spearman")
print(HP_cor)

#anova for HP gene expression between gout and sa
HP_anova = exp_table["ENSG00000257017",]
HP_anova = data.frame(t(HP_anova))
HP_anova$GROUP = sample_sheet$GROUP
HP_anova$NEUTROPHILS = sample_sheet$NEUTROPHILS
HP_anova = data.frame(t(HP_anova))
HP_anova = HP_anova[,15:42]
HP_anova = data.frame(t(HP_anova))
names(HP_anova)[1]= "HP"

anova_HP= aov(HP~GROUP, data= HP_anova)
summary(anova_HP)

HP_kruskal= kruskal.test(HP ~ GROUP, data = gene_HP)
print(HP_kruskal)

#GENE-2, SYMBOL- S100A8, ID-ENSG00000143546

gene_S100A8 = exp_table["ENSG00000143546",]
gene_S100A8 = data.frame(t(gene_S100A8))
names(gene_S100A8)[1]= "S100A8"
gene_S100A8$GROUP= sample_sheet$GROUP
gene_S100A8$NEUTROPHILS= sample_sheet$NEUTROPHILS
gene_S100A8$MONOCYTES= sample_sheet$MONOCYTES
gene_S100A8$SEX= sample_sheet$SEX

#making a histogram and density plot for expression

S100A8_density= ggplot(gene_S100A8, aes(x= log10(S100A8))) + geom_density()+
  labs(x= "Expression of S100A8", y="Density of Samples", title= "Expression for S100A8")+
  geom_density(colour= "white", fill="pink", size=2, alpha=0.5)
print(S100A8_density)

#making a boxplot against groups
S100A8_box= ggplot(gene_S100A8, aes(x= GROUP, y= log10(S100A8), fill= "GROUP")) + geom_boxplot()+ 
  geom_jitter(aes(color = GROUP), width = 0.2, alpha = 0.5) +  
  labs(x = "Group", y = "Expression for S100A8", title = "S100A8 v/s Groups")
print(S100A8_box)


#making a scatterplot against neutrophils
S100A8_neutro_scatter= ggplot(gene_S100A8, aes(x = log10(S100A8), y = NEUTROPHILS, colour= GROUP))+ geom_point()+
  labs(x = "expression", y= "NEUTROPHILS", title= "NEUTROPHILS v/s exp")+
  scale_color_manual(values = c("pink", "blue", "grey")) + 
  guides(color = guide_legend(title = "Groups"))  
print(S100A8_neutro_scatter)

#making a scatterplot against monocytes
S100A8_mono_scatter= ggplot(gene_S100A8, aes(x = log10(S100A8), y = MONOCYTES, colour= GROUP))+ geom_point()+
  labs(x = "expression", y= "MONOCYTES", title= "MONOCYTES v/s exp")+
  scale_color_manual(values = c("pink", "blue", "grey")) + 
  guides(color = guide_legend(title = "Groups"))
print(S100A8_mono_scatter)

transformed_S100A8= log10(gene_S100A8[,1])
gene_S100A8$transformed_S100A8= transformed_S100A8


#linear regression of HP gene expression against neutrophils

library(ggfortify)

S100A8_neutro_line = ggplot(gene_S100A8, aes(x=transformed_S100A8, y=NEUTROPHILS)) + geom_point() + 
  labs(x="Expression for S100A8", y="Neutrophil levels", title= "Neutrophils v/s S100A8") +
  geom_smooth(method = "lm", se = FALSE)
print(S100A8_neutro_line)

S100A8_lm= lm(gene_S100A8$transformed_S100A8~gene_S100A8$NEUTROPHILS)
summary(S100A8_lm)
anova(S100A8_lm)

autoplot(S100A8_lm)

hist(rstandard(S100A8_lm))

residuals_S100A8_lm = residuals(S100A8_lm)
shapiro_residuals= shapiro.test(residuals_S100A8_lm)
print(shapiro_residuals)

S100A8_cor= cor.test(gene_S100A8$S100A8, gene_S100A8$NEUTROPHILS, method = "spearman")
print(S100A8_cor)

#anova for S100A8 gene expression between gout and sa
S100A8_anova = exp_table["ENSG00000143546",]
S100A8_anova = data.frame(t(S100A8_anova))
S100A8_anova$GROUP = sample_sheet$GROUP
S100A8_anova = data.frame(t(S100A8_anova))
S100A8_anova = S100A8_anova[,15:42]
S100A8_anova = data.frame(t(S100A8_anova))
names(S100A8_anova)[1]= "S100A8"

anova_S100A8= aov(S100A8~GROUP, data= S100A8_anova)
summary(anova_S100A8)

S100A8_kruskal= kruskal.test(S100A8 ~ GROUP, data = gene_S100A8)
print(S100A8_kruskal)


#GENE-3, SYMBOL- FAM20A, ID-"ENSG00000108950"

gene_FAM20A = exp_table["ENSG00000108950",]
gene_FAM20A = data.frame(t(gene_FAM20A))
names(gene_FAM20A)[1]= "FAM20A"
gene_FAM20A$GROUP= sample_sheet$GROUP
gene_FAM20A$NEUTROPHILS= sample_sheet$NEUTROPHILS
gene_FAM20A$MONOCYTES= sample_sheet$MONOCYTES
gene_FAM20A$SEX= sample_sheet$SEX

#making a density plot and density plot for expression


FAM20A_density= ggplot(gene_FAM20A, aes(x= log10(FAM20A))) + geom_density()+
  labs(x= "expression", y="density of samples", title= "expression for FAM20A")+
  geom_density(colour= "white", fill="pink", size=2, alpha=0.5)
print(FAM20A_density)


#making a boxplot against groups
FAM20A_box= ggplot(gene_FAM20A, aes(x= GROUP, y= log10(FAM20A), fill= "GROUP")) + geom_boxplot()+ 
  geom_jitter(aes(color = GROUP), width = 0.2, alpha = 0.5) +  
  labs(x = "Group", y = "HP", title = "Boxplot with Jittered Points")
print(FAM20A_box)


#making a scatterplot against neutrophils
FAM20A_neutro_scatter= ggplot(gene_FAM20A, aes(x = log10(FAM20A), y = NEUTROPHILS, colour= GROUP))+ geom_point()+
  labs(x = "expression", y= "NEUTROPHILS", title= "NEUTROPHILS v/s exp")+
  scale_color_manual(values = c("pink", "blue", "grey")) + 
  guides(color = guide_legend(title = "Groups"))  
print(FAM20A_neutro_scatter)

#making a scatterplot against monocytes
FAM20A_mono_scatter= ggplot(gene_FAM20A, aes(x = log10(FAM20A), y = MONOCYTES, colour= GROUP))+ geom_point()+
  labs(x = "expression", y= "MONOCYTES", title= "MONOCYTES v/s exp")+
  scale_color_manual(values = c("pink", "blue", "grey")) + 
  guides(color = guide_legend(title = "Groups"))
print(FAM20A_mono_scatter)

transformed_FAM20A= log10(gene_FAM20A[,1])
gene_FAM20A$transformed_FAM20A= transformed_FAM20A


#linear regression of HP gene expression against neutrophils

FAM20A_neutro_line = ggplot(gene_FAM20A, aes(x=transformed_FAM20A, y=NEUTROPHILS)) + geom_point() + 
  labs(x="NEUTROPHILS", y="HP") +
  geom_smooth(method = "lm", se = FALSE)
print(FAM20A_neutro_line)

FAM20A_lm= lm(gene_FAM20A$transformed_FAM20A~gene_FAM20A$NEUTROPHILS)
summary(FAM20A_lm)
anova(FAM20A_lm)

hist(rstandard(FAM20A_lm))
residuals_FAM20A_lm = residuals(FAM20A_lm)
shapiro_residuals_FAM20A= shapiro.test(residuals_FAM20A_lm)
print(shapiro_residuals_FAM20A)


#spearman's correlation for neutrophils and hp expression as not normally distributed residuals

FAM20A_cor= cor.test(gene_FAM20A$FAM20A, gene_FAM20A$NEUTROPHILS, method = "spearman")
print(FAM20A_cor)

#getting significantly differential genes between gout and sa
samples_gout= row.names(subset(sample_sheet, GROUP== "GOUT"))
samples_SA= row.names(subset(sample_sheet, GROUP== "SA"))

em_GOUT= exp_table[,samples_gout]
em_SA= exp_table[,samples_SA]


de_gout_v_sa= as.data.frame(matrix(0, ncol=2, nrow= nrow(exp_table)))
names(de_gout_v_sa)= c("p", "log2fold")
row.names(de_gout_v_sa)= row.names(exp_table)


for(row in 1:nrow(exp_table))
{
  gene_data_gout= as.numeric(em_GOUT[row,])
  gene_data_sa= as.numeric(em_SA[row,])
  mean_gout= mean(gene_data_gout)
  mean_SA= mean(gene_data_sa)
  log2fold= log2(mean_gout)- log2(mean_SA)
  p= t.test(gene_data_gout, gene_data_sa)
  p= p$p.value
  de_gout_v_sa[row,"log2fold"]= log2fold
  de_gout_v_sa[row,"p"]= p
}

de_gout_v_sa = merge(annotations, de_gout_v_sa, by.x=0, by.y=0)
row.names(de_gout_v_sa)= de_gout_v_sa[,1]
de_gout_v_sa= de_gout_v_sa[,-1]
de_gout_v_sa = de_gout_v_sa[,-c(2:5)]
sorted_order= order(de_gout_v_sa[,"p"], decreasing= FALSE)
de_gout_v_sa= de_gout_v_sa[sorted_order,]


























