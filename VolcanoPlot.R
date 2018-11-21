library(ggplot2)
library(ggrepel)
data=read.table("/home/fyh/Desktop/RNA_edit/lab_work/Vol_graph/DIFF_DMSO/gene_exp.diff.chg",header = T)
gene=data$gene
gene_log2FC=data$log2.fold_change.
gene_logQ=-log10(data$q_value)
boolean_isInf=is.infinite(gene_log2FC)
filter_gene=as.vector(gene[!boolean_isInf])
filter_gene_log2FC=gene_log2FC[!boolean_isInf]
filter_gene_logQ=gene_logQ[!boolean_isInf]
significant=data$significant[!boolean_isInf]
filter_data=data.frame(filter_gene,filter_gene_log2FC,filter_gene_logQ)
boolean_tag=(filter_gene_log2FC>5 | filter_gene_log2FC< -4.5) & filter_gene_logQ>2.5
#tag_lst=c("c-myc","Ccnd1","Fos","Jun","Dusp1")
#tag_lst=c("Ocln","Itgal","Dlg5","Lipg","Cndp2","Ndst2")
#index_tag=which(!filter_gene %in% tag_lst)
#filter_gene[index_tag]=""
filter_gene[which(boolean_tag==F)]=""
figure=ggplot(filter_data,aes(x=filter_gene_log2FC,y=filter_gene_logQ))
figure+geom_point(aes(color=significant))+scale_color_manual(values = c("#377EB8","#999999","#E41A1C"))+labs(title="Volcanoplot-DMSO",x="log2FC",y="-log10(FDR)")+geom_hline(yintercept = 1.3,linetype=3)+geom_vline(xintercept=c(-1.5,1.5),linetype=3)+geom_text_repel(label=filter_gene)
