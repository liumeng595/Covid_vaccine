library(dplyr)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(knitr)
library(rmarkdown)
library("NbClust")
library(stringr)
library(ggpubr)
library(ggrepel)
library(grid)
library(gridExtra)
library(ggsci)
library("scales")
library(ggdendro)
library(zoo)
library(pheatmap)

#Load the data
final_DATA<-read.csv("processed_data.csv")
sample_data<-read.csv("clustering.csv")

hc3 <- agnes(sample_data[,-1], method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram", nodePar = list(lab.cex = 0.6, lab.col = "forest green", pch = NA)) 
rect.hclust(hc3, k = 6, border = 2:7)
hc3$ac
########################################################
#Dendrogram
dendr <- dendro_data(hc3, type = "rectangle") 
clust <- cutree(hc3, k = 6)               # find 'cut' clusters
cut<-6
clust.df <- data.frame(label = names(clust), cluster = clust)

height <- unique(dendr$segments$y)[order(unique(dendr$segments$y), decreasing = TRUE)]
cut.height <- mean(c(height[cut], height[cut-1]))
dendr$segments$line <- ifelse(dendr$segments$y == dendr$segments$yend &
                                dendr$segments$y > cut.height, 1, 2)
dendr$segments$line <- ifelse(dendr$segments$yend  > cut.height, 1, dendr$segments$line)

# Number the clusters
dendr$segments$cluster <- c(-1, diff(dendr$segments$line))
change <- which(dendr$segments$cluster == 1)
for (i in 1:cut) dendr$segments$cluster[change[i]] = i + 1
dendr$segments$cluster <-  ifelse(dendr$segments$line == 1, 1, 
                                  ifelse(dendr$segments$cluster == 0, NA, dendr$segments$cluster))
dendr$segments$cluster <- na.locf(dendr$segments$cluster) 

# Consistent numbering between segment$cluster and label$cluster
clust.df$label <- factor(clust.df$label, levels = levels(dendr$labels$label))
clust.df <- arrange(clust.df, label)
clust.df$cluster <- factor((clust.df$cluster), levels = unique(clust.df$cluster), labels = (1:cut) + 1)
dendr[["labels"]] <- merge(dendr[["labels"]], clust.df, by = "label")

# Positions for cluster labels
n.rle <- rle(dendr$segments$cluster)
N <- cumsum(n.rle$lengths)
N <- N[seq(1, length(N), 2)] + 1
N.df <- dendr$segments[N, ]
N.df$cluster <- N.df$cluster - 1


p8<-ggplot() + 
  geom_segment(data = segment(dendr), 
               aes(x=x, y=y, xend=xend, yend=yend, size=factor(line), colour=factor(cluster)), 
               lineend = "square", show.legend = FALSE) + 
  scale_colour_manual(values = c("grey60", rainbow(cut))) +
  scale_size_manual(values = c(.1, 1)) +
  geom_text(data = N.df, aes(x = x, y = y, label = factor(cluster),  colour = factor(cluster + 1)), 
            hjust = 1.5, show.legend = FALSE) +
  geom_text(data = label(dendr), aes(x, y, label = label, colour = factor(cluster)), 
            hjust = -0.2, size = 5, show.legend = FALSE) +
  #scale_y_reverse(expand = c(0.2, 0)) + 
  labs(x = NULL, y = NULL) +
  scale_color_jama()+
  scale_fill_jama() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        # panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),plot.title = element_text(hjust = 0.05))+
  ggtitle("Dendrogram of Hierarchical Clustering")
p8


sub_grp <- cutree(hc3, k = 6)
membership<- table(sub_grp)%>%as.data.frame()
colnames(membership)<-c("Group ID", "Member number")
paged_table(membership)
sample_data<-sample_data%>%
  mutate(cluster = sub_grp)



sample_data<-arrange(sample_data,cluster)


cluster<-sample_data
cluster$AGE_YRS<-final_DATA$AGE_YRS[match(cluster$VAERS_ID,final_DATA$VAERS_ID)]
cluster$NUMDAYS<-final_DATA$NUMDAYS[match(cluster$VAERS_ID,final_DATA$VAERS_ID)]
########################################################
#Age_cluster
cluster$cluster<-as.factor(cluster$cluster)

age_fivenum <- cluster %>% 
  group_by(cluster) %>% 
  summarise(five = list(fivenum(AGE_YRS))) %>% 
  tidyr::unnest()

p9<-ggplot(cluster, aes(x=cluster, y=AGE_YRS)) + 
  geom_boxplot(aes(fill=factor(cluster)),width = 0.5) + 
  scale_color_jama()+
  scale_fill_jama() +
  geom_text(data = age_fivenum, size = 5,
            aes(x = factor(cluster), y = five, label = five), 
            nudge_x = 0,nudge_y = 1.2)+
  ggtitle("Age Distribution by Clusters (Boxplot)")+
  xlab("Cluster") + ylab("Age (Year)")
########################################################
#DAYS_cluster

N_days_fivenum <- cluster %>% 
  group_by(cluster) %>% 
  summarise(five = list(fivenum(NUMDAYS))) %>% 
  tidyr::unnest()


p10<-ggplot(cluster, aes(x=cluster, y=NUMDAYS)) + 
  geom_boxplot(aes(fill=factor(cluster)),width = 0.5) + 
  scale_color_jama()+
  scale_fill_jama() +
  # geom_text(data = N_days_fivenum, size = 5,aes(x = factor(cluster), y = five, label = five), 
  #          position = position_stack(vjust = -1))+
  ggtitle("#Days by Clusters (Boxplot)")+
  ylim(0, 7)+
  xlab("Cluster") + ylab("#Days (Days)")
########################################################
#location_cluster


place_table<-cluster%>%
  arrange(AGE_YRS,cluster)%>% group_by(V_ADMINBY,cluster)%>%summarise(num_age=n())%>% group_by(V_ADMINBY)%>%mutate(count= sum(num_age))%>%
  group_by(cluster, add=TRUE) %>%
  mutate(per=paste0(round(100*num_age/count,1),'%'))%>%
  mutate(per_value=round(num_age/count,3))

mypal <- pal_jama("default")(7)
mypal <- c(mypal,"#7E6148FF","#8491B4FF")
p11<-ggplot(place_table, aes(fill=V_ADMINBY, y=num_age, x=cluster)) + 
  geom_bar(position = "fill",stat="identity",width = 0.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=mypal)+
  #geom_text(aes( y=per_value,label = per), size = 5,position =  position_fill())+
  ggtitle("Location Distribution by Clusters (Percentage Stacked barplot)")+
  
  xlab("Cluster") + ylab("Cases Percentage")



#ggarrange(p_h,p5, p6, p7,
# ncol = 2, nrow = 2, align = "v",labels = c("A","B", "C","D"))

#Heat map for most comment symptoms in the clusters
#merge "Unresponsive to stimuli" to "Death"
final_SYMPTOMS<-read.csv("Processed_SYMPTOMS.csv")
final_SYMPTOMS$SYMPTOM1[final_SYMPTOMS$SYMPTOM1=="Unresponsive to stimuli"]<-"Death"
final_SYMPTOMS$SYMPTOM2[final_SYMPTOMS$SYMPTOM2=="Unresponsive to stimuli"]<-"Death"
final_SYMPTOMS$SYMPTOM3[final_SYMPTOMS$SYMPTOM3=="Unresponsive to stimuli"]<-"Death"
final_SYMPTOMS$SYMPTOM4[final_SYMPTOMS$SYMPTOM4=="Unresponsive to stimuli"]<-"Death"
final_SYMPTOMS$SYMPTOM5[final_SYMPTOMS$SYMPTOM5=="Unresponsive to stimuli"]<-"Death"


N1<-arrange(as.data.frame(table(c(final_SYMPTOMS$SYMPTOM1[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==1]],
                                  final_SYMPTOMS$SYMPTOM2[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==1]],
                                  final_SYMPTOMS$SYMPTOM3[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==1]],
                                  final_SYMPTOMS$SYMPTOM4[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==1]],                                 final_SYMPTOMS$SYMPTOM5[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==1]]))),desc(Freq))



N2<-arrange(as.data.frame(table(c(final_SYMPTOMS$SYMPTOM1[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==2]],
                                  final_SYMPTOMS$SYMPTOM2[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==2]],
                                  final_SYMPTOMS$SYMPTOM3[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==2]],
                                  final_SYMPTOMS$SYMPTOM4[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==2]],
                                  final_SYMPTOMS$SYMPTOM5[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==2]]))),desc(Freq))


N3<-arrange(as.data.frame(table(c(final_SYMPTOMS$SYMPTOM1[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==3]],
                                  final_SYMPTOMS$SYMPTOM2[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==3]],
                                  final_SYMPTOMS$SYMPTOM3[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==3]],
                                  final_SYMPTOMS$SYMPTOM4[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==3]],
                                  final_SYMPTOMS$SYMPTOM5[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==3]]))),desc(Freq))


N4<-arrange(as.data.frame(table(c(final_SYMPTOMS$SYMPTOM1[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==4]],
                                  final_SYMPTOMS$SYMPTOM2[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==4]],
                                  final_SYMPTOMS$SYMPTOM3[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==4]],
                                  final_SYMPTOMS$SYMPTOM4[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==4]],
                                  final_SYMPTOMS$SYMPTOM5[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==4]]))),desc(Freq))



N5<-arrange(as.data.frame(table(c(final_SYMPTOMS$SYMPTOM1[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==5]],
                                  final_SYMPTOMS$SYMPTOM2[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==5]],
                                  final_SYMPTOMS$SYMPTOM3[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==5]],
                                  final_SYMPTOMS$SYMPTOM4[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==5]],
                                  final_SYMPTOMS$SYMPTOM5[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==5]]))),desc(Freq))


N6<-arrange(as.data.frame(table(c(final_SYMPTOMS$SYMPTOM1[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==6]],
                                  final_SYMPTOMS$SYMPTOM2[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==6]],
                                  final_SYMPTOMS$SYMPTOM3[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==6]],
                                  final_SYMPTOMS$SYMPTOM4[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==6]],
                                  final_SYMPTOMS$SYMPTOM5[final_SYMPTOMS$VAERS_ID%in%cluster$VAERS_ID[cluster$cluster==6]]))),desc(Freq))
colnames(N1)<-c("Symptoms of Group 1", "Frequency")
paged_table(N1)

colnames(N2)<-c("Symptoms of Group 2", "Frequency")
paged_table(N2)

colnames(N3)<-c("Symptoms of Group 3", "Frequency")
paged_table(N3)

colnames(N4)<-c("Symptoms of Group 4", "Frequency")
paged_table(N4)

colnames(N5)<-c("Symptoms of Group 5", "Frequency")
paged_table(N5)

colnames(N6)<-c("Symptoms of Group 6", "Frequency")
paged_table(N6)

N1$`Symptoms of Group 1`<-as.character(N1$`Symptoms of Group 1`)
N2$`Symptoms of Group 2`<-as.character(N2$`Symptoms of Group 2`)
N3$`Symptoms of Group 3`<-as.character(N3$`Symptoms of Group 3`)
N4$`Symptoms of Group 4`<-as.character(N4$`Symptoms of Group 4`)
N5$`Symptoms of Group 5`<-as.character(N5$`Symptoms of Group 5`)
N6$`Symptoms of Group 6`<-as.character(N6$`Symptoms of Group 6`)

tep<-table(cluster$cluster)%>%as.data.frame()
N1$per<-round(N1$Frequency/tep$Freq[1],2)
N2$per<-round(N2$Frequency/tep$Freq[2],2)
N3$per<-round(N3$Frequency/tep$Freq[3],2)
N4$per<-round(N4$Frequency/tep$Freq[4],2)
N5$per<-round(N5$Frequency/tep$Freq[5],2)
N6$per<-round(N6$Frequency/tep$Freq[6],2)





stp_name<-unique( c(N1$`Symptoms of Group 1`[1:5],
                    N2$`Symptoms of Group 2`[1:5],
                    N3$`Symptoms of Group 3`[1:5],
                    N4$`Symptoms of Group 4`[1:5],
                    N5$`Symptoms of Group 5`[1:5],
                    N6$`Symptoms of Group 6`[1:5]))
HEAT_MAP<-data.frame(Adverse_Event=stp_name,matrix(NA,ncol = 6, nrow = length(stp_name)))
colnames(HEAT_MAP)<-c("Adverse_Event",paste0("Cluster",1:6))


HEAT_MAP$Cluster1<-N1$per[match(HEAT_MAP$Adverse_Event,N1$`Symptoms of Group 1`)]
HEAT_MAP$Cluster2<-N2$per[match(HEAT_MAP$Adverse_Event,N2$`Symptoms of Group 2`)]
HEAT_MAP$Cluster3<-N3$per[match(HEAT_MAP$Adverse_Event,N3$`Symptoms of Group 3`)]
HEAT_MAP$Cluster4<-N4$per[match(HEAT_MAP$Adverse_Event,N4$`Symptoms of Group 4`)]
HEAT_MAP$Cluster5<-N5$per[match(HEAT_MAP$Adverse_Event,N5$`Symptoms of Group 5`)]
HEAT_MAP$Cluster6<-N6$per[match(HEAT_MAP$Adverse_Event,N6$`Symptoms of Group 6`)]

#install.packages('pheatmap')
rownames(HEAT_MAP)<-HEAT_MAP$Adverse_Event

p12<-  pheatmap(HEAT_MAP[,-1], display_numbers = T,main="Top 10 Frequent Adverse Event of Clusters",
                fontsize_number =25,clustering_method="ward")



###########################################################################################################################
###Covid infection cluster

Test_positive_table<-cluster%>% group_by(Covid_involved,cluster)%>%summarise(num_age=n())%>% group_by(cluster)%>%mutate(count= sum(num_age))%>%
  group_by(Covid_involved, add=TRUE) %>%
  mutate(per=paste0(round(100*num_age/count,1),'%'))%>%
  mutate(per_value=round(num_age/count,3))
Test_positive_table$Covid_involved<-as.character(Test_positive_table$Covid_involved)
Test_positive_table$Covid_involved[1:6]<-toupper("YES")
Test_positive_table$Covid_involved[7:12]<-"NO or UNKNOWN"
Test_positive_table$Covid_involved<-as.factor(Test_positive_table$Covid_involved)
Test_positive_table$Covid_involved<-factor(Test_positive_table$Covid_involved, levels=c(toupper("YES"),"NO or UNKNOWN" ))

color_2<-c("#B24745FF","#00A1D5FF")

paged_table(Test_positive_table)
colnames(Test_positive_table)[1]<-"Covid Infection"
p13<-ggplot(Test_positive_table, aes(fill=`Covid Infection`, y=num_age, x=cluster)) +
  geom_bar(position = "fill",stat="identity",width = 0.7)+
  scale_y_continuous(labels = scales::percent) +
  scale_color_jama()+
  scale_fill_jama()+
  geom_text(aes( y=per_value,label = per), size = 3, position = position_stack(vjust = 0.5))+
  xlab("Cluster") + ylab("Cases Percentage")+
  ggtitle("Covid Infection Grouped by Clusters")
p13<-set_palette(p13, palette =color_2)

#######################################################
#history

HISTORY_table<-cluster%>% group_by(HISTORY_summary,cluster)%>%summarise(num_age=n())%>% group_by(cluster)%>%mutate(count= sum(num_age))%>%
  group_by(HISTORY_summary, add=TRUE) %>%
  mutate(per=paste0(round(100*num_age/count,1),'%'))%>%
  mutate(per_value=round(num_age/count,3))
HISTORY_table$HISTORY_summary<-as.character(HISTORY_table$HISTORY_summary)
HISTORY_table$HISTORY_summary[1:6]<-"NO or UNKNOWN"
HISTORY_table$HISTORY_summary[7:12]<-toupper("Yes")
HISTORY_table$HISTORY_summary<-as.factor(HISTORY_table$HISTORY_summary)
HISTORY_table$HISTORY_summary<-factor(HISTORY_table$HISTORY_summary, levels=c(toupper("Yes"),"NO or UNKNOWN" ))

colnames(HISTORY_table)[1]<-"Pre-existing Conditions"

p14<-ggplot(HISTORY_table, aes(fill=`Pre-existing Conditions`, y=num_age, x=cluster)) +
  geom_bar(position = "fill",stat="identity",width = 0.7)+
  scale_color_jama()+
  scale_fill_jama()+
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes( y=per_value,label = per), size = 3, position = position_stack(vjust = 0.5))+
  xlab("Cluster") + ylab("Cases Percentage")+
  ggtitle("Pre-existing Conditions Grouped by Clusters")
p14<-set_palette(p14, palette =color_2) 

#############################
#local cluster


local_reactogenic_group<-cluster%>%
  arrange(AGE_YRS,local.reactogenic)%>% group_by(local.reactogenic,cluster)%>%summarise(num_age=n())%>% group_by(cluster)%>%mutate(count= sum(num_age))%>%
  group_by(local.reactogenic, add=TRUE) %>%
  mutate(per=paste0(round(100*num_age/count,1),'%'))%>%
  mutate(per_value=round(num_age/count,3))

paged_table(local_reactogenic_group)
local_reactogenic_group$local.reactogenic<-as.character(local_reactogenic_group$local.reactogenic)
local_reactogenic_group$local.reactogenic[local_reactogenic_group$local.reactogenic=="NO"]<-"NO or UNKNOWN"
local_reactogenic_group$local.reactogenic<-as.factor(local_reactogenic_group$local.reactogenic)

local_reactogenic_group$local.reactogenic<-factor(local_reactogenic_group$local.reactogenic, levels=c("YES" ,"NO or UNKNOWN" ))
colnames(local_reactogenic_group)[1]<-"Local Reactogenic"

p15<-ggplot(local_reactogenic_group, aes(fill=local.reactogenic, y=num_age, x=cluster)) + 
  geom_bar(position = "fill",stat="identity",width = 0.7)+
  scale_y_continuous(labels = scales::percent) +
  scale_color_jama()+
  scale_fill_jama()+
  geom_text(aes( y=per_value,label = per), size = 3, position = position_stack(vjust = 0.5))+
  xlab("Clusters") + ylab("Cases Percentage")+
  ggtitle("Local Reactogenic Grouped by Clusters")
p15<-set_palette(p15, palette =color_2) 
#grid.arrange(p1, p2,ncol = 2,top = textGrob("Local Reactogenic Grouped by Clusters",gp=gpar(fontsize=20,font=3)))

###########################################################################
#p16



Systemic_Reactogenic_group<-cluster%>%
  arrange(AGE_YRS,Systemic.Reactogenic)%>% group_by(Systemic.Reactogenic,cluster)%>%summarise(num_age=n())%>% group_by(cluster)%>%mutate(count= sum(num_age))%>%
  group_by(Systemic.Reactogenic, add=TRUE) %>%
  mutate(per=paste0(round(100*num_age/count,1),'%'))%>%
  mutate(per_value=round(num_age/count,3))

paged_table(Systemic_Reactogenic_group)
Systemic_Reactogenic_group$Systemic.Reactogenic<-as.character(Systemic_Reactogenic_group$Systemic.Reactogenic)
Systemic_Reactogenic_group$Systemic.Reactogenic[1:6]<-"NO or UNKNOWN"
Systemic_Reactogenic_group$Systemic.Reactogenic<-as.factor(Systemic_Reactogenic_group$Systemic.Reactogenic)
Systemic_Reactogenic_group$Systemic.Reactogenic<-factor(Systemic_Reactogenic_group$Systemic.Reactogenic, levels=c("YES","NO or UNKNOWN" ))


p16<-ggplot(Systemic_Reactogenic_group, aes(fill=Systemic.Reactogenic, y=num_age, x=cluster)) + 
  geom_bar(position = "fill",stat="identity",width = 0.7)+
  scale_y_continuous(labels = scales::percent) +
  scale_color_jama()+
  scale_fill_jama()+
  geom_text(aes( y=per_value,label = per), size = 3, position = position_stack(vjust = 0.5))+
  xlab("Cluster") + ylab("Cases Percentage")+
  ggtitle("Systemic Reactogenic Grouped by Clusters")
p16<-set_palette(p16, palette =color_2) 
