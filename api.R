# myfile.R
library(base64enc)
library(odbc)
library(ggplot2)
library(reshape2)
library(extrafont)
library(grid)
library(gridExtra)

#* @post /imageForAssetSizePeers
getImageForAssetSizePeers <- function(resF, chartName, institutionId){
	resFline <- melt(resF[,c("period_dt_txt","median_ratio_nat", "median_ratio_nat_peer", "median_ratio_reg_peer")],id="period_dt_txt")

	legend_labels = c("National \nMedian\n", "National \nPeer\n", "Regional \nPeer\n")
	legend_colors = c("#a61d7d", "#a61d7d", "#00529a")
	line_types = c("solid","dashed","dashed")
	
	#makeplot
	resPlot = ggplot() + 

	#plot bar using resF dataset
	geom_bar(data=resF, mapping=aes(x=period_dt_txt, y=ratio), stat="identity", fill="#5c6068", alpha=.5, width = .5) + 
	
	#plot lines using resFline dataset
	geom_line(data=resFline, mapping=aes(x=period_dt_txt, y=value, group= variable, color=variable, linetype = variable), size = 1) +

	#line graph attributes (in order of dataset resFline)
	theme(legend.title= element_blank(), legend.key = element_blank(), legend.background=element_blank(), legend.margin = margin(c(0,0,0,-5)),legend.key.size = unit(5,"pt"), legend.text = element_text(size=6))  +
	scale_color_manual(values=legend_colors,labels=legend_labels)+
	scale_linetype_manual(values=line_types, guide=FALSE) +
	
	#plot areas (go back and string together)
	theme(plot.margin = margin(5,5,10,10,unit="pt"))+
	theme(plot.background=element_rect(fill="#e6e6e6"), panel.background=element_rect(fill="#e6e6e6")) +
	theme(panel.grid.major.y = element_line(color="#d4d4d4")) +
	theme(panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank()) +

	#axis
	theme(text = element_text(family="Roboto Condensed")) +

	labs(y=NULL, x=NULL) +
	scale_y_continuous(expand = c(0,0)) + 
	theme(axis.line = element_line(color="#666666")) + 
	theme(axis.ticks.length = unit(6,"pt")) +
	theme(axis.text= element_text(size=6)) 

	#combine via grid
	resGrid <- arrangeGrob(resPlot)

	grid.arrange(resGrid)

	cleanmetric = sub("/","_to_",chartName)
	cleanmetric = sub(">","+",cleanmetric)
	graph_file = paste("riskcomp_",cleanmetric,"_PEER",sep="")

	resFilename = paste(institutionId," ", graph_file, ".jpg",sep="")
	ggsave(filename=resFilename,resGrid, path = "C:/R scripts/Analytics/PTscore/plots", width = 390/72, height= 158/72)
	
	#return a base64encoded string of the image
	resFilePath = paste("C:/R scripts/Analytics/PTscore/plots/",resFilename,sep="")
	base64encode(readBin(resFilePath, what="raw", n=1e6))
}

#* @post /imageForComparisonsByGroup
getImageForComparisonsByGroup <- function(resF, chartName, institutionId){
	resFline <- melt(resF[,c("period_dt_txt","median_ratio_nat", "median_ratio_reg", "median_ratio_state")],id="period_dt_txt")
	
	legend_labels = c("National \nMedian\n", "Regional \nMedian\n", "State \nMedian\n")
	legend_colors = c("#a61d7d", "#009b9a", "#00529a")
	line_types = c("solid","solid","solid")
	
	#makeplot
	resPlot = ggplot() + 

	#plot bar using resF dataset
	geom_bar(data=resF, mapping=aes(x=period_dt_txt, y=ratio), stat="identity", fill="#5c6068", alpha=.5, width = .5) + 

	#plot lines using resFline dataset
	geom_line(data=resFline, mapping=aes(x=period_dt_txt, y=value, group= variable, color=variable, linetype = variable), size = 1) +

	#line graph attributes (in order of dataset resFline)
	theme(legend.title= element_blank(), legend.key = element_blank(), legend.background=element_blank(), legend.margin = margin(c(0,0,0,-5)),legend.key.size = unit(5,"pt"), legend.text = element_text(size=6))  +
	scale_color_manual(values=legend_colors,labels=legend_labels)+
	scale_linetype_manual(values=line_types, guide=FALSE) +

	#plot areas (go back and string together)
	theme(plot.margin = margin(5,5,10,10,unit="pt"))+
	theme(plot.background=element_rect(fill="#e6e6e6"), panel.background=element_rect(fill="#e6e6e6")) +
	theme(panel.grid.major.y = element_line(color="#d4d4d4")) +
	theme(panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank()) +

	#axis
	theme(text = element_text(family="Roboto Condensed")) +

	labs(y=NULL, x=NULL) +
	scale_y_continuous(expand = c(0,0)) + 
	theme(axis.line = element_line(color="#666666")) + 
	theme(axis.ticks.length = unit(6,"pt")) +
	theme(axis.text= element_text(size=6)) 
	
	#combine via grid
	resGrid <- arrangeGrob(resPlot)

	grid.arrange(resGrid)

	cleanmetric = sub("/","_to_",chartName)
	cleanmetric = sub(">","+",cleanmetric)
	graph_file = paste("riskcomp_",cleanmetric,sep="")

	resFilename = paste(institutionId," ", graph_file, ".jpg",sep="")
	ggsave(filename=resFilename,resGrid, path = "C:/R scripts/Analytics/PTscore/plots", width = 390/72, height= 158/72)
	
	#return a base64encoded string of the image
	resFilePath = paste("C:/R scripts/Analytics/PTscore/plots/",resFilename,sep="")
	base64encode(readBin(resFilePath, what="raw", n=1e6))
}
