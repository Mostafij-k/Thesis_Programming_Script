# Draw heatmap

#install.packages("RColorBrewer")
#install.packages("pheatmap")

library(ggplot2) 		
library(RColorBrewer)  # Help to extract different col0r
library(pheatmap)

# Upload Data
dataset= read.csv('heatmap_logFC.csv', header = TRUE, sep = ',')
row.names(dataset)= dataset$Gene
colnames(dataset)= c('Gene', 'GSE122697_logFC', 'GSE63514_logFC','GSE64217_logFC',
                     'GSE67522_logFC', 'GSE40595_logFC', 'GSE105437_logFC', 'GSE12470_logFC',
                     'GSE36668_logFC', 'GSE17025_logFC', 'GSE3013_logFC', 'GSE56087_LogFC', 'GSE63678_LoFC')
dataset= dataset[, -1]
dataset= data.matrix(dataset)   # Convert dataframe to matrix

        # Construct heatmap
help(pheatmap)  # open help page

pheatmap(dataset, border_color = 'red',
         cellwidth = 40,
         fontsize_col = 15,
         scale='column')

C= colorRampPalette(rev(brewer.pal(n = 7, name =
                                              "RdYlBu")))(100)  # Default color for pheatmap

pheatmap(dataset, border_color = C,
         cellwidth = 40,
         fontsize_col = 15,
         scale='column')



# Use different color
display.brewer.all()   # It shows all available color palette 


greys= colorRampPalette(brewer.pal(9, 'Greys'))(100)

pheatmap(dataset, border_color = 'red',
         cellwidth = 40,
         fontsize_col = 15,
         scale='column',
         color = greys)

pairs= colorRampPalette(brewer.pal(9, 'Paired'))(100)

pheatmap(dataset, border_color = 'red',
         cellwidth = 40,
         fontsize_col = 15,
         scale='column',
         color = pairs)

purd= colorRampPalette(brewer.pal(7, 'PuRd'))(100)


pheatmap(dataset, border_color = 'red',
         cellwidth = 40,
         fontsize_col = 15,
         scale='column',
         color = purd)


blue= colorRampPalette(brewer.pal(7, 'Blues'))(100)


pheatmap(dataset, border_color = 'red',
         cellwidth = 40,
         fontsize_col = 15,
         scale='column',
         color = blue)



pheatmap(dataset, border_color = 'red',
         cellwidth = 40,
         fontsize_col = 18,
         color = pairs,
         scale='column',
         cluster_rows = FALSE,
         display_numbers = TRUE)



SET1 = colorRampPalette(brewer.pal(5, 'RdYlGn'))(100)


pheatmap(dataset, border_color = 'red',
         cellwidth = 40,
         fontsize_col = 18,
         color = SET1,
         cluster_rows = FALSE,
         display_numbers = TRUE)

SET2 = colorRampPalette(brewer.pal(5, 'Reds'))(100)

pheatmap(dataset, border_color = 'red',
         cellwidth = 40,
         fontsize_col = 18,
         color = SET2,
         cluster_rows = FALSE,
         display_numbers = TRUE)

SET3 = colorRampPalette(brewer.pal(5, 'Blues'))(100)

pheatmap(dataset, border_color = 'red',
         cellwidth = 40,
         fontsize_col = 18,
         color = SET3,
         cluster_rows = FALSE,
         display_numbers = TRUE)









