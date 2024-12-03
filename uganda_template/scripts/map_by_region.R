adm1_geojson<-snakemake@input[["adm1"]]
input_csv<-snakemake@input[["input_csv"]]
annotations_file<-snakemake@input[['annotations_file']]
regional_mutation<-snakemake@output[["regional_mutation"]]
join_on<-snakemake@params[["join_column"]]
breakpoints<-snakemake@params[["breakpoints"]]
shading_colors<-snakemake@params[["shading_colors"]]
labels<-snakemake@params[["labels"]]
longitudes<-snakemake@params[["longitudes"]]
latitudes<-snakemake@params[["latitudes"]]
mutation<-snakemake@wildcards[["mutation"]]
country_name<-snakemake@params[['country_name']]

#non-snakemake parameter passing for troubleshooting purposes in Rstudio

# adm1_geojson<-'input_shape_files/alternative_shape_files/tanzania-with-regions_1525.geojson'
# input_csv<-'input_tables/alternative_tables/prevalences_by_region_2021.csv'
# annotations_file<-'input_tables/annotations.csv'
# regional_mutation<-'output_files/2021-k13_Arg561His-prevalences.png'
# join_on<-"name"
# breakpoints<-c(-10,-0.0001,0.0001,2,4,6,100)
# shading_colors<-c("#FAFBF9", "#CCD1D1", "#FFD99FB3", "#FFAC7CB3", "#FB796CB3", "#E74075B3")
# labels<-c("not sampled", "0", "<2%", "<4%", "<6%", ">6%")
# longitudes<-c(29.5, 40.2)
# latitudes<-c(-1,-12)
# mutation<-'k13_Arg561His'
################################################
#prevalence choropleths
################################################
#load required libraries for base maps

#how to install the needed packages in R if not already present
#conda install -c conda-forge r-[package_name]
#library(dplyr, warn.conflicts = F)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(rnaturalearth))
suppressPackageStartupMessages(library(rnaturalearthdata))
library(sf) #see ubuntu issues here: https://rtask.thinkr.fr/installation-of-r-4-0-on-ubuntu-20-04-lts-and-tips-for-spatial-packages/
library(ggspatial)
library(ggrepel)
library(patchwork)

#rivers10 <- ne_download(scale = 10, type = 'rivers_lake_centerlines', 
#                        category = 'physical', returnclass = "sf")
lakes10 <- ne_download(scale = "medium", type = 'lakes', 
                       category = 'physical', returnclass = "sf") #useful to have these two for points of reference
#oceans10 <- ne_download(scale = "medium", type = "coastline",
#                        category = 'physical', returnclass = "sf")
sov110 <- (ne_download(scale="medium", type = "sovereignty",
                      category = "cultural", returnclass = "sf"))
admin1 <- read_sf(adm1_geojson)
##########################################
#561H national plot
##########################################
clinical_variants <- read.csv(input_csv)

annotations <- read.csv(annotations_file)
#annotations <- read.csv('input_tables/annotations.csv')

add_annotation <- function(row) {
  annotate(
    "text",
    x = as.numeric(row["x"]),
    y = as.numeric(row["y"]),
    fontface = row["fontface"],
    color = row["color"],
    label = row["label"],
    size = as.numeric(row["size"])
  )
}

#for user viewing only - not used by the script itself
#write.table(admin1, 'admin1_viewable.tsv', sep='\t')

print('countries are')
print(sov110$ADMIN)
print('*****double check the names of countries above to make sure one of them matches your country of interest in your config file******')


print('valid names are')
print(admin1[[join_on]])
print('*****double check the names of administrative regions above to make sure they match the ones in your prevalence csv file******')
#print('program will continue in 10 seconds')
#Sys.sleep(10)
tanz_regions <- dplyr::right_join(admin1,clinical_variants, by = join_on)


#make percent
tanz_regions[mutation] <- tanz_regions[[mutation]] * 100
#tanz_regions$k13_Arg561His <- tanz_regions$k13_Arg561His * 100

# Create breaks and labels
br <- c(breakpoints)
#br <- c(-0.1,0,1,2,4,6,8)
palzero <- c(shading_colors)
#custom labels
labs <- c(labels)

#discretize values
tanz_regions[mutation] <- cut(tanz_regions[[mutation]],
                             breaks = br,
                             dig.lab = 5)

#for user view only - shows the number of values that went into each category
#summary(tanz_regions[mutation])
#print(tanz_regions[mutation])
#new_regions<-tanz_regions
#new_regions$geometry <- NULL
#write.table(new_regions, "tanz_regions.tsv", sep="\t", col.names=TRUE, row.names=FALSE)

#plot figure 2A
ggplot()+
#fig2a <- ggplot()+

#  geom_sf(data=sov110, color='grey30', size=0.8, alpha = 0.2, linewidth = 1,
  #country borders - border color is grey30, Tanzania is white, other countries seagreen
  geom_sf(data=sov110, color='grey30', size=0.8, alpha = 0.2, linewidth = 1,
          fill = ifelse(sov110$ADMIN == country_name, 'white', 'seagreen1')) +

  # geom_sf(data=country_bbox, color='grey30', size=0.8, alpha = 0.2, linewidth = 1,
  #         fill = ifelse(africa_map$iso_a3 == "TZA", 'white', 'seagreen1')) +

#fill in the regions of Tanzania according to the discretized colors above
  geom_sf(data = tanz_regions, aes(fill = .data[[mutation]]), color = NA, show.legend=TRUE) +

#fill in the scale bar
  scale_fill_manual(values = palzero, drop = FALSE, na.value = "grey96",
                    labels = labs, na.translate = FALSE, name= paste(mutation, "Prevalence", sep="\n"),
                    guide = guide_legend(direction = "vertical",
                                         nrow = 11, label.position = "right", 
                                         show.legend=TRUE, alpha=TRUE)) +
  #fill in the lakes as light blue
  geom_sf(data=lakes10, color="grey40", fill ="lightblue", size= 0.8) +

  apply(annotations, 1, add_annotation) +
  #add Malawi label - hardcoded
  geom_sf(data=admin1, color="grey40", size= 0.6, alpha = 0.1) +
  #coordinates to plot - hardcoded
  coord_sf(xlim = c(longitudes), ylim = c(latitudes), expand = TRUE) +
  theme_void()+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))

#save plot
#ggsave("f.svg", dpi=600, width=6, height=5)
ggsave(regional_mutation, dpi=600, width=6, height=5)
