Plot gallery
================
Claire Levy
March 22, 2016

Stacked bar
-----------

CD66b+ populations from "plotting neutrophil subsets flow and April-May CyToF" in the GAPPS-analysis project

``` r
ggplot(meltCombinedData, aes(x=Sample, y = CD66bPos))+
  geom_bar(aes(fill = Population),stat="identity")+
  labs(y="Frequency (Percent)")+
  theme(axis.text.x=element_text(vjust=0.8, size=8),
        axis.title=element_text(size= 12),
        legend.title=element_text(size=12),
        axis.title.x=element_text(vjust=.2),
        plot.title = element_text(vjust=2),
        aspect.ratio = 0.6)+
    ggtitle("Frequency of CD66b+ sub-populations")
```

![](Plot_gallery_files/figure-markdown_github/unnamed-chunk-2-1.png)

Stacked bar with pretty colors
------------------------------

Populations found using testing CyToF with PBMC and vaginal cells from "30Apr15 CyTOF data.R" in GAPPS-analysis project

``` r
ggplot(freqMeltStackedBar, aes(x=NewSampleName, y = Frequency))+
  geom_bar(aes(fill=Cell.Type),stat="identity")+
  labs(x="Sample")+
  scale_fill_manual(name="Cell Type",values=c("#1b9e77","#4daf4a","#984ea3",
                             "#e7298a","#80cdc1"))+
  theme(axis.text.x=element_text(angle=20,vjust=.8, size=10),
        axis.title=element_text(size= 15),
        legend.title=element_text(size=15),
        axis.title.x=element_text(vjust=.5),
        plot.title = element_text(vjust=2))+
  ggtitle("Frequency of (live) cell types in cytobrushes, PBMC and vaginal
cell samples")
```

![](Plot_gallery_files/figure-markdown_github/unnamed-chunk-4-1.png)

Lotsa dots plus custom error bars
---------------------------------

qPCR results showing all donors and mean + se. From: J:SHAREDstudy-Project-1-qPCRresults.Rmd

The black vertical lines are showing the range of the SE on either side of the mean. This is done using the following:

`stat_summary(fun.data = mean_se, geom = "linerange")`

The black horizontal lines are actually a "+" symbol at the mean. This is done using the following:

`stat_summary(fun.y = mean, geom = "point",shape = 3, size = 6)`

To customize the x and y axis labels I used:

`labs(y = "Fold Change relative to CT of 40 \n (log 10 scale)", x = "Time (hrs)")`

``` r
ggplot(explantAnalysis,(aes(x = Time, y = log10(FoldChange))))+
  geom_point(aes(color = TissueID),size = 3.5 )+
  geom_line(aes(color = TissueID, group = TissueID),size=0.5)+
  scale_color_discrete (name = "Tissue ID")+
  stat_summary(fun.data = mean_se, geom = "linerange")+
  stat_summary(fun.y = mean, geom = "point",shape = 3, size = 6)+
theme(axis.text = element_text(size = 12),
      axis.title.x = element_text(size=12), 
    axis.title.y = element_text(size=12, vjust = 0.6 ),
    strip.text = element_text(size =12))+
facet_wrap(~ Treatment)+
labs(y = "Fold Change relative to CT of 40 \n (log 10 scale)", x = "Time (hrs)")
```

![](Plot_gallery_files/figure-markdown_github/unnamed-chunk-6-1.png)

Dots with scientific notation using scales package
--------------------------------------------------

Results of infecting TZM-bl cells with HIV (Bal) in the presence of different amounts of seminal exosomes per pfu. Plot is from the "TZM-bl\_infection\_assay\_001\_003\_004" script.

Note that you need to call `library(scales)` in order to use `scale_y_continuous(labels = scientific)`

``` r
library(scales)

ggplot(dat003Bal3xCntl,aes(x = Condition, y = RLU))+ scale_y_continuous(labels = scientific)+
geom_point(aes(), size = 3)+
ggtitle("Bal data: only reps 3x avg Cells Only control")
```

![](Plot_gallery_files/figure-markdown_github/unnamed-chunk-8-1.png)

Using `interaction()` to make `geom_line()` between points that share another aesthetic (color)
-----------------------------------------------------------------------------------------------

I wanted to plot the multiple data sets from each experiment in the same color, but also wanted geom\_line to connect unique instances of virus + experiment, not all the points from one experiment.

I used r`scale_shape_manual()` to make all the "No virus" conditions the same shape, even though they had different names.

``` r
ggplot(Bal26, aes(x = Day, y = RLU))+
  geom_point(aes(color = Experiment, shape = Virus),size = 3)+
  geom_line(aes(group = interaction(Experiment,Virus), color = Experiment))+
  scale_y_log10()+
  scale_shape_manual(values = c("Bal26 lot1" = 1, "No virus" = 4, "No virus1" = 4, "No virus2" = 4 ))+
  ggtitle("Bal26 lot 1 and No virus cntls")
```

![](Plot_gallery_files/figure-markdown_github/unnamed-chunk-10-1.png)

Heat map of MSD data
--------------------

-   Use geom\_tile to make the grid

-   Use `scale_fill_gradient()` and assign the colors I want to the hi low and midpoints

-   Set the midpoint to zero

-   Use `scale_y_discrete()` to make sure the labels have the correct greek letters instead of whatever weird symbols ggplot tries to use.

``` r
ggplot(merge24And3, aes(x = Donor, y = Assay))+
  geom_tile(aes(fill = max10fold), color = "gray")+
  scale_fill_gradient2(low = "green", mid = "white", high = "red", midpoint = 0, name = "log10 Fold Change")+
  facet_wrap(~Virus, scales = "free_x")+
  scale_y_discrete(breaks = merge24And3$Assay,labels =merge24And3$Assay)+
  ggtitle("log fold change of concentrations at 24hrs over 3 hrs")
```

![](Plot_gallery_files/figure-markdown_github/unnamed-chunk-12-1.png)

Scatter plot of MSD data for 3 cytokines
----------------------------------------

I was trying to make these plots in the same style as some others created (I think) in prism

Data is from "Projectsstudysamples016-2017\_CVL\_MSD\_analysis\_2018\_plot\_for\_FH1a\_IL1b\_IL1RA\_plot\_for\_FH.R"

-   Make a custom theme to create a plain background and put the legend in the top left corner

-   Convert RGB colors (guessed from prism) to fractional RGB then HEX

-   override the aesthetics set in geom\_point to be able to have the dots in the legend bigger than those in the plot, for better visibility.

-   Make nice looking exponents by using labels=trans\_format('log10',math\_format(10^.x)) from the scales package

``` r
#here's the colors I want to use. I have the RBG so I will convert to HEX
#I dont have the fractional RBG, so I need to change my full RBG values to fractional using maxColorValue



detectable <-rgb(0, 128, 192, maxColorValue = 255)
below <- rgb( 255, 0, 128, maxColorValue = 255)
above <- rgb( 251, 189, 47, maxColorValue = 255)



# plot theme

theme_pres <- theme (
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14, face = "bold", color = "black"), # black bold text for x axis labels
  axis.title.x = element_text(size = 14, vjust = -1),#move the title over a little
  axis.title.y = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.title = element_blank(), #no legend title
  axis.ticks = element_blank(), # no tick marks 
  legend.background = element_blank(), #no background color in  the legend box
  legend.key = element_blank(), #no background behind the symbols in the legend
  panel.border= element_rect(color = "gray", fill = NA), #add a border around the plot
  panel.grid.major = element_blank(), # remove grid lines
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),#no background color in plot
  legend.position = c(0.15,0.9) #put the legend on upper left of the the plot
)




ggplot(dat_no_cntl_no_std_in_range, aes(x = Assay, y = Dilution_corrected_conc))+
  geom_jitter(aes(color = `adj_detection_labels`))+
  scale_y_log10(limits = c(0.01, NA),breaks = c(0.1,1, 10,100, 1000,10000, 100000, 1000000), labels=trans_format('log10',math_format(10^.x)))+
  labs(y = "Quantified cytokine level pg/mL", x = "Cytokine")+
  theme_pres+
  scale_color_manual(values = c(above, below, detectable))+
  guides(color = guide_legend(override.aes = list(size = 4)))
```

![](Plot_gallery_files/figure-markdown_github/unnamed-chunk-14-1.png)
