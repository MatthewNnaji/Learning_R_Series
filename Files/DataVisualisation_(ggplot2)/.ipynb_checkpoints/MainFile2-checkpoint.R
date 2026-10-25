# ggplot2｜Data Vis. with R ################################################################
"STRONGLY Recommend opening this in Rstudio! There is an alternate jupyter notebook version, 'VisNotebook' available."

## Libraries and Init =================================================================
library(tidyverse)
library(dplyr)
library(ggplot2)

library(ggtext)
library(ggthemes)
library(RColorBrewer)
library(viridis)

library(datasets)
library(help = "datasets")

library(readxl)

"--"
#install.packages('ggtext') <- BAD MANNERS, install in console rather than script

## Tasks ===========================================================================
#~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・
# [1] first visualisation, point geometry, lines and smoothers,  geom_bar and geom_column
# [5] histograms, box-and-whisker diagrams, backgrounds, axes, using scales (e.g. scale_x_continuous)
# [10] Key/legend, Annotations, Titles, Themes (brewer & viridis), Faceting, secondary axis
#~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・~・—・


### [1] First Visualisation ------------------------------------------------------------------------------

library(tidyverse)
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college |>
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))
summary(college)

# ggplot() called alone makes a blank visualisation => using the 'grammar of graphics', we build upon this
ggplot()
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg))
#ggplot [data, type, axis, aesthetics]


### [2] Point Geometry  ------------------------------------------------------------------------------
#basics of any graph - each point has a coordinate for x and y

scat2 <- ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg,shape=control),colour='purple')+#shape can be used if you have factor variables
   labs(title = "Tuition has still not gone out of style",
         subtitle = "Fairly strong positive correlation remains between tuition hours received and SAT Scores",
         caption = "Standardised, collected by the board for economic opportunity",
         tag = "Fig. 1")+
  theme(plot.caption = element_text(hjust=0.5,color='purple4'),
        plot.title = element_text(colour='purple4',size=20,face='bold'),
        plot.subtitle= element_text(colour='purple3',size=13,face='bold'))+
  theme(panel.background=element_rect(fill='#fff9fc',linetype=4,linewidth=1,colour='purple4'),
        plot.background=element_rect(fill = '#f1e7ff',linewidth=1,colour='purple4'))

  
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg,colour=control)) #colour can also be used if you have factor variables

scat3 <- ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg,colour=control, size=undergrads), alpha=50/100) #size self explain; alpha ranges 0 to 1
print(scat3)

### [3] Lines and Smoothers  ------------------------------------------------------------------------------
#lines is effectively the same; replace point to line

ggplot(data=college) +
  geom_line(mapping=aes(x=tuition, y=sat_avg,colour=control, size=undergrads), alpha=50/100)+
  geom_point(mapping=aes(x=tuition, y=sat_avg,colour=control, size=undergrads), alpha=50/100) 
#you can run the mapping/grammar on the visualisation level; or the plot level
ggplot(data=college,mapping=aes(x=tuition, y=sat_avg,colour=control, size=undergrads),alpha=0.5) +
  geom_point() 


ggplot(data=college,mapping=aes(x=tuition, y=sat_avg,colour=control)) +
  geom_point(aes(size=undergrads),alpha=0.2) + geom_smooth(se=FALSE) #smooth fits line to dataset; SE=standard error of this line in fitting to data



### [4] geom_bar and column  ------------------------------------------------------------------------------

#geom_bar maps a variable; to the count/number of times it occurs (frequency)

ggplot(data=college) +
  geom_bar(mapping=aes(x=region,colour=control)) #for bar aesthetic, colour=outline

ggplot(data=college) +
  geom_bar(mapping=aes(x=region,fill=control)) #for bar aesthetic, fill=bar colouring in

#geom_col() is literally the same; except you manually set y
#we will quickly use dplyr to extract a statistic (average tuition); then map that

college %>%
  group_by(region) %>%
  summarize(average_tuition=mean(tuition)) %>%
  ggplot() + #don't put in college we use piping and save code just take the new college and pipe it as the input
  geom_col(mapping=aes(x=region, y=average_tuition, fill=region))

#done as asked. For explanation on group; its effectively pivot_tables* (excel mildly more efficient for small data but for 1m+ rows R for sure)

?group_by() #this is effectively saying =SUMIF() and forming a new table

# instead of 
# Name    | Region | Sales 
# Mary    | North  | 30
# Phil    | North  | 30
# Cara    | South  | 70
# Elliot  | North  | 15
# William | South  | 12
# Emma    | South  | 50
# 
# test <- dataframe %>%
#   group_by(Region) %>%
#   summarise(Total=sum(Sales))
# 
# we get
# 
# Region | TotalSales 
# North  | 30+30+15 = 75
# South  | 70+12+50 = 132
# 
# (be warned its just grouping, e.g. for max/min searches through all the north/south values)
# so in effect; group_by() groups the table so R treats them the same (adding to the "column" field; then the summarise() makes the table completely)
# 
# adding another column to the group(by) simply does that; add another column
# 
# 
# test <- dataframe %>%
#   group_by(Region,Season) %>%
#   summarise(Total=sum(Sales),.groups="drop") -> R; its ok to stop grouping now
#   
# 
# If u wanted to combine and have one of the variables be wide; just separately do a pivot_wider() operation
# 
# pivot_wider(dataframe,names_from=Season, values_from=Total)
# 
# Doing this means the original total column has been spread effectively into the four seasons
# 
# Hence you have to make a new column for total just do mutate(dataframe,Total=spring+summer+autumn+winter)


### [5] Histograms  ------------------------------------------------------------------------------
'Say you have a data value like (12,1) and (100000,1); the chart can be very wide and spaced out.'
'Histograms helps with this; by grouping data into "bins"'

college %>%
  ggplot() + geom_histogram(mapping=aes(x=undergrads),origin=0,bins=10)


ggplot(data=college) + geom_histogram(mapping=aes(x=undergrads,fill=region),boundary=0,binwidth=2000)

?geom_histogram

### [6] Box-and-Whisker Dia.  ------------------------------------------------------------------------------
college %>%
  ggplot() + geom_point(mapping=aes(x=control, y=tuition)) #not very useful...

college %>%
  ggplot() + geom_jitter(mapping=aes(x=control, y=tuition)) 
#similar; slightly averts the problem

college %>%
  ggplot() + geom_boxplot(mapping=aes(x=control, y=tuition))


### [7] Backgrounds  ------------------------------------------------------------------------------
'You can use HEX codes!! (thats all you need'
Plot1 <- college %>%
  group_by(region) %>%
  summarize(average_tuition=mean(tuition)) %>%
  ggplot() +
  geom_col(mapping=aes(x=region, y=average_tuition, fill=region)) +
  theme(plot.background=element_rect(fill='grey99')) +
  theme(panel.background=element_rect(fill='palegoldenrod'))

Plot2 <- college %>%
  group_by(region) %>%
  summarize(average_tuition=mean(tuition)) %>%
  ggplot() +
  geom_col(mapping=aes(x=region, y=average_tuition,fill=region),colour='grey15') +
  theme(plot.background=element_rect(fill='grey98')) +
  theme(panel.background=element_rect(fill='grey98')) + 
  theme(panel.grid.major=element_line(colour='grey15')) #changes grid line colours

colours()

Plot3 <- college %>%
  group_by(region) %>%
  summarize(average_tuition=mean(tuition)) %>%
  ggplot() +
  
  geom_col(mapping=aes(x=region, y=average_tuition, fill=region),colour='grey15')

Plot3 <- Plot3+
  theme(plot.background=element_rect(fill='snow')) +
  theme(panel.background=element_rect(fill='snow')) + 
  theme(panel.grid.major.y=element_line(colour='grey15')) + #major grid lines only for y
  theme(panel.grid.minor.y=element_line(colour='grey50')) +
  theme(panel.grid.major.x=element_blank())+
  theme(legend.background = element_rect(colour='grey15',fill = "snow"),legend.key = element_rect(fill = "snow"))

view(Plot1)

colours()
?theme()
### [8] Axes ------------------------------------------------------------------------------
#Setting up the chart
Plot4 <- ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control),colour='grey30') +
  theme(panel.background=element_rect(fill='snow'),plot.background=element_rect(fill='snow')) +
  theme(panel.grid.major.y=element_line(colour='grey15'),panel.grid.major.x=element_blank()) #major grid lines only for y


Plot4 + theme(panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank())
Plot4 + theme(legend.background = element_rect(colour='grey15',fill = "snow"),legend.key = element_rect(fill = "#fffcfc"))

#Adding labels
Plot4 + ylab('Number of schools (thousands)') + xlab('Region')

print(Plot4)
#Changing scale
Plot4 + ylim(0,500)

colnames(college)
### [9] Using scale_ ------------------------------------------------------------------------------
'Scales describe how data is mapped from the dataset to the visualisation.'
' Both discreet and continous scales exist; for discreet and continous variables respectively'
#To map a scale do scale_name_datatype
#scale_y_discrete | discrete variable on the y axis
#scale_x_continous | continous variable on the x axis
#scale_fill_discrete | discrete variable on the 'fill' axis/scale 
'Using Plot4 again; we can change label names using scale'
m
Plot5 <- Plot4 +
  scale_x_discrete(name='Region')+
  scale_y_continuous(name='Number of schools',limits=c(0,500))+
  scale_fill_manual(values=c('red3','steelblue'))
print(Plot5)

?scale_x_continuous

### [10] Key / Legend ------------------------------------------------------------------------------

Plot4 +
  scale_fill_manual(values=c('red3','steelblue'),guide=guide_legend(title='School Funding',nrow=1,
    label.position='left', keywidth=2.5 )) #changing the legend is to do with colour....
#.... and hence we edit it on the 'fill'(colour) scale
#this is changing the order of Schoolwidth > Private/Public > The colours(we changed width with keywidth)

Plot4 + theme(legend.position = 'bottom')
print(Plot4)

print(Plot3 + theme(legend.position = 'bottom')+
        labs(title='Inequality persists in America with contrasting regional tuition levels')+
        scale_fill_manual(values=c('red','orange','yellowgreen','azure'),guide=guide_legend(label.position='top',title='Cardinal',nrow=4,keywidth=40)))

### [11] Annotations ------------------------------------------------------------------------------
scat3 <- scat3 + theme(legend.background=element_rect(colour='grey15'))

#annotating with pure text, with labels; false borders with rectangles; real borders with rectangles
#full lines using geom_hline and vline; partial lines with annotate_segment()
scatter <- scat3 + 
        annotate('rect',xmin=40750,xmax=46700,ymin=1365,ymax=1390,colour='grey15',fill='white',alpha=0.6)+
        annotate("text",label='Elite Privates',x=43800,y=1380)+
        
        geom_hline(alpha=1,size=0.5,linetype=5,yintercept = mean(college$sat_avg))+
        annotate('rect',xmin=36000,xmax=43800,ymin=mean(college$sat_avg)-35,ymax=mean(college$sat_avg)-10,colour='grey15',fill='white',alpha=0.6)+
        annotate('text',label='Mean SAT Score',x=40000,y=mean(college$sat_avg)-20)+ 
        
        geom_vline(alpha=1,linetype=4,xintercept = mean(college$tuition))+
        annotate('rect',xmin=mean(college$tuition)+250,xmax=mean(college$tuition)+6300,ymin =835 ,ymax =860,colour='grey15',fill='white',alpha=0.6 )+
        annotate('text',label='Mean Tuition',x=mean(college$tuition)+3300,y=850)+
        annotate('label',label='Weak Positive Correlation',x=25000,y=1500,fill='white',colour='grey15',alpha=0.8,size=5,fontface='bold')+
  
        scale_x_continuous(name='Tuition Hours')+
        scale_y_continuous(name='Average SAT Scores')+
        scale_colour_discrete(name='Institution')+
        scale_size_continuous(name='Undergrads')+
  scat3
print(scatter) 
  
?geom_hline()

?annotate()

               
### [12] Titles ------------------------------------------------------------------------------

#use lab(title='') in base R or ggtitle()
scatter + ggtitle(label='More work still needs to be done to increase access to opportunities for disadvantaged communities',
                        subtitle = 'Tuition and SAT data across the United States, 2021. Collected by the Department of Education.')

windowsFonts()

#customising titles
print(scatter + 
        ggtitle(label='More work still needs to be done to increase access to opportunities for disadvantaged communities',
                  subtitle = 'Tuition and SAT data across the United States, 2021. Collected by the Department of Education.') +
        theme (plot.title = element_text(hjust=0.5,size=12,face='bold',family='serif'))+
        theme(plot.subtitle = element_text(hjust=0.5,size=8,family='mono')))
### [13]ThemesBrewerViridis------------------------------------------------------------------------------

display.brewer.all()
#《Viridis family》
# A = magma
# B = inferno
# C = plasma
# D = viridis (default)
# E = cividis (for publications)
# F = rocket
# G = mako
# H = turbo


#learning set colour sets in R

#download + install ggthemes(); and stick on the end
Plot1 + theme_bw()
#add colour_brewer and fill_brewer() instead of manual to use themes
scatter + theme_bw() + scale_colour_brewer(palette='Dark')
scatter + theme_solarized() +scale_colour_brewer(palette='Reds')

Plot2 + theme_excel() + scale_fill_brewer(palette='Blues') 
Plot2 + theme_excel_new() + scale_fill_brewer(palette='Set3') 


Plot2 + theme_fivethirtyeight() + scale_fill_brewer(palette='Blues') #'Spectrals' also cool
Plot3 + theme_wsj() + scale_fill_viridis(option = 'A',discrete = TRUE) #all A-H are nice but C
Plot4 + theme_economist() + scale_fill_viridis(option = 'A',discrete = TRUE) #not so good for stacked...


scatter +theme_fivethirtyeight()
scatter + theme_excel_new()
scatter + theme_few()

combineharvester <- scat2 / (scatter + 
           theme_solarized() +
           scale_colour_brewer(palette='Reds')+
           labs(title='The Elite 1% still dominate',
                subtitle='over 90% of the top SAT scores are by the extreme upper class',
                tag='Fig. 2')+
           theme(plot.background=element_rect(linetype=1,linewidth=1,colour='orangered3'),
                 plot.title=element_text(colour='red2',family='mono',face='bold',size=20),
                 plot.subtitle=element_text(colour='red3')))


### [14] Faceting ------------------------------------------------------------------------------
scat3 +facet_wrap(~region) #splits a graph up into all the diffrent classes
scat2 +facet_wrap(~region)
#facet_grid

scat3 + facet_grid(   rows = vars(gender),
                      cols = vars(region))
#as you can see here; the vast majority of schools are unisex (co-education)...

### [15] Secondary Axis ------------------------------------------------------------------------------
p <- ggplot(mtcars, aes(cyl, mpg)) +
  geom_point()

# Create a simple secondary axis, shifted by 10
p + scale_y_continuous(sec.axis = sec_axis(~ . + 10))

# Inherit the name from the primary axis
p + scale_y_continuous("Miles/gallon", sec.axis = sec_axis(~ . + 10, name = derive()))

#sadly... no clean way to scale axes for double graphs like in Excel.. have to manually code the data
#e.g. if right scale is 0 to 40,000 and left is -20 to 20 scale factor is 1000X
#temperature*1000
### [10] Key / Legend ------------------------------------------------------------------------------http://127.0.0.1:27214/graphics/79aead7b-ec6e-4805-8efd-9b00b55219d8.png
### [10] Key / Legend ------------------------------------------------------------------------------
### [10] Key / Legend ------------------------------------------------------------------------------
### [10] Key / Legend ------------------------------------------------------------------------------
### [10] Key / Legend ------------------------------------------------------------------------------
### [10] Key / Legend ------------------------------------------------------------------------------
### [10] Key / Legend ------------------------------------------------------------------------------
               
