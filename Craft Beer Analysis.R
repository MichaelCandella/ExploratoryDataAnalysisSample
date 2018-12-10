library(ggplot2)

##Distribution of Average Beer Advocate Score
ggplot(data=BeerNJ,aes(x=BeerNJ$Avg_Rate))+
  geom_histogram(fill="#F8CB1B",color="black",binwidth =.05)+
  ggtitle("Average Rating on Beer Advocate for 85 NJ Craft Breweries")+
  xlab("Beer Advocate Score 1-5 ")+
  ylab("Count")+
  theme(plot.title =element_text(face="bold",hjust=.5),axis.title = element_text(face="bold"))

#Summary and Variance of Beer Advocate Score
sqrt(var(BeerNJ$Avg_Rate))
summary(BeerNJ$Avg_Rate)

#Distribution of Pale Percentage
ggplot(data=BeerNJ,aes(x=BeerNJ$Pale_Percent))+
  geom_histogram(fill="#F8CB1B",color="black",binwidth = .1)+
  ggtitle("Distribution of IPA Percentages at 85 NJ Craft Breweries")+
  xlab("Percent")+
  ylab("Count")+
  theme(plot.title =element_text(face="bold",hjust=.5),axis.title = element_text(face="bold"))

#Pale Percent Summary statistics
summary(BeerNJ$Pale_Percent)
sqrt(var(BeerNJ$Pale_Percent))

#Freq Ratio Distribution
ggplot(data=BeerNJ,aes(x=BeerNJ$Freq_Ratio),stat="count")+
  geom_histogram(fill="#F8CB1B",color="black",binwidth = .25)+
  ggtitle("Distribution of Frequency Ratio of 85 Craft Breweries in NJ")+
  xlab("Frequency Ratio (Craft Beers Sold/Craft Beer Styles Sold)")+
  ylab("Count")+
  theme(plot.title =element_text(face="bold",hjust=.5),axis.title = element_text(face="bold"))
#Frequency Ratio Summary Statistics
summary(BeerNJ$Freq_Ratio)
sqrt(var(BeerNJ$Freq_Ratio))

#Scatterplot of Freq Ratio and Average Rating
ggplot(data=BeerNJ,aes(x=BeerNJ$Freq_Ratio,y=BeerNJ$Avg_Rate))+
  geom_point(size=3,color="#C6A215")+
  geom_smooth(method = 'lm')+
  ggtitle("Beer Advocate Rating vs Beer Style Frequency Ratio")+
  xlab("Frequency Ratio (Craft Beers Available/Craft Beer Styles Available)")+
  ylab("Brewery Rating on Beer Advocate")+
  theme(plot.title =element_text(face="bold",hjust=.5),axis.title = element_text(face="bold"))

#Linear Model of Average Rate on Freq Ratio
Model<-lm(BeerNJ$Avg_Rate~BeerNJ$Freq_Ratio,data=BeerNJ)
summary(Model)
Elasticity1<-((mean(BeerNJ$Avg_Rate)/mean(BeerNJ$Freq_Ratio))*.0955)

#Scatterplot of IPA Percentage on Average Rating
ggplot(data=BeerNJ,aes(x=BeerNJ$Pale_Percent,y=BeerNJ$Avg_Rate))+
  geom_point(size=3,color="#C6A215")+
  geom_smooth(method = 'lm')+
  ggtitle("Beer Advocate Rating vs Percentage of Pale Ales available at 85 NJ Craft Breweries")+
  xlab("Percent of Beers at NJ Craft Breweries that are Pale Ales")+
  ylab("Brewery Rating on Beer Advocate")+
  theme(plot.title =element_text(face="bold",hjust=.5),axis.title = element_text(face="bold"))

#Linear Model of Avaerage Rate on IPA Percentage
Model2<-lm(BeerNJ$Avg_Rate~BeerNJ$Pale_Percent,data=BeerNJ)
summary(Model2)
Elasticity2<-((mean(BeerNJ$Avg_Rate)/mean(BeerNJ$Pale_Percent))*.25981)
