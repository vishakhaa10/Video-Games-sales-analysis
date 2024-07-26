library(ggplot2)
library(dplyr)
install.packages("DT")
library(DT)
library(tidyr)
install.packages('wesanderson')
library(wesanderson)
install.packages("ggthemes")
library(ggthemes)

videogamesales <- read.csv("C:\Users\SRIDHAR\Downloads\Videogames.csv")
videogamesales <- videogamesales[!(videogamesales$Year %in% c("N/A", "2017", "2020")),]
videogamesales <- videogamesales %>% gather(Region, Revenue, 7:10) 
videogamesales$Region <- factor(videogamesales$Region)

mycolors <- c("#771C19", "#AA3929", "#8E9CA3", "#556670", "#000000", "#E25033", "#F27314", "#F8A31B", "#E2C59F", "#B6C5CC")

mytheme_1 <- function() {
  
  return(theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.4), plot.title = element_text(size = 15, vjust = 2),axis.title.x = element_text(size = 12, vjust = -0.35)))
  
}

mytheme_2 <- function() {
  
  return(theme(axis.text.x = element_text(size = 10, vjust = 0.4), plot.title = element_text(size = 15, vjust = 2),axis.title.x = element_text(size = 12, vjust = -0.35)))
  
}

ggplot(videogamesales, aes(Year)) + 
  geom_bar(fill = "Red")+
  mytheme_1() +
  ggtitle("Video Game Releases by Year")

revenue_by_year <- videogamesales %>% 
  group_by(Year) %>%
  summarize(Revenue = sum(Global_Sales))

ggplot(revenue_by_year, aes(Year, Revenue)) + 
  geom_bar(fill = "Green",stat = "identity") +
  mytheme_1() +
  ggtitle("Video Game Revenue by Year")

#Top Publisher

top_publisher_year <- videogamesales %>% 
  group_by(Year, Publisher) %>% 
  summarize(Revenue = sum(Global_Sales)) %>%
  top_n(1)

datatable(top_publisher_year)

ggplot(top_publisher_year, aes(Year, Revenue, fill = Publisher)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top Publisher by Revenue each Year") +
  mytheme_1() +
  theme(legend.position = "top")

### Top Genre by Revenue each Year



top_1 <- videogamesales %>% 
  group_by(Year, Genre) %>% 
  summarize(Revenue = sum(Global_Sales)) %>%
  top_n(1)

datatable(top_1)

ggplot(top_1, aes(Year, Revenue, fill = Genre)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top Genre by Revenue each Year") +
  mytheme_1() +
  theme(legend.position = "top") +
  scale_fill_manual(values = mycolors)

### Top Games by Revenue each year

top_games <- videogamesales %>%
  group_by(Year, Name) %>%
  summarize(Revenue = sum(Global_Sales)) %>%
  arrange(desc(Revenue)) %>%
  top_n(1)

datatable(top_games)


ggplot(top_games, aes(Year, Revenue, fill = Name)) + 
  geom_bar(stat = "identity") +
  mytheme_1() +
  ggtitle("Total Games by Revenue each year") +
  theme(legend.position = "top")

### Top Platform by Revenue each year


top_platforms <- videogamesales %>%
  group_by(Year, Platform) %>%
  summarize(Revenue = sum(Global_Sales)) %>%
  arrange(desc(Revenue)) %>%
  top_n(1)

datatable(top_platforms)


ggplot(top_platforms, aes(Year, Revenue, fill = Platform)) + 
  geom_bar(stat = "identity") +
  mytheme_1() +
  ggtitle("Top Platform by Revenue each year") +
  theme(legend.position = "top") + 
  scale_fill_manual(values = mycolors)

### How many publishers are in the market and who are the top publishers by number of releases? 


length(unique(videogamesales$Publisher))

by_publishers <- videogamesales %>% group_by(Publisher) %>% summarize(Total = n()) %>% arrange(desc(Total)) %>% head(10)
by_publishers$Percentage <- by_publishers$Total/dim(videogamesales)[1] * 100
by_publishers$Publisher <- factor(by_publishers$Publisher)

datatable(by_publishers, filter = "none")

ggplot(by_publishers, aes(reorder(Publisher, Total), Total, fill = Publisher)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Total), vjust = 1.5, colour = "Black")+
  ggtitle("Top 10 Publishers by Number of Releases") + 
  theme(legend.position = "none") + 
  xlab("Publisher") +
  mytheme_2() +
  coord_flip()


### Who are the top publishers by Total Revenue? 


top_publishers <- videogamesales %>% group_by(Publisher) %>% summarize(Revenue = sum(Global_Sales), Percentage = Revenue/sum(videogamesales$Global_Sales) * 100) %>% arrange(desc(Revenue)) %>% head(10)

top_publishers$Publisher <- factor(top_publishers$Publisher)

datatable(top_publishers)

ggplot(top_publishers, aes(reorder(Publisher, Revenue), Revenue, fill = Publisher)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Revenue), vjust = 1.5, colour = "Black")+
  ggtitle("Top 10 Publishers by Revenue") + 
  theme(legend.position = "none") + 
  xlab("Publisher") +
  ylab("Revenue in millions") +
  mytheme_2() +
  coord_flip()

### Lets check how top 10 publishers have grown over the years by number of releases


top_publishers <- videogamesales[videogamesales$Publisher %in% by_publishers$Publisher,] %>% group_by(Publisher, Year) %>% summarize(Total= n())

top_publishers$Publisher <- factor(top_publishers$Publisher)

ggplot(top_publishers, aes(Year, Publisher, fill = Total)) + 
  geom_tile(color = "white") + 
  mytheme_1() +
  ggtitle("Top 10 Publishers Releases by Year") +
  xlab("Year") +
  theme(legend.position = "top")

### Lets check how top 10 publishers have grown over the years in terms of revenue


top_publishers <- videogamesales[videogamesales$Publisher %in% by_publishers$Publisher,] %>% 
  group_by(Publisher, Year) %>% 
  summarize(Revenue = sum(Global_Sales))

top_publishers$Publisher <- factor(top_publishers$Publisher)

ggplot(top_publishers, aes(Year, Publisher, fill = Revenue)) + 
  geom_tile(color = "white") + 
  mytheme_1() +
  ggtitle("Top 10 Publishers by Revenue") +
  xlab("Year") +
  theme(legend.position = "top")

### Top 10 Games by Revenue


top_games <- videogamesales %>%
  group_by(Name) %>%
  summarize(Revenue = sum(Global_Sales), Percentage = Revenue/sum(videogamesales$Global_Sales) * 100) %>%
  arrange(desc(Revenue)) %>%
  head(10)

datatable(top_games)

ggplot(top_games, aes(reorder(Name, Revenue), Revenue, fill = Name)) + 
  geom_bar(stat = "identity") +

  ggtitle("Top 10 Games by Revenue") +
  ylab("Revenue in Millions") +
  xlab("Games") +
  mytheme_2() +
  theme(legend.position = "none") +
  coord_flip()

### Sales Revenue by regions


by_regions <- videogamesales %>% 
  group_by(Region) %>%
  summarize(TotalRevenue = sum(Revenue), Percentage = TotalRevenue/sum(videogamesales$Revenue) * 100) %>%
  arrange(desc(TotalRevenue))

datatable(by_regions)

ggplot(by_regions, aes(Region, TotalRevenue, fill = Region)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = TotalRevenue), vjust = 1.5, colour = "Black")+
  mytheme_2() +
  ggtitle("Total Revenue by Region") +
  theme(legend.position = "top")

### Distribution of Sales Revenue by regions

ggplot(videogamesales, aes(Region, Revenue, fill = Region)) + 
  geom_boxplot() +
  scale_y_log10() +
  mytheme_2() +
  ggtitle("Distribution of Sales Revenue") +
  theme(legend.position = "top") +
  coord_flip()

### Sales Revenue by regions and year


by_region_year <- videogamesales %>% 
  group_by(Year, Region) %>%
  summarize(TotalRevenue = sum(Revenue)) %>%
  arrange(desc(TotalRevenue))
by_region_year$Region <- factor(by_region_year$Region)
by_region_year$Year <- factor(by_region_year$Year)

datatable(by_region_year)

ggplot(by_region_year, aes(Year, TotalRevenue, color = Region)) +
  geom_point(size = 3) +
  mytheme_1() +
  ggtitle("Total Revenue by Region and Year") +
  theme(legend.position = "top")
##
genre_sales <-videogamesales %>%
  group_by(Genre) %>%
  summarise(sum_global_sales = sum(Global_Sales),.groups = 'drop') %>%
  arrange(desc(sum_global_sales))%>%
  mutate(percent = sum_global_sales/sum(sum_global_sales)*100)

options(repr.plot.width = 18, repr.plot.height = 10)
ggplot(data= genre_sales, aes(x= "",y=percent,fill = Genre))+
  geom_bar(stat="identity", width=1, color="white")+
  coord_polar("y", start=0)+
  ggtitle("Genre by % Global Sales") +
  xlab("") +
  ylab("") +
  theme_stata()+
  theme(legend.position="right")+
  geom_text(aes(label = paste0(round(percent),"%")), position = position_stack(vjust = 0.5),color = "black",size=5)

##
sumofsales <- videogamesales %>%
  group_by(Year) %>%
  summarise(sum_global_sales = sum(Global_Sales),sum_others_sales = sum(Other_Sales),
            sum_jb_sales = sum(JP_Sales),sum_eu_sales = sum(EU_Sales),
            sum_na_sales = sum(NA_Sales),.groups = 'drop')

colors <- c("Global Sales"="red", "North America Sales"="blue", "Europe Sales"="green", "Japan Sales"="orange",
            "The Rest of the World"="yellow")
options(repr.plot.width = 16, repr.plot.height = 8)
ggplot(data=sumofsales, aes(x= Year)) +
  geom_line(aes(y= sum_global_sales,group=1,color="Global Sales"),linetype = "dashed")+
  geom_line(aes(y= sum_na_sales,group=1,color="North America Sales"),linetype = "dashed")+
  geom_line(aes(y= sum_eu_sales,group=1,color="Europe Sales"),linetype = "dashed")+
  geom_line(aes(y= sum_jb_sales,group=1,color="Japan Sales"),linetype = "dashed")+
  geom_line(aes(y= sum_others_sales,group=1,color="The Rest of the World"),linetype = "dashed")+
  geom_point(aes(y= sum_global_sales)) +
  geom_point(aes(y= sum_na_sales)) +
  geom_point(aes(y= sum_eu_sales)) +
  geom_point(aes(y= sum_jb_sales)) +
  geom_point(aes(y= sum_others_sales)) +
  scale_color_manual(name="Sales",values = colors)+
  ggtitle("Sum of Global Sales by Year") +
  xlab("Years") +
  ylab("in millions") +
  theme_stata()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position="top")

### Top 3 Genres by Revenue in each Region

```{r}

top_genres_region <- videogamesales %>%
  group_by(Region, Genre) %>%
  summarize(Revenue = sum(Revenue)) %>%
  arrange(desc(Revenue)) %>%
  top_n(3)

datatable(top_genres_region)

ggplot(top_genres_region, aes(Region, Revenue, fill = Genre)) + 
  geom_bar(position = "dodge", stat = "identity")  +
  ggtitle("Top 3 Genres by Revenue in each Region") +
  ylab("Revenue in Millions") +
  xlab("Region") +
  mytheme_2() +
  theme(legend.position = "top")


### Top 3 Games by Revenue in each Region

```{r}

top_games_region <- videogamesales %>%
  group_by(Region, Name) %>%
  summarize(Revenue = sum(Revenue)) %>%
  arrange(desc(Revenue)) %>%
  top_n(3)

datatable(top_games_region)

ggplot(top_games_region, aes(Region, Revenue, fill = Name)) + 
  geom_bar(position = "dodge", stat = "identity")  +
  ggtitle("Top 3 Games by Revenue in each Region") +
  ylab("Revenue in Millions") +
  xlab("Region") +
  mytheme_2() +
  theme(legend.position = "top")

videogamesales%>% group_by(Platform) %>% 
  summarise(sales = sum(Global_Sales)) %>% ggplot() + 
  geom_bar(aes(reorder(Platform, sales), sales), stat = "identity", 
           fill = "blue") + 
  xlab("Platform") + ylab("Global sales") + 
  coord_flip()