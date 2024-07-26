data <- read.csv("C:\Users\SRIDHAR\Downloads\Videogames.csv", header = TRUE, sep = ",")
data

head(data)

summary(data)

na.omit(data)

summary(data)

names(data)

#Regression model
model <- lm(NA_Sales~Global_Sales, data=data)
summary(model)

library(ggplot2)

#create regression plot with ggplot NA-Glo
ggplot(data,aes(NA_Sales,Global_Sales)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='NA_Sales', y='Global_Sales', title='Linear Regression Plot NA-Glo') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

#create regression plot with ggplot NA-EU
ggplot(data,aes(NA_Sales,EU_Sales)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='NA_Sales', y='EU_Sales', title='Linear Regression Plot NA-EU') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

#create regression plot with ggplot NA-JP
ggplot(data,aes(NA_Sales,JP_Sales)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='NA_Sales', y='JP_Sales', title='Linear Regression Plot NA-JP') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

#create regression plot with ggplot User count - user score
ggplot(data,aes(User_Count,User_Score)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='User_Count', y='User_Score', title='Linear Regression Plot NA-Glo') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
