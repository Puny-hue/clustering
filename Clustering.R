# 1.) Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.

crime_data <- read.csv("C:/EXCELR/ASSIGNMENTS/Clustering/crime_data.csv")
View(crime_data)
library(animation)
# EDA
summary(crime_data)
# Normalization
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

normalized_crime_data <- as.data.frame(lapply(crime_data[,2:5], normalize))
summary(normalized_crime_data)

# applying k-means alogorithm
fit_crime_data <- kmeans.ani(normalized_crime_data,5)
summary(fit_crime_data)
str(fit_crime_data)
fit_crime_data$cluster
fit_crime_data$centers
membership_crime_data<- data.frame(crime_data, fit_crime_data$cluster) # append cluster membership
View(membership_crime_data_f)
membership_crime_data_f <- membership_crime_data[,c(ncol(membership_crime_data),1:(ncol(membership_crime_data)-1))]
aggregate(crime_data[,2:7], by=list(fit_crime_data$cluster), FUN=mean)

#wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
twss = c()
for (i in 2:15) twss[i] = sum(kmeans(normalized_crime_data, centers=i)$withinss)
plot(1:15, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
View(twss)
km$withinss

fit_crime_data <- kmeans.ani(normalized_crime_data,6)
summary(fit_crime_data)
str(fit_crime_data)
fit_crime_data$cluster
fit_crime_data$centers
membership_crime_data<- data.frame(crime_data, fit_crime_data$cluster) # append cluster membership
View(membership_crime_data_f)
membership_crime_data_f <- membership_crime_data[,c(ncol(membership_crime_data),1:(ncol(membership_crime_data)-1))]
aggregate(crime_data[,2:5], by=list(fit_crime_data$cluster), FUN=mean)




#2.) Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. 

library(xlsx)

# Importing data
airlines_data <- read.xlsx("C:/EXCELR/ASSIGNMENTS/Clustering/EastWestAirlines.xlsx",2)
View(airlines_data)

# EDA
colnames(airlines_data)
summary(airlines_data)

# Normalization
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

normalized_airlines_data <- as.data.frame(lapply(airlines_data[,2:11], normalize))
summary(normalized_airlines_data)
# hierarchical Clustering
?dist
d <- dist(normalized_airlines_data, method = "euclidean")# distance matrix

fit <- hclust(d, method="complete")
summary(fit)
?hclust
?dist
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=30) # cut tree into 30 clusters

?cutree
rect.hclust(fit, k=30, border="red")
?rect.hclust

membership<-as.matrix(groups)

final <- data.frame(airlines_data, membership)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
aggregate(airlines_data[,2:11], by=list(final$membership), FUN=mean)
library(data.table)
attach(final)


#K-means
#Elbow chart
twss = c()
for (i in 2:30) twss[i] = sum(kmeans(normalized_airlines_data, centers=i)$withinss)
plot(1:30, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
View(twss)
km$withinss
library(animation)
fit <- kmeans.ani(normalized_airlines_data, 7) # 7 cluster solution
str(fit)
final2<- data.frame(normalized_airlines_data, fit$cluster) # append cluster membership
final2
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(airlines_data[,2:11], by=list(fit$cluster), FUN=mean)

# Deployment

library(shiny)

ui= fluidPage(style = "border: 2px solid black",
              titlePanel(title= h3(tags$b("Clustering"), align="center", style="color:Blue")),
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons(inputId = "type",label = "Select dataset",choices = c("Crime","Airlines"), selected = "Crime"),
                  numericInput(inputId = "cluster_no",label = "Select No. of clusters", value = 3, min = 1 , max = 10, step=1),
                  style = "border: 2px solid cyan"),
                mainPanel(
                  br(),
                  br(),
                  tabsetPanel(type = "tabs",
                              tabPanel("Dendogram", textOutput("dendogram")),
                              tabPanel("Scree Plot"),
                              tabPanel("Hierarchical Cluster"),
                              tabPanel("Hierarchical Cluster Data"),
                              tabPanel("KMeans Cluster"),
                              tabPanel("KMeans Cluster Data")
                              
                  ),style = "border: 2px solid cyan"
                  
                )
                
                
              )
              
)





server = function(input,output){
  
  type_data <- reactive({
    input$type
  })
  
  cluster_no <- reactive({
    input$cluster_no
    
  })
  import_data <- reactive(
    if(type_data == "Crime" ){
      airlines_data = read.csv("C:/EXCELR/ASSIGNMENTS/Clustering/crime_data.csv")
      
      data_normal = as.data.frame(lapply(airlines_data[,2:11], normalize))
    }
    else{
      data = read.xlsx("C:/EXCELR/ASSIGNMENTS/Clustering/EastWestAirlines.xlsx",2)
      data_normal = 
    }
    
  )
  output$dendogram <- renderText(
    
    cluster_no())
  
}

shinyApp(ui=ui, server =server)
