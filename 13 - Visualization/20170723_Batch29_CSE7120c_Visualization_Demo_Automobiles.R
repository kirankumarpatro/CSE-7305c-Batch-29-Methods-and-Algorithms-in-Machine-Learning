rm(list = ls())
setwd("D:/AMITS_LABS/BATCH 29/Day 00 VISUALIZATION")
#############################################################################
#By the end of this activity you will be familiar with plotting graps in R
#using packages graphics and ggplot2  

##################################
#Topics Covered:
#Visualizing numeric attributes ,using histograms, boxplots and line plots
#Visualizing categorical attributes using bar plots
#Visualizing association of numreical and categorical attributes
#Visualizations uding ggplot2

##################################
#Packages to be installed
#install.packages("ggplot2")
library(ggplot2)
##################################
###Data reading and prepping
cars_data <- read.csv("Automobiles.csv")
colnames(cars_data)
str(cars_data)
normalized_losses <- NULL
sum(is.na(cars_data))
cars_data <- na.omit(cars_data) #omitting missing values

#Do if needed:
#Find prospective numeric attributes and convert them into numeric
#Find prospective categorical attributes and convert them into categorical

##################################
###Visualizing numeric attribtues

#Histograms : To view the distribution of one-dimensional data
attach(cars_data) #attach the dataframe so that cols can be referenced directly
default_par <- par() #save the default params
par(mfrow=c(1,2)) # Customize the params; mfrow=c(1,2) specifies 2 multiple figures in 1 row;

par(bg="grey")
hist(city_mpg, col="brown", xlab="City Mileage",
     main="Frequency Histogram:City Mileage",xlim = c(5,50), ylim = c(0,70))
hist(highway_mpg, col="red", xlab="Highway Mileage",
     main="Frequency Histogram:Highway Mileage", xlim = c(5,50), ylim = c(0,70)) 
par(default_par) #set the default params back

#Boxplots : To view & compare distributions of data
#Boxplot to view the distribution of data:
par(mfrow=c(1,1))
boxplot(city_mpg,main = "Box Plot: City Mileage", ylab = "City Mileage")
boxplot(highway_mpg,main = "Box Plot: Highway Mileage",
        ylab = "Highway Mileage")

#Boxplot to compare the distribution of data:
par(mfrow=c(1,1))
boxplot(city_mpg ~ aspiration, 
        main = "Mileage Vs. Engine Aspiration Type", xlab="Aspiration", ylab="mileage")

#Using plot() : when a factor of x values and a vector of y values is given to
#plot(), it automatically prints a boxplot
#Note: qplot() sia function from ggplot2; refers to quick plot.
plot(aspiration,city_mpg,
     main = "Mileage Vs. Engine Aspiration Type")
#Using qplot()
qplot(aspiration,city_mpg, geom = "boxplot",
      main = "Mileage Vs. Engine Aspiration Type")

#Linechart to view the distribution of data:
plot(horsepower,type = "l", main = "Mileage for each Car")

#######################################
### Visualizing Categorical attributes

##Bar Charts: To display numeric values(on y-axis), for different categories(on x-axis)
plot(aspiration, type="b",main = "Distribution of aspiration types")
plot(aspiration, type="b",main = "Distribution of aspiration types", horiz = T) #for horizontal bars
barplot(table(aspiration), main = "Distribution of aspiration types")
qplot(x = aspiration,  geom = "bar",main = "Distribution of aspiration genres")

##correlations
library(corrplot)
corMat <- cor(cars_data[,sapply(cars_data, is.numeric)])
corrplot::corrplot(corMat)
corrplot::corrplot(corMat,tl.cex = 0.7)
corrplot::corrplot(corMat, tl.cex = 0.7, method = "number")

##Stacked bar plots
counts <- table(num_cyl,fuel_system)
barplot(counts, main="#Cylinders in each fuel_system type",
        col=rainbow(7),
        xlab="Fuel_system", ylab = "#cars",
        legend.text = TRUE, 
        args.legend = list(x = "topright", bty = "n",
                           cex = 0.6, ncol=1,xjust=1))


qplot(fuel_system, data=cars_data, geom="bar",
      fill = num_cyl,main="#Cylinders in each fuel_system type",)

#Grouped bar plots
counts <- table(num_cyl,fuel_system)
barplot(counts, main="#Cylinders in each fuel_system type",
        col=rainbow(7),
        xlab="Fuel_system", ylab = "#cars",
        legend.text = TRUE,
        args.legend = list(x = "topright", bty = "n", 
                           cex = 0.6, ncol=1, xjust=1),beside = TRUE)


###A note on differences between histograms and bar charts: 
#Histograms are used to show distributions of variables while
#bar charts are used to compare variables. 
#Histograms plot binned quantitative data while bar charts plot 
#categorical data. 
#Bars can be reordered in bar charts but not in histograms.

###Scatter Plots: To display the relationship between two continuous variables
plot(kerb_weight, price, ylab="Price",main="Kerb_weight Vs. Car Price", pch=19)

## Often, a scatterplot will also have a line showing the predicted values based on some statistical model.
# Add fit lines
abline(lm(price~kerb_weight), col="red") # regression line (y~x) 
#Note: The scatterplot() function in the car package offers many enhanced features,
#including fit lines, marginal box plots, conditioning on a factor, and interactive point identification. 

##################################
###VISUALIZATIONS USING ggplot2

##Bar plots
# Start with a basic graph  in ggplot
# If y is a numeric vector, use stat="identity"; the default is stat="bin" for categorical

ggplot(cars_data,aes(x=fuel_system)) + geom_bar() + 
        xlab("Fuel_system") + ylab("# Cars") 

#Add title & count to ggplot
ggplot(cars_data,aes(x=fuel_system)) + geom_bar() +
        xlab("Fuel_system") + ylab("# Cars")  +
        ggtitle("#Cars in various fuel system types")+
     geom_text(stat='count',aes(label=..count..),vjust=0)
  
# Fill color based on number of cylinders
ggplot(cars_data,aes(x=fuel_system, fill=num_cyl)) + geom_bar() +
        xlab("Fuel_system") + ylab("# Cars")  +
        ggtitle("#Cars in various fuel system types")

ggplot(cars_data,aes(x=fuel_system)) + geom_bar(aes(fill=num_cyl)) +
        xlab("Fuel_system") + ylab("# Cars")  +
        ggtitle("#Cars in various fuel system types")

# Make a grouped bar chart with similar graph attributes
ggplot(cars_data,aes(x=fuel_system, fill=num_cyl)) + 
        geom_bar(position = "dodge") +
        xlab("Fuel_system") + ylab("# Cars")  +
        ggtitle("#Cars in various fuel system types")

# To adjust the width of the bars
ggplot(cars_data,aes(x=fuel_system,fill=num_cyl)) +
        geom_bar(width = 0.5) +
        xlab("Fuel_system") + ylab("# Cars")  +
        ggtitle("#Cars in various fuel system types")

# To add a black outline to the bars
ggplot(cars_data,aes(x=fuel_system,fill=num_cyl)) + 
        geom_bar(width = 0.5, colour = "black" ) +
        xlab("Fuel_system") + ylab("# Cars")  +
        ggtitle("#Cars in various fuel system types")

# Change back ground
#To remove grey background in plot
ggplot(cars_data,aes(x=fuel_system,fill=num_cyl)) +
        geom_bar(width = 0.5) +
        xlab("Fuel_system") + ylab("# Cars")  +
        ggtitle("#Cars in various fuel system types")+
        theme_bw()

# To remove grey and gridlines in the Bg
ggplot(cars_data,aes(x=fuel_system,fill=num_cyl)) +
        geom_bar(width = 0.5) +
        xlab("Fuel_system") + ylab("# Cars")  +
        ggtitle("#Cars in various fuel system types")+
        theme_classic()

# Adjust text size and angle of labels
ggplot(cars_data,aes(x=fuel_system,fill=num_cyl)) +
        geom_bar(width = 0.5) +
        xlab("Fuel_system") + ylab("# Cars")  +
        ggtitle("#Cars in various fuel system types")+
        theme_classic()+
        theme(axis.text.x=element_text(angle=45,size=10),
              text=element_text(size=12))

# Now remove the legend position
ggplot(cars_data,aes(x=fuel_system,fill=num_cyl)) +
        geom_bar(width = 0.5) +
        xlab("Fuel_system") + ylab("# Cars")  +
        ggtitle("#Cars in various fuel system types")+
        theme_classic()+
        theme(axis.text.x=element_text(angle=45,size=10),
              text=element_text(size=11), legend.position = "none")

#check the number of 2-cylindered cars
sum((num_cyl)=="two")

##Scatterplot
## Basic scatterplot with 2 dimensions, x and y
ggplot(data=cars_data, aes(x=city_mpg, y=price))+ geom_point() + 
        xlab("City_mpg")+ylab("Price") + 
        ggtitle("City_mpg Vs. Car Price")

# Scatterplot with 3 dimensions (by adding color to the basic plot)
ggplot(data=cars_data, aes(x=city_mpg, y=price, colour = aspiration)) + 
        geom_point() + 
        xlab("City_mpg")+ylab("Price") + 
        ggtitle("City_mpg Vs. Car Price")

# Scatterplot with 5 dimensions (by adding color, shape, & size to the basic plot)
ggplot(data=cars_data, aes(x=city_mpg, y=price, colour = aspiration, size = hoesepower, shape = drive_wheel)) + 
        geom_point() + 
        xlab("City_mpg")+ylab("Price") + 
        ggtitle("City_mpg Vs. Car Price")
#We see that rear wheel drives give less mileage and are very costly
#Also, higher the horsepower, higher is the cost and lesser is the mileage

#Faceting: To create a subplot for each level of a factor variable
ggplot(data=cars_data, aes(x=city_mpg, y=price, colour = aspiration, size = horesepower, shape = drive_wheel)) + 
        geom_point() + 
        xlab("City_mpg")+ylab("Price") + 
        ggtitle("City_mpg Vs. Car Price") + 
        facet_wrap(~fuel_system)
#We see that majority of idi and mfi systems are turbo aspirated
#Majority of idi types have rear-wheel drive systems
#Bluebarrel types are not turo aspirated
#mpfi covers good range of price, in combination with hp, aspiration and drive_wheels

##Add jitter 
#without jitter, points might be overlapped/crowded at a level
ggplot(data=cars_data, aes(x=drive_wheel, y=price, colour = aspiration, size = horesepower)) + 
        geom_point() + 
        xlab("City_mpg")+ylab("Price") + 
        ggtitle("drive_wheel Vs. Car Price") 

#Jitter widens the point space for each level
ggplot(data=cars_data, aes(x=drive_wheel, y=price, colour = aspiration, size = horesepower)) + 
        geom_jitter() + 
        xlab("City_mpg")+ylab("Price") + 
        ggtitle("drive_wheel Vs. Car Price") 

## Managing and grouping graphical gg objects to plot graphs
p1 <- ggplot(data = cars_data)
p2 <- aes(x = num_cyl, y = price)

p1 + p2 + geom_point() #scatterplot
p1 + p2 + geom_point(aes(color = factor(aspiration))) #scatterplot with 3 dimensions
p1 + p2 + geom_boxplot() #boxplot
p1 + p2 +geom_bar(stat="identity") #barplot

detach(cars_data) #detach the dataframe

###############################################################
#References:
#https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
#https://www.forbes.com/sites/naomirobbins/2012/01/04/a-histogram-is-not-a-bar-chart/#53a11ed36d77
#http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
#https://www.rdocumentation.org/packages/graphics/versions/3.4.0/topics/par
#http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
###############################################################