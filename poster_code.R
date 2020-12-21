install.packages("ggplot2")
install.packages("dplyr")
library("ggplot2")
library('dplyr')

fname_art <- file.choose()
cereal <- read.csv(fname_art, header = TRUE, stringsAsFactors = FALSE)

str(cereal)
View(cereal)
colnames(cereal)





#################################################################################################################
#Cleaning

colnames(cereal) <- c("Name", "Manufacturer", "Type", "Calories", "Protein", "Fat", "Sodium", "Fibre", "Carbohydrates", "Sugar", "Potassium", "Vitamins", "Shelf", "Weight", "Cups", "Rating")

# Create feature with full manufacturer name
cereal$Manufacturer_Name <- cereal$Manufacturer

cereal$Manufacturer_Name <- gsub(pattern = "P", replacement = "Post", x = cereal$Manufacturer_Name)
cereal$Manufacturer_Name <- gsub(pattern = "A", replacement = "American Home Food Products", x = cereal$Manufacturer_Name)
cereal$Manufacturer_Name <- gsub(pattern = "G", replacement = "General Mills", x = cereal$Manufacturer_Name)
cereal$Manufacturer_Name <- gsub(pattern = "K", replacement = "Kellogs", x = cereal$Manufacturer_Name)
cereal$Manufacturer_Name <- gsub(pattern = "N", replacement = "Nabisco", x = cereal$Manufacturer_Name)
cereal$Manufacturer_Name <- gsub(pattern = "Q", replacement = "Quaker Oats", x = cereal$Manufacturer_Name)
cereal$Manufacturer_Name <- gsub(pattern = "R", replacement = "Ralston Purina", x = cereal$Manufacturer_Name)

cereal$Manufacturer_Name

# Replace H and C in Type with Hot and Cold
cereal$Type <- gsub("H", "Hot", x = cereal$Type)
cereal$Type <- gsub("C", "Cold", x = cereal$Type)

# Change cereal type and shelf from character to factor
cereal$Type <- factor(cereal$Type)
cereal$Shelf <- factor(cereal$Shelf)
cereal$Manufacturer <- factor(cereal$Manufacturer)

sapply(cereal, FUN = class)

# Replace negative values with NA
cereal$Carbohydrates[cereal$Carbohydrates < 0] <- NA
cereal$Sugar[cereal$Sugar < 0] <- NA
cereal$Potassium[cereal$Potassium < 0] <- NA

#drop hot cereal so we don't have to deal with outliers
#df = subset(cereal, select = -c(Type) ) #this removes whole column. unnecessary
cereal<-cereal[!(cereal$Type=="Hot"),]


summary(cereal)
str(cereal)

colnames(cereal)
##################################################################################################################
ggplot(cereal) + 
  aes(x = Sugar, y = Calories
      , color = Manufacturer_Name
      #, shape = 
      , alpha = Rating
      , size = Rating) +
  geom_point() +
  theme_classic()


#too many cereal. subset and get top 20 and last 20 based on sugar content
top_sugar <- top_n(x=cereal,n=18,wt=cereal$Sugar)
low_sugar <- top_n(x=cereal,n=-18,wt=cereal$Sugar)

sugar_cereal <- rbind(top_sugar,low_sugar)
str(sugar_cereal)

#Side bar plot
ggplot(data = sugar_cereal, mapping = aes(x = reorder(sugar_cereal$Name,-Sugar) , y = as.numeric(sugar_cereal$Sugar), fill = "sugar")) +
  geom_bar( stat = "identity", alpha = .8, color = "white") +
  theme_classic() +
  theme(text = element_text(size=7)) +
  labs(title = "Sugars by Cereal",
       x = "Cereal Name",
       y = "Sugars" ) +
  coord_flip()


#PIE CHART
cp <-cereal

cp_select<-cp%>%subset(select=c(Manufacturer_Name,Calories),Calories>110)%>%
  count(Calories,Manufacturer_Name)

cp_select

cp_select <- cp_select %>%
  group_by(Manufacturer_Name) %>%
  summarise(Calories = mean(Calories))

x<-cp_select$Calories


brand <-cp_select$Manufacturer_Name


lbl<-paste(brand,"(",x," Cal )")


pie(x,
    labels = lbl,
    cex=0.8,
    col=rainbow(length(x)),
    #higher than median
    main="Manufacturers Highest Average Calories Per Serving")


#
#attempt of half pie
#ggplot(cp_select)+
 # aes(fill = Manufacturer_Name,ymax=sum(Calories), ymin=0, xmax = 2, xmin = 1) + 
  #geom_rect() + 
##  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2))

colnames(cereal)



#sugar per shelf
ggplot(data = cereal) +
  aes(x = cereal$Shelf, as.integer(Sugar), y = as.integer(cereal$Sugar)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(title = "Sugars per Shelf",
       y = "Average Sugars",
       x = "Shelf (Beginning from Floor)") +
  theme_classic()


the_good <- cereal %>% transmute(good = Protein+Fibre+Vitamins)
the_bad <-  cereal %>% transmute(bad = Fat+Sodium+Potassium+Sugar)

cereal.per.serving <- data.frame(
  name=cereal$Name,
  mfr = cereal$Manufacturer_Name,
  good=the_good,
  bad=the_bad,
  rating=cereal$Rating
)

cereal.per.serving
colnames(cereal)
#find what cereal.cup means, make it and finish this last graph to start 719 poster



                    
calories <- cereal %>% transmute(calories= round(as.numeric(cereal$Calories) / as.numeric(cereal$Cups)))
protein <- cereal %>% transmute(protein= round(as.numeric(cereal$Protein) / as.numeric(cereal$Cups)))
fat  <- cereal %>% transmute(fat= round(as.numeric(cereal$Fat) / as.numeric(cereal$Cups)))
sodium  <- cereal %>% transmute(sodium = round(as.numeric(cereal$Sodium) / as.numeric(cereal$Cups)))
fiber  <- cereal %>% transmute(fiber = round(as.numeric(cereal$Fibre) / as.numeric(cereal$Cups)))
carbo  <- cereal %>% transmute(carbo = round(as.numeric(cereal$Carbohydrates) / as.numeric(cereal$Cups)))
sugars  <- cereal %>% transmute(sugars = round(as.numeric(cereal$Sugar) / as.numeric(cereal$Cups)))
potass  <- cereal %>% transmute(potass = round(as.numeric(cereal$Potassium) / as.numeric(cereal$Cups)))
vitamins  <- cereal %>% transmute(vitamins = round(as.numeric(cereal$Vitamins) / as.numeric(cereal$Cups)))
shelf  <- cereal %>% transmute(shelf = round(as.numeric(cereal$Shelf) / as.numeric(cereal$Cups)))
rating  <- cereal %>% transmute(rating = cereal$Rating)
                    
                                        
cereal.cups <- data.frame( 
  name = cereal$Name,
  mfr = cereal$Manufacturer,
  Calories= calories,
  Protein= protein,
  Fat= fat,
  Sodium = sodium,
  Fibre = fiber,
  Carbohydrates = carbo,
  Sugar = sugars,
  Potassium = potass,
  Vitamins = vitamins,
  Shelf = shelf,
  Rating = rating
  )
str(cereal.cups)
the_good2 <- cereal.cup %>% transmute(good = protein+fiber+vitamins)
the_bad2 <-  cereal.cup %>% transmute(bad = fat+sodium+potass+sugars)

cereal.per.cup <- data.frame( 
  name = cereal.cups$name,
  mfr = cereal.cup$mfr,
  good = the_good2,
  bad = the_bad2,
  rating = rating
)


plot_combined <- function(cereals, xlim, ylim, title){
  ggplot(data=cereal, aes(x=the_bad, y=the_good, col=Rating)) +
    geom_jitter() +
    geom_text(aes(label=Manufacturer),hjust=0, vjust=0) +
    labs(x="Bad (fat, sodium, potass, sugars)", y="Good (protein, fiber, vitamins)") +
    coord_cartesian(xlim=xlim, ylim=ylim) +
    scale_color_gradient(low="blue", high="red") +
    ggtitle(title) +
    theme(legend.position="bottom")
}

install.packages("ggplot2")
library(ggplot2)
library(gridExtra)
plot1 <- plot_combined(cereals.per.serving, c(0, 1300), c(0, 150), "Combined Nutrition per Serving")
plot2 <- plot_combined(cereals.per.cup, c(0, 1300), c(0, 150), "Combined Nutrition per Cup")
grid.arrange(plot1, plot2, ncol=2)





