


#FINAL PROJECT - DATA VISARDS - MSA 8020 - DR. CHENG - FINAL SUBMISSION DATE - 10/9/20


# COMPILED R CODE FOR FINAL PRESENTATION





getwd()
setwd("C:/Users/Miren Patel/Desktop/FALL 2020 - MSDA/MSA 8020/Final Project - MSA 8020")

olist_products_dataset <- read.csv("olist_products_dataset.csv")
product_category_name_translation <- read.csv("product_category_name_translation.csv")
View(olist_products_dataset)
View(product_category_name_translation)

colnames(product_category_name_translation)[1] = "product_category_name"
View(product_category_name_translation)

Product <- merge(olist_products_dataset,product_category_name_translation, by = "product_category_name")
View(Product)
write.csv(Product,"Product.csv")

View(Product)

install.packages("dplyr")
install.packages("ggplot2")
install.packages("gcookbook")
install.packages("readr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("writexl")

require("dplyr")
require("ggplot2")
require("gcookbook")
require("readr")
require("tidyr")
require("lubridate")
require("writexl")

install.packages("choroplethrMaps") 
#Contains 3 maps. 1) US States 2) US Counties 3) Countries of the world.
require("choroplethrMaps")
install.packages("devtools")
require("devtools")
library(devtools)
install_github("trulia/choroplethrMaps")
#install_github("trulia/choroplethr") ## not working
#install.packages("XML", repos = "http://www.omegahat.net/R") ## not working

install.packages("choroplethr",dependencies = TRUE)
#install.packages("XML", type = "binary")
#install.packages("acepack")
require("choroplethr")
install.packages("Rtools")
require("Rtools")
install.packages("get_brmap")
install.packages("ggplot2")
install.packages("maps")
install.packages("mapdata")
install.packages("geobr")
install.packages("scales")
require(scales)
require("ggplot2")
require("maps")
require("mapdata")
require("geobr")

library(dplyr)
library(ggplot2) #popular visualization/charting package.  Created by Hadley Wickham @ Rice University
library(gcookbook)
library(readr)
library(tidyr)
library(lubridate)
library(writexl)

library(readxl)

install.packages("png")
library(png)
install.packages("gifski")
library(gifski)

order_items_products <- read_excel("C:/Users/Miren Patel/Desktop/FALL 2020 - MSDA/MSA 8020/Final Project - MSA 8020/order_items_products.xlsx")
View(order_items_products)


Deliverydummy <- select(order_items_products,order_purchase_timestamp,product_category_name_english)
View(Deliverydummy)


newDummy <- separate(Deliverydummy, order_purchase_timestamp , into = c("order_purchase_timestamp"), sep = " ")
View(newDummy)

Dumbo <- select(order_items_products,order_delivered_customer_date)
View(Dumbo)

newDummy1 <- separate(Dumbo, order_delivered_customer_date , into = c("order_delivered_customer_date"), sep = " ")
View(newDummy1)


FinalDev <- bind_cols(newDummy1[1],newDummy)
View(FinalDev)

#Finaldevdummy <- separate(FinalDev,order_purchase_timestamp, into = c("Year-Purhcase","Month-Purchase","Date-Purchase"))
#View(Finaldevdummy)
#Finaldevdummy1 <- separate(FinalDev,order_delivered_customer_date,into = c("Year-Delivered","Month-Delivered","Date-Delivered"))
#View(Finaldevdummy1)
#DUMMYF <- bind_cols(Finaldevdummy,Finaldevdummy1)
#View(DUMMYF) 

#slicedummy <- DUMMYF[1]
#View(slicedummy)
#remove(slicedummy)
?difftime
FinalDev <- FinalDev %>%
mutate(DeliveryDays = round(difftime(order_delivered_customer_date,order_purchase_timestamp),digits = 0))
View(FinalDev)

FinalDev <- filter(FinalDev,DeliveryDays!="NA")
View(FinalDev) # VERY IMPORTANT 

?group_by






FinalDev$order_delivered_customer_date <- as.POSIXct(FinalDev$order_delivered_customer_date)
FinalDev$order_delivered_customer_date <- format(FinalDev$order_delivered_customer_date,format='%Y-%m')


#FinalDev <- aggregate(FinalDev$DeliveryDays, by=list(FinalDev$DeliveryDays ), FUN = mean)

View(FinalDev)


FinalDev_dummy <- select(FinalDev,order_delivered_customer_date,product_category_name_english, DeliveryDays)

View(FinalDev_dummy)

summary(FinalDev_dummy)
str(FinalDev_dummy)
FinalDev_dummy$DeliveryDays <- as.numeric(FinalDev_dummy$DeliveryDays)
str(FinalDev_dummy)

summary(FinalDev_dummy)


FinalDev_dummy <- filter(FinalDev_dummy, DeliveryDays<=30)
View(FinalDev_dummy)

FVdummy <- group_by(FinalDev_dummy,order_delivered_customer_date,product_category_name_english) %>% summarise(Mean = mean(DeliveryDays))

View(FVdummy)

 


NEWVF <- filter(FVdummy, product_category_name_english == "health_beauty" |
                           product_category_name_english == "watches_gifts" |
                           product_category_name_english == "bed_bath_table" |
                           product_category_name_english == "sports_leisure" |
                           product_category_name_english == "computers_accessories"  )

View(NEWVF)


NEWVF$order_delivered_customer_date <- as.Date(paste(NEWVF$order_delivered_customer_date,1,sep="-"),"%Y-%m-%d")
View(NEWVF)

NEWVF <- NEWVF[1:55,]
View(NEWVF)

NEWVF$product_category_name_english <- colnames("Category")

names(NEWVF)[names(NEWVF)=="product_category_name_english"] <- "Product Category"
View(NEWVF)
#DELIVERY DAYS 
install.packages("gganimate")
install.packages("png")
install.packages("gifski")

library(gganimate)
library(png)
library(gifski)



#INTERACTIVE PLOT

p <- ggplot(
  NEWVF,
  aes(order_delivered_customer_date, Mean, group = `Product Category`, color = `Product Category` )
) +
  geom_line() +
  geom_point()+
 
  labs(x = "Month", y = "Delivery Days") 
  
p

p + 
  geom_point() +
  transition_reveal(order_delivered_customer_date)



install.packages("gganimate")
library(gganimate)
?save_animation
getwd()
#anim_save(p, "C:/Users/Miren Patel/Desktop/FALL 2020 - MSDA/MSA 8020/Final Project - MSA 8020" )

# PLOTTING 

payment_dummy <- select(order_items_products,order_id,payment_type,product_category_name_english)
payment_dummy <- filter(payment_dummy,payment_type!="NA")
View(payment_dummy)

newdfdummy <- group_by(payment_dummy,product_category_name_english)
View(newdfdummy)


newdf2 <- newdfdummy %>% count(product_category_name_english) %>% arrange(desc(n))
View(newdf2)

newdf2 <- newdf2 %>% top_n(10,n)
View(newdf2)
newdf2 <- newdf2[1:10,]
View(newdf2)


# BAR CHART 
ggplot(data=newdf2, aes(x = product_category_name_english , y = n, group=1)) +
  geom_bar(stat = "identity", color = "black", size=1, fill = "aquamarine" ) +
  labs(title = "Transaction Count of Vouchers Per Category", subtitle = "Sept 2017 to Aug 2018", x = "Product Category", y = "Transaction Count") +
  # theme(axis.text.x = element_text(angle = 45)) +
  theme_minimal()

install.packages("plotrix")

library(plotrix)

?p
slices <- c(newdf2$n)
lbls <- c(newdf2$product_category_name_english)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Sum of Vouchers Per Category")












#********************************** HARD THAKKAR'S CODE ****************************

install.packages("dplyr")
install.packages("ggplot2")
install.packages("gcookbook")
install.packages("readr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("writexl")

require("dplyr")
require("ggplot2")
require("gcookbook")
require("readr")
require("tidyr")
require("lubridate")
require("writexl")

install.packages("choroplethrMaps") 
#Contains 3 maps. 1) US States 2) US Counties 3) Countries of the world.
require("choroplethrMaps")
install.packages("devtools")
require("devtools")
library(devtools)
install_github("trulia/choroplethrMaps")
install_github("trulia/choroplethr") ## not working
install.packages("XML", repos = "http://www.omegahat.net/R") ## not working

install.packages("choroplethr",dependencies = TRUE)
#install.packages("XML", type = "binary")
#install.packages("acepack")
require("choroplethr")
install.packages("Rtools")
require("Rtools")
install.packages("get_brmap")
install.packages("ggplot2")
install.packages("maps")
install.packages("mapdata")
install.packages("geobr")
install.packages("scales")
install.packages("plotly")
library(plotly)
require(scales)
require("ggplot2")
require("maps")
require("mapdata")
require("geobr")

library(dplyr)
library(ggplot2) #popular visualization/charting package.  Created by Hadley Wickham @ Rice University
library(gcookbook)
library(readr)
library(tidyr)
library(lubridate)
library(writexl)

library(readxl)

Order_Customer_Product <- read_excel("C:/Users/Hard Thakkar/Desktop/FALL 2020/Data Visualization/HW/HW_2/datasets/Order_Customer1.xlsx")
View(Order_Customer_Product)

items <- read_excel("C:/Users/Hard Thakkar/Desktop/FALL 2020/Data Visualization/HW/HW_2/datasets/olist_order_items_dataset.xlsx")
View(items)


#Code for orderID and payment_type table
orders <- read_excel("C:/Users/Hard Thakkar/Desktop/FALL 2020/Data Visualization/HW/HW_2/datasets/olist_orders_dataset.xlsx")
View(orders)

payments <- read_excel("C:/Users/Hard Thakkar/Desktop/FALL 2020/Data Visualization/HW/HW_2/datasets/olist_order_payments_dataset.xlsx")
View(payments)

orders_payments <- filter(payments , payment_type == "voucher")
View(orders_payments)

orders_payments <- distinct(orders_payments , order_id , payment_type) #only contains order_id && payment type == voucher
View(orders_payments) 

orders <- left_join(orders , orders_payments , by = "order_id") #left join btw orders && payment type == voucher
View(orders)



# Code to merge OrderID and ItemID tables
order_items <- merge(orders , items , by="order_id")
View(order_items)

products <- read_excel("C:/Users/Hard Thakkar/Desktop/FALL 2020/Data Visualization/HW/HW_2/datasets/olist_products_dataset.xlsx")
View(products)

products_category <- read_excel("C:/Users/Hard Thakkar/Desktop/FALL 2020/Data Visualization/HW/HW_2/datasets/product_category_name_translation.xlsx")
View(products_category)


#Code for products dataset merging with product_translation === updated products 
products <- merge(products , products_category , by = "product_category_name")
View(products)


#Code for merging order_items and updates products table
order_items_products <- merge(order_items , products , by = "product_id")
View(order_items_products)

#Filtering the data for 1 year
order_items_products <- filter(order_items_products , order_purchase_timestamp >= "2017-10-01 00:00:00" & order_purchase_timestamp <="2018-09-01 00:00:00")
View(order_items_products)


#writing updated dataset in a xls file
write_xlsx(order_items_products, "C:/Users/Hard Thakkar/Desktop/FALL 2020/Data Visualization/HW/HW_2/datasets/order_items_products.xlsx")




#Call order_items_products dataset
order_items_products <- read_excel("C:/Users/Hard Thakkar/Desktop/FALL 2020/Data Visualization/HW/HW_2/datasets/order_items_products.xlsx")
View(order_items_products)

Order_Customer1 <- read_excel("C:/Users/Hard Thakkar/Desktop/FALL 2020/Data Visualization/HW/HW_2/datasets/Order_Customer1.xlsx")
View(Order_Customer1)

state_order_items_products <- left_join( order_items_products, Order_Customer1[ , c("order_id","Abb. State","State")] , by = "order_id") #left join btw orders && payment type == voucher
View(state_order_items_products)

write_xlsx(state_order_items_products, "C:/Users/Hard Thakkar/Desktop/FALL 2020/Data Visualization/HW/HW_2/datasets/state_order_items_products.xlsx")




#GEOSPATIAL GRAPH

install.packages("ggmap")
library("ggmap")
library(dplyr)


library("ggmap")
data(crime, package="ggmap")

# subset the data
library(dplyr)
rapes <- filter(crime, offense == "rape") %>%
  select(date, offense, address, lon, lat)

# view data
head(rapes)

register_google(key = "AIzaSyB2pJohxVFq2UqaCRK-jpLYylnSzNv9XUY")



Brazil <- read_country(year = 2018, simplified = TRUE, showProgress = TRUE)
Brazil

Brazil_states <- read_state(year=2018)
Brazil_states

dummy_state_freight <- group_by(state_order_items_products , State, `Abb. State`, product_category_name_english)
View(dummy_state_freight)
state_freight <- summarise(dummy_state_freight,  sum_freight_value = sum(freight_value))
View(state_freight)



state_freight <- filter(state_freight , product_category_name_english == c("bed_bath_table" , "computers_accessories" , "health_beauty" , "sports_leisure" , "watches_gifts"))
View(state_freight)

#state_freight <- cbind(state_freight , dummy_state_freight$product_category_name_english)
#View(state_freight)

Brazil_states <-left_join(Brazil_states, state_freight, by = c("abbrev_state" = "Abb. State"))
View(Brazil_states)

install.packages("crosstalk")
library(crosstalk)
install.packages("trelliscopejs")
library(trelliscopejs)
install.packages("htmlwidgets")
library(htmlwidgets)
install.packages("leaflet")
library(leaflet)


options(scipen=100000)

Brazil_states <- cbind(Brazil_states , dustate_freight$product_category_name_english)

tx <- highlight_key(Brazil_states)

p <- ggplot() +
  geom_sf(data=tx, aes(fill=sum_freight_value), color= NA, size=.15) +
  labs(subtitle="Total Freight Value, Brazilian States, 2017-2018", size=8) +
  scale_fill_distiller(palette = "RdBu", name="Total Freight Value") +
  theme_minimal()

ggplotly(p)

widgets <- bscols(
  widths = c(12),
  #filter_select("city", "Cities", tx, ~city),
  #filter_slider("sales", "Sales", tx, ~sales),
  filter_checkbox("product_category_name_english", "Product Categories", tx, ~product_category_name_english, inline = TRUE)
)
bscols(
  widths = c(4, 8), widgets, 
  ggplotly(ggplot() +
             geom_sf(data=tx, aes(fill=sum_freight_value), color= NA, size=.15) +
             labs(subtitle="Total Freight Value, Brazilian States, 2017-2018", size=8) +
             scale_fill_distiller(palette = "RdBu", name="Total Freight Value") +
             theme_minimal())
)











#********************************** JOHN ROCK'S CODE *******************************

setwd("/Users/johnrock/Desktop/hw4vis")

require("lubridate")
require("dplyr")
require("ggplot2")

library(dplyr)
library("readxl")

library(ggplot2)

df <- read_excel("order_items_products.xlsx")
df <- df %>% mutate(DeliveryDays = round(difftime(as.POSIXct(df$order_delivered_customer_date), as.POSIXct(df$order_purchase_timestamp), units="days"), digits = 0))

df2 <- aggregate(df$freight_value, list(df$order_id), FUN = sum)
df2 <- df2 %>% rename(order_id = Group.1, freight_value = x)

df3 <- aggregate(df$DeliveryDays, list(df$order_id), FUN = mean)
df3 <- df3 %>% rename(order_id = Group.1, DeliveryDays = x)

df2 <- df2 %>% mutate(freight_value_interval = case_when(freight_value >= 0  & freight_value < 20 ~ '0-20',
                                                         freight_value >= 20  & freight_value < 40 ~ '20-40',
                                                         freight_value >= 40  & freight_value < 60 ~ '40-60',
                                                         freight_value >= 60 & freight_value < 80 ~ '60-80',
                                                         freight_value >= 80 & freight_value < 100 ~ '80-100',
                                                         freight_value >= 100 ~ '100+'))


df4 <- df3 %>% inner_join(df2)
df4 <- aggregate(DeliveryDays ~ freight_value_interval, data=df4, mean)

g <- ggplot(df4, aes(x = reorder(freight_value_interval, DeliveryDays), y = DeliveryDays))
g + geom_bar(stat="identity", width = 0.5, fill="black") + 
  labs(title="Bar Chart", 
       subtitle="Average number of days for delivery by freight value", 
       caption="Source: Brazillian E-Commerce dataset") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) + xlab("Freight value") +
  ylab("Average number of days for delivery")

setwd("/Users/johnrock/Desktop/hw4vis")

require("lubridate")
require("dplyr")
require("ggplot2")

library(dplyr)
library("readxl")

library(ggplot2)
install.packages("gganimate")
library(gganimate)
theme_set(theme_bw())

install.packages("png")
install.packages("gifski")
library(png)
library(gifski)

ds <- read_excel("order_items_products.xlsx")

ds$order_purchase_timestamp <- as.POSIXct(ds$order_purchase_timestamp)
ds$order_purchase_timestamp <- format(ds$order_purchase_timestamp,format='%Y-%m')
sales_by_month_by_category <- aggregate(ds$price, by=list(Category=ds$product_category_name_english, Month=ds$order_purchase_timestamp), FUN = sum)

sales_by_month_by_category <- filter(sales_by_month_by_category, Category == "health_beauty" | 
                                       Category == "watches_gifts" |
                                       Category == "bed_bath_table" |
                                       Category == "sports_leisure" |
                                       Category == "computers_accesories" |
                                       Category == "furniture_decor" |
                                       Category == "cool_stuff" |
                                       Category == "housewares" |
                                       Category == "auto" |
                                       Category == "garden_tools")


sales_by_month_by_category$Month <- as.Date(paste(sales_by_month_by_category$Month,1,sep="-"),"%Y-%m-%d")

p <- ggplot(data=sales_by_month_by_category, aes(x=Month, y=x, group=Category, color=Category)) +
  geom_line()+
  geom_point()+ylab("Revenue")
p <- p + geom_point(aes(group = seq_along(Month))) + transition_reveal(Month)

orders <- read.csv("olist_orders_dataset.csv")
products <- read.csv("olist_products_dataset.csv")
order_items <- read.csv("olist_order_items_dataset.csv")
review <- read.csv("olist_order_reviews_dataset.csv")
translations <- read.csv("product_category_name_translation.csv")

df <- orders %>% inner_join(order_items)
df <- df %>% inner_join(products)
df <- df %>% left_join(review)
df <- df %>% inner_join(translations)

df <- filter(df, order_status == "delivered")

df$order_purchase_timestamp <- as.POSIXct(df$order_purchase_timestamp)
df$order_purchase_timestamp <- format(df$order_purchase_timestamp,format='%Y-%m')
df$order_purchase_timestamp <- as.Date(paste(df$order_purchase_timestamp,1,sep="-"),"%Y-%m-%d")

View(df)

df2 <- df %>% group_by(order_purchase_timestamp, order_id, product_category_name_english) %>% summarise(Score = mean(review_score, na.rm = TRUE))

df2 <- df2 %>% group_by(order_purchase_timestamp, product_category_name_english) %>% summarise(AverageRatingScore = mean(Score))

df2 <- filter(df2, order_purchase_timestamp >= "2017-10-01 00:00:00" & order_purchase_timestamp <= "2018-09-01 00:00:00")

df2 <- filter(df2,  product_category_name_english == "health_beauty" | 
                product_category_name_english == "watches_gifts" |
                product_category_name_english == "bed_bath_table" |
                product_category_name_english == "sports_leisure" |
                product_category_name_english == "computers_accessories" |
                product_category_name_english == "furniture_decor" |
                product_category_name_english == "cool_stuff" |
                product_category_name_english == "housewares" |
                product_category_name_english == "auto" |
                product_category_name_english == "garden_tools")

View(df2)
names(df2)[names(df2)=="product_category_name_english"] <- "ProductCategory"
p <- ggplot(data=df2, aes(x=order_purchase_timestamp, y=AverageRatingScore, group=ProductCategory, color=ProductCategory)) +
  geom_line()+
  geom_point()+ xlab("Month") + ylab("Average Order Rating")
p + geom_point(aes(group = seq_along(order_purchase_timestamp))) + transition_reveal(order_purchase_timestamp)

library("readxl")

library(ggplot2)

df <- read_excel("order_items_products.xlsx")
df <- filter(df, order_status == "delivered")
df <- filter(df, product_category_name_english == "health_beauty" | 
               product_category_name_english == "watches_gifts" |
               product_category_name_english == "bed_bath_table" |
               product_category_name_english == "sports_leisure" |
               product_category_name_english == "computers_accessories" |
               product_category_name_english == "furniture_decor" |
               product_category_name_english == "cool_stuff" |
               product_category_name_english == "housewares" |
               product_category_name_english == "auto" |
               product_category_name_english == "garden_tools")

df <- aggregate(df$order_id, by=list(Category=df$product_category_name_english), function(x){length(x)})
df

ggplot(df, aes(x = reorder(Category, -x), y = x)) +  geom_point(size = 5) + 
  geom_segment(aes(x = Category, xend = Category, 
                   y = 0, 
                   yend = x)) + 
  labs(title="", 
       subtitle="Total number of purchases by product category", 
       caption="Source: Brazillian E-Commerce dataset") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) + xlab("Product Category") + ylab("Total Orders")

orders <- read.csv("olist_orders_dataset.csv")
products <- read.csv("olist_products_dataset.csv")
order_items <- read.csv("olist_order_items_dataset.csv")
review <- read.csv("olist_order_reviews_dataset.csv")
translations <- read.csv("product_category_name_translation.csv")

df <- orders %>% inner_join(order_items)
df <- df %>% inner_join(products)
df <- df %>% left_join(review)
df <- df %>% inner_join(translations)

df <- filter(df, order_status == "delivered")

df$order_purchase_timestamp <- as.POSIXct(df$order_purchase_timestamp)
df$order_purchase_timestamp <- format(df$order_purchase_timestamp,format='%Y-%m')
df$order_purchase_timestamp <- as.Date(paste(df$order_purchase_timestamp,1,sep="-"),"%Y-%m-%d")

View(df)

df2 <- df %>% group_by(order_purchase_timestamp, order_id, product_category_name_english) %>% summarise(Score = mean(review_score, na.rm = TRUE))

df2 <- df2 %>% group_by(order_purchase_timestamp, product_category_name_english) %>% summarise(AverageRatingScore = mean(Score))

df2 <- filter(df2, order_purchase_timestamp >= "2017-10-01 00:00:00" & order_purchase_timestamp <= "2018-09-01 00:00:00")

df2 <- filter(df2,  product_category_name_english == "health_beauty" | 
                product_category_name_english == "watches_gifts" |
                product_category_name_english == "bed_bath_table" |
                product_category_name_english == "sports_leisure" |
                product_category_name_english == "computers_accessories" |
                product_category_name_english == "furniture_decor" |
                product_category_name_english == "cool_stuff" |
                product_category_name_english == "housewares" |
                product_category_name_english == "auto" |
                product_category_name_english == "garden_tools")

View(df2)
names(df2)[names(df2)=="product_category_name_english"] <- "ProductCategory"
p <- ggplot(data=df2, aes(x=order_purchase_timestamp, y=AverageRatingScore, group=ProductCategory, color=ProductCategory)) +
  geom_line()+
  geom_point()+ xlab("Month") + ylab("Average Order Rating")
p + geom_point(aes(group = seq_along(order_purchase_timestamp))) + transition_reveal(order_purchase_timestamp)

df <- read_excel("order_items_products.xlsx")

df <- filter(df,  product_category_name_english == "health_beauty" | 
               product_category_name_english == "watches_gifts" |
               product_category_name_english == "bed_bath_table" |
               product_category_name_english == "sports_leisure" |
               product_category_name_english == "computers_accessories" |
               product_category_name_english == "furniture_decor" |
               product_category_name_english == "cool_stuff" |
               product_category_name_english == "housewares" |
               product_category_name_english == "auto" |
               product_category_name_english == "garden_tools")

df$order_purchase_timestamp <- as.POSIXct(df$order_purchase_timestamp)
df$order_purchase_timestamp <- format(df$order_purchase_timestamp,format='%Y-%m')
df$order_purchase_timestamp <- as.Date(paste(df$order_purchase_timestamp,1,sep="-"),"%Y-%m-%d")

df2 <- df %>% group_by(order_purchase_timestamp, order_id, product_category_name_english) %>% summarise(Cost = sum(price, na.rm = TRUE))

df2 <- df2 %>% group_by(order_purchase_timestamp, product_category_name_english) %>% summarise(AverageTransactionCost = mean(Cost))
View(df2)
names(df2)[names(df2)=="product_category_name_english"] <- "ProductCategory"
p <- ggplot(data=df2, aes(x=order_purchase_timestamp, y=AverageTransactionCost, group=ProductCategory, color=ProductCategory)) +
  geom_line()+
  geom_point()+ xlab("Month") + ylab("Average Transaction Cost")
p
p + geom_point(aes(group = seq_along(order_purchase_timestamp))) + transition_reveal(order_purchase_timestamp)

ds <- read_excel("order_items_products.xlsx")

ds$order_purchase_timestamp <- as.POSIXct(ds$order_purchase_timestamp)
ds$order_purchase_timestamp <- format(ds$order_purchase_timestamp,format='%Y-%m')
sales_by_month_by_category <- aggregate(ds$price, by=list(Category=ds$product_category_name_english, Month=ds$order_purchase_timestamp), FUN = sum)

sales_by_month_by_category <- filter(sales_by_month_by_category, Category == "health_beauty" | 
                                       Category == "watches_gifts" |
                                       Category == "bed_bath_table" |
                                       Category == "sports_leisure" |
                                       Category == "computers_accesories" |
                                       Category == "furniture_decor" |
                                       Category == "cool_stuff" |
                                       Category == "housewares" |
                                       Category == "auto" |
                                       Category == "garden_tools")


sales_by_month_by_category$Month <- as.Date(paste(sales_by_month_by_category$Month,1,sep="-"),"%Y-%m-%d")
names(sales_by_month_by_category)[names(sales_by_month_by_category)=="Category"] <- "ProductCategory"
p <- ggplot(data=sales_by_month_by_category, aes(x=Month, y=x, group=ProductCategory, color=ProductCategory)) +
  geom_line()+
  geom_point()+ylab("Revenue")
p + geom_point(aes(group = seq_along(Month))) + transition_reveal(Month)

df <- read_excel("order_items_products.xlsx")
df <- df %>% mutate(DeliveryDays = round(difftime(as.POSIXct(df$order_delivered_customer_date), as.POSIXct(df$order_purchase_timestamp), units="days"), digits = 0))

df2 <- aggregate(df$price, list(df$order_id), FUN = sum)
df2 <- df2 %>% rename(order_id = Group.1, price = x)

df3 <- aggregate(df$DeliveryDays, list(df$order_id), FUN = mean)
df3 <- df3 %>% rename(order_id = Group.1, DeliveryDays = x)

df2 <- df2 %>% mutate(price_interval = case_when(price >= 0  & price < 50 ~ '0-50',
                                                 price >= 50  & price < 100 ~ '50-100',
                                                 price >= 100  & price < 150 ~ '100-150',
                                                 price >= 150 & price < 200 ~ '150-200',
                                                 price >= 200 & price < 250 ~ '200-250',
                                                 price >= 250 ~ '250+'))


df4 <- df3 %>% inner_join(df2)
df4 <- aggregate(DeliveryDays ~ price_interval, data=df4, mean)

g <- ggplot(df4, aes(x = reorder(price_interval, DeliveryDays), y = DeliveryDays))
g + geom_bar(stat="identity", width = 0.5, fill="black") + 
  labs(title="Bar Chart", 
       subtitle="Average number of days for delivery by cost of order", 
       caption="Source: Brazillian E-Commerce dataset") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) + xlab("Order cost") +
  ylab("Average number of days for delivery")

df <- read_excel("order_items_products.xlsx")
df <- df %>% mutate(DeliveryDays = round(difftime(as.POSIXct(df$order_delivered_customer_date), as.POSIXct(df$order_purchase_timestamp), units="days"), digits = 0))

df2 <- aggregate(df$product_weight_g, list(df$order_id), FUN = sum)
df2 <- df2 %>% rename(order_id = Group.1, order_weight_g = x)

df3 <- aggregate(df$DeliveryDays, list(df$order_id), FUN = mean)
df3 <- df3 %>% rename(order_id = Group.1, DeliveryDays = x)

df2 <- df2 %>% mutate(weight_interval = case_when(order_weight_g >= 0  & order_weight_g < 500 ~ '0-500',
                                                  order_weight_g >= 500  & order_weight_g < 1000 ~ '500-1000',
                                                  order_weight_g >= 1000  & order_weight_g < 1500 ~ '1000-1500',
                                                  order_weight_g >= 1500 & order_weight_g < 2000 ~ '1500-2000',
                                                  order_weight_g >= 2000 & order_weight_g < 2500 ~ '2000-2500',
                                                  order_weight_g>= 2500 ~ '2500+'))


df4 <- df3 %>% inner_join(df2)
df4 <- aggregate(DeliveryDays ~ weight_interval, data=df4, mean)

g <- ggplot(df4, aes(x = reorder(weight_interval, DeliveryDays), y = DeliveryDays))
g + geom_bar(stat="identity", width = 0.5, fill="black") + 
  labs(title="Bar Chart", 
       subtitle="Average number of days for delivery by order weight (in grams)", 
       caption="Source: Brazillian E-Commerce dataset") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) + xlab("Order weight") +
  ylab("Average number of days for delivery")











#********************************** KUNAL KALRA'S CODE ************************************

setwd("C:/Users/K2/OneDrive - Georgia State University/GSU - MSA - Assignment Submission/MSA 8020 - Data Visualization")
getwd()

library(dplyr)
library(sqldf)
library(readxl)
library(tidyverse)
library(ggplot2)
library(plotrix)

require(data.table)

df<-read_xlsx("Order_Customer.xlsx")

df_trxns<-read_xlsx("customers_transactions.xlsx")
df_trxns

rm(df_visits)

#Drop Rate Chart
ggplot(data=df_trxns, aes(x = Transactions, y = df_trxns$`%Customers`)) +
  geom_bar(stat="identity",color = "peru", size=1, fill = "dodgerblue4") +
  #geom_text() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Drop Rate", x = "Total Transactions", y = "%Customers") +
  geom_point() +
  geom_line(data=df_trxns,stat='identity') +
  theme_minimal()

#Transacting Customers chart
df_monthly_cust<-read_xlsx("monthly_transacting_customers.xlsx")
df_monthly_cust

df_monthly_cust$Month2<-format(as.Date(df_monthly_cust$Month, "%Y-%m-%d"), "%Y-%m")
df_monthly_cust

coeff<-0.00001

ggplot(data=df_monthly_cust, aes(x = Month)) +
  geom_bar(aes(y = `Transacting Customers`), stat="identity", color = "gray27", size=1, fill = "coral2") +
  geom_line(aes(y = `%Repeaters`/coeff), stat="identity", size=1.2, color = "dodgerblue3") +
  scale_y_continuous(
    name="Transacting Customers",
    sec.axis=sec_axis(~.*coeff,name = "%Repeat Customers", labels = scales::percent)
  )+
  labs(title = "Monthly Transacting Customers", x = "Month")+
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "coral2", size=13),
    axis.title.y.right = element_text(color = "dodgerblue3", size=13)
  )

#Monthly Revenue chart
df_monthly_rev<-read_xlsx("monthly_revenue.xlsx")
df_monthly_rev

coeff<-0.00000001

ggplot(data=df_monthly_rev, aes(x = Month)) +
  geom_bar(aes(y = `Revenue`), stat="identity", color = "gray28", size=1, fill = "indianred1") +
  geom_line(aes(y = `Revenue By Repeaters`/coeff), stat="identity", size=1.2, color = "lightseagreen") +
  scale_y_continuous(
    name="Revenue",
    sec.axis=sec_axis(~.*coeff,name = "%Revenue by Repeaters", labels = scales::percent)
  )+
  labs(title = "Monthly Revenue", x = "Month")+
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "indianred1", size=13),
    axis.title.y.right = element_text(color = "lightseagreen", size=13)
  )


#Payments Analysis - Graph for Transactions vs Order Value
df_order_value<-read_xlsx("Order_amount_bin.xlsx")
view(df_order_value)

#coeff<-0.000001

p<-ggplot(data=df_order_value, aes(x = reorder(`Order Amount`,num))) +
  geom_bar(aes(y = `Transactions`), stat="identity", color = "gray28", size=1, fill = "indianred1", position = "dodge") +
  #geom_bar(aes(y = `%Discount_Availed`/coeff), stat="identity", size=1.2, color = "lightseagreen", fill = "blue") +
  scale_y_continuous(
    name="Transactions"
    #sec.axis=sec_axis(~.*coeff,name = "%Discount Availed", labels = scales::percent)
  )+
  labs(title = "Transactions vs Order Value", x = "Order Value")+
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "indianred1", size=13)
    #axis.title.y.right = element_text(color = "lightseagreen", size=13)
  )
p

#Payments Analysis - Graph for Discount vs Order Value
ggplot(data=df_order_value, aes(x = reorder(`Order Amount`,num))) +
  #geom_bar(aes(y = `Transactions`), stat="identity", color = "gray28", size=1, fill = "indianred1", position = "dodge") +
  geom_bar(aes(y = `%Discount_Availed`), stat="identity", size=1, color = "lightseagreen", fill = "blue") +
  scale_y_continuous(
    name="%Discount Applied", labels = scales::percent
    #sec.axis=sec_axis(~.*coeff,name = "%Discount Availed", labels = scales::percent)
  )+
  labs(title = "Avg %Discount vs Order Value", x = "Order Value")+
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "lightseagreen", size=13)
    #axis.title.y.right = element_text(color = "lightseagreen", size=13)
  )

#Graph for Cancelled orders
cancelled_orders<-read_xlsx("Cancelled_Orders.xlsx")
view(cancelled_orders)

ggplot(data=cancelled_orders, aes(x = `Payment Mode`)) +
  geom_bar(aes(y = `Revenue_Cancelled_Orders`), stat="identity", color = "gray28", size=1, fill = "red", position = "dodge") +
  # geom_line(aes(y = `%Cancelled Transactions`), stat="identity", size=1.2, color = "lightseagreen", fill = "blue") +
  scale_y_continuous(
    name="Revenue (Cancelled Orders)", 
    #sec.axis=sec_axis(~.*coeff,name = "%Cancelled Orders", labels = scales::percent)
  )+
  labs(title = "Cancelled Orders' Revenue by Payment Mode", x = "Mode of Payment")+
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black", size=13)
    # axis.title.y.right = element_text(color = "lightseagreen", size=13)
  )


#Graph for Cancelled Orders by Payment Mode
coeff<-0.0005

ggplot(data=cancelled_orders, aes(x = `Payment Mode`)) +
  geom_bar(aes(y = `Cancelled Transactions`), stat="identity", color = "gray28", size=1, fill = "darkorange", position = "dodge") +
  geom_line(aes(y = `%Cancelled Transactions`/coeff), stat="identity", size=1.2, color = "slateblue1", group=1) +
  scale_y_continuous(
    name="Cancelled Transactions", 
    sec.axis=sec_axis(~.*coeff,name = "%Cancelled Orders", labels = scales::percent)
  )+
  labs(title = "Cancelled Orders by Payment Mode", x = "Mode of Payment")+
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "darkorange", size=13),
    axis.title.y.right = element_text(color = "slateblue1", size=13)
  )


#Which Categories drive Repeats?
Category_Repeat<-read_xlsx("Repeat_Category.xlsx")
Category_Repeat

ggplot(data=Category_Repeat, aes(x = reorder(`Product Category`,-`Repeat Transactions`))) +
  geom_bar(aes(y = `Repeat Transactions`), stat="identity", color = "gray28", size=1, fill = "cyan", position = "dodge") +
  labs(title = "Repeat Driving Categories", x = "Product Categories")+
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.text.x = element_text(angle = 45)
  )