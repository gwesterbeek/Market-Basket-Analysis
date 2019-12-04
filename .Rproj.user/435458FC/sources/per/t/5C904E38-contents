#Install packages and libraries####
install.packages("arules")
install.packages("arulesViz")
install.packages("caTools")
library("arules")
library("caTools")
library("arulesViz")
library("ggplot2")
library("dplyr")

# Read Datafile####
Dataset <- read.transactions("ElectronidexTransactions2017.csv",
format = "basket", sep=",", rm.duplicates=TRUE)

# Create categories####

laptop <- c("LG Touchscreen Laptop",
            "Acer Aspire",
            "HP Laptop",
            "ASUS Chromebook",
            
            "Apple MacBook Pro",
            
            "Apple MacBook Air",
            
            "Dell Laptop",
            
            "Eluktronics Pro Gaming Laptop",
            
            "Alienware AW17R4-7345SLV-PUS 17 Laptop",
            
            "HP Notebook Touchscreen Laptop PC",
            
            "Alienware Laptop")



desktop <- c("Lenovo Desktop Computer",
             
             "iMac",
             
             "HP Desktop",
             
             "ASUS Desktop",
             
             "Dell Desktop",
             
             "Intel Desktop",
             
             "Acer Desktop",
             
             "CYBERPOWER Gamer Desktop",
             
             "Dell 2 Desktop")



monitor <- c("Acer Monitor",
             
             "LG Monitor",
             
             "ASUS Monitor",
             
             "ASUS 2 Monitor",
             
             "Dell Monitor",
             
             "Samsung Monitor",
             
             "Sceptre Monitor",
             
             "ViewSonic Monitor",
             
             "AOC Monitor",
             
             "HP Monitor")



mouse <- c("3-Button Mouse",
           
           "Logitech Wireless Mouse",
           
           "Microsoft Basic Optical Mouse",
           
           "Logitech 3-button Mouse",
           
           "Redragon Gaming Mouse",
           
           "HP Wireless Mouse",
           
           "Generic Black 3-Button",
           
           "Wireless Portable Mouse",
           
           "Gaming Mouse Professional",
           
           "Slim Wireless Mouse")



keyboard <- c("HP USB Keyboard",
              
              "Logitech Wireless Keyboard",
              
              "Rii LED Keyboard",
              
              "Logitech Keyboard",
              
              "Backlit LED Gaming Keyboard",
              
              "Dell Wired Keyboard",
              
              "Apple Wired Keyboard",
              
              "Apple Wireless Keyboard",
              
              "Apple Magic Keyboard")



mousekeycombo <- c("Logitech MK550 Wireless Wave Keyboard and Mouse Combo",
                   
                   "Logitech Desktop MK120 Mouse and keyboard Combo",
                   
                   "Logitech MK270 Wireless Keyboard and Mouse Combo",
                   
                   "Dell KM117 Wireless Keyboard & Mouse",
                   
                   "EagleTec Wireless Combo Keyboard and Mouse",
                   
                   "Microsoft Wireless Comfort Keyboard and Mouse",
                   
                   "Microsoft Wireless Desktop Keyboard and Mouse",
                   
                   "Rii LED Gaming Keyboard & Mouse Combo",
                   
                   "Logitech MK360 Wireless Keyboard and Mouse Combo")



compheadphone <- c("Zombie Gaming Headset",
                   
                   "Logitech ClearChat Headset",
                   
                   "Panasonic On-Ear Stereo Headphones RP-HT21",
                   
                   "PC Gaming Headset",
                   
                   "Kensington Headphones",
                   
                   "Logitech Stereo Headset",
                   
                   "Koss Home Headphones",
                   
                   "Microsoft Headset",
                   
                   "Ailihen Stereo Headphones",
                   
                   "XIBERIA Gaming Headset")



actvheadphone <- c("Apple Earpods",
                   
                   "Monster Beats By Dr Dre",
                   
                   "Otium Wireless Sports Bluetooth Headphone",
                   
                   "Panasonic In-Ear Headphone",
                   
                   "APIE Bluetooth Headphone",
                   
                   "Philips Flexible Earhook Headphone",
                   
                   "Panasonic On-Ear Stereo Headphones")



cords <- c("HDMI Cable 6ft",
           
           "Ethernet Cable",
           
           "Etekcity Power Extension Cord Cable",
           
           "Audio Cable",
           
           "VGA Monitor Cable",
           
           "iPhone Charger Cable",
           
           "HDMI Adapter",
           
           "USB Cable",
           
           "Samsung Charging Cable")



software <- c("Microsoft Office Home and Student 2016",
              
              "Computer Game")



accesories <- c ("Belkin Mouse Pad",
                 
                 "Large Mouse Pad")



speaker <- c("Cambridge Bluetooth Speaker",
             
             "JBL Splashproof Portable Bluetooth Speaker",
             
             "DOSS Touch Wireless Bluetooth",
             
             "Logitech Multimedia Speakers",
             
             "Rokono Mini Speaker",
             
             "Cyber Acoustics",
             
             "Bose Companion Speakers",
             
             "Mackie CR Speakers",
             
             "Sonos")



printer <- c("Epson Printer",
             
             "HP Wireless Printer",
             
             "Canon Office Printer",
             
             "Brother Printer",
             
             "DYMO Label Manker")



printerink <- c("Epson Black Ink",
                
                "HP Black & Tri-color Ink",
                
                "Canon Ink",
                
                "Brother Printer Toner",
                
                "DYMO Labeling Tape")



computerstand <- c("Halter Acrylic Monitor Stand",
                   
                   "Height-Adjustable Standing Desk",
                   
                   "Multi Media Stand",
                   
                   "Halter Mesh Metal Monitor Stand",
                   
                   "Full Motion Monitor Mount")



tablets <- c("iPad",
             
             "iPad Pro",
             
             "Fire HD Tablet",
             
             "Samsung Galaxy Tab",
             
             "Samsung Galaxy Tablet",
             
             "Kindle")





harddrive <- c("1TB Portable External Hard Drive",
               
               "2TB Portable External Hard Drive",
               
               "5TB Desktop Hard Drive",
               
               "Slim 2TB Portable External Hard Drive",
               
               "3TB Portable External Hard Drive")



smarthomedevice <- c("Apple TV",
                     
                     "Google Home",
                     
                     "Smart Light Bulb",
                     
                     "Fire TV Stick",
                     
                     "Roku Express")


# Create dataframe
library(readxl)
Dictionarycategories <- read_excel("Dictionarycategories.xlsx", 
                                   sheet = "Blad1", col_names = FALSE)

names(Dictionarycategories) <- c("labels","categories")

Dataset@itemInfo <- Dataset@itemInfo %>% 
        left_join(y = Dictionarycategories, by = "labels")

cat_trans <- aggregate(Dataset, by = "categories")
summary(cat_trans)

#Explore Data####
duplicated(Dataset)
inspect(Dataset)
length(Dataset)
size(Dataset)
LIST(Dataset)
itemLabels(Dataset)
summary(Dataset)

#Visualize Data
par(mar=c(4,1,1,1))
itemFrequencyPlot(Dataset, topN = 5)
itemFrequencyPlot(Dataset, support = 0.1)
image(sample(Dataset, 100))
itemFrequencyPlot(Dataset, type = c("absolute"), 
                  weighted = FALSE, support = NULL, topN = 10,
                  population = NULL, popCol = "black", popLwd = 1,
                  lift = FALSE, horiz = FALSE)

#Top 10 most frequent items, both by frequency and absolute counts
par(mar=c(1,4,1,1))
par(mfrow=c(1,2))
itemFrequencyPlot(Dataset,
                  type="relative",
                  topN=10, # can be changed to the number of interest
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency, relative')

itemFrequencyPlot(Dataset,
                  type="absolute",
                  topN=15, # can be changed to the number of interest
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency, absolute')

# Least frequently bought items
par(mar=c(2,10,1,1))
mfrow=c(1,2)

barplot(sort(table(unlist(LIST(Dataset))))[1:10]/9835,
        horiz=TRUE,
        las=1,
        col='steelblue3',
        xlab='',
        main='Frequency, relative')

barplot(sort(table(unlist(LIST(Dataset))))[1:10],
        horiz=TRUE,
        las=1,
        col='steelblue3',
        xlab='',
        main='Frequency, absolute')

cont_table<- crossTable(Dataset, sort = TRUE)
cont_table[1:5,1:5]


#Apriori Principle####


RulesName<- apriori (Dataset, parameter = list(supp = 0.01,
                                                conf = 0.5))

summary(RulesName)
list(RulesName)

inspect(sort(RulesName, by='support', decreasing = T)[1:19])

inspect(sort(subset(RulesName, subset = items %in% "iMac"), 
                    by='lift', decreasing = T))

ItemRules <- subset(RulesName, items %in% "Dell Desktop")
is.redundant(RulesName)

#Visualise results####
plot(RulesName, interactive = T)
plot(RulesName[1:70], method="graph", control=list(type="items"))
plot(RulesName, method = "grouped matrix", engine = "interactive")
#Explore Data categrories####



duplicated(cat_trans)
inspect(cat_trans)
length(cat_trans)
size(cat_trans)
LIST(cat_trans)
itemLabels(cat_trans)
summary(cat_trans)
cat_trans$Product_Category <- labels()

Sales_Electro <- data.frame(cat_trans)

#Visualize Data categories####
par(mar=c(4,1,1,1))
itemFrequencyPlot(cat_trans, topN = 10)
itemFrequencyPlot(cat_trans, support = 0.1)
image(sample(cat_trans, 100))
itemFrequencyPlot(cat_trans, type = c("absolute"), 
                  weighted = FALSE, support = NULL, topN = 18,
                  population = NULL, popCol = "black", popLwd = 1,
                  lift = FALSE, horiz = FALSE)

#Top 10 most frequent items, both by frequency and absolute counts
par(mar=c(1,4,1,1))
par(mfrow=c(1,2))
itemFrequencyPlot(cat_trans,
                  type="relative",
                  topN=18, # can be changed to the number of interest
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency, relative')



itemFrequencyPlot(cat_trans,
                  type="absolute",
                  topN=18, 
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency, absolute')


cont_table<- crossTable(cat_trans, sort = TRUE)
cont_table[1:5,1:5]


#Apriori Principle categories####
RulesName1<- apriori (cat_trans, parameter = list(supp = 0.01,
                                               conf = 0.01))

summary(RulesName1)
inspect(sort(RulesName1, by='lift', decreasing = F)[1:100])

inspect(sort(subset(RulesName1, subset = items %in% "desktop"), 
             by='lift', decreasing = T))

ItemRules <- subset(RulesName, items %in% "Dell Desktop")
is.redundant(RulesName)


#check effect of appearance of Desktop

RulesNameDesktop<- apriori (cat_trans, parameter = list(supp = 0.01,
                                                        conf = 0.4)) 
                               
inspect(subset(RulesNameDesktop,
               subset=lhs %ain% c("desktop", "laptop")), by="lift")

subset <- subset(RulesName, 
                 lhs %in% c("Lenovo Desktop Computer",
                                     "Acer Aspire",
                                     "Acer Desktop",
                                     "ASUS Chromebook",
                                     "Alienware Laptop",
                                     "Apple MacBook Pro",
                                     "Apple MacBook Air",
                                     #"ASUS Chromebook",
                                     "CYBERPOWER Gamer Desktop",
                                     "Dell Desktop",
                                     "Dell Laptop",
                                     "HP Desktop",
                                     "HP Laptop",
                                     "HP Notebook Touchscreen Laptop PC",
                                     "iMac",
                                     "LG Touchscreen Laptop"))
inspect(subset)

saveRDS(subset, "subset_rules.rds")
inspect(subset)
inspect(sort(subset, by='lift', decreasing = T))
plot(subset, method = "grouped matrix", engine = "interactive")


summary(RulesNameDesktop)
inspect(sort(RulesNameDesktop, by='support', decreasing = T))

plot(RulesName1, interactive = T)

arulesViz::ruleExplorer(RulesName)

#Visualise results####
plot(subset, interactive = T)
plot(RulesName1[1:10], method="graph", control=list(type="items"))


# from dataframe to transaction matrix
temp <- as_tibble(t(as.matrix(cat_trans@data)))
arulesViz::ruleExplorer(subset)
arulesViz::ruleExplorer(RulesName)

