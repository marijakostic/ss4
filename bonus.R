# BONUS ZADATAK

#install.packages("htmlwidgets")
library(htmlwidgets)

string<-c("06435.213", "aswww", "2112*121", "011", "232424321", "1232", "23423aaa21321", "0.1", "3424",
          "123*131", "232 232", "2.3", "4543.45")

str_view(string, "((.*[^0123456789\\.].*)|(^0+[^\\.].*))",match = FALSE)

