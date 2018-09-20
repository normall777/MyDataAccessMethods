#Загрузите данные о землятресениях
anss <- readLines("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/earthquakes_2011.html", warn=FALSE)
#Выберите строки, которые содержат данные с помощью регулярных выражений и функции grep

vector<-grep(pattern="[0-9]{4}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}",anss)

#Проверьте что все строки (all.equal) в результирующем векторе подходят под шаблон.
#Проверка 1 - дополнен шаблон .[0-9]{2}
all.equal(anss[vector], anss[grep(pattern="[0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}",anss)])
#Проверка 2 - сверка по другому паттерну ,[0-9]{0,10}$
all.equal(anss[vector], anss[grep(pattern=",[0-9]{0,10}$",anss)])
