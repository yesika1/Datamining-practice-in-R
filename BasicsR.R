#important rJava and xlsx
#https://github.com/snowflakedb/dplyr-snowflakedb/wiki/Configuring-R-rJava-RJDBC-on-Mac-OS-X
#http://charlotte-ngs.github.io/2016/01/MacOsXrJavaProblem.html


a <-  c(1,2,3)
b <-  c(4,5,6)
c <- rbind(a,b)
print(c)
class(c)

mat <- matrix(1:9,nrow=3)
print(mat)
mat2 <- matrix(1:25,nrow=5, byrow = T)
print(mat2)
is.matrix(mat2)

split1 <-mat2[2:3,2:3]
split2 <-mat2[4:5,4:5]
split2 

sum(mat2)

v <- matrix(runif(20, min=1, max=100), nrow=4)
v


# Some made up weather data
days <- c('mon','tue','wed','thu','fri')
temp <- c(22.2,21,23,24.3,25)
rain <- c(TRUE, TRUE, FALSE, FALSE, TRUE)

# Pass in the vectors:
df <- data.frame(days,temp,rain)

str(df)
head(df)

df$days
df['days']
df[,'days']

sorting <- order(df['temp'])
sorting <- order(df[,'temp'])
sorting <- order(df$temp)
sorting  

df[sorting,]
df[order(-df['temp']),]

df
df[[3,'rain']]
df[3,'rain']


df[[3,'rain']] <-TRUE
df[3,'rain'] <-FALSE

df$temp
df['temp']
df[,'temp']
df[['temp']]

df[,c('temp','days')]
df[1:2,c('temp','days')]

df$new1 <- df$days
df
df[,'new2'] <- df$days
df
df[,'new3'] <- df[,'days']
df

head(mtcars)
mtcars[mtcars$mpg>20,]
df[is.na(mtcars)]


Age <- c(22,25,26)
Weight <- c(150,165,120)
Sex <- c('M','M','F')
people <- data.frame(Age,Weight,Sex)
rownames(people) <- c('Sam','Frank','Amy')

is.data.frame(mtcars) #true

mat3 <- matrix(1:25,nrow = 5)
as.data.frame(mat3)

cars <- mtcars
head(cars)
mean(cars$mpg) #[1] 20.09062

cars[cars$cyl==6,]
cars[,c('am','gear','carb')]

cars$performance <- cars$hp/cars$wt
cars$performance <-round(cars$performance,2)
head(cars)
str(cars)

cars100 <- cars[cars$hp > 100 & cars$wt >2.5,]
mean(cars100$mpg) #16.86364

cars['Hornet Sportabout', 'mpg'] #[1] 18.7

#list: for organization, not manipulation.
v <- c(1:5)
m <- matrix(1:25,nrow= 5)
df4 <-mtcars

myList <- list(v,m,df4)
#Variable indexes: [[1]] = v,    [[2]] = m,  [[3]] = df4
myList [[2]] #matrix

myNamedList <- list(myVector=v, myMatrix=m, myDf=df4)
# variable notation: $myVector, $myMatrix, $myDf
myNamedList$myMatrix # extract object
myNamedList[['myMatrix']] # extract object
myNamedList['myMatrix'] #tell object with header, not manipulate

doubleList <- c(myList,myNamedList)

write.csv(mtcars,file = 'myexample.csv')
example <-read.csv('myexample.csv')
head(example)
write.csv(example, file ='myNewEx.csv')

library(readxl)
excel_sheets('file.xls') #read name sheets in the workbook
df5 <- read_excel('file.xls', sheet = 'sheet1') #create the df
head(df5)

#generate list of sheets
entire.workbook <- lapply(excel_sheets('file.xls'), read_excel, path='file.xls')

#making rJava available
#dyn.load('/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/lib/server/libjvm.dylib')
#require(rJava)
#.jinit()
#.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
#sessionInfo()
#library(rJava)

library(xlsx)
write.xlsx(mtcars,'outputExample.xlsx')

#Often, %>% is called multiple times to "chain" functions together,
# which accomplishes the same result as nesting. 
# For example in the chain below, iris is passed to head(), 
# then the result of that is passed to summary().
# iris %>% head() %>% summary()
# Thus iris %>% head() %>% summary() is equivalent to summary(head(iris))


#scrapping 
# https://www.import.io/builder/

install.packages('rvest')
library(rvest)
demo(package = 'rvest')
demo(package = 'rvest', topic = 'tripadvisor')

# if conditionals
x <- 10
if (x==10){
  #code execute if condition is true
  print('x equal to 10')
} else if (x==12){
  print('x equal to 10')
} else{
  print('x is not equal to 10 or 12')
}

#Even number
x <- 4
if(x%%2==0){
  print('Even Number')
}else{
  print('Not Even Number')
}

#matrix
x <- matrix()
y <- list()
if (y %>% is.matrix() == TRUE){
  print('Is a Matrix')
}else{
  print('Not a Matrix')
}

#vector
x <- c(2,1,3)
y <- 0

if (x[1] < x[2]){
  y <-x[1]
  x[1] <- x[2]
  x[2] <-y
  #print(x)
 }| if (x[2] < x[3]){
  y <-x[2]
  x[2] <- x[3]
  x[3] <-y
  print(x)
}| if (x[1] < x[2]){
  y <-x[1]
  x[1] <- x[2]
  x[2] <-y
  print(x)
}

x <- c(2,1,3)
if (x[1] > x[2] & x[1] > x[3] ) {
  first <-(x[1] )
} else if (x[2] > x[3] ) {
  print(x[2])
} else {
  print(x[3])
}

print(x)
print(x[1])

#for loop
mat <- matrix(1:25,nrow = 5)
nrow(mat) #5
1:5 #sequence from 1 to 5 #[1] 1 2 3 4 5
1:nrow(mat) #sequence from 1 to 5(nrowmat) #[1] 1 2 3 4 5
#like giving the length

#print individual values by column
for (num in mat){
  print(num)
}

#nested
for (row in 1:nrow(mat)){
  for (col in 1:ncol(mat)){
    print(paste('The element at row:',row, 'and col:', col, 'is:', mat[row,col]))
  }
}

for (row in 1:nrow(mat)){
    print(paste('row',row, 'element:', mat[row,2]))
}

for (row in 1:nrow(mat)){
  print(paste('row',row, 'element:', mat[row,5]))
}

for (coli in 1:ncol(mat)){
  print(paste('col',coli, 'element:', mat[1,coli]))
}

v <- "global var"
stuff <- "global stuff"

fun <- function(stuff){
  #v <- 2
  print(v)
  stuff <-  "now im a local stuff"
  print(stuff)
}

fun(stuff)
print(stuff)

hello <- function(name){
  return(paste('Hello',name))
}

print(hello('sam'))

#product
product <- function(a,b){
  result <- a*b
  return(result)
}
product(2,3)

#check value
num_check <- function(num,vec){
  if (num %in% vec) {
    return(TRUE)
  }else{
    return(FALSE)
  }
}
num_check(4,c(1,2,3))

#counter
num_count <- function(num,vec){
  count <- 0
  if (num %in% vec) {
    count <- length(which(num==vec))
    return(count)
  }
}
#num_count(2,c(1,1,2,2,2,3))
num_count(1,c(1,1,2,2,3,1,4,5,5,2,2,1,3))

#ship bars
bar_count <- function(ship){
  if(ship<5){
    return(ship)
  }else{
    count <- 0
    large <- round(ship/5,0)
    small <- ship%%5
    count <- large +small
    return(count)
  }
}
bar_count(3)

#sum
summer <- function(a,b,c){
  if(a%%3==0 & b%%3==0 & c%%3==0){
    sum <- 0
    return(sum)
  }else if(a%%3==0 & b%%3==0 ){
    sum <- c
    return(sum)
  }else if(a%%3==0 & c%%3==0 ){
    sum <- b
    return(sum)
  }else if(b%%3==0 & c%%3==0 ){
    sum <- a
    return(sum)
  }else if(a%%3==0 ){
    sum <- b+c
    return(sum)
  }else if(b%%3==0 ){
    sum <- a+c
    return(sum)
  }else if(c%%3==0 ){
    sum <- a+b
    return(sum)
  }else{
    sum <- a+b+c
    return(sum)
  }
}
summer(9,11,12) 


#find prime
prime_check <- function(num){
  flag = 0
  if(num > 1) {
    flag = 1
    for(i in 2:(num-1)) {
      if ((num %% i) == 0) {
        flag = 0
        break
      }
    }
  } 
  
  if(num == 2)    flag = 1
  if(flag == 1) {
    print(paste(num,"is a prime number"))
  } else {
    print(paste(num,"is not a prime number"))
  }
}
prime_check(237)  

sample(x =1:10,3)

addran <- function(x){
  ran <- sample(1:100,1)
  return(x+ran)
}

addran()#calling the function
addran #passing the function

#List apply
v <- c(1:5)
result <-lapply(v,addran) #list of every element of v applying function addran
class(result) #list

#simplify apply:return a vector
result2 <-sapply(v,addran) 
class(result2) #vector, integer

#========================
#anonymous functions
#========================
#anonymous functions are like lambda expressions on python
# when you dont need to expend a lot of space, not use function a lot and dont need to assign a name.
v <- 1:5
times2 <- function(num){
  return(num*2)
}
result <- sapply(v, times2)

#as anonymous func. get rid of name and return statement: 
#function(num){(num*2)}
result2 <- sapply(v, function(num){num*2})
print(result2)


#apply with multiple inputs
v <- 1:5
add_choice <- function(num,choice){
  return(num+choice)
}
print(add_choice(2,10)) #12 
sapply(v,add_choice,choice=10) # sapply(vector1, function, additional arguents(variables in function))

#Regular expressions

#grepl = return logical (T,F)
#grep  =  return index

#grepl('elementToFind', vectorToLookIn)
text <- 'Hi there, do you know wh you are voting for?'
grepl('voting',text) #True
grepl('do you',text) #True

v<- c('a','b','c','d','d')
grepl('b',v) #FALSE  TRUE FALSE FALSE FALSE
grep('d',v) #index position in which occur: 4 5 

#todays date
today <-Sys.Date() #2018-01-11 ===R format
class(today) #"Date"

c <- '1990-01-01'
class(c) #"character"
myDate <- as.Date(c)
class(myDate) #"Date"

as.Date("Nov-03-90",format= '%b-%d-%y') #[1] "1990-11-03" == R format
# %d Day of month (Decimal number)
# %m Month (Decimal number)
# %b Month (string abbreviated)
# %B Month (string full name)
# %y Year (2 digit)
# %Y Year (4 digit)

as.Date('June,01,2002', format= '%B,%d,%Y') #"2002-06-01" 
as.Date('1/5/2011', format='%m/%d/%Y') #"2011-01-05"


#dplyr (di-pla-yer)
library(dplyr)
library(nycflights13)
head(flights)
summary(flights)
names(flights)
View(flights)

# filter: subset rows in df
head(filter(flights, month==11, day==3,carrier=='AA'))
filter()

#slide: select rows by position
slice(flights, 1:10)

#arrange: order rows by var  
head(arrange(flights, year,month,day,desc(arr_time)))

#select: subset columns in df
head(select(flights, carrier,arr_time, month))

#rename: rename columns
head(rename(flights, airline_carrier = carrier)) #(dataset, newName =oldName)

#distinct: find unique values by columns
distinct(select(flights,carrier))
distinct(flights[,'carrier']) #same

#mutate: new column: product of other columns
head(mutate(flights, new_col = arr_delay*dep_delay))

#transmute: just new column back
head(transmute(flights, new_col = arr_delay*dep_delay))

#summarise: math function (aggregate function) to a colection of col, just onw row back
summarise(flights,avg_air_time =mean(air_time,na.rm = T)) #150.6865

#sample_n: return random n rows from df
sample_n(flights,size=10)

#sample_frac: return a % of Df
sample_frac(flights,size=0.1)


#pipe operator %>%
#Often, %>% is called multiple times to "chain" functions together,
# iris %>% head() %>% summary()
# Thus iris %>% head() %>% summary() is equivalent to summary(head(iris))

library(dplyr)
df <-mtcars
#nesting: difficult to read
result <- arrange (sample_n (filter (df, mpg>20),5), desc(mpg))
print(result)

#using multiple assignments: cleaner and clear to read, but store a bunch of var in memory
a <- filter (df, mpg>20)
b <- sample_n (a,5)
result <- arrange(b, desc(mpg))
print(result)

#using pipe operator %>% 
# Data %>% operation1 %>% op2 %>% opn
result <- df %>% filter(mpg>20) %>% sample_n(5) %>% arrange(desc(mpg))

# subset
cars <-(mtcars)
names(cars) #"mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear" "carb"
fitering <- filter(cars, mpg>20, cyl==6)

#reorder
reordering <- arrange(cars, cyl, desc(wt))

#select columns
selecting <- select(cars, mpg, hp)

#select distinct
disct1  <- distinct(cars, gear)

#new calculated column
head(mutate(cars, Performance = hp/wt))

#mean
summarise(cars,avg_mpg =mean(mpg,na.rm = T))

#Use pipe operators to get the mean hp value for cars with 6 cylinders.
cars %>% filter (cyl==6) %>% summarise(result =  mean(hp,na.rm = T))


library(tidyr) #tai-dier
library(data.table)

#gather
#gather columns into key value pairs(two new columns)
comp <- c(1,1,1,2,2,2,3,3,3)
yr <- c(1998,1999,2000,1998,1999,2000,1998,1999,2000)
q1 <- runif(9, min=0, max=100)
q2 <- runif(9, min=0, max=100)
q3 <- runif(9, min=0, max=100)
q4 <- runif(9, min=0, max=100)

df <- data.frame(comp=comp,year=yr,Qtr1 = q1,Qtr2 = q2,Qtr3 = q3,Qtr4 = q4)

#gathering quarters in a column(Quarter) y assign its values to a new col(Revenue)
gather(df,Quarter,Revenue, Qtr1:Qtr4)

# Using Pipe Operator
head(df %>% gather(Quarter,Revenue,Qtr1:Qtr4))

stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocks.gathered <- stocks %>% gather(stock,price, X:Z)

## spread = opposing to gather
stocks.gathered %>% spread(stock, price)
spread(stocks.gathered,stock,price)



# Separate: separate a column into others by regular expressions
df <- data.frame(newCol = c(NA, "a.x", "b.y", "c.z"))

x <- separate(data=df, col= newCol, into =c('col1', 'col2')) #for diffrent pattern, , sep = '-'
df %>% separate(newCol, c("ABC", "XYZ"))

# unite: opposito to separate, combine multiple columns into one
unite(x,colJoin, col1,col2) #sep="---"


#ggplot: layers
#=================
# data > Aesthetics > Geometries > Facets > Statistics > Coordinates > Theme >labels
library(ggplot2)

pl <- ggplot(data=mtcars, aes(x=mpg, y=hp)) # not graph yet
pl + geom_point() # type of graph

pl <- ggplot(data=mtcars, aes(x=mpg, y=hp)) + geom_point()
pl + facet_grid(cyl~.) # multiple plots on a canvas: separated by levels of the third var

pl <- ggplot(data=mtcars, aes(x=mpg, y=hp)) + geom_point() + facet_grid(cyl~.) 
pl + stat_smooth() #smooth

pl <- ggplot(data=mtcars, aes(x=mpg, y=hp)) + geom_point() + facet_grid(cyl~.) + stat_smooth()
pl2 <-pl + coord_cartesian(xlim=c(15,25)) #limits in axis

pl2 + theme_bw() #last details

#histograms: counts continuos data
library(ggplot2)
library(ggplot2movies) #dataset in ggplot
head(movies)
colnames(movies)
hist1 <- ggplot(movies, aes(x=rating)) +geom_histogram()
hist2 <- ggplot(movies, aes(x=rating)) +geom_histogram(binwidth = 0.1, aes(fill=..count..)) # degrade in color: fill based on count of var
hist2 <- ggplot(movies, aes(x=rating)) +geom_histogram(binwidth = 0.1, color='red', fill='pink', alpha=0.4) #alpha=transparence of fill
#  Labels
hist3 <- hist2 + xlab('Movie Rating') + ylab('Count') +ggtitle("Histogram Movie Rating") #labels
hist3 <- hist2 + xlab('Movie Rating') + ylab('Count') +ggtitle("Histogram Movie Rating") +
  theme(plot.title = element_text(hjust = 0.5))

#scatterplot
df <- mtcars
scat1 <- ggplot(df, aes(x=wt, y=mpg)) + geom_point()
scat2 <- ggplot(df, aes(x=wt, y=mpg)) + geom_point(size=3, alpha=0.5) # apha+ perceive overlaping points
scat3 <- ggplot(df, aes(x=wt, y=mpg)) + geom_point(aes(size=hp) ) # aes inside the geom= add more var, size by continue var.
scat4 <- ggplot(df, aes(x=wt, y=mpg)) + geom_point(aes(color=hp) ) #degrade color continues feauture
scat5 <- ggplot(df, aes(x=wt, y=mpg)) + geom_point(aes(color=hp) ) + scale_color_gradient(low='green',high='red') #choosing scale of colors
scat6 <- ggplot(df, aes(x=wt, y=mpg)) + geom_point(aes(shape=factor(cyl), color=factor(cyl)), size=1.5 ) #shape= for factors
scat7 <- ggplot(df, aes(x=wt, y=mpg)) + geom_point(color='blue', alpha=0.1) +geom_smooth(color='red') # add trend line:line of best fit 

#Barplots: count categorical data (factors, space between level)
df <- mpg
bar1 <-ggplot(df, aes(x=class)) + geom_bar()                                      
bar2 <-ggplot(df, aes(x=class)) + geom_bar(aes(fill=drv))  
bar3 <-ggplot(df, aes(x=class)) + geom_bar(aes(fill=drv), position = 'dodge') # every color indep bar by count
bar4 <-ggplot(df, aes(x=class)) + geom_bar(aes(fill=drv), position = 'fill') # every color indep bar by %

#Boxplots: num data, quartile info 
# Depit groups of data, whiskers indicate de variability outside the upper and lower quartiles (lower:q1, middle band:q2median, upper:q3)
# end of whiskers can be the max value or sd
# https://www.youtube.com/watch?v=CoVf1jLxgj4
# x axix: categorical var
df <- mtcars
box1 <- ggplot(df, aes(x=factor(cyl), y= mpg)) +geom_boxplot()
box2 <- ggplot(df, aes(x=factor(cyl), y= mpg)) +geom_boxplot() + coord_flip() #change coord. vertical
box3 <- ggplot(df, aes(x=factor(cyl), y= mpg)) +geom_boxplot(aes(fill=factor(cyl))) + theme_dark()

# 2D bencher: Heat map, variable plotting 
# color indicates high or low counts
library(ggplot2movies)
hmap1 <- ggplot(movies, aes(x=year, y=rating)) + geom_bin2d()
hmap2 <- ggplot(movies, aes(x=year, y=rating)) + geom_bin2d() + scale_fill_gradient(high='red', low='green') #customize colors
hmap3 <- ggplot(movies, aes(x=year, y=rating)) + geom_bin2d(binwidth= c(3,1)) + scale_fill_gradient(high='red', low='blue') #customize colors

hexmap1 <- ggplot(movies, aes(x=year, y=rating)) + geom_hex() #como colmena
hexmap2 <- ggplot(movies, aes(x=year, y=rating)) + geom_hex() + scale_fill_gradient(high='red', low='blue') #customize colors

# Density plot
dens1 <- ggplot(movies, aes(x=year, y=rating)) + geom_density2d()

# coordinates 
pl1 <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
pl2 <- pl1 + coord_cartesian(xlim = c(1,4), ylim = c(15,30)) # c(lower, upper limit)
pl3 <- pl2 + coord_fixed(ratio = 1/3) #ratio: y/x

#facets
pl1 <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
pl2 <- pl1 +facet_grid(. ~cyl) # Y:facetByonYAxis ~ X:facetByonXAxis
pl3 <- pl1 +facet_grid(drv ~.) # one dimesnional facet
pl4 <- pl1 +facet_grid(drv ~cyl) #two dimensional facets

# themes
theme_set(theme_bw()) # if want to pass same theme to different graphs
pl1 <-ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
pl2 <-ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() + theme_dark()

library(ggthemes)
pl1 <-ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +theme_economist()

#plot.ly: Interacty visualizations in R
library(ggplot2)
library(plotly)

pl <- ggplot(mtcars,aes(mpg,wt)) +geom_point()
interPl <- ggplotly(pl)
# https://plot.ly/ggplot2// #documentation examples


