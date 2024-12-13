customer<-read.csv(file.choose(),header=T)

nrow(customer)
ncol(customer)
colnames(customer)

##Dealing with NA values
f <- replace(customer,customer=='',NA)  ##replacing the black spaces with NA
is.na(f)->k    ##Checking the presence of NA values
k
colSums(k)  ## To determine No. of NA's in different columns

nona<-replace(customer,customer=='',"Unknown") ## since NA's are present in Profession column replacing them with unknown
nona


##for having the clear information about the dataset
library(dplyr)
rename(nona,"Score"="Spending.Score..1.100.")->k  ##jst to shotten the name and can be used when required
k
 
select(nona,c(Gender,CustomerID))   ## to have an idea about customer based on their id and gender

filter(nona,Gender=="Male")  ##To inspect only male customers
rename(k,"Income"="Annual.Income....")->m
m
filter(m,Income>=15000) #TO get the details about the annual salaries of customers about 15000

arrange(m,Income)
mutate(nona,family_excluding_customer=Family.Size-1)

summary(nona)   ## summary of dataset on related measures of mean,.....etc.,
summary(m$Income)

m%>%select(c(CustomerID,Gender,Income))%>%arrange(Income)
m%>%arrange(Profession)%>%select(c(Profession,CustomerID))->h
h
##graphs analization
colours()   ## To get an idea about the colours


#analysis over annual salary of the customers using histogram
hist(m$Income,main="Salary analysis",
     xlab="Salary",ylab="No. of persons",breaks=5,
     col="deeppink",border="black",density=100)

#analysis of the family of first 100 customers
plot(head(m$Family.Size,50),
     xlab="NO of Families",ylab="no. of members",
     main="Analysis of the family",type="o",col="plum")

#analysis over the working experience
plot(density(m$Work.Experience),
     main="Working Experience of customers",
     ylab="Density",col="violetred2",lwd=3)

#analysis of the ages ("The average age,highest age,lowest age")
boxplot(m$Age,main="Age Division",
        ylab="Age",col="skyblue",border="darkblue",
        varwidth=FALSE)

#To get an idea of professions of the customer
str(table)
table(m$Profession)->tables
names(tables)
colour=c("skyblue","navyblue","aquamarine","seagreen","plum","deeppink","lightgreen","violet","pink","grey")
pie_percentage<-round(100*tables/sum(tables),1)
pie_percentage
pie(tables,main="Professions of Customers",labels=pie_percentage,
    col=colour,radius=.9)
legend("topright",names(tables),fill=colour,cex=0.6)

library(plotrix) #for 3dpie
pie3D(tables,main="Professions of Customers",labels=pie_percentage,
      col=colour,radius=0.8)
legend("topright",names(tables),fill=colour,cex=0.32)

#Analysis based on gender of the customers
gender<-table(m$Gender)
barplot(gender,col="turquoise3",border="darkblue",
        density=80,
        main="Gender based analysis of customers")
#The Spending score of the individual customers
table(m$Score)->scores
plot(scores,col="palevioletred",
     ylab="No. of Individuals",xlab="Score",
     main="Examining of customer scores")

