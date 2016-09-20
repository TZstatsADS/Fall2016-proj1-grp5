#combine data
filename<-"/Users/yifeihu/Documents/semester_3/applied_data_science/csv_pus(1)/ss14pusa.csv"
data_a<-read.csv(file=filename,header=TRUE)
filename<-"/Users/yifeihu/Documents/semester_3/applied_data_science/csv_pus(1)/ss14pusb.csv"
data_b<-read.csv(file=filename,header=TRUE)
data_all<-rbind(data_a,data_b)
save(data_all,file="/Users/yifeihu/Documents/semester_3/applied_data_science/csv_pus(1)/all.Rdata")

#choose the variables and the allocation flag associated with them
data_a1<-data_all[,c("PWGTP","SEMP","COW","SCHL","FOD1P","RAC1P","DIS","SEX","NATIVITY","CIT","FCOWP","FDISP","FCITP","FFODP","FRACP","FSCHLP","FSEMP","FSEXP")]
save(data_a1,file="/Users/yifeihu/Documents/semester_3/applied_data_science/csv_pus(1)/data_a1.Rdata")

#change the sequence of the variables to make it more convenient for the for loop
data_a2 <-data_a1[,c("PWGTP","SEMP","COW","SCHL","FOD1P","RAC1P","DIS","SEX","CIT","NATIVITY","FSEMP","FCOWP","FSCHLP","FFODP","FRACP","FDISP","FSEXP","FCITP")]

#fill in the missing by using the code reck method of the data without allocation flag
for(j in 2:9)
{
  if_not_NA<-!is.na(data_a2[,c(j)])
  if_flag_c<-data_a2[,c(j+9)]==0
  sample_c<-which(if_not_NA*if_flag_c==1)
  NA_c<-which(is.na(data_a2[,c(j)]))
  data_a2[,c(j)][NA_c]<-sample(rep(data_a2[,c(j)][sample_c],data_a2$PWGTP[sample_c]),length(data_a2[,c(j)][NA_c]),replace=TRUE)
}

save(data_a2,file="/Users/yifeihu/Documents/semester_3/applied_data_science/csv_pus(1)/data_a2.Rdata")

#choose the observations that are self-employment
data_a3<-data_a2[which(data_a2$COW==6 | data_a2$COW==7),]

#choose the variables to use
data_a3<-data_a3[,c("PWGTP","SEMP","COW","SCHL","FOD1P","RAC1P","DIS","SEX","CIT","NATIVITY")]
save(data_a3,file="/Users/yifeihu/Documents/semester_3/applied_data_science/csv_pus(1)/data_a3.Rdata")



#recode the classification of field of degree
#code the field of degree of mathmatics, computer science, and Finace as 1
#code other field of degree as 0
#the reason is that in the ads class, most of as are from one of the kind of degree in 1
for (i in c(2100:2107, 3700:3702,4005))
{
  #cat(i,'\n')
  data_a3$FOD1P[which(data_a3$FOD1P== i)]<-1  
}
data_a3$FOD1P[-which(data_a3$FOD1P == 1)] <- 0
#table(data3$FOD1P)
save(data_a3,file="/Users/yifeihu/Documents/semester_3/applied_data_science/csv_pus(1)/data_a3.Rdata")

#recode the classification of RACE 
#to code white alone as majority 1 and others as minority 0
data_a3$RAC1P[-which(data_a3$RAC1P==1)]<-0
  
#recode the classification of SCHL 
#to code Bachelor's degree as 31, master's degree as 32,doctorate degree as 33, and others as 30
  data_a3$SCHL[which(data_a3$SCHL==21)]<-31
  data_a3$SCHL[which(data_a3$SCHL==22)]<-32
  data_a3$SCHL[which(data_a3$SCHL==24)]<-33
  data_a3$SCHL[-(which(data_a3$SCHL==31 | data_a3$SCHL==32 | data_a3$SCHL==33))]<-30

#save the data after recode 
data_a4<-data_a3[,c("PWGTP","SEMP","COW","SCHL","FOD1P","RAC1P","DIS","SEX","NATIVITY")]
save(data_a4,file="/Users/yifeihu/Documents/semester_3/applied_data_science/csv_pus(1)/data_a4.Rdata")

#prepare for linear regression 
data_a5<-data_a4[,c("PWGTP","SEMP","SCHL","FOD1P","RAC1P","DIS","SEX","NATIVITY")]

#to make the category variable as only 1 and 0 category
new <- array()
for ( i in c(30:33))
{
  Y <- array(0, dim = c(length(data_a5$SCHL)))
  Y[which(data_a5$SCHL==i)] <- 1
  new <- cbind(new, Y)
}
new <- as.data.frame(new[,-1])
names <- c(0:3)
for (i in 0:3){
  names[i+1] <- c(paste("SCHL.i",i, sep = ""))
}
colnames(new) <- names
data_r<-cbind(data_a5,new)

data_r$DIS.i<-data_r$DIS-1
data_r$SEX.i<-data_r$SEX-1
data_r$NATIVITY.i<-data_r$NATIVITY-1
data_r$FOD1P.I<-data_r$FOD1P
data_r$RAC1P.I<-data_r$RAC1P
save(data_r,file="/Users/yifeihu/Documents/semester_3/applied_data_science/csv_pus(1)/data_r.Rdata")

#factor the variables
for(j in 9:17)
{
  data_r[,c(j)]<-factor(data_r[,c(j)])
}
  data_afterf<-data_r
save(data_afterf,file="/Users/yifeihu/Documents/semester_3/applied_data_science/csv_pus(1)/data_afterf.Rdata")

#do linear regression
m<-lm(SEMP~0+SCHL.i0+SCHL.i1+SCHL.i2+DIS.i+SEX.i+NATIVITY.i+FOD1P.I+RAC1P.I,weight=PWGTP,data=data_afterf)
summary(m)

#use SBC as the criterion to choose model
step(m,k=log(length(data_afterf)))

