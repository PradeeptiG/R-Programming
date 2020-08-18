#The dataset can be accessed using https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip

pollutantmean<-function(directory, pollutant, id)
{

df<-data.frame()
all_files<-list.files(directory, full.name = TRUE)

for(i in id)
 {
   df<-rbind(df, read.csv(all_files[i]))
 }

mean(df[ , pollutant] ,na.rm=TRUE)
}
################################################################################

complete<-function(directory, id=1:332)
{
	df<-data.frame("id"=numeric(0), "nobs"=numeric(0))
	
	all_files<-list.files(directory, full.name = TRUE)
	complete_file<-c()

	for(i in id)
 	{ 	n_obs<-0
   		file<-read.csv(all_files[i])
   		complete_file<-complete.cases(file)
   		for(j in 1:length(complete_file)) 
     			{if(complete_file[j]==TRUE)
       			{n_obs<-n_obs+1}
     			}
   
   		df<-rbind(df, data.frame(id=i, nobs=n_obs))  
	 }
	df
}

###############################################################################

corr<-function(directory, threshold=0)
{  
	all_files<-list.files(directory, full.name = TRUE)
 
   correlation<-numeric() 
   complete_obs=complete(directory, 1:332)
   for(i in 1:length(complete_obs$id))
   { 
      if(complete_obs[i, "nobs"]>threshold)
         {  file<-read.csv(all_files[i])
		new<-file[complete.cases(file), ]
		correlation<-c(correlation, cor(new$sulfate, new$nitrate))
         }
    }

   correlation

}
