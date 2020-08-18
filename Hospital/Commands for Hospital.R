plot_heart_attack<-function(){

outcome<-read.csv("outcome-of-care-measures.csv")
names(outcome)
dim(outcome)
hist(as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))

}

best<-function(state, outcome){

file<-read.csv("outcome-of-care-measures.csv")

if(!any(file$State==state))
{print("Invalid state")}
if(!(outcome=="heart attack"|outcome=="heart failure"|outcome=="pneumonia"))
{print("Invalid outcome")}

state_file<-file[which(file$State==state),]

if(outcome=="heart attack"){data<-state_file[, 11]}
if(outcome=="heart failure"){data<-state_file[, 17]}
if(outcome=="pneumonia"){data<-state_file[, 23]}

data<-as.numeric(data)
hosp_names<-data.frame(state_file$Hospital.Name, data)
complete_hosp<-hosp_names[complete.cases(hosp_names),]

mini<-min(complete_hosp$data)
best<-complete_hosp[which(complete_hosp$data==mini),]
best$state_file.Hospital.Name
}

rankhospital<- function(state, outcome, num){

file<-read.csv("outcome-of-care-measures.csv")

if(!any(file$State==state))
{print("Invalid state")}
if(!(outcome=="heart attack"|outcome=="heart failure"|outcome=="pneumonia"))
{print("Invalid outcome")}

state_file<-file[which(file$State==state),]

if(num=="best"){num<-1}


if(outcome=="heart attack"){data<-state_file[, 11]}
if(outcome=="heart failure"){data<-state_file[, 17]}
if(outcome=="pneumonia"){data<-state_file[, 23]}

data<-as.numeric(data)
hosp_names<-data.frame(state_file$Hospital.Name, data)
#hosp_names
complete_hosp<-hosp_names[complete.cases(hosp_names),]
#print(complete_hosp)
if(num=="worst"){num<-length(complete_hosp$state_file.Hospital.Name)}
if(num>length(complete_hosp$state_file.Hospital.Name)){print("NA")}
rank<-complete_hosp[order(complete_hosp$data, complete_hosp$state_file.Hospital.Name),]
#rank

rank_num=1:length(rank$data)
rank$rank_num=rank_num
#print(rank)
final<-rank[which(rank$rank_num==num),]
final$state_file.Hospital.Name
}

rankall<-function(outcome, num){   

df<-data.frame("hospital"=character(0), "state"=character(0))

file<-read.csv("outcome-of-care-measures.csv")
states<-unique(file$State)
states<-sort(states)

for(state in states)
{
hosp<-rankhospital(state, outcome, num)
df<-rbind(df, data.frame(hospital=hosp[1], state=state))
}

df

}

