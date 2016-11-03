#################
# 001_Load_Data #
#################

# Load each data file
dir = "C:/Users/jqu6215/Documents/Kaggle2016_Ghouls"
setwd(dir)

ptrain = paste(dir,"/Data/train.csv",sep="")
ptest = paste(dir,"/Data/test.csv",sep="")

train = read.csv2(ptrain, header=T,sep=",",dec=".")
test = read.csv2(ptest, header=T,sep=",",dec=".")

str(train)


# Check normality for each numeric
par(mfrow=c(1,2))

plotNorm = function(data) {
  
  par(mfrow=c(1,2))
  # Histogram
  hist(data,main="Histogram",col="red")
  
  # QQ plot
  qqnorm(data, main="Normal QQ-plot")
  qqline(data)
  
  par(mfrow=c(1,1))
  
  # Shapiro test
  print(shapiro.test(data))
  
}

num_var = c('bone_length','rotting_flesh','hair_length','has_soul')
for (i in num_var) {
  plotNorm(train[,i])
}



