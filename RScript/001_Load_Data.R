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
nrow(train)

# Check missing values
sapply(train, function(x) sum(is.na(x)))

# Summary
summary(train[,-1])

   # Balanced data set between ghost, ghoul and goblin


test$color = as.numeric(train$color)
test$color = as.numeric(test$color)

# Check normality for each numeric
plotNorm = function(data) {
  
  par(mfrow=c(1,2))
  # Histogram
  hist(data,main="Histogram",col="red",prob=T)
  lines(density(data),lwd=2)
  
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
   # No proof against normality at the 5% level

# Correlation
plot(train[,2:5])
cor(train[,2:5])
   # Max corr: hair_length, has_soul = 0.475


# Relation between type and explanatory variables

par(mfrow=c(2,2))
plot(train$type, train[,2], xlab = "Type", ylab = "Bone Length")
plot(train$type, train[,3], xlab = "Type", ylab = "Rotting Flesh")
plot(train$type, train[,4], xlab = "Type", ylab = "Hair Lenght")
plot(train$type, train[,5], xlab = "Type", ylab = "Has Soul")
par(mfrow=c(1,1))


# Relation between Color and Type
mytable <- table(train$color, train$type)
mytable

margin.table(mytable, 1)
margin.table(mytable, 2)

prop.table(mytable)

prop.table(mytable, 1)
   # 50% of blood-coloured creatures are Ghosts
prop.table(mytable, 2)
   # Same colour repartition for each type of creature





