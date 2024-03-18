## OVL: ------

veteran <- veteran %>% 
  filter(Survival_Days<800)

veteran %>% 
  filter(Treatment == "Test") %>% 
  summarise(mean=mean(Survival_Days))

veteran %>% 
  filter(Treatment == "Standard") %>% 
  dplyr::select(Survival_Days) -> standard

veteran %>% 
  filter(Treatment == "Test") %>% 
  dplyr::select(Survival_Days) -> test

standard$Survival_Days <- as.numeric(standard$Survival_Days)
test$Survival_Days <- as.numeric(test$Survival_Days)

set.seed(1)
rows <- sample(69)

standard_random = standard[rows,]

rows2 <- sample(66)

test_random = test[rows2,]

n = 2
d = 1.5
alpha = 0.05

  while((pf(d,df1=2*(n),df2=2*(n))-pf(1/d,df1=2*(n),df2=2*(n)))<(1-alpha)){
    n=n+1
  }
  
  R <- NULL
  lambda1 <- NULL
  lambda2<- NULL
  lower <- NULL
  upper <- NULL
  
    x <- standard_random[1:49]
    y <- test_random[1:49]
    
    lambda1 <- mean(x) - min(x)
    lambda2<- mean(y) - min(y)
    R <- lambda1 / lambda2
    lower <- R / d
    upper <- R * d

print(lower)
print(upper)
print(R)
print(lambda1)
print(lambda2)

outout=rbind(lambda1,lambda2,R,lower,upper)
colnames(outout) <-("Real Data Illustration")
View(outout)

test_random <- as.data.frame(y)
colnames(test_random) <- ("Survival in Days (Test Treatment)")

standard_random <- as.data.frame(x)
colnames(standard_random) <- ("Survival in Days (Standard Treatment)")

write.csv(veteran,"C:\\Users\\aschermaclean\\Desktop\\R Files\\Summer Research\\veteran_data.csv")
write.csv(standard_random, "C:\\Users\\aschermaclean\\Desktop\\R Files\\Summer Research\\standard_random.csv")
write.csv(test_random, "C:\\Users\\aschermaclean\\Desktop\\R Files\\Summer Research\\test_random.csv")
