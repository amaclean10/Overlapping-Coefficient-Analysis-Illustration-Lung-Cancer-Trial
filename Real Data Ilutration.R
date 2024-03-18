## Data Wrangling: -------

veteran <- read.table("~/Downloads/veteran.txt", quote="\"")

colnames(veteran) <- c("Treatment", 'Celltype', 'Survival_Days', 'Status', 'Karnofsky_Score', 'Months_From_Diagnosis', 'Age', 'Prior_Therapy')

veteran$Treatment <- as.factor(veteran$Treatment)
levels(veteran$Treatment) <- c("Standard", "Test")

table(veteran$Treatment)
median(veteran$Survival_Days)

veteran$Celltype <- as.factor(veteran$Celltype)
levels(veteran$Celltype) <- c("Squamous", "Smallcell", "Adeno", "Large")

veteran$Treatment <- as.factor(veteran$Treatment)
levels(veteran$Treatment) <- c("Standard", "Test")


veteran$Status <- as.factor(veteran$Status)
levels(veteran$Status) <- c("Dead", "Censored")


veteran$Prior_Therapy <- as.factor(veteran$Prior_Therapy)
levels(veteran$Prior_Therapy) <- c("No", "Yes")

## Exponential Verification: -------
hist(veteran$Survival_Days)
qqnorm(veteran$Survival_Days)

install.packages("moments")
library(moments)
skew <- skewness(veteran$Survival_Days)
skew
kurt <- kurtosis(veteran$Survival_Days)
kurt

##KS TEST: -----
library(MASS)

veteran %>% 
  filter(Treatment=="Standard") %>% 
  select(Survival_Days) -> standard_subset

veteran %>% 
  filter(Treatment=="Test") %>% 
  select(Survival_Days) -> test_subset

fit1=fitdistr(standard_subset$Survival_Days, "exponential")
fit2=fitdistr(test_subset$Survival_Days, "exponential")

ks.test(standard_subset, "pexp", fit1$estimate)
ks.test(test_subset, "pexp", fit2$estimate)

summary(standard_subset)
summary(test_subset)

test_no_outliers <- test_subset %>% 
  filter(Survival_Days<800)

boxplot(test_no_outliers$Survival_Days)

fit3=fitdistr(test_no_outliers$Survival_Days, "exponential")
ks.test(test_no_outliers, "pexp", fit3$estimate)


# Not normal


## OVL: ------

veteran <- veteran %>% 
  filter(Survival_Days<800)

View(veteran)

table(veteran$Treatment)


veteran %>% 
  filter(Treatment == "Test") %>% 
  summarise(mean=mean(Survival_Days))


ggplot(veteran, aes(x = Survival_Days))+
  geom_density(aes(fill=Treatment), alpha=0.5)

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
