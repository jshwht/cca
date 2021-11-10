rm(list = ls())

# Candisc is used to perform canonical correlation analysis:

library(candisc)

## IMPORT dtrust ## 

data_standardized <- data.frame(scale(dtrust[,2:11],center=TRUE,scale=TRUE))
  
attach(data_standardized)

cca <- cancor(cbind(satisf_economycntry,satisf_nationalgovernment,satisf_democracycntry)
              ~trust_cntryparliament+trust_legalsystem+trust_police+trust_politicians
              +trust_politicalparties+trust_EUparliament+trust_UN, data=data_standardized)
summary(cca)

# Loadings (correlations between canonical variates and original variables):

cca$structure$X.xscores
cca$structure$Y.yscores

redundancies<-redundancy(cca)
print(redundancies)

# Squared correlations between t and u for each of the 3 canonical pairs:

cor_squared <- cca$cancor^2

# For each Y canonical variate, the sum of the squared loading of each t on Y is taken
# and the sum is divided by the number of canonical variates (3) to give the proportion
# of variance in Y explained by t. This can be written as the variance of t over the 
# variance of Y:

vart_over_varY <- apply(cca$structure$Y.yscores^2,2,sum)
vart_over_varY = vart_over_varY/(dim(cca$structure$Y.yscores)[1])

# Creation of the redundancies table to see how much variance in Y is explained by U of the 
# corresponding canonical variate:

y_redundancies <- redundancies$Ycan.redun
redundancies_table <- cbind(cor_squared,vart_over_varY,y_redundancies,cumulative=cumsum(y_redundancies))
redundancies_table

# Viewing the summary of the canonical correlations to see significance of canonical variates at 
# 95% significance level:

summary(cca)

# Viewing the loadings for the first two canonical variates (the third is not significant). 
# Xcan1 is u1, Xcan2 is u2, Ycan1 is t1, Ycan2 is t2. 
# These tables show the loadings on the x-variates and y-variates by their respective 
# response categories. Interpretations can be made about what high loadings on u1, u2, 
# t1, and t2 represent.

cca$structure$X.xscores[,1:2]
cca$structure$Y.yscores[,1:2]

data_standardized<-cbind(dtrust[,1],data_standardized)
colnames(data_standardized)[colnames(data_standardized) == 'dtrust[, 1]'] <- 'Country'

library(ggplot2)

# Plots of scores on pairs of canonical variates

sweden_cv1 <- data.frame(cbind(cca$scores$X[data_standardized$Country=="Sweden",1],cca$scores$Y[data_standardized$Country=="Sweden",1]))
poland_cv1 <- data.frame(cbind(cca$scores$X[data_standardized$Country=="Poland",1],cca$scores$Y[data_standardized$Country=="Poland",1]))

sweden_cv2 <- data.frame(cbind(cca$scores$X[data_standardized$Country=="Sweden",2],cca$scores$Y[data_standardized$Country=="Sweden",2]))
poland_cv2 <- data.frame(cbind(cca$scores$X[data_standardized$Country=="Poland",2],cca$scores$Y[data_standardized$Country=="Poland",2]))

plot_cv1<-ggplot()+geom_point(data=sweden_cv1,aes(x=X1,y=X2,color="Sweden"))+
  geom_point(data=poland_cv1,aes(x=X1,y=X2,color="Poland"))+
  scale_color_manual(name="Country",values=c("Sweden"="red","Poland"="blue"))+
  xlab("u1")+
  ylab("t1")+
  ggtitle("Respondent scores on first pair of canonical variates by country")+
  theme_bw()
plot_cv1

plot_cv2<-ggplot()+geom_point(data=sweden_cv2,aes(x=X1,y=X2,color="Sweden"))+
  geom_point(data=poland_cv2,aes(x=X1,y=X2,color="Poland"))+
  scale_color_manual(name="Country",values=c("Sweden"="red","Poland"="blue"))+
  xlab("u2")+
  ylab("t2")+
  ggtitle("Respondent scores on second pair of canonical variates by country")+
  theme_bw()
plot_cv2

detach(data_standardized)
