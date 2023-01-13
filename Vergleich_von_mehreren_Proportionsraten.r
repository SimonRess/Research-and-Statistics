data = data.frame(group = c(1,1,0,1,0,1,0,0,0,1,1,1,0,0),
                  sit1 =  c(1,0,0,1,1,1,0,0,1,1,0,1,1,0),
                  sit2 =  c(0,1,1,1,0,0,1,1,0,0,0,0,1,1),
                  cov1 =  c(1,2,3,1,0,0,1,1,0,2,3,1,1,3),
                  cov2 =  c(0,1,0,0,1,0,2,0,2,0,2,3,1,0))


#Single Models
  model = glm(cbind(sit1,sit2) ~ group, cov1, cov2, data = data, family = binomial())
  summary(model)
  
  model1 = glm(sit1 ~ group + cov1 + cov2, data = data, family = binomial())
  summary(model1)


# One Model

  #transfrom data set
    library(tidyr)
    
    data1 = gather(data, sit, measurement, sit1:sit2)
    data1$sitnew = NA
    data1$sitnew =ifelse(data1$sit=="sit1" & data1$measurement==1 , "sit1", data1$sitnew)
    data1$sitnew =ifelse(data1$sit=="sit2" & data1$measurement==1 , "sit2", data1$sitnew)
    data1 = data1[!is.na(data1$sitnew),]

  #build model
    model = glm(group ~ sitnew + sitnew:group + cov1 + cov2, data = data1, family = binomial())
    summary(model)