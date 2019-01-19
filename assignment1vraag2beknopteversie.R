# Change Mean to 0 or 1 to create both results
rm(list = ls(all = TRUE))

set.seed(123)
S <- 100
n1 = c(10, 100, 200)
s2 = c(1, 2, 10)

### H1 ###

RESULT <- c()
  for (j in 1:3){
    for (k in 1:3){
      h1_result_Welch <- c()
      h1_result_Student <- c()
      for (i in 1:S){
      #Create groups
      h1_group_n1.1<- rnorm(n1[j], mean = 0, sd = sqrt(1))
      h1_group_n2.1 <- rnorm(200, mean = 0, sd = sqrt(s2[k]))
      
      #Welch test
      h1_ttest_Welch1 <- t.test(h1_group_n1.1, h1_group_n2.1, paired = FALSE, var.equal = FALSE)
      h1_result_Welch <- c(h1_result_Welch,h1_ttest_Welch1[3])
      
      #Student test
      h1_ttest_Student1 <- t.test(h1_group_n1.1, h1_group_n2.1, paired = FALSE, var.equal = TRUE)
      h1_result_Student <- c(h1_result_Student,h1_ttest_Student1[3])
    }
    
    h1_power_Welch <- sum(h1_result_Welch<0.05)/S
    h1_power_Student <- sum(h1_result_Student<0.05)/S
    
    res <- c(h1_power_Welch,h1_power_Student)
    RESULT <- rbind(RESULT,res)
    }
  }
RESULT

