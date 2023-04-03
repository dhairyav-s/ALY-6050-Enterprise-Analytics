## ALY6050 Module 4 Project A Prescriptive Model for Strategic Decision-making, An Inventory Management Decision Model
## By - Dhairyav Jatin Shah
## Due Date - 3/23/2023

# Importing Libraries 
library("plyr")
library("dplyr")

# Part 1

# Given Data
AD <- 16500                                     ## Annual Demand
CPU <- 79                                       ## Cost per Unit
OC <- 200                                       ## Order Cost
HCP <- 0.125                                    ## Holding Cost Percentage
HC_PU <- round(CPU*HCP, digits = 2)             ## Total holding cost per unit

# Computing the reorder point 
RP <- round(sqrt((2*AD*OC)/HC_PU),digits = 0)   ## Reorder Point
RP

RP_C <- round(RP*1.9, digits = 0)               ## Reorder point as per company policy
RP_C

# Computing Annual Ordering Cost
AOC <-  AD*OC/RP_C                              ## Annual ordering cost to the company
AOC

# Computing Annual Holding Cost
AHC <- (RP*HC_PU)/2                             ## Annual Holding Cost to the company 
AHC

# Computing Total Inventory Cost
TIC <- AOC+AHC                                  ## Total inventory cost to the company 
TIC

# Part 2 

# Given values for the triangular distribution 
a = 12000
b = 21000
c = 19000
K = (c - a) / (b - a)
M = (b - a) * (c - a)
N = (b - a) * (b - c)


# Computing stimulation for annual demand
set.seed(124)
Sim = round(runif(5000), 2)
Sim <- as.data.frame(Sim)
colnames(Sim) <- "Random_Values"

# Computing triangular probability distribution using the above data frame 
Sim$Tri_Dist <- if_else(Sim$Random_Values <= K, 
                           a + sqrt(Sim$Random_Values*M),
                           b - sqrt((1-Sim$Random_Values)*N))


# Computing the required parameters using the data given above

ADT <- round(Sim$Tri_Dist,digits = 0)            ## Annual Demand for Triangular Distribution

# Computing the reorder point 
RPT <- round(sqrt((2*ADT*OC)/HC_PU),digits = 0)  ## Reorder Point for Triangular Distribution

RP_CT <- round(RP*1.9, digits = 0)               ## Reorder point as per company policy for Triangular Distribution

# Computing Annual Ordering Cost for Triangular Distribution
AOCT <-  ADT*OC/RP_C                             ## Annual ordering cost to the company for Triangular Distribution

# Computing Annual Holding Cost for Triangular Distribution
AHCT <- (RP*HC_PU)/2                             ## Annual Holding Cost to the company for Triangular Distribution

# Computing Total Inventory Cost for Triangular Distribution
TICT <- AOCT + AHCT                              ## Total inventory cost to the company for Triangular Distribution

# Part 2 (i)

alpha = 0.05 ## Confidence Interval of 95%
df = 5000-1 ## Degree of Freedom
T_score <- qt(p=alpha/2,df=df,lower.tail = F)    ## Calculating the T-score using qt function

# Estimating the minimum total cost with alpha = 0.05

TICT.m <- mean(TICT)                             ## Mean of the Total Inventory Cost for Triangular Distribution
TICT.sd <- sd(TICT)                              ## Std Dev of the Total Inventory Cost for Triangular Distribution
TICT.se <- TICT.sd/sqrt(5000)                    ## Standard error of mean of the Total Inventory Cost for Triangular Distribution
M_error <- T_score*TICT.se                       ## Computing the mean error as a product of standard mean error and t-score
L_boundry <- round(TICT.m - M_error, digits = 2) ## Computing the lower limit
U_boundry <- round(TICT.m + M_error, digits = 2) ## Computing the upper limit
paste0("The estimate minimum total cost fall in the range between :(",L_boundry,",",U_boundry,")" )

hist(TICT, 
     xlab = "Total Inventory Cost", 
     ylab = "Frequency", 
     main = "Distribution of Total Inventory Cost",
     col = cm.colors(12))


# Checking if distribution is normal distribution

Exp_TIC <- rnorm(5000, mean = TICT , sd = TICT)    
chisq.test(TICT,Exp_TIC)

# Estimating the expected order quantity with alpha = 0.05

RPT.m <- mean(RPT)                                ## Mean of the expected order quantity for Triangular Distribution
RPT.sd <- sd(RPT)                                 ## Std Dev of the expected order quantity for Triangular Distribution
RPT.se <- RPT.sd/sqrt(5000)                       ## Standard error of mean of the expected order quantity for Triangular Distribution
M_error1 <- T_score*RPT.se                        ## Computing the mean error as a product of standard mean error and t-score
L_boundry1 <- round(RPT.m - M_error1, digits = 2) ## Computing the lower limit
U_boundry1 <- round(RPT.m + M_error1, digits = 2) ## Computing the upper limit
paste0("The estimate expected order quantity fall in the range between :(",L_boundry1,",",U_boundry1,")" )

hist(RPT, 
     xlab = "Order Qunantity", 
     ylab = "Frequency", 
     main = "Distribution of Order Qunantity",
     col = cm.colors(12))

# Checking if distribution is normal distribution

Exp_RPT <- rnorm(5000, mean = RPT , sd = RPT)    
chisq.test(RPT,Exp_RPT)

# Estimating the expected annual number of orders with alpha = 0.05

# Computing the Annual number of orders
ANO <- ADT/RP_CT                                  ## Annual number of orders for triangular distribution

ANO.m <- mean(ANO)                                ## Mean of the Annual number of orders for Triangular Distribution
ANO.sd <- sd(ANO)                                 ## Std Dev of the Annual number of orders for Triangular Distribution
ANO.se <- ANO.sd/sqrt(5000)                       ## Standard error of mean of the Annual number of orders for Triangular Distribution
M_error2 <- T_score*ANO.se                        ## Computing the mean error as a product of standard mean error and t-score
L_boundry2 <- round(ANO.m - M_error2, digits = 2) ## Computing the lower limit
U_boundry2 <- round(ANO.m + M_error2, digits = 2) ## Computing the upper limit
paste0("The estimate annual number of orders fall in the range between :(",L_boundry2,",",U_boundry2,")" )

hist(ANO, 
     xlab = "Annual Number of Orders", 
     ylab = "Frequency", 
     main = "Distribution of Annual Number of Orders",
     col = cm.colors(12))

# Checking if distribution is normal distribution

Exp_ANO <- rnorm(5000, mean = ANO , sd = ANO)    
chisq.test(ANO,Exp_ANO)

