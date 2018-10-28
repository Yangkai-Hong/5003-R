
library(tidyr)
library(dplyr)
library(forcats)


qs <- read.csv("combined_cleaned.csv", header = F, stringsAsFactors = F, row.names = "V1")
qs <- qs[, 1:71]

questions <- row.names(qs)
qname <- paste0("Q", 1:70)

qref <- data.frame(Questions = questions, row.names = qname, stringsAsFactors = F)

# qref["Q60", 1]

qs.t <- as.data.frame(t(qs), stringsAsFactors = F)
colnames(qs.t) <- qname

### try to find which variables can be convert to numeric values, the idea is if the numeric value in the answer
### have more than 50% than it is likely to convert to numeric

y <- vector()
l <- vector("list", length = length(qref$Questions))
# l <- data.frame()
for (i in seq_along(qref$Questions)) {
  #qref[i,1]
  y[i] <- suppressWarnings(mean(is.na(as.numeric(qs.t[,i]))))
  if (y[i] < 0.5) {
    l[i] <- data.frame(NAV = suppressWarnings(qs.t[is.na(as.numeric(qs.t[,i])), i]))
  }
}


## check the result
qname[y < 0.5 & y > 0]
l[y < 0.5 & y > 0]


## change some obvious error in the data, or change characters to numbers
qs.t[is.na(qs.t$Q14), "Q14"] <- "No Answer"
qs.t[, "Q17"] <- fct_recode(qs.t[, "Q17"], "40000" = "4000D",
                            "100000" = "prefer not to say"
) %>% as.character()
qs.t[is.na(qs.t$Q17), "Q17"] <- "100000"
qs.t[is.na(qs.t$Q26), "Q26"] <- "No"
qs.t[is.na(qs.t$Q60), "Q60"] <- "N/A"

qs.t[is.na(qs.t$Q23), "Q23"] <- "N/A"
qs.t[qs.t$Q24 == "Master of Data Science ", "Q24"] <- "Master of Data Science"
qs.t[qs.t$Q51 == "N ", "Q51"] <- "N"
qs.t[qs.t$Q51 == "Y ", "Q51"] <- "Y"

##Q12 has miss interpretted by students due to monthly earn, so need to convert yearly earn to monthly earn

qs.t$Q12 <- fct_recode(as.character(qs.t[, "Q12"]), "1600" = "1600",
                       "4000" = "4000",
                       "1000" = "1000",
                       "0" = "0",
                       "25000" = "25000",
                       "5000" = "5000",
                       "4750" = "4750",
                       "6700" = "6700",
                       "30000" = "30000",
                       "6667" = "80000",
                       "8334" = "100000",
                       "7500" = "90000",
                       "12500" = "12500",
                       "1500" = "1500",
                       "6250" = "75000",
                       "2000" = "2000",
                       "10000" = "10000",
                       "4167" = "50000",
                       "3000" = "3000",
                       "6000" = "6000",
                       "1750" = "1750",
                       "5500" = "5500",
                       "13000" = "13000",
                       "7000" = "7000",
                       "15000" = "15000",
                       "12100" = "12100",
                       "2300" = "2300",
                       "17084" = "205000",
                       "15000" = "180000",
                       "11657" = "11657.16",
                       "20000" = "20000",
                       "5000" = "60000",
                       "12000" = "12000",
                       "7084" = "85000"
) %>% as.character() %>% as.numeric() 

# fix Q28 & Q29 & Q36
qs.t$Q28 <- as.numeric(qs.t$Q28)
qs.t$Q29 <- as.numeric(qs.t$Q29)
qs.t[is.na(qs.t$Q28), "Q28"] <- mean(qs.t$Q28, na.rm = T)
qs.t[is.na(qs.t$Q29), "Q29"] <- mean(qs.t$Q29, na.rm = T)
qs.t[, "Q36"] <- ifelse(qs.t$Q36 == "Y", 1, 0)


y <- vector()
for (i in seq_along(qref$Questions)) {
  #qref[i,1]
  y[i] <- suppressWarnings(mean(is.na(qs.t[,i])))
}

## columns to remove, if they have NULL values and it will be not appropriate for our analysis

## check these question names
qref[qname[y != 0],]

## remove columns with NA answers
qs.data <- qs.t[, qname[y == 0]]



## remove categorical variables with too many factor levels, the criteria is if it > 6 then it will
## be meaningless here so we remove it from our data

y <- vector()
for (i in 1:dim(qs.data)[2]) {
  #qref[i,1]
  y[i] <- suppressWarnings(mean(is.na(as.numeric(qs.data[,i]))))
}

for (i in 1:dim(qs.data)[2]) {
  if (y[i] == 0) {
    qs.data[, i] <- as.numeric(qs.data[, i])
  } else {
    qs.data[, i] <- as.factor(qs.data[, i])
  }
}

xx <- sapply(qs.data, nlevels)

qs.data <- qs.data[, !(xx > 6)]

qs.data$Q17 <- ifelse(qs.data$Q17 >= 200000, 1, 0)

str(qs.data)

View(qs.data)
