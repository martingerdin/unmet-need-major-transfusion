rm(list = ls()) # clear work space
# getwd()
require(data.table)
source("./makeTable1.R")
## read the raw data, data.table = FALSE returns dataframe
titco <- fread("../data/titco-2016-03-03-080722.csv",
               stringsAsFactors = FALSE,
               data.table = FALSE)
## create a vector with variables to be kept
keep_columns <- c("hos",
                  "pid",
                  "ti",
                  "sbp_1",
                  "hr_1",
                  "ubr_1",
                  "ubr_1_24",
                  "age",
                  "moi",
                  "sex",
                  "fast")
library(dplyr)
library(tidyr)
## in addition of variables in keep_columns, select and keep variables from
## titco that start with "fast_", "op_" and "ct_"
titco <- select(titco,
                one_of(keep_columns),
                starts_with("fast_"),
                starts_with("op_"),
                starts_with("ct_"))
nrow(titco) # 16047
## return rows where age>=15
titco <- filter(titco, age >= 15)
nrow(titco) # 13258
## return rows where there are no NAs in the following variables
titco <- filter(titco,
                !is.na(sbp_1),
                !is.na(hr_1),
                !is.na(ti),
                !is.na(moi),
                !is.na(fast),
                !is.na(ubr_1),
                !is.na(ubr_1_24))
nrow(titco) # 10810
## omit all burn patients using grepl that returns the pattern "Burn" in the
## character vector "moi" where matches are sought
titco <- filter(titco, !grepl("Burn", moi))
nrow(titco) # 10372
## define empty strings as NA in case there are any, brackets indicate positions
## in the matrix. square brackets are used to indicate only a certain part of a
## matrix
titco[titco == ""] <- NA
## put all the fast-findings together, ie concatenate. The 0 removes the space
fast_findings <- paste0("fast_", 1:11)
## create a vector with all of the fast-columns
fast_columns <- titco[, fast_findings]
## make your own function from is.na() and all() that checks for rows with all NA's
all_na <- function(x) {
  all(is.na(x))
}
## L is a vector that returns TRUE for all rows where the function all_na is
## true, ie where fast_1 to fast_11 are all missing. Can be used for filtering
## the data from observations where fast is completely missing. 1 indicates rows
L <- apply(fast_columns, 1, all_na)

#------create a vector for our icd-codes
icds <- c("s26.0",
          "s35.2",
          "s36.0",
          "s36.1",
          "s36.8")
## create a vector with all icd-columns in titco
icd_columns <- select(titco, ends_with("_icd"))
#------ any_match returns TRUE for rows where any of icds is present
any_match <- function(y, x = icds){
  any(x %in% y)
}
## M is a vector that returns TRUE for all rows where the any_match function is
## true, ie rows with any of our icds
M <- apply(icd_columns, 1, any_match)
## filter all NA
titco2 <- filter(titco, !L | M | fast == "Yes")
nrow(titco2) # 6318
titco2 <- select(titco2, -ends_with("_icd"))
## combine all ct and op columns into one, separate with " -"
ct_op_cols <- sort(unlist(lapply(c("ct", "op"),
                                 function(x) {
                                     grep(x, colnames(titco2))
                                 })))
ct_op <- apply(titco2[, ct_op_cols], 1, paste, collapse = " -")
library(stringr)
ct_op <- str_replace_all(ct_op, "NA -", "") %>%str_replace(" -NA", "")
## combine all fast findings into one column
fast_cols <- grep("fast_", colnames(titco2))
fast_strings <- apply(titco2[, fast_cols], 1, paste, collapse = " -")
fast_strings <- str_replace_all(fast_strings, "NA -", "") %>%str_replace(" -NA", "")
## create data frame to be used for coding the fast variable
fast_data <- select(titco2, hos, pid)
fast_data$ct_op <- ct_op
fast_data$fast_findings <- fast_strings
fast_data$fast <- ""
write.csv(fast_data, file = "fast_data.csv")
## combine the fast coded dataframe with titco2. read_delim reads a table and
## creates a data frame from it.
library(readr)
fast_data2 <- read_delim("../data/fast_data2.csv", 
                         ";",
                         escape_double = FALSE,
                         trim_ws = TRUE)
## to combine titco2 and fast_data2 we first need to add the ct_op and
## fast_strings vectors to titco2
titco2$ct_op <- ct_op
titco2$fast_findings <- fast_strings
## then we need to rename the fast variable in fast_data2 to avoid confusing it
## with the fast variable in titco2
colnames(fast_data2)[colnames(fast_data2) == "fast"] <- "fastpos"
## then we use merge
titco2 <- merge(x = titco2,
                y = fast_data2,
                all.x = TRUE,
                by = c("hos", "pid", "ct_op", "fast_findings"))
nrow(titco2)
## now we need to set fast_pos to 0 if fast is 1 but there were no findings
titco2[with(titco2, fast == "Yes" & is.na(fastpos)), "fastpos"] <- 0
## only keep variables with necessary information for table1, abc-scores and
## outcome calculation
titco2 <- select(titco2,
                 -starts_with("fast_"),
                 -starts_with("op_"),
                 -starts_with("ct_"))
titco2 <- titco2[, -grep("0", colnames(titco2))]
## create a variable list which we want in table1
char_vars <- c("age", "sex", "moi", "sbp_1", "hr_1", "ti", "fastpos")
## define categorical variables
catVars <- c("sex", "ti", "moi", "fastpos")
contVars <- c("age", "sbp_1", "hr_1")
## create table1 how can I add median, 25th and 75th percentile as well as min
## and max for the continuous variables? there seems to be some error here as
## well, I tried to copy Mike's code
require(tableone)
table1 <- makeTable1(variables = char_vars,
                     data = titco2,
                     quan.stats = c("median", "iqr", "range"),
                     quan.digits = 2)
#create a table that can be exported to Word
require(xtable)
write(print(xtable(table1), type = "html"), file = "table1.html")

# calculating abc-score
# arrival systolic blood pressure = sbp_1, injury mechanism = ti, arrival heart rate = hr_1, FAST = fast.
# Penetrating mechanism (0=no, 1=yes) - ti = Penetrating
# Arrival Systolic Blood Pressure <= 90 mm Hg (0=no, 1=yes) - sbp_1
# Arrival Heart Rate >= 120 bpm (0=no, 1=yes) - hr_1
# Positive focused assessment sonography for trauma (FAST) (0=no, 1 or 2 =yes)

titco2$score = 
  as.integer(titco2$ti == "Penetrating") +
  as.integer(titco2$sbp_1 <= 90) + 
  as.integer(titco2$hr_1 >= 120) +
  as.integer(titco2$fastpos != 0)
titco2$mt_needed = as.integer(titco2$score >= 2)

#sum of all transfusions in the first hour and 1-24 hours by row in a new variable "tot_transfusion"
titco2$tot_transfusion = (titco2$ubr_1 + titco2$ubr_1_24)
#number of patients who received MT in our study sample is 5?
sum(titco2$tot_transfusion >= 10)
#number of patients predicted to need MT is 494?
sum(titco2$mt_needed == 1)
#proportion a and b
prop_a <- (sum(titco2$mt_needed == 1) / nrow(titco2)); prop_a
prop_b <- (sum(titco2$tot_transfusion >= 10) / nrow(titco2)); prop_b
#primary outcome
outcome <- (prop_a - prop_b)
#sensitivity analysis with MT defined as >= 4 units of PRBC in 24 hours, instead of 10
outcome2 <- (prop_a - sum(titco2$tot_transfusion >= 4) / nrow(titco2))
#sensitivity analysis restricting the abc-score calculation to true FAST-positives, ie no ct or op surrogates used
titco2$score2 = 
  as.integer(titco2$ti == "Penetrating") +
  as.integer(titco2$sbp_1 <= 90) + 
  as.integer(titco2$hr_1 >= 120) +
  as.integer(titco2$fastpos == 1)
titco2$mt_needed2 = as.integer(titco2$score2 >= 2)
prop_a2 <- (sum(titco2$score2 >= 2) / nrow(titco2))
prop_b2 <- (sum(titco2$tot_transfusion >= 10) / nrow(titco2))
outcome3 <- (prop_a2 - prop_b2)
## mcnemars test main
mcnemar.test(as.factor(titco2$mt_needed), as.factor(titco2$tot_transfusion >= 10))
## mcnemars test sensitivity analysis only fast findings
mcnemar.test(as.factor(titco2$mt_needed2), as.factor(titco2$tot_transfusion >= 10))
## mcnemars test sensitivity analysis mt limit 4 units
mcnemar.test(as.factor(titco2$mt_needed), as.factor(titco2$tot_transfusion >= 4))
## find patient with mt_needed 0 but who received mt
titco2[titco2$mt_needed == 0 & titco2$tot_transfusion >= 10, c("sbp_1", "hr_1", "fast", "fastpos", "score")]
colnames(titco2)
## the code below performs an exact mcnemars test and report or with 95% ci
require(exact2x2)
exact2x2(x = as.factor(titco2$mt_needed),
         y = as.factor(titco2$tot_transfusion >= 10),
         paired = TRUE,
         conf.int = TRUE,
         conf.level = 0.95)
## calculate unmet neet and associated 95% ci
titco2$mt <- ifelse(titco2$tot_transfusion >= 10, 1, 0)
titco2$mt2 <- ifelse(titco2$tot_transfusion >= 4, 1, 0)
calc.un <- function(mt, mt_needed) {
    un <- 1 - sum(mt == 1)/sum(mt_needed == 1)
    return (un)
}
analyses <- list(main = "mt, mt_needed",
                 sens1 = "mt2, mt_needed",
                 sens2 = "mt, mt_needed2")
set.seed(428)
estimates <- lapply(analyses, function(a) {
    calc <- paste0("calc.un(", a, ")")
    un_pe <- with(titco2, eval(parse(text = calc)))
    bs <- lapply(1:1000, function(x)
        titco2[sample(1:nrow(titco2), nrow(titco2), replace = TRUE), ])
    un_bs <- unlist(lapply(bs, function(x)
        with(x, eval(parse(text = calc)))))
    un_diff <- un_bs - un_pe
    qs <- un_pe - quantile(un_diff, c(0.025, 0.975))
    est <- sprintf("%.1f", c(un_pe, min(qs), max(qs)) * 100)
    return(paste0(est[1], " (95% CI ", est[2], "-", est[3], ")"))
})
estimates
