# data processing for FIG results PDFs

library(tidyverse)
library(tabulizer)

location <- "https://www.gymnasticsresults.com/worlds/2018/mag/qual.pdf"
out <- extract_tables(location)

# construct a matrix out of the output
final <- do.call(rbind,out)

# convert to data frame for some tidverse slapping
results = data.frame(final)

# remove extraneous columns
results = results %>% select(-c("X9","X10", "X11", "X12", "X13", "X14"))

# filter out the repeated column headings from the pdf
# note that nasty special character
results = results %>% filter(X1 != "RankBibNameNOC\rCode") 

# create rank, bib, name, noc columns
# make sure to count the number of digits to get acurate rank number:
# digits = 3 digit bib number + up to 3 digit rank num
results = results %>% mutate(rank=as.numeric(str_sub(X1, 1, str_count(X1,"\\d")-3)),
                             bib = as.numeric(str_sub(X1, str_count(X1,"\\d")-2,str_count(X1,"\\d"))),
                             name = str_sub(X1,str_count(X1,"\\d")+1,-7),
                             noc = str_sub(X1,-6,-4))
# drop the old column
results <- subset(results, select=-X1)



# get rid of DNS's
levels(results$X2)[levels(results$X2)=="DNS"] <- "0.000 00.000 (0)\r0.000"
levels(results$X3)[levels(results$X3)=="DNS"] <- "0.000 00.000 (0)\r0.000"
levels(results$X4)[levels(results$X4)=="DNS"] <- "0.000 00.000 (0)\r0.000"
levels(results$X5)[levels(results$X5)=="DNS"] <- "0.000 00.000 (0)\r0.000"
levels(results$X6)[levels(results$X6)=="DNS"] <- "0.000 00.000 (0)\r0.000"
levels(results$X7)[levels(results$X7)=="DNS"] <- "0.000 00.000 (0)\r0.000"


# seperate out individual event scores
# floor
results = results %>% mutate(fx_d=str_sub(X2, 1, 5),
                             fx=str_extract(X2, "\\d\\d\\.\\d\\d\\d"),
                             fx_rank=str_extract(X2, "\\(\\d+\\)") %>% str_sub(2,-2),
                             fx_e=str_extract(X2, "\\)\\\r\\d\\.\\d\\d\\d") %>% str_sub(3, -1),
                             fx_n=str_extract(X2, "\\-\\d\\.\\d") %>% str_sub(2,-1))

# pommel horse
results = results %>% mutate(ph_d=str_sub(X3, 1, 5),
                             ph=str_extract(X3, "\\d\\d\\.\\d\\d\\d"),
                             ph_rank=str_extract(X3, "\\(\\d+\\)") %>% str_sub(2,-2),
                             ph_e=str_extract(X3, "\\)\\\r\\d\\.\\d\\d\\d") %>% str_sub(3, -1),
                             ph_n=str_extract(X3, "\\-\\d\\.\\d") %>% str_sub(2,-1))
# rings
results = results %>% mutate(sr_d=str_sub(X4, 1, 5),
                             sr=str_extract(X4, "\\d\\d\\.\\d\\d\\d"),
                             sr_rank=str_extract(X4, "\\(\\d+\\)") %>% str_sub(2,-2),
                             sr_e=str_extract(X4, "\\)\\\r\\d\\.\\d\\d\\d") %>% str_sub(3, -1),
                             sr_n=str_extract(X4, "\\-\\d\\.\\d") %>% str_sub(2,-1))

# vault
results = results %>% mutate(vt_d=str_sub(X5, 1, 5),
                             vt=str_extract(X5, "\\d\\d\\.\\d\\d\\d"),
                             vt_rank=str_extract(X5, "\\(\\d+\\)") %>% str_sub(2,-2),
                             vt_e=str_extract(X5, "\\)\\\r\\d\\.\\d\\d\\d") %>% str_sub(3, -1),
                             vt_n=str_extract(X5, "\\-\\d\\.\\d") %>% str_sub(2,-1))


# parallel bars
results = results %>% mutate(pb_d=str_sub(X6, 1, 5),
                             pb=str_extract(X6, "\\d\\d\\.\\d\\d\\d"),
                             pb_rank=str_extract(X6, "\\(\\d+\\)") %>% str_sub(2,-2),
                             pb_e=str_extract(X6, "\\)\\\r\\d\\.\\d\\d\\d") %>% str_sub(3, -1),
                             pb_n=str_extract(X6, "\\-\\d\\.\\d") %>% str_sub(2,-1))

# horizontal bar
results = results %>% mutate(hb_d=str_sub(X7, 1, 5),
                             hb=str_extract(X7, "\\d\\d\\.\\d\\d\\d"),
                             hb_rank=str_extract(X7, "\\(\\d+\\)") %>% str_sub(2,-2),
                             hb_e=str_extract(X7, "\\)\\\r\\d\\.\\d\\d\\d") %>% str_sub(3, -1),
                             hb_n=str_extract(X7, "\\-\\d\\.\\d") %>% str_sub(2,-1))

# drop the former event columns
results <- subset(results, select=-c(X2, X3, X4,X5, X6, X7))

# convert needed to numeric
vars = c("fx_d", "fx_e", "fx_n", "fx", "fx_rank",
         "ph_d", "ph_e", "ph_n", "ph", "ph_rank",
         "sr_d", "sr_e", "sr_n", "sr", "sr_rank",
         "vt_d", "vt_e", "vt_n", "vt", "vt_rank",
         "pb_d", "pb_e", "pb_n", "pb", "pb_rank",
         "hb_d", "hb_e", "hb_n", "hb", "hb_rank")
results[vars] = sapply(results[vars], as.numeric)

# # remove NAs: brute force
# results$fx_d = results$fx_d %>% replace_na(0)
# results$fx_e = results$fx_e %>% replace_na(0)
# results$fx_n = results$fx_n %>% replace_na(0)
# results$fx = results$fx %>% replace_na(0)
# results$fx_rank = results$fx_rank %>% replace_na(0)
# results$ph_d = results$ph_d %>% replace_na(0)
# results$ph_e = results$ph_e %>% replace_na(0)
# results$ph_n = results$ph_n %>% replace_na(0)
# results$ph = results$ph %>% replace_na(0)
# results$ph_rank = results$ph_rank %>% replace_na(0)
# results$sr_d = results$sr_d %>% replace_na(0)
# results$sr_e = results$sr_e %>% replace_na(0)
# results$sr_n = results$sr_n %>% replace_na(0)
# results$sr = results$sr %>% replace_na(0)
# results$sr_rank = results$sr_rank %>% replace_na(0)
# results$vt_d = results$vt_d %>% replace_na(0)
# results$vt_e = results$vt_e %>% replace_na(0)
# results$vt_n = results$vt_n %>% replace_na(0)
# results$vt = results$vt %>% replace_na(0)
# results$vt_rank = results$vt_rank %>% replace_na(0)
# results$pb_d = results$pb_d %>% replace_na(0)
# results$pb_e = results$pb_e %>% replace_na(0)
# results$pb_n = results$pb_n %>% replace_na(0)
# results$pb = results$pb %>% replace_na(0)
# results$pb_rank = results$pb_rank %>% replace_na(0)
# results$hb_d = results$hb_d %>% replace_na(0)
# results$hb_e = results$hb_e %>% replace_na(0)
# results$hb_n = results$hb_n %>% replace_na(0)
# results$hb = results$hb %>% replace_na(0)
# results$hb_rank = results$hb_rank %>% replace_na(0)

# better loop version
# replace NA scores with 0's
for (v in vars) {
  results[v][,1] = results[v][,1] %>% replace_na(0)
}

# compute AA and Qualification
results = results %>% mutate(AA=fx+ph+sr+vt+pb+hb,
                             Q = str_extract(X8, "[:alpha:]"))

# DNF's will return a 'D'
# change those to a 'N'
results$Q[results$Q == "D"] = "N"

# Change the NAs to N
results$Q = results$Q %>% replace_na("N")

# drop the column
results = subset(results, select=-X8)

# write to csv
write_csv(results, "results.csv")

