#hw 12

#Package
library(dplyr)

#Upload data file 
dat <- read_csv(url("http://mgimond.github.io/ES218/Data/ACS.csv"))

#Create scatterplot on the median houshold income vs percent of county pop
#compute a percentage value from the educational attainment count estimates 
ACS <- dat %>%
  mutate(PercBD = B23006023/B23006001 * 100) %>%
  select(B19013001, PercBD)

#Plot Scatterplot 
plot(B19013001 ~ PercBD, ACS, pch = 16, col = rgb(0,0,0,.1), 
     xlab = "Percentage of county population that has a a bachelor’s 
     degree or greater", ylab = "Median household income ($)") 

#Plot same graph in boxplot form ordered by median state income values
#Limit states to ME, NH, VT, MA, RI, CT, NY 

#Filter for those states 
ASC1 <- dat %>%
  select(State, B19013001) %>% 
  filter(State %in%  c("me", "ma", "vt", "nh","ct","ny","ri")) %>%
  mutate(State = reorder(State, B19013001, median), 
         State = droplevels(State))

#Create boxplot
boxplot(B19013001 ~ State, ASC1, xlab = "State", ylab = "Median Income ($)") 








