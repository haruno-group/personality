
# install.packages('pacman') 
source('xxx/person_funcR6.R')
parse = scoring$new()

dat = readr::read_csv("yyy/rawdata.csv")
parse$AQ(dat)
parse$Big5(dat)
