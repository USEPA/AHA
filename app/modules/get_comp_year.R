# This function takes the following inputs: Compliance Year, Program Code and provides the 
# allowance transfer deadline for the inputted compliance year. It also accounts for leap years.
# Will need to be updated each year for additional compliance periods.
# ##### Transfer Deadline for OTC is 12/31 of the year, for NBP it is NOV 30 of the year #####

library(hash)
library(data.table)
library(dplyr)

get_comp_year <- function(compliance_year, program_code ){
  program_date_hash <-
    hash(
      c(
        "1995",
        "1996",
        "1997",
        "1998",
        "1999",
        "2000",
        "2001",
        "2002",
        "2003",
        "2004",
        "2005",
        "2006",
        "2007",
        "2008",
        "2009",
        "2010",
        "2011",
        "2012",
        "2013",
        "2014",
        "2015",
        "2016",
        "2017",
        "2018",
        "2019",
        "2020"
      ),
      c(
        "1996/2/29",
        "1997/3/3",
        "1998/3/2",
        "1999/3/1",
        "2000/2/29",
        "2001/3/1",
        "2002/3/1",
        "2003/3/3",
        "2004/3/1",
        "2005/3/1",
        "2006/3/1",
        "2007/3/1",
        "2008/2/29",
        "2009/3/2",
        "2010/3/1",
        "2011/3/1",
        "2012/2/29",
        "2013/3/1",
        "2014/3/3",
        "2015/3/2",
        "2016/2/29",
        "2017/3/1",
        "2018/3/1",
        "2019/3/1",
        "2020/3/2",
        "2021/3/1"
      )
    )
  program_date_otc <-
    hash(
      c("1999", "2000", "2001", "2002"),
      c("1999/12/31", "2001/1/1", "2001/12/31", "2002/12/31")
    )
  program_date_nbp <-
    hash(
      c("2003", "2004", "2005", "2006", "2007", "2008"),
      c(
        "2003/12/1",
        "2004/11/30",
        "2005/11/30",
        "2006/11/30",
        "2007/11/30",
        "2008/12/1"
      )
    )
  last_trans_year <- as.character(compliance_year)
  
  last_trans_date <-
    if (program_code == 'NBP') {
      program_date_nbp[[last_trans_year]]
    }
  else{
    if (program_code == 'OTC') {
      program_date_otc[[last_trans_year]]
    }
    else{
      program_date_hash[[last_trans_year]]
    }
  }
}
