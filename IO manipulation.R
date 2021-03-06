#----------------------------------------------------------------------
# Database and data manipulation for macroeconomic modeling
#----------------------------------------------------------------------
#
#----------------------------------------------------------------------
# Input-output manipulation
#----------------------------------------------------------------------
library(tidyverse)                        # For data manipulation / visualsation
library(zoo)

#----------------------------------------------------------------------
# Read in raw data from ABS - TO DO, automate download from ABS website
#----------------------------------------------------------------------

# TABLE 8. INDUSTRY BY INDUSTRY FLOW TABLE (INDIRECT ALLOCATION OF IMPORTS)
IO_tab_8_1617 <- readxl::read_xls("C:/Users/aelde/OneDrive/Documents/GitHub/METRIC/Model/data/ABS/IO tables/520905500108_201617.xls", sheet = "Table 8")


names_tab3 <- tibble(file_name =c("(16-17)","(15-16)","(14-15)","(13-14)","(12-13)","(09-10)","(08-09)","(07-08)","(06-07)"),
                list_name = c("16-17","15-16","14-15","13-14","12-13","09-10","08-09","07-08","06-07"))

# TABLE 3. IMPORTS BY INDUSTRY TO INDUSTRY
IO_tab_3 <- list()
for(i in 1:dim(names_tab3)[1]){
  
  file_name <- names_tab3[["file_name"]][i]
  list_name <- names_tab3[["list_name"]][i]
  
  IO_tab_3[[paste(list_name)]] <- readxl::read_xls(paste0("C:/Users/aelde/OneDrive/Documents/GitHub/METRIC/Model/data/ABS/IO tables/520905500103 ",file_name,".xls"), sheet = "Table 3")

  
}


# Table 40. IOIG(2015) to ANZSIC06 CONCORDANCE 
IOIG_ANZSIC <- readxl::read_xls("C:/Users/aelde/OneDrive/Documents/GitHub/METRIC/Model/data/ABS/IO tables/IO Concordances.xls", sheet = "IOIG(2015) to ANZSIC06")

# ANZSIC06 1 DIGIT TO 4 DIGIT  CONCORDANCE
ANZSIC1to4 <- readxl::read_xls("C:/Users/aelde/OneDrive/Documents/GitHub/METRIC/Model/data/ABS/IO tables/1292.0.55.002_anzsic 2006 - codes and titles.xls", sheet ="Classes")

#----------------------------------------------------------------------
# Create concordances
#----------------------------------------------------------------------

## ANZSIC 1 DIGIT TO ANZSIC 4 DIGIT ##
# Fair bit of work here to get this into the required shape
ANZSIC1to4 <- ANZSIC1to4[-c(1:5),-1]

# Split the columns into names and codes
ANZSIC1to4 <- ANZSIC1to4 %>% 
  mutate('1D_NAME' = gsub("*[0-9]",NA, ...3),
         '2D_NAME' = gsub("*[0-9]",NA, ...4),
         '3D_NAME' = gsub("*[0-9]",NA, ...5),
         '4D_NAME' = gsub("*[0-9]",NA, ...6),
         '2D_CODE' = gsub("*[^0-9]",NA, ...3),
         '3D_CODE' = gsub("*[^0-9]",NA, ...4),
         '4D_CODE' = gsub("*[^0-9]",NA, ...5),
         '1D_CODE' = ...2) %>% 
  select("1D_CODE","1D_NAME","2D_CODE","2D_NAME",
         "3D_CODE","3D_NAME","4D_CODE","4D_NAME") 

# Carry across the data in the first 3 rows, so NA.LOCF will work
ANZSIC1to4[1,-c(1:2)] <- ANZSIC1to4[1,2]
ANZSIC1to4[2,-c(1:4)] <- ANZSIC1to4[2,4]
ANZSIC1to4[3,-c(1:6)] <- ANZSIC1to4[3,6]

# Carry down the last observations before NA in each column. Note a cleaner way to do this is with SAPPLY
ANZSIC1to4 <- ANZSIC1to4 %>% 
  mutate('1D_NAME' = na.locf(`1D_NAME`),
         '2D_NAME' = na.locf(`2D_NAME`),
         '3D_NAME' = na.locf(`3D_NAME`),
         '4D_NAME' = na.locf(`4D_NAME`),
         '2D_CODE' = na.locf(`2D_CODE`),
         '3D_CODE' = na.locf(`3D_CODE`),
         '4D_CODE' = na.locf(`4D_CODE`),
         '1D_CODE' = na.locf(`1D_CODE`)) 

# Remove duplicate codes from 4 DIGIT CODE column
ANZSIC1to4 <- ANZSIC1to4[-c(1:3),]
ANZSIC1to4 <- ANZSIC1to4[!duplicated(ANZSIC1to4$`4D_CODE`),]


## IOIG TO ANZSIC 4 DIGIT ##
# Clean concordance so it is tidy format
colnames(IOIG_ANZSIC) <- IOIG_ANZSIC[1,] 
IOIG_ANZSIC <- IOIG_ANZSIC[-1,]
IOIG_ANZSIC <- IOIG_ANZSIC[rowSums(is.na(IOIG_ANZSIC)) != ncol(IOIG_ANZSIC),]

# Some leading zeros were lost when importing from excel, these are fixed, then blank rows are carried forward
IOIG_ANZSIC <- IOIG_ANZSIC %>% 
  mutate(IOIG = if_else(nchar(IOIG) == 3, paste0(0,IOIG), IOIG),
         IOIG = zoo::na.locf(IOIG)) %>%
  mutate(`ANZSIC Code` = if_else(nchar(`ANZSIC Code`) == 3, paste0(0,`ANZSIC Code`), `ANZSIC Code`),
         `ANZSIC Code` = zoo::na.locf(`ANZSIC Code`)) %>% 
  mutate(`IOIG Descriptor` =zoo::na.locf(`IOIG Descriptor`)) %>% 
  rename(`4D_CODE` = `ANZSIC Code`) %>% 
  left_join(ANZSIC1to4)

# Wine spirits and tobbaco (1205 in the IO table) is split into 3 cat in the concordance, adjusting 

IOIG_ANZSIC$IOIG <- gsub("1203","1205",IOIG_ANZSIC$IOIG )
IOIG_ANZSIC$IOIG <- gsub("1204","1205",IOIG_ANZSIC$IOIG )


# Clean IO Table data and aggregate to ANZISC 1 digit.
IOIG_ANZSIC_1D <- IOIG_ANZSIC %>% 
  select(IOIG,`1D_CODE`,`1D_NAME`) %>% 
  distinct()

#----------------------------------------------------------------------
# Join concordances to tables
#----------------------------------------------------------------------


IO_tab_8_1617 <- IO_tab_8_1617 %>% 
  rename(`IOIG` = ...1) %>%
  left_join(IOIG_ANZSIC_1D) 

IO_tab_8_1617 <- IO_tab_8_1617[rowSums(is.na(IO_tab_8_1617)) != ncol(IO_tab_8_1617),]

IO_tab_3 <-  sapply(IO_tab_3,function(x){
  
  
  colnames(x)[1] <- "IOIG"
  
  x <-  left_join(x,IOIG_ANZSIC_1D) 
  
  x[rowSums(is.na(x)) != ncol(x),]
  
  
  })


#----------------------------------------------------------------------
# Create aggregated IO numbers
#----------------------------------------------------------------------
# Create aggregation matrix S = (number of ANZSIC 1 D x number of IOIG)
# Aggregated T matrix NT = ST'S
# Aggregated NFD = SFD
# Aggregated total output NX = SX

SMAT <- IO_tab_8_1617 %>% 
  select(`1D_CODE`,IOIG) %>%
  filter(!is.na(`1D_CODE`)) %>% 
  mutate(Fill = `1D_CODE`) %>% 
  spread(IOIG, Fill)

SMAT[,-1] <- sapply(SMAT[,-1],function(x){
  
  ifelse(is.na(x),0,1)
  
})

# Transaction matrix

TMAT <- IO_tab_8_1617[3:116,3:116] %>%  
  sapply(function(x)as.numeric(x))

TMAT <- as.matrix(SMAT[,-1])%*%as.matrix(TMAT)%*%t(as.matrix(SMAT[,-1]))

colnames(TMAT) <- unique(IO_tab_8_1617$`1D_CODE`)[-1]
row.names(TMAT) <- unique(IO_tab_8_1617$`1D_CODE`)[-1]

# Final demand matrix

FDMAT <- IO_tab_8_1617[3:116,-c(1:2,3:116,127:128)] %>% 
  sapply(function(x)as.numeric(x))

FDMAT <- as.matrix(SMAT[,-1])%*%as.matrix(FDMAT)

colnames(FDMAT) <- IO_tab_8_1617[2,-c(1:2,3:116,127:128)]
row.names(FDMAT) <- unique(IO_tab_8_1617$`1D_CODE`)[-1]

# Total outlays matrix

TOMAT <- IO_tab_8_1617[-c(1:2,3:116,127),3:116] %>% 
  sapply(function(x)as.numeric(x))

TOMAT <- as.matrix(SMAT[,-1])%*%t(as.matrix(TOMAT)) %>% 
  t()

colnames(TOMAT) <- unique(IO_tab_8_1617$`1D_CODE`)[-1]
row.names(TOMAT) <- c("T1","P1","P2","P3","P4","P5","D","P6","Y","GVA")

# Total outlays on FD side

TOFD <-  IO_tab_8_1617[-c(1:2,3:116,127),-c(1:2,3:116,127:128)] %>% 
  sapply(function(x)as.numeric(x))

colnames(TOFD) <- IO_tab_8_1617[2,-c(1:2,3:116,127:128)]
row.names(TOFD) <- c("T1","P1","P2","P3","P4","P5","D","P6","Y","GVA")

# Put back together

IO_TAB_1D_1617 <- tibble(From_IND = c(unique(IO_tab_8_1617$`1D_CODE`)[-1],"T1","P1","P2","P3","P4","P5","D","P6","Y","GVA")) %>% 
  cbind(rbind(TMAT,TOMAT)) %>% 
  cbind(rbind(FDMAT,TOFD))

# Seems to get to 09-07
IO_tab_3_agg <- IO_tab_3[-c(1,9)]

IO_tab_3_agg <- lapply(IO_tab_3_agg,function(x){


  SMAT <- x %>% 
    select(`1D_CODE`,IOIG) %>%
    filter(!is.na(`1D_CODE`)) %>% 
    mutate(Fill = `1D_CODE`) %>% 
    spread(IOIG, Fill)
  
  SMAT[,-1] <- sapply(SMAT[,-1],function(x){
    
    ifelse(is.na(x),0,1)
    
  })
  
  # Transaction matrix #x[complete.cases(x),3:116] %>%  
  TMAT <- x[grepl("^[0-9]",x$IOIG),grepl("^[0-9]",x[which(x[,2]=="USE"),])] 
  TMAT <- TMAT[complete.cases(TMAT),] %>% 
        sapply(function(x)as.numeric(x))
  
  TMAT <- as.matrix(SMAT[,-1])%*%as.matrix(TMAT)%*%t(as.matrix(SMAT[,-1]))
  
  colnames(TMAT) <- unique(x$`1D_CODE`)[-1]
  row.names(TMAT) <- unique(x$`1D_CODE`)[-1]
  
  # Final demand matrix
  FDMAT <- x[grepl("^[0-9]",x$IOIG), grepl("T4|Q1|Q2|Q3|Q4|Q5|Q6|Q7|T5|T6",x[which(x[,1]=="SUPPLY"),])]
  FDMAT <- FDMAT[complete.cases(FDMAT),] %>% 
    sapply(function(x)as.numeric(x))
  
  FDMAT[is.na(FDMAT)] <- 0
  
  FDMAT <- as.matrix(SMAT[,-1])%*%as.matrix(FDMAT)
  
  colnames(FDMAT) <- c("T4",	"Q1",	"Q2",	"Q3","Q4", "Q5",	"Q6",	"Q7",	"T5",	"T6")
  row.names(FDMAT) <- unique(IO_tab_8_1617$`1D_CODE`)[-1]
  
  # Total competing imports matrix
  
  TOMAT <- colSums(TMAT)
  TOMAT <-  t(as.matrix(TOMAT))
  row.names(TOMAT) <- c("TOTAL COMPETING IMPORTS")
  
  # Total outlays on FD side
  TOFD <- colSums(FDMAT)
  TOFD <-  t(as.matrix(TOFD))
  
  colnames(TOFD) <- c("T4",	"Q1",	"Q2",	"Q3","Q4", "Q5",	"Q6",	"Q7",	"T5",	"T6")
  row.names(TOFD) <- c("TOTAL COMPETING IMPORTS")
  
  # Put back together
  
  x <- tibble(From_IND = c(unique(x$`1D_CODE`)[-1],"TOTAL COMPETING IMPORTS")) %>% 
    cbind(rbind(TMAT,TOMAT)) %>% 
    cbind(rbind(FDMAT,TOFD))
  

return(x)
  
})


agg_io_data <- function(x){
  
#  x <- IO_tab_3$`16-17`  
  
  SMAT <- x %>% 
    select(`1D_CODE`,IOIG) %>%
    filter(!is.na(`1D_CODE`)) %>% 
    mutate(Fill = `1D_CODE`) %>% 
    spread(IOIG, Fill)
  
  SMAT[,-1] <- sapply(SMAT[,-1],function(x){
    
    ifelse(is.na(x),0,1)
    
  })
  
  # Transaction matrix #x[complete.cases(x),3:116] %>%  
  
  TMAT <- x[3:116,3:116] %>%  
    sapply(function(x)as.numeric(x))
  
  TMAT <- as.matrix(SMAT[,-1])%*%as.matrix(TMAT)%*%t(as.matrix(SMAT[,-1]))
  
  colnames(TMAT) <- unique(x$`1D_CODE`)[-1]
  row.names(TMAT) <- unique(x$`1D_CODE`)[-1]
  
  # Final demand matrix
  
  FDMAT <- x[3:116,-c(1:2,3:116,127:128)] %>% 
    sapply(function(x)as.numeric(x))
  
  FDMAT[is.na(FDMAT)] <- 0
  
  FDMAT <- as.matrix(SMAT[,-1])%*%as.matrix(FDMAT)
  
  colnames(FDMAT) <- x[2,-c(1:2,3:116,127:128)]
  row.names(FDMAT) <- unique(x$`1D_CODE`)[-1]
  
  
  # Total competing imports matrix
  
  TOMAT <- colSums(TMAT)
  TOMAT <-  t(as.matrix(TOMAT))
  row.names(TOMAT) <- c("TOTAL COMPETING IMPORTS")
  
  # Total outlays on FD side
  TOFD <- colSums(FDMAT)
  TOFD <-  t(as.matrix(TOFD))
  
  colnames(TOFD) <- c("T4",	"Q1",	"Q2",	"Q3","Q4", "Q5",	"Q6",	"Q7",	"T5",	"T6")
  row.names(TOFD) <- c("TOTAL COMPETING IMPORTS")
  
  # Put back together
  
  x <- tibble(From_IND = c(unique(x$`1D_CODE`)[-1],"TOTAL COMPETING IMPORTS")) %>% 
    cbind(rbind(TMAT,TOMAT)) %>% 
    cbind(rbind(FDMAT,TOFD))
  
  
  return(x)
  
}

IO_tab_3_agg[["16-17"]] <- agg_io_data(IO_tab_3$`16-17`)

# Interpolate missing years with spline (should think about RASING each one)

name_list <- tibble(IO_tab_3_name = names(IO_tab_3_agg),
                    new_name = c("2016","2015","2014","2013","2010","2009","2008","2017"))

IO_TAB_3 <- list()
for(i in seq_along(IO_tab_3_agg)){
  
  new_name <- name_list$new_name[i]
  IO_tab_3_agg_name <- name_list$IO_tab_3_name[i]
  
   IO_TAB_3[[new_name]] <- IO_tab_3_agg[[IO_tab_3_agg_name]] %>%
    mutate(Year = new_name) %>%
    gather(Use, Value, -Year, -From_IND) %>% 
    rename(Supply = From_IND)
  
  
}

IO_TAB_3 <- bind_rows(IO_TAB_3)

# Create NAs for missing years
IO_TAB_3 <- IO_TAB_3 %>% 
  mutate(Year = as.numeric(Year)) %>% 
  group_by(Year,Supply,Use) %>%
  complete(Year = 2008:2017) %>% 
  distinct(Value) %>% 
  group_by(Supply,Use) %>% 
  mutate(Value = na.spline(Value))

#----------------------------------------------------------------------
# Create charts
#----------------------------------------------------------------------

# new_name <- tibble(old_name1 = names(IO_tab_3_agg$`16-17`)[-1],
#                    new_name1 = c("AG","MIN","MAN",rep())
#                    old_name2 = row.names(IO_tab_3_agg$`16-17`)[-20])

IO_TAB_3 <-  IO_TAB_3 %>%
ungroup() %>%
  group_by(Year, Use) %>% 
  mutate(Supply =  str_replace_all(Supply, pattern = model_anzsic_conc$`1D_CODE`, replacement = model_anzsic_conc$model_name )) %>%
  ungroup() %>% 
  group_by(Supply,Year) %>% 
  mutate(Use =  str_replace_all(Use, pattern = model_anzsic_conc$`1D_CODE`, replacement = model_anzsic_conc$model_name )) %>%
  mutate(Use =  ifelse(Use == "R", "ARS", Use),
         Use =  ifelse(Use == "S", "OS", Use)) %>% 
  ungroup() %>% 
  mutate( Supply = gsub("TOTAGL COMPETING IMPORTS", "TOTAL COMPETING IMPORTS", .$Supply))

# Use

IO_TAB_3 %>% 
  filter(Supply != "TOTAL COMPETING IMPORTS") %>%
  filter(Use %in% model_anzsic_conc$model_name ) %>%
  left_join(IO_TAB_3 %>%
              filter(Supply == "TOTAL COMPETING IMPORTS") %>%
              ungroup() %>% 
              select(-Supply) %>% 
              rename(`TOTAL COMPETING IMPORTS` = Value)) %>%
  ggplot()+
  geom_bar(aes(x = Year, y = Value, fill = Supply), stat = "identity", colour = "grey") +
  geom_line(aes(x = Year, y = `TOTAL COMPETING IMPORTS`), size= 1)+
  theme_bw()+
  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0))+ 
  facet_trelliscope(~Use, scales = "free" , as_plotly = TRUE, path = paste0(getwd(),"/temp"))



#----------------------------------------------------------------------
# Create additional matrices for Leontief
#----------------------------------------------------------------------

AMAT <-  TMAT%*%((FDMAT[,"T6"])^-1*diag(dim(TMAT)[1])) 

LMAT <- solve(diag(dim(TMAT)[1])-AMAT)

IO_FD <- LMAT%*%FDMAT[,c("Q1","Q2","Q3","Q4","Q5","Q6","Q7")]

IO_FD <- IO_FD%*%((colSums(IO_FD)+colSums(TOFD[c("P3","P6"),c("Q1","Q2","Q3","Q4","Q5","Q6","Q7")]))^-1*diag(dim(IO_FD)[2]))
colnames(IO_FD) <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7")

IO_COEFS <- tibble(Ind = c(unique(IO_tab_8_1617$`1D_CODE`)[-1])) %>% 
  cbind(IO_FD) %>% 
  gather(Expenditure, Coef, -Ind)  # Note To_ind is the row of the L matrix

#----------------------------------------------------------------------
# Save to list to be read into model data base for RASing
#----------------------------------------------------------------------

IODATA <- list(IO_1D_1617  = list(IO_TAB_1D_1617 = IO_TAB_1D_1617,
                                  TMAT = TMAT,
                                  FDMAT = FDMAT,
                                  TOMAT = TOMAT,
                                  TOFD = TOFD,
                                  AMAT = AMAT,
                                  LMAT = LMAT,
                                  IO_COEFS = IO_COEFS))


saveRDS(IODATA, "C:/Users/aelde/OneDrive/Documents/GitHub/METRIC/Model/data/ABS/IO tables/IODATA.rds")
