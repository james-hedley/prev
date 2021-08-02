# Project: R function to update values in a vector or variable iteratively, based on their previous or next values
#          Normally, R will update an entire vector/variable in one go, rather than value by value
#          Sometimes though, you want to update each value one by one
#          This is intended to reproduce the effect of Stata's var[n_1] in R
# Created by: James Hedley
# Date created: 29th July 2021
# Last updated: 2nd August 2021

# Load libraries
library('tidyr')
library('dplyr')

# Create an example dataset
set.seed(12345)
example_data <- data.frame(id=rep(seq(1:200),5)) %>%
  as_tibble() %>%
  arrange(id) %>%
  group_by(id) %>%
  mutate(seq=seq_len(n())) %>%
  ungroup() %>%
  mutate(var1=rnorm(n()))


# Define function to iteratively update a variable
prev <- function(data,
                 var,
                 newvar=NULL,
                 index=1,
                 FUN=function(x) {x+1},
                 groupby=NULL,
                 default=NA,
                 ignore_out_of_range=TRUE) {
  
    ## Create a group sequence variable
    data <- data %>%
      group_by(across(all_of(groupby))) %>%
      mutate(groupseq_=seq_len(n())) %>%
      mutate(maxgroupseq_=max(groupseq_)) %>%
      ungroup()
    
    ## Determine the maximum number of rows within any group
    maxgroupseq <- max(select(data,maxgroupseq_))
    
    ## Initialise new variable with same values as the old variable
    data <- data %>% mutate(newvar_=data[[var]])
    
    ## Determine how the variable will be indexed (negative numbers use lag, positive numbers use lead)
    if(index>0) {
      if(!ignore_out_of_range) {
        indexvar <- function() {
          temp <- lag(x=data$newvar_,n=abs(index),default=default)
          return(temp)
        }
      }
      if(ignore_out_of_range) {
        indexvar <- function() {
          temp1 <- data$newvar_[data$groupseq_<=abs(index)]
          temp2 <- lag(x=data$newvar_,n=abs(index),default=default)[data$groupseq_>abs(index)]

          temp1_order <- grep(TRUE,data$groupseq_<=abs(index))
          temp2_order <- grep(TRUE,data$groupseq_>abs(index))
          temp_order <- c(temp1_order,temp2_order)
          
          temp <- c(temp1,temp2) %>%
            bind_cols(temp_order) %>%
            arrange(temp_order)
          
          temp <- as.vector(temp[,1])
          
          return(temp)
        }
      }
    }
    if(index<0) {
      if(!ignore_out_of_range) {
          indexvar <- function() {
            temp <- lead(x=data$newvar_,n=abs(index),default=default)
            return(temp)
          }
      }
      if(ignore_out_of_range) {
        indexvar <- function() {
          temp1 <- lead(x=data$newvar_,n=abs(index),default=default)[data$groupseq_<=((data$maxgroupseq_)-abs(index))]
          temp2 <- data$newvar_[data$groupseq_>((data$maxgroupseq_)-abs(index))]
          
          temp1_order <- grep(TRUE,data$groupseq_<=((data$maxgroupseq_)-abs(index)))
          temp2_order <- grep(TRUE,data$groupseq_>((data$maxgroupseq_)-abs(index)))
          temp_order <- c(temp1_order,temp2_order)
          
          temp <- c(temp1,temp2) %>%
            bind_cols(temp_order) %>%
            arrange(temp_order)
          
          temp <- as.vector(temp[,1])
          
          return(temp)
        }
      }
    }
    
    
    ## Create a variable to flag whether observations are out of range of the index    
    data$out_of_range_ <- (data$groupseq_ - index)<=0 | (data$groupseq_ - index)>data$maxgroupseq_
    
  
    ## Loop through each value of groupseq to make changes to the new variable     
    for(i in 1:maxgroupseq) {
      suppressMessages(
        data <- data %>% 
          mutate(tempnewvar1_=unlist(indexvar())) %>%
          mutate(tempnewvar2_=FUN(tempnewvar1_)) %>%
          mutate(newvar_=ifelse(groupseq_==i & out_of_range_==FALSE,tempnewvar2_,newvar_))
      )
    }
    
    
    ## Update the value of the existing variable (if newvar is not specified)
    if(is.null(newvar)) {
      data[,var] <- data[,newvar_]
    }
    
    ## Add specified new variable to the dataset (if newvar is specified)
    if(!is.null(newvar)) {
      data[,newvar] <- data$newvar_
    }
    
    ## Remove temporary variables
    data <- data %>% select(-groupseq_,-maxgroupseq_,-newvar_,-tempnewvar1_,-tempnewvar2_,-out_of_range_)
    
    
    ## Return the dataset
    return(data)
    
}
