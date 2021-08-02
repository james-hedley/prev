\name{prev}
\alias{prev}

\title{prev}

\description{
Make changes to a variable based on its previous or susbequent values, iteratively.

By default, R treats a variable within a dataset as a vector. If you use something like dplyr::lag()
the entire vector will be lagged at the same time.

However, sometimes you may want to apply changes to a variable row by row. prev::prev() makes changes to a variable row by row, which produces similar results to other statistical software packages (e.g. Stata or SAS). 

See examples below

}
\usage{
prev(data, var, newvar = NULL, index = 1, FUN = function(x) {x+1}, groupby = NULL,
  default = NA, ignore_out_of_range = TRUE)

}
\arguments{
  \item{data}{
A tidy dataset containing the variable you want to apply changes to iteratively, based on its previous or subsequent values
}
  \item{var}{
The variable you want to apply changes to iteratively, based on its previous or subsequent values
}
  \item{newvar}{
The name of a new variable that should be created. By defualt this is NULL, meaning that changes will override the current values of var
}
  \item{index}{
The index of the rows that should be referred to when applying changes to var. How many previous rows of var shoudl changes be applied to. 0 means changes are applied to each value of var itself.
A positive index (e.g. 2) means that change are applied to the value 2 rows prior to the current row. A negative index (e.g. -3) means that changes are applied to the value 3 rows subsequent to the current row. By default this is set to +1 (i.e. refers to the previous row)
}
  \item{FUN}{
A function that should be applied to previous or subsequent values of var. By defualt this will add 1, iteratively
}
  \item{groupby}{
The ID variable which the data can be grouped by when referring to previous or subsequent rows. Specifying this can make the ufnction run a lot faster. See examples.
}
  \item{default}{
The 'default' option that will be passed to dplyr::lag() or dplyr::lead(). Value used for non-existent rows. Defaults to NA.
}
  \item{ignore_out_of_range}{
A logical indicating whether values that are out of range should be ignored. For example, if we want to refer to the previous row for each value of var, the first row does not have a previous row. If you specify ignore_out_of_range, then the current value will be used instead. Otherwise, the value will be replaced by NA, which will then be passed down to all subsequent rows since the changes are applied iteratively row by row. 
}
}

\examples{
example_data


# Add 1 to the previous value of var1, using dplyr::lag()
# Note that each row refers to the previous ORIGINAL row of var1
example_data %>% 
  group_by(id) %>%
  mutate(var2=lag(var1)+1) %>%
  ungroup()
  

# Add 1 to the previous value of var1, applying the changes iteratively row by row
example_data %>% 
  prev(var = 'var1',
       newvar = 'var2',
       groupby = 'id', 
       FUN = function(x) {x+1})


# If we hadn't specified groupby = 'id', then we would have continuted to add 1 for all rows, ignoring that they were for different IDs
# This also takes much longer to run, because the data needs to be updated 1000 times (once for each row), instead of just 5 times (one for each row for each ID)
example_data %>% 
  prev(var = 'var1',
       newvar = 'var2',
       FUN = function(x) {x+1})
       
}


\references{
Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2021). dplyr: A Grammar of Data Manipulation. R package
  version 1.0.7. https://CRAN.R-project.org/package=dplyr
}

\author{
James Hedley
}

