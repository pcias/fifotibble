# README #

This is an R package with simple fifo valuation helpers

### What is this repository for? ###

* This is a set of couple functions to help fifo valuation of entries in an R tibble
* 0.1.1


### How do I get set up? ###

* We'll put it on CRAN

### Contribution guidelines ###

* 
* The the main function to use is 'tidyfifo', but the 'fifotibble' is extesively step-by-step commented to show by example its work 
* The most useful e.g. for stock capital gains calculation is the gainOnSell function, careful with execution times, there is some R 
*    iteration there
* See also the tests as they demonstrate the basic usage
* TODO: cleanup sicne both 'tidyfifo' and 'fifotibble' actually implement same algorithm kind of twice


### Who do I talk to? ###

* repo owner:  przemyslaw.cias@gmail.com
