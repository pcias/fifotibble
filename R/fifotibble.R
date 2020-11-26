## You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'



#' Create fifo quants/valuation tibble
#'
#' This function receives movements quantities and prices (purchases/selss) and returns a tibble of open stocks per remaining valuation
#' quants
#' @param qty vector of quantities of movements (receipts positive, issues negative)
#' @param price vector of prices of movements (should be same lenght as qty)
#' @return tibbles: qty_movt, price, quant_stock
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr if_else
#' @importFrom tibble add_row
#' @importFrom tibble tibble
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr ungroup
#' @export
fifotibble <- function(qty, price) {

  x <- tibble(qty, price)
  #stock initialization
  #       qty price
  #        10     10
  #        20     11
  #       -15     10

  x <- x %>% mutate(stock = ifelse(qty>0, qty, 0))
  #       qty     price stock
  #        10     10    10
  #        -5     12    NA
  #        20     11    20
  #       -15     10    NA

  x <- x %>% add_row(stock=x$qty[x$qty < 0] , .before = 1)
  #       qty     price stock
  #        NA     NA    -15
  #        10     10    10
  #        20     11    20
  #       -15     10    NA
  #  qty > 0 - positions, qty < 0 sells event, qty = NA - technical row for calculation


  x <- x %>% mutate(workingopenstock = cumsum(stock))

  #closing of quants on sell
  #set openstock to value <0;stock>
  x <- x %>% mutate(openstock  = if_else(workingopenstock < 0 , 0 ,if_else(workingopenstock > stock, stock, workingopenstock)))
  x <- x  %>% mutate(value = openstock * price)
  # A tibble: 6 x 6
  #     qty price stock workingopenstock openstock value
  #1    NA    NA    -5               -5         0    NA
  #2    NA    NA   -15              -20         0    NA
  #3    10    10    10              -10         0     0
  #4    -5    12    -5              -15         0     0
  #5    20    11    20                5         5    55
  #6   -15    10   -15              -10         0     0


  x <- x %>% mutate(cost_revenues = (openstock-qty)*price)


  #cleanup - return only open quants
  x <- x %>% select(qty, price, openstock,value) %>% filter(!is.na(qty) & openstock > 0 )


  return(x)
}




#' Create fifo quants/valuation tibble
#'
#' This function receives a quant tibble (see \code{\link{fifotibble}} ) and simulates sell of a quantity at a price, returning resulting gain or loss
#' @param fifotbl tibble of fifo valuation quants (result of fifotibble function)
#' @param qtyToSell quantity of stock to sell
#' @param price sale price
#' @param verbose = F, if true returns the full tibble describing quants sold, otherwise returns just the gain/loss value
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr rowwise
#' @importFrom dplyr lag
#' @importFrom tibble add_column
#' @return tibble or single value of gain/loss
#' @seealso \code{\link{fifotibble}}
#' @export
gainOnSell <- function(fifotbl, qtyToSell, price, verbose = F) {

  names(fifotbl) <- c("qty","price","openstock","value")
  x <- fifotbl %>% filter(openstock>0)
  x <- x %>% add_column(remain_to_sell = 0) %>% add_row(remain_to_sell = qtyToSell, openstock = 0, .before = 1)
  x <- x %>% mutate(remain_to_sell = cumsum(remain_to_sell-openstock)) %>% rowwise() %>% mutate(remain_to_sell = max(0, remain_to_sell)) %>% ungroup()
  x <- x %>% mutate(sold = lag(remain_to_sell) - remain_to_sell) %>% mutate(cost = sold * price)

  gain <- sum(x$sold, na.rm = T)*price - sum(x$cost, na.rm = T)

  if(verbose)
    return(x)
  else
    return(gain)
}




reLU <- function(x) {
  if(x < 0)
    return(0)
  else
    return(x)
}
