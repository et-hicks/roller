
# package 'roller' inspired by Gaston Sanchez's cointoss package
# his code can be found on github: https://github.com/gastonstat/cointoss

# private function to check whether or not the sides of a device are valid
check_sides <- function(vec){
  if(length(levels(factor(vec))) != length(vec)){
    stop("\n'sides' cannot have duplicated elements")
  }
  if(length(vec) == 1){
    stop("\n'sides' must be a vector of length greater than 1")
  }
  if (!(is.numeric(vec)) & !(is.character(vec))) {
    stop("\n'sides' must be a character or numeric vector")
  }
  TRUE
}

# private function to check whether or not the probabilites of the device are valid
check_prob <- function(vec){
  if(class(vec) != "numeric"){
    stop("\n'prob' must a vector of numbers between 0 and 1")
  }
  if(sum(vec) != 1){
    stop("\nelements in 'prob' must add up to 1")
  }
  if(length(vec) == 1){
    stop("\n'prob' must be length greater than 1")
  }
  TRUE
}

#' @export
print.device <- function(device) {
  cat('object "device"\n\n')
  cd <- data.frame(
    sides = device$sides, prob = device$prob
  )
  print(cd)
  invisible(device)
}

#' @title Device
#' @description Creates an object of class \code{"device"}
#' @param sides a character or numeric vector that will act as the names of the sides of the device
#' @param prob a numeric vector of decimals that sum to one that will act as the probabilities of the sides of the device
#' @return an object of class \code{"device"}
#' @export
#' @examples
#'
#' default_device <- device()
#'
#' loded_die <- device(sides = 1:6, prob = c(.1, .1, .1, .1, .1, .5))
#'
#' fair_die <- device(sides = 1:6, prob = rep(1/6, 6))
#'
#' named_coin <- device(sides = c("heads", "tails", "left", "right"), prob = rep(1/4, 4))
#'
device <- function(sides = 1:2, prob = c(.5, .5)){
  if(length(sides) != length(prob)){
    stop("\nSides must have same length as probabilities")
  }
  check_sides(sides)
  check_prob(prob)

  obj <- list(sides = sides, prob = prob)
  class(obj) <- "device"

  return(obj)

}

#' @title is.device
#' @description A function to see if an R object has the class of "device"
#' @param src Any R object
#' @export
is.device <- function(src){
  if(class(src) == "device"){
    return(TRUE)
  }
  return(FALSE)
}

# private function to check vector of 'times'
check_times <- function(vec){
  if(!(vec >= 1)){
    stop("/nTimes must be greater than zero")
  }
  if(!(is.numeric(vec))){
    stop("/nTimes must be a number")
  }
}

#' @title Make the roll object
#' @description This function is what creates the R object "rolls"
#' @param device the device object that is rolled
#' @param rolls vector of the rolls of the device
#' @keywords internal
make_roll <- function(device, rolls){
  obj <- list(rolls = rolls,
              sides = device$sides,
              prob = device$prob,
              total = length(rolls))
  class(obj) <- "rolls"

  return(obj)
}

#' @title Roll
#' @description This function creates the rolls that the device contains, and rolls them as many times as the user defines
#' @param device an R object of class \code{"device"}
#' @param times the number of times the user wishes to roll the device
#' @return an object of class \code{"rolls"}
#' @return an item \code{"rolls"}: a vector of length(times) containing the rolls of the device
#' @return an item \code{"sides"}: a vector of the names or numbers of the sides of the rolled device
#' @return an item \code{"prob"}: a vector of probabilites for each side that the rolled device has
#' @return an item \code{"total"}: a single number equal to the number of times the device was rolled, which is equal to parameter times
#' @export
#' @examples
#'
#' # creating the device that makes the roll
#' device1 <- device()
#'
#' # Rolls the device 50 times
#' roll50 <- roll(device, 50)
#'
#' # the user can add more rolls to the basic device:
#' roll150 <- roll50 + 100
#'
roll <- function(device, times = 1){
  check_times(times)
  if(!(class(device) == "device")){
    stop("\nDevice of incorrect class")
  }
  rolls <- sample(device$sides, size = times, replace = TRUE, prob = device$prob)
  make_roll(device, rolls)
}

#' @export
print.rolls <- function(src){
  cat('object "rolls"\n\n')
  cd <- list(
    rolls = src$rolls, sides = src$sides, prob = src$prob, total = src$total
  )
  cat('$rolls\n')
  print(cd$rolls)
  invisible(src)
}

#' @export
summary.rolls <- function(src){
  count <- c()
  for(i in src$sides){
    i_count <- sum(i == src$rolls)
    count <- c(count, i_count)
  }
  prop <- c()
  for(i in count){
    proportion <- i/length(src$rolls)
    prop <- c(prop, proportion)
  }
  freqs <- data.frame(
    side = src$sides, count = count, prop = prop
  )
  obj <- list(freqs = freqs)
  class(obj) <- "summary.rolls"
  return(obj)
}

#' @export
print.summary.rolls <- function(src){
  cat('summary "rolls"\n\n')
  print(src$freqs)
  invisible(src)
}

#' @title Plot of object rolls
#' @description In a barplot, plots the relative frequencies of the rolls against the sides of the device
#' @param rolls an R object of class \code{"rolls"}
#' @param \dots arguments to be passed to/from other methods
#' @export
#' @examples
#'
#' \dontrun{
#' # Create the basic default device object, a fair sided coin
#'
#' device1 <- device()
#'
#' # roll the basic device 100 times
#'
#' roll100 <- roll(device = device1, times = 100)
#'
#' plot(roll100)
#'
#' # can add more to roll100
#'
#' roll200 <- roll100 + 100
#'
#' # can plot that too
#'
#' plot(roll200)
#'
#' }
plot.rolls <- function(rolls, ...){
  plotted <- summary(rolls)
  barplot(
    height = plotted$freqs$prop,
    xlab = "sides of device",
    ylab = "relative frequencies",
    names.arg = rolls$sides
    )
  title(sprintf("Relative Frequencies in a series of %s coin tosses", rolls$total))
}

#' @export
"[<-.rolls" <- function(src, i, value){
  if(!(value %in% src$sides)){
    stop("Replacing value must be in 'sides' vector")
  }
  if(i > src$total){
    stop("Replacement must be in original range")
  }
  src$rolls[i] <- value
  make_roll(src, src$rolls)
}

#' @export
"[.rolls" <- function(src, i) {
  if(i > src$total){
    stop("Index out of bounds")
  }

  src$rolls[i]
}

#' @rdname roll
#' @param src any R object
#' @return boolean value indicating if the source object is of class \code{"rolls"}
#' @export
is.rolls <- function(src) {
  inherits(src, "rolls")
}

#' @export
"+.rolls" <- function(src, incr){
  if(incr <= 0){
    stop("Roll increment must be greater than zero")
  }
  if(length(incr) > 1){
    stop("Roll increment must be one number")
  }
  new_rolls <- roll(device(src$sides, src$prob), incr)
  make_roll(src, c(src$rolls, new_rolls$rolls))
}

