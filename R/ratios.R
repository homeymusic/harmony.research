ratios <- tibble::tibble(
  # Up ratios
  up.numerator =     c(1,16,9,6,5,4,99,3,8,5,16,15,2),
  up.denominator =   c(1,15,8,5,4,3,70,2,5,3, 9, 8,1),
  # Down ratios
  down.numerator =   rev(.data$up.denominator),
  down.denominator = rev(.data$up.numerator),
)
