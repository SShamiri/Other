ageGroup = function(Age) {
  labs = c(paste(seq(0, 80, by = 5), seq(0 + 5 - 1, 85 - 1, by = 5), sep = "-"), paste(85, "+", sep = ""))
  as.character(cut(Age, breaks = c(seq(0, 85, by = 5), Inf), labels = labs, right = FALSE))
}

age = function(dob, age.day = today(), units = "years", floor = TRUE) {
  require('lubridate')
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}


ageAs = function(dob, age.as, units = "years", floor = TRUE) {
  require('lubridate')
  calc.age = interval(dob, age.as) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}