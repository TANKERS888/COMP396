supers <- data.frame(
  first=c("Bruce", "Hal", "Clark", "Diana"),
  last=c("Wayne", "Jordan", "Kent", "Prince"),
  is=c("Batman", "Green Lantern", "Superman", "Wonder Woman")
)
pystr_format("{first} {last} is really {is} but you shouldn't call them {first} in public.", supers)