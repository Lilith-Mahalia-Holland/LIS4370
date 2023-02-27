is.object(mtcars) & !isS4(mtcars)
# Based off of this it is apparent that mtcars is an s3 object
# this could also be determined simply by acknowledging that any dataframe uses $ to address variables
mtcars$mpg
# The ability to use $ instead of @ also shows that mtcars and for that matter all dataframes in R are s3.




# The easiest way to determine if an object is s3 or not would be to do
# is.object(object) & !isS4(object) as if something is an object but not s4 it must be s3
# You can also determine what object system an object is associated with through the use of $ and @ as
# an object can only use one. It's a crude test but rather simple.
# You could also use pryr::otype(object), or str(object) = formal class

# To determine the type an object is you can simply use the function typeof(object) which will return
# list, int, double, bool, etc...

# For s3 a generic function is a function that does not belong to any specific objects or classes and can be tested by
# typing the function name and seeing if UseMethod is called in the function. For s3 any method belong to the classless
# function rather than any specific class or object.
# For s4 a generic function belongs to the class or object and cannot be called directly outside of the class unless
# through a method.

# The largest difference between s3 and s4 would have to be safety as s4 has more checks and also a more rigorous system
# for creating methods and functions that are able to be class dependent.




student <- function(n, a, g){
  if(g>4 || g<0) stop("GPA must be between 0 and 4")
  value <- list(name = n, age = a, gpa = g)
  attr(value, "class") <- "student"
  value
}

grade <- function(obj) {
  UseMethod("grade")
}
grade.student <- function(obj){
  if (obj$gpa > 3.5){
    "A"
  } else if (obj$gpa > 3){
    "B"
  } else if (obj$gpa > 2.5){
    "c"
  } else if (obj$gpa > 2){
    "D"
  } else if (obj$gpa > 0){
    "F"
  } else {
    "Incorrect gpa"
  }
}
grade.default <- function(obj) "Unknown class"
s3 <- student("Bill", 23, 4)
s3
s3$name <- "John"
s3
grade(s3)
ftype(grade)




student <- setClass("student", slots=list(name="character", age="numeric", gpa="numeric"))
setMethod("show",
          "student",
          function(object) {
            if (object@gpa > 4 || object@gpa < 0) stop("GPA must be between 0 and 4")
            cat(object@name, "\n")
            cat(object@age, "\n")
            cat("GPA:", object@gpa, "\n")
          }
         )
setMethod("grade",
          "student",
          function(obj){
            if (obj@gpa > 3.5){
              "A"
            } else if (obj$gpa > 3){
              "B"
            } else if (obj$gpa > 2.5){
              "c"
            } else if (obj$gpa > 2){
              "D"
            } else if (obj$gpa > 0){
              "F"
            } else {
              "Incorrect gpa"
            }
          }
          )
s4 <- new("student", name="Jack", age=23, gpa=3.9)
s4
slot(s4, "name") <- "Bill"
s4
grade(s4)
