# Taking user input
# Ref: Stackoverflow t.ly/yDG8b

res <- readline("Please enter numbers separated by comma\t")
v <- as.numeric(strsplit(res, ", ")[[1]])

# scan will repeatedly take user input until without entering anything
# enter is pressed
# "" empty string for user input
cat("Please enter numbers.")
scan("")

scan("", what = character())

cat("Please enter numbers separated by comma\t")
scan("", what=character(), sep=",")

# you could have passed F or T as well
# it just checks the class of what you have passed and 
# uses the constructor with the entered user input to make an object
# of that class
scan("", what = logical())

# Let's create an S3 class object, class = "employee"
emp <- list(name="Abhishek", salary=5e4)
class(emp) <- "employee"
str(emp)

print.employee <- function(emp){
  cat(emp$name, "\n")
  cat("salary:", emp$salary, "\n")
}

print(emp)

# Enter details in the same order, i.e. name, salary
(emp.new <- scan("", what=emp))


# Suppose you want the user to input multiple entries and coerce them into
# different data types

dtypes <- list(name=character(), age=integer(), 
               gender=character(), knows.R=logical())

# NOTE: It will show as read 1 record if you terminate after 4 entries
person <- scan("", what = dtypes)

# If you keep entering if will start from the first entry and append to that
# if not all entries are entered for the second person it will resort to NA

people <- scan("", what = dtypes)

as.data.frame(people)
