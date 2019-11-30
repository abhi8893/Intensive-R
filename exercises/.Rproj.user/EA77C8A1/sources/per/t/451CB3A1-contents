# Writing S3 Classes
# Ref: Pg 214 Art of R Programming

emp <- list(name="Abhishek", salary=5e4)
class(emp) <- "employee"
attributes(emp)

# R (using UseMethod("print")) tries to find print.employee method of the class,
# when it does not find it resorts to printing is as a list only.
# S3 class objects are just lists
print(emp)
is.list(emp)
str(emp)

print.employee <- function(emp){
  cat(emp$name, "\n")
  cat("salary:", emp$salary, "\n")
}

print(emp)

# Inheritance 
prgrmr <- list(name="Abhishek", salary=5e4, prog.lang=c("R", "Python"))
class(prgrmr) <- c("programmer", "employee")

# R first tries to find print.programmer,
# when it does not find it resorts to print.employee
print(prgrmr)

print.programmer <- function(prgmr){
  cat(prgrmr$name, "\n")
  cat("salary:", prgrmr$salary, "\n")
  cat("Programming languages:", 
      do.call(paste, c(as.list(prgrmr$prog.lang), list(sep=", "))))
}

print(prgrmr)

# This is a generic function
# which will dispatch the call to "f" method of the object of any class
f <- function(obj){
  UseMethod("f")
}
emp1 <- emp
# Error since we haven't implemented 
# f.employee function
# i.e. f method of class employee
f(emp1)

f.employee <- function(emp){
  var.name <- substitute(emp) 
  cat("Calling f.employee method on", var.name)
} 

f(emp1)

# Disadvantages
# You can create a list with any attributes and class employee to it
# You might not want to have prog.lang in employee object but R won't
# raise any error

emp <- list(name="Abhishek", salary=5e4, prog.lang=c("R", "Python"))
class(emp) <- "employee"

# suppose you misspelled prog.lang as prg.lang, then also no error is raised
prgrmr <- list(name="Abhishek", salary=5e4, prg.lang=c("R", "Python"))
class(prgrmr) <- c("programmer", "employee")

# Referencing non-existent attributes return NULL (no error)
prgrmr$gender

# Since it's just a list you can assign any attribute to it w/o error
prgrmr$gender <- "M"
