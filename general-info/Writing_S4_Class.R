# Writing S4 Classes
# Ref: Pg 223 Art of R Programming

# S4 class is "Formal class"
# Explicitly create a class


setClass("employee",
         representation(
           name="character",
           salary="numeric"
         ))

setClass("programmer",
         representation(
           name="character",
           salary="numeric",
           prog.lang="character"
         ))

# gender is not allowed
new("programmer",
    name="Abhishek",
    salary=50000,
    gender="M")

# If an attribute is not specified, the associated class contructor with
# no arguments is returned.
# i.e. the class of prog.lang is defined to be "character"
# so if no value is passed, character() is assigned to prog.lang
new("programmer",
    name="Abhishek",
    salary=50000)

prgrmr <- new("programmer",
           name="Abhishek",
           salary=50000,
           prog.lang=c("R", "Python"))

# Referencing attributes
slot(prgrmr, "name")
prgrmr@name

# Referencing non-existent attributes raises error
prgrmr@gender

# You can't assign a not allowed attribute to an object of S4 class type
prgrmr@gender <- "M"

# You can change the value of an attribute though
prgrmr@salary <- 55000
slot(prgrmr, "salary") <- 60000


# "show" is S4's analog of S3's generic "print"
setMethod("show", "programmer",
          function (object){
            prog.lang <- object@prog.lang
            num.lang <- length(prog.lang)
            if (num.lang == 0){
              prog.msg <- "no languages"
            } else if (num.lang == 1){
              prog.msg <- paste("only", prog.lang)
            } else{
              prog.msg <- do.call(paste, 
                             c(
                               as.list(prog.lang[1:(length(prog.lang)-1)]),
                               list(sep=", "))
                             )
              prog.msg <- paste(prog.msg, "and", prog.lang[length(prog.lang)])
            }
            
            cat(object@name, "has a salary of", object@salary, "and", 
                "knows", prog.msg)
          })


prgrmr

str(prgrmr)
attributes(prgrmr)
