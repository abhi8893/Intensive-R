# cat function
# It's better to use cat instead of print
# as print has it's output numbered.

# You can specify multiple separators
x <- c("My", "name", "is",":", "Abhishek", "Bhatia")
cat(x, sep=c(rep(" ", 3), "\n\t", " "))
