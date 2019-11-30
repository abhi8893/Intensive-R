# A bit about summary function
# Ref: Read Pg 15 <- The Art of R Programming 

# When summary is called on an object R searches for
# the apt summary functions for that datatype
# For e.g. on output of 
#   lm() <- summary.lm is used
#   data.frame() <- summary.data.frame is used
# summary is a generic function and the
# vector of all summary family of functions is:
methods(summary)

# Similiarly for plot
methods(plot)

# Basically an instance of an S3 object is just a list with an extra attribute
# of the class which when identified by the plot or summary function
# calls the apt function for that instance depending on its class
