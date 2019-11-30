# Implement a function which takes Pathname in windows format and outputs R format
# (raw string) C:\Users\abhi -> C:/Users/abhi
# Stackoverflow: https://t.ly/PZWY8

# TODO: Add example as part of a functiom
win2linux <- function(win.path){
  gsub("\\\\", "/", win.path)
}

win.path <- "C:\\Users\\abhi"
win2linux(win.path)

