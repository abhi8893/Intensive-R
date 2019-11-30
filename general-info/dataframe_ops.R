all2006 <- read.csv('../data/artofr_data/2006.csv.short', 
                    header=T, as.is=T, na.strings = "")
is.yearly <- all2006$Wage_Per == 'Year'
rlstic.wage <- all2006$Wage_Offered_From > 20000
prv.wage.is_yearly <- all2006$Prevailing_Wage_Amount > 200

df <- all2006[is.yearly & rlstic.wage & prv.wage.is_yearly, ]
df$rat <- df$Wage_Offered_From/df$Prevailing_Wage_Amount

# NOTE: No need to write full string to match with grep
se <- df[grep("Software Engineer", df), ]
prg <- df[grep("Programmer", df), ]
ee <- df[grep("Electronics Engineer", df), ]

makecorp <- function(corpname){
  t <- df[df$Employer_Name == corpname, ]
  return(t)
}

corplist <- c('MICROSOFT CORPORATION', 'ms', 'INTEL CORPORATION', 'intel',
              'SUN MICROSYSTEMS')
