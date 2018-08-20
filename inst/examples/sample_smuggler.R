library(rhli)
library(qoma.smuggler)

stopifnot(open_hli())

dbname <- paste(Sys.getenv("FAME"),"util","driecon",sep="/")
cat(dbname,sep="\n")
famedata <- read_fame(dbname)

cat(famedata$get_meta('GDP'))

txtfile <- file.path(tempdir(),"tmp.txt")
dbfile <- file.path(tempdir(),"tmp.db")
write_fame(dbfile,famedata)

status <- Integer(-1)
cmd <- Character(paste(
  "open<acc read>\"",dbfile,"\" as db; ",
  "output<acc over>\"",txtfile,"\"; ",
  "whats gdp;",
  "output terminal; ",
  "close db; ",  
  sep=""))
cfmfame(status, cmd)
cat(readLines(txtfile), sep = '\n')

close_hli()

file.remove(dbfile)
file.remove(txtfile)

unloadNamespace('qoma.smuggler')
unloadNamespace('rhli')
