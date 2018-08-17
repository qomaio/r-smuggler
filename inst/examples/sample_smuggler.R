library(rhli)
library(qoma.smuggler)

stopifnot(open_hli())

dbname <- paste(Sys.getenv("FAME"),"util","driecon",sep="/")
cat(dbname,sep="\n")
famedata <- read_fame(dbname)

cat(famedata$get_meta('GDP'))

write_fame("mydb",famedata)

status <- Integer(-1)
cmd <- Character(paste(
  "open<acc read> mydb; ",
  "output<acc over> tmp.txt; ",
  "whats gdp;",
  "output terminal; ",
  "close mydb; ",  
  sep=""))
cfmfame(status, cmd)
cat(readLines("tmp.txt"), sep = '\n')

close_hli()

file.remove("mydb.db")
file.remove("tmp.txt")

unloadNamespace('qoma.smuggler')
unloadNamespace('rhli')
