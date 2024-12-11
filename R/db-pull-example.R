## Example pulling data from a .Mdb file.

db.path = "2023 Pre-Season Chinook DB.mdb"

con.base = DBI::dbConnect(
  drv = odbc::odbc(),
  .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", 
                              db.path, ";"))
## list available tables
dbListTables(con.base)
## use query to get columns of interest from table of interest
## NOTE: dbGetQuery can't do complex SQL operations.
fish.id = dbGetQuery(con.base, "SELECT FisheryID, FisheryName FROM Fishery")
dbDisconnect(con.base)