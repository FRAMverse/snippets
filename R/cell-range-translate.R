## Translates excel cell address or range to data frame rows and columns
## 
## Note that this is now included in TAMMsupport; updates to function will live there.

cell_range_translate = function(x, #excel cell address or cell range, e.g. "D6" or "D6:BG27"
                                expand = TRUE, #if TRUE, provides the row and 
                                #            column of all cells in a cell range. 
                                #            If false, just the start and end cells
                                start = "A1"){ #if the spreadsheet was read in 
                                #starting with a cell other than A1, provide here 
                                # to translate excel address to appropriate row/col
  stopifnot(length(x)==1)
  stopifnot(class(x) == "character")
  cells = strsplit(x, ":")[[1]] 
  cols.let = strsplit(gsub("[0-9]*", "", cells), "")
  rows = as.numeric(gsub("[A-Z]*", "", cells))
  cols.num = numeric(length(cols.let))
  for(i in 1:length(cols.let)){
    if(length(cols.let[[i]])==1){ #simple case
      cols.num[i] = which(LETTERS == cols.let[[i]])
    } else { #prefix letter, base 26 counding
      cols.num[i] = 26 * which(LETTERS == cols.let[[i]][1]) +
        which(LETTERS == cols.let[[i]][2])
    }
  }
  
  if(expand & length(cells) == 2){
    res = as.data.frame(expand.grid(row = rows[1]:rows[2], col = cols.num[1]:cols.num[2]))
  } else {
    res = data.frame(row = rows, col = cols.num)
  }
  
  if(start != "A1"){
    offset.vals = cell_range_translate(start)
    res$row = res$row - offset.vals$row+1
    res$col = res$col - offset.vals$col+1
  }
  return(res)
}

cell_range_translate("D6")

cell_range_translate("D6", start = "D6")

cell_range_translate("D6:BG27")

cell_range_translate("D6:BG27", expand = FALSE)
