dim(mdata)
# PROBLEM: not the expected number of rows

###### Where is the problem?
dim(read.csv(files[1], encoding = "latin1")) # PROBLEM (403964) SHOULD BE MORE
dim(read.csv(files[2], encoding = "latin1")) # Fine
dim(read.csv(files[3], encoding = "latin1")) # Fine
dim(read.csv(files[4], encoding = "latin1")) # Fine
dim(read.csv(files[5], encoding = "latin1")) # Fine
dim(read.csv(files[6], encoding = "latin1")) # Fine

### Different fields?
# Check the number of fields in each line
fields <- count.fields(files[1], sep = ",")

# Find lines with an inconsistent number of fields
inconsistent_lines <- which(fields != max(fields))

# Output inconsistent lines for inspection
inconsistent_lines

test <- read.csv(files[1], encoding = "windows-1252")
dim(test)
test <- read.csv(files[1], encoding = "latin1")
dim(test)
lol <- read.csv(files[2], encoding = "latin1")
colnames(test) == colnames(lol)
# RESULT: no problem with column names or number

### Bad lines?
# Read the file line by line
lines <- readLines(files[1], warn = TRUE, encoding="latin1")
linesnoinsidecommas <- gsub("\\\\,", ";", lines)
#raw <- gsub("\\\"\\\"", "\"", lines)
lines2 <- read.table(text=raw, header = TRUE, sep=",")

# Loop through each line to detect where issues might arise
for (i in seq_along(lines[1:5])) {
  tryCatch({
    # Try parsing each line individually
    temp <- read.csv(text = lines[i], stringsAsFactors = FALSE, encoding="latin1")
  }, warning = function(w) {
    message("Warning in line ", i, ": ", conditionMessage(w))
  }, error = function(e) {
    message("Error in line ", i, ": ", conditionMessage(e))
  })
}
# RESULT: Plenty of invalid multibyte strings at different positions in the line

lines[2:4]

# TESTS
find_offending_character <- function(x, maxStringLength=256){  
  print(x)
  for (c in 1:maxStringLength){
    offendingChar <- substr(x,c,c)
    print(offendingChar) #uncomment if you want the indiv characters printed
    #the next character is the offending multibyte Character
  }    
}

lapply(lines[1], find_offending_character)
lapply(lines[4], find_offending_character)

test <- read.csv(files[1], fileEncoding = "latin1", encoding= "latin1")
dim(test)

lol <- read.csv(files[2], encoding = "latin1")

text <- "priorit\xe9 2"
# Convert the encoding to UTF-8
corrected_text <- iconv(text, from = "latin1", to = "UTF-8")
print(corrected_text)  # Should output "priorité 2"

options(encoding = "windows-1252")
test <- read.csv(files[1], fileEncoding = "latin1", quote="\"", sep=",")
dim(test)

testtext <- read.table("W:/01_Services/SCR_Informations Patrimoine Naturel/00_Projects/2024_LetzFindThem_DATA/-1997TESTING.txt", header=TRUE, sep="\t", encoding = "latin1")



###### Redo of the original idea after correction
files <- list.files("W:/01_Services/SCR_Informations Patrimoine Naturel/00_Projects/2024_LetzFindThem_DATA", full.names = TRUE)[c(1, 2, 3, 4, 5, 7)]
mdata <- do.call(rbind, lapply(files, function(x) read.csv(x, encoding="latin1")))
dim(mdata)

testt <- fread("W:/01_Services/SCR_Informations Patrimoine Naturel/00_Projects/2024_LetzFindThem_DATA/-1997TESTING.csv", encoding = )

