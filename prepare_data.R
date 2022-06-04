require(dplyr)
require(stringr)

df = read.csv("C:/code/Datasets/TheOfficeLines/the-office_lines.csv", encoding="UTF-8")

df$Line <- str_replace_all(df$Line, "’", "'")

#removing action descriptions (ex."[talking on phone]")
df$Line <-  str_replace_all(df$Line, "\\[.*?\\][ ]?", "")

write.csv(x = df, file="C:/code/The-Office-Lines-Analysis/lines_prepared.csv",
          fileEncoding = "UTF-8", row.names = FALSE)
