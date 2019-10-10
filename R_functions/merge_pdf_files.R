
## merging pdf files

library(tabulizer)

# files to merge (path)
pdfFiles = c('J:\\My Documents\\SAS_SQL.pdf','J:\\My Documents\\cindy_lake_safe_in_a_nutshell.pdf')

# merge pdf
mergedFile = file.path('J:\\My Documents', "merged.pdf")
merge_pdfs(pdfFiles, mergedFile)



