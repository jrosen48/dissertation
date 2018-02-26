#! '/Users/joshuarosenberg/Google Drive/1_Research/dissertation/rosenberg-diss/0_render.R'

# 1. rendering PDF
rmarkdown::render_site(output_format = 'bookdown::pdf_book', encoding = 'UTF-8')

# 1b. rendering site
# rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')

# 2. fixing text
source("fix_tex.R")

# 3. copying cached files
if (!dir.exists("docs/rosenberg-dissertation_files")) dir.create("docs/rosenberg-dissertation_files")

file.copy("_bookdown_files/rosenberg-dissertation_files",
          "docs", recursive=TRUE)

#########################################
### 4. MANUALLY RENDER THE FILE HERE!!!
#########################################

# 5. updating github
system("git status")
system("git add *")
system("git add -u")
system("git commit -m 'update diss'")
system("git push")

# 6 (optional). create word doc
# cd '/Users/joshuarosenberg/Google Drive/1_Research/dissertation/rosenberg-diss/docs'
# pandoc rosenberg-dissertation_mod.tex -s -o rosenberg-dissertation.docx


