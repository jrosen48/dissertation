#! '/Users/joshuarosenberg/Google Drive/1_Research/dissertation/rosenberg-diss/0_render.R'

# rendering PDF
rmarkdown::render_site(output_format = 'bookdown::pdf_book', encoding = 'UTF-8')

# rendering site
# rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')

# fixing text
source("fix_tex.R")

# copying cached files
if (!dir.exists("docs/rosenberg-dissertation_files")) dir.create("docs/rosenberg-dissertation_files")

file.copy("_bookdown_files/rosenberg-dissertation_files",
          "docs", recursive=TRUE)

# updating github
system("git status")
system("git add *")
system("git add -u")
system("git commit -m 'update diss'")
system("git push")
