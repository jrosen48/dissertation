#! '/Users/joshuarosenberg/Google Drive/1_Research/dissertation/rosenberg-diss/0_render.R'

# 1. render PDF
rmarkdown::render_site(output_format = 'bookdown::pdf_book', encoding = 'UTF-8')

# 1b. render site
# rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')

# 2. fix text
source("fix_tex.R")

# 3. copy cached files
if (!dir.exists("docs/rosenberg-dissertation_files")) dir.create("docs/rosenberg-dissertation_files")
file.copy("_bookdown_files/rosenberg-dissertation_files", "docs", recursive=TRUE)

# 4. convert tex to PDF
system("cd docs; pdflatex rosenberg-dissertation_mod.tex")
# system("find -name rosenberg-dissertation_mod.tex -execdir pdflatex {} \;")

# 5. update github
system("git status; git add *; git add -u")
system("git commit -m 'update diss'")
system("git push")

# 6. clean up
source("0_clean-up.R")
