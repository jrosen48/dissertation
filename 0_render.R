#! '/Users/joshuarosenberg/Google Drive/1_Research/dissertation/rosenberg-diss/0_render.R'

# 1. rendering PDF
rmarkdown::render_site(output_format = 'bookdown::pdf_book', encoding = 'UTF-8')

# 1b. rendering site
rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')

# 2. fixing text
source("fix_tex.R")

# 3. copying cached files
if (!dir.exists("docs/rosenberg-dissertation_files")) dir.create("docs/rosenberg-dissertation_files")

file.copy("_bookdown_files/rosenberg-dissertation_files", "docs", recursive=TRUE)

#########################################
### 4. MANUALLY RENDER THE FILE HERE!!!
#########################################

# this doesn't work - need a way to render to .tex to PDF with the class
# ! Undefined control sequence.
# \@bspredate ...\[\baselineskip ]\msu@fieldofstudy
# ~\msu@fieldseparator ~\msu...
# l.75 \begin
#
# pandoc: Error producing PDF
# system("cd '/Users/joshuarosenberg/Google Drive/1_Research/dissertation/rosenberg-diss/docs'")
# system("pandoc rosenberg-dissertation_mod.tex -s --variable documentclass=msu-thesis -o rosenberg-dissertation_mod.pdf")

# 4b (optional). create word doc
# system("cd '/Users/joshuarosenberg/Google Drive/1_Research/dissertation/rosenberg-diss/docs'")
# system("pandoc rosenberg-dissertation_mod.tex -s --reference-docx=rosenberg-template.docx -o  rosenberg-dissertation.docx")

# 5. updating github
system("git status")
system("git add *")
system("git add -u")
system("git commit -m 'update diss'")
system("git push")

# 6. Cleaning up

source("0_clean-up.R")
