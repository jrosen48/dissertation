#! '/Users/joshuarosenberg/Google Drive/1_Research/dissertation/rosenberg-diss/000_render.R'

# 1. render PDF
rmarkdown::render_site(output_format = 'bookdown::pdf_book', encoding = 'UTF-8')

# 1b. render site
# bookdown::serve_book()

# 2. fix text
source("fix_tex.R")

# 3. copy cached files
if (!dir.exists("docs/rosenberg-dissertation_files")) dir.create("docs/rosenberg-dissertation_files")
file.copy("_bookdown_files/rosenberg-dissertation_files", "docs", recursive=TRUE)

# 4. convert tex to PDF
# make two figures for profiles be in the figure tag
# \begin{flushleft}\emph{Note}. *: \emph{p} \textless{}.05; +: p \textless{} .10\end{flushleft}
# replace (B (SE)) with the proper LaTeX
# add subscripts
# TukeyHSD(x3) # for aff, full and eng and compt but not challenged don't differ, rest do
# TukeyHSD(x2) # for beh, full and eng and compt but not challenged don't differ, rest do
# TukeyHSD(x1) # for cog, only affective and only beh don't differ
# TukeyHSD(x4) # for chall, only affective and only beh don't differ
# TukeyHSD(x5) # for comp, only affective and only beh, all moderate and only affective, and full and eng and compt but not challenged don't differ

system("cd docs; pdflatex rosenberg-dissertation_mod.tex")

# 5. convert tex to .docx
# system("cd docs; pandoc rosenberg-dissertation_mod.tex --reference-docx=rosenberg-template.docx -s -o rosenberg-dissertation_mod.docx")
rmarkdown::render_site(output_format = 'bookdown::word_document2', encoding = 'UTF-8')

# 6. clean up
source("0_clean-up.R")

# 7. update github
system("git status; git add *; git add -u")
system("git commit -m 'update diss'")
system("git push")
