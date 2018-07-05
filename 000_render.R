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

# 1. make two figures for profiles be in the same figure tag
# 2. add this below table 4.7, before table tags: \begin{flushleft}\emph{Note}. *: \emph{p} \textless{}.05; +: p \textless{} .10\end{flushleft}
# 3. replace (B (SE)) with the proper LaTeX: \beta(\textit SE)
# 4. add subscripts:
# Universally low & 1.550 & 1.766 & 1.538 & 1.775 & 2.327\\
# Only behavioral & 3.292 & 2.484$^3$ & 1.641 & 2.132$^4$ & 2.778$^5$\\
# Only affective & 1.670 & 2.516$^3$ & 3.330 & 2.191$^4$ & 2.954$^5$\\
# All moderate & 3.060 & 2.826 & 3.110 & 2.489 & 2.953$^5$\\
# Eng. and comp. but not chall. & 3.909$^1$ & 3.487 & 3.822$^1$ & 1.276 & 3.604$^6$\\
# Full & 3.959$^1$ & 3.801 & 3.881$^1$ & 3.742 & 3.631$^6$\\

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
