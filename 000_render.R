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
# 2. add this below table 4.7, before landscape tags: \begin{flushleft}\emph{Note}. *: \emph{p} \textless{}.05; +: p \textless{} .10\end{flushleft}
# 3. replace (B (SE)) with the proper LaTeX: $\beta(\textit SE)$
# 3.5 cors - & Pre-int. & Cog. eng. & Beh. eng. & Aff. eng. & Chall. & Comp. & Ask. & Obs. & Gen. & Mod. & Com. & 3em
# 3.75 - 4.7, 3em
# 4. add subscripts and move note up:
# \begin{flushleft}\emph{Note}. The subscripts indicate the mean values subscripts
# indicating values that were not statistically significantly different on
# the basis of the ANOVA analyses.\end{flushleft}
# Universally low & 1.550 & 1.766 & 1.538 & 1.775 & 2.327\\
# Only behavioral & 3.292 & 2.484$_2$ & 1.641 & 2.132$_4$ & 2.778$_5$\\
# Only affective & 1.670 & 2.516$_2$ & 3.330 & 2.191$_4$ & 2.954$_5$\\
# All moderate & 3.060 & 2.826 & 3.110 & 2.489 & 2.953$_5$\\
# Eng. and comp. but not chall. & 3.909$_1$ & 3.487 & 3.822$_3$ & 1.276 & 3.604$_6$\\
# Full & 3.959$_1$ & 3.801 & 3.881$_3$ & 3.742 & 3.631$_6$\\

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
