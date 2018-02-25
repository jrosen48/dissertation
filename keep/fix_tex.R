f_orig <- readLines("docs/rosenberg-dissertation.tex")

start_good_content <- grep("placemarker", f_orig) + 1
end <- length(f_orig)

f_good <- f_orig[start_good_content:end]

f_rep <- readLines("correct-front-matter.tex")

f_out <- c(f_rep, f_good)

file_conn <-file("docs/rosenberg-dissertation_mod.tex")
writeLines(f_out, file_conn)
close(file_conn)
