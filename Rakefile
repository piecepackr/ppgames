desc "Build files for packaging"
task :default do
    sh 'Rscript -e "devtools::document()"'
    sh 'pandoc README.rst -o README.md'
    sh 'pandoc README.rst -o README.html'
    # sh 'Rscript -e "knitr::knit(\"README.Rmd\")"'
    # sh 'pandoc README.md -o README.html'
    sh 'Rscript -e "pkgdown::build_site()"'
end
