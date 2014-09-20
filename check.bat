perl -I../circlize/lib -MR::Comment2Man -e "R::Comment2Man->draft('R')"

cd ..

R CMD REMOVE GlobalOptions
R CMD build --compact-vignettes=gs+qpdf GlobalOptions
R CMD check GlobalOptions_0.0.2.tar.gz
R CMD INSTALL GlobalOptions_0.0.2.tar.gz