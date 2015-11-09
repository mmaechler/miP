# miP: Multiple Imputation Plots - R package using mainstream CRAN packages `mice`, `mi`, `Amelia`,..

Plots to visualize missing data and datasets that were produced by Multiple Imputation.
The goal is is to visualize MI objects produced by the R packages `mi`, `mice`
or `Amelia` II to provide a better understanding of the imputed data and to
evaluate the MI process.

This originated as result of Paul Brix' master thesis with Anthony Unwin in
Augsburg in 2011.  As the package was archived in Spring 2015, and it
seemed useful, Martin Maechler started maintaining it in November 2015.

Slightly more generally, the goal is using mainstream and widely used CRAN
packages for missing values' data analysis, and more easily compare their methods,
notably **M**ultiple **I**mputation.

Currently, results from `mice`, `mi`, and `Amelia`  are (partly) supported.
