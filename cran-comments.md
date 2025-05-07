## Resubmission - 1.0.0
The following modifications proposed by Konstanze Lauseker have been implemented:
  * "It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object.Instead of cat() rather use message()/warning() or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console. (except for print, summary, interactive functions) -> R/subpop_children.R, R/subpop_noChildren.R"
    * FIXED: included "interactive" parameter in R/subpop_children.R, R/subpop_noChildren.R functions
  
  * "Please do not install packages in your functions, examples or vignette. This can make the functions,examples and cran-check very slow. -> inst/doc/DESA_Guide.R"
    * FIXED: Removed vignette from this submission, will resubmit with a more complete vignette at a later time.

## Resubmission - 1.0.0
* Removed package name from Title field

## Resubmission - Major version update
* Package name is update from ATQ -> DESA to better represent the functionality and purpose of the package.

## Resubmission - c

* Minor fix addressing the note, 
"'Packages which use Internet resources should fail gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error).'

This needs correction whether or not the resource recovers."


## Resubmission - patch

* Made a minor change to one of the functions after initial submission causing vignette build to fail. Made necessary changes to pass checks
* Fixed a typo in Description as well.

## R CMD check results

0 errors | 0 warnings | 1 note

