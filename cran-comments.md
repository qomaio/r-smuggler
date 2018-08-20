# CRAN comments for qoma.smuggler
## Resubmission 1
CRAN team -

This is a first resubmission of a new package, responding to feedback from Swetlana Herbrandt on intial submission below.

* please write package names, software names and API names in 
single quotes in Title and Description
    * done.
* Please add an URL for 'FAME' in your Description text in the form
<http:...> or <[https:...]https:...>
with angle brackets for auto-linking and no space after 'http:' and 
'https:'.
    * a link was added after the first reference to 'FAME'.  I'm not familiar with this auto-linking syntax.  Please let me know if further modification is required.
* Please add small executable examples in your Rd-files.
    * done.
* Please ensure that your functions do not write by default or in your 
examples/vignettes/tests in the user's home filespace. That is not allow 
by CRAN policies. Please only write/save files if the user has specified 
a directory. In your examples/vignettes/tests you can write to tempdir().
    * Thank you for brining this policy to my attention.  I now only write to a file in tempdir().

Thank you for reviewing my submission!

Regards,

Kevin

---

## Initial submission notes
This is a new package that moves data and analytic commands between R and FAME, a commercial time series analysis environment. qoma.smuggler uses the lower level CRAN package rhli.

For useful operation, FAME is required. 
R Markdown PDF output generated on a system with FAME is available on GitHub https://github.com/qomaio/r-smuggler/tree/master/inst/examples .

## Test environments
* Windows 10, R 3.5.1, x64 + i386
* Debian 9.5, R 3.5.1
* win-builder, R Under development, x64 + i386

## R CMD check results
0 errors | 0 warnings | 0 notes
    
## Downstream dependencies
There are currently no downstream dependencies for this package.