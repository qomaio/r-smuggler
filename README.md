# Qoma Smuggler

## Prerequisites

* Install R Studio and successfully render an R Markdown page.
* Install FAME and successfully enter and exit the FAME 4GL environment.
* `install.packages("devtools")`
    * this is temporary to facilitate `install_github()` below
    * we are submitting this package to CRAN
    * when this package is on CRAN `devtools` will not be required for installation
    
## Installation and testing

From an R console,

* `devtools::install_github("qomaio/r-smuggler")`
* `source(file.path(find.package("qoma.smuggler"),"examples/sample_smuggler.R"))`

From an R console inside R Studio,

* `file.edit(file.path(find.package("qoma.smuggler"),"examples/0_example_hello_utils.Rmd"))`
* `file.edit(file.path(find.package("qoma.smuggler"),"examples/1_example_read_fame_db.Rmd"))`
* `file.edit(file.path(find.package("qoma.smuggler"),"examples/2_example_access_4gl.Rmd"))`
* `file.edit(file.path(find.package("qoma.smuggler"),"examples/3_example_write_fame_db.Rmd"))`
* `file.edit(file.path(find.package("qoma.smuggler"),"examples/4_example_write_fame_db.Rmd"))`
* `file.edit(file.path(find.package("qoma.smuggler"),"examples/5_sample_smuggler.Rmd"))`
    * from the editor, hit the `Knitr` button to render the R Markdown example
    * PDF output is available [here](inst/examples)

On first use, you will be directed to a website to obtain a `QOMA_PIN` which will entitle you to use the RHLI for the duration of your FAME license.

