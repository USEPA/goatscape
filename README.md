# goatscape

This repository hosts the development version of the `goatscape` R package.  The purpose of `goatscape` is to provide tools to summarize various data sets across an input landscape polygon and provide it in a [tidyverse](https://tidyverse.org) friendly format.  This is a common data aggregation step for many ecological studies, yet standarized tools do not exist in R.

# What's in a name?

While the name may seem odd, it isn't completely non sensical.  [Goatscaping](https://blog.epa.gov/blog/2016/06/goatscaping/) is actually a thing, and refers to the use of goats to remove excess vegetation so that what is left is more manageable and tidy (see what I did there?).  Similarly, the `goatscape` package provides tools that take several large datasets and aggreagates those datasets into more managebale summaries in a tidy format.

And we would be remiss if we didn't credit [@maelle](https://github.com/maelle) and [@bhive01](https://github.com/bhive01) for help with coming up with the name.  The [twitter thread](https://twitter.com/jhollist/status/910530041011949568) that led to this name is entertaining

# Installation

To install the development version:

```
install.packages("devtools")
library("devtools")
install_github("jhollist/goatscape")
library("goatscape")
```

To install from CRAN (currently Not On CRAN):

```
install.packages("goatscape")
library("goatscape")
```

# Basic use

The plan is to provide these tools, at least initially, for US based landscapes.  The datasets we hope to include, via APIs,  are:

- Census
- National Land Cover Dataset
- Elevation

# EPA Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.