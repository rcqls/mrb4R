# Ruby for R

This is an attempt to embed the ruby language in R.

## Install

Clone this git and

	R CMD INSTALL rb4R

## Example (very basic)
```{.R execute="false"}
require(rb4R)
.rbBegin()
.rb("[1,3,2]") -> a
a
a*2
sin(a)
.rbEnd()
```
