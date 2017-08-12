### `lefftpack::`  
<hr>

A handful of R utility functions that make my life easier. 

Some are probably useful for any R user (esp `quiet_attach()`); others are idiosyncratic to the kind of work I do.

Feedback welcome + appreciated!

-[tim leffel](http://lefft.xyz)


### usage 
<hr>

```r
# install the package 
devtools::install_github("lefft/lefftpack")
```

```r
# make all functions available in session
library("lefftpack")
```

```r
# just use `quiet_attach()` 
# (you get a message if you try something that's not installed)
lefftpack::quiet_attach("dplyr", "magrittr", "ggplot2", shhh="failonly")
```

