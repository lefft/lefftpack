
# `lefftpack::`
<hr>

A handful of R utility functions that make my life easier. 

Some are probably useful for any R user (esp. `lazy_setup()` and `quiet_attach()`); others are idiosyncratic to the kind of work I do.

Feedback welcome + appreciated! 

-[tim leffel](http://lefft.xyz)


### basic usage 
<hr>

To install the package:
```r
devtools::install_github("lefft/lefftpack")
```

To attach the package (i.e. make all functions directly available in a session):
```r
library("lefftpack")
```

Or after install, you can use just a single function with `lefftpack::`, as in: 
```r
# e.g. to attach a handful of common packages and set a nice plot theme
lefftpack::lazy_setup()

# e.g. to quickly and quietly attach many packages:
lefftpack::quiet_attach("quanteda","tm","lme4","lmerTest", shhh="failonly")
```	

### misc. notes
<hr>

The name of the `lazy_setup()` function is meant to evoke "sloth", not "lazy evaluation" -- argument defaults are pre-set to attach a handful of useful packages (the ones I use most) and set a nice `ggplot2::` theme. The default behavior of `lazy_setup()` produces the console message below:

```r
# (to prevent console output, set `show_message=FALSE`)
lefftpack::lazy_setup()
```
```
>> these packages are now attached: 
     lefftpack reshape2 ggplot2 magrittr dplyr
>> custom `ggplot2::` plot theme is now set
```

You can also pass `lazy_setup()` a vector of (quoted) package names via the `packages` parameter, in which case it'll just attach the ones you give it. 

```r
lefftpack::lazy_setup(c("dplyr","quanteda","tm","ggplot2"))
```

To prevent *all* console messages, just set `quiet=TRUE`. Though note that you'll still get a warning if you've tried to attach a non-existent package -- imo that's better than the aggressive error `library("blahh")` would normally give you by default! :p)


