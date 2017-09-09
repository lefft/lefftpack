
# `lefftpack::`
<hr>

A handful of R utility functions that make my life easier. 

Some are probably useful for any R user (esp. `lazy_setup()` and `quiet_attach()`); others are idiosyncratic to the kind of work I do.

Also contains a few language-related datasets for experimenting with language/text analysis techniques.

Feedback welcome + appreciated! 

-[tim leffel](http://lefft.xyz)


### basic usage 
<!-- <hr> -->

To install the package:
```r
devtools::install_github("lefft/lefftpack")
```

To load and attach the package (i.e. make all functions directly available in a session):
```r
library("lefftpack")
```

Or you can use `lefftpack::` to call just a single function without attaching the whole package, as in: 
```r
# e.g. to load and attach a handful of common packages + set a nice plot theme
lefftpack::lazy_setup()

# e.g. to quickly and quietly load and attach many packages:
lefftpack::quiet_attach("quanteda","tm","lme4","lmerTest", shhh="failonly")
```	


### bundled data
<!-- <hr> -->

The package also contains three datasets, each in a different format. I find them useful for demonstrating/practicing/experimenting with various linguistic analysis and NLP techniques. 

Here's some basic info about them (see documentation for more detail): 

- **`lefftpack::dataset_word_freq`**: <br> data frame with basic info about the 5k most frequent words in COCA (English) [[source](https://www.wordfrequency.info/sample.asp)]
- **`lefftpack::text_una_manifesto`**: <br> a length-314 character vector containing the full text of the unabomber manifesto (~35k total words)
- **`lefftpack::text_googmem`**: <br> a single string (length-1 character vector) containing full text of james damore's infamous "google memo" from 2017


**NB.** The raw text datasets both have a fairly formal style, and are actually interestingly similar to one another linguistically (more than you might guess, and in certain specific ways, at least). This is the *only* reason they're included. 


### misc. notes
<!-- <hr> -->

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

