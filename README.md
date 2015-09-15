neurone
=======

`embla` is an [R-package](https://www.r-project.org/) for reading physiologic data recorded with an [Embla device](http://www.natus.com/index.cfm?page=products_1&crid=998).

Installation
------------
To install the `embla` package in R, proceed as follows in R.

First install the `devtools`-package
```
   install.packages("devtools")
```

You can now install the `embla` package:
```
   install_github("bwrc/embla-r")
```

Usage
-----
Read all all ebm-files in a directory containg multiple signals, one signal per ebm-file:
```
library(embla)
datapath <- "/tmp/my_recording/"
recording <- read.ebm(datapath)
```

Read one signal from an ebm-file:
```
datafile <- "/tmp/ecg.ebm"
recording <- read.ebm(datafil)
```

Read two signals from a directory  directory containg multiple signals, one signal per ebm-file:

```
datapath <- "/tmp/my_recording/"
recording <- read.ebm(datapath, channels = c("ecg", "Fz", "Pz"))
```

You can also specify additional arguments to `read.ebm`, such as the offset offset in seconds from the beginning of the file where to start reading data and how much data that should be read (using the parameter `data.length`).