# pbmcapply: tracking the progress of mc*apply in R.

A package helps you track and visualize the progress of parallel version of vectorized R functions (mc*apply).

**Notice:** This package works on *nix (Linux, Unix such as macOS) only due to the lack of fork() functionality, which is essential for mcapply, on Windows.

## Installation

### Stable release on CRAN 

Version: ![CRAN version](http://www.r-pkg.org/badges/version/pbmcapply) ![Download count](http://cranlogs.r-pkg.org/badges/grand-total/pbmcapply)

Package page: https://cran.r-project.org/web/packages/pbmcapply 

Install `pbmcapply` by pasting this command in your R console:

```
install.packages(pbmcapply)
```

### Development release on Github 

Version: [![GitHub release](https://img.shields.io/github/release/kvnkuang/pbmcapply.svg?maxAge=2592000)]()

Project page: https://github.com/kvnkuang/pbmcapply

Build `pbmcapply` by pasting these commands in your R console:

```
library(devtools)
install_github("kvnkuang/pbmcapply")
```

## License:

[MIT license](https://opensource.org/licenses/MIT)
