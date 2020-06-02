# feisr 1.0.2

### Major Changes

- Included dplyr `bind_rows()` for faster tranformation of large data (now depends on dplyr (>= 1.0.0)).

- Added vignette.

- Bug fix `feis()`, `feistest()`, `bsfeistest()`: Standard errors can be computed even if some coefficients are NA.

- Added `terms` option to `feistest()` and `bsfeistest()`: Allows to perform Hausman / Chi_sq test on subset of coefficients.

- Added `tol` option to `feis()` for detecting linear dependencies in slopes.

- Drop export of extract. Method now available in texreg (>= 1.37.1). Added test to keep compatibility.


### Minor Changes

- Manual updated, minor corrections.

- Added citation.

- Corrected linking / documentation of S3 methods.
