This is a re-submission

The test failing on ATLAS now runs conditional on QR results, which should resolve the issue.
However, as this is a BLAS specific issue, I cannot check if it is resolved on environments available to me.

Thanks a lot for checking and best wishes
Tobias

## Submission feisr v.1.2.0
This is anew version v.1.2.0 of feisr.

Thank you very much for checking and best wishes, 
Tobias

## Test environments
* local: WIN10 64Bit, R 3.6.3, R 4.0.4, devel
* Travis-CI: Ubuntu 16.04, R 3.6.3, R 4.0.2, devel
* Travis-CI: Mac OS X 10.13.6, R 3.6.3, R 4.0.4

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

R CMD check --as-cran was run on all reverse dependencies locally (insight, texreg). All checks passed.