## Test environments

- Ubuntu Linux 16.04 LTS, R-release, GCC
- Fedora Linux, R-devel, clang, gfortran
- Debian Linux, R-devel, GCC ASAN/UBSAN
- x86_64-w64-mingw32 (64-bit) / R 3.3.2

- https://builder.r-hub.io/status/rtext_0.1.19.tar.gz-38b00c910a24473fbe20842df71be304
- https://builder.r-hub.io/status/rtext_0.1.19.tar.gz-58a85b56e335413ea0af1dbc85acacd9
- https://builder.r-hub.io/status/rtext_0.1.19.tar.gz-83f730c49e0e4bf8a72f418fabe1b62f
- http://builder.r-hub.io/status/rtext_0.1.19.tar.gz-9b67d31ad4f64eadb9f205106e1d0f76


## R CMD check results

Linux builds: 

no errors, no warnings, and no notes except notes stating that this is a new submission


Windows build:

while local Windows build on Win10/64Bit R 3.3.1 succeeds the above mentioned build on Windows Server complaints:

- Note: found 2 marked UTF-8 strings
- Note: Namespaces in Imports field not imported from: 'RSQLite' 'stats' All declared Imports should be used.

Invastigating the first note locally via tools::showNonASCIIfile() on files in R and data folder did result in anything.

The second note is strange since RSQLite are explicitly used throughout the package via things like: stats::agregate... or RSQLite::dbWriteTable...


I therefore think these complaints by one build only are artifacts that can be ignored. 
