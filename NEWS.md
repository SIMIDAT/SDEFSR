# Changelog

----

**Version 0.7.0.0**

* Added a new subgroup discovery algorithm: **FuGePSD**.
* New dataset files support: 
    + ARFF files support from *read.keel()* 
    + Conversion from data.frame to *keel* with *keelFromDataFrame()* function.
* Minor optimizations when reading a file.
* Some algorithm optimizations, changed a bit the way of calling the functions **Be aware with this!**
* Fixed problem of the algorithms that returns four files instead of three. Now all the algorithms (except FuGePSD) returns three files.
* Created S3 method to visualize *keel* objects with *print()* and summarize with *summary()*
* **Changes of the UI:**
    + Algorithm selection an parameters now are on the tab panel of the right side of the UI after "Exploratory analysis". We think with this change the user has a better workflow and a better visualitation and organization of the results.
    + Also, the new functionality is available on the GUI, except the conversion from data.frame.
