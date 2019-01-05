# asia-polmeth-2019

Paper and slides for Asia POLMETH in Kyoto, Japan, 5-6 January 2019. 

Slides are published at www.andybeger.com/asia-polmeth-2019. They do not update automatically, I just copied over the contents of the `slides` folder. 

Paper is at www.andybeger.com/asia-polmeth-2019/pdf/BegerWard_HFC_AsiaPolmeth2019.pdf.

To convert slides to PDF:

```r
library("webshot")
# if needed, for macos also can do via homebrew
#install_phantomjs()
file_name <- paste0("file://", normalizePath("slides/index.html"))
webshot(file_name, "docs/pdf/slides.pdf")
```