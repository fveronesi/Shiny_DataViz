# Shiny_DataViz

I first developed a Shiny app similar to this as part of the video course "Learning R for Data Visualization", which I published with Packt:

![](https://d1ldz4te4covpm.cloudfront.net/sites/default/files/imagecache/ppv4_main_book_cover/bookretailers/9781785882890.jpg =200x400)

In the video course the focus was on developing an Shiny app for the first time, so the final app was relatively simple. This new version includes more plots, a list is available below:

- Histograms (with option for faceting)
- Barchart (with error bars, and option for color with dodging and faceting)
- BoxPlots (with option for faceting)
- Scatterplots (with options for color, size and faceting)
- TimeSeries 

Error bars in barcharts are computed with the mean_se function in ggplot2, which computes error bars as mean ± standard error. When the color option is set, barcharts are plotted one next to the other for each color (option dodging).

For scatterplots, if the option for faceting is provided each plot will include a linear regression lines.

## Usage

- Users can import either a csv or a txt file
- The first selection box allows to choose the separation symbol between comma, space and semicolon
- Please be aware that the the function read.table (which was use in the app) expects the data file to have missing values identified with NA (default option na.strings=NA), and the file to have column labels (default option header=T).
- Column names become the labels of the plot (on the two axis and legend), so it might be a good idea to have informative column names.
- If you need a variable to be identified as factorial please encode it with letters: e.g. T1, T2. Numbers are identified by R as numerical columns.


## Download Buttons

Version 2 of the app allows users to download plots at 600dpi. The file size is around 50Mb, but I decided to just allow one save because if users need low-res versions they can simply right-click on the image on screen and save it.
I would like to thank hplieninger for his support!!
