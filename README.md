# Shiny_DataViz

I first developed a Shiny app similar to this as part of the video course "[Learning R for Data Visualization](https://www.packtpub.com/big-data-and-business-intelligence/learning-r-data-visualization-video)", which I published with Packt:

<img src="https://d1ldz4te4covpm.cloudfront.net/sites/default/files/imagecache/ppv4_main_book_cover/bookretailers/9781785882890.jpg" width="200" height="300" />


In the video course the focus was on developing an Shiny app for the first time, so the final app was relatively simple. This new version includes more plots, a list is available below:

- Histograms (with option for faceting)
- Barchart (with confidence intervals, and option for color with dodging and faceting, plus a checkbox in case on non-normal distributions)
- BoxPlots (with option for faceting)
- Scatterplots (with options for color, size and faceting)
- TimeSeries 

Error bars in barcharts are computed as 1.96 times the standard error of the mean (1.57 times the standard error of the median in case on non-normal distribution). When the color option is set, barcharts are plotted one next to the other for each color (option dodging).

For scatterplots, if the option for faceting is provided each plot will include a linear regression lines.

## Usage

- Users can import either a csv or a txt file
- The first selection box allows to choose the separation symbol between comma, space and semicolon
- Please be aware that the the function read.table (which was use in the app) expects the data file to have missing values identified with NA (default option na.strings=NA), and the file to have column labels (default option header=T).
- Column names become the labels of the plot (on the two axis and legend), so it might be a good idea to have informative column names.
- If you need a variable to be identified as factorial please encode it with letters: e.g. T1, T2. Numbers are identified by R as numerical columns.


## Download Buttons

Version 3 of the app allows users to download plots at high resolution, specifying both width and height of the output image.
I would like to thank hplieninger for his support!!
