# Shiny_DataViz

I first developed a Shiny app similar to this as part of the video course "Learning R for Data Visualization", which I published with Packt:

![Learning R for Data Visualization](https://d1ldz4te4covpm.cloudfront.net/sites/default/files/imagecache/ppv4_main_book_cover/bookretailers/9781785882890.jpg)

In the video course the focus was on developing an Shiny app for the first time, so the final app was relatively simple. This new version includes more plots, a list is available below:

- Histograms (with option for faceting)
- Barchart (with error bars, and option for color with dodging and faceting)
- BoxPlots (with option for faceting)
- Scatterplots (with options for color, size and faceting)
- TimeSeries 

Error bars in barcharts are computed with the mean_se function in ggplot2, which computes error bars as mean ± standard error. When the color option is set, barcharts are plotted one next to the other for each color (option dodging).

For scatterplots, if the option for faceting is provided each plot will include a linear regression lines.



## Download Buttons

Version 2 of the app allows users to download plots at 600dpi. The file size is around 50Mb, but I decided to just allow one save because if users need low-res versions they can simply right-click on the image on screen and save it.
I would like to thank hplieninger for his support!!
