Chart gallaley
================
Ko ABE
2024-02-26

## icon array

``` r
library(ggsomestat)
```

    ## Loading required package: ggplot2

``` r
HEC <- as.data.frame(HairEyeColor)
ggplot(HEC, aes(x=Freq, colour=Sex))+
  stat_countgrid(shape=15)+
  facet_grid(Hair~Eye, labeller = label_both)
```

![](example_files/figure-gfm/HEC-1.png)<!-- -->

## ecdf

``` r
ggplot(searchConsole, aes(impressions, pagePath)) +
  stat_ecdf2()
```

![](example_files/figure-gfm/ecdf2-1.png)<!-- -->

## binomial confidence interval

``` r
entry0117 <- searchConsole[searchConsole$pagePath=="/entry/2015/01/17/064522",]
ggplot(entry0117, aes(date, clicks/impressions)) +
  geom_line()+
  stat_binomCI(geom = "ribbon", aes(numerator=clicks, denominator=impressions), alpha=0.3)
```

    ## Warning: The following aesthetics were dropped during statistical transformation:
    ## numerator, denominator, and y.
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

![](example_files/figure-gfm/binomCI-1.png)<!-- -->

## moving avarage

``` r
ggplot(economics, aes(date, unemploy)) +
  geom_line()+
  stat_ma(colour="royalblue", windowsize=300)
```

![](example_files/figure-gfm/ma-1.png)<!-- -->
