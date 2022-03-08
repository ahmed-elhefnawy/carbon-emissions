---
title: <center> <h1>Change of CO~2~ emissions per capita across countries in the long run</h1> </center> <h1 style="font-size:40px;"></h1>
output: 
   html_document:
     keep_md: true
editor_options: 
  chunk_output_type: console
---



\

I wanted to see if the change of carbon dioxide emissions per capita, measured in metric tons, is significant in the long run. We expect it to be positive and distinguishable from zero.
The dataset is [here](https://data.worldbank.org/indicator/EN.ATM.CO2E.PC?view=chart)

First, we might calculate the change rate of the emissions for each country over the period 1960-2016.


```r
#load the libraries we need.
library(readxl)
library(dplyr)
library(data.table)
library(EnvStats)
library(DT)

#import the dataset. I renamed the file as you can see.
emission<-read_xls("carbondioxideemmision.xls")

#now let's see the first small part of the data
emission[1:7,1:7] #alternatively, you can use head() function to review the first parts of the data.
```

```
# A tibble: 7 × 7
  `Data Source`     `World Development Indicators` ...3  ...4  ...5  ...6  ...7 
  <chr>             <chr>                          <chr> <chr> <chr> <chr> <chr>
1 Last Updated Date 44274                          <NA>  <NA>  <NA>  <NA>  <NA> 
2 <NA>              <NA>                           <NA>  <NA>  <NA>  <NA>  <NA> 
3 Country Name      Country Code                   Indi… Indi… 1960  1961  1962 
4 Aruba             ABW                            CO2 … EN.A… 204.… 208.… 226.…
5 Afghanistan       AFG                            CO2 … EN.A… 0.04… 0.05… 0.07…
6 Angola            AGO                            CO2 … EN.A… 0.10… 0.08… 0.21…
7 Albania           ALB                            CO2 … EN.A… 1.25… 1.37… 1.43…
```

```r
#remove the first two rows, since they contain nothing.
emission<-emission[-c(1:2),]

#the first row contains the names of columns, yet the columns are not named properly. Change the columns names to be as the first row
colnames(emission)<-emission[1,]

#after we renamed the columns by the first row, we don't need the first row anymore, so let's delete it.
emission<-emission[-c(1),]

#Now let's see again:

emission[1:5,]
```

```
# A tibble: 5 × 65
  `Country Name` `Country Code` `Indicator Name`  `Indicator Code` `1960` `1961`
  <chr>          <chr>          <chr>             <chr>            <chr>  <chr> 
1 Aruba          ABW            CO2 emissions (m… EN.ATM.CO2E.PC   204.6… 208.8…
2 Afghanistan    AFG            CO2 emissions (m… EN.ATM.CO2E.PC   0.046… 0.053…
3 Angola         AGO            CO2 emissions (m… EN.ATM.CO2E.PC   0.100… 0.082…
4 Albania        ALB            CO2 emissions (m… EN.ATM.CO2E.PC   1.258… 1.374…
5 Andorra        AND            CO2 emissions (m… EN.ATM.CO2E.PC   <NA>   <NA>  
# … with 59 more variables: `1962` <chr>, `1963` <chr>, `1964` <chr>,
#   `1965` <chr>, `1966` <chr>, `1967` <chr>, `1968` <chr>, `1969` <chr>,
#   `1970` <chr>, `1971` <chr>, `1972` <chr>, `1973` <chr>, `1974` <chr>,
#   `1975` <chr>, `1976` <chr>, `1977` <chr>, `1978` <chr>, `1979` <chr>,
#   `1980` <chr>, `1981` <chr>, `1982` <chr>, `1983` <chr>, `1984` <chr>,
#   `1985` <chr>, `1986` <chr>, `1987` <chr>, `1988` <chr>, `1989` <chr>,
#   `1990` <chr>, `1991` <chr>, `1992` <chr>, `1993` <chr>, `1994` <chr>, …
```

```r
#since we have "years" as variables (columns), we can transpose the data so that countries will be variables and years as rows. I'll tell you later why we did that.
emission<-as.data.frame(t(emission))

#again column names are not properly named, so we change them as before.
colnames(emission)<-emission[1,]

#the first four rows are no use for us, we can remove them. The first row contains country names, which we had already used to name our columns, the remaining rows contain country code, indicator name, and indicator code, respectively, and we'll not use any of them.
emission<-emission[-c(1:4),]

#now let's have a look
emission[1:7,1:7]
```

```
                  Aruba          Afghanistan               Angola
1960 204.62037224917452 0.046056712629903414   0.1008353356494021
1961 208.82281106822035 0.053588835050455808 0.082203796747050334
1962 226.11807914628724 0.073720830832381873  0.21053147709234077
1963 214.80037040303375 0.074160724829865854  0.20273730345395635
1964 207.61577710758871 0.086173614368552767  0.21356034931902876
1965 185.20395746164576  0.10128491249779034  0.20589092585307864
1966 172.11995148574894  0.10739888092545179  0.26894143686775823
                Albania Andorra          Arab World United Arab Emirates
1960 1.2581949278965689    <NA> 0.60744755675365358  0.11903525287281698
1961 1.3741860465116278    <NA> 0.66063794460729808  0.10914123576332393
1962 1.4399559637916719    <NA> 0.72494431853622876  0.16353306337965359
1963 1.1816811441597486    <NA> 0.85056679990052364  0.17583313354111724
1964 1.1117419596667282    <NA> 0.96947620360177611  0.13282478140235732
1965 1.1660990427345477    <NA>  1.1352713004209469  0.14681996836984593
1966 1.3330554645866206    <NA>  1.2482565638723149  0.16045531829774465
```

```r
#now before playing with these numbers let's see if R recognize them as numbers. Let's take the first country as an example.
class(emission$Aruba)
```

```
[1] "character"
```

```r
#R doesn't recognize the values as numbers. So we can use lapply function to declare all of them as numbers to R. Now here comes why we transposed the data before. The idea is to not have a country column, so I'd declare all columns as numbers to R without being concerned about a character column (the country column) 
emission[]<-lapply(emission,function(x) as.numeric(as.character(x)))

#then let's remove the countries that we don't have any observation about it.
emission<-emission[, colSums(is.na(emission))<nrow(emission)]

#the "year" is not recognized as a column by R yet. Let's declare it.
setDT(emission,keep.rownames = "year")

#let's have a quick look
emission[1:5,1:5]
```

```
   year    Aruba Afghanistan    Angola  Albania
1: 1960 204.6204  0.04605671 0.1008353 1.258195
2: 1961 208.8228  0.05358884 0.0822038 1.374186
3: 1962 226.1181  0.07372083 0.2105315 1.439956
4: 1963 214.8004  0.07416072 0.2027373 1.181681
5: 1964 207.6158  0.08617361 0.2135603 1.111742
```

```r
#let's transform our data to long format so that we'd have the countries as a column altogether, resulting in a panel data.
emission<-melt(emission, id.vars="year",variable.name = "country")

head(emission)
```

```
   year country    value
1: 1960   Aruba 204.6204
2: 1961   Aruba 208.8228
3: 1962   Aruba 226.1181
4: 1963   Aruba 214.8004
5: 1964   Aruba 207.6158
6: 1965   Aruba 185.2040
```



```r
#filter our data so that we would have only the observations for 1960 and 2016.
emission_6016<-dplyr::filter(emission,year %in% c(1960,2016))

#let's calculate the change rate.
emission_6016<- emission_6016 %>%
    group_by(country) %>%
    mutate(change=(((value/lag(value))^(1/(2016-1960)))-1)*100)

#let's have a look.
head(emission_6016)
```

```
# A tibble: 6 × 4
# Groups:   country [3]
  year  country        value change
  <chr> <fct>          <dbl>  <dbl>
1 1960  Aruba       205.      NA   
2 2016  Aruba         8.43    -5.54
3 1960  Afghanistan   0.0461  NA   
4 2016  Afghanistan   0.245    3.03
5 1960  Angola        0.101   NA   
6 2016  Angola        1.20     4.53
```

```r
#we only need the "country" and its change rate, so we can remove the other two columns.
emission_6016<-emission_6016[,-c(1,3)]

#remove the missing values, since it's only one value for each country.
emission_6016<-na.omit(emission_6016)

#let's have another look.
head(emission_6016)
```

```
# A tibble: 6 × 2
# Groups:   country [6]
  country              change
  <fct>                 <dbl>
1 Aruba                -5.54 
2 Afghanistan           3.03 
3 Angola                4.53 
4 Albania               0.404
5 Arab World            3.73 
6 United Arab Emirates  9.77 
```

In addition, let's consider the average emissions per capita for each country over the same period, and the coefficient of variation as well.


```r
#turning back to our original data, calculate the mean and the coefficient of variation by country and then merge it with the subset with the change rate that we created it before.
emission<-inner_join(emission_6016,emission %>%
             group_by(country) %>%
             summarise(mean=mean(value,na.rm = T), cv=cv(value,na.rm=T)))
```

```
Joining, by = "country"
```

```r
#round our variables to the fourth digit
emission$change<-round(emission$change,4)
emission$mean<-round(emission$mean,4)
emission$cv<-round(emission$cv,4)

head(emission)
```

```
# A tibble: 6 × 4
# Groups:   country [6]
  country              change    mean    cv
  <fct>                 <dbl>   <dbl> <dbl>
1 Aruba                -5.54  105.    0.996
2 Afghanistan           3.03    0.148 0.603
3 Angola                4.53    0.652 0.558
4 Albania               0.404   1.65  0.393
5 Arab World            3.73    3.03  0.396
6 United Arab Emirates  9.77   31.0   0.681
```

```r
#since the world bank includes regions among countries, I created a vector manually to contain all regions that are not individual countries, since including them might bias our calculations.
regions<-c("Arab World","Central Europe and the Baltics","Caribbean small states","East Asia & Pacific (excluding high income)","Early-demographic dividend","East Asia & Pacific","Europe & Central Asia (excluding high income)","Europe & Central Asia","European Union","Fragile and conflict affected situations","High income","Heavily indebted poor countries (HIPC)","IBRD only", "IDA & IBRD total","IDA total","IDA blend","IDA only","Latin America & Caribbean (excluding high income)","Latin America & Caribbean","Least developed countries: UN classification","Low income","Lower middle income","Low & middle income","Late-demographic dividend","Middle East & North Africa","Middle income","Middle East & North Africa (excluding high income)","North America","OECD members","Pre-demographic dividend","Pacific island small states","Other small states","Post-demographic dividend","South Asia","Sub-Saharan Africa (excluding high income)","Sub-Saharan Africa","Small states","East Asia & Pacific (IDA & IBRD countries)","Europe & Central Asia (IDA & IBRD countries)","Latin America & the Caribbean (IDA & IBRD countries)","Middle East & North Africa (IDA & IBRD countries)","South Asia (IDA & IBRD)","Sub-Saharan Africa (IDA & IBRD countries)","Upper middle income","World")

#quickly create a new subset to include these regions only, and then filter our data to include only the places that are not in the subset created. there are many ways to do that, I just chose what came to my mind. 
emission2<-filter(emission,country%in%regions)
emission<-emission[!(emission$country %in% emission2$country),]

#let's create a table to show our data.
datatable(emission, colnames = c("country","percentage change of emissions per capita \n (1960-2016)","Average of emissions per capita (in metric tons) \n (1960-2016)","coefficent of variation"))
```

```{=html}
<div id="htmlwidget-0dfa5b423c3b4294ee92" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0dfa5b423c3b4294ee92">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153"],["Aruba","Afghanistan","Angola","Albania","United Arab Emirates","Argentina","Antigua and Barbuda","Australia","Austria","Belgium","Benin","Burkina Faso","Bangladesh","Bulgaria","Bahrain","Bahamas, The","Belize","Bermuda","Bolivia","Brazil","Barbados","Brunei Darussalam","Central African Republic","Canada","Switzerland","Chile","China","Cote d'Ivoire","Cameroon","Congo, Dem. Rep.","Congo, Rep.","Colombia","Comoros","Cabo Verde","Costa Rica","Cuba","Cayman Islands","Cyprus","Djibouti","Dominica","Denmark","Dominican Republic","Algeria","Ecuador","Egypt, Arab Rep.","Spain","Ethiopia","Finland","Fiji","Faroe Islands","Gabon","United Kingdom","Ghana","Gibraltar","Guinea","Gambia, The","Guinea-Bissau","Equatorial Guinea","Greece","Grenada","Greenland","Guatemala","Guyana","Hong Kong SAR, China","Honduras","Haiti","Hungary","Indonesia","India","Ireland","Iran, Islamic Rep.","Iraq","Iceland","Israel","Jamaica","Jordan","Japan","Kenya","Cambodia","St. Kitts and Nevis","Korea, Rep.","Kuwait","Lao PDR","Lebanon","Liberia","Libya","St. Lucia","Sri Lanka","Luxembourg","Macao SAR, China","Morocco","Madagascar","Mexico","Mali","Malta","Myanmar","Mongolia","Mozambique","Mauritania","Mauritius","Malaysia","New Caledonia","Niger","Nigeria","Nicaragua","Netherlands","Norway","Nepal","New Zealand","Pakistan","Panama","Peru","Philippines","Palau","Papua New Guinea","Poland","Korea, Dem. People’s Rep.","Portugal","Paraguay","French Polynesia","Qatar","Romania","Russian Federation","Rwanda","Saudi Arabia","Senegal","Singapore","Solomon Islands","Sierra Leone","El Salvador","Somalia","Sao Tome and Principe","Suriname","Sweden","Eswatini","Syrian Arab Republic","Chad","Togo","Thailand","Tonga","Trinidad and Tobago","Tunisia","Turkey","Tanzania","Uganda","Uruguay","United States","St. Vincent and the Grenadines","Venezuela, RB","Vietnam","Samoa","Yemen, Rep.","South Africa"],[-5.5368,3.0304,4.5262,0.4043,9.7721,1.1886,3.9396,1.0656,0.8521,-0.2688,3.9968,5.506,1.0641,1.3031,3.3329,0.4139,2.1146,1.77,3.5695,2.2362,3.2464,2.7099,0.1976,0.6039,0.2084,1.8823,3.2912,2.0286,3.4302,-3.1304,1.9808,1.2335,2.6827,4.0765,2.6956,0.4705,3.3246,2.3419,0.5807,4.7911,-0.2828,3.7084,3.4383,3.3786,2.5915,2.1367,4.008,1.6115,2.8209,3.7182,4.2078,-1.1675,1.7584,4.0501,1.3942,2.8882,3.1013,7.383,2.9637,4.1832,0.4944,2.1281,1.7739,3.3089,2.2637,2.3702,0.0371,3.9661,3.4802,1.2511,2.8682,2.754,-0.2073,1.6439,2.061,2.1522,2.2904,0.3576,4.9967,5.6303,5.8497,-0.2684,7.832,1.7092,1.2889,5.1079,4.8355,2.8471,-1.5332,4.38,3.2223,1.2467,1.5453,3.7117,1.87,2.4186,3.2899,0.1506,4.9865,4.633,5.3481,1.021,4.4097,3.9122,1.9556,0.8054,1.3696,6.8947,0.7329,2.0641,1.9902,1.5036,2.3726,3.875,4.4477,0.2783,-1.1072,2.9462,3.4923,3.2425,4.2834,0.3418,-0.0123,1.7467,6.0276,1.8644,3.7619,1.9302,-1.2901,2.9284,0.7106,2.2521,1.2875,-0.7318,4.3015,1.5326,2.4157,4.1146,6.2834,3.5631,4.2812,3.3723,3.6945,1.8145,1.4977,0.2672,-0.0564,4.929,-0.4319,4.4265,4.0731,6.575,0.7035],[104.7306,0.148,0.6517,1.6517,31.0151,3.6885,5.2642,14.5646,7.1936,11.1275,0.2176,0.0714,0.2413,7.0697,20.4557,12.6245,1.3571,7.5946,0.9748,1.5104,3.2366,20.913,0.0677,15.7752,5.6816,2.8843,2.7341,0.4556,0.2387,0.0931,0.464,1.5424,0.1476,0.4463,1.1229,2.5115,7.8378,5.0497,0.6923,1.0261,9.983,1.4625,2.5084,1.5853,1.4477,5.2077,0.0574,9.5432,1.0979,10.4409,4.4558,9.7055,0.3101,6.6509,0.1939,0.1829,0.1454,2.4384,5.7733,1.1918,8.8976,0.6261,1.9819,4.2088,0.651,0.1444,6.2494,0.943,0.7597,7.9648,4.6137,3.151,7.2322,6.9423,2.9976,2.3331,7.916,0.2842,0.1447,2.2936,5.8387,28.1521,0.2363,2.9562,0.4251,7.751,1.3324,0.3959,27.2603,2.2823,0.9646,0.1132,3.4244,0.0671,4.271,0.1793,4.1,0.18,0.4846,1.4815,3.7809,12.5874,0.0711,0.5706,0.6396,10.4779,8.1035,0.0811,6.717,0.5982,1.7285,1.2275,0.7503,11.0785,0.4751,9.3094,4.492,3.6178,0.5356,2.1754,53.6178,5.7855,16.7677,0.061,12.6174,0.4258,9.6861,0.3566,0.1521,0.6683,0.0754,0.3712,4.1732,7.1811,0.826,2.2426,0.051,0.2191,1.814,0.6759,17.1814,1.5716,2.498,0.1244,0.0786,1.7782,19.0121,0.9817,6.0106,0.6517,0.6365,0.4488,8.1874],[0.9957,0.6027,0.5584,0.3928,0.6809,0.1667,0.5957,0.1859,0.1503,0.1328,0.7736,0.6308,0.5605,0.2531,0.3546,0.9974,0.2748,0.3053,0.4629,0.3317,0.4399,0.7876,0.2268,0.1164,0.1387,0.3427,0.7767,0.355,0.7174,0.5691,0.4634,0.1478,0.3603,0.7892,0.3784,0.2044,0.4392,0.3909,0.2644,0.7757,0.1931,0.4745,0.4244,0.4666,0.456,0.3283,0.4595,0.2471,0.3544,0.3802,0.6093,0.1516,0.3348,0.7819,0.1322,0.3307,0.3002,1.523,0.4145,0.6775,0.2525,0.3285,0.2245,0.4389,0.391,0.438,0.1908,0.6143,0.5755,0.2305,0.4323,0.312,0.0998,0.282,0.2853,0.3937,0.2535,0.1762,0.9601,0.7181,0.6661,0.453,1.633,0.3086,0.7731,0.4346,0.5781,0.5561,0.272,0.5034,0.4955,0.2823,0.2733,0.5997,0.4617,0.4061,0.6209,0.5763,0.6599,0.7057,0.6885,0.3056,0.4863,0.4541,0.243,0.1408,0.249,0.9419,0.1875,0.3733,0.3128,0.2305,0.254,0.2685,0.4368,0.1873,0.5255,0.4682,0.4522,0.3394,0.3925,0.3468,0.3554,0.4857,0.4304,0.3271,0.4664,0.3393,0.4071,0.4617,0.4613,0.3869,0.2841,0.277,0.3758,0.4145,0.3722,0.4703,0.7679,0.5748,0.5816,0.4326,0.4709,0.3628,0.4399,0.1969,0.0924,0.7709,0.1112,0.8192,0.4885,0.8496,0.1465]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>country<\/th>\n      <th>percentage change of emissions per capita \n (1960-2016)<\/th>\n      <th>Average of emissions per capita (in metric tons) \n (1960-2016)<\/th>\n      <th>coefficent of variation<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```



We can see which country has the highest and the lowest emission change in the long run.


```r
> #country with the minimum emission change 
> emission[which.min(emission$change),]
```

```
# A tibble: 1 × 4
# Groups:   country [1]
  country change  mean    cv
  <fct>    <dbl> <dbl> <dbl>
1 Aruba    -5.54  105. 0.996
```

```r
> #country with the maximum emission change
> emission[which.max(emission$change),]
```

```
# A tibble: 1 × 4
# Groups:   country [1]
  country              change  mean    cv
  <fct>                 <dbl> <dbl> <dbl>
1 United Arab Emirates   9.77  31.0 0.681
```

## **Probability distribution of the change rate of CO~2~ emissions per capita in the long run:**

* Let's have a closer look to our variable distribution:


```r
#plot the empirical probability density function.
epdfPlot(emission$change,xlab=expression('Change of CO'[2] ~ 'emissions per capita in the long run'),main=expression('Empirical PDF of the percentage change of CO'[2]~ 'emissions per capita in the long run'),cex.main=0.95)
abline(v=mean(emission$change),col="darkgray")
```

![](carbon-emissions_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The dark gray vertical line represents the mean in our data, as you might expected from the code.



```r
#plot of the empirical cumulative distribution function.
ecdfPlot(emission$change, xlab =expression('Order Statistics for the change rate of CO'[2]~'emissions per capita'),main=expression('Empirical CDF of the percentage change of CO'[2]~ 'emissions per capita in the long run'),cex.main=0.95)
```

![](carbon-emissions_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


We can see that the probability that the emission change rate in the long run is less than or equal to zero is almost $0$, supporting our hypothesis that the change rate is positive. \par
Simply, let $X$ be the change of CO~2~ emissions per capita, then $P(X \le 0) \approx 0$, but we didn't check yet for its significance from zero, which comes later.

Now, let's "boxplot" it!


```r
boxplot(emission$change,horizontal=T,xlab=expression('Change of CO'[2] ~ 'emissions per capita in the long run'),varwidth=T)
abline(v=0)
```

![](carbon-emissions_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

It doesn't seem close to zero but it might not be significant, so let's test if the change in the long run across countries is significant from zero.


```r
#using t-test.
t.test(emission$change)
```

```

	One Sample t-test

data:  emission$change
t = 14.803, df = 152, p-value < 2.2e-16
alternative hypothesis: true mean is not equal to 0
95 percent confidence interval:
 2.149774 2.811979
sample estimates:
mean of x 
 2.480876 
```

The p-value is extremely small almost equal to $0$, thus the change in the long run is significant from zero at all levels.\par
In other words, the carbon dioxide emissions per capita, in metric tons, across countries over the period $1960-2016$ increased significantly by more than $2\%$, on average.

We can play more with our data, but I'll leave that to you, which is why I inserted the interactive table. 

Have fun!

