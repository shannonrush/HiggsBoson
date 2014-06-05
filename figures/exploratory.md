Exploratory Figures for Higgs Boson Challenge
========================================================

```r
train <- read.csv("../data/processed/processed_train.csv")
```


```r
library(ggplot2)
vars <- names(train)[2:31]
for (v in vars) {
    d <- which(train[, v] != -999)
    f <- train[d, ]
    p <- ggplot(f, aes(x = EventId, y = f[, v], color = Label), environment = environment())
    l <- p + labs(y = v)
    print(l + geom_point())
}
```

![plot of chunk plot_each](figure/plot_each1.png) ![plot of chunk plot_each](figure/plot_each2.png) ![plot of chunk plot_each](figure/plot_each3.png) ![plot of chunk plot_each](figure/plot_each4.png) ![plot of chunk plot_each](figure/plot_each5.png) ![plot of chunk plot_each](figure/plot_each6.png) ![plot of chunk plot_each](figure/plot_each7.png) ![plot of chunk plot_each](figure/plot_each8.png) ![plot of chunk plot_each](figure/plot_each9.png) ![plot of chunk plot_each](figure/plot_each10.png) ![plot of chunk plot_each](figure/plot_each11.png) ![plot of chunk plot_each](figure/plot_each12.png) ![plot of chunk plot_each](figure/plot_each13.png) ![plot of chunk plot_each](figure/plot_each14.png) ![plot of chunk plot_each](figure/plot_each15.png) ![plot of chunk plot_each](figure/plot_each16.png) ![plot of chunk plot_each](figure/plot_each17.png) ![plot of chunk plot_each](figure/plot_each18.png) ![plot of chunk plot_each](figure/plot_each19.png) ![plot of chunk plot_each](figure/plot_each20.png) ![plot of chunk plot_each](figure/plot_each21.png) ![plot of chunk plot_each](figure/plot_each22.png) ![plot of chunk plot_each](figure/plot_each23.png) ![plot of chunk plot_each](figure/plot_each24.png) ![plot of chunk plot_each](figure/plot_each25.png) ![plot of chunk plot_each](figure/plot_each26.png) ![plot of chunk plot_each](figure/plot_each27.png) ![plot of chunk plot_each](figure/plot_each28.png) ![plot of chunk plot_each](figure/plot_each29.png) ![plot of chunk plot_each](figure/plot_each30.png) 




