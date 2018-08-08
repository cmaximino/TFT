R Notebook for effects of fluoxetine and metergoline on conditional approach in guppies (LaNeC)
================
Caio Maximino[1]

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the data analysis of the research project "Role of serotonin on conditional approach in guppies".

Data packages for the research project "Role of serotonin on conditional approach in guppies". Data are produced by members from Laboratório de Neurociências e Comportamento "Frederico Guilherme Graeff", affiliated to Universidade Federal do Sul e Sudeste do Pará and Universidade do Estado do Pará. The package includes primary data for behavioral experiments on the effects of acute fluoxetine and metergoline on guppy conditional approach, as well as scripts for statistical analysis of these data.

When you execute code within the notebook, the results appear beneath the code.

-   Load needed libraries:

``` 
{r}
    if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
    }
    if(!require(RCurl)){
    install.packages("RCurl")
    library(RCurl)
    }
    if(!require(sjstats)){
    install.packages("sjstats")
    library(sjstats)
    }
```

-   Load data for experiment 1

``` {r}
    x1 <- getURL("https://raw.githubusercontent.com/lanec-unifesspa/TFT/master/validation/exp1.csv")
    exp1 <- read.csv(text = x1)
    exp1$group <- as.factor(exp1$group)
    x1 <- getURL("https://raw.githubusercontent.com/lanec-unifesspa/TFT/master/validation/freezing-exp1.csv")
    exp1.freezing <-read.csv(text = x1)
    View(exp1.freezing)
```

-   Run t-tests and plot data

1.  Refuge use

``` {r}
refuge.pilot <- t.test(Refuge ~ group, data = exp1)
refuge.pilot
ggplot(data = exp1, aes(x = group, y = Refuge))  + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", aes(fill = group)) + geom_hline(yintercept = 0) + xlab("Group") + ylab ("Change in refuge use (%)") + theme(legend.position = "none")
```

2.  Time in avoidance area

``` {r}
avoid.pilot <- t.test(Avoidance ~ group, data = exp1)
avoid.pilot
ggplot(data = exp1, aes(x = group, y = Avoidance))  + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", aes(fill = group)) + geom_hline(yintercept = 0) + xlab("Group") + ylab ("Change in time in avoidance zone (%)") + theme(legend.position = "none")
```

3.  Time in inspection zone

``` {r}
inspect.pilot <- t.test(Inspection ~ group, data = exp1)
inspect.pilot
ggplot(data = exp1, aes(x = group, y = Inspection))  + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", aes(fill = group)) + geom_hline(yintercept = 0) + xlab("Group") + ylab ("Change in time in inspection zone (%)") + theme(legend.position = "none")
```

-   Run RM-ANOVA and plot data

4.  Freezing

``` {r}
anova.freezing.exp1 <- aov(Freezing ~ Group + Time.block + Group:Time.block, data = exp1.freezing)
summary(anova.freezing.exp1)
TukeyHSD(anova.freezing.exp1)

ggplot(data = exp1.freezing, aes(x = Time.block, y = Freezing, fill = Group))  + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", aes(fill = Group)) + geom_hline(yintercept = 0) + xlab("Group") + ylab ("Freezing (s)")
```

-   Load data for experiment 2

``` {r}
    x1 <- getURL("https://raw.githubusercontent.com/lanec-unifesspa/TFT/master/5HT/drugs.csv")
    exp2 <- read.csv(text = x1)
    exp2$group <- as.factor(exp2$group)
    x1 <- getURL("https://raw.githubusercontent.com/lanec-unifesspa/TFT/master/5HT/freezing-drugs.csv")
    exp2.freezing <-read.csv(text = x1)
    View(exp2.freezing)
```

-   Run ANOVAs and plot data

1.  Refuge use

``` {r}
refuge.drugs <- aov(Refuge ~ group, data = exp2)
summary(refuge.drugs)
omega_sq(refuge.drugs)
TukeyHSD(refuge.drugs)
ggplot(data = exp2, aes(x = group, y = Refuge))  + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", aes(fill = group)) + geom_hline(yintercept = 0) + xlab("Group") + ylab ("Change in refuge use (%)") + theme(legend.position = "none")
```

2.  Time in avoidance area

``` {r}
avoid.drugs <- aov(Avoidance ~ group, data = exp2)
summary(avoid.drugs)
omega_sq(avoid.drugs)
TukeyHSD(avoid.drugs)
ggplot(data = exp2, aes(x = group, y = Avoidance))  + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", aes(fill = group)) + geom_hline(yintercept = 0) + xlab("Group") + ylab ("Change in time in avoidance zone (%)") + theme(legend.position = "none")
```

3.  Time in inspection zone

``` {r}
inspect.drugs <- aov(Inspection ~ group, data = exp2)
summary(inspect.drugs)
omega_sq(inspect.drugs)
TukeyHSD(inspect.drugs)
ggplot(data = exp2, aes(x = group, y = Inspection))  + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", aes(fill = group)) + geom_hline(yintercept = 0) + xlab("Group") + ylab ("Change in time in inspection zone (%)") + theme(legend.position = "none")
```

-   Run RM-ANOVA and plot data

4.  Freezing

``` {r}
anova.freezing.exp2 <- aov(freezing ~ group + block + group:block, data = exp2.freezing)
summary(anova.freezing.exp2)
omega_sq(anova.freezing.exp2)
TukeyHSD(anova.freezing.exp2)

ggplot(data = exp2.freezing, aes(x = block, y = freezing, fill = group))  + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", aes(fill = group)) + geom_hline(yintercept = 0) + scale_fill_manual(values=colours_vector) + xlab("Group") + ylab ("Freezing (s)")
```

[1] Universidade Federal do Sul e Sudeste do Pará

[2] Universidade do Estado do Pará
