###--------------------------------------------------
### CA Vaccines
### Kieran Healy
### 1/29/15
### http://www.cdph.ca.gov/programs/immunize/pages/immunizationlevels.aspx
### Specifically,
### http://www.cdph.ca.gov/programs/immunize/Documents/2014-15%20CA%20Kindergarten%20Data.xlsx
###--------------------------------------------------


library(ggplot2)
library(dplyr)
library(stringr)
library(kjhutils) # for the credit line

cb.palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                    "#0072B2", "#D55E00", "#CC79A7")


## Clean/colhead version.
## There's a Los Angeles typo in the raw data
data <- read.csv("data/elementary-schools-1415-CA.csv", header=TRUE)

## Missing enrollment data
no.counts <- is.na(data$enrollment)

## Recalculate the exemption rate
data$Exempt <- round((data$PBE.n/data$enrollment)*100,2)


## Working data
data.sub <- subset(data, subset=!no.counts, select=c("code", "county", "name", "Type", "district", "city", "enrollment", "PBE.pct", "Exempt"))

## Look
arrange(data.sub, desc(Exempt))[1:100,c("name", "county", "city","enrollment", "Exempt")]
state.rate <- mean(data.sub$Exempt, na.rm=TRUE)
summarize(data.sub, Mean.PBE = mean(Exempt, na.rm = TRUE))


###--------------------------------------------------
### Summary tables
###--------------------------------------------------

### By Pub/Private
by.type <- data.sub %>% group_by(Type) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm = TRUE))


### By County
by.county <- data.sub %>% group_by(county) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

### By School within County
school.by.county <- data.sub %>% group_by(county, code) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

### By City
by.city <- data.sub %>% group_by(city) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

### By School within City
school.by.city <- data.sub %>% group_by(city, code) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

### By District
by.district <- data.sub %>% group_by(district) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))

### By School within District
school.by.district <-  data.sub %>% group_by(district, code) %>% summarize(Mean.PBE = round(mean(Exempt, na.rm=TRUE), 2), Students=sum(enrollment, na.rm=TRUE)) %>% na.omit() %>% arrange(desc(Mean.PBE))


###--------------------------------------------------
### Plots
###--------------------------------------------------

### County Level
p <- ggplot(by.county, aes(x=log(Students), y=Mean.PBE))
p1 <- p + geom_point(alpha=0.6) + theme_bw() +
    ylab("Percent of Students with a Personal Belief Exemption") +
        xlab("log N Students in County \n") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, County Level") + scale_color_manual(values=cb.palette)

### Pick out some outliers
ind <- with(by.county, (Mean.PBE>2*IQR(Mean.PBE)))
data.out <- droplevels(by.county[ind,])

pdf(file="figures/pbe-by-county.pdf", height=5, width=8)
p2 <- p1 + geom_text(data = data.out, aes(x=log(Students),
                         y=Mean.PBE, label=county), hjust=-0.1, size=2)
print(p2)
credit("Data: CA Dept of Public Health, 2015. Kieran Healy: http://kieranhealy.org")
dev.off()

ggsave(
    "figures/pbe-by-county.png",
    p2,
    width=8,
    height=5,
    dpi=300
    )


## District Level
p <- ggplot(by.district, aes(x=log(Students), y=Mean.PBE, label=district))
p + geom_point(alpha=0.6) + theme_bw() +
    ylab("Percent of Students with a Personal Belief Exemption") +
        xlab("log N Students in District") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, District Level (Public Schools Only)") + ylim(0,65) + scale_color_manual(values=cb.palette)


p <- ggplot(by.district, aes(x=log(Students), y=Mean.PBE))
p1 <- p + geom_point(alpha=0.6) + theme_bw() +
    ylab("Percent of Students with a Personal Belief Exemption") +
        xlab("log N Students in District\n") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, District Level (Public Schools Only)") + ylim(0,65) + scale_color_manual(values=cb.palette)

### Pick out some outliers
ind <- with(by.district, (Mean.PBE>6*IQR(Mean.PBE)))
data.out <- droplevels(by.district[ind,])

p2 <- p1 + geom_text(data = data.out, aes(x=log(Students),
                   y=Mean.PBE, label=district), hjust=-0.05, size=2)


pdf(file="figures/pbe-by-district.pdf", width=10, height=8)
print(p2)
credit("Data: CA Dept of Public Health, 2015. Kieran Healy: http://kieranhealy.org")
dev.off()

ggsave(
    "figures/pbe-by-district.png",
    p2,
    width=10,
    height=8,
    dpi=300
    )



## School level

## ID outliers
ind <- with(data.sub, (enrollment>200 & Exempt > 10))
data.out <- data.sub[ind,]


p <- ggplot(data.sub, aes(x=log(enrollment), y=Exempt, color=Type))
p1 <- p + geom_point(alpha=0.5) + theme_bw() +
    ylab("Percent of Kindergarten Students with a Personal Belief Exemption") +
        xlab("log N Kindergarten Students\n") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, School Level") + ylim(0,100) + scale_color_manual(values=cb.palette[c(2,6)]) + theme(legend.position="top")

p2 <- p1 + geom_text(data=data.out, aes(x=log(enrollment), y=Exempt, label=name), hjust=0.8, vjust=-1.2, size=2, alpha=1)


pdf(file="figures/pbe-by-school.pdf", width=8, height=8)
print(p2)
credit("Data: CA Dept of Public Health, 2015. Kieran Healy: http://kieranhealy.org")
dev.off()

ggsave(
    "figures/pbe-by-school.png",
    p2,
    width=8,
    height=8,
    dpi=300
    )


## School level, unlogged
ind <- with(data.sub, (enrollment>200 & Exempt > 10))
data.out <- data.sub[ind,]

p <- ggplot(data.sub, aes(x=enrollment, y=Exempt, color=Type))
p1 <- p + geom_point(alpha=0.5) + theme_bw() +
    ylab("Percent of Kindergarten Students with a Personal Belief Exemption") +
        xlab("Number of Kindergarten Students\n") +  ggtitle("Kindergarten Vaccine Exemption Rates in California, School Level") + ylim(0,100) + scale_color_manual(values=cb.palette[c(2,6)]) + theme(legend.position="top")

p2 <- p1 + geom_text(data=data.out, aes(x=enrollment, y=Exempt, label=name), hjust=0.8, vjust=-1.2, size=2, alpha=1)



pdf(file="figures/pbe-by-school-unlogged.pdf", width=10, height=8)
print(p2)
credit("Data: CA Dept of Public Health, 2015. Kieran Healy: http://kieranhealy.org")
dev.off()

ggsave(
    "figures/pbe-by-school-unlogged.png",
    p2,
    width=10,
    height=8,
    dpi=300
    )
