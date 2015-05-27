#  Exploratory Data Analysis
## Subinoy Biswas
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)

# reading data
data <- read.csv("transrate_contigs.csv", header=T)

# preview data frame
tbl_df(data)
names(data)

# creating new data frame of score and p_good
t_table <- select(data, p_good, score)

# Checking missing values
sum(!is.na(t_table))
sum(!is.na(filt_dat)) == length(row.names(filt_dat))*2

# Function for exploratory statistics
stat2 <- function(df) {
    d_min=apply(df, 2, min)
    d_max=apply(df, 2, max)
    d_mean=apply(df,2, mean)
    d_median=apply(df, 2, median)
    d_var=apply(df, 2, var)
    d_sd=apply(df, 2, sd)
    result <- list(min=d_min, max=d_max, mean=d_mean, median=d_median,
                   var=d_var, sd=d_sd)
    return(result)
    
}

stat(t_table)

# Long format conversion
m_t_table <- melt(t_table)

# Preview table
tbl_df(m_t_table)

# t-test
t.test(value ~ variable, m_t_table)

# Box plot function
my_boxplot <- function(df){
    require(ggplot2)
    
    ggplot(df, aes(x=variable, y=value, fill=variable)) +
        geom_boxplot()+ guides(fill=FALSE)+
        labs(title="Box plot of p_good and score")
    
}

my_boxplot(m_t_table)

# Density plot function
my_density_plot <- function(df){
    require(ggplot2)
    require(plyr)
    
    cdat <- ddply(df, "variable", summarise, value.mean=mean(value))
    ggplot(df, aes(x=value, fill=variable)) + 
        geom_density(alpha=.3) +
        geom_vline(data=cdat, aes(xintercept=value.mean,  colour=variable), 
                   linetype="dashed", size=1)+
        labs(title="Density plot of p_good and score")
}

my_density_plot(m_t_table)

# Histogram plot function
my_hist_plot <- function(df){
    require(ggplot2)
    
    ggplot(df, aes(x=value, fill=variable)) + geom_histogram() +
        labs(title="Histogram plot")
}

my_hist_plot(m_t_table)

# Linear model
fit <- lm(filter_data$p_good ~ filter_data$score)
fit

summary(fit)

plot(fit)

# correlation value
cor_total_table <- cor(t_table$p_good, t_table$score)

cor_total_table


# Filtering with score threshold
filt_dat <- filter(t_table, score>0.3)

tbl_df(filt_dat)

stat(filt_dat)

# Long format conversion
m_filter_dat <- melt(filt_dat)

tbl_df(m_filter_dat)

# t-test of filtered dataset
t.test(value ~ variable, m_filter_dat)

# Plots
my_boxplot(m_filter_dat)
my_density_plot(m_filter_dat)
my_hist_plot(m_filter_dat)

# Linear model
fit1 <- lm(p_good ~ score, data = filt_dat)
fit1

summary(fit1)

plot(fit1)

# correlation
cor_filt_dat <- cor(filt_dat$p_good, filt_dat$score)

cor_filt_dat

