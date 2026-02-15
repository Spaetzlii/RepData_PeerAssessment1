
---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



# Introduction
This report analyzes personal activity monitoring data collected at 5-minute intervals over October–November 2012. The dataset contains three variables: `steps` (number of steps in the interval), `date` (YYYY-MM-DD), and `interval` (5-minute identifier). There are 17,568 observations in total.

> **Note**: All code used to generate the results appears with `echo=TRUE` to facilitate peer review and reproducibility.

# Loading and preprocessing the data


``` r
# Read the CSV that comes with the assignment repository
activity <- read.csv("activity.csv")

# Convert the date column to Date class
activity$date <- as.Date(activity$date)

# Quick structure check
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

# What is the mean total number of steps taken per day?
For this section we ignore the missing values in the dataset.


``` r
# Count non-missing records per day
non_missing_counts <- tapply(!is.na(activity$steps), activity$date, sum)

# Compute total steps per day (skipping NAs in the sum)
steps_per_day <- tapply(activity$steps, activity$date, sum, na.rm = TRUE)

# Keep only days that had at least one non-missing interval
valid_days <- non_missing_counts > 0
steps_per_day_valid <- steps_per_day[valid_days]

# Histogram of total steps per day
hist(steps_per_day_valid,
     breaks = 20,
     col = "steelblue",
     main = "Total steps per day (missing days removed)",
     xlab = "Steps")
```

![](PA1_template_files/figure-html/total_per_day-1.png)<!-- -->

``` r
# Mean and median of total daily steps
mean_steps <- mean(steps_per_day_valid)
median_steps <- median(steps_per_day_valid)
mean_steps; median_steps
```

```
## [1] 10766.19
```

```
## [1] 10765
```

The **mean** total steps per day is **1.076619\times 10^{4}**, and the **median** is **1.0765\times 10^{4}**.

# What is the average daily activity pattern?


``` r
# Average steps for each 5-minute interval across all days (ignoring missing values)
interval_means <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)

# Time series plot of the average number of steps, by interval
plot(interval_means$interval, interval_means$steps, type = "l",
     xlab = "5-minute interval",
     ylab = "Average steps across days",
     main = "Average daily activity pattern")
```

![](PA1_template_files/figure-html/pattern-1.png)<!-- -->

``` r
# Interval with the maximum average steps
max_row <- which.max(interval_means$steps)
max_interval <- interval_means$interval[max_row]
max_interval
```

```
## [1] 835
```

On average, the **835** interval contains the maximum number of steps.

# Imputing missing values
Missing values are imputed using a simple and common strategy: for any missing `steps` value we substitute the **mean for that 5-minute interval** across all available days.


``` r
# Total number of missing values in the dataset
num_na <- sum(is.na(activity$steps))
num_na
```

```
## [1] 2304
```

``` r
# Build a lookup of interval -> mean steps
interval_mean_map <- setNames(interval_means$steps, interval_means$interval)

# Create a copy and fill in missing steps with the interval mean
activity_imputed <- activity
missing_idx <- is.na(activity_imputed$steps)
activity_imputed$steps[missing_idx] <- interval_mean_map[ as.character(activity_imputed$interval[missing_idx]) ]

# Sanity check: no NAs should remain
sum(is.na(activity_imputed$steps))
```

```
## [1] 0
```

Now recompute the totals per day and compare the distribution and summary statistics to the pre-imputation results.


``` r
# New total steps per day after imputation
steps_per_day_imputed <- tapply(activity_imputed$steps, activity_imputed$date, sum, na.rm = TRUE)

# Histogram after imputation
hist(steps_per_day_imputed,
     breaks = 20,
     col = "seagreen",
     main = "Total steps per day (after imputing missing values)",
     xlab = "Steps")
```

![](PA1_template_files/figure-html/totals_after_impute-1.png)<!-- -->

``` r
# Mean and median after imputation
mean_steps_imp <- mean(steps_per_day_imputed)
median_steps_imp <- median(steps_per_day_imputed)
mean_steps_imp; median_steps_imp
```

```
## [1] 10766.19
```

```
## [1] 10766.19
```

**Impact of imputation**: With interval-mean imputation, the **mean** daily steps remains essentially unchanged (**1.076619\times 10^{4}** vs. **1.076619\times 10^{4}**), while the **median** daily steps typically shifts slightly toward the mean (**1.076619\times 10^{4}** vs. **1.0765\times 10^{4}**). The distribution becomes more concentrated because missing-day totals are now filled rather than omitted.

# Are there differences in activity patterns between weekdays and weekends?
We classify each observation as **weekday** or **weekend**, then compare the average activity pattern.


``` r
# Create the weekday/weekend factor
is_weekend <- weekdays(activity_imputed$date) %in% c("Saturday", "Sunday")
activity_imputed$day_type <- factor(ifelse(is_weekend, "weekend", "weekday"))

# Compute the average steps by interval and day_type
interval_means_type <- aggregate(steps ~ interval + day_type,
                                data = activity_imputed,
                                FUN = mean)

# Panel time series plot using lattice
xyplot(steps ~ interval | day_type,
                data = interval_means_type,
                type = "l",
                layout = c(1, 2),
                xlab = "5-minute interval",
                ylab = "Average steps",
                main = "Weekday vs Weekend Activity Patterns")
```

![](PA1_template_files/figure-html/weekday_weekend-1.png)<!-- -->
