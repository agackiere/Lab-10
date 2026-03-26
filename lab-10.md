Lab 10 - Grading the professor
================
Anaelle Gackiere
03-27-2026

Here is a link to the [lab
instructions](https://datascience4psych.github.io/DataScience4Psych/lab10.html).

## Load Packages and Data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
```

# Part 1

## Exercise 1

The distribution of scores is negatively skewed, so students are
relatively generous. The mean score is 4.175, and the minimum score
given was 2.3 (at least not a 0!).

``` r
# descriptives
summary(evals$score)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.300   3.800   4.300   4.175   4.600   5.000

``` r
# visualization
ggplot(evals, aes(x = score)) +
  geom_bar( fill = "plum4", bins = 30) +
  theme_minimal()
```

    ## Warning in geom_bar(fill = "plum4", bins = 30): Ignoring unknown parameters:
    ## `bins`

![](lab-10_files/figure-gfm/exercise1_code-1.png)<!-- -->

## Exercise 2

I actually initially did geom_jitter, but doing geom_point is
problematic for discrete variables, and will result in a strange looking
scatterplot like this one below (see description at exercise 3). The
issue with geom_point here is that we are not (EXPLAIN).

``` r
ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(title = "Profesor Evaluation Score vs Average Beauty",
       x = "Average Professor Beauty Rating",
       y = "Evaluation Score") +
  theme_classic() 
```

![](lab-10_files/figure-gfm/exercise2_code-1.png)<!-- -->

## Exercise 3

``` r
ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_jitter() +
  labs(title = "Profesor Evaluation Score vs Average Beauty",
       x = "Average Professor Beauty Rating",
       y = "Evaluation Score") +
  theme_classic() 
```

![](lab-10_files/figure-gfm/exercise3-code-1.png)<!-- -->

We have a few outliers of low scores with low attractiveness, but
overall most of the points are high regardless of beauty level. After
closer inspection, there might be a somewhat positive trend between
beauty and score, but it’s unclear to what extent and whether or not
it’s linear.

# Part 2

### Exercise 4

The fitted model is: **y = 3.88 + 0.0666 × bty_avg** For every one-point
increase in average beauty rating, the model predicts evaluation score
increases by about 0.07 points. For exampple, if a professor goes from a
beauty rating of 4 to a 5, their predicted evaluation score goes up by
0.07.

``` r
# model
m_bty <- lm(score ~ bty_avg, data = evals)

# view output
m_bty
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Coefficients:
    ## (Intercept)      bty_avg  
    ##     3.88034      0.06664

``` r
# effect check
glance(m_bty)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0350        0.0329 0.535      16.7 0.0000508     1  -366.  738.  751.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

### Exercise 5

``` r
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_jitter(width = 0.1, height = 0.1, color = "black", alpha = 0.5) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  labs(
    title = "Evaluation Score vs. Beauty Rating",
    subtitle = "Regression Line in Orange",
    x = "Average Professor Beauty Rating",
    y = "Evaluation Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](lab-10_files/figure-gfm/regression-plot-1.png)<!-- -->

### Exercise 6

As mentioned before, for every one-point increase in average beauty
rating, the model predicts evaluation score increases by about 0.07
points; thus, as beauty ratings increase, so do evaluation scores. The
intercept is not meaningful here since there is no true 0. This is a
statistically detectable but practically trivial effect, since beauty
explains only 3.5% of the variation in scores (R² = 0.035), meaning the
vast majority of what drives evaluation scores has nothing to do with
looks. Right now, it seems that beauty is a minor piece of the story.
The issue with the shading here is it can make a trivial relationship
look like a big deal just because it takes up visual space.

# Part 3

### Exercise 7

The reference level is “Female”; R can’t put “female” and “male” into a
math equation directly, so it turns one into 0 and the other into 1.
Female = 0 (the baseline), male = 1. The two coefficients are the
Intercept = 4.09, which is the predicted score for female professors
(the reference level) and gendermale = 0.14. Male professors score 0.14
points higher than female professors on average

Predicted mean evaluation score for each gender:

Female: 4.09 + 0.14(0) = 4.09

Male: 4.09 + 0.14(1) = 4.23

``` r
m_gen <- lm(score ~ gender, data = evals)
tidy(m_gen)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)    4.09     0.0387    106.   0      
    ## 2 gendermale     0.142    0.0508      2.78 0.00558

### Exercise 8

``` r
# new variables

evals <- evals %>%
  mutate(
    rank_relevel = relevel(factor(rank), ref = "tenure track"),
    tenure_eligible = ifelse(rank == "teaching", "no", "yes")
  )

# Model 1, og rank variable 
m_rank <- lm(score ~ rank, data = evals)

# Model 2, rank with tenure track as reference
m_rank_relevel <- lm(score ~ rank_relevel, data = evals)

# Model 3, tenure eligible variable
m_tenure_eligible <- lm(score ~ tenure_eligible, data = evals)

tidy(m_rank)
```

    ## # A tibble: 3 × 5
    ##   term             estimate std.error statistic   p.value
    ##   <chr>               <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         4.28     0.0537     79.9  1.02e-271
    ## 2 ranktenure track   -0.130    0.0748     -1.73 8.37e-  2
    ## 3 ranktenured        -0.145    0.0636     -2.28 2.28e-  2

``` r
tidy(m_rank_relevel)
```

    ## # A tibble: 3 × 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            4.15      0.0521    79.7   2.58e-271
    ## 2 rank_relevelteaching   0.130     0.0748     1.73  8.37e-  2
    ## 3 rank_releveltenured   -0.0155    0.0623    -0.249 8.04e-  1

``` r
tidy(m_tenure_eligible)
```

    ## # A tibble: 2 × 5
    ##   term               estimate std.error statistic   p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           4.28     0.0536     79.9  2.72e-272
    ## 2 tenure_eligibleyes   -0.141    0.0607     -2.32 2.10e-  2

``` r
glance(m_rank)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0116       0.00733 0.542      2.71  0.0679     2  -372.  752.  768.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
glance(m_rank_relevel)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0116       0.00733 0.542      2.71  0.0679     2  -372.  752.  768.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
glance(m_tenure_eligible)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0115       0.00935 0.541      5.36  0.0210     1  -372.  750.  762.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

### Exercise 9

Model 1 uses teaching as the reference level, giving an intercept of
4.28, meaning teaching faculty have a predicted evaluation score of
4.28. Tenure track faculty score 0.13 points lower (4.15) and tenured
faculty score 0.15 points lower (4.13) than teaching faculty. Model 2
switches the reference to tenure track, giving an intercept of 4.15.
Teaching faculty score 0.13 points higher (4.28) than tenure track,
while tenured faculty score essentially the same as tenure track, only
0.015 points lower (4.14). Model 3 collapses tenure track and tenured
into one “yes” group, with teaching as the reference at 4.28. Tenure
eligible faculty score 0.14 points lower (4.14) than teaching faculty.
Overall, teaching faculty score slightly higher than tenure track and
tenured faculty, but the differences are small.

### Exercise 10

R-squared for Model 1: 0.01 (p = 0.068)

R-squared for Model 2: 0.01 (p = 0.068)

R-squared for Model 3: 0.01 (p \< 0.05)

Based on the R-squared of 0.01, rank explains only 1% of the variation
in evaluation scores. Even though the third model’s R-squared is
statistically significant, that does not mean much, practically
speaking.

# Part 4

Is the “beauty effect” still there once we account for gender?

``` r
# beauty only
m_bty <- lm(score ~ bty_avg, data = evals)
tidy(m_bty)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.88      0.0761     51.0  1.56e-191
    ## 2 bty_avg       0.0666    0.0163      4.09 5.08e-  5

``` r
glance(m_bty)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0350        0.0329 0.535      16.7 0.0000508     1  -366.  738.  751.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
# beauty + gender 
m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
tidy(m_bty_gen)
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.75      0.0847     44.3  6.23e-168
    ## 2 bty_avg       0.0742    0.0163      4.56 6.48e-  6
    ## 3 gendermale    0.172     0.0502      3.43 6.52e-  4

``` r
glance(m_bty_gen)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic     p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>       <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0591        0.0550 0.529      14.5 0.000000818     2  -360.  729.  745.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

### Exercise 11

When gender is added, the beauty slope increases slightly from 0.067 to
0.074.

### Exercise 12

For two professors with the same beauty rating, since the gender
coefficient is 0.17, the male professor is still predicted to score 0.17
points higher.

### Exercise 13

Adjusted R-squared for Beauty only Model: 0.033

Adjusted R-squared for Beauty and Gender Model: 0.055

Beauty is doing most of the work. Adding gender helps a little, since it
bumps the adjusted r-squared from 0.033 to 0.055, but that’s only a 2
percentage point improvement. Both models are still weak overall and
neither beauty nor gender explains evaluation scores in a meaningful
way.

### Exercise 14

``` r
m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
tidy(m_bty_rank)
```

    ## # A tibble: 4 × 5
    ##   term             estimate std.error statistic   p.value
    ##   <chr>               <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)        3.98      0.0908     43.9  2.92e-166
    ## 2 bty_avg            0.0678    0.0165      4.10 4.92e-  5
    ## 3 ranktenure track  -0.161     0.0740     -2.17 3.03e-  2
    ## 4 ranktenured       -0.126     0.0627     -2.01 4.45e-  2

``` r
glance(m_bty_rank)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0465        0.0403 0.533      7.46 0.0000688     3  -363.  737.  758.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

The intercept is 3.98, which is the predicted score for teaching faculty
with a beauty rating of 0. The beauty slope is 0.068, meaning for every
one point increase in beauty rating, evaluation scores increase by 0.068
points regardless of rank. Even after controlling for rank, beauty still
has an effect.

For rank, tenure track professors score 0.16 points lower than teaching
faculty and tenured professors score 0.13 points lower than teaching
faculty, holding beauty constant. Teaching faculty are still the
highest.

# Part 5

: The Search for the Best Model Going forward, only consider the
following variables as potential predictors: rank, ethnicity, gender,
language, age, cls_perc_eval, cls_did_eval, cls_students, cls_level,
cls_profs, cls_credits, bty_avg.

Which variable, on its own, would you expect to be the worst predictor
of evaluation scores? Why? Hint: Think about which variable would you
expect to not have any association with the professor’s score.

Check your suspicions from the previous exercise. Include the model
output for that variable in your response.

Suppose you wanted to fit a full model with the variables listed above.
If you are already going to include cls_perc_eval and cls_students,
which variable should you not include as an additional predictor? Why?

Fit a full model with all predictors listed above (except for the one
you decided to exclude) in the previous question.

Using backward-selection with adjusted R-squared as the selection
criterion, determine the best model. You do not need to show all steps
in your answer, just the output for the final model. Also, write out the
linear model for predicting score based on the final model you settle
on.

Interpret the slopes of one numerical and one categorical predictor
based on your final model.

Based on your final model, describe the characteristics of a professor
and course at University of Texas at Austin that would be associated
with a high evaluation score.

Would you be comfortable generalizing your conclusions to apply to
professors generally (at any university)? Why or why not?

I would be hesitant to generalize my conclusions to professors at any
university, as students and professors are nested in the university, so
I would want to run an MLM to test this across multiple universities,
and then we may be able to generalize more confidently to the general
university population.
