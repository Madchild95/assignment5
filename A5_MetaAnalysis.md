Building on the shoulders of giants: meta-analysis
==================================================

Questions to be answered
------------------------

1.  What is the current evidence for distinctive vocal patterns in
    schizophrenia? Report how many papers report quantitative estimates,
    comment on what percentage of the overall studies reviewed they
    represent (see PRISMA chart) your method to analyze them, the
    estimated effect size of the difference (mean effect size and
    standard error) and forest plots representing it. N.B. Only measures
    of pitch mean and pitch sd are required for the assignment. Feel
    free to ignore the rest (although pause behavior looks interesting,
    if you check my article).

2.  Do the results match your own analysis from Assignment 3? If you add
    your results to the meta-analysis, do the estimated effect sizes
    change? Report the new estimates and the new forest plots.

3.  Assess the quality of the literature: report and comment on
    heterogeneity of the studies (tau, I2), on publication bias (funnel
    plot), and on influential studies.

Tips on the process to follow:
------------------------------

-   Download the data on all published articles analyzing voice in
    schizophrenia and the prisma chart as reference of all articles
    found and reviewed
-   Look through the dataset to find out which columns to use, and if
    there is any additional information written as comments (real world
    data is always messy!).
    -   Hint: PITCH\_F0M and PITCH\_F0SD group of variables are what you
        need
    -   Hint: Make sure you read the comments in the columns:
        `pitch_f0_variability`, `frequency`, `Title`,
        `ACOUST_ANA_DESCR`, `DESCRIPTION`, and `COMMENTS`

<!-- -->

    library(pacman, tidyverse)
    p_load(readxl, tidyverse, lmer, lmerTest, metafor, meta)

    ## Warning: package 'lmer' is not available (for R version 3.6.1)

    ## Warning: 'BiocManager' not available.  Could not check Bioconductor.
    ## 
    ## Please use `install.packages('BiocManager')` and then retry.

    ## Warning in p_install(package, character.only = TRUE, ...):

    ## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
    ## logical.return = TRUE, : there is no package called 'lmer'

    ## Warning in p_load(readxl, tidyverse, lmer, lmerTest, metafor, meta): Failed to install/load:
    ## lmer

    #loading file
    xl <- read_excel("Matrix_MetaAnalysis_Diagnosis_updated290719.xlsx")

    ## New names:
    ## * frequency -> frequency...68
    ## * frequency -> frequency...73
    ## * frequency -> frequency...78
    ## * frequency -> frequency...83
    ## * frequency -> frequency...88
    ## * … and 7 more problems

    #selecting relevant variables
    xl_subset <- xl %>% select(
      StudyID, #studyID
      DIAGNOSIS, #diagnosis
      SAMPLE_SIZE_SZ, # n(sz)                     -> gives us the number of sz
      SAMPLE_SIZE_HC, # n(hc)                     -> gives us the number of hc
      # Feature 1:
      PITCH_F0_HC_M, # mean(mean(hc_pitch))       -> gives us the mean of pitch means, for hc (group mean of means)
      PITCH_F0_HC_SD, # sd(mean(hc_pitch))        -> gives us the variability in pitch means, for HC
      PITCH_F0_SZ_M, # mean(mean(sz_pitch))       -> gives us the mean of pitch means, for sz (group mean of means)
      PITCH_F0_SZ_SD, # sd(mean(sz_pitch))        -> gives us the variability in pitch means, for SZ 
      # Feature 2:
      PITCH_F0SD_HC_M, # mean(sd(hc_pitch))       -> gives us the mean of pitch variability, for HC
      PITCH_F0SD_HC_SD, # sd(sd(hc_pitch))        -> gives us how pitch variability, varies for HC
      PITCH_F0SD_SZ_M, # mean(sd(sz_pitch))       -> gives us the mean of pitch variability, for SZ
      PITCH_F0SD_SZ_SD, # sd(sd(sz_pitch))         -> gives us how pitch variability, varies for SZ
      TYPE_OF_TASK,
      Article
      )

    colnamelist <- c("studyID","diagnosis","n_sz","n_hc","m_m_hc","sd_m_hc","m_m_sz","sd_m_sz","m_sd_hc","sd_sd_hc","m_sd_sz","sd_sd_sz","task", "article")

    colnames(xl_subset) <- colnamelist
    # Above, HC is healthy controls and SZ is Schizophrenics
    # the _ means "of" in the column names

    #Below we check for comments to see if there is additional information we should take into account.
    comment_subset <- xl %>% select(
      StudyID,
      pitch_f0_variability, 
      frequency...68, 
      frequency...73, 
      frequency...78, 
      frequency...83, 
      frequency...88,
      Title)
    # Here we see multiple things:
    # 1. One of the Hz is in semitones, this we fix:
    xl_subset$m_m_hc <- ifelse(xl_subset$studyID == 18, NA, xl_subset$m_m_hc)
    xl_subset$m_m_sz <- ifelse(xl_subset$studyID == 18, NA, xl_subset$m_m_sz)

    # 2. study 15 will be overwrited with NA
    xl_subset$m_sd_hc <- ifelse(xl_subset$studyID == 15, NA, xl_subset$m_sd_hc)
    xl_subset$m_sd_sz <- ifelse(xl_subset$studyID == 15, NA, xl_subset$m_sd_sz)
    xl_subset$sd_sd_hc <- ifelse(xl_subset$studyID == 15, NA, xl_subset$sd_sd_hc)
    xl_subset$sd_sd_sz <- ifelse(xl_subset$studyID == 15, NA, xl_subset$sd_sd_sz)

    #task as factor
    xl_subset$task <- as.factor(xl_subset$task)

-   Following the procedure in the slides calculate effect size and
    standard error of the effect size per each study. N.B. we focus on
    pitch mean and pitch standard deviation. . first try using lmer (to
    connect to what you know of mixed effects models) . then use rma()
    (to get some juicy additional statistics)

<!-- -->

    xl_subset <- escalc("SMD", #"SMD" for the standardized mean difference.
                          n1i = n_sz,
                          n2i = n_hc,
                          m1i = m_m_sz,
                          m2i = m_m_hc,
                          sd1i = sd_m_sz,
                          sd2i = sd_m_hc,
                          data = xl_subset)
    colnames(xl_subset)[15] <- "ES_mean"
    colnames(xl_subset)[16] <- "var_ES_mean"

    xl_subset <- escalc("SMD", #"SMD" for the standardized mean difference.
                          n1i = n_sz,
                          n2i = n_hc,
                          m1i = m_sd_sz,
                          m2i = m_sd_hc,
                          sd1i = sd_sd_sz,
                          sd2i = sd_sd_hc,
                          data = xl_subset)
    colnames(xl_subset)[17] <- "ES_variability"
    colnames(xl_subset)[18] <- "var_ES_variability"


    #Difference of effect size / is manipulation doing a difference / baseline = level
    f1_model1 <- lmer(ES_mean ~ 1 + task + (1+task|article), #should be on article level,
         weights = 1/var_ES_mean, #vi = sqaure of sd 
         REML=F,
         control = lmerControl(
              check.nobs.vs.nlev="ignore", 
              check.nobs.vs.nRE="ignore"),
         data = xl_subset)

    ## boundary (singular) fit: see ?isSingular

    summary(f1_model1)

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: ES_mean ~ 1 + task + (1 + task | article)
    ##    Data: xl_subset
    ## Weights: 1/var_ES_mean
    ## Control: 
    ## lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##      0.8     -1.6      5.6    -11.2       -1 
    ## 
    ## Scaled residuals: 
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0015944  0.0000000  0.0004464  0.0004847  0.0012483 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev. Corr 
    ##  article  (Intercept) 1.040e-01 0.322546      
    ##           taskSOCIAL  1.040e-01 0.322546 -1.00
    ##  Residual             1.502e-06 0.001225      
    ## Number of obs: 5, groups:  article, 5
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)  
    ## (Intercept)   0.2768     0.1613  4.9960   1.716    0.147  
    ## taskSOCIAL   -0.3950     0.1613  4.9960  -2.449    0.058 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## taskSOCIAL -1.000
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    f2_model1 <- lmer(ES_variability ~ 1 + task + (1+task|article), #should be on article level,
         weights = 1/var_ES_variability, #vi = sqaure of sd 
         REML=F,
         control = lmerControl(
              check.nobs.vs.nlev="ignore", 
              check.nobs.vs.nRE="ignore"),
         data = xl_subset)

    ## boundary (singular) fit: see ?isSingular

    ## Warning: Model failed to converge with 1 negative eigenvalue: -2.4e-03

    summary(f2_model1)

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: ES_variability ~ 1 + task + (1 + task | article)
    ##    Data: xl_subset
    ## Weights: 1/var_ES_variability
    ## Control: 
    ## lmerControl(check.nobs.vs.nlev = "ignore", check.nobs.vs.nRE = "ignore")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     23.3     29.7     -1.7      3.3        4 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.30900 -0.24086  0.06515  0.11286  1.05464 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr       
    ##  article  (Intercept) 0.14760  0.3842              
    ##           taskFREE    0.03168  0.1780   -0.66      
    ##           taskSOCIAL  0.01315  0.1147    1.00 -0.66
    ##  Residual             0.05251  0.2292              
    ## Number of obs: 14, groups:  article, 11
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)  
    ## (Intercept)  -0.5389     0.1527  4.7337  -3.528   0.0184 *
    ## taskFREE      0.4840     0.2159  8.6108   2.242   0.0529 .
    ## taskSOCIAL   -0.4429     0.1043  2.3413  -4.244   0.0386 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr) tsFREE
    ## taskFREE   -0.708       
    ## taskSOCIAL  0.150 -0.106
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

-   Build a forest plot of the results (forest(model))

<!-- -->

    #Forst plot for feature 1
    f1_rma <- rma(ES_mean,var_ES_mean,data=xl_subset,slab=article)

    ## Warning in rma(ES_mean, var_ES_mean, data = xl_subset, slab = article):
    ## Studies with NAs omitted from model fitting.

    forest(f1_rma)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    summary(f1_rma)

    ## 
    ## Random-Effects Model (k = 5; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ##  -2.2022    4.4043    8.4043    7.1769   20.4043   
    ## 
    ## tau^2 (estimated amount of total heterogeneity): 0.1052 (SE = 0.1309)
    ## tau (square root of estimated tau^2 value):      0.3243
    ## I^2 (total heterogeneity / total variability):   57.80%
    ## H^2 (total variability / sampling variability):  2.37
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 4) = 9.4274, p-val = 0.0513
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub 
    ##   0.1518  0.1928  0.7873  0.4311  -0.2261  0.5297    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    #Forst plot for feature 2
    f2_rma <- rma(ES_variability,var_ES_variability,data=xl_subset,slab=article)

    ## Warning in rma(ES_variability, var_ES_variability, data = xl_subset, slab =
    ## article): Studies with NAs omitted from model fitting.

    forest(f2_rma)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-2.png)

    #Forest plot with fixed effect
    f1_rma_fix <- rma(ES_mean,var_ES_mean,mods=cbind(task),data=xl_subset,slab=article)

    ## Warning in rma(ES_mean, var_ES_mean, mods = cbind(task), data =
    ## xl_subset, : Studies with NAs omitted from model fitting.

    forest(f1_rma_fix)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-3.png)

    summary(f1_rma_fix)

    ## 
    ## Mixed-Effects Model (k = 5; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ##  -1.8585    3.7171    9.7171    7.0129   33.7171   
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.1362 (SE = 0.1854)
    ## tau (square root of estimated tau^2 value):             0.3691
    ## I^2 (residual heterogeneity / unaccounted variability): 60.81%
    ## H^2 (unaccounted variability / sampling variability):   2.55
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 3) = 8.2816, p-val = 0.0405
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.5337, p-val = 0.4651
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.4229  0.4177   1.0125  0.3113  -0.3957  1.2416    
    ## task      -0.1804  0.2469  -0.7305  0.4651  -0.6644  0.3036    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    f2_rma_fix <- rma(ES_variability,var_ES_variability,mods=cbind(task),data=xl_subset,slab=article)

    ## Warning in rma(ES_variability, var_ES_variability, mods = cbind(task), data
    ## = xl_subset, : Studies with NAs omitted from model fitting.

    forest(f2_rma_fix)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-3-4.png)

    summary(f2_rma_fix)

    ## 
    ## Mixed-Effects Model (k = 14; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ## -10.8470   21.6941   27.6941   29.1488   30.6941   
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2648 (SE = 0.1371)
    ## tau (square root of estimated tau^2 value):             0.5146
    ## I^2 (residual heterogeneity / unaccounted variability): 81.52%
    ## H^2 (unaccounted variability / sampling variability):   5.41
    ## R^2 (amount of heterogeneity accounted for):            10.78%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 12) = 54.3149, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 2.4704, p-val = 0.1160
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.1798  0.4408   0.4078  0.6834  -0.6842  1.0437    
    ## task      -0.3263  0.2076  -1.5717  0.1160  -0.7332  0.0806    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ?rma
    45+8+39+60+46+94+22+45+36+57 

    ## [1] 452

    (13+4+12+16+23+9+6+13+16) 

    ## [1] 112

    35+36+18+19+20+101+20+19+25+30 

    ## [1] 323

    (13+16+10+6+45+10+12+10+15)

    ## [1] 137

    452+143

    ## [1] 595

    323+153

    ## [1] 476

-   Go back to Assignment 3, add your own study to the data table, and
    re-run meta-analysis. Do the results change?

<!-- -->

    # Load non scaled data from assignment 3
    voice <- read.csv("features_unscaled.csv")

    # devide in subsets for hc and sz
    voice_sz <- subset(voice, Diagnosis == "1") 
    voice_hc <- subset(voice, Diagnosis == "0")

    # Make a new row where the right values are extracted
    new.row <- data.frame(
      studyID=51,
      article="Fusaroli et. al (2019)",
      diagnosis="SZ", 
      task = "SOCIAL", 
      n_sz = sum(n_distinct(voice_sz$Participant)),
      n_hc = sum(n_distinct(voice_hc$Participant)),
      m_m_hc = mean(voice_hc$mean_pitch),
      sd_m_hc = sd(voice_hc$mean_pitch),
      m_m_sz = mean(voice_sz$mean_pitch),
      sd_m_sz = sd(voice_sz$mean_pitch),
      m_sd_hc = mean(voice_hc$sd_pitch),
      sd_sd_hc = sd(voice_hc$sd_pitch),
      m_sd_sz = mean(voice_sz$sd_pitch),
      sd_sd_sz = sd(voice_sz$sd_pitch))

    # add escalc values for both features
    new.row <- escalc("SMD", #"SMD" for the standardized mean difference.
                          n1i = n_sz,
                          n2i = n_hc,
                          m1i = m_m_sz,
                          m2i = m_m_hc,
                          sd1i = sd_m_sz,
                          sd2i = sd_m_hc,
                          data = new.row)
    colnames(new.row)[15] <- "ES_mean"
    colnames(new.row)[16] <- "var_ES_mean"

    new.row <- escalc("SMD", #"SMD" for the standardized mean difference.
                          n1i = n_sz,
                          n2i = n_hc,
                          m1i = m_sd_sz,
                          m2i = m_sd_hc,
                          sd1i = sd_sd_sz,
                          sd2i = sd_sd_hc,
                          data = new.row)
    colnames(new.row)[17] <- "ES_variability"
    colnames(new.row)[18] <- "var_ES_variability"

    # Merge new row with the dataframe xl_subset
    xl_subset <- rbind(xl_subset, new.row)

-   Now look at the output of rma() and check tau and I2

<!-- -->

    #Forst plot for feature 1
    f1_rma_new <- rma(ES_mean,var_ES_mean,mods=cbind(task),data=xl_subset,slab=article)

    ## Warning in rma(ES_mean, var_ES_mean, mods = cbind(task), data =
    ## xl_subset, : Studies with NAs omitted from model fitting.

    summary(f1_rma_new)

    ## 
    ## Mixed-Effects Model (k = 6; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ##  -1.8526    3.7052    9.7052    7.8641   33.7052   
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.0826 (SE = 0.1062)
    ## tau (square root of estimated tau^2 value):             0.2874
    ## I^2 (residual heterogeneity / unaccounted variability): 57.37%
    ## H^2 (unaccounted variability / sampling variability):   2.35
    ## R^2 (amount of heterogeneity accounted for):            0.00%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 4) = 8.8334, p-val = 0.0654
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.5754, p-val = 0.4481
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.3492  0.3327   1.0496  0.2939  -0.3029  1.0012    
    ## task      -0.1193  0.1572  -0.7585  0.4481  -0.4274  0.1889    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    forest(f1_rma_new)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    #Forst plot for feature 2
    f2_rma_new <- rma(ES_variability,var_ES_variability,mods=cbind(task),data=xl_subset,slab=article)

    ## Warning in rma(ES_variability, var_ES_variability, mods = cbind(task), data
    ## = xl_subset, : Studies with NAs omitted from model fitting.

    forest(f2_rma_new)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-5-2.png)

    summary(f2_rma_new)

    ## 
    ## Mixed-Effects Model (k = 15; tau^2 estimator: REML)
    ## 
    ##   logLik  deviance       AIC       BIC      AICc 
    ## -11.7650   23.5299   29.5299   31.2248   32.1966   
    ## 
    ## tau^2 (estimated amount of residual heterogeneity):     0.2677 (SE = 0.1312)
    ## tau (square root of estimated tau^2 value):             0.5174
    ## I^2 (residual heterogeneity / unaccounted variability): 83.45%
    ## H^2 (unaccounted variability / sampling variability):   6.04
    ## R^2 (amount of heterogeneity accounted for):            1.38%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 13) = 62.2300, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.5718, p-val = 0.2100
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub 
    ## intrcpt    0.0619  0.4296   0.1440  0.8855  -0.7802  0.9039    
    ## task      -0.2441  0.1947  -1.2537  0.2100  -0.6258  0.1375    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    #Funnel plots
    funnel(f1_rma_new, main = "Funnel plot feature 1", xlab = "Standardized Mean Difference")

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-5-3.png)

    funnel(f2_rma_new, main = "Funnel plot feature 2", xlab = "Standardized Mean Difference")

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-5-4.png)

-   Checking for influential studies

<!-- -->

    #Checking for influential data points for feature 1
    inf_1 <- influence(f1_rma_new)
    print(inf_1)

    ## $inf
    ## 
    ##                        rstudent  dffits cook.d  cov.r tau2.del QE.del 
    ## Pinheiro et al. (2016)   0.3569  0.1579 0.0287 1.8916   0.1110 8.3310 
    ## Martínez et al. (2015)  -2.7941 -2.1151 1.7447 0.1561   0.0000 1.0262 
    ## Graux et al. (2011)      1.4712  0.9892 0.7237 0.6020   0.0397 4.9914 
    ## Pinheiro et al. (2017)   0.3496  0.1481 0.0249 1.8283   0.1097 8.3899 
    ## Thanh (2018)            -0.3250 -0.2821 0.1152 3.5376   0.1362 8.2816 
    ## Fusaroli et. al (2019)   0.3250  0.3717 0.2190 4.4216   0.1362 8.2816 
    ##                           hat  weight inf 
    ## Pinheiro et al. (2016) 0.2159 12.2025     
    ## Martínez et al. (2015) 0.3207 18.1293   * 
    ## Graux et al. (2011)    0.2628 14.8548     
    ## Pinheiro et al. (2017) 0.2006 11.3426     
    ## Thanh (2018)           0.4185 18.1940     
    ## Fusaroli et. al (2019) 0.5815 25.2769     
    ## 
    ## $dfbs
    ## 
    ##                        intrcpt    task 
    ## Pinheiro et al. (2016)  0.1410 -0.0920 
    ## Martínez et al. (2015) -1.7417  1.3080 
    ## Graux et al. (2011)     0.9514 -0.7252 
    ## Pinheiro et al. (2017)  0.1316 -0.0857 
    ## Thanh (2018)            0.0534 -0.1774 
    ## Fusaroli et. al (2019) -0.1914  0.3269

    plot(inf_1)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    #Checking for influential data points for feature 2
    inf_2 <- influence(f2_rma_new)
    print(inf_2)

    ## $inf
    ## 
    ##                         rstudent  dffits cook.d  cov.r tau2.del  QE.del 
    ## Martínez et al. (2015)   -0.0834 -0.0449 0.0027 1.5126   0.3000 62.2063 
    ## Bedwell et al. (2014)     1.0067  0.2414 0.0587 1.0743   0.2705 60.5934 
    ## Rapcan et al. (2010)     -0.2570 -0.1263 0.0177 1.4502   0.2961 61.9932 
    ## Dickey et al. (2012).1   -0.3667 -0.1813 0.0358 1.4443   0.2945 61.6690 
    ## Cohen et al. (2008)      -0.1927 -0.0382 0.0018 1.2673   0.2970 61.2532 
    ## Alpert et al. (2000)     -0.4163 -0.1668 0.0296 1.3551   0.2899 58.2422 
    ## Compton et al. (2018).1   0.7011  0.2116 0.0479 1.2124   0.2869 59.1345 
    ## Compton et al. (2018).2   0.5226  0.1601 0.0281 1.2563   0.2938 61.4901 
    ## Kliper et al. (2015)     -2.2860 -0.9494 0.7138 0.6409   0.1835 45.2374 
    ## Ross et al. (2001).1     -1.9186 -0.8665 0.6094 0.7423   0.1993 44.7586 
    ## Ross et al. (2001).2     -1.7391 -0.8227 0.5901 0.9135   0.2218 51.9885 
    ## Meaux et al. (2018).1     1.1204  0.2984 0.0880 1.0452   0.2635 58.6440 
    ## Meaux et al. (2018).2     1.1633  0.3085 0.0932 1.0289   0.2609 58.3192 
    ## Thanh (2018)              0.4565  0.2272 0.0562 1.4223   0.2960 62.1583 
    ## Fusaroli et. al (2019)    1.1122  0.5491 0.2985 1.2242   0.2648 54.3149 
    ##                            hat weight inf 
    ## Martínez et al. (2015)  0.2059 7.0211     
    ## Bedwell et al. (2014)   0.0536 5.3170     
    ## Rapcan et al. (2010)    0.1876 6.3988     
    ## Dickey et al. (2012).1  0.1921 6.5493     
    ## Cohen et al. (2008)     0.0666 6.6058     
    ## Alpert et al. (2000)    0.1601 6.4759     
    ## Compton et al. (2018).1 0.0783 7.7656     
    ## Compton et al. (2018).2 0.0755 7.4961     
    ## Kliper et al. (2015)    0.1369 5.5374     
    ## Ross et al. (2001).1    0.1524 6.1673     
    ## Ross et al. (2001).2    0.1869 6.3740     
    ## Meaux et al. (2018).1   0.0672 6.6660     
    ## Meaux et al. (2018).2   0.0672 6.6645     
    ## Thanh (2018)            0.1731 7.0029     
    ## Fusaroli et. al (2019)  0.1967 7.9584     
    ## 
    ## $dfbs
    ## 
    ##                         intrcpt    task 
    ## Martínez et al. (2015)  -0.0491  0.0491 
    ## Bedwell et al. (2014)    0.1022 -0.0197 
    ## Rapcan et al. (2010)    -0.1267  0.1136 
    ## Dickey et al. (2012).1  -0.1795  0.1577 
    ## Cohen et al. (2008)     -0.0297  0.0180 
    ## Alpert et al. (2000)     0.0854 -0.1299 
    ## Compton et al. (2018).1  0.0821 -0.0086 
    ## Compton et al. (2018).2  0.0570 -0.0009 
    ## Kliper et al. (2015)     0.4787 -0.7453 
    ## Ross et al. (2001).1     0.4309 -0.6692 
    ## Ross et al. (2001).2    -0.7845  0.6449 
    ## Meaux et al. (2018).1    0.1304 -0.0288 
    ## Meaux et al. (2018).2    0.1361 -0.0312 
    ## Thanh (2018)            -0.1119  0.1740 
    ## Fusaroli et. al (2019)  -0.2756  0.4236

    plot(inf_2)

![](A5_MetaAnalysis_files/figure-markdown_strict/unnamed-chunk-6-2.png)
