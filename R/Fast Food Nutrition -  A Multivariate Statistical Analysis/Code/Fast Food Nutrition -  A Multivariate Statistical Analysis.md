**Coding Part for Mini-Project**


```R
# Installing Packages
# https://www.kaggle.com/datasets/ulrikthygepedersen/fastfood-nutrition
install.packages(c("readr", "dplyr", "FactoMineR", "factoextra", "MVar", "fastDummies", "ggplot2", "corrplot","schoolmath","CCP","CCA","psych","biotools"))
```

    Installing packages into ‘/usr/local/lib/R/site-library’
    (as ‘lib’ is unspecified)
    
    


```R
# Load libraries
library(readr)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(MVar)
library(fastDummies)
library(ggplot2)
library(corrplot)
library(schoolmath)
library(CCA)
library(CCP)
library(MASS)
library(biotools)
library(psych)

# Load dataset
data <- read_csv("fastfood.csv")

# Inspect the data
print(head(data))
#  structure of fastfood data
print(str(data))
print(summary(data))
```

    ---
    biotools version 4.3
    
    
    Attaching package: ‘psych’
    
    
    The following object is masked from ‘package:fields’:
    
        describe
    
    
    The following objects are masked from ‘package:ggplot2’:
    
        %+%, alpha
    
    
    [1mRows: [22m[34m515[39m [1mColumns: [22m[34m17[39m
    [36m──[39m [1mColumn specification[22m [36m────────────────────────────────────────────────────────[39m
    [1mDelimiter:[22m ","
    [31mchr[39m  (3): restaurant, item, salad
    [32mdbl[39m (14): calories, cal_fat, total_fat, sat_fat, trans_fat, cholesterol, sod...
    
    [36mℹ[39m Use `spec()` to retrieve the full column specification for this data.
    [36mℹ[39m Specify the column types or set `show_col_types = FALSE` to quiet this message.
    

    [90m# A tibble: 6 × 17[39m
      restaurant item       calories cal_fat total_fat sat_fat trans_fat cholesterol
      [3m[90m<chr>[39m[23m      [3m[90m<chr>[39m[23m         [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m     [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m     [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m
    [90m1[39m Mcdonalds  Artisan G…      380      60         7       2       0            95
    [90m2[39m Mcdonalds  Single Ba…      840     410        45      17       1.5         130
    [90m3[39m Mcdonalds  Double Ba…     [4m1[24m130     600        67      27       3           220
    [90m4[39m Mcdonalds  Grilled B…      750     280        31      10       0.5         155
    [90m5[39m Mcdonalds  Crispy Ba…      920     410        45      12       0.5         120
    [90m6[39m Mcdonalds  Big Mac         540     250        28      10       1            80
    [90m# ℹ 9 more variables: sodium <dbl>, total_carb <dbl>, fiber <dbl>, sugar <dbl>,[39m
    [90m#   protein <dbl>, vit_a <dbl>, vit_c <dbl>, calcium <dbl>, salad <chr>[39m
    spc_tbl_ [515 × 17] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
     $ restaurant : chr [1:515] "Mcdonalds" "Mcdonalds" "Mcdonalds" "Mcdonalds" ...
     $ item       : chr [1:515] "Artisan Grilled Chicken Sandwich" "Single Bacon Smokehouse Burger" "Double Bacon Smokehouse Burger" "Grilled Bacon Smokehouse Chicken Sandwich" ...
     $ calories   : num [1:515] 380 840 1130 750 920 540 300 510 430 770 ...
     $ cal_fat    : num [1:515] 60 410 600 280 410 250 100 210 190 400 ...
     $ total_fat  : num [1:515] 7 45 67 31 45 28 12 24 21 45 ...
     $ sat_fat    : num [1:515] 2 17 27 10 12 10 5 4 11 21 ...
     $ trans_fat  : num [1:515] 0 1.5 3 0.5 0.5 1 0.5 0 1 2.5 ...
     $ cholesterol: num [1:515] 95 130 220 155 120 80 40 65 85 175 ...
     $ sodium     : num [1:515] 1110 1580 1920 1940 1980 950 680 1040 1040 1290 ...
     $ total_carb : num [1:515] 44 62 63 62 81 46 33 49 35 42 ...
     $ fiber      : num [1:515] 3 2 3 2 4 3 2 3 2 3 ...
     $ sugar      : num [1:515] 11 18 18 18 18 9 7 6 7 10 ...
     $ protein    : num [1:515] 37 46 70 55 46 25 15 25 25 51 ...
     $ vit_a      : num [1:515] 4 6 10 6 6 10 10 0 20 20 ...
     $ vit_c      : num [1:515] 20 20 20 25 20 2 2 4 4 6 ...
     $ calcium    : num [1:515] 20 20 50 20 20 15 10 2 15 20 ...
     $ salad      : chr [1:515] "Other" "Other" "Other" "Other" ...
     - attr(*, "spec")=
      .. cols(
      ..   restaurant = [31mcol_character()[39m,
      ..   item = [31mcol_character()[39m,
      ..   calories = [32mcol_double()[39m,
      ..   cal_fat = [32mcol_double()[39m,
      ..   total_fat = [32mcol_double()[39m,
      ..   sat_fat = [32mcol_double()[39m,
      ..   trans_fat = [32mcol_double()[39m,
      ..   cholesterol = [32mcol_double()[39m,
      ..   sodium = [32mcol_double()[39m,
      ..   total_carb = [32mcol_double()[39m,
      ..   fiber = [32mcol_double()[39m,
      ..   sugar = [32mcol_double()[39m,
      ..   protein = [32mcol_double()[39m,
      ..   vit_a = [32mcol_double()[39m,
      ..   vit_c = [32mcol_double()[39m,
      ..   calcium = [32mcol_double()[39m,
      ..   salad = [31mcol_character()[39m
      .. )
     - attr(*, "problems")=<externalptr> 
    NULL
      restaurant            item              calories         cal_fat      
     Length:515         Length:515         Min.   :  20.0   Min.   :   0.0  
     Class :character   Class :character   1st Qu.: 330.0   1st Qu.: 120.0  
     Mode  :character   Mode  :character   Median : 490.0   Median : 210.0  
                                           Mean   : 530.9   Mean   : 238.8  
                                           3rd Qu.: 690.0   3rd Qu.: 310.0  
                                           Max.   :2430.0   Max.   :1270.0  
                                                                            
       total_fat         sat_fat         trans_fat      cholesterol    
     Min.   :  0.00   Min.   : 0.000   Min.   :0.000   Min.   :  0.00  
     1st Qu.: 14.00   1st Qu.: 4.000   1st Qu.:0.000   1st Qu.: 35.00  
     Median : 23.00   Median : 7.000   Median :0.000   Median : 60.00  
     Mean   : 26.59   Mean   : 8.153   Mean   :0.465   Mean   : 72.46  
     3rd Qu.: 35.00   3rd Qu.:11.000   3rd Qu.:1.000   3rd Qu.: 95.00  
     Max.   :141.00   Max.   :47.000   Max.   :8.000   Max.   :805.00  
                                                                       
         sodium       total_carb         fiber            sugar       
     Min.   :  15   Min.   :  0.00   Min.   : 0.000   Min.   : 0.000  
     1st Qu.: 800   1st Qu.: 28.50   1st Qu.: 2.000   1st Qu.: 3.000  
     Median :1110   Median : 44.00   Median : 3.000   Median : 6.000  
     Mean   :1247   Mean   : 45.66   Mean   : 4.137   Mean   : 7.262  
     3rd Qu.:1550   3rd Qu.: 57.00   3rd Qu.: 5.000   3rd Qu.: 9.000  
     Max.   :6080   Max.   :156.00   Max.   :17.000   Max.   :87.000  
                                     NA's   :12                       
        protein           vit_a            vit_c           calcium      
     Min.   :  1.00   Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
     1st Qu.: 16.00   1st Qu.:  4.00   1st Qu.:  4.00   1st Qu.:  8.00  
     Median : 24.50   Median : 10.00   Median : 10.00   Median : 20.00  
     Mean   : 27.89   Mean   : 18.86   Mean   : 20.17   Mean   : 24.85  
     3rd Qu.: 36.00   3rd Qu.: 20.00   3rd Qu.: 30.00   3rd Qu.: 30.00  
     Max.   :186.00   Max.   :180.00   Max.   :400.00   Max.   :290.00  
     NA's   :1        NA's   :214      NA's   :210      NA's   :210     
        salad          
     Length:515        
     Class :character  
     Mode  :character  
                       
                       
                       
                       
    

**Data Preparation**


```R
# remove duplicates
data <- data[!duplicated(data), ]

# Removes rows with any NA values
data <- na.omit(data)
numeric_data <- data %>%
  dplyr::select(where(is.numeric))

# removing Outliers
remove_outliers <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) {
      Q1 <- quantile(x, 0.25, na.rm = TRUE)
      Q3 <- quantile(x, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      x[x < lower | x > upper] <- NA
    }
    x
  })
  df <- na.omit(df)
  return(df)
}

numeric_data <- remove_outliers(numeric_data)

# standardize data
data_scaled <- as.data.frame(scale(numeric_data))
colnames(data_scaled) <- colnames(numeric_data)
```

**Principal Component Analysis (PCA)**


```R
pca_result <- prcomp(data_scaled, center = FALSE, scale. = FALSE)

# View summary of PCA results
summary(pca_result)
```


    Importance of components:
                              PC1    PC2     PC3     PC4     PC5    PC6     PC7
    Standard deviation     2.5214 1.6449 1.13260 1.07329 0.83300 0.6980 0.67135
    Proportion of Variance 0.4541 0.1933 0.09163 0.08228 0.04956 0.0348 0.03219
    Cumulative Proportion  0.4541 0.6474 0.73901 0.82129 0.87085 0.9056 0.93784
                              PC8     PC9    PC10    PC11    PC12    PC13    PC14
    Standard deviation     0.5600 0.48564 0.39733 0.28615 0.24109 0.14843 0.02804
    Proportion of Variance 0.0224 0.01685 0.01128 0.00585 0.00415 0.00157 0.00006
    Cumulative Proportion  0.9603 0.97709 0.98837 0.99422 0.99837 0.99994 1.00000



```R
fviz_pca_var(pca_result,
col.var = "contrib", # Color by contributions to the PC
gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
repel = TRUE)
```


    
![png](output_7_0.png)
    



```R
# Calculate explained variance
explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2)
explained_var_percent <- explained_var * 100

# Scree plot using ggplot2
scree_df <- data.frame(PC = 1:length(explained_var_percent),ExplainedVariance = explained_var_percent)

ggplot(scree_df, aes(x = PC, y = ExplainedVariance)) +geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +geom_line(color = "red", size = 1) +geom_point(color = "red", size = 2) +labs(
    title = "Scree Plot: Explained Variance by PCA Components",
    x = "Principal Component",
    y = "Explained Variance (%)"
  ) +
  theme_minimal()

# Print the percentage for each PC (all PCs)
for (i in seq_along(explained_var_percent)) {
  cat("PC", i, "explains", round(explained_var_percent[i], 2), "% of the variance\n")
}

# Print total explained variance by first two components
total_var_2 <- sum(explained_var_percent[1:2])
cat("Total Explained Variance by First 2 Components:", round(total_var_2, 1), "%\n\n")

# Print loadings for the first and second principal components
cat("PCA Component Loadings (PC1 and PC2):\n")
print(round(pca_result$rotation[, 1:2], 6))
```

    Warning message:
    “[1m[22mUsing `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    [36mℹ[39m Please use `linewidth` instead.”
    

    PC 1 explains 45.41 % of the variance
    PC 2 explains 19.33 % of the variance
    PC 3 explains 9.16 % of the variance
    PC 4 explains 8.23 % of the variance
    PC 5 explains 4.96 % of the variance
    PC 6 explains 3.48 % of the variance
    PC 7 explains 3.22 % of the variance
    PC 8 explains 2.24 % of the variance
    PC 9 explains 1.68 % of the variance
    PC 10 explains 1.13 % of the variance
    PC 11 explains 0.58 % of the variance
    PC 12 explains 0.42 % of the variance
    PC 13 explains 0.16 % of the variance
    PC 14 explains 0.01 % of the variance
    Total Explained Variance by First 2 Components: 64.7 %
    
    PCA Component Loadings (PC1 and PC2):
                      PC1       PC2
    calories     0.371321 -0.009872
    cal_fat      0.349468 -0.193527
    total_fat    0.349001 -0.195025
    sat_fat      0.334564 -0.152444
    trans_fat    0.210851 -0.237565
    cholesterol  0.302057 -0.196447
    sodium       0.297842  0.063483
    total_carb   0.281559  0.280438
    fiber        0.108390  0.474319
    sugar        0.209624  0.266095
    protein      0.308376 -0.026004
    vit_a        0.077625  0.349037
    vit_c       -0.008072  0.410736
    calcium      0.213772  0.369432
    


    
![png](output_8_2.png)
    


**Factor Analysis (FA)**


```R
# Check data suitability
KMO(numeric_data)
cortest.bartlett(numeric_data)
```


    Kaiser-Meyer-Olkin factor adequacy
    Call: KMO(r = numeric_data)
    Overall MSA =  0.72
    MSA for each item = 
       calories     cal_fat   total_fat     sat_fat   trans_fat cholesterol 
           0.92        0.80        0.80        0.68        0.63        0.65 
         sodium  total_carb       fiber       sugar     protein       vit_a 
           0.84        0.67        0.62        0.62        0.63        0.62 
          vit_c     calcium 
           0.54        0.70 


    R was not square, finding R from data
    
    


<dl>
	<dt>$chisq</dt>
		<dd>3949.44307850366</dd>
	<dt>$p.value</dt>
		<dd>0</dd>
	<dt>$df</dt>
		<dd>91</dd>
</dl>




```R
# Determine Number of Factors
# Parallel analysis and scree plot
fa.parallel(numeric_data, fa="fa")
scree(numeric_data, factors=FALSE)
```

    R_zmq_msg_send errno: 4 strerror: Interrupted system call
    


    
![png](output_11_1.png)
    



    
![png](output_11_2.png)
    



```R
# Factor Extraction
fa_results <- psych::fa(numeric_data, nfactors=3, rotate="varimax", fm="minres", scores="Anderson")
```

    Warning message in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    “The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method.”
    


```R
# Interpret Results
# Loadings table (|loading| >0.3 considered meaningful)
print(fa_results$loadings, cutoff=0.3)

# Variance explained
fa_results$Vaccounted  # Shows cumulative variance [2]
```

    
    Loadings:
                MR1    MR2    MR3   
    calories     0.808  0.495       
    cal_fat      0.890              
    total_fat    0.890              
    sat_fat      0.819              
    trans_fat    0.581              
    cholesterol  0.924              
    sodium       0.584  0.403       
    total_carb   0.353  0.879       
    fiber               0.766       
    sugar        0.308  0.420  0.365
    protein      0.749         0.314
    vit_a               0.324  0.398
    vit_c                      0.836
    calcium             0.707       
    
                     MR1   MR2   MR3
    SS loadings    5.305 2.784 1.418
    Proportion Var 0.379 0.199 0.101
    Cumulative Var 0.379 0.578 0.679
    


<table class="dataframe">
<caption>A matrix: 5 × 3 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>MR1</th><th scope=col>MR2</th><th scope=col>MR3</th></tr>
</thead>
<tbody>
	<tr><th scope=row>SS loadings</th><td>5.3050970</td><td>2.7840277</td><td>1.4182943</td></tr>
	<tr><th scope=row>Proportion Var</th><td>0.3789355</td><td>0.1988591</td><td>0.1013067</td></tr>
	<tr><th scope=row>Cumulative Var</th><td>0.3789355</td><td>0.5777946</td><td>0.6791014</td></tr>
	<tr><th scope=row>Proportion Explained</th><td>0.5579955</td><td>0.2928269</td><td>0.1491776</td></tr>
	<tr><th scope=row>Cumulative Proportion</th><td>0.5579955</td><td>0.8508224</td><td>1.0000000</td></tr>
</tbody>
</table>




```R
# Visualization
# Install visualization tools
devtools::install_github('mattkcole/FAtools')
library(FAtools)


# Loadings plot
loadings_plot(fa_results$loadings)

# Factor diagram
fa.diagram(fa_results)
```

    Downloading GitHub repo mattkcole/FAtools@HEAD
    
    

    caTools  (NA -> 1.18.3 ) [CRAN]
    gtools   (NA -> 3.9.5  ) [CRAN]
    gplots   (NA -> 3.2.0  ) [CRAN]
    nFactors (NA -> 2.4.1.1) [CRAN]
    

    Installing 4 packages: caTools, gtools, gplots, nFactors
    
    Installing packages into ‘/usr/local/lib/R/site-library’
    (as ‘lib’ is unspecified)
    
    

    [36m──[39m [36mR CMD build[39m [36m─────────────────────────────────────────────────────────────────[39m
    * checking for file ‘/tmp/RtmpLyp5XD/remotes3a995c2d1a54/xmc2-FAtools-b02e149/DESCRIPTION’ ... OK
    * preparing ‘FAtools’:
    * checking DESCRIPTION meta-information ... OK
    * checking for LF line-endings in source and make files and shell scripts
    * checking for empty or unneeded directories
    Omitted ‘LazyData’ from DESCRIPTION
    * building ‘FAtools_0.0.1.9057.tar.gz’
    
    

    Installing package into ‘/usr/local/lib/R/site-library’
    (as ‘lib’ is unspecified)
    
    


    
![png](output_14_5.png)
    



    
![png](output_14_6.png)
    



```R
# Rotation Methods
# Compare rotations
fa(numeric_data, nfactors=3, rotate="oblimin")$loadings
fa(numeric_data, nfactors=3, rotate="varimax")$loadings

```

    Loading required namespace: GPArotation
    
    Warning message in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    “The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method.”
    


    
    Loadings:
                MR1    MR2    MR3   
    calories     0.683  0.442 -0.145
    cal_fat      0.784  0.168 -0.315
    total_fat    0.785  0.165 -0.315
    sat_fat      0.747  0.154 -0.184
    trans_fat    0.583 -0.116 -0.114
    cholesterol  1.042 -0.254  0.228
    sodium       0.516  0.364       
    total_carb   0.141  0.889       
    fiber       -0.236  0.809  0.211
    sugar        0.302  0.403  0.321
    protein      0.799         0.247
    vit_a               0.330  0.384
    vit_c               0.178  0.850
    calcium             0.717  0.186
    
                     MR1   MR2   MR3
    SS loadings    4.762 2.752 1.439
    Proportion Var 0.340 0.197 0.103
    Cumulative Var 0.340 0.537 0.640


    Warning message in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    “The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method.”
    


    
    Loadings:
                MR1    MR2    MR3   
    calories     0.808  0.495       
    cal_fat      0.890  0.237 -0.227
    total_fat    0.890  0.234 -0.227
    sat_fat      0.819  0.220 -0.103
    trans_fat    0.581              
    cholesterol  0.924 -0.150  0.293
    sodium       0.584  0.403       
    total_carb   0.353  0.879       
    fiber       -0.111  0.766  0.240
    sugar        0.308  0.420  0.365
    protein      0.749  0.149  0.314
    vit_a               0.324  0.398
    vit_c       -0.172  0.173  0.836
    calcium      0.200  0.707  0.238
    
                     MR1   MR2   MR3
    SS loadings    5.305 2.784 1.418
    Proportion Var 0.379 0.199 0.101
    Cumulative Var 0.379 0.578 0.679


**Discriminant Analysis (DA)**


```R
#Create the Group Variable
numeric_data$group <- ifelse(numeric_data$calories < 400 & numeric_data$total_fat < 15 & numeric_data$sodium < 700, "Healthy", "Unhealthy")
numeric_data$group <- as.factor(numeric_data$group)

```


```R
# Check Assumptions for LD
by(numeric_data$total_fat, numeric_data$group, shapiro.test)

boxM(numeric_data[,c("calories","cal_fat"	,"total_fat"	,"sat_fat",	"trans_fat",	"cholesterol"	,"sodium"	,"total_carb",	"fiber",	"sugar"	,"protein"	,"vit_a"	,"vit_c"	,"calcium" )], numeric_data$group)
cor(numeric_data[,c("calories","cal_fat"	,"total_fat"	,"sat_fat",	"trans_fat",	"cholesterol"	,"sodium"	,"total_carb",	"fiber",	"sugar"	,"protein"	,"vit_a"	,"vit_c"	,"calcium" )])
```


    numeric_data$group: Healthy
    
    	Shapiro-Wilk normality test
    
    data:  dd[x, ]
    W = 0.87754, p-value = 0.001741
    
    ------------------------------------------------------------ 
    numeric_data$group: Unhealthy
    
    	Shapiro-Wilk normality test
    
    data:  dd[x, ]
    W = 0.97648, p-value = 0.004146
    



    
    	Box's M-test for Homogeneity of Covariance Matrices
    
    data:  numeric_data[, c("calories", "cal_fat", "total_fat", "sat_fat",     "trans_fat", "cholesterol", "sodium", "total_carb", "fiber",     "sugar", "protein", "vit_a", "vit_c", "calcium")]
    Chi-Sq (approx.) = 436.93, df = 105, p-value < 2.2e-16
    



<table class="dataframe">
<caption>A matrix: 14 × 14 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>calories</th><th scope=col>cal_fat</th><th scope=col>total_fat</th><th scope=col>sat_fat</th><th scope=col>trans_fat</th><th scope=col>cholesterol</th><th scope=col>sodium</th><th scope=col>total_carb</th><th scope=col>fiber</th><th scope=col>sugar</th><th scope=col>protein</th><th scope=col>vit_a</th><th scope=col>vit_c</th><th scope=col>calcium</th></tr>
</thead>
<tbody>
	<tr><th scope=row>calories</th><td> 1.0000000</td><td> 0.83440118</td><td> 0.83387525</td><td> 0.73457935</td><td> 0.41656857</td><td> 0.668124471</td><td>0.696296427</td><td>0.75041712</td><td> 0.28430450</td><td>0.4337429</td><td>0.71826238</td><td>0.100649077</td><td>-0.112059018</td><td>0.4713187</td></tr>
	<tr><th scope=row>cal_fat</th><td> 0.8344012</td><td> 1.00000000</td><td> 0.99913798</td><td> 0.83692480</td><td> 0.53633184</td><td> 0.684339186</td><td>0.594332235</td><td>0.48691001</td><td> 0.07387536</td><td>0.2514663</td><td>0.58910729</td><td>0.032159621</td><td>-0.246643838</td><td>0.2479584</td></tr>
	<tr><th scope=row>total_fat</th><td> 0.8338752</td><td> 0.99913798</td><td> 1.00000000</td><td> 0.83465619</td><td> 0.53927662</td><td> 0.685973890</td><td>0.589356338</td><td>0.48420983</td><td> 0.07443413</td><td>0.2499798</td><td>0.58942679</td><td>0.029650902</td><td>-0.247508626</td><td>0.2454147</td></tr>
	<tr><th scope=row>sat_fat</th><td> 0.7345794</td><td> 0.83692480</td><td> 0.83465619</td><td> 1.00000000</td><td> 0.75479473</td><td> 0.641313700</td><td>0.472479329</td><td>0.42921775</td><td> 0.02252763</td><td>0.3291139</td><td>0.47821875</td><td>0.187431241</td><td>-0.180129543</td><td>0.4050881</td></tr>
	<tr><th scope=row>trans_fat</th><td> 0.4165686</td><td> 0.53633184</td><td> 0.53927662</td><td> 0.75479473</td><td> 1.00000000</td><td> 0.480316816</td><td>0.129334139</td><td>0.07589849</td><td>-0.14747869</td><td>0.2106543</td><td>0.26983306</td><td>0.059185339</td><td>-0.133402414</td><td>0.1159796</td></tr>
	<tr><th scope=row>cholesterol</th><td> 0.6681245</td><td> 0.68433919</td><td> 0.68597389</td><td> 0.64131370</td><td> 0.48031682</td><td> 1.000000000</td><td>0.537671291</td><td>0.21219792</td><td>-0.14721232</td><td>0.3242229</td><td>0.88242268</td><td>0.003301258</td><td> 0.015962359</td><td>0.1482292</td></tr>
	<tr><th scope=row>sodium</th><td> 0.6962964</td><td> 0.59433223</td><td> 0.58935634</td><td> 0.47247933</td><td> 0.12933414</td><td> 0.537671291</td><td>1.000000000</td><td>0.60818768</td><td> 0.27496434</td><td>0.3864127</td><td>0.67864085</td><td>0.078147511</td><td> 0.009718598</td><td>0.3683148</td></tr>
	<tr><th scope=row>total_carb</th><td> 0.7504171</td><td> 0.48691001</td><td> 0.48420983</td><td> 0.42921775</td><td> 0.07589849</td><td> 0.212197923</td><td>0.608187681</td><td>1.00000000</td><td> 0.59996684</td><td>0.5702376</td><td>0.44368628</td><td>0.213150336</td><td> 0.014446387</td><td>0.6925179</td></tr>
	<tr><th scope=row>fiber</th><td> 0.2843045</td><td> 0.07387536</td><td> 0.07443413</td><td> 0.02252763</td><td>-0.14747869</td><td>-0.147212321</td><td>0.274964340</td><td>0.59996684</td><td> 1.00000000</td><td>0.3073246</td><td>0.06894013</td><td>0.366554960</td><td> 0.395639392</td><td>0.6063160</td></tr>
	<tr><th scope=row>sugar</th><td> 0.4337429</td><td> 0.25146633</td><td> 0.24997976</td><td> 0.32911390</td><td> 0.21065430</td><td> 0.324222886</td><td>0.386412672</td><td>0.57023761</td><td> 0.30732462</td><td>1.0000000</td><td>0.37543791</td><td>0.265839391</td><td> 0.358579060</td><td>0.4302693</td></tr>
	<tr><th scope=row>protein</th><td> 0.7182624</td><td> 0.58910729</td><td> 0.58942679</td><td> 0.47821875</td><td> 0.26983306</td><td> 0.882422682</td><td>0.678640847</td><td>0.44368628</td><td> 0.06894013</td><td>0.3754379</td><td>1.00000000</td><td>0.074339180</td><td> 0.123641831</td><td>0.3389268</td></tr>
	<tr><th scope=row>vit_a</th><td> 0.1006491</td><td> 0.03215962</td><td> 0.02965090</td><td> 0.18743124</td><td> 0.05918534</td><td> 0.003301258</td><td>0.078147511</td><td>0.21315034</td><td> 0.36655496</td><td>0.2658394</td><td>0.07433918</td><td>1.000000000</td><td> 0.488762844</td><td>0.3883421</td></tr>
	<tr><th scope=row>vit_c</th><td>-0.1120590</td><td>-0.24664384</td><td>-0.24750863</td><td>-0.18012954</td><td>-0.13340241</td><td> 0.015962359</td><td>0.009718598</td><td>0.01444639</td><td> 0.39563939</td><td>0.3585791</td><td>0.12364183</td><td>0.488762844</td><td> 1.000000000</td><td>0.2499279</td></tr>
	<tr><th scope=row>calcium</th><td> 0.4713187</td><td> 0.24795836</td><td> 0.24541468</td><td> 0.40508806</td><td> 0.11597965</td><td> 0.148229179</td><td>0.368314823</td><td>0.69251787</td><td> 0.60631595</td><td>0.4302693</td><td>0.33892685</td><td>0.388342084</td><td> 0.249927875</td><td>1.0000000</td></tr>
</tbody>
</table>




```R
# Perform Discriminant Analysis
qda_model <- qda(group ~ calories + cal_fat + total_fat + sat_fat + trans_fat + cholesterol + sodium + total_carb + fiber + sugar + protein + vit_a + vit_c + calcium , data = numeric_data)

pred <- predict(qda_model)
table(Predicted = pred$class, Actual = numeric_data$group)
```


               Actual
    Predicted   Healthy Unhealthy
      Healthy        32         4
      Unhealthy       0       174


**Canonical Correlation Analysis (CCA)**


```R
X <- numeric_data[, c("calories", "total_fat", "sat_fat", "trans_fat", "cholesterol", "sodium")]
Y <- numeric_data[, c("total_carb", "fiber", "sugar", "protein", "vit_a", "vit_c", "calcium")]
```


```R
X <- scale(X)
Y <- scale(Y)
cca_result <- cc(X, Y)

plt.cc(cca_result)

p.asym(cca_result$cor, nrow(X), ncol(X), ncol(Y))

# Canonical correlations
cat("Canonical correlations:\n")
print(cca_result$cor)

# Canonical coefficients
cat("\nCanonical coefficients for X (Set 1):\n")
print(cca_result$xcoef)

cat("\nCanonical coefficients for Y (Set 2):\n")
print(cca_result$ycoef)

```

    Wilks' Lambda, using F-approximation (Rao's F):
                    stat     approx df1      df2      p.value
    1 to 6:  0.006581585 42.3613433  42 927.4640 0.000000e+00
    2 to 6:  0.091365976 21.6730049  30 794.0000 0.000000e+00
    3 to 6:  0.448181281  9.0474919  20 660.9582 0.000000e+00
    4 to 6:  0.813370503  3.5827485  12 529.4418 3.706824e-05
    5 to 6:  0.959763506  1.3900130   6 402.0000 2.172319e-01
    6 to 6:  0.996203403  0.3849177   2 202.0000 6.810047e-01
    Canonical correlations:
    [1] 0.96330919 0.89226710 0.67006166 0.39055126 0.19125578 0.06161653
    
    Canonical coefficients for X (Set 1):
                        [,1]       [,2]       [,3]        [,4]       [,5]
    calories    -0.124527661  1.2056212  0.1541874 -0.82296950 -0.2981270
    total_fat    0.001593897 -0.1892110  1.3550906  1.80213842  0.2326191
    sat_fat      0.571705375  0.3277385 -2.1235549 -0.11230904  1.0188611
    trans_fat   -0.130052707 -0.2665370  0.4421978  0.08697406 -1.6134698
    cholesterol -1.047463658 -0.8240668 -0.2409668 -0.25557226  0.4605341
    sodium      -0.210714818  0.1560761  0.1716996 -0.35166269 -0.5293214
                       [,6]
    calories    -1.45804961
    total_fat    0.35672565
    sat_fat      0.43739949
    trans_fat   -0.09842484
    cholesterol -0.26292316
    sodium       1.30843605
    
    Canonical coefficients for Y (Set 2):
                      [,1]        [,2]       [,3]        [,4]       [,5]       [,6]
    total_carb  0.26799392  1.15006992  0.5754769 -0.20373776  0.7006857 -0.9896277
    fiber      -0.10563505  0.02898702  0.6670834  0.10210220 -0.5639028  1.1822695
    sugar      -0.16228013 -0.20796108 -0.3924982  0.04620889 -1.3340243  0.1606226
    protein    -1.08127266 -0.16870569  0.1030195  0.20928707  0.1811481  0.4209405
    vit_a       0.07402743  0.06535543 -0.4293598  0.45443724  0.2041258  0.7523923
    vit_c       0.05035821 -0.09206800  0.3036891 -1.13390967  0.5625970 -0.5784557
    calcium     0.19590873 -0.04325816 -1.1396139 -0.28474642  0.2303286 -0.2439576
    


    
![png](output_22_1.png)
    

