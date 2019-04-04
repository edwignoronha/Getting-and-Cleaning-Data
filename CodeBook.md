
### Reading the activitie file

``` r
activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[,2] <- as.character(activities[,2])
head(activities)
```

    ##   V1                 V2
    ## 1  1            WALKING
    ## 2  2   WALKING_UPSTAIRS
    ## 3  3 WALKING_DOWNSTAIRS
    ## 4  4            SITTING
    ## 5  5           STANDING
    ## 6  6             LAYING

### Reading the features file and extracting mean and std variables

``` r
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])
extractredFeatures <- grep(".*mean.*|.*std.*", features[,2])
extractredFeatures.names <- features[extractredFeatures,2]
extractredFeatures.names <- gsub('-mean', 'Mean', extractredFeatures.names)
extractredFeatures.names <- gsub('-std', 'Std', extractredFeatures.names)
extractredFeatures.names <- gsub('[-()]', '', extractredFeatures.names)
extractredFeatures.names
```

    ##  [1] "tBodyAccMeanX"                "tBodyAccMeanY"               
    ##  [3] "tBodyAccMeanZ"                "tBodyAccStdX"                
    ##  [5] "tBodyAccStdY"                 "tBodyAccStdZ"                
    ##  [7] "tGravityAccMeanX"             "tGravityAccMeanY"            
    ##  [9] "tGravityAccMeanZ"             "tGravityAccStdX"             
    ## [11] "tGravityAccStdY"              "tGravityAccStdZ"             
    ## [13] "tBodyAccJerkMeanX"            "tBodyAccJerkMeanY"           
    ## [15] "tBodyAccJerkMeanZ"            "tBodyAccJerkStdX"            
    ## [17] "tBodyAccJerkStdY"             "tBodyAccJerkStdZ"            
    ## [19] "tBodyGyroMeanX"               "tBodyGyroMeanY"              
    ## [21] "tBodyGyroMeanZ"               "tBodyGyroStdX"               
    ## [23] "tBodyGyroStdY"                "tBodyGyroStdZ"               
    ## [25] "tBodyGyroJerkMeanX"           "tBodyGyroJerkMeanY"          
    ## [27] "tBodyGyroJerkMeanZ"           "tBodyGyroJerkStdX"           
    ## [29] "tBodyGyroJerkStdY"            "tBodyGyroJerkStdZ"           
    ## [31] "tBodyAccMagMean"              "tBodyAccMagStd"              
    ## [33] "tGravityAccMagMean"           "tGravityAccMagStd"           
    ## [35] "tBodyAccJerkMagMean"          "tBodyAccJerkMagStd"          
    ## [37] "tBodyGyroMagMean"             "tBodyGyroMagStd"             
    ## [39] "tBodyGyroJerkMagMean"         "tBodyGyroJerkMagStd"         
    ## [41] "fBodyAccMeanX"                "fBodyAccMeanY"               
    ## [43] "fBodyAccMeanZ"                "fBodyAccStdX"                
    ## [45] "fBodyAccStdY"                 "fBodyAccStdZ"                
    ## [47] "fBodyAccMeanFreqX"            "fBodyAccMeanFreqY"           
    ## [49] "fBodyAccMeanFreqZ"            "fBodyAccJerkMeanX"           
    ## [51] "fBodyAccJerkMeanY"            "fBodyAccJerkMeanZ"           
    ## [53] "fBodyAccJerkStdX"             "fBodyAccJerkStdY"            
    ## [55] "fBodyAccJerkStdZ"             "fBodyAccJerkMeanFreqX"       
    ## [57] "fBodyAccJerkMeanFreqY"        "fBodyAccJerkMeanFreqZ"       
    ## [59] "fBodyGyroMeanX"               "fBodyGyroMeanY"              
    ## [61] "fBodyGyroMeanZ"               "fBodyGyroStdX"               
    ## [63] "fBodyGyroStdY"                "fBodyGyroStdZ"               
    ## [65] "fBodyGyroMeanFreqX"           "fBodyGyroMeanFreqY"          
    ## [67] "fBodyGyroMeanFreqZ"           "fBodyAccMagMean"             
    ## [69] "fBodyAccMagStd"               "fBodyAccMagMeanFreq"         
    ## [71] "fBodyBodyAccJerkMagMean"      "fBodyBodyAccJerkMagStd"      
    ## [73] "fBodyBodyAccJerkMagMeanFreq"  "fBodyBodyGyroMagMean"        
    ## [75] "fBodyBodyGyroMagStd"          "fBodyBodyGyroMagMeanFreq"    
    ## [77] "fBodyBodyGyroJerkMagMean"     "fBodyBodyGyroJerkMagStd"     
    ## [79] "fBodyBodyGyroJerkMagMeanFreq"

### Loading, Merging data sets and labeling the variables. Using descriptive activity names in resulting data set

``` r
library(plyr)
traindata <- read.table("UCI HAR Dataset/train/X_train.txt")[extractredFeatures]
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
traindataset <- cbind(trainSubjects, trainActivities, traindata)
testdata <- read.table("UCI HAR Dataset/test/X_test.txt")[extractredFeatures]
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
testdataset <- cbind(testSubjects, testActivities, testdata)
finaldata <- rbind(traindataset, testdataset)
colnames(finaldata) <- c("subject", "activity", extractredFeatures.names)
finaldata$activity <- mapvalues(finaldata$activity, from=as.character(activities[,1]), to=as.character(activities[,2]))
finaldata$activity <- as.factor(finaldata$activity)
head(finaldata)
```

    ##   subject activity tBodyAccMeanX tBodyAccMeanY tBodyAccMeanZ tBodyAccStdX
    ## 1       1 STANDING     0.2885845   -0.02029417    -0.1329051   -0.9952786
    ## 2       1 STANDING     0.2784188   -0.01641057    -0.1235202   -0.9982453
    ## 3       1 STANDING     0.2796531   -0.01946716    -0.1134617   -0.9953796
    ## 4       1 STANDING     0.2791739   -0.02620065    -0.1232826   -0.9960915
    ## 5       1 STANDING     0.2766288   -0.01656965    -0.1153619   -0.9981386
    ## 6       1 STANDING     0.2771988   -0.01009785    -0.1051373   -0.9973350
    ##   tBodyAccStdY tBodyAccStdZ tGravityAccMeanX tGravityAccMeanY
    ## 1   -0.9831106   -0.9135264        0.9633961       -0.1408397
    ## 2   -0.9753002   -0.9603220        0.9665611       -0.1415513
    ## 3   -0.9671870   -0.9789440        0.9668781       -0.1420098
    ## 4   -0.9834027   -0.9906751        0.9676152       -0.1439765
    ## 5   -0.9808173   -0.9904816        0.9682244       -0.1487502
    ## 6   -0.9904868   -0.9954200        0.9679482       -0.1482100
    ##   tGravityAccMeanZ tGravityAccStdX tGravityAccStdY tGravityAccStdZ
    ## 1       0.11537494      -0.9852497      -0.9817084      -0.8776250
    ## 2       0.10937881      -0.9974113      -0.9894474      -0.9316387
    ## 3       0.10188392      -0.9995740      -0.9928658      -0.9929172
    ## 4       0.09985014      -0.9966456      -0.9813928      -0.9784764
    ## 5       0.09448590      -0.9984293      -0.9880982      -0.9787449
    ## 6       0.09190972      -0.9989793      -0.9867539      -0.9973064
    ##   tBodyAccJerkMeanX tBodyAccJerkMeanY tBodyAccJerkMeanZ tBodyAccJerkStdX
    ## 1        0.07799634       0.005000803      -0.067830808       -0.9935191
    ## 2        0.07400671       0.005771104       0.029376633       -0.9955481
    ## 3        0.07363596       0.003104037      -0.009045631       -0.9907428
    ## 4        0.07732061       0.020057642      -0.009864772       -0.9926974
    ## 5        0.07344436       0.019121574       0.016779979       -0.9964202
    ## 6        0.07793244       0.018684046       0.009344434       -0.9948136
    ##   tBodyAccJerkStdY tBodyAccJerkStdZ tBodyGyroMeanX tBodyGyroMeanY
    ## 1       -0.9883600       -0.9935750   -0.006100849    -0.03136479
    ## 2       -0.9810636       -0.9918457   -0.016111620    -0.08389378
    ## 3       -0.9809556       -0.9896866   -0.031698294    -0.10233542
    ## 4       -0.9875527       -0.9934976   -0.043409983    -0.09138618
    ## 5       -0.9883587       -0.9924549   -0.033960416    -0.07470803
    ## 6       -0.9887145       -0.9922663   -0.028775508    -0.07039311
    ##   tBodyGyroMeanZ tBodyGyroStdX tBodyGyroStdY tBodyGyroStdZ
    ## 1     0.10772540    -0.9853103    -0.9766234    -0.9922053
    ## 2     0.10058429    -0.9831200    -0.9890458    -0.9891212
    ## 3     0.09612688    -0.9762921    -0.9935518    -0.9863787
    ## 4     0.08553770    -0.9913848    -0.9924073    -0.9875542
    ## 5     0.07739203    -0.9851836    -0.9923781    -0.9874019
    ## 6     0.07901214    -0.9851808    -0.9921175    -0.9830768
    ##   tBodyGyroJerkMeanX tBodyGyroJerkMeanY tBodyGyroJerkMeanZ
    ## 1        -0.09916740        -0.05551737        -0.06198580
    ## 2        -0.11050283        -0.04481873        -0.05924282
    ## 3        -0.10848567        -0.04241031        -0.05582883
    ## 4        -0.09116989        -0.03633262        -0.06046466
    ## 5        -0.09077010        -0.03763253        -0.05828932
    ## 6        -0.09424758        -0.04335526        -0.04193600
    ##   tBodyGyroJerkStdX tBodyGyroJerkStdY tBodyGyroJerkStdZ tBodyAccMagMean
    ## 1        -0.9921107        -0.9925193        -0.9920553      -0.9594339
    ## 2        -0.9898726        -0.9972926        -0.9938510      -0.9792892
    ## 3        -0.9884618        -0.9956321        -0.9915318      -0.9837031
    ## 4        -0.9911194        -0.9966410        -0.9933289      -0.9865418
    ## 5        -0.9913545        -0.9964730        -0.9945110      -0.9928271
    ## 6        -0.9916216        -0.9960147        -0.9930906      -0.9942950
    ##   tBodyAccMagStd tGravityAccMagMean tGravityAccMagStd tBodyAccJerkMagMean
    ## 1     -0.9505515         -0.9594339        -0.9505515          -0.9933059
    ## 2     -0.9760571         -0.9792892        -0.9760571          -0.9912535
    ## 3     -0.9880196         -0.9837031        -0.9880196          -0.9885313
    ## 4     -0.9864213         -0.9865418        -0.9864213          -0.9930780
    ## 5     -0.9912754         -0.9928271        -0.9912754          -0.9934800
    ## 6     -0.9952490         -0.9942950        -0.9952490          -0.9930177
    ##   tBodyAccJerkMagStd tBodyGyroMagMean tBodyGyroMagStd tBodyGyroJerkMagMean
    ## 1         -0.9943364       -0.9689591      -0.9643352           -0.9942478
    ## 2         -0.9916944       -0.9806831      -0.9837542           -0.9951232
    ## 3         -0.9903969       -0.9763171      -0.9860515           -0.9934032
    ## 4         -0.9933808       -0.9820599      -0.9873511           -0.9955022
    ## 5         -0.9958537       -0.9852037      -0.9890626           -0.9958076
    ## 6         -0.9954243       -0.9858944      -0.9864403           -0.9952748
    ##   tBodyGyroJerkMagStd fBodyAccMeanX fBodyAccMeanY fBodyAccMeanZ
    ## 1          -0.9913676    -0.9947832    -0.9829841    -0.9392687
    ## 2          -0.9961016    -0.9974507    -0.9768517    -0.9735227
    ## 3          -0.9950910    -0.9935941    -0.9725115    -0.9833040
    ## 4          -0.9952666    -0.9954906    -0.9835697    -0.9910798
    ## 5          -0.9952580    -0.9972859    -0.9823010    -0.9883694
    ## 6          -0.9952050    -0.9966567    -0.9869395    -0.9927386
    ##   fBodyAccStdX fBodyAccStdY fBodyAccStdZ fBodyAccMeanFreqX
    ## 1   -0.9954217   -0.9831330   -0.9061650        0.25248290
    ## 2   -0.9986803   -0.9749298   -0.9554381        0.27130855
    ## 3   -0.9963128   -0.9655059   -0.9770493        0.12453124
    ## 4   -0.9963121   -0.9832444   -0.9902291        0.02904438
    ## 5   -0.9986065   -0.9801295   -0.9919150        0.18108977
    ## 6   -0.9976438   -0.9922637   -0.9970459        0.15738377
    ##   fBodyAccMeanFreqY fBodyAccMeanFreqZ fBodyAccJerkMeanX fBodyAccJerkMeanY
    ## 1        0.13183575       -0.05205025        -0.9923325        -0.9871699
    ## 2        0.04286364       -0.01430976        -0.9950322        -0.9813115
    ## 3       -0.06461056        0.08267692        -0.9909937        -0.9816423
    ## 4        0.08030227        0.18569468        -0.9944466        -0.9887272
    ## 5        0.05798789        0.55978632        -0.9962920        -0.9887900
    ## 6        0.31883523        0.60559943        -0.9948507        -0.9882443
    ##   fBodyAccJerkMeanZ fBodyAccJerkStdX fBodyAccJerkStdY fBodyAccJerkStdZ
    ## 1        -0.9896961       -0.9958207       -0.9909363       -0.9970517
    ## 2        -0.9897398       -0.9966523       -0.9820839       -0.9926268
    ## 3        -0.9875663       -0.9912488       -0.9814148       -0.9904159
    ## 4        -0.9913542       -0.9913783       -0.9869269       -0.9943908
    ## 5        -0.9906244       -0.9969025       -0.9886067       -0.9929065
    ## 6        -0.9901575       -0.9952180       -0.9901788       -0.9930667
    ##   fBodyAccJerkMeanFreqX fBodyAccJerkMeanFreqY fBodyAccJerkMeanFreqZ
    ## 1            0.87038451            0.21069700            0.26370789
    ## 2            0.60851352           -0.05367561            0.06314827
    ## 3            0.11543400           -0.19343634            0.03825433
    ## 4            0.03579805           -0.09303585            0.16809523
    ## 5            0.27335020            0.07913538            0.29238418
    ## 6            0.32883589            0.05477140            0.32094497
    ##   fBodyGyroMeanX fBodyGyroMeanY fBodyGyroMeanZ fBodyGyroStdX fBodyGyroStdY
    ## 1     -0.9865744     -0.9817615     -0.9895148    -0.9850326    -0.9738861
    ## 2     -0.9773867     -0.9925300     -0.9896058    -0.9849043    -0.9871681
    ## 3     -0.9754332     -0.9937147     -0.9867557    -0.9766422    -0.9933990
    ## 4     -0.9871096     -0.9936015     -0.9871913    -0.9928104    -0.9916460
    ## 5     -0.9824465     -0.9929838     -0.9886664    -0.9859818    -0.9919558
    ## 6     -0.9848902     -0.9927862     -0.9807784    -0.9852871    -0.9916595
    ##   fBodyGyroStdZ fBodyGyroMeanFreqX fBodyGyroMeanFreqY fBodyGyroMeanFreqZ
    ## 1    -0.9940349        -0.25754888         0.09794711         0.54715105
    ## 2    -0.9897847        -0.04816744        -0.40160791        -0.06817833
    ## 3    -0.9873282        -0.21668507        -0.01726417        -0.11072029
    ## 4    -0.9886776         0.21686246        -0.13524536        -0.04972798
    ## 5    -0.9879443        -0.15334258        -0.08840273        -0.16223039
    ## 6    -0.9853661        -0.36303968        -0.13323831         0.19483324
    ##   fBodyAccMagMean fBodyAccMagStd fBodyAccMagMeanFreq
    ## 1      -0.9521547     -0.9561340         -0.08843612
    ## 2      -0.9808566     -0.9758658         -0.04414989
    ## 3      -0.9877948     -0.9890155          0.25789914
    ## 4      -0.9875187     -0.9867420          0.07358150
    ## 5      -0.9935909     -0.9900635          0.39431033
    ## 6      -0.9948360     -0.9952833          0.43796212
    ##   fBodyBodyAccJerkMagMean fBodyBodyAccJerkMagStd
    ## 1              -0.9937257             -0.9937550
    ## 2              -0.9903355             -0.9919603
    ## 3              -0.9892801             -0.9908667
    ## 4              -0.9927689             -0.9916998
    ## 5              -0.9955228             -0.9943890
    ## 6              -0.9947329             -0.9951562
    ##   fBodyBodyAccJerkMagMeanFreq fBodyBodyGyroMagMean fBodyBodyGyroMagStd
    ## 1                   0.3469885           -0.9801349          -0.9613094
    ## 2                   0.5320605           -0.9882956          -0.9833219
    ## 3                   0.6607950           -0.9892548          -0.9860277
    ## 4                   0.6789213           -0.9894128          -0.9878358
    ## 5                   0.5590577           -0.9914330          -0.9890594
    ## 6                   0.2469096           -0.9905000          -0.9858609
    ##   fBodyBodyGyroMagMeanFreq fBodyBodyGyroJerkMagMean
    ## 1               -0.1289889               -0.9919904
    ## 2               -0.2719585               -0.9958539
    ## 3               -0.2127279               -0.9950305
    ## 4               -0.0356842               -0.9952207
    ## 5               -0.2735820               -0.9950928
    ## 6               -0.2973291               -0.9951433
    ##   fBodyBodyGyroJerkMagStd fBodyBodyGyroJerkMagMeanFreq
    ## 1              -0.9906975                  -0.07432303
    ## 2              -0.9963995                   0.15807454
    ## 3              -0.9951274                   0.41450281
    ## 4              -0.9952369                   0.40457253
    ## 5              -0.9954648                   0.08775301
    ## 6              -0.9952387                   0.01995331

### Averaging data for subject, activity and variable into tidy data set

``` r
library(reshape2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
finaldata.melted <- melt(finaldata, id = c("subject", "activity"))
groupeddata <- group_by(finaldata.melted, subject,activity,variable)
summarydata <- summarize(groupeddata, avg = mean(value, na.rm = T))
head(summarydata)
```

    ## # A tibble: 6 x 4
    ## # Groups:   subject, activity [1]
    ##   subject activity variable          avg
    ##     <int> <fct>    <fct>           <dbl>
    ## 1       1 LAYING   tBodyAccMeanX  0.222 
    ## 2       1 LAYING   tBodyAccMeanY -0.0405
    ## 3       1 LAYING   tBodyAccMeanZ -0.113 
    ## 4       1 LAYING   tBodyAccStdX  -0.928 
    ## 5       1 LAYING   tBodyAccStdY  -0.837 
    ## 6       1 LAYING   tBodyAccStdZ  -0.826
