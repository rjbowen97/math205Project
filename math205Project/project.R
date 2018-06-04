#Part 4
standardDeviationsPassRuleOfThumb<-function(x,y) {
    if (sd(x) >= sd(y)) {
        if ((sd(x) / sd(y)) < 2) {
            return(TRUE)
        }
        else {
            return(FALSE)
        }
    }

    else {
        if ((sd(y) / sd(x)) < 2) {
            return(TRUE)
        }

        else {
            return(FALSE)
        }
    }
}

#End Part 4

#START part 2
#returns true if there are outliers present
hasOutliers<-function(x) {
#max<-quantile(x,probs=seq(0,1,.25), na.rm=TRUE) #+ (IQR(x, na.rm=TRUE) * 1.5 )
max <- quantile(x, .75, na.rm=TRUE, type=6) + (IQR(x, na.rm=TRUE) * 1.5 )
min <- quantile(x, .25, na.rm=TRUE, type=6) - (IQR(x, na.rm=TRUE) * 1.5 )

for(i in 1:length(x)) {
    if (x[i] > max) {
        return(TRUE)
    }

    if (x[i] < min) {
        return(TRUE)
    }
}

return(FALSE)

}

#END part 2

isSmallSampleNormal<-function(sample) { #uses the shapiro test to determine this
    shapiroTestResults <- shapiro.test(sample)

    if (shapiroTestResults$p.value > 0.05) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

executeSignTest <- function(sample) {
    print("Testing for median of differences equal to 0 using sign test")
    sampleWithoutZeros <- sample[!sample %in% c(0)]

    pairsWithPositiveDifferenceCount = 0
    for (currentValue in sampleWithoutZeros) {
       if (currentValue > 0) {
          pairsWithPositiveDifferenceCount <- pairsWithPositiveDifferenceCount + 1
       }
    }

    remainingPairsCount <- length(sampleWithoutZeros)
    binomTestResults <- binom.test(pairsWithPositiveDifferenceCount, remainingPairsCount, p = 0.5, alternative = "two.sided")

    if (binomTestResults$p.value < 0.05) {
        cat("Reject Ho: ")
    } else {
        cat("Failed to reject Ho: ")
    }
    cat("p = ", binomTestResults$p.value, "\n")
}

executePairedTTest <- function(x1, y1) {
    print("Testing for paired means equal to zero using a paired t-test")
    pairedTTestResults <- t.test(x1,y1,paired=TRUE)
    if (pairedTTestResults$p.value < 0.05) {
        cat("Reject Ho: ")
    } else {
        cat("Failed to reject Ho: ")
    }
    cat("p = ", pairedTTestResults$p.value, "\n")
}

testCDAssumptionsAndRunAppropriateTest <- function(x1, y1) {
    differencesBetweeny1x1 <- y1 - x1
    if (isSmallSampleNormal(differencesBetweeny1x1)) {
        if (hasOutliers(differencesBetweeny1x1)) {
            print("Paired t-test aborted due to outliers")
            executeSignTest(differencesBetweeny1x1)
        } else {
            executePairedTTest(x1,y1)
        }
    } else {
        print("Paired t-test aborted due to lack of normality")
        executeSignTest(differencesBetweeny1x1)
    }
}

runAppropriateCITest <- function(preferPooledTwoSampleTTest, x1, y1) {
    if (preferPooledTwoSampleTTest) {
        print("Testing for equal means using a pooled two-sample t-test")

        pooledTTestResult <- t.test(x1, y1, var.equal = TRUE)

        if (pooledTTestResult$p.value < 0.05) {
            cat("Reject Ho: ")
        } else {
            cat("Failed to reject Ho: ")
        }
        cat("p = ", pooledTTestResult$p.value, "\n")
    } else {
        print("Testing for equal means using a two-sample t-test")
        plainTTestResult <- t.test(x1, y1)

        if (plainTTestResult$p.value < 0.05) {
            cat("Reject Ho: ")
        } else {
            cat("Failed to reject Ho: ")
        }
        cat("p = ", plainTTestResult$p.value, "\n")
    }
}

#START Part 1
#2, 7, 9 for C D
x <<- read.csv(file="TestCase_S18/testcase10.csv",header=TRUE,sep=',')
x1 <- x[,1]
y1 <- x[,2]
x1 <- x1[!is.na(x1)]
y1 <- y1[!is.na(y1)]
n1 <- length(x1)
n2 <- length(y1)
CSK <- as.character(x[1,3])
DI <- as.character(x[2,3])
x1y1 <- c(x1,y1)

if (CSK == "S") {
    print("Testing equal variances with F-test")
    if (isSmallSampleNormal(x1) & isSmallSampleNormal(y1)) {
        varianceTestResult <- var.test(x1, y1, alternative = "two.sided")

        if (varianceTestResult$p.value < 0.05) {
            cat("Reject Ho: ")
        } else {
            cat("Failed to reject Ho: ")
        }

        cat("p =  ", varianceTestResult$p.value, "\n")
    } else {
        print("Aborted: Samples are not normal")
    }


} else if (CSK == "C") {
    if (DI == "D") {
        if (length(x1) < 15) {
            testCDAssumptionsAndRunAppropriateTest(x1, y1)
        } else if (length(x1) >= 15 & length(x1) < 40) {
            testCDAssumptionsAndRunAppropriateTest(x1, y1)
        } else {
            executePairedTTest(x1, y1)
        }

    } else { #DI == "I"
        preferPooledTwoSampleTTest = standardDeviationsPassRuleOfThumb(x1,y1)
        if (length(x1y1) < 15) {
            if (isSmallSampleNormal(x1) & isSmallSampleNormal(y1)) {
                if (hasOutliers(x1) | hasOutliers(y1)) {
                   print("Aborting two-sample test, outliers found")
                } else {
                    runAppropriateCITest(preferPooledTwoSampleTTest, x1, y1)
                }
            } else {
                print("Aborting two-sample test, samples are not normal")
            }

        } else if (length(x1y1) >= 15 & length(x1y1) < 40) {
            if (hasOutliers(x1) | hasOutliers(y1)) {
                print("Aborting two-sample test, outliers found")
            } else {
                runAppropriateCITest(preferPooledTwoSampleTTest, x1, y1)
            }

        } else {
            runAppropriateCITest(TRUE, x1, y1)
        }
    }
} else { #CKS == "K"
    print("Testing for equal proportions using two-proportion test")
    if (sum(x1) < 10 | n1 - sum(x1) < 10 | sum(y1) < 10 | n2 - sum(y1) < 10) {
        print("Aborting test, success or failure count below 10")
    } else {
        res<-prop.test(c(sum(x1),sum(y1)), n=c(n1, n2), alternative = c("two.sided"), correct=FALSE)
        if (res$p.value < 0.05) {
            cat("Reject Ho: ")
        } else {
            cat("Failed to reject Ho: ")
        }
        cat("p =  ", res$p.value, "\n")
    }
}