a <- c(27,20,21,26,27,31,24,21,20,19,23,24,28,19,24,29,18,20,17,31,20,25,28,21,27)
b <- c(21,22,15,12,16,19,15,22,24,19,23,13,22,20,24,18,20)

twoSampleRandomPermutationTest  <- function(a,b,c){
  originalMeanDiff  <- mean(a) - mean(b)
  blender  <- c(a,b)
  numberOfIterations  <- c
  print (originalMeanDiff)
  newDiff <- c(1)
    for (i in 1:numberOfIterations)
      {
      blend  <- sample(blender, length(blender))
      diffRandom  <- NULL
      aRandom  <- blend[1:length(a)]
      bRandom  <- blend[length(a) + 1: length(b)]
      diffRandom[i]  <- mean(aRandom) - mean(bRandom)
      newDiff[i]  <- c(round(diffRandom[i],1))
      }
  x  <- as.data.frame(table(newDiff))
  plot(x)
  return(x)
}
twoSampleRandomPermutationTest(a,b,6000)




