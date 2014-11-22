cacheMatrixTestGet1 <- function() {
        # 1 0
        # 0 1
        a <- matrix(c(1,0,0,1), 2, 2)
        aCache <- makeCacheMatrix(a)
        checkEquals(a, aCache$get())
        checkEquals(NULL, aCache$getInverse())
}

cacheMatrixTestSet1 <- function() {
        # 1 0
        # 0 1
        a <- matrix(c(1,0,0,1), 2, 2)
        aCache <- makeCacheMatrix(a)
        # 1 0
        # 1 1
        b <- matrix(c(1,1,0,1), 2, 2)
        aCache$set(b)
        checkEquals(b, aCache$get())
        checkEquals(NULL, aCache$getInverse())
}

cacheMatrixTestSolve1 <- function() {
        # 1 0
        # 1 1
        b <- matrix(c(1, 1, 0, 1), 2, 2)
        bCache <- makeCacheMatrix(b)
        checkEquals(b, bCache$get())
        checkEquals(NULL, bCache$getInverse())
        
        inv <- matrix(c(1, -1, 0, 1), 2, 2)
        returnedInv <- cacheSolve(bCache)
        checkEquals(inv, returnedInv)
        checkEquals(inv, bCache$getInverse())
}

cacheMatrixTestSolve2 <- function() {
        # 1 0
        # 1 1
        b <- matrix(c(1, 1, 0, 1), 2, 2)
        bCache <- makeCacheMatrix(b)
        checkEquals(b, bCache$get())
        checkEquals(NULL, bCache$getInverse())
        
        #  1 0
        # -1 1
        inv <- matrix(c(1, -1, 0, 1), 2, 2)
        returnedInv <- cacheSolve(bCache)
        checkEquals(inv, returnedInv)
        checkEquals(inv, bCache$getInverse())
        
        # 2 0
        # 0 2
        d <- matrix(c(2, 0, 0, 2), 2, 2)
        bCache$set(d)
        checkEquals(d, bCache$get())
        checkEquals(NULL, bCache$getInverse())
        
        # 0.5   0
        #   0 0.5
        inv2 <- matrix(c(0.5, 0, 0, 0.5), 2, 2)
        returnedInv2 <- cacheSolve(bCache)
        checkEquals(inv2, returnedInv2)
        checkEquals(inv2, bCache$getInverse())
}

runCacheMatrixTests <- function() {
        testSuite <- defineTestSuite("cacheMatrix",
                dirs = getwd(),
                testFileRegexp = "^cachematrixTests\\.R",
                testFuncRegexp = "^cacheMatrixTest.+",
                rngKind = "Marsaglia-Multicarry",
                rngNormalKind = "Kinderman-Ramage")
        testResult <- runTestSuite(testSuite)
        printTextProtocol(testResult)
}