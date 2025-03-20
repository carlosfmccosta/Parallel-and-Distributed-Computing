import System.CPUTime
import Text.Printf

-- ======== MATRIX INITIALIZATIOJS =========

initMatrixA :: Int -> [[Double]]
initMatrixA m_ar = replicate m_ar (replicate m_ar 1.0)

initMatrixB :: Int -> Int -> [[Double]]
initMatrixB m_ar m_br = [[fromIntegral (i + 1) | j <- [1..m_br]] | i <- [0..(m_ar - 1)]]

initZeroMatrix :: Int -> Int -> [[Double]]
initZeroMatrix m_ar m_br = replicate m_ar (replicate m_br 0.0)




-- ========== UTIL FUNCTIONS =======

dotProduct :: [Double] -> [Double] -> Double
dotProduct xs ys = sum (zipWith (*) xs ys)

transpose :: [[Double]] -> [[Double]]
transpose ([]:_) = []
transpose mat = map head mat : transpose (map tail mat)




-- ======= ACTUAL FUNCTIONS =======

-- 1st c/c++ way
matrixMultiply :: [[Double]] -> [[Double]] -> [[Double]]
matrixMultiply a b = [[ dotProduct row col| col <- colsB] | row <- a]
  where
    colsB = [ [bRow !! j | bRow <- b] | j <- [0..length (head b) - 1] ]


-- 2nd c/c++ way


onMultLine :: [[Double]] -> [[Double]] -> [[Double]]
onMultLine a b = multiply a (transpose b)
multiply :: [[Double]] -> [[Double]] -> [[Double]]
multiply a bT = [[ sum [ar !! k * bc !! k | k <- [0 .. length ar - 1]] | bc <- bT ] | ar <- a ]








-- ======= PRINTING AND TESTING ======

printMatrix :: [[Double]] -> IO ()
printMatrix = mapM_ (putStrLn . unwords . map show)

printMatrixFirstRow :: [[Double]] -> IO ()
printMatrixFirstRow matrix = case matrix of
    (firstRow:_) -> putStrLn . unwords . map show $ take 10 firstRow
    []           -> putStrLn "Matrix is empty."

-- ======= PRINTING AND TESTING ======

writeResultsToFile :: String -> Int -> Double -> IO ()
writeResultsToFile algorithm size time = do
    let filename = "results.txt"
    appendFile filename (printf "%s,%d,%.9f\n" algorithm size time)

-- ======= MAIN FUNCTION =======

main :: IO ()
main = do
    let standardSizes = [600,600,600,600,600,1000,1000,1000,1000,1000,1400,1400,1400,1400,1400,1800,1800,1800,1800,1800,2200,2200,2200,2200,2200,2600,2600,2600,2600,2600,3000,3000,3000,3000,3000]
    let lineSizes = [600,600,600,600,600,1000,1000,1000,1000,1000,1400,1400,1400,1400,1400,1800,1800,1800,1800,1800,2200,2200,2200,2200,2200,2600,2600,2600,2600,2600,3000,3000,3000,3000,3000,4096,4096,4096,4096,4096,6144,6144,6144,6144,6144,8192,8192,8192,8192,8192,10240,10240,10240,10240,10240]

    -- Run Standard Multiplication first
    mapM_ (\size -> executeAndLog "Standard Multiplication" matrixMultiply size) standardSizes

    -- Then run Line Multiplication
    mapM_ (\size -> executeAndLog "Line Multiplication" onMultLine size) lineSizes

    putStrLn "Results written to results.txt"

executeAndLog :: String -> ([[Double]] -> [[Double]] -> [[Double]]) -> Int -> IO ()
executeAndLog name func size = do
    let matrixA = initMatrixA size
        matrixB = initMatrixB size size
    start <- getCPUTime
    let _ = func matrixA matrixB
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12) :: Double
    writeResultsToFile name size diff
