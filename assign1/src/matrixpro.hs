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

main :: IO ()
main = do
    let m_ar = 1000
        m_br = m_ar 
        matrixA = initMatrixA m_ar
        matrixB = initMatrixB m_ar m_br

    start <- getCPUTime

    let resultMatrix = onMultLine matrixA matrixB

    end <- getCPUTime

    let diff = fromIntegral (end - start) / (10^12) :: Double
    
    putStrLn "\nFirst 10 elements of the first row of result matrix:"
    printMatrixFirstRow resultMatrix

    -- Print the time taken for matrix multiplication (in seconds)
    printf "\nTime taken for multiplication: %.9f seconds\n" diff