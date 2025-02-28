import System.CPUTime
import Text.Printf

initMatrixA :: Int -> [[Double]]
initMatrixA m_ar = replicate m_ar (replicate m_ar 1.0)

initMatrixB :: Int -> Int -> [[Double]]
initMatrixB m_ar m_br = [[fromIntegral (i + 1) | j <- [1..m_br]] | i <- [0..(m_ar - 1)]]

dotProduct :: [Double] -> [Double] -> Double
dotProduct xs ys = sum (zipWith (*) xs ys)

matrixMultiply :: [[Double]] -> [[Double]] -> [[Double]]
matrixMultiply a b =
    [[ dotProduct row col| col <- colsB] | row <- a]
  where
    colsB = [ [bRow !! j | bRow <- b] | j <- [0..length (head b) - 1] ]


printMatrix :: [[Double]] -> IO ()
printMatrix matrix = mapM_ (putStrLn . unwords . map show) matrix

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

    let resultMatrix = matrixMultiply matrixA matrixB

    end <- getCPUTime

    let diff = fromIntegral (end - start) / (10^12) :: Double
    
    putStrLn "\nFirst 10 elements of the first row of result matrix:"
    printMatrixFirstRow resultMatrix

    -- Print the time taken for matrix multiplication (in seconds)
    printf "\nTime taken for multiplication: %.9f seconds\n" diff