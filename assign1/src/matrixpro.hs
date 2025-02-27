import System.CPUTime
import Text.Printf

initMatrixA :: Int -> [[Double]]
initMatrixA m_ar = replicate m_ar (replicate m_ar 1.0)

initMatrixB :: Int -> Int -> [[Double]]
initMatrixB m_ar m_br = [[fromIntegral (i + 1) | j <- [1..m_br]] | i <- [0..(m_ar - 1)]]

transpose :: [[Double]] -> [[Double]]
transpose ([]:_) = []
transpose mat = map head mat : transpose (map tail mat)

dotProduct :: [Double] -> [Double] -> Double
dotProduct xs ys = sum (zipWith (*) xs ys)

matMult :: [[Double]] -> [[Double]] -> [[Double]]
matMult a b = 
    let bT = transpose b
    in [[dotProduct row col | col <- bT] | row <- a]

printMatrix :: [[Double]] -> IO ()
printMatrix matrix = mapM_ (putStrLn . unwords . map show) matrix

main :: IO ()
main = do
    let m_ar = 100 
        m_br = m_ar 
        matrixA = initMatrixA m_ar
        matrixB = initMatrixB m_ar m_br

    start <- getCPUTime

    let resultMatrix = matMult matrixA matrixB

    end <- getCPUTime

    let diff = fromIntegral (end - start) / (10^12) :: Double

    -- Display the result (optional)
    putStrLn "Matrix A:"
    printMatrix matrixA

    putStrLn "\nMatrix B:"
    printMatrix matrixB

    putStrLn "\nResult Matrix (A × B):"
    printMatrix resultMatrix

    -- Print the time taken for matrix multiplication (in seconds)
    printf "\nTime taken for multiplication: %.6f seconds\n" diff