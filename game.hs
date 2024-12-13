type IntTuple = (Int, Int)
type ListMatrix = [[IntTuple]]

-- ������� ����� ��� ������� ������������
winningMatrix :: ListMatrix
winningMatrix = [[(3, 3), (0, 5)], [(5, 0), (1, 1)]]

-- ������� ������ ��������� �� ����
nashEquilibrium :: [Int] -> ListMatrix -> Int
nashEquilibrium _ winning =
    fst $ head [ (i, j) | i <- [0..1], j <- [0..1], isNash i j]
    where
        isNash i j = and [bestRow i j, bestCol i j]
        
        -- ��������, ��� i ����� ����� � ������
        bestRow i j = all (\k -> fst (winning !! i !! j) >= fst (winning !! k !! j)) [0..1]
        
        -- ��������, ��� j ����� ����� � �������
        bestCol i j = all (\l -> snd (winning !! i !! j) >= snd (winning !! i !! l)) [0..1]

-- ������� "������" ���������
kindStrategy :: [Int] -> ListMatrix -> Int
kindStrategy prevMoves _
    | any (== 1) prevMoves = 1 -- ���� �������� �����-���� ������, �������� ���������
    | otherwise = 0 -- ������������ �� ������� �������������

-- �������� ������ ����
playGame :: ([Int] -> ListMatrix -> Int) -> ListMatrix -> [Int] -> [IntTuple]
playGame strategyF winningMatrix moves = results
    where
        computerMoves = map (\idx -> strategyF (take idx moves) winningMatrix) [1..length moves]
        results = zipWith (\playerMove cMove -> (playerMove, cMove)) moves computerMoves

scoreIncr :: IntTuple -> IntTuple -> IntTuple
scoreIncr (playerScore, computerScore) scores = (playerScore + fst scores, computerScore + snd scores)

-- ������� �����
calcScore :: [IntTuple] -> IntTuple -> IntTuple
calcScore [] (playerScore, computerScore) = (playerScore, computerScore)
calcScore ((pChoice, cChoice):results) (playerScore, computerScore)
    | (pChoice, cChoice) == (1, 1) = calcScore results $ scoreIncr (playerScore, computerScore) $ winningMatrix !! 0 !! 0
    | (pChoice, cChoice) == (1, 0) = calcScore results $ scoreIncr (playerScore, computerScore) $ winningMatrix !! 0 !! 1
    | (pChoice, cChoice) == (0, 1) = calcScore results $ scoreIncr (playerScore, computerScore) $ winningMatrix !! 1 !! 0
    | (pChoice, cChoice) == (0, 0) = calcScore results $ scoreIncr (playerScore, computerScore) $ winningMatrix !! 1 !! 1

-- ������ ���� � ������� �����������
main :: IO ()
main = do
    -- 1 - �������, 0 - ������� ��������
    let playerMoves = [0, 1, 1, 0, 0, 1, 1, 0, 1, 0] -- ������ ����� ������
    
    let nashRounds = playGame nashEquilibrium winningMatrix playerMoves
    let kindRounds = playGame kindStrategy winningMatrix playerMoves

    let nashScores = calcScore nashRounds (0, 0)
    let kindScores = calcScore kindRounds (0, 0)
    putStrLn "1 - betray, 0 - cooperate"
    putStrLn "Nash:"
    print nashRounds
    putStrLn $ "Scores (Player, Computer): " ++ show nashScores

    putStrLn "Kind:"
    print kindRounds
    putStrLn $ "Scores (Player, Computer): " ++ show kindScores
