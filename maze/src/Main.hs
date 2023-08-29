-- Gerador de labirintos em Haskell criado originalmente por Joe Wingbermuehle. Esse código implementa um jogo de labirinto, onde o mesmo cria um labirinto com paredes, espaço vazio, portas, jogador, alvo e corredores. O jogador pode se mover pelas setas ou pelas teclas 'W', 'A', 'S' e 'D'. O objetivo é alcançar o  alvo. O código usa a biblioteca Gloss para renderização e manipulação de eventos. A estrutura MazeData guarda os dados do labirinto, e a mônada MazeState permite leitura e modificação desses dados. O labirinto é gerado aleatoriamente, e o jogador se move apenas em áreas vazias, portas e corredores. O código também verifica se o jogador alcançou o alvo e gera um novo labirinto quando isso ocorre.

-- Esse trecho de código define a estrutura básica do programa e importa as bibliotecas e módulos necessários para a implementação do jogo de labirinto em Haskell. Mais abaixo ha uma breve explicação de cada parte daa função:

-- Isso declara que estamos definindo o módulo principal do programa e que seu nome é "Main". Um módulo é uma forma de organizar e encapsular o código em Haskell.
module Main where

    -- Aqui importarmos o módulo Control.Monad, que fornece funções e tipos relacionados à programação monádica. Monads são usadas para lidar com efeitos colaterais de maneira controlada e previsível.
    import Control.Monad
    
    -- Isso importa o módulo Control.Monad.State, que é uma extensão do conceito de monads para lidar com estados mutáveis em programas. Isso é útil para manter o estado do labirinto e do jogador enquanto o jogo é executado.
    import Control.Monad.State
    
    -- Este import importa o módulo Control.Monad.Reader, que também está relacionado a monads, mas é usado para fornecer acesso somente leitura a um ambiente compartilhado. Neste caso, ele pode ser usado para compartilhar informações sobre o labirinto.
    import Control.Monad.Reader
    
    -- Importa o módulo Data.Map, que fornece tipos e funções para trabalhar com mapas (também conhecidos como dicionários) em Haskell. No contexto do jogo, pode ser usado para representar o labirinto.
    import Data.Map
    
    -- System.Random fornece funções relacionadas à geração de números aleatórios. Isso é usado para criar o labirinto de forma aleatória.
    import System.Random
    
    -- Aqui importarmos o módulo Graphics.Gloss.Interface.Pure.Game, que é parte da biblioteca Gloss. Essa biblioteca é usada para criar interfaces gráficas simples em Haskell e será usada para renderizar o jogo na tela. Basicamente, essa parte do código define as dependências do programa e importa as funcionalidades necessárias para implementar o jogo de labirinto usando o paradigma monádico, manipulação de estados, geração de números aleatórios e renderização gráfica.
    import Graphics.Gloss.Interface.Pure.Game
    
    -- Aqui é definida a cor de fundo do jogo. A cor é definida usando a função makeColorI, que aceita valores inteiros para os componentes de cor (vermelho, verde, azul e alpha). A cor definida é um tom de azul claro e pode ser alterada para a qual o jogador desejar.
    background :: Color
    background = makeColorI 173 216 230 255
    
    -- Isso define a taxa de atualização do jogo, que é medida em quadros por segundo (frames per second). Nesse caso, o valor é definido como 60, o que significa que o jogo será atualizado 60 vezes por segundo.    
    fps :: Int
    fps = 60
    
    -- Aqui definimos um tipo de dado enumerado chamado ElementType, que representa os diferentes tipos de elementos que podem existir no labirinto. Os elementos incluem espaço vazio, parede, marcações, visitados, porta, pessoa (jogador), comida e corredor. A derivativa (Eq) permite que os elementos sejam comparados usando o operador ==.
    data ElementType = Space | Wall | Marked | Visited | Door | Person | Food | Hall
        deriving (Eq)
    
    -- Aqui é definido um tipo de dado enumerado chamado Direction, que representa as direções possíveis que o jogador pode se mover no labirinto. As direções incluem esquerda, direita, cima e baixo. A derivativa (Enum) permite que os valores desse tipo sejam enumerados sequencialmente.
    data Direction = DLeft | DRight | DUp | DDown
        deriving (Enum)
    
    -- Isso define uma estrutura de dados chamada MazeData, que representa os dados do jogo, incluindo o tamanho do labirinto, o estado do gerador de números aleatórios, as posições do jogador e do objetivo, bem como um mapa que representa o próprio labirinto.
    data MazeData = MazeData {
        width :: Int,
        height :: Int,
        gen :: StdGen,
        playerX :: Int,
        playerY :: Int,
        targetX :: Int,
        targetY :: Int,
        maze :: Map (Int, Int) ElementType
    }
    
    -- Aqui é definido um sinônimo de tipo MazeState para encapsular a lógica do jogo em um contexto monádico. Ele combina as monads ReaderT e State para representar o estado do labirinto e fornecer acesso somente leitura a algumas informações do labirinto. Essas definições e tipos são cruciais para a implementação do jogo, pois fornecem estruturas de dados para representar o estado do jogo, os elementos do labirinto, as direções possíveis e as configurações de monads para gerenciar o estado e os efeitos colaterais durante a execução do jogo.
    type MazeState a = ReaderT (Int, Int) (State MazeData) a
    
    -- A função initMaze serve para criar a estrutura inicial do labirinto dentro do jogo. Ela é responsável por configurar os elementos iniciais do labirinto, tais como as paredes, espaços vazios, portas e a posição inicial e final. Dessa forma, ela é fundamental para estabelecer o cenário inicial do labirinto, definindo as paredes, espaços vazios, portas e outras informações necessárias para a execução do jogo.
    initMaze :: Int -> Int -> Int -> MazeData
    initMaze w h s =
        let xs = [0 .. w - 1] in
        let ys = [0 .. h - 1] in
        let top = [(x, 0) | x <- xs] in
        let bottom = [(x, h - 1) | x <- xs] in
        let left = [(0, y) | y <- ys] in
        let right = [(w - 1, y) | y <- ys] in
        let start = [(2, 2)] in
        let spaces = Prelude.foldl (++) [] [top, bottom, left, right, start] in
        let m1 = fromList [(p, Space) | p <- spaces] in
        let doors = [(2, 2), (2, 1), (w - 3, h - 2)] in
        let m2 = fromList [(p, Door) | p <- doors] in
        MazeData {
            width = w, height = h, gen = mkStdGen s, playerX = 2, playerY = 1, targetX = w - 3, targetY = h - 2,
            maze = union m1 m2
        }
    
    -- A função getPosition é responsável por calcular a próxima posição no labirinto com base na direção dada. Ela recebe uma tupla representando a posição atual (x, y) e uma direção Direction. Com base na direção fornecida, a função retorna uma nova tupla de coordenadas que representa a próxima posição no labirinto.
    getPosition :: (Int, Int) -> Direction -> (Int, Int)
    getPosition (x, y) DLeft = (x - 1, y)
    getPosition (x, y) DRight = (x + 1, y)
    getPosition (x, y) DUp = (x, y - 1)
    getPosition (x, y) DDown = (x, y + 1)
    
    -- Essa função é responsável por mover o jogador dentro do labirinto na direção especificada e atualizar o estado do labirinto de acordo com a movimentação. Ela recebe dois argumentos: o tipo de elemento de preenchimento ElementType que será usado para preencher as posições percorridas e a direção Direction na qual o jogador deseja se mover. Além disso, retorna um valor encapsulado em um contexto MazeState, que é uma combinação de leitura e atualização do estado do labirinto.
    move :: ElementType -> Direction -> MazeState (Int, Int)
    move fill d = do
        pos <- ask; st <- get
        let mid = getPosition pos d
        let m = insert mid fill $ maze st
        let next = getPosition mid d
        put $ st { maze = insert next fill m }
        return next
    
    -- A função canMove verifica se é possível mover o jogador na direção especificada, considerando se os elementos nas posições intermediárias e finais correspondem a um determinado tipo de elemento especificado por match. Ela tambem retorna um valor booleano encapsulado, verificando se as posições intermediárias e finais do movimento contêm o tipo de elemento esperado. Isso é usado para verificar se o jogador pode realizar uma movimentação válida no labirinto.
    canMove :: ElementType -> Direction -> MazeState Bool
    canMove match d = do
        pos <- ask; st <- get
        let mid = getPosition pos d
        let e1 = findWithDefault Wall mid $ maze st
        let next = getPosition mid d
        let e2 = findWithDefault Wall next $ maze st
        return $ e1 == match && e2 == match
    
    -- Em resumo, a função carve serve para esculpir um labirinto, tentando escavar caminhos em várias direções a partir de uma posição inicial aleatória, desde que seja possível esculpir caminhos válidos. Isso é feito através de chamadas recursivas da função carve com diferentes direções, criando assim um labirinto escavado.
    carve :: MazeState ()
    carve = do
        st <- get
        let (start, g') = randomR (0, 3) $ gen st
        put $ st { gen = g' }
        mapM_ tryCarve [toEnum ((start + i) `mod` 4) | i <- [0 .. 3]]
        where
            tryCarve d = do
                cm <- canMove Wall d
                when cm (move Space d >>= (\n -> local (const n) carve))
    
    -- A função generate é responsável por gerar o labirinto completo. Ela recebe a largura (w), altura (h) e uma semente de geração aleatória (s) como parâmetros e retorna uma estrutura MazeData representando o labirinto gerado.
    generate :: Int -> Int -> Int -> MazeData
    generate w h s = execState (runReaderT carve (2, 2)) (initMaze w h s)

    -- canWalk verifica se é possível caminhar para uma determinada posição (x, y) no labirinto. Essa função é utilizada para verificar se é seguro mover o jogador para uma determinada posição, levando em consideração as paredes do labirinto e outras condições específicas.
    canWalk :: Int -> Int -> MazeData -> Bool
    canWalk x y md 
            | (findWithDefault Wall (x, y) (maze md)) == Space && x < 38 && y < 22 && y > 0 = True
            | (findWithDefault Wall (x, y) (maze md)) == Door && x < 38 && y < 22 && y > 0 = True
            | otherwise = False

    -- finishedMaze verifica se o jogador concluiu o labirinto, ou seja, se o jogador está na mesma posição que o alvo. Ela retorna True se a posição do jogador (playerX md, playerY md) for igual à posição do alvo (targetX md, targetY md). Caso contrário, retorna False. Isso é usado para verificar se o jogador alcançou o objetivo do labirinto e concluiu o jogo.
    finishedMaze :: MazeData -> Bool
    finishedMaze md
            | ((playerX md) == (targetX md)) && ((playerY md) == (targetY md)) = True
            | otherwise = False
        
    -- As funções handleKeys é responsável por lidar com eventos de teclado e atualizar o estado do labirinto (MazeData) de acordo com as teclas pressionadas. O código apresenta várias cláusulas de padrões para diferentes teclas, incluindo as teclas "w", "s", "d", "a" e as teclas de setas, que são utilizadas para realizar os movimentoos do jogador.
    handleKeys :: Event -> MazeData -> MazeData
    handleKeys (EventKey (Char 'w') keystate _ _) md = do
        let newY = (playerY md) - 1
        if((keystate == Down) && (canWalk (playerX md) newY md)) then
            md {playerY = newY}
        else
            md
    
    handleKeys (EventKey (Char 's') keystate _ _) md
        | keystate == Down && (finishedMaze md) = generate 39 23 start
        | keystate == Down && (canWalk (playerX md) newY md) = md{playerY = newY, gen = g'}
        | otherwise = md
            where newY = (playerY md) + 1
                  (start, g') = randomR (0, 100) $ gen md
        
    
    handleKeys (EventKey (Char 'd') keystate _ _) md = do
        let newX = (playerX md) + 1
        if((keystate == Down) && (canWalk newX (playerY md) md)) then
            md {playerX = newX}
        else
            md

    handleKeys (EventKey (Char 'a') keystate _ _) md = do
        let newX = (playerX md) - 1
        if((keystate == Down) && (canWalk newX (playerY md) md)) then
            md {playerX = newX}
        else
            md
    
    handleKeys (EventKey (SpecialKey KeyUp) keystate _ _) md = do
        let newY = (playerY md) - 1
        if((keystate == Down) && (canWalk (playerX md) newY md)) then
            md {playerY = newY}
        else
            md
    
    handleKeys (EventKey (SpecialKey KeyDown) keystate _ _) md
        | keystate == Down && (finishedMaze md) = generate 39 23 start
        | keystate == Down && (canWalk (playerX md) newY md) = md{playerY = newY, gen = g'}
        | otherwise = md
            where newY = (playerY md) + 1
                  (start, g') = randomR (0, 100) $ gen md
        
    
    handleKeys (EventKey (SpecialKey KeyRight) keystate _ _) md = do
        let newX = (playerX md) + 1
        if((keystate == Down) && (canWalk newX (playerY md) md)) then
            md {playerX = newX}
        else
            md

    handleKeys (EventKey (SpecialKey KeyLeft) keystate _ _) md = do
        let newX = (playerX md) - 1
        if((keystate == Down) && (canWalk newX (playerY md) md)) then
            md {playerX = newX}
        else
            md

    handleKeys _ md = md
    
    -- Chamada periodicamente pela biblioteca Gloss, ela serve para atualizar o estado do jogo. No entanto, neste código, a função não faz nada de especial. Ela simplesmente retorna o mesmo estado do labirinto que recebeu. Isso significa que o estado do objeto labirinto não é alterado com o tempo.
    myUpdate :: Float -> MazeData -> MazeData
    myUpdate _ md = md
    
    -- Basicamente, a função render é uma ponte entre o estado do labirinto e a representação visual dele na tela. Ela usa a função drawing para criar uma imagem que reflete o estado atual do labirinto e retorna essa imagem para a biblioteca Gloss exibi-la na interface gráfica.
    render :: MazeData -> Picture
    render md = drawing 0 0 md

    -- Essa função é responsável por desenhar um elemento do labirinto na tela com base nas coordenadas x e y. Por exemplo, se o elemento for Space, Door ou Visited, ele é desenhado como um retângulo sólido colorido com a cor de fundo. Se for um Wall, ele é desenhado como um retângulo preto. Se for um Person, é desenhado como um círculo azul, e assim por diante.
    drawElement :: ElementType -> Float -> Float -> Picture
    drawElement e x y
        | e == Space || e == Door || e == Visited = translate ((x * 20) -390) ((y * (-20)) + 230) $ color background $ rectangleSolid 20 20
        | e == Wall = translate ((x * 20) -390) ((y * (-20)) + 230) $ color black $ rectangleSolid 20 20
        | e == Person = translate ((x * 20) -390) ((y * (-20)) + 230) $ color blue $ circleSolid 8 
        | e == Food = translate ((x * 20) -390) ((y * (-20)) + 230) $ color red $ thickCircle 8 8
        | e == Hall = pictures [translate 310 (-350) $ color red $ rectangleSolid 20 300, translate 350 (-350) $ color red $ rectangleSolid 20 300]
        | otherwise = translate ((x * 20) -390) ((y * (-20)) + 230) $ color blue $ rectangleSolid 20 20    

    -- A função drawing itera sobre as posições do labirinto e desenha os elementos correspondentes em suas coordenadas corretas para criar uma representação visual do labirinto atualizado.
    drawing :: Int -> Int -> MazeData -> Picture
    drawing x y md
        | (x, y) == ((playerX md), (playerY md)) = pictures [drawElement Person (fromIntegral x) (fromIntegral y), drawing (x + 1) y md] 
        | (x, y) == ((targetX md), (targetY md)) = pictures [drawElement Food (fromIntegral x) (fromIntegral y), drawing (x + 1) y md] 
        | x < 38 = pictures [drawElement (findWithDefault Wall (x, y) (maze md)) (fromIntegral x) (fromIntegral y), drawing (x + 1) y md]
        | x == 38 && y < 22 = pictures [drawElement (findWithDefault Wall (x, y) (maze md)) (fromIntegral x) (fromIntegral y), drawing 0 (y + 1) md]
        | finishedMaze md = pictures [drawElement Hall 0 0]
        | otherwise = Blank
    
    -- A função principal que inicia o jogo. Ela cria um labirinto inicial usando a função generate com as dimensões e a semente apropriadas. Em seguida, chama a função play da biblioteca Gloss para iniciar a interface gráfica do jogo, permitindo com que o jogo seja executado com interatividade.    
    main :: IO ()
    main = do
        let maze = generate 39 23 5
        play FullScreen background fps maze render handleKeys myUpdate
