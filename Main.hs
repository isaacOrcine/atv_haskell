-- Grupo: 1
-- Nome: Isaac Orcine Silva - 202110223

-- atv 1
insere_no_fim :: t -> [t] -> [t]
insere_no_fim e (c:r) = c : insere_no_fim e r
insere_no_fim e [] = [e]
-- insere um elemento no final da lista, sendo que, o caso base da recursão é quando a lista está vazia.

-- auxiliar atv 7
pertence :: Eq t => t -> [t] -> Bool
pertence e (c:r) = (e == c) || pertence e r
pertence _ [] = False
-- retorna true se um elemento está contido na lista

-- atv 7
remover_repetidos :: Eq t => [t] -> [t]
remover_repetidos (c:r)
    | pertence c r = remover_repetidos r
    | otherwise = c : remover_repetidos r
remover_repetidos [] = []
-- inclui um elemnto na lista se ele não estiver contido nela, nesse sentido, sempre mantém a última ocorrência do elemento.

-- Atv 13
sequencia :: Int -> Int -> [Int]
sequencia n m
    | n == 0 = []
    | otherwise = [m..(m+n-1)]
--cria uma lista de acordo com o tamanho do n incrementando m.

--maior_de_dois :: Int -> Int -> Int
--maior_de_dois a b
--    | a > b = a
--    | otherwise = b 

-- Atv 19
insere_ordenado :: Ord t => t -> [t] -> [t]
insere_ordenado e (c:r) 
    | e > c = c : insere_ordenado e r
    | otherwise = e : c : r
insere_ordenado e [] = [e]
-- se a lista que recebemos é ordenada, então, assim que e > c, inserimos e na lista antes do c.

-- Atv 25
rodar_direita :: Int -> [t] -> [t]
rodar_direita n (c:r) 
    | n == 0 = c:r
    | otherwise = rodar_direita (n-1) (insere_no_fim c r)
-- A cabeça da lista vai para o fim da lista enquanto n > 0.
-- Sempre esperamos que n seja um inteiro positivo

-- função auxiliar para atv 31
index :: [t] -> Int -> t
index (c:r) n
    | n == 1 = c
    | otherwise = index r (n-1)
-- indice começando em 1 e não em 0, para satisfazer ex 31.

-- Atv 31
seleciona :: [t] -> [Int] -> [t]
seleciona l (c:r)
    | r == [] = [index l c]
    | otherwise = index l c : seleciona l r
seleciona l [] = []
--se r = [] então, tenho um unico elemento na lista de indices, logo, chamo a função index para essa cabeça.
-- caso contrário, chamo a função index para a cabeça e concateno com a chamada recursiva da função seleciona.


-- Atv 37
compactar :: [Int] -> [[Int]]
compactar (c:r) = compactarAux c r 1
compactar [] = []

-- Função auxiliar da atv 37 para processar o número atual
compactarAux :: Int -> [Int] -> Int -> [[Int]]
compactarAux e (c:r) n
    | e == c = compactarAux e r (n + 1)            
    | otherwise = (if n > 1 then [n, e] else [e]) : compactar (c:r)
compactarAux e [] n = [if n > 1 then [n, e] else [e]]
-- se o número atual for igual ao próximo, incrementa o contador n, caso contrário, adiciona o contador e o número atual na lista.