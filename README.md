# ProjectHaskell
Projeto para a disciplina de Paradigmas de Programação da Universidade Federal do ABC (UFABC). O projeto consiste em um jogo de labirinto no qual o objetivo do jogador é chegar na saída. Caso isso aconteça um novo labirinto é gerado para desafiar o jogador. 

_Eai, consegue escapar!?_

## Como Instalar 
Para instalar o jogo é necessário instalar as bibliotecas Random e Gloss após a instalação da linguagem de programação Haskell, seguindo os comandos abaixos:

```
$ sudo stack update
$ sudo apt-get install -y libgl-dev
$ sudo apt install freeglut3-dev
$ sudo stack install random
$ sudo stack install gloss
```

## Como Executar
Para executar o jogo é necessário executar os comandos abaixos já dentro do diretório do projeto.

```
$ stack build
$ stack run
```

## Comandos
Os comandos para jogar são as teclas direcionais do teclado e/ou A, W, S, D para movimentar o jogador durante seu trajeto dentro do labirinto.

| Comando                  | Explicação         |
|--------------------------|--------------------------|
| ⬆️ / W                  | Move o jogador para cima |
| ⬇️ / S                   | Move o jogador para baixo |
| ➡️ / D                    | Move o jogador para direita     |
| ⬅️ / A                     | Move o jogador para esquerda     |
| ESC                                | Sai do jogo, fechando a tela  |

## Referências

- O algoritmo original Maze Generator foi feito por Joe Wingbermuehle. Você pode obter o original no link abaixo: https://github.com/joewing/maze/blob/master/maze.hs

- https://haskell.pesquisa.ufabc.edu.br/cursos/21.q2.haskell/
