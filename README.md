# Interpretador-Haskell

Este projeto é um pequeno interpretador de uma linguagem funcional escrito em Haskell. Ele possui suporte a:

- Aritmética básica (`+`, `-`, `*`)
- Operadores lógicos (`and`, `or`, `not`)
- Operadores relacionais (`==`, `<`)
- Condicionais (`if ... then ... else ...`)
- Cálculo Lambda com aplicação de funções (`λx:T. e`)
- Declarações `let`
- Suporte a listas com operações:
  - Construção (`LCons`)
  - Lista vazia (`LEmpty`)
  - Cabeça da lista (`LHead`)
  - Cauda da lista (`LTail`)
  - Verificação de lista vazia (`LIsEmpty`)

## Estrutura do Projeto

- **Lexer.hs**: Define a estrutura da linguagem, incluindo expressões e tipos.
- **Parser.hs**: Define regras de parsing, gerado pelo Parser.y.
- **Eval.hs**: Define as regras de avaliação (redução) das expressões.
- **TypeChecker.hs**: Sistema de tipos que verifica se as expressões são bem-tipadas.
- **Main.hs**: Ponto de entrada que chama cada etapa do interpretador.

## Testes

- Existem 3 exemplos de algoritmos implementados na pasta `/examples`, baseados em problemas do Beecrowd.
- Para rodar qualquer um deles, basta colar o comando abaixo no terminal (subsituindo o nome do arquivo).

### No Windows

```bash
runghc Main.hs < examples\<nomedoarquivo>
```

### No Linux

```bash
runghc Main.hs < examples/<nomedoarquivo>
```
