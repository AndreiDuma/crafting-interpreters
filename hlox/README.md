## Hlox

Hlox is a Haskell implementation of the tree-walk interpreter for the Lox language described in the book [Crafting Interpreters](https://craftinginterpreters.com) by Robert Nystrom. I started this project with several purposes in mind:
- Learn how to structure a somewhat larger Haskell codebase:
    - When I discover a new Haskell design pattern I come back to this repository to see whether it can improve its design.
- Learn how to use Megaparsec in a real project;
- Use as many real-world packages as possible, as long as it makes sense; so far I've used:
    - `text` for all text manipulation;
    - `megaparsec` for parsing the Lox syntax;
    - `mtl` for threading the evaluation environment state and for exception handling;
    - `containers` for the `Map` used in the evaluation environment.

Ideas that I haven't got the time to put into practice yet:
- use `optparse-applicative` for parsing the (very simple) command-line options;
- extract the effectful operations from the AST evaluation (namely printing) to a higher level (possibly doing evaluation in the Writer monad and returning print commands lazily).

Interesting things I've learned about thanks to this project:
- GADTs (see SPJ's [paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/gadt-pldi.pdf)), although I haven't used them here;
- Typed tagless final approach to interpreters (see Oleg Kiselyov's [paper](https://www.okmij.org/ftp/tagless-final/course/lecture.pdf)); although it doesn't seem to be applicable here, it is an intriguing way to implement EDSLs without GADTs.

It would be nice if this became the 'canonical' Haskell implementation of Lox's tree-walk interpreter. I'm interested in doing this as close to "the Haskell way" as possible. Please submit an issue if you have any suggestions.
