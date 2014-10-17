nlp
===

A chatbot program written in Prolog, that can extract knowledge (in the form of Prolog-facts) from simple sentences, 
and later on use this knowledge to answer simple questions.

Due to the very limited vocabulary of the program, it's not feasible to try random sentences without first looking in 
the code.

Example of a user session:

\>  peter met lisa s dog in the park
  
\> who did peter meet?

  lisas dog
  
\> who met a dog?

  peter
  
\> did peter meet lisa s dog in the park?

  yes
  
\> what does lisa have?

  dog





Project structure:
=================

syntax.pl: Input parsing, and generation of syntax-trees

semantics.pl: Parsing of syntax tree and generation of Prolog facts

main.pl: overall structure of the program, main-loop

