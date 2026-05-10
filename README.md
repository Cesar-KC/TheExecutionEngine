#NAME: CESAR Giner
#CLASS: CS441 - Program Design

#Description:
- Program 3 - The Execution Engine (Interpreter) is a complete interpreter for a simple imperative language, it implements a full pipeline: 
source string → scanner → parser (AST) → evaluator → output.The evaluator walks the AST recursively, threading an immutable environment through every statement to track variable state.                                                                 
- Supports assignment, print, if/else, while loops, arithmetic, and comparisons. 

#Environment:
- The environment (symbol table) is implemented as an immutable association list; each variable binding is stored as a dotted pair (var . value). Assignment prepends a new binding to the
front, and old bindings are shadowed, never deleted. Lookup always finds the most recent binding first. The environment is threaded functionally by recursively executing statements and
passing the updated environment as well. Initial environment is empty '() — variables must be assigned before use, or an 'unbound variable' error is raised. 


#Notes:
- Program 3 code implementation starts at line 345, all previous code is from Program 2 - Recursive Descent Parser, as we need a valid AST output for our Evaluator!
- Tests are below as well to validate the program
- Thanks for a great semester!
