# Compiler Project

A compiler that translates source code into Three-Address Code (TAC), featuring lexical analysis, syntax parsing, semantic analysis, and code generation.

## Table of Contents
1. [Features](#features)
2. [Prerequisites](#prerequisites)
3. [Installation](#installation)
4. [Building the Compiler](#building-the-compiler)
5. [Usage](#usage)
6. [Testing](#testing)
7. [File Structure](#file-structure)
8. [Troubleshooting](#troubleshooting)
9. [Contributing](#contributing)
10. [License](#license)

## Features
- ✅ Lexical analysis using Flex
- ✅ Syntax parsing using Bison  
- ✅ Semantic analysis with type checking
- ✅ Three-address code generation
- ✅ Error handling with line numbers
- ✅ Support for variables, control structures, and I/O operations

## Prerequisites
- **OS**: Ubuntu 20.04/Linux
- **Tools**:
  - Bison 3.5+
  - Flex 2.6+ 
  - GCC/G++ 9.0+
  - Make 4.0+

## Installation

### Ubuntu/Debian
```bash
sudo apt update && sudo apt upgrade -y
sudo apt install -y bison flex g++ make git
```

## Building the Compiler

### Method 1: Manual Build
```bash
bison -d parser.y           # Generate parser
flex lexer.L                # Generate lexer
g++ parser.tab.c lex.yy.c main.cpp -o compiler -lfl  # Compile
```

### Method 2: Using Makefile
```bash
make        # Build project
make clean  # Remove build artifacts
```

## Usage

Run the compiler:
```bash
./compiler
```

Enter input file when prompted (e.g. input.txt)
Get generated TAC in <filename>.tac

### Example Session
```bash
$ ./compiler
Input file: test.txt
Parsing Done.
ThreeAC saved in: test.tac
```

## Testing

### Sample Test File (test.txt)
```c
int main() {
    int x := 5;
    if (x > 3) {
        print("Valid");
    }
    return 0;
}
```

### Expected Output (test.tac)
```
t0 := 5
x := t0
t1 := x > 3
if t1 == 0 goto L0
param "Valid"
call print, 1
L0:
return 0
```

## File Structure
```
.
├── lexer.L           # Flex lexer rules
├── parser.y          # Bison grammar  
├── main.cpp          # Driver program
├── ast.h             # AST definitions
├── input.txt         # Example input
├── Makefile          # Build script
└── README.md         # This file
```

## Troubleshooting

| Error | Solution |
|-------|----------|
| bison: command not found | sudo apt install bison |
| fatal error: 'parser.tab.h' not found | Run bison -d parser.y first |
| segmentation fault | Check for infinite recursion in grammar rules |
| undefined reference to 'yylex' | Add -lfl flag when compiling |

## Contributing
1. Fork the repo
2. Create your branch (`git checkout -b your-feature`)
3. Commit changes (`git commit -m 'Add feature'`)
4. Push (`git push origin your-feature`)
5. Open a PR

## License
MIT License © 2023 Muhammad Abbas
