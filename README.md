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

