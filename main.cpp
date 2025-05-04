#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include "parser.tab.h"
using namespace std;
#include "ast.h"

extern FILE* yyin;
extern int yyparse();
extern std::vector<std::string> threeAddressCode;



int main() {
    std::string inputFilename;
    cout<<"\n\n\n\n   =========================================================================\n";
    cout<<"   =========================================================================\n";
    cout<<"   ======================== WELCOME TO OUR COMPILER ========================\n";
    cout<<"   =========================================================================\n";
    cout<<"   =========================================================================\n\n\n\n";
    cout << "Input file: ";
    cin >> inputFilename;
    
    bool hasError = false;
    
    FILE* inputFile = nullptr;
    inputFile = fopen(inputFilename.c_str(), "r");
if (!inputFile) {
    cerr << "Error opening file." << std::endl;
    return 1;
}

    yyin = inputFile;
    
    int parseStatus = yyparse();
    
    fclose(inputFile);
    
    if (!hasError) {
        cout << "Parsing Done." << std::endl;
        
        
        std::string outputFilename = inputFilename.substr(0, inputFilename.find_last_of('.')) + ".tac";
        std::ofstream outFile(outputFilename);
    	if (!outFile) {
        	cerr << "Error: Could not open output file " << outputFilename << std::endl;
        	
    	}
    
    	for (const auto& line : threeAddressCode) {
        	outFile << line << std::endl;
    	}
    
    	outFile.close();
    	cout << "ThreeAC saved in: " << outputFilename << std::endl;
        
        
        return 0;
    } else {
         cerr << "Parsing failed." << std::endl;
        return 1;
    }
}
