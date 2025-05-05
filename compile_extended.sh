#!/bin/bash
set -e

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}Compiling Extended Symkell FFI to shared library...${NC}"

# Determine platform-specific library extension
case "$(uname -s)" in
    Linux*)     LIB_EXT=so;;
    Darwin*)    LIB_EXT=dylib;;
    CYGWIN*|MINGW*|MSYS*) LIB_EXT=dll;;
    *)          LIB_EXT=so;;
esac

# Make script executable
chmod +x "$0"

# Build the shared library
echo -e "${BLUE}Building shared library...${NC}"
ghc -O2 -dynamic -shared -fPIC symkell-ffi-extended.hs -package text -o "libsymkell.$LIB_EXT"

if [ $? -ne 0 ]; then
    echo -e "${RED}Failed to build shared library${NC}"
    exit 1
fi

echo -e "${GREEN}Shared library built successfully!${NC}"

# Try to copy to Python directory if it exists
if [ -d "pysymkell" ]; then
    echo -e "${BLUE}Copying shared library to pysymkell/ directory${NC}"
    cp "libsymkell.$LIB_EXT" pysymkell/
    echo -e "${GREEN}Library copied to pysymkell/ directory${NC}"
    
    echo -e "${BLUE}Instructions for using the Python wrapper:${NC}"
    echo -e "1. Change directory to pysymkell: ${GREEN}cd pysymkell${NC}"
    echo -e "2. Install the package: ${GREEN}pip install -e .${NC}"
    echo -e "3. Run the example: ${GREEN}python example.py${NC}"
fi

echo -e "${GREEN}Done!${NC}"
echo -e "${BLUE}The extended implementation now supports:${NC}"
echo -e "- differentiation"
echo -e "- integration"
echo -e "- simplification"
echo -e "- limit calculation"
echo -e "- Taylor series expansion"
echo -e "- Laurent series"
echo -e "- evaluation" 