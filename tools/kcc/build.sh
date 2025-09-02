#!/bin/bash

# KCC Build Script
echo "Building KCC (Kayte C Compiler)..."

# Create build directory
mkdir -p build
cd build

# Run CMake
cmake .. -DCMAKE_BUILD_TYPE=Release

# Build the project
make -j$(nproc)

if [ $? -eq 0 ]; then
    echo ""
    echo "✅ KCC built successfully!"
    echo ""
    echo "Usage examples:"
    echo "  ./kcc --help"
    echo "  ./kcc --version"
    echo "  ./kcc ../examples/hello.c"
    echo "  ./kcc -v -d ../examples/hello.c"
    echo ""

    # Test with example file if it exists
    if [ -f "../example/hello.c" ]; then
        echo "Testing with example file..."
        ./kcc -v ../examples/hello.c

        if [ $? -eq 0 ]; then
            echo "✅ Example compiled successfully!"
            echo "Generated assembly output in a.out"
        else
            echo "❌ Example compilation failed"
        fi
    fi
else
    echo "❌ Build failed!"
    exit 1
fi
