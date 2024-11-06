// src/index.js
import { KayteVM } from './kayteVM.js';

// Sample bytecode for testing
const bytecode = [1, 0, 42, 2, 0, 1, 3, 4];

const vm = new KayteVM();
vm.loadProgram(bytecode);
vm.run();
console.log("Program completed");

