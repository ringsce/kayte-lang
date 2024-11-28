// src/kayteVM.js
import { instructions } from './instructions.js';

export class KayteVM {
  constructor(memorySize = 1024, registerCount = 16) {
    this.memory = new Uint8Array(memorySize);
    this.registers = new Int32Array(registerCount);
    this.pc = 0;
    this.running = true;
  }

  loadProgram(bytecode) {
    this.memory.set(bytecode, 0);  // Load bytecode into memory starting at position 0
  }

  step() {
    if (!this.running) return;

    const opcode = this.memory[this.pc];
    this.pc += 1;

    const instruction = Object.keys(instructions)[opcode];
    if (instruction) {
      instructions[instruction](this);
    } else {
      console.error(`Unknown opcode: ${opcode}`);
      this.running = false;
    }
  }

  run() {
    while (this.running && this.pc < this.memory.length) {
      this.step();
    }
  }
}

