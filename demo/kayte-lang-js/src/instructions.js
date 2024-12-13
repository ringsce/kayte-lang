// src/instructions.js

export const instructions = {
  NOP: () => { /* No operation */ },
  LOAD: (vm, register, value) => { vm.registers[register] = value; },
  ADD: (vm, reg1, reg2, dest) => { vm.registers[dest] = vm.registers[reg1] + vm.registers[reg2]; },
  SUB: (vm, reg1, reg2, dest) => { vm.registers[dest] = vm.registers[reg1] - vm.registers[reg2]; },
  HALT: (vm) => { vm.running = false; },
  // Add more instructions as needed
};

