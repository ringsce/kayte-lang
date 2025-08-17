import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.util.Stack;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.nio.ByteBuffer;

// Define a simple Instruction class to hold opcode and operand
class Instruction {
    private int opcode;
    private int operand;

    public Instruction(int opcode, int operand) {
        this.opcode = opcode;
        this.operand = operand;
    }

    public int getOpcode() { return opcode; }
    public int getOperand() { return operand; }
}

public class KayteVM {

    // VM State
    private static Stack<Object> stack;
    private static List<String> stringLiterals;
    private static List<Long> integerLiterals;
    private static List<Instruction> instructions;
    private static int instructionPointer;

    // Bytecode constants (replace with your actual opcode values)
    private static final int OP_LOAD_CONST = 0;
    private static final int OP_CALL_FUNCTION = 1;
    private static final int OP_RETURN = 2;
    private static final int OP_ADD = 3;
    private static final int OP_PRINT = 4;
    private static final int OP_HALT = 5;

    public static void execute(byte[] kayteBytecode) {
        // --- 1. Deserialization ---
        ByteArrayInputStream bais = new ByteArrayInputStream(kayteBytecode);
        DataInputStream dis = new DataInputStream(bais);

        try {
            // Read ProgramTitle (for demo purposes, not used in execution)
            int titleLength = dis.readInt();
            dis.skipBytes(titleLength);

            // Read Instructions
            int instructionCount = dis.readInt();
            instructions = new ArrayList<>();
            for (int i = 0; i < instructionCount; i++) {
                int opcode = dis.readInt();
                int operand = dis.readInt();
                instructions.add(new Instruction(opcode, operand));
            }

            // Read StringLiterals
            int stringLiteralCount = dis.readInt();
            stringLiterals = new ArrayList<>();
            for (int i = 0; i < stringLiteralCount; i++) {
                int strLen = dis.readInt();
                byte[] strBytes = new byte[strLen];
                dis.readFully(strBytes);
                stringLiterals.add(new String(strBytes, "UTF-8"));
            }

            // Read IntegerLiterals
            int intLiteralCount = dis.readInt();
            integerLiterals = new ArrayList<>();
            for (int i = 0; i < intLiteralCount; i++) {
                integerLiterals.add(dis.readLong());
            }

            // (Add logic to read VariableMap, SubroutineMap, etc. here)

            // --- 2. VM Execution ---
            stack = new Stack<>();
            instructionPointer = 0;

            System.out.println("Starting VM Execution...");

            while (instructionPointer < instructions.size()) {
                Instruction currentInstruction = instructions.get(instructionPointer);
                instructionPointer++; // Increment before executing

                switch (currentInstruction.getOpcode()) {
                    case OP_LOAD_CONST:
                        // Push a constant (either string or integer)
                        // A more complex VM would need to differentiate types
                        stack.push(stringLiterals.get(currentInstruction.getOperand()));
                        break;
                    case OP_PRINT:
                        // Pop a value from the stack and print it
                        System.out.println(stack.pop());
                        break;
                    case OP_HALT:
                        System.out.println("VM Halted.");
                        return; // Exit the execution loop
                    default:
                        System.err.println("Unknown opcode: " + currentInstruction.getOpcode());
                        return;
                }
            }
        } catch (IOException e) {
            System.err.println("Failed to deserialize Kayte bytecode: " + e.getMessage());
        } finally {
            try {
                dis.close();
                bais.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
