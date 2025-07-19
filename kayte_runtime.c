#include <stdio.h>   // For printf
#include <stdlib.h>  // For malloc, free, exit
#include <string.h>  // For strlen, strcpy, strcat, strcmp, strdup

// --- Kayte Runtime Functions (Declarations) ---
// These match the 'declare' statements in the LLVM IR (.ll) files.

// Prints a string to the console.
void kayte_print_string(const char* str);

// Shows a form with the given name. (Dummy implementation)
void kayte_show_form(const char* form_name);

// Gets the text content of a control on a form. (Dummy implementation)
// Returns a newly allocated string, which the caller must free.
char* kayte_get_control_text(const char* form_name, const char* control_name);

// Compares two strings for equality. Returns 1 for true, 0 for false.
int kayte_string_equal(const char* s1, const char* s2);

// Closes a form with the given name. (Dummy implementation)
void kayte_close_form(const char* form_name);

// Concatenates two strings. Returns a newly allocated string, which the caller must free.
char* kayte_concat_strings(const char* s1, const char* s2);


// --- Kayte Runtime Functions (Implementations) ---

void kayte_print_string(const char* str) {
    if (str) {
        printf("Kayte Output: %s\n", str);
    } else {
        printf("Kayte Output: (null)\n");
    }
}

void kayte_show_form(const char* form_name) {
    if (form_name) {
        printf("Kayte Runtime (GUI): Request to show form '%s'. (Dummy: No actual GUI displayed)\n", form_name);
    } else {
        printf("Kayte Runtime (GUI): Request to show unnamed form. (Dummy)\n");
    }
    // In a real application, you would integrate with your LCL/GUI framework here.
    // Example (conceptual): LCL_ShowForm(form_name);
}

char* kayte_get_control_text(const char* form_name, const char* control_name) {
    printf("Kayte Runtime (GUI): Request to get text from control '%s' on form '%s'. (Dummy: Returning hardcoded value)\n", control_name, form_name);

    // --- DUMMY LOGIC FOR DEMONSTRATION ---
    // In a real application, this would query the actual GUI control.
    if (strcmp(form_name, "LoginForm") == 0) {
        if (strcmp(control_name, "EditUsername") == 0) {
            return strdup("admin"); // Simulate "admin" entered
        } else if (strcmp(control_name, "EditPassword") == 0) {
            return strdup("password"); // Simulate "password" entered
        }
    }
    // --- END DUMMY LOGIC ---

    // Return an empty string if control not found or no dummy value
    return strdup(""); // strdup allocates memory, must be freed by caller
}

int kayte_string_equal(const char* s1, const char* s2) {
    if (!s1 || !s2) {
        // Handle null pointers gracefully, e.g., only equal if both are null
        return (s1 == s2);
    }
    return strcmp(s1, s2) == 0;
}

void kayte_close_form(const char* form_name) {
    if (form_name) {
        printf("Kayte Runtime (GUI): Request to close form '%s'. (Dummy: No actual GUI closed)\n", form_name);
    } else {
        printf("Kayte Runtime (GUI): Request to close unnamed form. (Dummy)\n");
    }
    // In a real application, you would integrate with your LCL/GUI framework here.
    // Example (conceptual): LCL_CloseForm(form_name);
}

char* kayte_concat_strings(const char* s1, const char* s2) {
    if (!s1) s1 = ""; // Treat null as empty string for concatenation
    if (!s2) s2 = "";

    size_t len1 = strlen(s1);
    size_t len2 = strlen(s2);
    char* result = (char*)malloc(len1 + len2 + 1); // +1 for null terminator

    if (result == NULL) {
        fprintf(stderr, "Kayte Runtime Error: Memory allocation failed for string concatenation.\n");
        exit(EXIT_FAILURE); // Or handle more gracefully
    }

    strcpy(result, s1);
    strcat(result, s2);

    return result; // Caller is responsible for freeing this memory
}

// Add more runtime functions as needed (e.g., for integer operations,
// more complex string manipulations, file I/O, etc.)

