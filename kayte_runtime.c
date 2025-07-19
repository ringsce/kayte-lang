#include <stdio.h>   // For printf
#include <stdlib.h>  // For malloc, free, exit
#include <string.h>  // For strlen, strcpy, strcat, strcmp, strdup

// --- Kayte Runtime Functions (Declarations) ---
void kayte_print_string(const char* str);
void kayte_show_form(const char* form_name);
char* kayte_get_control_text(const char* form_name, const char* control_name);
int kayte_string_equal(const char* s1, const char* s2);
void kayte_close_form(const char* form_name);
char* kayte_concat_strings(const char* s1, const char* s2);
char* kayte_int_to_string(long long value); // <<< NEW: For converting int to string

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
}

char* kayte_get_control_text(const char* form_name, const char* control_name) {
    printf("Kayte Runtime (GUI): Request to get text from control '%s' on form '%s'. (Dummy: Returning hardcoded value)\n", control_name, form_name);

    if (strcmp(form_name, "LoginForm") == 0) {
        if (strcmp(control_name, "EditUsername") == 0) {
            return strdup("admin");
        } else if (strcmp(control_name, "EditPassword") == 0) {
            return strdup("password");
        }
    }
    return strdup("");
}

int kayte_string_equal(const char* s1, const char* s2) {
    if (!s1 || !s2) {
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
}

char* kayte_concat_strings(const char* s1, const char* s2) {
    if (!s1) s1 = "";
    if (!s2) s2 = "";

    size_t len1 = strlen(s1);
    size_t len2 = strlen(s2);
    char* result = (char*)malloc(len1 + len2 + 1);

    if (result == NULL) {
        fprintf(stderr, "Kayte Runtime Error: Memory allocation failed for string concatenation.\n");
        exit(EXIT_FAILURE);
    }

    strcpy(result, s1);
    strcat(result, s2);

    return result;
}

char* kayte_int_to_string(long long value) {
    // A simple implementation, consider snprintf for robustness
    // Max long long is ~19 digits + sign + null terminator
    char buffer[25];
    snprintf(buffer, sizeof(buffer), "%lld", value);
    return strdup(buffer); // Caller must free
}

