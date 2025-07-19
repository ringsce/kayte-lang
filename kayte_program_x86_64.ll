; ModuleID = 'kayte_program_x86_64'
source_filename = "kayte_program.kayte"
; --- Target for macOS x86_64 (Intel) ---
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx14.0.0" ; Adjust macOS version as needed (e.g., 10.15.0, 13.0.0)

; --- External Runtime Function Declarations ---
; These functions would be implemented in a C/C++ runtime library
declare void @kayte_print_string(i8*)
declare void @kayte_show_form(i8*)
declare i8* @kayte_get_control_text(i8*, i8*)
declare i1 @kayte_string_equal(i8*, i8*)
declare void @kayte_close_form(i8*)
declare i8* @kayte_concat_strings(i8*, i8*) ; For string concatenation

; --- Global String Literals ---
@.str.start_app = private unnamed_addr constant [28 x i8] c"Starting Kayte application!\00", align 1@.str.login_form_name = private unnamed_addr constant [10 x i8] c"LoginForm\00", align 1
@.str.guest = private unnamed_addr constant [6 x i8] c"guest\00", align 1
@.str.123 = private unnamed_addr constant [4 x i8] c"123\00", align 1
@.str.admin = private unnamed_addr constant [6 x i8] c"admin\00", align 1
@.str.welcome_admin = private unnamed_addr constant [16 x i8] c"Welcome, admin!\00", align 1
@.str.edit_username = private unnamed_addr constant [12 x i8] c"EditUsername\00", align 1
@.str.edit_password = private unnamed_addr constant [12 x i8] c"EditPassword\00", align 1
@.str.password = private unnamed_addr constant [9 x i8] c"password\00", align 1
@.str.login_success_button = private unnamed_addr constant [27 x i8] c"Login successful via button!\00", align 1
@.str.login_failed_button = private unnamed_addr constant [25 x i8] c"Login failed via button!\00", align 1
@.str.form_closed = private unnamed_addr constant [13 x i8] c"Form closed.\00", align 1


; --- Global Variables (for simplicity, often better to manage via stack/heap) ---
; These represent global variables in Kayte
@UserName = common global i8* null, align 8
@Password = common global i8* null, align 8
@EnteredUser = common global i8* null, align 8
@EnteredPass = common global i8* null, align 8


; --- Main Program Entry Point ---
; This function represents the top-level Kayte program execution
define void @kayte_main() {
entry:
  ; PRINT "Starting Kayte application!"
  %0 = getelementptr inbounds ([25 x i8], [25 x i8]* @.str.start_app, i64 0, i64 0)
  call void @kayte_print_string(i8* %0)

  ; SHOW LoginForm
  %1 = getelementptr inbounds ([10 x i8], [10 x i8]* @.str.login_form_name, i64 0, i64 0)
  call void @kayte_show_form(i8* %1)

  ; LET UserName = "guest"
  %2 = getelementptr inbounds ([6 x i8], [6 x i8]* @.str.guest, i64 0, i64 0)
  store i8* %2, i8** @UserName, align 8

  ; LET Password = "123"
  %3 = getelementptr inbounds ([4 x i8], [4 x i8]* @.str.123, i64 0, i64 0)
  store i8* %3, i8** @Password, align 8

  ; IF UserName = "admin" THEN
  %4 = load i8*, i8** @UserName, align 8
  %5 = getelementptr inbounds ([6 x i8], [6 x i8]* @.str.admin, i64 0, i64 0)
  %6 = call i1 @kayte_string_equal(i8* %4, i8* %5)
  br i1 %6, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  ; PRINT "Welcome, admin!"
  %7 = getelementptr inbounds ([16 x i8], [16 x i8]* @.str.welcome_admin, i64 0, i64 0)
  call void @kayte_print_string(i8* %7)
  br label %if.end

if.end:                                           ; preds = %if.then, %entry
  ; END (implicit program termination after main function)
  ret void
}


; --- Event Handler: HandleLoginButton ---
define void @HandleLoginButton() {
entry:
  ; LET EnteredUser = GetControlText("LoginForm", "EditUsername")
  %0 = getelementptr inbounds ([10 x i8], [10 x i8]* @.str.login_form_name, i64 0, i64 0)
  %1 = getelementptr inbounds ([12 x i8], [12 x i8]* @.str.edit_username, i64 0, i64 0)
  %2 = call i8* @kayte_get_control_text(i8* %0, i8* %1)
  store i8* %2, i8** @EnteredUser, align 8

  ; LET EnteredPass = GetControlText("LoginForm", "EditPassword")
  %3 = getelementptr inbounds ([10 x i8], [10 x i8]* @.str.login_form_name, i64 0, i64 0)
  %4 = getelementptr inbounds ([12 x i8], [12 x i8]* @.str.edit_password, i64 0, i64 0)
  %5 = call i8* @kayte_get_control_text(i8* %3, i8* %4)
  store i8* %5, i8** @EnteredPass, align 8

  ; IF EnteredUser = "admin" AND EnteredPass = "password" THEN
  %6 = load i8*, i8** @EnteredUser, align 8
  %7 = getelementptr inbounds ([6 x i8], [6 x i8]* @.str.admin, i64 0, i64 0)
  %8 = call i1 @kayte_string_equal(i8* %6, i8* %7) ; Check EnteredUser = "admin"

  %9 = load i8*, i8** @EnteredPass, align 8
  %10 = getelementptr inbounds ([9 x i8], [9 x i8]* @.str.password, i64 0, i64 0)
  %11 = call i1 @kayte_string_equal(i8* %9, i8* %10) ; Check EnteredPass = "password"

  %12 = and i1 %8, %11 ; Logical AND of both conditions
  br i1 %12, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  ; PRINT "Login successful via button!"
  %13 = getelementptr inbounds ([27 x i8], [27 x i8]* @.str.login_success_button, i64 0, i64 0)
  call void @kayte_print_string(i8* %13)
  br label %if.end

if.else:                                          ; preds = %entry
  ; PRINT "Login failed via button!"
  %14 = getelementptr inbounds ([25 x i8], [25 x i8]* @.str.login_failed_button, i64 0, i64 0)
  call void @kayte_print_string(i8* %14)
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  ret void
}

; --- Event Handler: HandleCancelButton ---
define void @HandleCancelButton() {
entry:
  ; CloseForm("LoginForm")
  %0 = getelementptr inbounds ([10 x i8], [10 x i8]* @.str.login_form_name, i64 0, i64 0)
  call void @kayte_close_form(i8* %0)

  ; PRINT "Form closed."
  %1 = getelementptr inbounds ([13 x i8], [13 x i8]* @.str.form_closed, i64 0, i64 0)
  call void @kayte_print_string(i8* %1)

  ret void
}

