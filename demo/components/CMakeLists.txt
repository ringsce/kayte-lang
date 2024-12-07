cmake_minimum_required(VERSION 3.29)
project(KayteLang)

# Define the output directory for the build artifacts
set(OUTPUT_DIR "${CMAKE_BINARY_DIR}/output")
set(ZIP_FILE "${CMAKE_BINARY_DIR}/kayte-lang_monthly.zip")

# Create the output directory
file(MAKE_DIRECTORY ${OUTPUT_DIR})

# Set file extensions to process
set(FILE_EXTENSIONS "kayte" "php")

# Add all .kayte and .php files from the source directory
file(GLOB_RECURSE KAYTE_FILES RELATIVE "${CMAKE_SOURCE_DIR}" "*.kayte")
file(GLOB_RECURSE PHP_FILES RELATIVE "${CMAKE_SOURCE_DIR}" "*.php")

# Copy files to the output directory
foreach(FILE ${KAYTE_FILES} ${PHP_FILES})
    add_custom_command(
        OUTPUT "${OUTPUT_DIR}/${FILE}"
        COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_SOURCE_DIR}/${FILE}" "${OUTPUT_DIR}/${FILE}"
        DEPENDS "${CMAKE_SOURCE_DIR}/${FILE}"
    )
endforeach()

# Add a custom target for building the project
add_custom_target(copy_files ALL
    DEPENDS ${OUTPUT_DIR}
)

# Create a .zip file containing all the files in the output directory
add_custom_command(
    OUTPUT ${ZIP_FILE}
    COMMAND ${CMAKE_COMMAND} -E tar "cf" "${ZIP_FILE}" --format=zip -- "${OUTPUT_DIR}/"
    DEPENDS copy_files
)

# Add a target to build the zip file
add_custom_target(create_zip ALL
    DEPENDS ${ZIP_FILE}
)

# Output a message after a successful build
add_custom_command(TARGET create_zip POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E echo "Kayte Lang build complete: ${ZIP_FILE}"
)

