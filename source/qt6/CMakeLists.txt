# Builds libkayte_qt6 (.dylib / .so / .dll), the C ABI shim that
# source/kayte_qt6.pas loads at runtime. See source/qt6/README.md.
#
#   cmake -S source/qt6 -B build/qt6 [-DCMAKE_PREFIX_PATH=<qt6 prefix>]
#   cmake --build build/qt6 --config Release

cmake_minimum_required(VERSION 3.16)
project(kayte_qt6 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)

find_package(Qt6 REQUIRED COMPONENTS Widgets)
# QML commands (qml, qmlsource, qmlwidget, ...) need Qt Quick; without it
# the library builds widgets-only and those commands report how to get it.
find_package(Qt6 QUIET COMPONENTS Qml Quick QuickWidgets)
option(KAYTE_QT6_QML "Build QML support when Qt Quick is available" ON)

add_library(kayte_qt6 SHARED kayte_qt6.cpp)
target_link_libraries(kayte_qt6 PRIVATE Qt6::Widgets)

# QT "loadform" also reads Qt Designer .ui files when UiTools is there.
find_package(Qt6 QUIET COMPONENTS UiTools)
if(Qt6UiTools_FOUND)
  target_compile_definitions(kayte_qt6 PRIVATE KAYTE_QT6_UITOOLS)
  target_link_libraries(kayte_qt6 PRIVATE Qt6::UiTools)
  message(STATUS "kayte_qt6: Designer .ui support enabled")
endif()

if(KAYTE_QT6_QML AND Qt6Qml_FOUND AND Qt6Quick_FOUND AND Qt6QuickWidgets_FOUND)
  target_compile_definitions(kayte_qt6 PRIVATE KAYTE_QT6_QML)
  target_link_libraries(kayte_qt6 PRIVATE Qt6::Qml Qt6::Quick Qt6::QuickWidgets)
  message(STATUS "kayte_qt6: QML support enabled")
else()
  message(STATUS "kayte_qt6: QML support disabled (Qt Quick not found)")
endif()

