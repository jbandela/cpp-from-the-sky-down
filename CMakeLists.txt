cmake_minimum_required(VERSION 3.24)
project(Skydown VERSION 0.0.1 LANGUAGES CXX)

# Add the subdirectories for each library in the collection.
# This makes their targets (like Skydown::spl) available.
add_subdirectory(spl/library)

# --- Installation and Packaging ---
# This part gathers all targets exported from the sub-libraries
# into a single package configuration for consumers.
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

# Install the headers from all sub-libraries
install(
        DIRECTORY spl/library/include/
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

# Install the generated export targets file
install(
        EXPORT SkydownTargets
        FILE SkydownTargets.cmake
        NAMESPACE Skydown::
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Skydown
)

# Generate and install the version and config files
write_basic_package_version_file(
        "SkydownConfigVersion.cmake"
        VERSION ${PROJECT_VERSION}
        COMPATIBILITY AnyNewerVersion
)

configure_package_config_file(
        "cmake/SkydownConfig.cmake.in" # You'll need to create this helper file
        "SkydownConfig.cmake"
        INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/Skydown"
)

install(
        FILES
        "${CMAKE_CURRENT_BINARY_DIR}/SkydownConfig.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/SkydownConfigVersion.cmake"
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/Skydown"
)