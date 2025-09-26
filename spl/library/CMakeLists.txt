# Define the 'spl' library target
add_library(skydown_spl INTERFACE)

# Specify its public include directory
target_include_directories(skydown_spl INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include> # Path when installed
        )

# Create a modern, namespaced alias for consumers to use
add_library(Skydown::spl ALIAS skydown_spl)

# Add this target to the export set for packaging
install(
        TARGETS skydown_spl
        EXPORT SkydownTargets
)
