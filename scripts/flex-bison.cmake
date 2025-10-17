function(fetch_flex_bison)
	if(MSVC)
		include(FetchContent)

		FetchContent_Declare(FlexBison_fetch GIT_REPOSITORY https://github.com/lexxmark/winflexbison.git)
		FetchContent_MakeAvailable(FlexBison_fetch)

		add_executable(flex ALIAS win_flex)
		add_executable(bison ALIAS win_bison)
	else()
		include(ExternalProject)

		find_program(MAKE_EXECUTABLE NAMES gmake make mingw32-make REQUIRED)

		add_executable(flex IMPORTED)
		add_executable(bison IMPORTED)

		find_program(FLEX_EXECUTABLE flex flex++)
		if(FLEX_EXECUTABLE MATCHES "FLEX_EXECUTABLE-NOTFOUND")
			set(config_flags)  # parameters desired for ./configure of Autotools

			set(FLEX_EXECUTABLE ${CMAKE_BINARY_DIR}/flex_fetch-prefix/src/flex_fetch-build/src/flex)

			ExternalProject_Add(flex_fetch
				URL https://github.com/westes/flex/files/981163/flex-2.6.4.tar.gz
				DOWNLOAD_EXTRACT_TIMESTAMP true
				CONFIGURE_COMMAND <SOURCE_DIR>/configure ${config_flags}
				BUILD_COMMAND ${MAKE_EXECUTABLE} -j
				INSTALL_COMMAND ""
				TEST_COMMAND ""
			)
			add_dependencies(flex flex_fetch)
		endif()
		# message(${FLEX_EXECUTABLE})

		find_program(BISON_EXECUTABLE bison)
		if(BISON_EXECUTABLE MATCHES "BISON_EXECUTABLE-NOTFOUND")
			set(config_flags)  # parameters desired for ./configure of Autotools

			set(BISON_EXECUTABLE ${CMAKE_BINARY_DIR}/bison_fetch-prefix/src/bison_fetch-build/src/bison)

			ExternalProject_Add(bison_fetch
				URL https://ftp.gnu.org/gnu/bison/bison-3.8.2.tar.gz
				URL_HASH MD5=1e541a097cda9eca675d29dd2832921f
				DOWNLOAD_EXTRACT_TIMESTAMP true
				CONFIGURE_HANDLED_BY_BUILD true
				CONFIGURE_COMMAND <SOURCE_DIR>/configure ${config_flags}
				BUILD_COMMAND ${MAKE_EXECUTABLE} -j
				INSTALL_COMMAND ""
				TEST_COMMAND ""
			)
			add_dependencies(bison bison_fetch)

			configure_file(scripts/bison-shim.sh.in ${CMAKE_BINARY_DIR}/bison-shim.sh)
			set(BISON_EXECUTABLE ${CMAKE_BINARY_DIR}/bison-shim.sh)
		endif()
		# message(${BISON_EXECUTABLE})

		set_property(TARGET flex PROPERTY IMPORTED_LOCATION ${FLEX_EXECUTABLE})
		set_property(TARGET bison PROPERTY IMPORTED_LOCATION ${BISON_EXECUTABLE})
	endif()
endfunction()

function(add_flex_target TARGET IN OUTPUT OUT)
	cmake_path(GET OUT PARENT_PATH parentPath)
	file(MAKE_DIRECTORY ${parentPath})

	add_custom_command(
		OUTPUT ${OUT}
		COMMAND flex --outfile=${OUT} ${IN} ${ARGN}
		DEPENDS ${IN}
	)
	add_custom_target(${TARGET} ALL DEPENDS ${OUT} ${IN})
	add_dependencies(${TARGET} flex)
endfunction()

function(add_bison_target TARGET IN OUTPUT OUT)
	cmake_path(GET OUT PARENT_PATH parentPath)
	file(MAKE_DIRECTORY ${parentPath})

	add_custom_command(
		OUTPUT ${OUT}
		COMMAND bison --output=${OUT} ${IN} ${ARGN}
		DEPENDS ${IN}
	)
	add_custom_target(${TARGET} ALL DEPENDS ${OUT} ${IN})
	add_dependencies(${TARGET} bison)
endfunction()