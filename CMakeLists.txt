cmake_minimum_required(VERSION 3.10)

# set the project name
project(stiffness)
enable_language(Fortran)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-ffixed-form -std=legacy -fimplicit-none")
    set(bounds "-fbounds-check")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(dialect "-stand none -fixed -implicitnone")
    set(bounds "-check bounds")
endif()

set(src IA.FOR
		PrepArq.for
		Nos.for
		Elementos.for
		Rotacao.for
		Port2d.for
		Trel2d.for
		Sistema.for
		Gauss.for
		Reacoes.for
		Respostas.for
		Esforcos.for
		ANEST.for)

# add the executable
add_executable(${PROJECT_NAME} ${src})
# target_link_libraries(stiffness ${LIBS})

# References
# http://fortranwiki.org/fortran/show/Build+tools
# https://software.intel.com/sites/default/files/m/f/8/5/8/0/6366-ifort.txt
# https://riptutorial.com/cmake/example/22391/-hello-world--with-multiple-source-files 
# https://stackoverflow.com/questions/24588019/cmake-fortran-project-with-multiple-executables
