name = anest
flags = -ffixed-form -Wextra -O3
source = IA.FOR PrepArq.for Nos.for Elementos.for Rotacao.for Port2d.for Trel2d.for Sistema.for Gauss.for Reacoes.for Respostas.for Esforcos.for ANEST.for

build:
	gfortran $(flags) $(source) -o $(name)

# Links for build your own make files
# Instructional Video [1]:https://youtu.be/2Tr4hPdLcdA "Getting Started with Makefiles"
# Instructional Video [2]:https://youtu.be/_r7i5X0rXJk "How to Create a Simple Makefile - Introduction to Makefiles"

