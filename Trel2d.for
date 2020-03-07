************************************************************************
* FORMA��O DA MATRIZ DE RIGIDEZ E DE FOR�AS EQUIVALENTES TRELI�A PLANA *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE TREL2D (S,E,A,L,FP,FEQV)                     
*-----------------------------------------------------------------------
* VARI�VEIS LOCAIS:                                                    *
*                                                                      *
*  FEQV.... VETOR DE FOR�AS NODAIS EQUIVALENTES NO REFERENCIAL LOCAL   * 
*  S....... MATRIZ DE RIGIDEZ DE UM ELEMENTO                           *
*  L....... COMPRIMENTO DE UM ELEMENTO                                 *

* DEFINI��O DE VARI�VEIS
 	IMPLICIT NONE
	INTEGER I,J
      REAL*8 E,A,S(6,6),L,FP,FEQV(6)

* MATRIZ [S]
      S=0.
      S(1,1) = E*A/L
      S(1,4) =-E*A/L
      S(4,1) =-E*A/L
      S(4,4) = E*A/L

* VETOR DE CARGAS NODAIS EQUIVALENTES 
      FEQV(1) = FP
	FEQV(2) = 0.
	FEQV(3) = 0.
      FEQV(4) =-FP
      FEQV(5) = 0.
      FEQV(6) = 0.

      END 

