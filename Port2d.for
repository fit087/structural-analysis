************************************************************************
* FORMAÇÃO DA MATRIZ DE RIGIDEZ E DE FORÇAS EQUIVALENTES PORTICO PLANO *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE PORT2D (S,E,A,IZ,L,QX,QY,FP,FEQV) 
*-----------------------------------------------------------------------
* VARIÁVEIS LOCAIS:                                                    *
*                                                                      *
*  FEQV.... VETOR DE FORÇAS NODAIS EQUIVALENTES NO REFERENCIAL LOCAL   * 
*  S....... MATRIZ DE RIGIDEZ DE UM ELEMENTO                           *
*  UE...... VETOR DE DESLOCAMENTOS DE UM ELEMENTO NO REF. LOCAL        *
*  L....... COMPRIMENTO DE UM ELEMENTO                                 *
*  CX,CY... COSSENOS DIRETORES DA DIREÇÃO DE UM ELEMENTO               *

* DEFINIÇÃO DE VARIÁVEIS
      IMPLICIT NONE
	INTEGER I,J
      REAL*8 E,A,IZ,S(6,6),L,QX,QY,FP,FEQV(6)

* PARTE TRIANGULAR SUPERIOR DE [S]
      S=0.
      S(1,1) = E*A/L
      S(1,4) =-E*A/L
      S(2,2) = 12*E*IZ/L**3
      S(2,3) =  6*E*IZ/L**2
      S(2,5) =-12*E*IZ/L**3
      S(2,6) =  6*E*IZ/L**2
      S(3,3) =  4*E*IZ/L
      S(3,5) = -6*E*IZ/L**2
      S(3,6) =  2*E*IZ/L
      S(4,4) = E*A/L
      S(5,5) = 12*E*IZ/L**3
      S(5,6) = -6*E*IZ/L**2
      S(6,6) =  4*E*IZ/L

* PARTE TRIANGULAR INFERIOR DE [S]
      DO I=2,6
        DO J=1,(I-1)
          S(I,J)=S(J,I)
        END DO
      END DO

* VETOR DE CARGAS NODAIS EQUIVALENTES 
      FEQV(1) = QX*L/2     + FP
	FEQV(2) = QY*L/2
      FEQV(3) = QY*L**2/12
      FEQV(4) = QX*L/2     - FP
      FEQV(5) = QY*L/2
      FEQV(6) =-QY*L**2/12

      END 

