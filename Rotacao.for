************************************************************************
*    FORMA��O DA MATRIZ DE ROTACAO PARA ELEMENTO DE BARRA NO PLANO     *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE ROTACAO (R,X,Y,L,NO,I,NNO,NEL) 
*-----------------------------------------------------------------------
* VARI�VEIS LOCAIS:                                                    *
*                                                                      *
*  R....... MATRIZ DE ROTA��O DE UM ELEMENTO                           *
*  S....... MATRIZ DE RIGIDEZ DE UM ELEMENTO                           *
*  L....... COMPRIMENTO DE UM ELEMENTO                                 *
*  CX,CY... COSSENOS DIRETORES DA DIRE��O DE UM ELEMENTO               *
*  DX,DY... PROJE��ES DE UM ELEMENTO NAS DIRE��ES X E Y                *

* DEFINI��O DE VARI�VEIS
      IMPLICIT NONE
	INTEGER NEL,NO(2,NEL),I,NNO
      REAL*8 X(NNO),Y(NNO),R(6,6),L,CX,CY,DX,DY

* PROJE��ES (DX,DY), COMPRIMENTO (L) E COSSENOS DIRETORES (CX,CY)
      DX=X(NO(2,I))-X(NO(1,I))
      DY=Y(NO(2,I))-Y(NO(1,I))
      L=SQRT(DX**2+DY**2)
      CX=DX/L
      CY=DY/L

* MATRIZ DE ROTA��O [R]
      R=0.
      R(1,1) = CX
      R(1,2) = CY
      R(2,1) =-CY
      R(2,2) = CX
      R(3,3) = 1.
      R(4,4) = CX
      R(4,5) = CY
      R(5,4) =-CY
      R(5,5) = CX
      R(6,6) = 1.

      END 

