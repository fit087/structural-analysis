************************************************************************
*   LEITURA E IMPRESS�O DOS DADOS DOS N�S (COORD., V�NCULOS E CARGAS)  *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE NOS (NNO,NNV,X,Y,FA,D,V,NV,GLN)
*-----------------------------------------------------------------------

* DEFINI��O DE VARI�VEIS
      IMPLICIT NONE
      INTEGER I,J,K,GLN,NNO,NNC,NNV,NV(NNO),D(NNO,GLN),V(NNO,GLN)
	REAL*8 X(NNO),Y(NNO),FA(NNO,GLN)

* LER E IMPRIMIR PONTOS NODAIS
      WRITE (2,5) 
    5 FORMAT('AN�LISE EST�TICA LINEAR DE ESTRUTURAS DE BARRAS NO PLANO'
     &       ///'PONTOS NODAIS:'//5X'N�'8X'X'14X'Y'/)
      DO I=1,NNO
        READ  (1,* ) J,X(J),Y(J)
        WRITE (2,15) J,X(J),Y(J)
   15   FORMAT(2X, I5, 2(G15.5))
      END DO

* LER E IMPRIMIR N�S VINCULADOS
      WRITE (2,50)
   50 FORMAT(//'V�NCULOS:'//5X,'N�'7X'DESL.X'4X'DESL.Y'4X'ROT.Z'/)
      READ (1,*) NNV
	DO I=1,NNV
        READ  (1,* ) NV(I),(V(NV(I),J),J=1,GLN)
        WRITE (2,60) NV(I),(V(NV(I),J),J=1,GLN)
   60   FORMAT(2X,I5,3(5X,I5))
      END DO

* LER E IMPRIMIR N�S CARREGADOS
	READ (1,*) NNC
      IF (NNC.NE.0)  WRITE (2,65)
   65 FORMAT(//'FOR�AS NODAIS:'//5X'N�'10X'FX'13X'FY'13X'MZ'/)
      DO I=1,NNC
        READ  (1,* ) J,(FA(J,K),K=1,GLN)
        WRITE (2,75) J,(FA(J,K),K=1,GLN)
   75   FORMAT(2X,I5,3(F15.5))
	END DO

* FORMAR MATRIZ D COM NUMERA��O DAS DIRECOES DOS G.L. DO N�S
	DO I=1,NNO
	  DO J=1,GLN
		D(I,J)=3*(I-1)+J 
        END DO
      END DO

	END
