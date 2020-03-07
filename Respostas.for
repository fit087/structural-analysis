************************************************************************
*      IMPRESSAO DAS RESPOSTAS: DESLOCAMENTOS, ESFORÇOS E REACOES      *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE RESPOSTAS (U,ESF,RA,NO,NV,G,D,TE,NGL,NNO,NEL,NNV,GLN,
     &	                  NELT,NELP)
*-----------------------------------------------------------------------

* DEFINIÇÃO DE VARIÁVEIS
	IMPLICIT NONE
      INTEGER I,J,NNO,NGL,NEL,GLN,NNV,D(NNO,GLN),TE(NEL),
     &     	NO(2,NEL),NV(NNO),NELT,NELP,G(NEL,2*GLN)
	REAL(8) U(NGL),ESF(6,NEL),RA(NGL)
	CHARACTER*6 DESL(6)
	DATA DESL /'DX NO1','DY NO1','RZ NO1','DX NO2','DY NO2','RZ NO2'/

* IMPRIMIR DESLOCAMENTOS NODAIS
      WRITE (2,5)
    5 FORMAT(//'DESLOCAMENTOS:'//5X'NÓ'7X'DESL.X'9X'DESL.Y'9X'ROT.Z'/) 
      DO I=1,NNO
        WRITE (2,10) I,(U(D(I,J)),J=1,GLN)
   10   FORMAT(2X,I5,3(F15.9))
	END DO

* IMPRIMIR DESLOCAMENTOS NAS LIBERAÇÕES LOCAIS
      WRITE (2,15)
   15 FORMAT(//'DESLOCAMENTOS NAS LIBERAÇÕES LOCAIS:'//
     &         5X'ELEM.'3X'DESLOC.'9X'VALOR'/) 
      DO J=1,NEL
	  DO I=1,6
          IF (G(J,I)>GLN*NNO)  WRITE (2,20) J,DESL(I),(U(G(J,I)))
   20     FORMAT(2X,I5,6X,A6,3X,F15.9)
        END DO
	END DO

* IMPRIMIR ESFORÇOS NOS ELEMENTOS DE PÓRTICO
      WRITE (2,25)
   25 FORMAT(//'ESFORÇOS NO ELEMENTOS DE PÓRTICO:'//
     &         4X,'ELEM.'5X'NÓ'11X'N'14X'Q'14X'M'/)
	DO I=1,NEL
        IF (TE(I)==1) WRITE (2,30) I,NO(1,I),(ESF(J,I),J=1,3),
     &                               NO(2,I),(ESF(J,I),J=4,6)
   30   FORMAT(2X,I5,4X,I5,3(F15.3),/,11X,I5,3(F15.3),/)
      END DO

* IMPRIMIR ESFORÇOS NOS ELEMENTOS DE TRELIÇA
      IF (NELT/=0) WRITE (2,35)
   35 FORMAT(//'ESFORÇOS NO ELEMENTOS DE TRELIÇA: ( TRAÇÃO + )'//
     &       4X,'ELEM.'13X'N'/)
	DO I=1,NEL
        IF (TE(I)==2) WRITE (2,40) I,ESF(4,I)
   40   FORMAT(2X,I5,4X,F15.3)
      END DO

* IMPRIMIR REAÇÕES DE APOIO 
	WRITE (2,45)
   45 FORMAT(//'REAÇÕES DE APOIO:'//5X'NÓ'10X'RX'13X'RY'13X'MZ'/)
	DO I=1,NNV
        WRITE(2,50) NV(I),(RA(D(NV(I),J)),J=1,3)
   50   FORMAT(2X,I5,3(F15.3))
 	END DO


      END