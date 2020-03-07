************************************************************************
*                       PREPARAÇÃO DOS ARQUIVOS                        *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE PREPARQ (NNO,NEL,NTC,NGL,GLN)
*-----------------------------------------------------------------------

* DEFINIÇÃO DE VARIÁVEIS
      IMPLICIT NONE
      INTEGER NNO,NEL,NGL,GLN,NTC
      INTEGER I
      CHARACTER LINHA*80,ARQ*20,ARQEXT*20

* ABRIR ARQUIVO DE DADOS
      WRITE (*,*) 'ARQUIVO DE DADOS (SEM EXTENSÃO)'
      READ (*,'(A20)') ARQ
      OPEN(4,FILE=ARQ)
      OPEN(1,FILE='ANEST.DAT')

* PREPARAR ARQUIVO DE DADOS (ANEST.DAT)
      I=0
!     DO WHILE (.NOT. EOF(4))
!	  READ (4,'(A80)') LINHA
      DO WHILE (I==0)
	  READ (4,'(A80)',IOSTAT=I) LINHA
	  IF ((LINHA(:1)).NE.";") WRITE (1,'(A80)') LINHA
      END DO
	REWIND(1)

* ABRIR ARQUIVO DE SAÍDA COM EXTENSÃO .SAI

      ARQEXT=ARQ(1:(INDEX(ARQ,' ')-1))//'.SAI'
      OPEN(UNIT=2,FILE=ARQEXT)

* ABRIR ARQUIVO 'SCRATCH' PARA GRAVAR [SG] E {F}
      OPEN (3,STATUS='SCRATCH',FORM='unformatted')

* LER E IMPRIMIR TITULO
      READ (1,*) LINHA
	WRITE (2,*) LINHA

* LER NNO,NEL,NTC E DEFINIR GLN,NGL
 
      READ (1,*) NNO,NEL,NTC
 	GLN=3
	NGL=GLN*NNO

      END
