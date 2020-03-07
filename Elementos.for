************************************************************************
*             LEITURA E IMPRESS�O DOS DADOS DOS ELEMENTOS              *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE ELEMENTOS (E,A,IZ,FP,NO,TE,G,IL,QX,QY,NEL,NELT,NELP,
     &                      NGL,NLL,NTC,NC)
*-----------------------------------------------------------------------

* DEFINI��O DE VARI�VEIS
      IMPLICIT NONE
      INTEGER I,J,K,NEL,NELP,NELT,NO(2,NEL),TE(NEL),G(NEL,6),NLL,NGL,
     &        IL(NEL,6),NTC,NC(NEL)
	REAL*8 E(NTC),A(NTC),IZ(NTC),FP(NEL),QX(NEL),QY(NEL)

* LER E IMPRIMIR TIPOS DE CARACTER�STICAS
	WRITE (2,35)
   35 FORMAT(//'CARACTER�STICAS DOS ELEMENTOS:'//3X'CARACT.'7X'E'15X
     &       'A'14X'I'/)
      DO I=1,NTC
        READ  (1,* ) J,E(J),A(J),IZ(J)
        WRITE (2,40) J,E(J),A(J),IZ(J)
   40   FORMAT(2X,I5,E15.4,2(F15.5))
      END DO

* LER E IMPRIMIR ELEMENTOS DE P�RTICO
	WRITE (2,5)
    5 FORMAT(//'ELEMENTOS DE P�RTICO:'//4X'ELEM.'3X'N�1'4X'N�2'3X
     &       'CARACT'8X'FP'12X'QX'13X'QY'/)
      READ (1,*) NELP
      DO I=1,NELP
        READ  (1,* ) J,NO(1,J),NO(2,J),NC(J),FP(J),QX(J),QY(J)
        WRITE (2,10) J,NO(1,J),NO(2,J),NC(J),FP(J),QX(J),QY(J)
   10   FORMAT(2X,I5,3(2X,I5),3(F15.5))
	  TE(J)=1
      END DO

* LER E IMPRIMIR ELEMENTOS DE TRELI�A
      READ (1,*) NELT
      IF (NELT.NE.0)WRITE (2,15)
   15 FORMAT(//'ELEMENTOS DE TRELI�A:'//4X'ELEM.'3X'N�1'4X'N�2'3X
     &       'CARACT'8X'FP'/)
      DO I=1,NELT
        READ  (1,* ) J,NO(1,J),NO(2,J),NC(J),FP(J)
        WRITE (2,20) J,NO(1,J),NO(2,J),NC(J),FP(J)
   20   FORMAT(2X,I5,3(2X,I5),F15.5)
	  TE(J)=2
      END DO

*FORMAR MATRIZ G - NUMERA��O DOS GRAUS DE LIBERDADE DOS ELEMENTOS
      DO J=1,NEL
   	  DO I=1,3
	    G(J,I  )=3*(NO(1,J)-1)+I
          G(J,I+3)=3*(NO(2,J)-1)+I
        END DO
	END DO 

* LER E IMPRIMIR LIBERA��ES LOCAIS
	READ (1,*) NLL
      IF (NLL/=0) WRITE (2,25)
   25 FORMAT(//'LIBERA��ES LOCAIS:'//4X'ELEM.'2(2X'DX'3X'DY'3X'RZ')/)
      DO I=1,NLL
        READ  (1,* ) K,(IL(K,J),J=1,6)
        WRITE (2,30) K,(IL(K,J),J=1,6)
   30   FORMAT(2X,I5,6I5)
	  DO J=1,6
          IF (IL(K,J)==0) THEN
            NGL=NGL+1          ! SOMAR G.L. ADICIONAL P/ LIBERACAO
            G(K,J)=NGL         ! ATUALIZAR NUMERO DO G.L. LIBERADO
   	    END IF
        END DO
	END DO
	END
