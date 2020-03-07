************************************************************************
*             LEITURA E IMPRESSÃO DOS DADOS DOS ELEMENTOS              *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE ELEMENTOS (E,A,IZ,FP,NO,TE,G,IL,QX,QY,NEL,NELT,NELP,
     &                      NGL,NLL,NTC,NC)
*-----------------------------------------------------------------------

* DEFINIÇÃO DE VARIÁVEIS
      IMPLICIT NONE
      INTEGER I,J,K,NEL,NELP,NELT,NO(2,NEL),TE(NEL),G(NEL,6),NLL,NGL,
     &        IL(NEL,6),NTC,NC(NEL)
	REAL*8 E(NTC),A(NTC),IZ(NTC),FP(NEL),QX(NEL),QY(NEL)

* LER E IMPRIMIR TIPOS DE CARACTERÍSTICAS
	WRITE (2,35)
   35 FORMAT(//'CARACTERÍSTICAS DOS ELEMENTOS:'//3X'CARACT.'7X'E'15X
     &       'A'14X'I'/)
      DO I=1,NTC
        READ  (1,* ) J,E(J),A(J),IZ(J)
        WRITE (2,40) J,E(J),A(J),IZ(J)
   40   FORMAT(2X,I5,E15.4,2(F15.5))
      END DO

* LER E IMPRIMIR ELEMENTOS DE PÓRTICO
	WRITE (2,5)
    5 FORMAT(//'ELEMENTOS DE PÓRTICO:'//4X'ELEM.'3X'NÓ1'4X'NÓ2'3X
     &       'CARACT'8X'FP'12X'QX'13X'QY'/)
      READ (1,*) NELP
      DO I=1,NELP
        READ  (1,* ) J,NO(1,J),NO(2,J),NC(J),FP(J),QX(J),QY(J)
        WRITE (2,10) J,NO(1,J),NO(2,J),NC(J),FP(J),QX(J),QY(J)
   10   FORMAT(2X,I5,3(2X,I5),3(F15.5))
	  TE(J)=1
      END DO

* LER E IMPRIMIR ELEMENTOS DE TRELIÇA
      READ (1,*) NELT
      IF (NELT.NE.0)WRITE (2,15)
   15 FORMAT(//'ELEMENTOS DE TRELIÇA:'//4X'ELEM.'3X'NÓ1'4X'NÓ2'3X
     &       'CARACT'8X'FP'/)
      DO I=1,NELT
        READ  (1,* ) J,NO(1,J),NO(2,J),NC(J),FP(J)
        WRITE (2,20) J,NO(1,J),NO(2,J),NC(J),FP(J)
   20   FORMAT(2X,I5,3(2X,I5),F15.5)
	  TE(J)=2
      END DO

*FORMAR MATRIZ G - NUMERAÇÃO DOS GRAUS DE LIBERDADE DOS ELEMENTOS
      DO J=1,NEL
   	  DO I=1,3
	    G(J,I  )=3*(NO(1,J)-1)+I
          G(J,I+3)=3*(NO(2,J)-1)+I
        END DO
	END DO 

* LER E IMPRIMIR LIBERAÇÕES LOCAIS
	READ (1,*) NLL
      IF (NLL/=0) WRITE (2,25)
   25 FORMAT(//'LIBERAÇÕES LOCAIS:'//4X'ELEM.'2(2X'DX'3X'DY'3X'RZ')/)
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
