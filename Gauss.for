************************************************************************
*                    SOLUÇÃO DO SISTEMA [SG].{U}={F}                   *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE GAUSS (SG,U,F,NGL)
*-----------------------------------------------------------------------

* DEFINIÇÃO DE VARIÁVEIS
	IMPLICIT NONE
      INTEGER I,J,K,NGL
	REAL*8 SG(NGL,NGL),F(NGL),U(NGL)

* TRIANGULARIZACAO

      DO K=2,NGL
        DO I=K,NGL
	    F(I)=F(I)-SG(I,K-1)/SG(K-1,K-1)*F(K-1)
          DO J=K,NGL
            SG(I,J)=SG(I,J)-SG(I,K-1)/SG(K-1,K-1)*SG(K-1,J)
          END DO
	  END DO
	END DO

* RETRO-SUBSTITUICAO

      U(NGL)=F(NGL)/SG(NGL,NGL)

      DO K=(NGL-1),1,-1
   	  U(K)=F(K)
        DO J=K+1,NGL
          U(K)=U(K)-SG(K,J)*U(J)
        END DO
        U(K)=U(K)/SG(K,K)
      END DO

      END