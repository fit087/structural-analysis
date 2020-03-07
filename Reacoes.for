************************************************************************
*                   CÁLCULO DAS REAÇÕES DE APOIO                       *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE REACOES (RA,U,SG,F,NV,D,V,NGL,GLN,NNO,NNV)                      
*-----------------------------------------------------------------------

* DEFINIÇÃO DE VARIÁVEIS
      IMPLICIT NONE
      INTEGER I,J,K,NGL,NNV,GLN,NNO,NV(NNO),D(NNO,GLN),V(NNO,GLN)
	REAL*8 SG(NGL,NGL),U(NGL),F(NGL),RA(NGL)

* LER [SG], {F} INICIAIS (ANTES DAS VINVULACOES)
      REWIND(3)
      READ (3) SG,F

* EFETUAR PRODUTO [SG].{U}
      RA=0.
      DO I=1,NGL
        DO J=1,NGL
          RA(I)=RA(I)+SG(I,J)*U(J)
        END DO
	END DO

* SUBTRAIR {F}
      DO I=1,NGL
        RA(I)=RA(I)-F(I)
	END DO

* ZERAR REAÇÕES INEXISTENTES

	DO I=1,NNV
	  DO J=1,GLN
	    K=D(NV(I),J)           ! G.L. NÓ VINCULADO I, DIR. J
          RA(K)=V(NV(I),J)*RA(K) ! V(NV(I),J) =0 OU =1
        END DO
      END DO

	END

