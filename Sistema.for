************************************************************************
*            FORMA��O DO SISTEMA DE EQUA��ES [SG].{U}={F}              *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE SISTEMA(X,Y,E,A,IZ,SG,F,FA,NO,D,V,TE,NGL,NNO,NEL,G,GLN,
     &                   FP,QX,QY,NTC,NC)                         
*-----------------------------------------------------------------------
* VARI�VEIS LOCAIS:                                                    *
*                                                                      *
*  FEQV.... VETOR DE FOR�AS NODAIS EQUIVALENTES NO REFERENCIAL LOCAL   * 
*  R....... MATRIZ DE ROTA��O DE UM ELEMENTO                           *
*  S....... MATRIZ DE RIGIDEZ DE UM ELEMENTO                           *
*  UE...... VETOR DE DESLOCAMENTOS DE UM ELEMENTO NO REF. LOCAL        *
*  SR...... PRODUTO S.R                                                *
*  RSR..... PRODUTO R.SR                                               *
*  L....... COMPRIMENTO DE UM ELEMENTO                                 *
*  FEQVG... VETOR DE FOR�AS NODAIS EQUIVALENTES NO REFERENCIAL GLOBAL  * 

* DEFINI��O DE VARI�VEIS
      IMPLICIT NONE
      INTEGER I,J,K,NNO,NEL,NGL,GLN,N,D(NNO,GLN),V(NNO,GLN),NO(2,NEL),
     &        G(NEL,6),TE(NEL),NC(NEL),NTC
      REAL*8 X(NNO),Y(NNO),E(NTC),A(NTC),IZ(NTC),FA(NNO,GLN),F(NGL),L,
     &       SG(NGL,NGL),R(6,6),S(6,6),SR(6,6),RSR(6,6),FP(NEL),QX(NEL),
     &       QY(NEL),FEQV(6),FEQVG(6)

* PERCORRER TODOS OS ELEMENTOS
      DO N=1,NEL

* FORMAR MATRIZ DE ROTACAO
        CALL ROTACAO (R,X,Y,L,NO,N,NNO,NEL) 

* FORMAR MATRIZ DE RIGIDEZ E FORCAS NODAIS EQUIVALENTES 
	  SELECT CASE (TE(N))  

* ELEMENTO DE P�RTICO
          CASE (1) 
          CALL PORT2D (S,E(NC(N)),A(NC(N)),IZ(NC(N)),L,QX(N),QY(N),
     &                 FP(N),FEQV)

* ELEMENTO DE TRELICA 
	    CASE (2) 
          CALL TREL2D (S,E(NC(N)),A(NC(N)),L,FP(N),FEQV)  
	  END SELECT

* MULTIPLICACAO SR=S.R
        DO I=1,6
          DO J=1,6
            SR(I,J)=0.
            DO K=1,6
              SR(I,J)=SR(I,J)+S(I,K)*R(K,J)
            END DO
	    END DO
	  END DO

* MULTIPLICACAO RSR=R'.SR
        DO I=1,6
          DO J=1,6
            RSR(I,J)=0.
            DO K=1,6
              RSR(I,J)=RSR(I,J)+R(K,I)*SR(K,J)
            END DO
	     END DO
	  END DO

* ADICIONAR MATRIZ DE RIGIDEZ DO ELEMENTO NA MATRIZ GLOBAL
	  DO I=1,6
	    DO J=1,6
            SG(G(N,I),G(N,J)) = SG(G(N,I),G(N,J)) + RSR(I,J)
          END DO
        END DO

* FORCAS EQUIVALENTES NO REFERENCIAL GLOBAL
        DO I=1,6
        FEQVG(I)=0.
          DO J=1,6
            FEQVG(I)=FEQVG(I)+R(J,I)*FEQV(J)
          END DO
        END DO

* ADICIONAR FORCAS NODAIS EQUIVALENTES DO ELEMENTO EM {F}
	  DO I=1,6 
          F(G(N,I))=F(G(N,I))+FEQVG(I)
        END DO

* PR�XIMO ELEMENTO
      END DO

* ADICIONAR FORCAS NODAIS NO VETOR DE FOR�AS {F}
      DO I=1,NNO
        DO J=1,GLN
          F(D(I,J))= F(D(I,J)) + FA(I,J)
        END DO
	END DO

* GRAVAR [SG], {F} INICIAIS (ANTES DAS VINVULACOES)
      WRITE (3) SG,F

* IMPOR V�NCULOS COM A T�CNICA DE 1 E 0
	DO I=1,NNO
        DO J=1,3
          IF (V(I,J).EQ.1) THEN 
            DO K=1,NGL
              SG(K,D(I,J))=0.
              SG(D(I,J),K)=0.
            END DO
            SG(D(I,J),D(I,J))=1.
            F(D(I,J))=0.
          END IF
	  END DO
	END DO

 	END
