************************************************************************
*                  CÁLCULO DOS ESFORÇOS NOS ELEMENTOS                  *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE ESFORCOS(X,Y,E,A,IZ,U,ESF,NO,TE,NEL,NGL,G,NNO,FP,QX,QY,
     &                    NTC,NC)                      
*-----------------------------------------------------------------------
* VARIÁVEIS LOCAIS:                                                    *
*                                                                      *
*  FEQV.... VETOR DE FORÇAS NODAIS EQUIVALENTES NO REFERENCIAL LOCAL   * 
*  R....... MATRIZ DE ROTAÇÃO DE UM ELEMENTO                           *
*  S....... MATRIZ DE RIGIDEZ DE UM ELEMENTO                           *
*  UE...... VETOR DE DESLOCAMENTOS DE UM ELEMENTO NO REF. LOCAL        *
*  L....... COMPRIMENTO DE UM ELEMENTO                                 *

* DEFINIÇÃO DE VARIÁVEIS
      IMPLICIT NONE
      INTEGER I,J,K,NGL,NNO,NEL,NO(2,NEL),G(NEL,6),TE(NEL),NTC,NC(NEL)
	REAL*8 X(NNO),Y(NNO),E(NEL),A(NEL),U(NGL),IZ(NEL),F(NGL),R(6,6),
     &       S(6,6),ESF(6,NEL),FP(NEL),QX(NEL),QY(NEL),UE(6),FEQV(6),L

* PERCORRER TODOS OS ELEMENTOS
      DO I=1,NEL

* FORMAR MATRIZ DE ROTACAO
        CALL ROTACAO (R,X,Y,L,NO,I,NNO,NEL) 
 
* FORMAR MATRIZ DE RIGIDEZ E FORCAS NODAIS EQUIVALENTES 
	  SELECT CASE (TE(I))  

* ELEMENTO DE PÓRTICO
          CASE (1) 
          CALL PORT2D (S,E(NC(I)),A(NC(I)),IZ(NC(I)),L,QX(I),QY(I),
     &                 FP(I),FEQV)

* ELEMENTO DE TRELICA 
	    CASE (2) 
          CALL TREL2D (S,E(NC(I)),A(NC(I)),L,FP(I),FEQV)  
	  END SELECT

* DESLOCAMENTOS DO ELEMENTO NO REFERENCIAL LOCAL {UE}=[R].{U}
        DO J=1,6
          UE(J)=0.
          DO K=1,6
            UE(J)=UE(J)+R(J,K)*U(G(I,K))
	    END DO
	  END DO

* PRODUTO [S].{UE}
	  DO J=1,6
          DO K=1,6
           ESF(J,I) = ESF(J,I) + S(J,K)*UE(K)
          END DO
	  END DO

* SUBTRAIR FEQV
	  DO J=1,6
          ESF(J,I) = ESF(J,I) - FEQV(J)
	  END DO

* PROXIMO ELEMENTO
	END DO

	END

