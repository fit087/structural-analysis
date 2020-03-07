************************************************************************
**   A N E S T  :    A N � L I S E   E S T R U T U R A L   (ABR/2008) **
************************************************************************
**   TIPO "STANDARD GRAPHICS APPLICATION" OU "QUICKWIN APPLICATION"   **
************************************************************************
* VARI�VEIS GLOBAIS:                                                   *
*                                                                      *
* INTEIROS - ESCALARES                                                 *
*  GLN..... N�MERO DE GRAUS DE LIBERDADE POR N�S                       *
*  NEL..... N�MERO DE ELEMENTOS                                        *
*  NELP.... N�MERO DE ELEMENTOS DE P�RTICO                             *
*  NELT.... N�MERO DE ELEMENTOS DE TRELI�A                             *
*  NGL..... N�MERO DE GRAUS DE LIBERDADE                               *
*  NNO..... N�MERO DE N�S                                              *
*  NNV..... N�MERO DE N�S VINCULADOS                                   *
*  NLL..... N�MERO DE LIBERACOES LOCAIS                                *
*  NTC..... N�MERO DE TIPOS DE CARACTER�STICAS DE ELEMENTO             *
*                                                                      *
* INTEIROS - VETORES                                                   *
*  NV(I)... N�MERO DO I-�SIMO N� VINCULADO                             *
*  TE(I)... TIPO DO ELEMENTO I (P�RTICO=1, TRELI�A=2)                  *
*  NC(I)... N�MERO DA CARACTER�STICA DO ELEMENTO I                     *
*                                                                      *
* INTEIROS - MATRIZES                                                  *
*  D(I,J).. DIRECAO DO GRAU DE LIBERDADE DO N� I NA DIRE��O J          *
*  G(I,J).. G.L. GLOBAL DO ELEMENTO I NA DIRE��O J                     *
*  IL(I,J). INDICADOR DE LIBERACAO LOCAL N� I, DIR. J (0=LIVRE,1=IMP)  *
*  NO(I,J). N� I DO ELEMENTO J                                         *
*  V(I,J).. V�NCULO DO N� I NA DIRE��O J (0=LIVRE, 1=IMPEDIDO)         *
*                                                                      *
* REAIS - VETORES                                                      *
*  A(I).... �REA DA SE��O DA CARACTER�STICA I                          *
*  E(I).... M�DULO DE ELASTICIDADE DA CARACTER�STICA I                 *
*  F....... VETOR DE FOR�AS GLOBAIS                                    *
*  FP(I)... FORCA DE PROTENS�O NO ELEMENTO I                           *
*  IZ(I)... IN�RCIA DA DA CARACTER�STICA I                             *
*  QX(I)... CARGA DISTRIBU�DA DIR. X NO ELEMENTO I                     *
*  QY(I)... CARGA DISTRIBU�DA DIR. Y NO ELEMENTO I                     *
*  RA...... VETOR DE REA��ES DE APOIO = [SG].{U} - {F}                 *
*  U....... VETOR DE DESLOCAMENTOS                                     *
*  X(I).... COORDENADA X DO N� I                                       *
*  Y(I).... COORDENADA Y DO N� I                                       *
*                                                                      *
* REAIS - MATRIZES                                                     *
*  ESF(I,J) ESFOR�O DIRE��O I ELEMENTO J                               *
*  FA(I,J). FOR�A APLICADA (CARGA NODAL) NO N� I NA DIRE��O J          *
*  SG...... MATRIZ DE RIGIDEZ GLOBAL                                   *
*                                                                      *
************************************************************************
*                               +-----------+                          *
*  ESTRUTURA DO PROGRAMA:       | A N E S T |                          *
*                               +-----------+                          *
*  SEQUENCIA:                         |                                *                                                       *
*                                     |   +---------+                  *                                                       *
*       1-PREPARAR ARQUIVOS           |-->| PREPARQ |                  *                                                          *
*                                     |   +---------+                  *                                                       *
*       2-DADOS DOS N�S               |-->|   NOS   |                  *                                                          *
*                                     |   +---------+                  *                                                       *
*       3-DADOS DOS ELEMENTOS         |-->|ELEMENTOS|                  *                                                          *
*                                     |   +---------+     +---------+  *                                                       *
*       4-FORMAR SISTEMA              |-->| SISTEMA |---->| PORT2D  |  *                                                          *
*                                     |   +---------+  |  +---------+  *                                                       *
*       5-RESOLVER SISTEMA            |-->|  GAUSS  |  |->| TREL2D  |  *                                                          *
*                                     |   +---------+  |  +---------+  *                                                       *
*       6-CALCULAR ESFORCOS           |-->| ESFORCOS|---->| ROTACAO |  *                                                          *
*                                     |   +---------+     +---------+  *                                                       *
*       7-CALCULAR REA��ES            |-->| REACOES |                  *                                                          *
*                                     |   +---------+                  *                                                       *
*       8-IMPRIMIR RESPOSTAS          |-->|RESPOSTAS|                  *                                                          *
*                                     |   +---------+     +---------+  *                                                       *
*       9-PLOTAR MODELO               |-->| PLOTAR  |---->|   IA    |  *                                                          *
*                                         +---------+     +---------+  *                                                       *
*                                                                      *
************************************************************************


* DEFINI��O DE VARI�VEIS
      IMPLICIT NONE
      INTEGER NNO,NEL,NGL,NNV,GLN,NELT,NELP,NLL,NTC
	INTEGER,ALLOCATABLE:: NV(:),TE(:),NO(:,:),G(:,:),V(:,:),D(:,:),
     &                      IL(:,:),NC(:)
      REAL(8),ALLOCATABLE:: X(:),Y(:),E(:),A(:),IZ(:),RA(:),FP(:),F(:),
     &                      U(:),QX(:),QY(:),FA(:,:),SG(:,:),ESF(:,:)

* PREPARAR ARQUIVOS
      CALL PREPARQ (NNO,NEL,NTC,NGL,GLN)
      


* DIMENSIONAR VETORES E MATRIZES 
	ALLOCATE (D(NNO,GLN),V(NNO,GLN),X(NNO),Y(NNO),FA(NNO,GLN),NV(NNO),
     &          NO(2,NEL),TE(NEL),G(NEL,2*GLN),QX(NEL),QY(NEL),NC(NEL),
     &          E(NTC),IL(NEL,2*GLN),A(NTC),IZ(NTC),FP(NEL),ESF(6,NEL))
      
*Zerando variaveis
      FA=0.;
      
* LER E IMPRIMIR DADOS DOS N�S (COORDENADAS, V�NCULOS E CARGAS)
      CALL NOS (NNO,NNV,X,Y,FA,D,V,NV,GLN)

* LER E IMPRIMIR DADOS DOS ELEMENTOS (INCID�NCIAS, CARGAS E LIBERA��ES)
      CALL ELEMENTOS(E,A,IZ,FP,NO,TE,G,IL,QX,QY,NEL,NELT,NELP,NGL,NLL,
     &               NTC,NC)

* DIMENSIONAR MATRIZ DE RIGIDEZ E VETORES DE FOR�AS E DE DESLOCAMENTOS
      ALLOCATE (SG(NGL,NGL),F(NGL),U(NGL),RA(NGL))

* ZERAR SG, F, RA, ESF
	SG=0.; F=0.; RA=0.; ESF=0.

* FORMAR SISTEMA DE EQUA��ES 
      CALL SISTEMA (X,Y,E,A,IZ,SG,F,FA,NO,D,V,TE,NGL,NNO,NEL,G,GLN,FP,
     &              QX,QY,NTC,NC)                        

* RESOLVER SISTEMA
	CALL GAUSS (SG,U,F,NGL)

* CALCULAR ESFORCOS NOS ELEMENTOS
      CALL ESFORCOS (X,Y,E,A,IZ,U,ESF,NO,TE,NEL,NGL,G,NNO,FP,QX,QY,NTC,
     &               NC)

* CALCULAR REA��ES DE APOIO
      CALL REACOES (RA,U,SG,F,NV,D,V,NGL,GLN,NNO,NNV)  

* IMPRIMIR RESPOSTAS
      CALL RESPOSTAS (U,ESF,RA,NO,NV,G,D,TE,NGL,NNO,NEL,NNV,GLN,NELT,
     &                NELP)
* PLOTAR MODELO
!	CALL PLOTAR (X,Y,NO,NEL,NNO,TE) 

      END
   


