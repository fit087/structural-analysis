************************************************************************
**   A N E S T  :    A N Á L I S E   E S T R U T U R A L   (ABR/2008) **
************************************************************************
**   TIPO "STANDARD GRAPHICS APPLICATION" OU "QUICKWIN APPLICATION"   **
************************************************************************
* VARIÁVEIS GLOBAIS:                                                   *
*                                                                      *
* INTEIROS - ESCALARES                                                 *
*  GLN..... NÚMERO DE GRAUS DE LIBERDADE POR NÓS                       *
*  NEL..... NÚMERO DE ELEMENTOS                                        *
*  NELP.... NÚMERO DE ELEMENTOS DE PÓRTICO                             *
*  NELT.... NÚMERO DE ELEMENTOS DE TRELIÇA                             *
*  NGL..... NÚMERO DE GRAUS DE LIBERDADE                               *
*  NNO..... NÚMERO DE NÓS                                              *
*  NNV..... NÚMERO DE NÓS VINCULADOS                                   *
*  NLL..... NÚMERO DE LIBERACOES LOCAIS                                *
*  NTC..... NÚMERO DE TIPOS DE CARACTERÍSTICAS DE ELEMENTO             *
*                                                                      *
* INTEIROS - VETORES                                                   *
*  NV(I)... NÚMERO DO I-ÉSIMO NÓ VINCULADO                             *
*  TE(I)... TIPO DO ELEMENTO I (PÓRTICO=1, TRELIÇA=2)                  *
*  NC(I)... NÚMERO DA CARACTERÍSTICA DO ELEMENTO I                     *
*                                                                      *
* INTEIROS - MATRIZES                                                  *
*  D(I,J).. DIRECAO DO GRAU DE LIBERDADE DO NÓ I NA DIREÇÃO J          *
*  G(I,J).. G.L. GLOBAL DO ELEMENTO I NA DIREÇÃO J                     *
*  IL(I,J). INDICADOR DE LIBERACAO LOCAL NÓ I, DIR. J (0=LIVRE,1=IMP)  *
*  NO(I,J). NÓ I DO ELEMENTO J                                         *
*  V(I,J).. VÍNCULO DO NÓ I NA DIREÇÃO J (0=LIVRE, 1=IMPEDIDO)         *
*                                                                      *
* REAIS - VETORES                                                      *
*  A(I).... ÁREA DA SEÇÃO DA CARACTERÍSTICA I                          *
*  E(I).... MÓDULO DE ELASTICIDADE DA CARACTERÍSTICA I                 *
*  F....... VETOR DE FORÇAS GLOBAIS                                    *
*  FP(I)... FORCA DE PROTENSÃO NO ELEMENTO I                           *
*  IZ(I)... INÉRCIA DA DA CARACTERÍSTICA I                             *
*  QX(I)... CARGA DISTRIBUÍDA DIR. X NO ELEMENTO I                     *
*  QY(I)... CARGA DISTRIBUÍDA DIR. Y NO ELEMENTO I                     *
*  RA...... VETOR DE REAÇÕES DE APOIO = [SG].{U} - {F}                 *
*  U....... VETOR DE DESLOCAMENTOS                                     *
*  X(I).... COORDENADA X DO NÓ I                                       *
*  Y(I).... COORDENADA Y DO NÓ I                                       *
*                                                                      *
* REAIS - MATRIZES                                                     *
*  ESF(I,J) ESFORÇO DIREÇÃO I ELEMENTO J                               *
*  FA(I,J). FORÇA APLICADA (CARGA NODAL) NO NÓ I NA DIREÇÃO J          *
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
*       2-DADOS DOS NÓS               |-->|   NOS   |                  *                                                          *
*                                     |   +---------+                  *                                                       *
*       3-DADOS DOS ELEMENTOS         |-->|ELEMENTOS|                  *                                                          *
*                                     |   +---------+     +---------+  *                                                       *
*       4-FORMAR SISTEMA              |-->| SISTEMA |---->| PORT2D  |  *                                                          *
*                                     |   +---------+  |  +---------+  *                                                       *
*       5-RESOLVER SISTEMA            |-->|  GAUSS  |  |->| TREL2D  |  *                                                          *
*                                     |   +---------+  |  +---------+  *                                                       *
*       6-CALCULAR ESFORCOS           |-->| ESFORCOS|---->| ROTACAO |  *                                                          *
*                                     |   +---------+     +---------+  *                                                       *
*       7-CALCULAR REAÇÕES            |-->| REACOES |                  *                                                          *
*                                     |   +---------+                  *                                                       *
*       8-IMPRIMIR RESPOSTAS          |-->|RESPOSTAS|                  *                                                          *
*                                     |   +---------+     +---------+  *                                                       *
*       9-PLOTAR MODELO               |-->| PLOTAR  |---->|   IA    |  *                                                          *
*                                         +---------+     +---------+  *                                                       *
*                                                                      *
************************************************************************


* DEFINIÇÃO DE VARIÁVEIS
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
      
* LER E IMPRIMIR DADOS DOS NÓS (COORDENADAS, VÍNCULOS E CARGAS)
      CALL NOS (NNO,NNV,X,Y,FA,D,V,NV,GLN)

* LER E IMPRIMIR DADOS DOS ELEMENTOS (INCIDÊNCIAS, CARGAS E LIBERAÇÕES)
      CALL ELEMENTOS(E,A,IZ,FP,NO,TE,G,IL,QX,QY,NEL,NELT,NELP,NGL,NLL,
     &               NTC,NC)

* DIMENSIONAR MATRIZ DE RIGIDEZ E VETORES DE FORÇAS E DE DESLOCAMENTOS
      ALLOCATE (SG(NGL,NGL),F(NGL),U(NGL),RA(NGL))

* ZERAR SG, F, RA, ESF
	SG=0.; F=0.; RA=0.; ESF=0.

* FORMAR SISTEMA DE EQUAÇÕES 
      CALL SISTEMA (X,Y,E,A,IZ,SG,F,FA,NO,D,V,TE,NGL,NNO,NEL,G,GLN,FP,
     &              QX,QY,NTC,NC)                        

* RESOLVER SISTEMA
	CALL GAUSS (SG,U,F,NGL)

* CALCULAR ESFORCOS NOS ELEMENTOS
      CALL ESFORCOS (X,Y,E,A,IZ,U,ESF,NO,TE,NEL,NGL,G,NNO,FP,QX,QY,NTC,
     &               NC)

* CALCULAR REAÇÕES DE APOIO
      CALL REACOES (RA,U,SG,F,NV,D,V,NGL,GLN,NNO,NNV)  

* IMPRIMIR RESPOSTAS
      CALL RESPOSTAS (U,ESF,RA,NO,NV,G,D,TE,NGL,NNO,NEL,NNV,GLN,NELT,
     &                NELP)
* PLOTAR MODELO
!	CALL PLOTAR (X,Y,NO,NEL,NNO,TE) 

      END
   


