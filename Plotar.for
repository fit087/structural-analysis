************************************************************************
*                       PLOTAGEM DA ESTRUTURA                          *
************************************************************************

*-----------------------------------------------------------------------
      SUBROUTINE PLOTAR (X,Y,NO,NEL,NNO,TE)                       
*-----------------------------------------------------------------------

* DEFINIÇÃO DE VARIÁVEIS
 	USE MSFLIB ! Microsoft Fortran Library
!	USE DFLIB
	IMPLICIT NONE
      RECORD /XYCOORD/ XY
      RECORD /VIDEOCONFIG/ VIDEO
      INTEGER I,STATO,MX,MY,NEL,NNO,NO(2,NEL),XT(NNO),YT(NNO),TE(NEL)
      REAL*8 XMED,YMED,XMAX,YMAX,XMIN,YMIN,ESC,X(NNO),Y(NNO)
	CHARACTER*2 A2
      CHARACTER*1 TECLA

      STATO = INITIALIZEFONTS()
      STATO = SETFONT('t''Arial''n11')
  
* MODO GRAFICO COM RESOLUCAO MAXIMA
      STATO=SETVIDEOMODE( $MAXRESMODE )
      CALL GETVIDEOCONFIG( VIDEO )
      MX = VIDEO.NUMXPIXELS
      MY = VIDEO.NUMYPIXELS

* ESCALA
	XMAX=MAXVAL(X)
      YMAX=MAXVAL(Y) 
	XMIN=MINVAL(X)
      YMIN=MINVAL(Y)
	XMED=(XMAX+XMIN)/2;	YMED=(YMAX+YMIN)/2
      ESC=0.8*MIN(MX/(XMAX-XMIN),MY/(YMAX-YMIN))

* PERCORRER TELAS DE PLOTAGEM   
      DO WHILE (TECLA/=' ')
        CALL CLEARSCREEN($GCLEARSCREEN)

* COORDENADAS NA TELA
        DO I=1,NNO
	    XT(I)=(MX/2+(X(I)-XMED)*ESC)
          YT(I)=(MY/2-(Y(I)-YMED)*ESC)
        END DO

* PLOTAR NÓS EM AZUL CLARO
        STATO = SETCOLOR(11)
	  DO I=1,NNO
          STATO=ELLIPSE($GBORDER,XT(I)-2,YT(I)-2,XT(I)+2,YT(I)+2)
          CALL MOVETO(XT(I)+3,YT(I)-8,XY)
          CALL IA(I,A2)   
 	    CALL OUTGTEXT(A2)
        END DO

* PLOTAR ELEMENTOS
        DO I=1,NEL
          STATO = SETCOLOR(16-TE(I))
          CALL MOVETO(XT(NO(1,I)),YT(NO(1,I)),XY)
          STATO = LINETO(XT(NO(2,I)),YT(NO(2,I)))
          CALL IA(I,A2)   
	    CALL MOVETO(INT((XT(NO(1,I))+XT(NO(2,I)))/2)+3,
     &                INT((YT(NO(1,I))+YT(NO(2,I)))/2)+3,XY)
          CALL OUTGTEXT(A2)
	  END DO

* OPÇÕES DE TELAS
        TECLA=GETCHARQQ()
        SELECT CASE (TECLA)
	    CASE ('+'); ESC=ESC*1.2           ! ZOOM +
          CASE ('-'); ESC=ESC/1.25          ! ZOOM -
	    CASE ('4'); XMED=XMED+MX/(ESC*20) ! MOVER P/ ESQUERDA
	    CASE ('6'); XMED=XMED-MX/(ESC*20) ! MOVER P/ DIREITA
	    CASE ('8'); YMED=YMED-MY/(ESC*20) ! MOVER P/ CIMA
	    CASE ('2'); YMED=YMED+MY/(ESC*20) ! MOVER P/ BAIXO
	    CASE (' '); EXIT                  ! TERMINAR
        END SELECT
 	END DO

      END
