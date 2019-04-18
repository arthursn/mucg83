        PROGRAM MAP_STEEL_MUCG83
C     Version 0.1 -- 3th May 2006 -- MJP
C     Modified version of the map program MUCG73 available at
C       http://www.msm.cam.ac.uk/map/
C
C       Modifications by Mathew Peet
C       Subroutines MAT_MS and MAT_BS Copyright Mathew Peet
C
C     Original Program (MUCG73.FOR)
C     Copyright Dr. H. K. D. H. Bhadeshia, University of Cambridge
C     Department of Materials Science and Metallurgy, Pembroke St. Cambridge CB2 3QZ
C     
C     Produced 6th March 1987.
C     Modified for MAP February 12th, 1999.
C     Subroutine BOUND modified to prevent program going into endless loop if input
C     data is incorrect.
C     
C     FTVSCLR PROGRAM=.THERMO:MUCG45 DATA=.DATA
C     
C     TYPICAL DATASET FOLLOWS:
C     1 (identification number)
C     0.39  2.05  0.00  4.08  0.00  0.00  0.00  2.00  1.50  0.50  2.00 
C     C     Si    Mn    Ni    Mo    Cr    V     Co    Cu    Al    W   
C     
C     MAP subroutines used :-       MAP functions used :-
C     
C     STEEL :-                      STEEL :-
C                                      MAP_STEEL_AFEG    
C     MAP_STEEL_GMAAX             MAP_STEEL_CG          
C     MAP_STEEL_HETRO             MAP_STEEL_DAFEG      
C     MAP_STEEL_OMEGA2            MAP_STEEL_DCG   
C     MAP_STEEL_ENERGY_REVERT (MAP_STEEL_ENERGY)           
C     MAP_STEEL_FTO1
C     MAP_STEEL_G91 
C     MAP_STEEL_XALPH
C     
C     MAT_MS
C     MAT_BS
C
C
C     UNUSED
C     MAP_STEEL_ENERGY2
C     MAP_STEEL_GALGA
C
C     UTIL :-                         
C     
C     MAP_UTIL_ANALY
C     MAP_UTIL_REED
C     MAP_UTIL_REEDI
C     
C     -------------------------------------------------------------------
C     
C     
      IMPLICIT NONE
C     
      DOUBLE PRECISION C(12),D(12),DDFTO(1000),DIFFH(1000),DT4(1000)
      DOUBLE PRECISION DXQ(1000),SHEARH(1000)
C     
      DOUBLE PRECISION A,AEQ,AFE,AFEH,AFEQ,AFE44,AH,AJ,AJH,AJ1,A1,A1H
      DOUBLE PRECISION A44,BS,CONST,CORR,DAFE44,DA44,DFTO,DF441,DIFFT
      DOUBLE PRECISION ETEQ,ETEQ2,F,FH,FPRO,FPROA,FSON,FTO,FTO400,F44
      DOUBLE PRECISION GMAX,GMAXH,G9,G9400,H,H1,MS,MT,R,S,SHEART
      DOUBLE PRECISION SLOPE,STRAIN,S1,T,TEQ,TEQ2,T10,T10H,T20
      DOUBLE PRECISION T20H,T4,VOLF,V14,W,WH,WS,WS1,W1,X,XA
      DOUBLE PRECISION XBAR,XBARH,XEQ,XEQ2,XH,XTO,XTO400,X1,X44
C
      DOUBLE PRECISION MAP_STEEL_AFEG,MAP_STEEL_CG,MAP_STEEL_DAFEG
      DOUBLE PRECISION MAP_STEEL_DCG,MAP_STEEL_ENERGY_REVERT
C      DOUBLE PRECISION MAP_STEEL_ENERGY2
      DOUBLE PRECISION MAP_STEEL_FTO1
      DOUBLE PRECISION MAP_STEEL_G91,MAP_STEEL_XALPH 
C
      DOUBLE PRECISION MAT_MS,MAT_BS
C
      INTEGER I,ID,J1,J2,J6,J5,J8,J9,J98,J99
C     
      T4  = 0.00D0
      WS1 = 0.00D0
      R   = 8.31432D0
      MT  = 1793D0
      J1  = 20
C     
      DO 380 I=1,J1,1
         CALL LOGO
         WRITE(*,1)
         CALL MAP_UTIL_REEDI(J6)
         IF (J6.EQ.0) GOTO 390
C     C
         WRITE(*,2)
         CALL MAP_UTIL_REED(C(1))
         IF(C(1) .LT. 0.1D-03 .OR. C(1) .GT. 2.0D+00)
     &        CALL BOUND(C(1),0.1D-03,2.0D+00)
C     Si
         WRITE(*,3)
         CALL MAP_UTIL_REED(C(2))
         IF(C(2) .LT. 0.0D+00 .OR. C(2) .GT. 2.5D+00)
     &        CALL BOUND(C(2),0.0D+00,2.5D+00)
C     Mn
         WRITE(*,4)
         CALL MAP_UTIL_REED(C(3))
         IF(C(3) .LT. 0.0D+00 .OR. C(3) .GT. 5.5D+00)
     &        CALL BOUND(C(3),0.0D+00,5.5D+00)
C     Ni
         WRITE(*,5)
         CALL MAP_UTIL_REED(C(4))
         IF(C(4) .LT. 0.0D+00 .OR. C(4) .GT. 4.5D+00)
     &        CALL BOUND(C(4),0.0D+00,4.5D+00)
C     Mo
         WRITE(*,6)
         CALL MAP_UTIL_REED(C(5))
         IF(C(5) .LT. 0.0D+00 .OR. C(5) .GT. 1.5D+00)
     &        CALL BOUND(C(5),0.0D+00,1.5D+00)
C     Cr
         WRITE(*,7)
         CALL MAP_UTIL_REED(C(6))
         IF(C(6) .LT. 0.0D+00 .OR. C(6) .GT. 3.5D+00)
     &        CALL BOUND(C(6),0.0D+00,3.5D+00)
C     V
         WRITE(*,8)
         CALL MAP_UTIL_REED(C(7))
         IF(C(7) .LT. 0.0D+00 .OR. C(7) .GT. 1.5D+00)
     &        CALL BOUND(C(7),0.0D+00,1.5D+00)
C     Co
         WRITE(*,9)
         CALL MAP_UTIL_REED(C(8))
         IF(C(8) .LT. 0.0D+00 .OR. C(8) .GT. 4.0D+00)
     &        CALL BOUND(C(8),0.0D+00,4.0D+00)
C     Cu
         WRITE(*,10)
         CALL MAP_UTIL_REED(C(9))
         IF(C(9) .LT. 0.0D+00 .OR. C(9) .GT. 4.0D+00)
     &        CALL BOUND(C(9),0.0D+00,4.0D+00)
C     Al
         WRITE(*,11)
         CALL MAP_UTIL_REED(C(10))
         IF(C(10) .LT. 0.0D+00 .OR. C(10) .GT. 2.0D+00)
     &        CALL BOUND(C(10),0.0D+00,2.0D+00)
C     W
         WRITE(*,12)
         CALL MAP_UTIL_REED(C(11))
         IF(C(11) .LT. 0.0D+00 .OR. C(11) .GT. 4.0D+00)
     &        CALL BOUND(C(11),0.0D+00,4.0D+00)
C     
C     Write compositional data to output
C     
         WRITE(*,13)
         WRITE(*,14)
         WRITE(*,15) J6
         WRITE(*,16) (C(J5),J5 = 1,6)
         WRITE(*,17) (C(J5),J5 = 7,11)
         WRITE(*,14)
C     
         ID = 1
         CALL MAP_STEEL_OMEGA2(C,W,XBAR,T10,T20,ID)
         WRITE(*,16) (C(J5),J5 = 1,6)
         WRITE(*,17) (C(J5),J5 = 7,11)
C     
         ID = 2
         J8=0
         J9=0
         WS=0.0D0
         FTO=-1.0D0
         X1=C(1)
         XA=0.001D0
         W1=48570.0D0
         H=38575.0D0
         S=13.48D0
         XEQ2=0.15D0
         STRAIN=400.00D+00
         XEQ=0.2D0
         XTO=0.07D0
         XTO400=0.06D0
         X44=0.1D0
         CONST=1.0D+00
         CORR=1.0D+00
         SLOPE=1.0D+00
C     
         WRITE (6,14)
         WRITE (6,18) X1,T10,T20,W
C     CALL MAP_STEEL_HETRO(C,D,MT,R)
C     WRITE (6,19) MT
C     WRITE(6,16) (D(J5),J5=1,6)
C     WRITE(6,17) (D(J5),J5=7,11)
         WRITE(6,13)
         WRITE(6,20)
C     CALL MAP_STEEL_OMEGA2(D,WH,XBARH,T10H,T20H,ID)
C     
C     Q MODIFIED TO CALCULATE FROM 100 DEGREES, 5 DEGREES INTERVAL
         DO 320 J2=473,1173,5
C      DO 320 J2=173,1173,1
            J8=J8+1
            J98=0
            J99=0
            XTO=XTO+0.0001
            XTO400=XTO400+0.0001
            X44=0.3*XEQ
            T=J2*1.0D+00
            IF (T .LE. 1000.0) GOTO 100
            H1=105525
            S1=45.34521
            GOTO 110
 100        H1=111918
            S1=51.44
 110        F=MAP_STEEL_ENERGY_REVERT(T,T10,T20)
            AJ=1-DEXP(-W/(R*T))
            AJ1=1-DEXP(-W1/(R*T))
 120        TEQ=R*T*MAP_STEEL_AFEG(XEQ,AJ)-F
            IF (DABS(TEQ) .LT. 1.0) GOTO 130
            ETEQ=MAP_STEEL_DAFEG(XEQ,AJ)*R*T
            XEQ=XEQ-TEQ/ETEQ
            GOTO 120
 130        TEQ2=R*T*MAP_STEEL_AFEG(XEQ2,AJ)-F-STRAIN
            IF (DABS(TEQ2) .LT. 1.0) GOTO 140
            ETEQ2=MAP_STEEL_DAFEG(XEQ2,AJ)*R*T
            XEQ2=XEQ2-TEQ2/ETEQ2
            GOTO 130
 140        AEQ=MAP_STEEL_CG(XEQ,T,W,R)
            AFEQ=MAP_STEEL_AFEG(XEQ,AJ)
            IF (XEQ .LT. 1.1*X1) GOTO 150
            GOTO 170
 150        IF(WS .EQ. 0.0) GOTO 160
            GOTO 330
 160        WS=T4
            GOTO 330
 170        A=MAP_STEEL_CG(X1,T,W,R)
            AFE=MAP_STEEL_AFEG(X1,AJ)
            FSON=R*T*(XA*(AEQ-A)+(1-XA)*(AFEQ-AFE))
            FPRO=R*T*(X1*(AEQ-A)+(1-X1)*(AFEQ-AFE))
C     
C     V14 is the free energy needed to nucleate Widmanstatten ferrite.
C     If V14=0, jump to line 210.
C     AFTER SB126.DELF
C     
            V14=3.574*(T-273)-2374.0
            IF (V14 .GE. 0.0) GOTO 210
            IF (FPRO .GE. V14) GOTO 200
 180        A44=MAP_STEEL_CG(X44,T,W,R)
            AFE44=MAP_STEEL_AFEG(X44,AJ)
            F44=R*T*(X44*(AEQ-A44)+(1D0-X44)*(AFEQ-AFE44))
            F44=F44-V14
            IF (DABS(F44) .GE. 10.0) GOTO 190
            GOTO 230
 190        DA44=MAP_STEEL_DCG(X44,T,W,R)
            DAFE44=MAP_STEEL_DAFEG(X44,AJ)
            DF441=R*T*(AEQ-X44*DA44-AFEQ+X44*DAFE44-DAFE44-A44+AFE44)
            X44=X44-F44/DF441
            GOTO 180
 200        X44=0.0D0
            IF (J9 .GT. 0) GOTO 230
            J9=1
            WS=T4
            GOTO 230
 210        X44=99999999999.99999
C     
C     Note that the stored energy 50J/mol of Wid ferrite must be satisfied.
C     
            IF(FPRO .GT. -50.0)GOTO 230
            IF(J9 .GT. 2) GOTO 220
            J9=3
            WS1=T4
 220        WS=T4
 230        FPROA=FPRO*((XEQ-XA)/(XEQ-X1))
            CALL MAP_STEEL_GMAAX(A1,A,W1,F,R,T,X,AFE,H1,S1)
            GMAX=R*T*(A1-A)
            T4=T-273
            IF (FTO .GE. 0.0) GOTO 240
            FTO=MAP_STEEL_FTO1(H,S,X1,T,W,W1,H1,S1,F,AJ,AJ1,R)
            GOTO 250
 240        FTO =0.0
 250        DFTO=MAP_STEEL_FTO1(H,S,XTO,T,W,W1,H1,S1,F,AJ,AJ1,R)
            J98=J98+1
            IF (DABS(DFTO) .LE. 10.0) GOTO 270
            G9=MAP_STEEL_G91(XTO,T,W,W1,H1,S1,F,H,S,AJ,AJ1)
            IF (J98 .GE.9) GOTO 270
            XTO=XTO-DFTO/G9
            IF (XTO .LE. 0.0001) GOTO 260
            GOTO 250
 260        XTO=0.0000
 270        FTO400=MAP_STEEL_FTO1(H,S,XTO400,T,W,W1,H1,S1,F,AJ,AJ1,R)
     &      +400.0
            J99=J99+1
            IF(J99 .GE. 9) GOTO 290
            IF (DABS(FTO400) .LE. 10.0) GOTO 290
            G9400=MAP_STEEL_G91(XTO400,T,W,W1,H1,S1,F,H,S,AJ,AJ1)
            XTO400=XTO400-FTO400/G9400
            IF (XTO400 .LE. 0.0001) GOTO 280
            GOTO 270
 280        XTO400=0.0000
 290        CALL SHEAR(SHEART,GMAX,T)
            CALL DIFFU(DIFFT,GMAX,T)
C-----------------------------------------------------------------------------
C     
C     Calculation of the EFFECT of solute depleted region ON C CURVES
C     NOTE calc wrong if bainite curve has to be cut off before intersection
C     with upper c curve.
            FH=MAP_STEEL_ENERGY_REVERT(T,T10H,T20H)
            AJH=1.0D+00-DEXP(-WH/(R*T))
            AFEH=MAP_STEEL_AFEG(D(1),AJH)
            AH=MAP_STEEL_CG(D(1),T,WH,R)
            CALL MAP_STEEL_GMAAX(A1H,AH,W1,FH,R,T,XH,AFEH,H1,S1)
            GMAXH=R*T*(A1H-AH)
            CALL SHEAR(SHEARH,GMAXH,T)
            CALL DIFFU(DIFFH,GMAXH,T)
C-----------------------------------------------------------------------------
C     IF (X44 .EQ. 0.0) GOTO 300
C     GOTO 310
C     300       SHEART=1D+20
 310        VOLF = (XEQ-XBAR)/(XEQ-MAP_STEEL_XALPH(T))
            WRITE(6,21)  FPRO,FPROA,GMAX,T4,X,FSON,XEQ,XEQ2,FTO,XTO,
     &           VOLF,X44,XTO400,J99,SHEART,DIFFT
            DXQ(J8)=XEQ
            DDFTO(J8)=FTO
            DT4(J8)=T4
 320     CONTINUE
 330     J8=J8-1
         WRITE(6,14)
         WRITE(6,22)
CC
C
C        CALL MAP_UTIL_ANALY(J8,10,CONST,SLOPE,CORR,DT4,DDFTO)
C
C         BS=(-400.0-CONST)/SLOPE
C         MS=(-1120.0D+00-10568.0D+00*X1+94.1D+00-CONST)/SLOPE
         BS=MAT_BS(J8,DT4,DDFTO)
         MS=MAT_MS(J8,DT4,DDFTO,X1)
C
        IF (J9 .EQ. 1) GOTO 340
         WRITE(6,23)WS1,WS
         GOTO 350
 340     WRITE(6,24)WS
         WRITE(6,25)BS
 350     IF (WS .LT. BS) GOTO 360
         GOTO 370
 360     BS=WS
 370     WRITE(6,26)BS
         WRITE(6,27)MS
         WRITE(6,13)
         WRITE(*,*) "BS AND MS CALCULATED BY INTERPOLATION"
         WRITE(*,*) "FROM POINTS ABOVE AND BELOW CRITICAL TEMP"
         WRITE(*,*) "MODIFICATION BY MATHEW PEET"
         WRITE(6,13)
 380  CONTINUE
 390  STOP
C     
 1    FORMAT(////19X,' Identification Number ?'/
     &     19X,' (zero to exit program)')
 2    FORMAT(19X,' Carbon wt.% ?')
 3    FORMAT(19X,' Silicon wt.% ?')
 4    FORMAT(19X,' Manganese wt.% ?')
 5    FORMAT(19X,' Nickel wt.% ?')
 6    FORMAT(19X,' Molybdenum wt.% ?')
 7    FORMAT(19X,' Chromium wt.% ?')
 8    FORMAT(19X,' Vanadium wt.% ?')
 9    FORMAT(19X,' Cobalt wt.% ?')
 10   FORMAT(19X,' Copper wt.% ?')
 11   FORMAT(19X,' Aluminium wt.% ?')
 12   FORMAT(19X,' Tungsten wt.% ?')
 13   FORMAT('--------------------------------------------------',
     &     '-------------------------------',
     &     '----------------------------------------------')
 14   FORMAT (4H    )
 15   FORMAT (16H *******  NUMBER,I5,11H  *********)
 16   FORMAT (6H    C=,F8.4,6H   Si=,F8.4,6H   Mn=,F8.4,
     &     6H   Ni=,F8.4,6H   Mo=,F8.4,6H   Cr=,F8.4)
 17   FORMAT (6H    V=,F8.4,6H   Co=,F8.4,6H   Cu=,F8.4,
     &     6H   Al=,F8.4,6H    W=,F8.4)
 18   FORMAT (17H  CARBON CONTENT=,F10.5,6H  T10=,F10.6,
     &     6H  T20=,F10.6,10H   WGAMMA=,F7.0)
 19   FORMAT('  MELTING TEMPERATURE OF DELTA FERRITE = ',
     &     F8.0, ' KELVIN')
 20   FORMAT('   FPRO  FPROA   GMAX  CTEMP',
     &     11H  X NUCLEUS,6H  FSON,9H      XEQ,9H    XEQ50,
     &     6H   FTO,9H      XTO,18H     VOLF    X44  ,
     &     11H    XTO400 ,15H      SHEART   ,10H  DIFFT   )
 21   FORMAT (1H ,F7.0,2F7.0,F6.0,D10.2,F8.0,
     &     2F9.4,F7.0,F8.4,1H ,F8.4,D10.3,F8.4,1H ,
     &     I2,D11.2,D11.2)
 22   FORMAT('    ***** FTO VERSUS TEMPERATURE ****   ')
 23   FORMAT(' WIDMANSTATTEN START TEMP RANGE=',F7.0,' C TO ',F7.0,
     &     ' C')
 24   FORMAT(' WIDMANSTATTEN FERRITE START TEMPERATURE  =',F7.0,' C')
 25   FORMAT(' GROWTH LIMITED BAINITE START TEMPERATURE =',F7.0,' C')
 26   FORMAT(' NUCLEATION LIMITED BAINITE START TEMP    =',F7.0,' C')
 27   FORMAT(' MARTENSITE START TEMPERATURE             =',F7.0,' C')
      END
C     
C#################################################################
C     
C*****************************************************************
C     
C     H. K. D. H. Bhadeshia, 1984
C     Natural logarithm of the activity of iron in austenite
C     
      DOUBLE PRECISION FUNCTION MAP_STEEL_AFEG(XEQ,AJ)
C     
      IMPLICIT NONE
      DOUBLE PRECISION XEQ,AJ,DEQ,TEQ
C     
      DEQ=DSQRT(1-2*(1+2*AJ)*XEQ+(1+8*AJ)*XEQ*XEQ)
      TEQ=5.0*DLOG((1-XEQ)/(1-2*XEQ))
      TEQ=TEQ+6.0*DLOG((1-2*AJ+(4*AJ-1)*XEQ-DEQ)/(2*AJ*(2*XEQ-1)))
      MAP_STEEL_AFEG=TEQ
C     
         RETURN
         END
C     
C*****************************************************************
C     
C     H. K. D. H. Bhadeshia, 1984
C     Differential of the natural logarithm of the activity of iron in gamma
C     
      DOUBLE PRECISION FUNCTION MAP_STEEL_DAFEG(XEQ,AJ)
C     
      IMPLICIT NONE
      DOUBLE PRECISION ETEQ,ETEQ2,DEQ,XEQ,AJ
      
      DEQ=DSQRT(1-2*(1+2*AJ)*XEQ+(1+8*AJ)*XEQ*XEQ)
      ETEQ=5*((1/(XEQ-1))+2/(1-2*XEQ))
      ETEQ2=6*((4*AJ-1-(0.5/DEQ)*(-2-4*AJ+2*XEQ+16*XEQ*AJ))
     &     /(1-2*AJ+(4*AJ-1)*XEQ-DEQ))+6*(4*AJ/(2*AJ*(2*XEQ-1)))
      MAP_STEEL_DAFEG=ETEQ+ETEQ2
         RETURN
         END
C     
C*****************************************************************
C     
C     
C     Produced by :-
C     
C     H. K. D. H. Bhadeshia
C     
C     Department of Materials Science and Metallurgy,
C     Pembroke Street, Cambridge CB2 3QZ, U. K.
C     31 March 1987
C     
C     Calculates the free energy change accompanying the transformation
C     from austenite to ferrite of the same chemical composition, including
C     a Zener ordering term. The latter describes the ordering of carbon
C     atoms, that leads the body-centered cubic lattice of ferrite becoming
C     body-centered tetragonal. The degree of ordering increases as the
C     carbon concentration increases, or as the transformation temperature
C     decreases.
C     
C     References:
C     
C     1. C. Zener Trans. AIME 167 (1946) 550.
C     
C     2. J. C. Fisher, Metals Transactions 185 (1949) 688-690.
C     
C     3. H. K. D. H. Bhadeshia and D. V. Edmonds
C     Acta Metallurgica 28 (1980) 1265-1273.
C     Also see laboratory book 7.
C     
C----------------------------------------------------------------------
C     
      DOUBLE PRECISION FUNCTION MAP_STEEL_FTO1(H,S,X,T,W,W1,
     &     H1,S1,F,AJ,AJ1,R)
C     
C     
C     modified July 2002, to correct calculation for the range T60 .gt. 0.25
C     courtesy Chikaro Nakao
C     
      IMPLICIT NONE
C     
C     Input arguments.
C     
      DOUBLE PRECISION AJ, AJ1, F, H, H1, R, S, S1, T, W, W1, X
C     
C     Local variables.
C     
      DOUBLE PRECISION D, D11, T60, ZENER, ZEN1, ZEN2
C     
C     Begin function :-
C     
      D  = SQRT(1-2*(1+2*AJ)*X+(1+8*AJ)*X*X)
      D11= SQRT(9-6*X*(2*AJ1+3)+(9+16*AJ1)*X*X)
C     
C     Calculate T60, which is the value of the actual temperature T
C     divided by the critical temperature beyond which there is no
C     Zener ordering. X is the mole fraction of carbon in the ferrite.
C     If T60>1, the transformation occurs above the ordering temperature
C     and Zener ordering does not occur.
C     
      T60 = T*(1-X)/(28080*X)
C     
      IF (T60 .GT. 1.0) THEN
         GOTO 30
      END  IF
C     
C     Calculate the contribution to the free energy change due to
C     Zener ordering.
C     
      IF (T60 .LT. 0.25) THEN
         GOTO 10
      END  IF
C     
C     Polynomial fit to data in Table 2 of Fischer (1949)
C     
      ZEN1=0.2307+42.7974*T60-233.8631*(T60**2)+645.4485*(T60**3)
     &     -954.3995*(T60**4)+711.8095*(T60**5)-211.5136*(T60**6)
      ZEN2=-2.6702+45.6337*T60-225.3965*(T60**2)+567.7112*(T60**3)
     &     -771.6466*(T60**4)+538.1778*(T60**5)-151.3818*(T60**6)
      GOTO 20
C     
 10   CONTINUE
      ZEN2 = 1
      ZEN1 = 3.295
C     
 20   CONTINUE
      ZENER=(((ZEN2*X)**2)*(-50898.56)/(1-X))+ZEN1*T*X*0.6623741
C     
C     Convert from calories per mole to Joules per mole
C     
      ZENER=ZENER*4.187
      GOTO 40
C     
 30   CONTINUE
      ZENER=0.0
C     
C     Calculation of the Zener ordering term complete.  Calculate the free
C     energy change accompanying the change from austenite to ferrite of the
C     same composition, including Zener ordering term. See Bhadeshia
C     and Edmonds [3].
C     
 40   CONTINUE
C     
      MAP_STEEL_FTO1 = X*R*T*DLOG(X*X)+X*(H1-(H)-(S1-(S)
     &  )*T+4*W1-6*W)-R*T*(1-X)*DLOG((1-X)**4)+5*R*T*(1-2*X)*DLOG(1-2*X)
     &  -R*T*X*DLOG(((D-1+3*X)/(D+1-3*X))**6)-R*T*(1-X)*DLOG(((1-2*AJ+(4
     &  *AJ-1)*X-D)/(2*AJ*(2*X-1)))**6)+3*R*T*X*DLOG(3-4*X)+R*T*
     &  X*DLOG(((D11-3+5*X)/(D11+3-5*X))**4)+(1-X)*F+ZENER
C
C
         RETURN
       END
C
C*****************************************************************
C     
      DOUBLE PRECISION FUNCTION MAP_STEEL_G91(XTO,T,W,W1,H1,S1,F,
     &     H,S,AJ,AJ1)
C     
C     To calculate the differential of FTO1 with respect to XTO.
C     See subroutine FTO1 for description of parameters.
C     The differentiation is used in order to apply the Newton Iteration.
C     
C     Unpublished work, H. K. D. H. Bhadeshia, University of Cambridge.
C     See laboratory book 5B
C     
C     
      IMPLICIT NONE
C     
      DOUBLE PRECISION AJ,AJ1,DT5,DT6,DZEN1,DZEN2,DZEN3,DZEN6,DZEN7
      DOUBLE PRECISION DZEN8,F,FD,FD1,G1,H,H1,R,S,S1,T
      DOUBLE PRECISION V1,V2,V3,V4,V5,V6,V7,V8,V9
      DOUBLE PRECISION W,W1,XTO
C     
C     R is the universal gas constant
C     
      R=8.31432D0
C     
      FD=DSQRT(1D0-2D0*(1D0+2D0*AJ)*XTO+(1D0+8D0*AJ)*XTO*XTO)
      FD1=DSQRT(9D0-6D0*XTO*(2D0*AJ1+3D0)+(9D0+16D0*AJ1)*XTO*XTO)
C     
      DT5=28080D0*XTO/(1D0-XTO)
      DT6=T/DT5
C     
      IF (DT6 .GT. 1.0) GOTO 120
      IF (DT6 .LT. 0.25) GOTO 100
C     
      DZEN1=0.2307D0 + 42.7974D0*DT6 - 233.8631D0*(DT6**2) + 
     &     645.4485D0*(DT6**3) - 954.3995D0*(DT6**4) +
     &     711.8095D0*(DT6**5) - 211.5136D0*(DT6**6)
      DZEN2= -2.6702D0 + 45.6337D0*DT6 - 225.3965D0*(DT6**2) + 
     &  567.7112D0*(DT6**3) - 771.6466D0*(DT6**4) + 538.1778D0*(DT6**5) 
     &     - 151.3818D0*(DT6**6)
C     
      GOTO 110
C     
 100  DZEN2=1D0
      DZEN1=3.295D0
 110  DZEN3=(((DZEN2*XTO)**2)*(-50898.56D0)/(1D0-XTO)) + DZEN1*T*XTO*
     &     (0.6623741D0)
C     
      DZEN3=DZEN3*4.187D0
      GOTO 130
 120  DZEN3=0.0D0
C     
 130  V1=FD-1D0+3D0*XTO
      V2=FD+1D0-3D0*XTO
      V3=1D0-2D0*AJ+(4D0*AJ-1D0)*XTO-FD
      V4=2D0*AJ*(2D0*XTO-1D0)
      V5=FD1-3D0+5D0*XTO
      V6=FD1+3D0-5D0*XTO
      V7=(XTO-1D0-2D0*AJ+8D0*XTO*AJ)/FD
      V8=(9D0*XTO-9D0-6D0*AJ1+16D0*AJ1*XTO)/FD1
      V9=H1-(H)-(S1-(S))*T-6D0*W+4D0*W1
      G1=2D0 + DLOG(XTO**2) + 4D0 + DLOG((1D0-XTO)**4) - 10D0 
     &     - DLOG((1D0-2D0*XTO)**10) - DLOG((V1/V2)**6)
     &     - 6D0*((XTO/V1)*(V7+3D0+V1*(3D0-V7)/V2))
     &     + DLOG((V3/V4)**6) - 6D0*((1D0-XTO)/V3)*(4D0*AJ*(1D0-(V3/V4))
     &     -V7) + 3D0*(DLOG(3D0-4D0*XTO)-4D0*XTO/(3D0-4D0*XTO))
     &     + DLOG((V5/V6)**4) + 4D0*(XTO/V5)*(V8+5D0+(V5/V6)*(5D0-V8))
C     
      IF (DT6 .GT. 1.0) GOTO 140
      DZEN6=(-3.3948D0+13.6112D0*DT6-13.4376D0*(DT6**2))*T/(28080D0*
     &     ((1D0-XTO)**2))
      DZEN7=(-3.3118D0+15.7462D0*DT6-23.2449D0*(DT6**2))*T/(28080D0*
     &     ((1D0-XTO)**2))
      DZEN8=50898D0*((DZEN2*XTO)**2)/((1D0-XTO)**2)+(-50898D0*(2D0*
     &     DZEN2*DZEN6*(XTO**2)+2D0*XTO*(DZEN2**2))/(1D0-XTO))+DZEN1
     &     *T*0.6623741D0+DZEN7*T*XTO*0.6623741D0
      GOTO 150
C     
 140  DZEN8=0.0D0
C     
 150  MAP_STEEL_G91=V9+R*T*G1+DZEN8-F
C     
         RETURN
         END
C     
C*****************************************************************
C     MATHEW JAMES PEET, 21 APRIL 2006
C     UNIVERSITY OF CAMBRIDGE
C     
      DOUBLE PRECISION FUNCTION MAT_BS(IMAX,T,G)
      IMPLICIT NONE
      INTEGER I,IMAX,IBS
      DOUBLE PRECISION T(1000),G(1000)
      DOUBLE PRECISION BCOND
C     
      BCOND=-400.0
C      
C      WRITE(*,*) "BCOND"
C      WRITE(*,*) BCOND
      DO 1 I=1,IMAX
         IF(G(I) .LT. BCOND) THEN
            MAT_BS = T(I)
            IBS = I
C            WRITE (*,*) MAT_BS,I
         ENDIF
C         
 1    CONTINUE
C      WRITE (*,*) MAT_BS
C      WRITE (*,*) "BCOND, T(IBS-1) DG(IBS-1)  "
C      WRITE (*,*) BCOND,T(IBS-1),G(IBS-1)
C      WRITE (*,*) "G(IBS) T(IBS))"
C      WRITE (*,*) G(IBS),T(IBS)
      MAT_BS = T(IBS-1)+(T(IBS)-T(IBS-1))*
     &  ((G(IBS-1)-BCOND)/(G(IBS-1)-G(IBS)))  
 2    RETURN
         END
C
C
C***************************************************************************      
C     MATHEW JAMES PEET, 21 APRIL 2006
C     UNIVERSITY OF CAMBRIDGE
C
      DOUBLE PRECISION FUNCTION MAT_MS(IMAX,T,G,X1)
      IMPLICIT NONE
      INTEGER I,IMAX,IMS
      DOUBLE PRECISION T(1000),G(1000)
      DOUBLE PRECISION MCOND
      DOUBLE PRECISION X1
      MCOND=-1120.0D+00-10568.0D+00*X1+94.1D+00
C     
C      WRITE(*,*) "MCOND, X1"
C      WRITE(*,*) MCOND,X1
C
      DO 1 I=1,IMAX
         IF(G(I) .LT. MCOND) THEN
            MAT_MS = T(I)            
            IMS = I
C           WRITE (*,*) MAT_MS,I
C            
         ENDIF
C         
 1    CONTINUE
C      WRITE (*,*) MAT_MS
C      MAT_MS = 0.5*(T(IMS)+T(IMS-1))
      MAT_MS = T(IMS-1)+(T(IMS)-T(IMS-1))*
     &  ((G(IMS-1)-MCOND)/(G(IMS-1)-G(IMS)))
      RETURN 
      END
C******************************************************************
CMP      SUBROUTINE MAP_UTIL_ANALY(J8,J9,CONST,SLOPE,CORR,X,Y)
C     
C      DOUBLE PRECISION X(1000),Y(1000),AX,AY,AX2,AY2,AXY
C      DOUBLE PRECISION CONST,SLOPE,CORR
C      INTEGER I1,J8,J9,I
C     
C      I1=J8
C      J8=J8-J9
C      AX=0.0D+00
C      AY=0.0D+00
C      AX2=0.0D+00
C      AY2=0.0D+00
C      AXY=0.0D+00
C      DO 1 I=1,J8
C         AX=AX+X(I)
C         AY=AY+Y(I)
C         AXY=AXY+X(I)*Y(I)
C         AX2=AX2+X(I)*X(I)
C         AY2=AY2+Y(I)*Y(I)
C 1    CONTINUE
C      CONST=(AY*AX2-AX*AXY)/(J8*AX2-AX*AX)
C      SLOPE=((J8*AXY-AX*AY)/(J8*AX2-AX*AX))
C      CORR=(J8*AXY-AX*AY)/(DSQRT((J8*AX2-AX*AX)*
C     &     (J8*AY2-AY*AY)))
C      J8=I1
C      RETURN
C      END
C     
C*****************************************************************
C     
C     DOUBLE PRECISION FUNCTION GIVING LFG  LN(ACTIVITY) OF CARBON IN AUSTENITE
C     
      DOUBLE PRECISION FUNCTION MAP_STEEL_CG(X,T,W,R)
C     
      IMPLICIT NONE
      DOUBLE PRECISION AJ,DG,EG,T,R,W,X
C     
      AJ=1.0-DEXP(-W/(R*T))
      IF(X .LE. 1.0D-10) THEN
         MAP_STEEL_CG=DLOG(1.0D-10)
         ELSE
            DG=DSQRT(1.0-2.0*(1.0+2.0*AJ)*X+(1.0+8.0*AJ)*X*X)
            EG=5.0*DLOG((1.0-2.0*X)/X)+6.0*W/(R*T)+
     &           (38575.0-13.48*T)/(R*T)
            MAP_STEEL_CG=EG+6.0D+00*DLOG((DG-1+3*X)/(DG+1-3*X))
            ENDIF
            RETURN
            END
C     
C*************************************************************************
C     
C     FUNCTION GIVING DIFFERENTIAL OF LN(ACTIVITY) OF CARBON IN AUSTENITE, LFG
C     DIFFERENTIAL IS WITH RESPECT TO X
C     
      DOUBLE PRECISION FUNCTION MAP_STEEL_DCG(X,T,W,R)
C     
      IMPLICIT NONE
      DOUBLE PRECISION AJ,DG,DDG,X,T,W,R
C     
      AJ=1.0-DEXP(-W/(R*T))
      DG=DSQRT(1.0-2.0*(1.0+2.0*AJ)*X+(1.0+8.0*AJ)*X*X)
      DDG=(0.5/DG)*(-2.0-4.0*AJ+2.0*X+16.0*AJ*X)
      MAP_STEEL_DCG=-((10.0/(1.0-2.0*X))+(5.0/X))+6.0*((DDG+3.0)
     &     /(DG-1.0+3.0*X)-(DDG-3.0)/(DG+1.0-3.0*X))
         RETURN
         END
C     
C*******************************************************************************
C     
C     Function giving the equilibrium mole fraction of carbon in ferrite
C     based on my paper on first order quasichemical theory, Metal Science
C     
      DOUBLE PRECISION FUNCTION MAP_STEEL_XALPH(T)
C     
      IMPLICIT NONE
      DOUBLE PRECISION T,CTEMP,T0
      PARAMETER(T0=273.0D0)
C     
      CTEMP = (T-T0)/900.0D+00
      MAP_STEEL_XALPH=0.1528D-02-0.8816D-02*CTEMP+0.2450D-01*CTEMP**2
     &     -0.2417D-01*CTEMP**3+
     &     0.6966D-02*CTEMP**4
C     
         RETURN
         END
C     
C**********************************************************************
C     
      SUBROUTINE MAP_STEEL_HETRO(C,D,M,R)
C     
      IMPLICIT NONE
      DOUBLE PRECISION C(7),D(8),M,A,PSI,PMN,PNI,PCR,PMO,PV,R
C     
C     PROGRAM TO CALCULATE COMPOSITIONS FOR SOLUTE DEPLETED REGIONS
C     M=MELTING POINT IN KELVIN, LIQUIDUS TEMPERATURE
C     CARBON PARTITIONING IGNORED
C     THERMODYNAMIC DATA FROM KIRKALDY AND BAGANIS
C     SOLIDIFICATION AS DELTA FERRITE
C     
      A=4.187D+00
      PSI=DEXP((3.9*M-8200.0)*A/(R*M))
      PMN=DEXP((3100.0-2.308*M)*A/(R*M))
      PNI=DEXP((-2120.0-0.38*M)*A/(R*M))
      PCR=DEXP((2.19*M-4600.0)*A/(R*M))
      PMO=DEXP((2.29*M-6600.0)*A/(R*M))
      PV=DEXP((-5100.0+2.3*M)*A/(R*M))
      D(1)=C(1)
      D(2)=C(2)*PSI
      D(3)=C(3)*PMN
      D(4)=C(4)*PNI
      D(5)=C(5)*PMO
      D(6)=C(6)*PCR
      D(7)=C(7)*PV
      D(8)=1.0D+02-D(1)-D(2)-D(3)-D(4)-D(5)-D(6)-D(7)
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE MAP_STEEL_GMAAX(A1,A,W1,F,R,T,X,AFE,H1,S1)
C
      IMPLICIT NONE             
      DOUBLE PRECISION A1,A,W1,F,R,T,X,AFE,H1,S1
      DOUBLE PRECISION AJ1,D1,B1,B2,B3,A1FE,TEST,DA1,DA2
      DOUBLE PRECISION ERROR,DA1FE
C
C     AVER=1.2
C     CALCULATION OF THE OPTIMUM NUCLEUS C CONTENT AND ACTIVITY OF C IN
C     FERRITE NUCLEUS
C     
C     Estimate a value for X, the optimum carbon concentration of
C     the ferrite nucleus, in mole fraction
C     February 1991
      X=6.3998D-07*T-3.027D-04
      IF(X .LE. 0.0D+00)  X=1.0D-13
      IF(X .GT. 0.32D-03) X=0.32D-03
C     Estimation complete
C     
      AJ1=1.0D+00-DEXP(-W1/(R*T))
 1    D1=DSQRT(9.0D+00-6.0D+00*X*(2.0D+00*AJ1+3.0D+00)
     &     +(9.0D+00+16.0D+00*AJ1)*X*X)
      B1=((D1-3.0D+00+5.0D+00*X)/(D1+3.0D+00-5.0D+00*X))
      B2=((3.0D+00-4.0D+00*X)/X)
      B3=(H1 - S1*T + 4.0D+00*W1)/(R*T)
      
      A1=DLOG(B1*B1*B1*B1*B2*B2*B2)+B3
C     
      IF(X .GT. 1.0D-08)THEN
         A1FE=DLOG(1.0D+00-X)
      ELSE
         A1FE=X
      ENDIF
C     
      TEST=F+R*T*(A1FE-AFE) - R*T*(A1-A)
C     
C     Newton iteration
C     
      IF (DABS(TEST) .GT. 10.0) THEN
         DA1=(3.0D+00*X/(3.0D+00-4.0D+00*X))*(
     &        (4.0D+00*X-3.0D+00)/(X*X)-4.0D+00/X)
         DA2=(0.5D+00/D1)*(-12.0D+00*AJ1-18.0D+00+
     &        18.0D+00*X+32.0D+00*AJ1*X)
         DA2=4.0D+00*(((DA2+5.0D+00)/(D1-3.0D+00+5.0D+00*X))
     &        -((DA2-5.0D+00)/(D1+3.0D+00-5.0D+00*X)))
         DA1=DA1+DA2
         DA1FE=1.0D+00/(X-1.0D+00)
         ERROR=TEST/(R*T*(DA1FE-DA1))
         IF (ERROR .GT. X) ERROR = 0.3D+00*X
         X=DABS(X-ERROR)
         GOTO 1
      ENDIF
C     End of iteration
      RETURN
      END
C     
C*****************************************************************
C     
CMP      DOUBLE PRECISION FUNCTION MAP_STEEL_ENERGY2(T,T10,T20)
C     
C     Subroutine for calculating the free energy change for the transformation
C     of austenite to ferrite of the same chemical composition. 
C     Output is in J/mole
C     The routine has been modified by Roger
C     1. To take care of discontinuity
C     2. Better presentation
C     
CMP      IMPLICIT NONE
CMP      DOUBLE PRECISION T,T10,T20,F,T7
CMP      T7=T-100D0*T20
CMP      CALL MAP_STEEL_GALGA(F,T7)
CMP      MAP_STEEL_ENERGY2=141D0*T10 + F
CMP         RETURN
CMP         END
C     
C     *************************************************************************
C     
CMP      SUBROUTINE MAP_STEEL_GALGA (G,T)
C     
C     SUBROUTINE FOR DELTA G_O FE  
C     Subroutine to calculate pure Fe's difference in Gibbs Free Energy for
C     the reaction ferrite -> austenite, Delta G_o Fe{ferrite to austenite}.
C     Data due to Kaufman, Clougherty and Weiss, Acta Met. 11 1963 p323.
C     Their data used here, with corrections. Range 0-1810K.
C     This set of polynomials: R.C.Reed, 30th April 1989,
C     written up in RCR10.AABS1.GALGA, with graphs.
C     NB Input T in degrees Kelvin, output GC is in J/mol.
C     NNB This routine will return a value above the melting point of pure iron.
C     
CMP      IMPLICIT NONE
CMP      DOUBLE PRECISION G,T
      
CMP      IF (T.LT.3.56D+02) THEN
CMP         G =  0.13132885D+04 + 1.3141025D-02*T - 2.580128186D-03*T*T
CMP      ENDIF
      
CMP      IF (T.GT.3.56D+02 .AND. T.LT.4.28D+02) THEN
CMP         G =  1.1224052D+03 + 3.542835682D0*T - 2.2496774D-02*T*T
CMP     &        + 4.326701963D-05*T*T*T - 3.072096339D-08*T*T*T*T
CMP      ENDIF
      
CMP      IF (T.GE.4.28D+02 .AND. T.LT.6.34D+02) THEN
CMP         G = - 1.7031057D+04 + 0.1448353906D+03*T - 0.428166357D0*T*T
CMP     &        + 5.50871582D-04*T*T*T - 2.633397102D-07*T*T*T*T
CMP      ENDIF
      
CMP      IF (T.GE.6.34D+02 .AND. T.LT.7.40D+02) THEN
CMP         G = 0.13022482D+04 + 0.522110231D0*T - 5.980360202D-03*T*T
CMP     &        + 6.659625695D-06*T*T*T - 2.473169175D-09*T*T*T*T
CMP      ENDIF
      
CMP      IF (T.GE.7.40D+02 .AND. T.LT.8.60D+02) THEN
CMP         G = - 0.64489093D5 + 3.095104796D2*T - 0.547164618D0*T*T
CMP     &        + 4.255071973D-04*T*T*T - 1.233061536D-07*T*T*T*T
CMP      ENDIF
      
CMP      IF (T.GE.8.60D+02 .AND. T.LT.9.27D+02) THEN
CMP         G = + 0.13015055D+07 - 5.763825D+03*T + 9.571875D0*T*T
CMP     &        - 7.0625D-03*T*T*T + 1.953125D-06*T*T*T*T
CMP      ENDIF
      
CMP      IF (T.GE.9.27D+02 .AND. T.LT.1.08D+03) THEN
CMP         G = + 0.58257237D+05 - 2.229275965D2*T + 0.322821329D0*T*T
CMP     &        - 2.089186484D-04*T*T*T + 5.084325376D-08*T*T*T*T
CMP      ENDIF
      
CMP      IF (T.GE.1.08D+03 .AND. T.LT.1.24D+03) THEN
CMP         G = 0.14043448D+06 - 4.704834516D2*T + 0.591517384D0*T*T
CMP     &        - 3.306547594D-04*T*T*T + 6.93139101D-08*T*T*T*T
CMP      ENDIF
      
CMP      IF (T.GE.1.24D+03 .AND. T.LT.1.502D+03) THEN
CMP         G = 0.15744376D+05 - 4.475440789D+01*T + 4.7873355D-02*T*T
CMP     &        - 2.286184211D-05*T*T*T + 4.111842105D-09*T*T*T*T
CMP      ENDIF
      
CMP      IF (T.GE.1.502D+03 .AND. T.LT.1.616D+03) THEN
CMPCMP         G = - 0.58637400D+04 + 1.147433313D+01*T
CMP     &        - 7.5500D-03*T*T + 1.6666666D-06*T*T*T
CMP      ENDIF
      
CMP      IF (T.GE.1.616D+03 .AND. T.LT.1.68D+03) THEN
CMP         G = 0.32972410D+06 - 8.211649405D2*T + 0.766426516D0*T*T
CMP     &        - 3.177724674D-04*T*T*T + 4.938936812D-08*T*T*T*T
CMP      ENDIF
      
CMP      IF (T.GE.1.68D+03) THEN
CMP         G = 0.33271186D+07 - 7.657409945D+03*T
CMP     &        + 6.607976863D0*T*T - 2.534098538D-03*T*T*T
CMP     &        + 3.643939352D-07*T*T*T*T
CMP      ENDIF
      
C     Convert units from KCW's cal/mol to J/mol:
CMP      G = -G * 8.314D+00 / 1.9858D+00
CMP      RETURN
CMP      END
C     
C*****************************************************************
C     
      SUBROUTINE MAP_STEEL_OMEGA2(C,W,XBAR,T10,T20,ID)
C     
C-----------------------------------------------------------------------------
C     SUBROUTINE TO CALCULATE THE CARBON CARBON INTERACTION ENERGY IN
C     AUSTENITE, AS A FUNCTION OF ALLOY COMPOSITION.  BASED ON .MUCG18
C     THE ANSWER IS IN JOULES PER MOL.   **7 OCTOBER 1981**
C     Feed in the alloy concentration in the following order
C     C    SI   MN   NI   MO   CR   V    CO   CU   AL    W    FE
C     In this Program first the Weight % is converted into Mol fraction
C     Correction to P(11) by Mihai Tarcolea, June 2002
C----------------------------------------------------------------------------
C     
C     1.   Y() contains only the at% of substitutional considering alone
C     T10 and T20 is calculated for the alloy concentration based on Y()
C     2.   Then C-C interaction (W) is calculated based on the ploynomials derived
C     Trial and Error for the interaction energies for Al and Cu
C     MODIFIED FOR OTHER ALLOYING ELEMENTS BY SURESH BABU - C - C APPROX
C     THE CARBON - CARBON INTERACTION MISSING FOR CU AND AL
C----------------------------------------------------------------------------
C     
      IMPLICIT NONE
C     
      DOUBLE PRECISION B1,B2,B3,T10,T20,W,XBAR
      DOUBLE PRECISION C(12), D(12), P(12), Y(12)
C     
      INTEGER I,ID,IU,NCO
C     
      B3=0.0D+00
      IF (ID .GE. 2) GOTO 110
      NCO = 12
      IF (NCO .GT. 12) THEN
         WRITE (6,*) '*** Error in MAP_STEEL_OMEGA2 ***'
         WRITE (6,*) ' Number of elements is greater than 12.'
         STOP
      ENDIF
      D(12)=C(1)+C(2)+C(3)+C(4)+C(5)+C(6)+C(7)+C(8)+C(9)+C(10)+C(11)
      D(12)=100.0D+00-D(12)
      D(12) =D(12)/55.84D+00
      D(1)  =C(1)/12.0115D+00
      D(2)  =C(2)/28.09D+00
      D(3)  =C(3)/54.94D+00
      D(4)  =C(4)/58.71D+00
      D(5)  =C(5)/95.94D+00
      D(6)  =C(6)/52.0D+00
      D(7)  =C(7)/50.94D+00
      D(8)  =C(8)/58.94D+00
      D(9)  =C(9)/63.54D+00
      D(10) =C(10)/26.98D+00
      D(11) =C(11)/183.85D+00
      B1=D(1)+D(2)+D(3)+D(4)+D(5)+D(6)+D(7)+D(8)+D(9)+D(10)+D(11)+D(12)
C------------------------
      DO 100 IU=1,12
         D(IU)=D(IU)/B1
 100  CONTINUE
C------------------------
 110  DO 120 IU=2,11
         Y(IU)=D(IU)/D(12)
 120  CONTINUE
      XBAR=D(1)
      XBAR=DINT(10000.0D+00*XBAR)
      XBAR=XBAR/10000
C------------------------------------------------------------------------------
C     Delta T_nm and Delta T_mag were taken for Co from
C     Aaronson, Pound and Domain work - Trans.Met.Soc. AIME,
C     Vol.236 May, 1966, p. 769   (S. Babu, 18.7.1989)
C     C    SI   MN   NI   MO   CR   V    CO   CU   AL    W      FE
C     1    2    3    4    5    6    7    8    9    10    11     12
C------------------------------------------------------------------------------
      T10=Y(2)*(-3)+Y(3)*2+Y(4)*12+Y(5)*(-9)+Y(6)*(-1)+Y(7)*(-12)+
     &     Y(8)* 3.5 +Y(9)*7+Y(10)*(-7)+Y(11)*(-9)
C     
C     T10 coefficients are actually Delt_mag - DelT_non.mag
C     T20 coefficients are actually Delt_mag
      T20=-  3*Y(2)-37.5*Y(3)-6*Y(4) -26*Y(5)-19*Y(6)-44*Y(7)+
     &     19.5*Y(8)- 4.5*Y(9)+8*Y(10)-26*Y(11)
C------------------------------------------------------------------------------
      P(2)=2013.0341D0+763.8167D0*D(2)+45802.87D0*D(2)**2-280061.63D0*
     &  D(2)**3+3.864D+06*D(2)**4-2.4233D+07*D(2)**5+6.9547D+07*D(2)**6
      P(3)=2012.067D0-1764.095D0*D(3)+6287.52D0*D(3)**2-21647.96D0*
     &  D(3)**3-2.0119D+06*D(3)**4+3.1716D+07*D(3)**5-1.3885D+08*D(3)**6
      P(4)=2006.8017D0+2330.2424D0*D(4)-54915.32D0*D(4)**2+1.6216D+06*
     &  D(4)**3-2.4968D+07*D(4)**4+1.8838D+08*D(4)**5-5.5531D+08*D(4)**6
      P(5)=2006.834D0-2997.314D0*D(5)-37906.61D0*D(5)**2+1.0328D+06*
     &  D(5)**3-1.3306D+07*D(5)**4+8.411D+07*D(5)**5-2.0826D+08*D(5)**6
      P(6)=2012.367D0-9224.2655D0*D(6)+33657.8D0*D(6)**2-566827.83D0*
     &  D(6)**3+8.5676D+06*D(6)**4-6.7482D+07*D(6)**5+2.0837D+08*D(6)**6
      P(7)=2011.9996D0-6247.9118D0*D(7)+5411.7566D0*D(7)**2
     &     +250118.1085D0*D(7)**3-4.1676D+06*D(7)**4
C------------------------------------------------------------------------------
C     CO  CU   AL    W      FE
C     8   9    10    11     12
C     EFFECT TAKEN FROM HKDB - MET.SCI - 1981
C     RESULT IN J/MOLE    (S. Babu, 18.7.1989)
C     W SAME AS MOLYBDENUM  IN INTERACION ENERGY
C------------------------------------------------------------------------------
      P(8)=(8427.00D0+5986D0*D(8))/4.187D0
      P(9)=  2011.0D0
      P(10)= 2011.0D0
      P(11)= 2006.834D0-2997.314D0*D(11)-37906.61D0*D(11)**2+1.0328D+06*
     &     D(11)**3-1.3306D+07*D(11)**4+8.411D+07*D(11)**5
     &     -2.0826D+08*D(11)**6
      B2=0.0D+00
      DO 130 IU=2,11
         B3=B3+P(IU)*Y(IU)
         B2=B2+Y(IU)
 130  CONTINUE
      IF (DABS(B2) .LT. 1D-40) THEN
         W=8054.0D0
      ELSE
         W=(B3/B2)*4.187
      ENDIF
      IF (DABS(B3) .LT. 1D-40)THEN
         W = 8054.0D0
      ENDIF
      DO 140 I = 1, 12
         C(I) = D(I)
 140  CONTINUE
      RETURN
      END
C     
C----------------------------------------------------------------------------
C     
      SUBROUTINE SHEAR(STIME,DEFMM,TEMP)
C     
      IMPLICIT NONE
      DOUBLE PRECISION DEFM,DEFMM,STIME,TEMP
C     
      DEFM = DABS(DEFMM)
      STIME=DEXP(
     &     (0.2432D+06/(8.31432D0*TEMP))
     &     -0.135D+03 + 20.0D0*DLOG(TEMP)
     &     -5D0*DLOG(DEFM)
     &     )
      RETURN
      END
C     
C----------------------------------------------------------------------------
C     
      SUBROUTINE  DIFFU(DTIME,DEFMM,TEMP)
C     
      IMPLICIT NONE
      DOUBLE PRECISION DEFMM,DEFM,DTIME,TEMP
C     
      DEFM = DABS(DEFMM)
      DTIME=DEXP(
     &     (0.6031D+06/(8.31432D0*TEMP))
     &     -0.1905D+03 + 20.0D0*DLOG(TEMP)
     &     -4D0*DLOG(DEFM)
     &     )
      RETURN
      END
C     
C*********************************************************************
C     
      SUBROUTINE LOGO
      WRITE(*,432)
      WRITE(*,1)
      WRITE(*,2)
      WRITE(*,2)
      WRITE(*,6)
      WRITE(*,2)
      WRITE(*,2)
 300  WRITE(*,2)
      WRITE(*,8)
      WRITE(*,2)
      WRITE(*,9)
      WRITE(*,10)
      WRITE(*,2)
      WRITE(*,2)
      WRITE(*,1)
      WRITE(*,433)
 432  FORMAT(//////////)
 1    FORMAT(
     &     10X,' *************************************************',
     &     '**********')
 2    FORMAT(10X,' **',55X,'**')
 6    FORMAT(10X,' **',14X,'  TTT DIAGRAMS FOR STEELS ',15X,'**')
 8    FORMAT(10X,' **',8X,'                  by                  ',
     &     9X,'**')
 9    FORMAT(10X,' **',8X,'         H. K. D. H. Bhadeshia        ',
     &     9X,'**')
 10   FORMAT(10X,' **',8X,'        University of Cambridge       ',
     &     9X,'**')
 433  FORMAT(////)
C     PAUSE
      RETURN
      END
C     
C-----------------------------------------------------------------------
C     
      SUBROUTINE BOUND(A,B,C)
      
      IMPLICIT NONE
      DOUBLE PRECISION A,B,C
      INTEGER I
      
      I=0
 100  I=I+1
      WRITE(*,1) B,C
      RETURN
C      WRITE(*,2) 
C      CALL MAP_UTIL_REED(A)


      IF(A .GE. B .AND. A .LE. C) THEN
         RETURN
      ELSE
         IF (I.LT.10) GOTO 100 
         WRITE (*,3)
         STOP
      ENDIF
      RETURN
 1    FORMAT(12X,' Value out of bounds'/
     &     14X,' The limits are ',F8.3,' to ', F8.3/)
 2    FORMAT(1H ,'Input new value: ')
 3    FORMAT( 1H ,' Too many attempts. Program terminated.')
      END
C     
C-----------------------------------------------------------------------
C     
C     To read a real number in a way which to some extent traps typing errors
C     Modification added 12-2-1999 to stop the program going into an endless loop.
C     
      SUBROUTINE MAP_UTIL_REED(A)
      IMPLICIT NONE
      DOUBLE PRECISION A
      INTEGER J
C     
      J=0
 100  J=J+1
      READ(*,*,ERR=110) A
      GOTO 120   
 110  IF (J.LT.10) THEN
         WRITE(*,1)
         GOTO 100
      ELSE
         WRITE(*,2)
         STOP
      ENDIF
C     
 120  RETURN
 1    FORMAT(19X,' Incorrect Input. Try again'/)
 2    FORMAT(1H ,19X,'Too many attempts.  Program terminated.')
      END
C     
C-----------------------------------------------------------------------
C     
C     To read integers in a way which to some extent traps typing errors
C     
      SUBROUTINE MAP_UTIL_REEDI(I)
      IMPLICIT NONE
      INTEGER I,J
C     
      J=0
 100  J=J+1
      READ(*,*,ERR=110) I
      GOTO 120
 110  IF (J.LT.10) THEN
         WRITE(*,1)
         GOTO 100
      ELSE
         WRITE(*,2)
         STOP
      ENDIF
C     
 120  RETURN
 1    FORMAT(19X,' Incorrect Input. Try again'/)
 2    FORMAT(1H ,19X,'Too many attempts.  Program terminated.')
      END



C*********************************************************************
C
      DOUBLE PRECISION FUNCTION MAP_STEEL_ENERGY_REVERT(T,T10,T20)
      IMPLICIT NONE
      DOUBLE PRECISION T,T10,T20,F,T7,T8
      T7=T-100.0D+0*T20
      T8=T7-1140.0D+0
      IF (T7 .LT. 300) F=1.38*T7-1499.0
      IF (T7.LT.700.AND.T7.GE.300)  F=1.65786*T7-1581.0
      IF (T7.LT.940.AND.T7.GE.700)  F=1.30089*T7-1331.0
      IF (T7.LT.940) GOTO 10
      F=T8*(-3.58434D-9*T8 + 2.70013D-6) - 1.04923D-3
      F=T8*(F*T8 + 0.26557D+0) - 8.88909D+0
 10   MAP_STEEL_ENERGY_REVERT=(141.0*T10 + F)*4.187
      RETURN
      END
C
C 
C*********************************************************************
