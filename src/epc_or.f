      real*8 function epc_func(EBEAM,Z1,N1,FERMI1,MPI,PART,SCAL1,P,THP,
     + rad_l,USEFIT1)
c
c
c  ebeam = incident beam energy ( MeV)
c  Z1 = target Z
c  N1 = target N
c  FERMI1 = 'N' choose subroutines KINE_OR or PART_OR 
c          = "Y" choose subroutine KINEDEL or PARTDEL ( for Delta)
c  MPI = "Y" use multipion formulas 
c  PART = "P" sets AN=N/3.d0+2.d0*Z/3.d0 MP = proton mass  IP=1   IP is flag for hadron or pion production
c         "N" sets AN=Z/3.d0+2.d0*N/3.d0 MP = neutron mass  IP=-1
c         "PI+" sets AN=Z/3.d0 MP = AMP pi+ mass  IP=2
c         "PI-" sets AN=N/3.d0 MP = AMP pi+ mass IP=2
c         "PI0" sets AN=2.d0*(N+Z)/3.d0 MP = MPI0 IP=0
c   SCAL1= "Y" photo production
c          "N" electro production
c   P = Particle Momentum (MeV/c)
c   THP = Particle outgoing angle (deg)
c   rad_l = radiation lenght of target (%)
c   USEFIT1 = "Y" then use Oscar's fit in S2PI subroutine instead of Wiser
c
c

CGAW      PROGRAM EPC

C  VAX VERSION
C  ELECTROPRODUCTION YIELDS OF NUCLEONS AND PIONS 
C  WRITTEN BY J.S. OCONNELL AND J.W. LIGHTBODY, JR. 
C  NATIONAL BUREAU OF STANDARDS 
C  APRIL 1988 
C  TRANSVERSE SCALING REGION ADDED

C 	Modified by OARA to plot like older EPC's
C       Modified slightly by Glen Warren to compile under Linux (g77) Sept. 02
C     Modified by Jixie Zhang:  
C     IN S2PI(), add angle correction to wise result
C     add flag USEFIT to tell whether to use scaling fit result or not
C     Change the output 'epc_xn' unit from nb/(MeV/c)/sr to nb/(GeV/c)/sr such that it 
C     is consistant with electron production xs
     

      IMPLICIT REAL*8 (A-H,O-Z) 
      REAL*8 MP,MPI0,MN
      CHARACTER*1 MPI,SCAL,FERMI,FERMI1,SCAL1,USEFIT1,USEFIT
      CHARACTER*3 PART

      COMMON/SG/IA
      COMMON/QD/QDF 
      COMMON/DEL/IP 
      COMMON /FER/ FERMI,SCAL
      COMMON/M/Z,N
      COMMON/KF/E1
      DATA AM/938.28/,AMP/139.6/,MPI0/135.9/,MN/939.56/

c      write(*,*)EBEAM,Z1,N1,FERMI1,MPI,PART,P,THP

c      SCAL  = 'Y'
      SCAL=SCAL1
      FERMI = FERMI1
      Z     = Z1
      N     = N1
      E1    = EBEAM
c      USEFIT = 'Y'
      USEFIT = USEFIT1

      PI=ACOS(-1.0D0)
      IA=Z+N
      
C  'AN' is effective number of nucleons for one pion production 

      IF(PART.EQ.'P')THEN 
         AN=N/3.d0+2.d0*Z/3.d0
	 MP = AM
         IP=1 
      ELSEIF(PART.EQ.'N')THEN 
         AN=Z/3.d0+2.d0*N/3.d0
	 MP = MN
         IP=-1
      ELSEIF(PART.EQ.'PI+')THEN 
         AN=Z/3.d0
	 MP = AMP
         IP=2 
      ELSEIF(PART.EQ.'PI-')THEN 
         AN=N/3.d0
	 MP = AMP
         IP=2 
      ELSEIF(PART.EQ.'PI0')THEN 
         AN=2.d0*(N+Z)/3.d0 
	 MP = MPI0
         IP=0 
c         write(*,*)PART,' ',AM,MP
      ELSE
	CALL EXIT
      ENDIF 

      IF(ABS(IP).EQ.1)THEN
        IF(IA.EQ.1)THEN 
	  DLF = 0
	ELSE
  	  IF(IA.GT.1.AND.IA.LT.5)THEN
            DLF=IA
          ELSE 
            DLF=7.
          ENDIF

          AL=DLF        ! use default setting for Levinger factor
          QDF=AL*N*Z/FLOAT(IA) 
	END IF
      ENDIF 

      TH=THP*PI/180.
      IF(ABS(IP).EQ.1)THEN
        E=SQRT(P**2+AM**2) 
        TP=P**2/(E+AM) 
        AJ=P/E		! Converts cross section from 1/MeV to 1/MeV/c
        IF(IA.EQ.1)THEN
          D2QD=0. 
          D2QF=0. 
        ELSEIF(IA.GT.1)THEN
          CALL DEP_OR(E1,TP,TH,IP,D2QD)
	  IF(SCAL.NE.'Y') THEN
            D2QD=D2QD*AJ
            CALL EP_OR(E1,TP,TH,D2QF)
            D2QF=D2QF*AJ
	  END IF
        ENDIF
      ELSEIF(ABS(IP).EQ.2.OR.IP.EQ.0)THEN 
        E=SQRT(P**2+AMP**2)
        TP=P**2/(E+AMP)
        AJ=P/E 
        D2QD=0.
        D2QF=0.
      ELSE
	CALL EXIT
      ENDIF 
      if(P.lt.500)then
      CALL DELTA_OR(E1,TP,TH,D2DEL) 
c      write(*,*)'Called Delta_Or with ',E1,TP,TH,D2DEL
      IF(SCAL.EQ.'Y') THEN
        D2DEL=AN*D2DEL
      ELSE
        D2DEL=AN*D2DEL*AJ
      END IF
      endif
c comment added by Jixie, discussed with Oscar Rondon
c D2SC1 is the cross section for production at the single pion threshold,
c and D2SC2 is for production at the two pion threshold
c By charge conservation pi+ are produced on protons at the single pion
c threshold, but at the two pion threshold on neutrons, and vice versa for
c pi-. But pi0's are always produced at the single pion threshold.
c therefore for PI0, D2SC=IA*D2SC1, not IA*D2SC1+D2SC2)/2

      D2SC = 0.d0
      IF(MPI.EQ.'Y') THEN
        IF(ABS(IP).EQ.2.OR.IP.EQ.0)THEN
          CALL S2PI(2,E1,TP,TH,D2SC1,USEFIT) 
          CALL S2PI(-2,E1,TP,TH,D2SC2,USEFIT) 
          IF(PART.EQ.'PI+')THEN 
            D2SC=Z*D2SC1+N*D2SC2 
          ELSEIF(PART.EQ.'PI-')THEN 
            D2SC=N*D2SC1+Z*D2SC2 
          ELSEIF(PART.EQ.'PI0')THEN 
c             write(*,*) ' d2sc = ',ia,D2SC1 
            D2SC=IA*D2SC1
c            write(*,*)'PI=-',D2SC1*rad_l/100.*P,D2SC2*rad_l/100.*P
          ELSE
            CALL EXIT
          ENDIF 
        ELSEIF(ABS(IP).EQ.1)THEN 
          CALL S2PI(1,E1,TP,TH,D2SC1,USEFIT) 
          D2SC=IA*D2SC1 
       ENDIF
      END IF

      TOTAL=D2QD+D2QF+D2DEL+D2SC
      IF(SCAL.EQ.'Y') THEN
        TOTALE=TOTAL*P*1.0D-6
      ELSE
        TOTALE=TOTAL/AJ 
      END IF
c      write(*,*) ' d2= ',D2QD,D2QF,D2DEL,D2SC,aj
c      write(*,*) D2QD+D2QF+D2DEL+D2SC,AJ

      if(SCAL.eq.'Y')then
         epc_func = rad_l/100.*D2SC*(P**2/E*1e-3) ! bremstrahlung photo-production xn  ubarns / GeV/c / sr 
      else
         epc_func = D2SC   !electro-production  microbarns / GeV/c / sr
      endif

c      write(*,*) ' pion wiser = ',p,p*p/E,pionwiser
c      write(18,*) E,TH,D2SC*1.d6,D2DEL*1.d6
      return
      END 

C------------------------------------------------------------------------------

*VTP_OR
      SUBROUTINE VTP_OR(AMT,AM1,EI,W0,TP,TH,GN)
C  TIATOR-WRIGHT VIRTUAL PHOTON SPECTRUM
C  PHYS. REV. C26,2349(1982) AND NUC. PHYS. A379,407(1982)
      IMPLICIT REAL*8 (A-H,O-Z) 
      DATA AME/.511/
	PI=ACOS(-1.0D0)
      EF0=EI-W0 
      AKI=SQRT(EI**2-AME**2)
      AKF0=SQRT(EF0**2-AME**2)
      AKP=SQRT(TP**2+2.*AM1*TP) 
      EP=TP+AM1 
      AR=EI+AMT-EP
      BR=EF0*(AKP*COS(TH)-AKI)/AKF0 
C     BRP=(AKF0/EF0)**2*BR
      A=AME**2-EI*EF0 
      B=AKI*AKF0
      D=-AME**2*BR*(EI/EF0-1.)/AR 
      AP=A-D
      BP=B+D
      AN1=1./137./2./PI*W0**2/AKI**2
!      APB=-AME**2*(AKI-AKF0)**2/(AME**2+EI*EF0+AKI*AKF0)
!	PRINT *,'APB',APB
      APB = A + B 	! New
!	PRINT *,'A+B',APB
      AN1=AN1*B/BP*(AR+BR)/(AR-AP/BP*BR)
      AN2=1.-2.*A/W0**2 
      AN4=((AP-BP)*(AR+BR)/APB/(AR-BR)) 
      IF(AN4.LE.0.)GO TO 1
      AN2=AN2*LOG(AN4)
      AN3=-4.*B/W0**2 
      ANE=AN1*(AN2+AN3) 
      D0=AMT+EI-EP+EF0/AKF0*(AKP*COS(TH)-AKI) 
      R=(AMT+W0-EP/AKP*W0*COS(TH))/D0 
      GN=ANE*R/W0 
      IF(GN.LT.0.)GN=0. 
!	PRINT *,'TP,W0,R,ANE,GN',TP,W0,R,ANE,GN
      RETURN
    1 GN=0. 
      RETURN
      END 
*DEP
      SUBROUTINE DEP_OR(E1,TP,TH,IP,D2QD)
C  QUASI-DEUTERON CROSS SECTION 
      IMPLICIT REAL*8 (A-H,O-Z) 
      CHARACTER*1 SCAL,FERMI
      COMMON/SG/IA
      COMMON/QD/QDF 
 	COMMON /FER/ FERMI,SCAL
      DATA AM/939./,AMD/1876./
      IF(IA.EQ.1)GOTO 1 
      PN=SQRT(TP**2+2.*AM*TP) 
	EP = TP + AM
      CALL KINE_OR(AMD,AM,AM,PN,TH,W0,THC) 
      IF(W0.GE.E1)GO TO 1 
      IF(W0.LE.0.)GO TO 1 
      W0G=W0/1000.
      CALL SIGD_OR(W0G,THC,IP,DSQD)
      CALL PART_OR(AMD,AM,AM,PN,TH,AJT,AJW)
	IF(SCAL.NE.'Y') THEN
      CALL VTP_OR(AMD,AM,E1,W0,TP,TH,PHI)
C  CROSS SECTION IN UB/MEV-SR 
C	OARA: AJW gives (w+-w-)/(p+-p-) so it is dw/dp, not dw/dt!
C	Cross section is then in ub/((MeV/c).sr) directly
!	PRINT *,'QDF,PHI,DSQD,AJW,AJT',QDF,PHI,DSQD,AJW,AJT
      D2QD=QDF*PHI*DSQD*AJW*AJT*EP/PN
	ELSE
      BREM=1./W0
C  CROSS SECTION IN UB/MEV-SR-Q
C	Cross section was then in ub/(Q.(MeV/c).sr) directly
	D2QD = QDF*BREM*DSQD*AJW*AJT*EP/PN	! From GPC
!	PRINT *,'QDF,BREM,DSQD,AJW,AJT',QDF,BREM,DSQD,AJW,AJT
!	D2QD = QDF*BREM*DSQD*AJW*AJT	One step conversion: next line
!	D2QD = D2QD*EP*1.0D6/PN**2 ! Convert to ub.c/(Q.(GeV/c)^2.sr)
	D2QD = D2QD*1.0D6/PN	! Convert to units ub.c/(Q.(GeV/c)^2.sr)
	END IF
      RETURN
    1 D2QD=0. 
      RETURN
      END 
*SIGD_OR 
      SUBROUTINE SIGD_OR(E,TH,IP,DSQD) 
C  DEUTERON CROSS SECTION 
C  BASED ON FIT OF THORLACIUS & FEARING 
C  PHYS. REV. C33,1830(1986)
C  PHOTON ENERGY RANGE 10 - 625 MEV 
C  E[GEV] IN LAB SYSTEM 
C  TH[RAD] & DSQD[UB/SR] IN CENTER-OF-MOMENTUM SYSTEM 
      IMPLICIT REAL*8 (A-H,O-Z) 
      DIMENSION C0(8),C1(4),C2(4),C3(4),C4(4) 
      DIMENSION A(0:4),B(4,4)
      DATA C0/2.61E2,-1.10E2,2.46E1,-1.71E1,5.76E0, 
     #    -2.05E0,2.67E-1,1.13E2/ 
      DATA C1/1.68E1,-4.66E1,2.56E0,-4.72E0/
      DATA C2/-2.03E2,-8.12E1,-4.05E0,-5.99E0/
      DATA C3/-1.77E1,-3.74E1,-5.07E-1,-5.40E0/ 
      DATA C4/-2.05E0,-7.05E0,9.40E-1,-2.05E0/
      X=COS(TH) 
      IF(E.LE.625.)THEN 
C  TEST FOR NEUTRON 
         X=IP*X 
C  COEFICIENTS
         A(0)=C0(1)*EXP(C0(2)*E)+C0(3)*EXP(C0(4)*E) 
         A(0)=A(0)+(C0(5)+C0(6)*E)/(1.+C0(8)*(E-C0(7))**2)
         DSQD=A(0)*PL(0,X)
         DO 2 L=1,4 
         B(1,L)=C1(L) 
         B(2,L)=C2(L) 
         B(3,L)=C3(L) 
    2    B(4,L)=C4(L) 
         DO 1 L=1,4 
         A(L)=B(L,1)*EXP(B(L,2)*E)+ B(L,3)*EXP(B(L,4)*E)
    1    DSQD=DSQD+A(L)*PL(L,X) 
      ELSEIF(E.LT..700)THEN 
         DSQD=.3
      ELSEIF(E.LT..800)THEN 
         DSQD=.15 
      ELSEIF(E.LT..900)THEN 
         DSQD=.1
      ELSE
         DSQD=55./(E-.350)
      ENDIF 
      RETURN
      END 
*LEG
      REAL*8 FUNCTION PL(L,X) 
C  LEGENDRE POLYNOMIALS 
      IMPLICIT REAL*8 (A-H,O-Z) 
      IF(L.EQ.0)THEN
         PL=1.
      ELSEIF(L.EQ.1)THEN
         PL=X 
      ELSEIF(L.EQ.2)THEN
         PL=.5*(3.*X**2-1.) 
      ELSEIF(L.EQ.3)THEN
         PL=.5*(5.*X**3-3.*X) 
      ELSEIF(L.EQ.4)THEN
         PL=1./8.*(35.*X**4-30.*X**2+3.)
      ELSE
         PL=0.
      ENDIF 
      RETURN
      END 
*DELTA_OR
      SUBROUTINE DELTA_OR(E1,TP,TH,D2DEL)
C  PHOTOPRODUCTION OF NUCLEONS AND PIONS VIA DELTA
      IMPLICIT REAL*8 (A-H,O-Z) 
      COMMON/DEL/IP
      CHARACTER*1 SCAL,FERMI
 	COMMON /FER/ FERMI,SCAL
      DATA AM/939./,AMP/139./ 
      IF(ABS(IP).EQ.1)THEN
         AM1=AM 
         AM2=AMP
      ELSE
         AM1=AMP
         AM2=AM 
      ENDIF 
      EP=TP+AM1 
      PN=SQRT(EP**2-AM1**2) 
!	PRINT *
!	PRINT *,'DELTA',PN
c      write(*,*)FERMI
	IF(FERMI.NE.'Y') THEN
           CALL KINE_OR(AM,AM1,AM2,PN,TH,W,TC)
          CPF = 1.
	ELSE
           CALL KINDEL(AM,AM1,AM2,PN,TH,W,TC,CPF)
	END IF
c
c     Added to select delta
c        Hovhannes
        WW2 = AM**2+2.*W*AM   ! W 
        WW = SQRT(WW2)
        if(abs(WW-1500).gt.500)goto 1
c        write(*,*)W,WW,TP,TH*180/3.141

cccccccccc

c	PRINT *,'PN,W,CPF',PN,W,CPF
      IF(W.LE.0.)GO TO 1
!      IF(W.GE.E1)GO TO 1
    
      IF(W.GT.E1)GO TO 1
      IF(FERMI.NE.'Y') THEN
         CALL PART_OR(AM,AM1,AM2,PN,TH,AJT,AJW) 
      ELSE
         CALL PARTDEL(AM,AM1,AM2,PN,TH,AJT,AJW) 
      END IF

      CALL SIGMA_OR(W,TC,DSIGG)
      IF(SCAL.NE.'Y') THEN
C     CROSS SECTION IN UB/MEV-SR
         CALL VTP_OR(AM,AM1,E1,W,TP,TH,PHI) 
!     WRITE(*,100) AM,AM1,E1,W,TH,PHI
 100     FORMAT(2X,2F8.1,5G12.3)
         D2DEL = PHI*DSIGG*AJT*CPF 
c         write(*,*)PHI,DSIGG,AJT,CPF,D2DEL
c         write(*,*)
!     PRINT *,'PHI,AJT,DSIGG,D2DEL,CPF',PHI,AJT,DSIGG,D2DEL,CPF
!     PRINT *
      ELSE
C     CROSS SECTION IN UB/MEV-SR-Q
         BREM = 1./W
         DSIG = BREM*DSIGG*AJT*AJW
!     D2DEL = DSIG*CPF
!     D2DEL = D2DEL*EP*1.0D6/PN**2 ! Convert to ub.c/(Q.(GeV/c)^2.sr)
         D2DEL = DSIG*EP/PN
         D2DEL = D2DEL*1.0D6/PN	! Convert to units ub.c/(Q.(GeV/c)^2.sr)
         D2DEL = D2DEL*CPF
c         write(*,*)BREM,DSIGG,AJT,AJW
c         write(*,*)
      END IF
      RETURN
    1 D2DEL=0.
      RETURN
      END 
*PART_OR 
      SUBROUTINE PART_OR(AMT,AM1,AM2,PN,TN,AJT,AJW)
C  PARTIAL DERIVATIVES
      IMPLICIT REAL*8 (A-H,O-Z) 
      PI=ACOS(-1.0D0)
!      DT=PI/180.0D0
!      DP=10.0D0
      DT=PI/720.0D0
      DP=2.0D0
C  ANGLE
      TNP=TN+DT 
      TNM=TN-DT 
      TN0=TN
      CALL KINE_OR(AMT,AM1,AM2,PN,TNP,WP,TCP) 
      CALL KINE_OR(AMT,AM1,AM2,PN,TNM,WM,TCM) 
      CALL KINE_OR(AMT,AM1,AM2,PN,TN0,W,TC0) 
      DEN=COS(TNP)-COS(TNM) 
      DEN=ABS(DEN)
!	PRINT *,'DEN,TCP,TCM,TC0',DEN,TCP,TCM,TC0
      IF(DEN.GT.1.0D-3.AND.(W*WP*WM.GT.0.))THEN
         AJT=(COS(TCP)-COS(TCM))/DEN
         AJT=ABS(AJT) 
      ELSE
         AJT=(COS(TC0)-COS(TCM))/(COS(TN0)-COS(TNM))
         AJT=ABS(AJT) 
      ENDIF 
C  ENERGY 
      PNP=PN+DP 
      PNM=PN-DP 
      CALL KINE_OR(AMT,AM1,AM2,PNP,TN,WP,TC) 
      CALL KINE_OR(AMT,AM1,AM2,PNM,TN,WM,TC) 
	AM12 = AM1**2
	TP = SQRT(PNP**2 + AM12) - AM1
	TM = SQRT(PNM**2 + AM12) - AM1
      AJW=(WP-WM)/(PNP-PNM) 
!	AJW=(WP-WM)/(TP-TM) 
      AJW=ABS(AJW)
      RETURN
      END 
*KINE_OR 
      SUBROUTINE KINE_OR(AMT,AM1,AM2,PN,TH,W,TC)
c     Kinematics REAL photon only
c 
C  COMPUTES CM VARIABLES FROM LAB VARIABLES 
      IMPLICIT REAL*8 (A-H,O-Z) 
	COMMON /KF/ E1
	AMT2 = AMT**2
	AM12 = AM1**2
      EP=SQRT(PN**2+AM12) 
      PNT=PN*SIN(TH)
      PNL=PN*COS(TH)
!      ANUM=PN**2+AM2**2-(AMT-EP)**2 
      ANUM = AM2**2 - AM12 - AMT2 +2*EP*AMT
      DEN=2.*(PNL+AMT-EP) 
      W=ANUM/DEN   ! photon energy
      IF(W.LE.0.)W=0. 
C  INVARIANT MASS 
      WW2 = AMT**2+2.*W*AMT ! W 
      WW = SQRT(WW2)
c      write(*,*)'KINE_OR ',W,WW
!	PRINT *,' P,W,E_thr ',PN,WW,W,ANUM/DEN
C  CM VARIABLES 
      PCT=PNT 
      B=W/(AMT+W) 
      G=(W+AMT)/WW
      PCL=G*(PNL-B*EP)
      PCS=PCL**2+PCT**2 
      PC=SQRT(PCS)
      CTHC=PCL/PC 
      TC=ACOS(CTHC) 
c      write(*,*)AMT,AM1,AM2,PN,TH*180/3.141,W,TC*180/3.141
      RETURN
      END 
*SIGMA_OR
      SUBROUTINE SIGMA_OR(E,THRCM,SIGCM) 
C  REAL PHOTON CROSS SECTION IN DELTA REGION
C  MICROBARNS PER STERADIAN 
      IMPLICIT REAL*8 (A-H,O-Z) 
      GAM=100.
	PI=ACOS(-1.0D0)
      IF(E.GT.420.)THEN 
        SIGCM=(1.+420./E)*90./4./PI 
      ELSE
        SIGCM=360.*(5.-3.*COS(THRCM)**2)
        SIGCM=SIGCM/16./PI/(1.+(E-320)**2/GAM**2) 
      ENDIF 
      RETURN
      END 
*EP_OR 
      SUBROUTINE EP_OR(E1,TP,THP,DSEP) 
C  ELECTRO PROTON PRODUCTION CROSS SECTIONS 
      IMPLICIT REAL*8 (A-H,O-Z) 
      COMMON/Pcm/PH(10),WPH(10) 
      DATA AML/.511/
	PI=ACOS(-1.0D0)
      CALL GAUSAB_OR(10,PH,WPH,0.D0,2.*PI,PI)
      AK=SQRT(E1**2-AML**2) 
      CALL SEP_OR(AK,TP,THP,DSEP)
      DSEP=DSEP*1.E4
C  CROSS SECTION IN UB/MEV-SR 
      END 
*DOT_OR
      REAL*8 FUNCTION DOT_OR(V,U)
      IMPLICIT REAL*8 (A-H,O-Z) 
      DIMENSION V(3),U(3) 
      DOT_OR=0.
      DO 1 I=1,3
    1 DOT_OR=DOT_OR+V(I)*U(I) 
      RETURN
      END 
*CROSS
      SUBROUTINE CROSS_OR(V,U,W) 
      IMPLICIT REAL*8 (A-H,O-Z) 
      DIMENSION V(3),U(3),W(3)
      W(1)=V(2)*U(3)-V(3)*U(2)
      W(2)=V(3)*U(1)-V(1)*U(3)
      W(3)=V(1)*U(2)-V(2)*U(1)
      RETURN
      END 
*GAUSAB_OR 
      SUBROUTINE GAUSAB_OR(N,E,W,A,B,C)
      IMPLICIT REAL*8 (A-H,O-Z) 
      DIMENSION E(24),W(24)
      DATA EPS/1.D-16/
      IF(A.GE.C.OR.C.GE.B)STOP
C           STOPS PROGRAM IF A, C, B ARE OUT OF SEQUENCE
	PI=ACOS(-1.0D0)
      AL=(C*(A+B)-2*A*B)/(B-A)
      BE=(A+B-2*C)/(B-A)
      M=(N+1)/2 
      DN=N
      DO 5 I=1,M
       DI=I 
       X=PI*(4.D0*(DN-DI)+3.D0)/(4.D0*DN+2.D0)
       XN=(1.D0-(DN-1.D0)/(8.D0*DN*DN*DN))*COS(X) 
       IF(I.GT.N/2) XN=0
       DO 3 ITER=1,10 
        X=XN
        Y1=1.D0 
        Y=X 
        IF(N.LT.2) GO TO 2
        DO 1 J=2,N
         DJ=J 
         Y2=Y1
         Y1=Y 
    1    Y=((2.D0*DJ-1.D0)*X*Y1-(DJ-1.D0)*Y2)/DJ
    2   CONTINUE
        YS=DN*(X*Y-Y1)/(X*X-1.D0) 
        H=-Y/YS 
        XN=X+H
        IF(ABS(H).LT.EPS) GO TO 4 
    3   CONTINUE
    4  E(I)=(C+AL*X)/(1.D0-BE*X)
       E(N-I+1)=(C-AL*X)/(1.D0+BE*X)
       GEW=2.D0/((1.D0-X*X)*YS*YS)
       W(I)=GEW*(AL+BE*C)/(1.D0-BE*X)**2
       W(N-I+1)=GEW*(AL+BE*C)/(1.D0+BE*X)**2
    5  CONTINUE
      RETURN
      END 
*VECT_OR 
      SUBROUTINE VECT_OR(THP,THE,PHI,P,AK1,AK2)
C  CARTESIAN COMPONENTS OF ELECTRON AND PROTON VECTORS
      IMPLICIT REAL*8 (A-H,O-Z) 
      COMMON/V/AK1V(3),AK2V(3),QV(3),PV(3),PP(3)
      PV(1)=P*SIN(THP)
      PV(2)=0.
      PV(3)=P*COS(THP)
      AK1V(1)=0.
      AK1V(2)=0.
      AK1V(3)=AK1 
      AK2V(1)=AK2*SIN(THE)*COS(PHI) 
      AK2V(2)=AK2*SIN(THE)*SIN(PHI) 
      AK2V(3)=AK2*COS(THE)
      QV(1)=AK1V(1)-AK2V(1) 
      QV(2)=AK1V(2)-AK2V(2) 
      QV(3)=AK1V(3)-AK2V(3) 
      PP(1)=PV(1)-QV(1) 
      PP(2)=PV(2)-QV(2) 
      PP(3)=PV(3)-QV(3) 
      RETURN
      END 
*AMAG_OR 
      REAL*8 FUNCTION AMAG_OR(V) 
      IMPLICIT REAL*8 (A-H,O-Z) 
      DIMENSION V(3)
      AMAG_OR=0. 
      DO 1 I=1,3
    1 AMAG_OR=AMAG_OR+V(I)**2 
      AMAG_OR=SQRT(AMAG_OR) 
      RETURN
      END 
*LEPT_OR 
      SUBROUTINE LEPT_OR(E1,E2,AK1,AK2,AML,QS,QUS,THE,V) 
C  LEPTON FACTORS FOR COINCIDENCE CROSS SECTION 
      IMPLICIT REAL*8 (A-H,O-Z) 
      DIMENSION V(5)
      V(1)=(QUS/QS)**2*(E1*E2+AK1*AK2*COS(THE)+AML**2)
      X=AK1*AK2*SIN(THE)
      V(2)=X**2/QS+QUS/2. 
      V(3)=QUS/QS*X/SQRT(QS)*(E1+E2)
      V(4)=X**2/QS
      V(5)=0. 
      RETURN
      END 
*D4S_OR
      SUBROUTINE D4S_OR(AK1,AK2,THE,P,PP,THQP,CPHIP,DSIG)
C  FULLY DIFFERENTIAL CROSS SECTION 
      IMPLICIT REAL*8 (A-H,O-Z) 
      DIMENSION V(5),W(5) 
      DATA AM/939./,AML/.511/,A/855./ 
      QS=AK1**2+AK2**2-2.*AK1*AK2*COS(THE)
      E1=SQRT(AK1**2+AML**2)
      E2=SQRT(AK2**2+AML**2)
      QUS=2.*(E1*E2-AK1*AK2*COS(THE)-AML**2)
      SM=2.*(1.44)**2/QUS**2*AK2/AK1
      EP=SQRT(AM**2+P**2) 
      PS=EP*P 
      FNS=1./(1.+QUS/A**2)**4 
      CALL LEPT_OR(E1,E2,AK1,AK2,AML,QS,QUS,THE,V) 
      CALL FORM_OR(QS,P,THQP,CPHIP,W)
      SUM=0.
      DO 1 I=1,5
    1 SUM=SUM+V(I)*W(I) 
      DSIG=SM*PS*FNS*SUM*SGSL_OR(PP) 
      RETURN
      END 
*STHE_OR 
      SUBROUTINE STHE_OR(D2S)
C  INTEGRAL OVER ELECTRON POLAR ANGLE 
      IMPLICIT REAL*8 (A-H,O-Z) 
      COMMON/S/ AK1,AK2,THE,P,THP
      COMMON/E/TH1(12),WT1(12),TH2(12),WT2(12)
      COMMON/E1/TH3(24),WT3(24) 
      D2S1=0. 
      DO 1 I=1,12 
      THE=TH1(I)
      CALL SPHI_OR(D3S)
    1 D2S1=D2S1+D3S*WT1(I)*SIN(THE) 
      D2S2=0. 
      DO 2 I=1,12 
      THE=TH2(I)
      CALL SPHI_OR(D3S)
    2 D2S2=D2S2+D3S*WT2(I)*SIN(THE) 
      D2S3=0. 
      DO 3 I=1,24 
      THE=TH3(I)
      CALL SPHI_OR(D3S)
    3 D2S3=D2S3+D3S*WT3(I)*SIN(THE) 
      D2S=D2S1+D2S2+D2S3
      RETURN
      END 
*SPHI_OR 
      SUBROUTINE SPHI_OR(D3S)
C  INTEGRATE OVER ELECTRON AZIMUTHAL ANGLE
      IMPLICIT REAL*8 (A-H,O-Z) 
      COMMON/S/ AK1,AK2,THE,P,THP 
      COMMON/V/AK1V(3),AK2V(3),QV(3),PV(3),PP(3)
      COMMON/Pcm/PH(10),WPH(10) 
      DIMENSION QXP(3),AK1X2(3) 
      D3S=0.
      DO 1 I=1,10 
      PHI=PH(I) 
      CALL VECT_OR(THP,THE,PHI,P,AK1,AK2)
      CALL CROSS_OR(QV,PV,QXP) 
      CALL CROSS_OR(AK1V,AK2V,AK1X2) 
C  PROTON THETA 
      CTHEP=DOT_OR(PV,QV)/AMAG_OR(PV)/AMAG_OR(QV)
      THQP=ACOS(CTHEP)
C  PROTON PHI 
      CPHIP=DOT_OR(QXP,AK1X2)
      IF (CPHIP.EQ.0.) THEN 
         CPHIP=1. 
      ELSE
         CPHIP=CPHIP/AMAG_OR(QXP)/AMAG_OR(AK1X2)
      ENDIF 
      PPM=AMAG_OR(PP)
      CALL D4S_OR(AK1,AK2,THE,P,PPM,THQP,CPHIP,DSIG) 
    1 D3S=D3S+DSIG*WPH(I) 
      RETURN
      END 
*FORM_OR 
      SUBROUTINE FORM_OR(QS,P,THQP,CPHIP,W)
C  NUCLEAR FORM FACTORS 
      IMPLICIT REAL*8 (A-H,O-Z) 
      COMMON/M/ZZ,NN
      COMMON/DEL/IP 
      DIMENSION W(5)
      DATA AM/939./,UP/2.79/,UN/-1.91/
      IF(IP.EQ.1)THEN 
         Z=ZZ 
         N=0. 
      ELSEIF(IP.EQ.-1)THEN
         Z=0. 
         N=NN 
      ELSE
         Z=0. 
         N=0. 
      ENDIF 
      Y=P/AM*SIN(THQP)
      W(1)=Z
      W(2)=Z*Y**2 
      W(2)=W(2)+(Z*UP**2+N*UN**2)*QS/2./AM**2 
      W(3)=-2.*Z*Y*CPHIP
      W(4)=Z*Y**2*(2.*CPHIP**2-1.)
      W(5)=0. 
      RETURN
      END 
*SEP_OR
      SUBROUTINE SEP_OR(AK,TP,THPP,D2S)
      IMPLICIT REAL*8 (A-H,O-Z) 
      COMMON/S/AK1,AK2,THE,P,THP
      COMMON/E/TH1(12),WT1(12),TH2(12),WT2(12)
      COMMON/E1/TH3(24),WT3(24) 
      DATA AM/939./,AML/.511/,BE/16./ 

	PI=ACOS(-1.0D0)
      THP=THPP
      AK1=AK
      AK2=AK1-TP-BE 
C  GAUSSIAN POINTS FOR THE
      THEMAX=AML*(AK1-AK2)/AK1/AK2
      CALL GAUSAB_OR(12,TH1,WT1,0.D0,2.*THEMAX,THEMAX) 
      CALL GAUSAB_OR(12,TH2,WT2,2.*THEMAX,100.*THEMAX,10.*THEMAX)
      A3=100.*THEMAX
      C3=A3+(PI-A3)/10. 
      CALL GAUSAB_OR(24,TH3,WT3,A3,PI,C3)
      P=SQRT(TP**2+2.*TP*AM)
      CALL STHE_OR(D2S)
      IF(AK2.LE.0.)D2S=0. 
      RETURN
      END 
*SGSL_OR 
      REAL*8 FUNCTION SGSL_OR(P) 
      IMPLICIT REAL*8 (A-H,O-Z) 
C  P INTEGRAL OVER SGSL_OR NORMALIZED TO 1/4PI 
      COMMON/SG/IA
      COMMON/QD/QDF 
      IF(IA.EQ.2)THEN 
C  BEGIN 2-H
         PP=P/197.3 
         SGS=3.697-7.428*PP-2.257*PP**2 
         SGS=SGS+3.618*PP**3-1.377*PP**4+.221*PP**5-.013*PP**6
         IF(SGS.LT.-293.)GO TO 1
         SGS=EXP(SGS) 
         SGS=SGS/.18825/4./3.1416/(197.3)**3
!         SGSL_OR=SGS/1.
      ELSEIF(IA.EQ.3)THEN 
C  BEGIN 3-HE 
         IF(-(P/33)**2.LT.-293.)GO TO 1 
         SGS=2.4101E-6*EXP(-P/33) 
         SGS=SGS-1.4461E-6*EXP(-(P/33)**2)
         SGS=SGS+1.6871E-10*EXP(-(P/493)**2)
         SGSL_OR=SGS/2.0D0
      ELSEIF(IA.EQ.4)THEN 
C   BEGIN 4-HE
         IF(-(P/113.24)**2.LT.-293.)GO TO 1 
         SGS=1.39066E-6*EXP(-(P/113.24)**2) 
         SGS=SGS+3.96476E-9*EXP(-(P/390.75)**2) 
         SGSL_OR=SGS/2.0D0
         SGSL_OR=SGSL_OR/2.0D0/3.1416
      ELSEIF(IA.GT.4.AND.IA.LT.12)THEN
         IF(-(P/127)**2.LT.-293.)GO TO 1
         SGS=1.7052E-7*(1.+(P/127)**2)*EXP(-(P/127)**2) 
         SGS=SGS+1.7052E-9*EXP(-(P/493)**2) 
         SGSL_OR=SGS/(FLOAT(IA)/2.0D0)
      ELSEIF(IA.EQ.12)THEN
C  BEGIN 12-C 
         IF(-(P/127)**2.LT.-293.)GO TO 1
         SGS=1.7052E-7*(1.+(P/127)**2)*EXP(-(P/127)**2) 
         SGS=SGS+1.7052E-9*EXP(-(P/493)**2) 
         SGSL_OR=SGS/6.
      ELSE
C  BEGIN 16-O 
         IF(-(P/120)**2.LT.-293.)GO TO 1
         SGS=3.0124E-7*(1.+(P/120)**2)*EXP(-(P/120)**2) 
         SGS=SGS+1.1296E-9*EXP(-(P/493)**2) 
         SGSL_OR=SGS/(FLOAT(IA)/2.0D0)
      ENDIF 
      RETURN
    1 SGSL_OR=0. 
      RETURN
      END 
*S2PI 
*USEFIT='Y' will use scaling fit result other than wiser
      SUBROUTINE S2PI(IP,E1,TP,TH,D2SC,USEFIT) 
C  INTEGRAL OVER SCALING CROSS SECTION
      IMPLICIT REAL*8 (A-H,O-Z) 
      CHARACTER*1 SCAL,FERMI
 	COMMON /FER/ FERMI,SCAL
      CHARACTER*1 USEFIT
      DATA AM/939./,AMP/139./ 
      IF(ABS(IP).EQ.1)THEN
C  ONE PION THR 
         AP=AM
         AM2=AMP
      ELSEIF(IP.EQ.2.OR.IP.EQ.0)THEN
C  ONE PION THR 
         AP=AMP 
         AM2=AM 
      ELSEIF(IP.EQ.-2)THEN
C  TWO PION THR 
         AP=AMP 
         AM2=AM+AMP 
      ELSE
         STOP 
      ENDIF 
      P=SQRT(TP**2+2.*AP*TP)
      E=TP+AP 
!	PRINT *,'S2PI',P
	IF(FERMI.NE.'Y') THEN
      CALL KINE_OR(AM,AP,AM2,P,TH,THR,TC)
	CPF = 1.
	ELSE
      CALL KINDEL(AM,AP,AM2,P,TH,THR,TC,CPF)
	END IF
!	 PRINT *,'P,THR',P,THR
      IF(THR.LE.0.)GOTO 2 
!      IF(E1.LE.THR)GOTO 2 
      IF(E1.LT.THR)GOTO 2 
      DW=(E1-THR)/20.0D0
      SUM=0.0D0
      SUMBR=0.0D0
!      DO 1 I=1,20 
      DO I=1,20 
        W=THR+(FLOAT(I)-.5D0)*DW
        IF(W.LT.(E1-0.511)) THEN
          CALL VTP_OR(AM,AMP,E1,W,TP,TH,GN)
!	  WRITE(*,100) I,AM,AMP,E1,W,DW,THR,GN
100	FORMAT(I4,2F8.1,5G12.3)
          IF (USEFIT.EQ.'Y')THEN
             CALL SCALINGFIT(P/1.E3,TH,F) ! F is cross section
          ELSE
             CALL WISER(W/1.E3,P/1.E3,TH,F)
          END IF
!	  PRINT *,'P,W,GN,F',P,W,GN,F
          SUM=SUM+GN*F*DW 
	  SUMBR = SUMBR + F*DW/W
c             write(*,*) ' scal fit = ',P,TH,F,sumbr
!    1 CONTINUE
        END IF
      END DO
	IF(SCAL.EQ.'Y') THEN
	D2SC=SUMBR  ! bremstrahlung photo-production xn = Ed^3sigma/dp^3 ubarns/sr/(Gev/c) per equivalent quanta
	ELSE
	D2SC=SUM*(P**2/E*1e-3) ! electro-production converts to d^2sigma/domega/dp microbarns/sr/(GeV/c)
c        write(*,*)'W',Sum,CPF,D2SC*CPF*1.E+6
	END IF
	D2SC = D2SC*CPF  
c	write(*,*)'D2PI',D2SC,CPF
      RETURN
    2 D2SC=0. 
      RETURN
      END 
*WISER
      SUBROUTINE WISER(W,P,TH,F)
C  INVARIANT INCLUSIVE CROSS SECTION
C  UNITS IN GEV 
      IMPLICIT REAL*8 (A-H,O-Z) 
      COMMON/DEL/IP 
      DIMENSION A(7),B(7),C(7)
      DATA A/5.66E2,8.29E2,1.79,2.10,-5.49,-1.73,0./
      DATA B/4.86E2,1.15E2,1.77,2.18,-5.23,-1.82,0./
      DATA C/1.33E5,5.69E4,1.41,0.72,-6.77,1.90,-1.17E-2/ 
      DATA AM/.939/,AMP/.139/ 
      IF(ABS(IP).EQ.1)THEN
         AP=AM
      ELSEIF(ABS(IP).EQ.2.OR.IP.EQ.0)THEN 
         AP=AMP 
      ELSE
         STOP 
      ENDIF 
      E=SQRT(P**2+AP**2)
C  MANDELSTAM VARIABLES 
      S=2.*W*AM+AM**2 
C     T=-2.*W*E+2.*W*P*COS(TH) +AP**2 
      U=AM**2-2.*AM*E+AP**2 
C  FITTING VARIABLES
      PT=P*SIN(TH)
      AML=SQRT(PT**2+AP**2) 
      CALL FXR(W,P,E,TH,XR) 
C  FITTED 
      IF(IP.EQ.2.OR.IP.EQ.0)THEN
         X1=A(1)+A(2)/SQRT(S) 
         X2=(1.-XR+A(3)**2/S)**A(4) 
         X3=EXP(A(5)*AML) 
         X4=EXP(A(6)*PT**2/E) 
         F=X1*X2*X3*X4
      ELSEIF(IP.EQ.-2)THEN
         X1=B(1)+B(2)/SQRT(S) 
         X2=(1.-XR+B(3)**2/S)**B(4) 
         X3=EXP(B(5)*AML) 
         X4=EXP(B(6)*PT**2/E) 
         F=X1*X2*X3*X4
      ELSEIF(ABS(IP).EQ.1)THEN
         X1=C(1)+C(2)/SQRT(S) 
         X2=(1.-XR+C(3)**2/S)**C(4) 
         X3=EXP(C(5)*AML) 
         X4=1./(1.+ABS(U))**(C(6)+C(7)*S) 
         F=X1*X2*X3*X4
      ELSE
         STOP 
      ENDIF 
      RETURN
      END 
*FXR
      SUBROUTINE FXR(W,P,E,TH,XR) 
C  COMPUTES RATIO OF CM PARTICLE MOMENTUM TO PHOTON MOMENTUM
C  GEV UNITS
      IMPLICIT REAL*8 (A-H,O-Z) 
      DATA AM/.939/ 
      PT=P*SIN(TH)
      PL=P*COS(TH)
C  LORENTZ TRANSFORMATION 
      B=W/(W+AM)
      D=SQRT(2.*W*AM+AM**2) 
      G=(W+AM)/D
      BG=B*G
C CM VARIABLES
      WC=G*W-BG*W 
      PLC=G*PL-BG*E 
      PC=SQRT(PT**2+PLC**2) 
      XR=PC/WC
      RETURN
      END 
*KINDEL
      SUBROUTINE KINDEL(AMT,AM1,AM2,PN,TH,W,TC,CPF) 
C  COMPUTES CM VARIABLES FROM LAB VARIABLES 
      IMPLICIT REAL*8 (A-H,O-Z) 
	COMMON /KF/ E1
	DATA COSAV/0.63662/
	AMT2 = AMT**2
	AM12 = AM1**2
      EP=SQRT(PN**2+AM12) 
      PNT=PN*SIN(TH)
      PNL=PN*COS(TH)
!      ANUM=PN**2+AM2**2-(AMT-EP)**2 
      ANUM = AM2**2 - AM12 - AMT2 +2*EP*AMT
      DEN=2.*(PNL+AMT-EP) 
      W=ANUM/DEN
c        write(*,*)'KINDEL       dsssssssss', W,ww
      IF(W.LE.0.)W=0. 
C  INVARIANT MASS 
      WW2 = AMT**2+2.*W*AMT
      WW = SQRT(WW2)
!	PRINT *,' P,W,E_thr ',PN,WW,W,ANUM/DEN

C	Section on Fermi motion

	IF(CPF.GE.0.) THEN	! CPF = -1. for Jacobian computation
	CPF = 1.0D0
	IF(W.GE.E1) THEN
	DM2 = WW2 - AMT2 
	PFMIN = E1*((DM2/2./E1)**2 - AMT2)/DM2
	CPF0 = SGSL_OR(0.d0)
	CPF = SGSL_OR(PFMIN)
	IF(CPF.GT.0.0D0) THEN
	DPF = 10.0D0
	PF = PFMIN
	DSPF = CPF*PF
	SUMPF = DSPF
	SUMCF = CPF
	DO WHILE((DSPF/SUMPF).GT.1.0D-4)
	PF = PF + DPF
	CPF = SGSL_OR(PF)
	DSPF = CPF*PF
	SUMPF = DSPF + SUMPF
	SUMCF = CPF + SUMCF
	END DO
!	PRINT *,'PF,DSPF,SUMPF',PF,DSPF,SUMPF
	PFMEAN = SUMPF/SUMCF
	PFMEAN = PFMEAN*COSAV
	CPF = SGSL_OR(PFMEAN)/CPF0
	CPF = 0.5*CPF
	EF = SQRT(PFMEAN**2 + AMT**2)
	WEFF = DM2/(2.0D0*(EF + PFMEAN))
!	WEFF = DM2/(2.0D0*EF)
!	PRINT *,'W,WW,PFMIN,CPF0',W,WW,PFMIN,CPF0
!	PRINT *,'PFMEAN,CPF,WEFF',PFMEAN,CPF,WEFF
	W = WEFF
	ELSE
	W = 0.
	END IF
	END IF
	END IF

C  CM VARIABLES 
      PCT=PNT 
      B=W/(AMT+W) 
      G=(W+AMT)/WW
      PCL=G*(PNL-B*EP)
      PCS=PCL**2+PCT**2 
      PC=SQRT(PCS)
      CTHC=PCL/PC 
      TC=ACOS(CTHC) 
      RETURN
      END 
*PARTDEL
      SUBROUTINE PARTDEL(AMT,AM1,AM2,PN,TN,AJT,AJW)
C  PARTIAL DERIVATIVES
      IMPLICIT REAL*8 (A-H,O-Z) 
      PI=ACOS(-1.0D0)
!      DT=PI/180.0D0
!      DP=10.0D0
      DT=PI/720.0D0
      DP=2.0D0
C  ANGLE
      TNP=TN+DT 
      TNM=TN-DT 
      TN0=TN
	CPF = -1.
      CALL KINDEL(AMT,AM1,AM2,PN,TNP,WP,TCP,CPF)
	CPF = -1.
      CALL KINDEL(AMT,AM1,AM2,PN,TNM,WM,TCM,CPF)
	CPF = -1.
      CALL KINDEL(AMT,AM1,AM2,PN,TN0,W,TC0,CPF)
      DEN=COS(TNP)-COS(TNM) 
      DEN=ABS(DEN)
!	PRINT *,'DEN,TCP,TCM,TC0',DEN,TCP,TCM,TC0
      IF(DEN.GT.1.0D-3.AND.(W*WP*WM.GT.0.))THEN
         AJT=(COS(TCP)-COS(TCM))/DEN
         AJT=ABS(AJT) 
      ELSE
         AJT=(COS(TC0)-COS(TCM))/(COS(TN0)-COS(TNM))
         AJT=ABS(AJT) 
      ENDIF 
C  ENERGY 
      PNP=PN+DP 
      PNM=PN-DP 
      CALL KINE_OR(AMT,AM1,AM2,PNP,TN,WP,TC) 
      CALL KINE_OR(AMT,AM1,AM2,PNM,TN,WM,TC) 
	AM12 = AM1**2
	TP = SQRT(PNP**2 + AM12) - AM1
	TM = SQRT(PNM**2 + AM12) - AM1
      AJW=(WP-WM)/(PNP-PNM) 
!	AJW=(WP-WM)/(TP-TM) 
      AJW=ABS(AJW)
      RETURN
      END 
*OARA
	SUBROUTINE OARA(W,P,TH,DSDWDP)
	IMPLICIT REAL*8 (A-H,M-Z)
	INTEGER N
	COMMON/SG/IA
	COMMON/QD/QDF 
      	COMMON/DEL/IP 
        CHARACTER*1 SCAL,FERMI
 	COMMON /FER/ FERMI,SCAL
      	COMMON/M/Z,N
	COMMON/KF/E1
	
	DATA MPI/0.140/
	DATA B/7.0D0/,C/9.0D0/,STOT/125.0D0/
	DATA CP/8.0D3/,T0/124.0D-3/

	IF(ABS(IP).EQ.1) THEN
	DSDWDP = STOT*C*EXP(-B*P**2)
	ELSE IF (ABS(IP).EQ.2.OR.IP.EQ.0) THEN
	T = SQRT(P**2+MPI**2) - MPI
	DSDWDP = CP*EXP(-T/T0)
	END IF
	RETURN
	END

*SCALEFIT
! By Jixie @ Dec 10, 2014
! Update parameter using the latest fitted parameters, E in unit of [ub/sr/(GeV/c)^2][c/Q]
! https://userweb.jlab.org/~rondon/analysis/pairs/scalfit-photo.pdf for pi+/pi-
! https://userweb.jlab.org/~rondon/analysis/pairs/scal-fit.pdf for pi0
! https://hallcweb.jlab.org/experiments/sane/wiki/index.php/Inclusive_pion_and_nucleon_electroproduction
	SUBROUTINE SCALINGFIT(P,TH,F)
        REAL*8 P,TH,F
c  P is momentum in GeV/c
c  Th is angle in radians
c  F is photo production cross section = E d^3sigma/d^3p microbarns/sr/(GeV/c)(c/Q)  Q=equivalent quanta
c        F = 9577*EXP(-8.759*P*SIN(TH))  !for pi-
c        F = 11289*EXP(-8.536*P*SIN(TH)) !for pi+
        F = 10219*EXP(-8.675*P*SIN(TH))/12.  !for pi0 on 12C divied by 12 for per nucleon cross section
c        PRINT *, 'P,TH,F',P,TH,p*sin(th),F
        RETURN
        END
