*CMZ :          02/08/94  19.29.57  by  S.Ravndal
*-- Author :
      SUBROUTINE GXPHYS
C.
C.    ******************************************************************
C.    *                                                                *
C.    *      Physics parameters control commands                       *
C.    *                                                                *
C.    *       Author:    R.Brun      **********                        *
C.    *                                                                *
C.    ******************************************************************
C.
*KEEP,GCBANK.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
*KEEP,GCPHYS.
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
*KEEP,GCCUTS.
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      REAL          CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS
C
*KEEP,GCONSP.
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
*KEEP,GCUNIT.
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
*KEEP,GCMULO.
      COMMON/GCMULO/SINMUL(101),COSMUL(101),SQRMUL(101),OMCMOL,CHCMOL
     +  ,EKMIN,EKMAX,NEKBIN,NEK1,EKINV,GEKA,GEKB,EKBIN(200),ELOW(200)
C
      REAL SINMUL,COSMUL,SQRMUL,OMCMOL,CHCMOL,EKMIN,EKMAX,ELOW,EKINV
      REAL GEKA,GEKB,EKBIN
      INTEGER NEKBIN,NEK1
C
*KEND.
      DIMENSION UCUTS(10),ULCUTS(10)
      EQUIVALENCE(UCUTS(1),CUTGAM)
      DIMENSION MECA(5,13)
      EQUIVALENCE (MECA(1,1),IPAIR)
      CHARACTER*6 CUTNAM(10)
      CHARACTER*4 CEN(10)
      CHARACTER*32 CHPATL
      CHARACTER*(*) CHNUMB
      PARAMETER (CHNUMB='1234567890')
      DATA CUTNAM/'CUTGAM','CUTELE','CUTNEU','CUTHAD','CUTMUO',
     +            'BCUTE' ,'BCUTM' ,'DCUTE' ,'DCUTM' ,'PPCUTM'/
C.
C.    ------------------------------------------------------------------
C.
      CALL KUPATL(CHPATL,NPAR)
*
      IF(CHPATL.EQ.'ANNI')THEN
         CALL KUGETI(IANNI)
*
      ELSEIF(CHPATL.EQ.'AUTO')THEN
         CALL KUGETI(IAUTO)
*
      ELSEIF(CHPATL.EQ.'BREM')THEN
         CALL KUGETI(IBREM)
*
      ELSEIF(CHPATL.EQ.'CKOV')THEN
         CALL KUGETI(ICKOV)
*
      ELSEIF(CHPATL.EQ.'COMP')THEN
         CALL KUGETI(ICOMP)
*
      ELSEIF(CHPATL.EQ.'DCAY')THEN
         CALL KUGETI(IDCAY)
*
      ELSEIF(CHPATL.EQ.'DRAY')THEN
         CALL KUGETI(IDRAY)
*
      ELSEIF(CHPATL.EQ.'ERAN')THEN
         CALL KUGETR(EKMIN)
         CALL KUGETR(EKMAX)
         CALL KUGETI(NEKBIN)
         NEKBIN=MIN(NEKBIN,199)
*
      ELSEIF(CHPATL.EQ.'HADR')THEN
         CALL KUGETI(IHADR)
*
      ELSEIF(CHPATL.EQ.'LABS')THEN
         CALL KUGETI(ILABS)
*
      ELSEIF(CHPATL.EQ.'LOSS')THEN
         CALL KUGETI(ILOSS)
         IF(ILOSS.EQ.0.OR.ILOSS.EQ.2)THEN
            IDRAY=0
         ELSE
            IDRAY=1
         ENDIF
*
      ELSEIF(CHPATL.EQ.'MULS')THEN
         CALL KUGETI(IMULS)
*
      ELSEIF(CHPATL.EQ.'MUNU')THEN
         CALL KUGETI(IMUNU)
*
      ELSEIF(CHPATL.EQ.'PAIR')THEN
         CALL KUGETI(IPAIR)
*
      ELSEIF(CHPATL.EQ.'PFIS')THEN
         CALL KUGETI(IPFIS)
*
      ELSEIF(CHPATL.EQ.'PHOT')THEN
         CALL KUGETI(IPHOT)
*
      ELSEIF(CHPATL.EQ.'RAYL')THEN
         CALL KUGETI(IRAYL)
*
      ELSEIF(CHPATL.EQ.'STRA')THEN
         CALL KUGETI(ISTRA)
*
      ELSEIF(CHPATL.EQ.'SYNC')THEN
         CALL KUGETI(ISYNC)
*
      ELSEIF(CHPATL.EQ.'CUTS')THEN
         IF(NPAR.LE.0)THEN
            WRITE(LOUT,10000)
10000       FORMAT(/,' Current PHYSICS parameters:',/)
            DO 10 I=1,10
               CALL GEVKEV(UCUTS(I),ULCUTS(I),CEN(I))
               WRITE(LOUT,10100)CUTNAM(I),ULCUTS(I),CEN(I)
10100          FORMAT(5X,A,' = ',F7.2,1X,A)
   10       CONTINUE
            GO TO 999
         ENDIF
         CALL KUGETR(CUTGAM)
         CALL KUGETR(CUTELE)
         CALL KUGETR(CUTHAD)
         CALL KUGETR(CUTNEU)
         CALL KUGETR(CUTMUO)
         CALL KUGETR(BCUTE)
         CALL KUGETR(BCUTM)
         CALL KUGETR(DCUTE)
         CALL KUGETR(DCUTM)
         CALL KUGETR(PPCUTM)
         CALL KUGETR(TOFMAX)
         CALL KUGETR(GCUTS(1))
         IF(BCUTE.LE.0.)BCUTE=CUTGAM
         IF(BCUTM.LE.0.)BCUTM=CUTGAM
         IF(DCUTE.LE.0.)DCUTE=CUTELE
         IF(DCUTM.LE.0.)DCUTM=CUTELE
         IF(PPCUTM.LT.4.*EMASS)PPCUTM=4.*EMASS
*
      ELSEIF(CHPATL.EQ.'DRPRT')THEN
         CALL KUGETI(IPART)
         CALL KUGETI(IMATE)
         CALL KUGETR(STEP)
         CALL KUGETI(NPOINT)
         CALL GDRPRT(IPART,IMATE,STEP,NPOINT)
*
      ELSEIF(CHPATL.EQ.'PHYSI')THEN
         IF(JTMED.GT.0)THEN
            DO 30 I=1,IQ(JTMED-2)
               JTM=LQ(JTMED-I)
               IF(JTM.LE.0)GO TO 30
               IF(IQ(JTM-2).EQ.0)THEN
                  CALL MZPUSH(IXCONS,JTM,10,0,'I')
                  GO TO 30
               ENDIF
               DO 20 J=1,10
                  JTMI=LQ(JTM-J)
                  IF(JTMI.GT.0)THEN
                     CALL MZDROP(IXCONS,JTMI,' ')
                  ENDIF
   20          CONTINUE
   30       CONTINUE
            CALL UCOPY(CUTGAM,Q(JTMED+1),10)
            DO 40 I=1,13
               Q(JTMED+10+I)=MECA(1,I)
   40       CONTINUE
         ENDIF
         IF(JMATE.LE.0)GO TO 999
         DO 60 I=1,IQ(JMATE-2)
            JMA=LQ(JMATE-I)
            IF(JMA.LE.0)GO TO 60
            DO 50 J=1,IQ(JMA-2)
               IF(J.EQ.4.OR.J.EQ.5)GO TO 60
               JM=LQ(JMA-J)
               IF(JM.LE.0)GO TO 50
               CALL MZDROP(IXCONS,JM,'L')
   50       CONTINUE
   60    CONTINUE
         CALL MZGARB (IXCONS, 0)
         CALL GPHYSI
      ENDIF
*
  999 END