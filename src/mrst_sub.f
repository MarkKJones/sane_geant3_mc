***********************************************************************
*                                                                     *
*    Program for generating Electromagnetic Structure Functions using *
*    consistent treatment of charm and bottom structure functions     *
*    Not included are the effects due to NLO corrections to photon-   *
*    gluon fusion.   Charm mass = 1.35 GeV   Bottom mass = 4.3 GeV    *
*                                                                     *
*    The program should be run only with iord set to 1                *
*    The calculation of F_L includes the full order(alpha_s^2)        *
*    contribution                                                     *
*    The program is self contained, only requiring the subroutine     *
*    mrst2001.f and the grid file alf119.dat to be accessible         *
*                                                                     *
***********************************************************************
      subroutine mrst_sub(q2,x,f1p,f2p,f1n,f2n)
      implicit real*8(a-h,o-z)
     
      COMMON/GAUS96/XI(96),WI(96),XX(97),NTERMS
      COMMON/INPUT/alambda,flavor,qsct,qsdt,iord
      common/alfscl/sclfac

      call wate96
      iord=1
      flavor=3
      qsdt=8.18
      sclfac=0.0
      qsct=74.
      mode=1
      alambda=0.323
c   99 read(5,*) x,q2
      call sfun(x,q2,mode,f2p,flp,f1p,rp,f2n,fln,f1n,rn,
     +     f2c,flc,f1c,f2b,flb,f1b)
c      scale=dsqrt(q2)
c      call mrst2001(x,scale,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
cc      sing=upv+dnv+2.*(usea+dsea+str+chm+bot)

*GAW      print 100,x,q2,f2p,flp,f1p,f2n,fln,f1n
*GAW      print 101,f2c,flc,f1c,f2b,flb,f1b

C      go to 99

  100 format(1x,'x= ',f10.6,' Q2= ',f14.3/
     11x,' Proton Structure Functions '/
     25x,' F2= ',f8.4,' FL= ',f8.4,' 2xF1= ',f8.4/
     31x,' Neutron Structure Functions '/
     45x,' F2= ',f8.4,' FL= ',f8.4,' 2xF1= ',f8.4/)
  101 format(1x,' Contributions from Charm'/
     15x,' F2(c)= ',f8.4,' FL(c)= ',f8.4,' 2xF1(c)= ',f8.4/
     21x,' Contributions from Bottom'/
     35x,' F2(b)= ',f8.4,' FL(b)= ',f8.4,' 2xF1(b)= ',f8.4/)
      return
      end

      subroutine sfun(x,q2,mode,f2p,flp,f1p,rp,f2n,fln,f1n,rn,
     xf2c,flc,f1c,f2b,flb,f1b)
      implicit real*8(a-h,o-z)
      COMMON/GAUS96/XI(96),WI(96),XX(97),NTERMS
      COMMON/INPUT/alambda,flavor,qsct,qsdt,iord
      common/alfscl/sclfac
      common/comptc/chmq,chmg,chmgsw,chmdcg
      common/comptcl/chmlq,chmlg
      common/comptb/botq,botg,botgsw,botdcg
      data pi,pi2/3.14159,9.8696/
      XLAM=alambda
      IFL=4
      xlam2=xlam*xlam
      t=dlog(q2/xlam2)
      al=alpha(t)/(4.*pi)
      tt=t+sclfac
      argmin=qsdt/4./xlam2
      scale=dsqrt(q2)

      cf=4./3.
      ca=3.
      enf=flavor
      iorder=iord
      dpsi2=2./9.

      ww2=(1.-x)*q2/x
      epsc4=qsdt/q2
      fpsc4=qsdt/ww2
      epsc=epsc4/4.
      thcq=1.
      if(epsc.gt.1.) thcq=0.
      thcw=1.
      if(fpsc4.gt.1.) thcw=0.
      epsb4=qsct/q2
      fpsb4=qsct/ww2
      epsb=epsb4/4.
      thbq=1.
      if(epsb.gt.1.) thbq=0.
      thbw=1.
      if(fpsb4.gt.1.) thbw=0.
      call mrst2001(x,scale,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
      if(epsc.gt.1.) chm=0.
      if(epsb.gt.1.) bot=0.
      fp=(4.*upv+dnv+8.*usea+2.*dsea+2.*str)/9.
      fn=(4.*dnv+upv+2.*usea+8.*dsea+2.*str)/9.
      fc=8.*chm/9.
      fb=2.*bot/9.

      ffp=0.
      ffn=0.
      ffc=0.
      ffb=0.
      chmq=0.
      chmg=0.
      chmgsw=0.
      chmdcg=0.
      chmlq=0.
      chmlg=0.
      botq=0.
      botg=0.
      botgsw=0.
      botdcg=0.
      fflp=0.
      ffln=0.
      fflc=0.
      fflb=0.

      IF(IORD.LE.0.) THEN 
        GO TO 27
      ELSE
        GO TO 22
      ENDIF
   22 CONTINUE
      IF3=0

      FAC=FLAVOR
      facc=1.
      facb=1.

      IF(ifl.EQ.3.OR.ifl.EQ.4) then
      FAC=6./9.
      facc=4./9.
      facb=1./9.
      endif

      IF(IFL.EQ.1) IF3=1
      CF=4./3.
      AL1=dLOG(1.-X)

      ffp=Fp+Fp*AL*CF*(-9.-2.*PI2/3.+AL1*(-3.+2.*AL1))
      ffn=Fn+Fn*AL*CF*(-9.-2.*PI2/3.+AL1*(-3.+2.*AL1))
c      ffc=ffc+fc*AL*CF*(-9.-2.*PI2/3.+AL1*(-3.+2.*AL1))
c      ffb=ffb+fb*AL*CF*(-9.-2.*PI2/3.+AL1*(-3.+2.*AL1))

      DO 23 I=1,NTERMS
      Y=0.5*(1.-X)*XI(I)+0.5*(1.+X)
      XY=X/Y
      AL1=dLOG(1.-Y)
      call mrst2001(xy,scale,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
      if(epsc.gt.1.) chm=0.
      if(epsb.gt.1.) bot=0.
      fpxy=(4.*upv+dnv+8.*usea+2.*dsea+2.*str)/9.
      fnxy=(4.*dnv+upv+2.*usea+8.*dsea+2.*str)/9.
      fcxy=8.*chm/9.
      fbxy=2.*bot/9.
      gluxy=glu
      C22=CF*(6.+4.*Y-2.*(1.+Y*Y)/(1.-Y)*dLOG(Y)-2.*(1.+Y)*AL1
     2-IF3*2.*(1.+Y))
      C23=CF*(-3.+4.*AL1)/(1.-Y)
      CG2=2.*FAC*(-1.+8.*Y*(1.-Y)+(1.-2.*Y+2.*Y*Y)*dLOG(1./Y-1.))
      f1lq=4.*cf*y
      f1lg=8.*enf*y*(1.-y)

      ffp=ffp+.5*(1.-X)*WI(I)*AL*(C22*fpxy+C23*(fpxy-fp))
      ffn=ffn+.5*(1.-X)*WI(I)*AL*(C22*fnxy+C23*(fnxy-fn))
c      ffc=ffc+.5*(1.-X)*WI(I)*AL*(C22*fcxy+C23*(fcxy-fc))
c      ffb=ffb+.5*(1.-X)*WI(I)*AL*(C22*fbxy+C23*(fbxy-fb))

      fflp=fflp+.5*(1.-x)*wi(i)*al*f1lq*fpxy
      ffln=ffln+.5*(1.-x)*wi(i)*al*f1lq*fnxy

      IF(IFL-1) 23,23,24
   24 CONTINUE

      ffp=ffp+.5*(1.-X)*WI(I)*AL*CG2*gluxy
      ffn=ffn+.5*(1.-X)*WI(I)*AL*CG2*gluxy

      fflp=fflp+.5*(1.-x)*wi(i)*al*dpsi2*f1lg*gluxy
      ffln=ffln+.5*(1.-x)*wi(i)*al*dpsi2*f1lg*gluxy

   23 CONTINUE
   21 CONTINUE

      r7=dsqrt(7.d0)
      xcmax=1./(1.+epsc4)
      if(xcmax.le.x) go to 321
      DO 323 I=1,NTERMS
      Y=0.5*(xcmax-X)*XI(I)+0.5*(xcmax+X)
      XY=X/Y
      call mrst2001(xy,scale,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
      if(epsc.gt.1.) chm=0.
      gluxy=glu
      fcxy=8.*chm/9.
      del=0.01
      xyu=xy*(1.+0.5*del)
      if(xyu.gt.1.d0) then
      dfcxy=0.d0
      go to 1324
      endif
      call mrst2001(xyu,scale,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
      if(epsc.gt.1.) chm=0.
      fcxyu=8.*chm/9.
      xyl=xy*(1.-0.5*del)
      call mrst2001(xyl,scale,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
      if(epsc.gt.1.) chm=0.
      fcxyl=8.*chm/9.
      dfcxy=(fcxyu-fcxyl)/del
 1324 c0c=cheavy(2,y,epsc)
      p0qg=y**2+(1.-y)**2
      if(epsc.gt.1.d0) c0c=0.d0
      cg21c=2.*facc*cheavy(1,y,epsc)
      cg22c=2.*facc*c0c*dlog(1./epsc)
      clg2c=2.*facc*cheavy(3,y,epsc)
      f1lq=cheavy(4,y,epsc)      

      ffc=ffc+0.5*(xcmax-x)*wi(i)*c0c*(-dfcxy+3.*fcxy)
      ffc=ffc+0.5*(xcmax-x)*wi(i)*al*(cg21c-cg22c)*gluxy
      fflc=fflc+0.5*(xcmax-x)*wi(i)*al*f1lq*fcxy
      fflc=fflc+0.5*(xcmax-x)*wi(i)*al*clg2c*gluxy

      chmg=chmg+.5*(xcmax-X)*WI(I)*AL*(cg21c-cg22c)*gluxy
      chmgsw=chmgsw+.5*(xcmax-X)*WI(I)*AL*(cg21c      )*gluxy
      chmdcg=chmdcg+.5*(xcmax-X)*WI(I)*AL*(cg22c      )*gluxy
      chmlg=chmlg+0.5*(xcmax-x)*wi(i)*al*clg2c*gluxy

      fcextra=coeff2(y,epsc)
      ffc=ffc+0.5*(xcmax-x)*wi(i)*fcxy*fcextra

  323 CONTINUE
  321 CONTINUE

      xbmax=1./(1.+epsb4)
      if(xbmax.le.x) go to 421
      DO 423 I=1,NTERMS
      Y=0.5*(xbmax-X)*XI(I)+0.5*(xbmax+X)
      XY=X/Y
      call mrst2001(xy,scale,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
      if(epsb.gt.1.) bot=0.
      gluxy=glu
      fbxy=2.*bot/9.
      del=0.01
      xyu=xy*(1.+0.5*del)
      if(xyu.gt.1.d0) then
      dfbxy=0.d0
      go to 1424
      endif
      call mrst2001(xyu,scale,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
      if(epsb.gt.1.) bot=0.
      fbxyu=2.*bot/9.
      xyl=xy*(1.-0.5*del)
      call mrst2001(xyl,scale,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
      if(epsb.gt.1.) bot=0.
      fbxyl=2.*bot/9.
      dfbxy=(fbxyu-fbxyl)/del

 1424 c0b=cheavy(2,y,epsb)
      if(epsb.gt.1.d0) c0b=0.d0
      cg21b=2.*facb*cheavy(1,y,epsb)
      cg22b=2.*facb*c0b*dlog(1./epsb)
      clg2b=2.*facb*cheavy(3,y,epsb)
      f1lq=cheavy(4,y,epsb)      

      ffb=ffb+0.5*(xbmax-x)*wi(i)*c0b*(-dfbxy+3.*fbxy)
      ffb=ffb+0.5*(xbmax-x)*wi(i)*al*(cg21b-cg22b)*gluxy
      fflb=fflb+0.5*(xcmax-x)*wi(i)*al*f1lq*fbxy
      fflb=fflb+0.5*(xcmax-x)*wi(i)*al*clg2b*gluxy

      botg=botg+.5*(xbmax-X)*WI(I)*AL*(cg21b-cg22b)*gluxy
      botgsw=botgsw+.5*(xbmax-X)*WI(I)*AL*(cg21b      )*gluxy
      botdcg=botdcg+.5*(xbmax-X)*WI(I)*AL*(cg22b      )*gluxy

      fbextra=coeff2(y,epsb)
      ffb=ffb+0.5*(xbmax-x)*wi(i)*fbxy*fbextra

  423 CONTINUE
  421 CONTINUE

      if(ffc.lt.0.) ffc=0.
      if(ffb.lt.0.) ffb=0.
      f2p=ffp+ffc+ffb
      f2n=ffn+ffc+ffb
      f2c=ffc
      f2b=ffb
      chmq=ffc-chmg
      chmlq=fflc-chmlg
      botq=ffb-botg

      flp=fflp+fflc+fflb
      fln=ffln+fflc+fflb
      flc=fflc
      flb=fflb
      f1p=f2p-flp
      f1n=f2n-fln
      f1c=f2c-flc
      f1b=f2b-flb
      rp=flp/f1p
      rn=fln/f1n

   27 RETURN
      END

      real*8 function coeff2(y,eps)
      implicit real*8(a-h,o-z)
      dimension z1(17),z2(17)
  
      data z1/-0.183E+01,0.400E+01,0.159E+02,-0.357E+02,
     .-0.186E+00,-0.988E+02,0.712E+02,0.631E+02,-0.136E+01,
     .0.175E+03,-0.158E+03,-0.433E+02,0.375E+01,-0.913E+02,
     .0.842E+02,0.107E+02,0.629E+00/
      data z2/-0.204E+01,-0.127E+01,0.117E+01,-0.208E+00,
     .-0.228E+02,0.261E+03,-0.340E+03,0.153E+03,-0.591E+02,
     .-0.199E+04,-0.113E+04,0.299E+04,-0.408E+04,-0.446E+04,
     . 0.307E+05,-0.282E+05,0.144E+02/

      epsl=eps
      eps4=4.d0*eps
      x0=1.d0/(1.d0+eps4)
      yx0=y/x0
      yx01=1.d0-yx0
      epsl2=epsl*epsl
      epsl3=epsl2*epsl
      if(y.lt.0.05) go to 100    
      a0=z1(1)+z1(2)*epsl+z1(3)*epsl2+z1(4)*epsl3
      a1=z1(5)+z1(6)*epsl+z1(7)*epsl2+z1(8)*epsl3
      a2=z1(9)+z1(10)*epsl+z1(11)*epsl2+z1(12)*epsl3
      a3=z1(13)+z1(14)*epsl+z1(15)*epsl2+z1(16)*epsl3
      fac=a0+a1*yx0+a2*yx0*yx0+a3*yx0*yx0*yx0
      coeff2=fac*yx01**z1(17)
      return
  100 continue
      a0=z2(1)+z2(2)*epsl+z2(3)*epsl2+z2(4)*epsl3
      a1=z2(5)+z2(6)*epsl+z2(7)*epsl2+z2(8)*epsl3
      a2=z2(9)+z2(10)*epsl+z2(11)*epsl2+z2(12)*epsl3
      a3=z2(13)+z2(14)*epsl+z2(15)*epsl2+z2(16)*epsl3
      fac=a0+a1*yx0+a2*yx0*yx0+a3*yx0*yx0*yx0
      coeff2=fac*yx01**z2(17)
      return
      end

      real*8 function coeff3(y,eps)
      implicit real*8(a-h,o-z)
      dimension z1(17),z2(17)

      data z1/-0.857E+00,0.217E+02,-0.199E+02,0.162E+02,
     .0.204E+00,-0.142E+03,0.101E+03,-0.575E+02,0.832E+00,
     .0.175E+03,-0.139E+03,0.604E+02,0.344E-01,-0.648E+02,
     .0.538E+02,-0.160E+02,0.000E+00/
      data z2/-0.334E-03,0.677E-02,-0.108E-02,0.825E-03,
     .-0.425E+04,0.883E+05,-0.562E+05,0.601E+05,0.562E+07,
     .-0.122E+09,0.912E+08,-0.928E+08,-0.236E+10,0.499E+11,
     .-0.449E+11,0.412E+11,0.000E+00/

      epsl=eps
      eps4=4.d0*eps
      x0=1.d0/(1.d0+eps)
      yx0=y/x0
      yx01=1.d0-yx0
      epsl2=epsl*epsl
      epsl3=epsl2*epsl
      if(y.lt.0.05) go to 100    
      a0=z1(1)+z1(2)*epsl+z1(3)*epsl2+z1(4)*epsl3
      a1=z1(5)+z1(6)*epsl+z1(7)*epsl2+z1(8)*epsl3
      a2=z1(9)+z1(10)*epsl+z1(11)*epsl2+z1(12)*epsl3
      a3=z1(13)+z1(14)*epsl+z1(15)*epsl2+z1(16)*epsl3
      fac=a0+a1*yx0+a2*yx0*yx0+a3*yx0*yx0*yx0
      coeff3=fac*yx01**z1(17)
      return
  100 continue
      a0=z2(1)+z2(2)*epsl+z2(3)*epsl2+z2(4)*epsl3
      a1=z2(5)+z2(6)*epsl+z2(7)*epsl2+z2(8)*epsl3
      a2=z2(9)+z2(10)*epsl+z2(11)*epsl2+z2(12)*epsl3
      a3=z2(13)+z2(14)*epsl+z2(15)*epsl2+z2(16)*epsl3
      fac=a0+a1*yx0+a2*yx0*yx0+a3*yx0*yx0*yx0
      coeff3=fac*yx01**z2(17)
      return
      end

      DOUBLE PRECISION FUNCTION	ALPHA(T)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/INPUT/alambda,flavor,qsct,qsdt,iord
      DATA PI/3.14159/
      DATA TOL/.0005/
      ITH=0
      TT=T
      qsdtt=qsdt/4.
      qsctt=qsct/4.
      AL=ALAMBDA
      AL2=AL*AL
      FLAV=4.
      QS=AL2*dEXP(T)

      if(qs.lt.0.5d0) then   !!  running stops below 0.5
          qs=0.5d0
          t=dlog(qs/al2)
          tt=t
      endif

      IF(QS.gt.QSCTT) GO	TO 12  
      IF(QS.lt.QSDTT) GO	TO 312  
   11 CONTINUE
      B0=11-2.*FLAV/3. 
      IF(IORD)1,1,2
c     IF(IORD)2,2,2	!TAKE CARE !!
    1 CONTINUE
      ALPHA=4.*PI/B0/T
      RETURN
    2 CONTINUE
      X1=4.*PI/B0
      B1=102.-38.*FLAV/3.
      X2=B1/B0**2
      AS=X1/T*(1.-X2*dLOG(T)/T)
    5 CONTINUE
      F=-T+X1/AS-X2*dLOG(X1/AS+X2)
      FP=-X1/AS**2*(1.-X2/(X1/AS+X2))
      AS2=AS-F/FP
      DEL=ABS(F/FP/AS)
      IF(DEL-TOL)3,3,4
    3 CONTINUE
      ALPHA=AS2
      IF(ITH.EQ.0) RETURN
      GO TO (13,14,15) ITH
    4 CONTINUE
      AS=AS2
      GO TO 5
   12 ITH=1
      T=dLOG(QSCTT/AL2)
      GO TO 11
   13 ALFQC4=ALPHA
      FLAV=5.   
      ITH=2
      GO TO 11
   14 ALFQC5=ALPHA
      ITH=3
      T=TT
      GO TO 11
   15 ALFQS5=ALPHA
      ALFINV=1./ALFQS5+1./ALFQC4-1./ALFQC5
      ALPHA=1./ALFINV
      RETURN

  311 CONTINUE
      B0=11-2.*FLAV/3. 
      IF(IORD)31,31,32
c     IF(IORD)32,32,32	!TAKE CARE !!
   31 CONTINUE
      ALPHA=4.*PI/B0/T
      RETURN
   32 CONTINUE
      X1=4.*PI/B0
      B1=102.-38.*FLAV/3.
      X2=B1/B0**2
      AS=X1/T*(1.-X2*dLOG(T)/T)
   35 CONTINUE
      F=-T+X1/AS-X2*dLOG(X1/AS+X2)
      FP=-X1/AS**2*(1.-X2/(X1/AS+X2))
      AS2=AS-F/FP
      DEL=ABS(F/FP/AS)
      IF(DEL-TOL)33,33,34
   33 CONTINUE
      ALPHA=AS2
      IF(ITH.EQ.0) RETURN
      GO TO (313,314,315) ITH
   34 CONTINUE
      AS=AS2
      GO TO 35
  312 ITH=1
      T=dLOG(QSDTT/AL2)
      GO TO 311
  313 ALFQC4=ALPHA
      FLAV=3.   
      ITH=2
      GO TO 311
  314 ALFQC3=ALPHA
      ITH=3
      T=TT
      GO TO 311
  315 ALFQS3=ALPHA
      ALFINV=1./ALFQS3+1./ALFQC4-1./ALFQC3
      ALPHA=1./ALFINV
      RETURN
      END


      SUBROUTINE WATE96
C*******************************************************************
C*****							       *****
C***** THE X(I)	AND W(I) ARE THE DIRECT	OUTPUT FROM A PROGRAM  *****
C***** USING NAG ROUTINE D01BCF	TO CALCULATE THE	       *****
C***** GAUSS-LEGENDRE WEIGHTS FOR 96 POINT INTEGRATION.	       *****
C***** THEY AGREE TO TYPICALLY 14 DECIMAL PLACES WITH THE      *****
C***** TABLE IN	ABRAMOWITZ & STEGUN, PAGE 919.		       *****
C*****							       *****
C***** ---->   PETER HARRIMAN, APRIL 3RD 1990.		       *****
C*****							       *****
C*******************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION	X(48),W(48)
      COMMON/GAUS96/XI(96),WI(96),XX(97),NTERMS
      NTERMS=96

      X( 1)=   0.01627674484960183561
      X( 2)=   0.04881298513604856015
      X( 3)=   0.08129749546442434360
      X( 4)=   0.11369585011066471632
      X( 5)=   0.14597371465489567682
      X( 6)=   0.17809688236761733390
      X( 7)=   0.21003131046056591064
      X( 8)=   0.24174315616383866556
      X( 9)=   0.27319881259104774468
      X(10)=   0.30436494435449495954
      X(11)=   0.33520852289262397655
      X(12)=   0.36569686147231213885
      X(13)=   0.39579764982890709712
      X(14)=   0.42547898840729897474
      X(15)=   0.45470942216774136446
      X(16)=   0.48345797392059470382
      X(17)=   0.51169417715466604391
      X(18)=   0.53938810832435567233
      X(19)=   0.56651041856139533470
      X(20)=   0.59303236477757022282
      X(21)=   0.61892584012546672523
      X(22)=   0.64416340378496526886
      X(23)=   0.66871831004391424358
      X(24)=   0.69256453664216964528
      X(25)=   0.71567681234896561582
      X(26)=   0.73803064374439816819
      X(27)=   0.75960234117664555964
      X(28)=   0.78036904386743123629
      X(29)=   0.80030874413913884180
      X(30)=   0.81940031073792957139
      X(31)=   0.83762351122818502758
      X(32)=   0.85495903343459936363
      X(33)=   0.87138850590929436968
      X(34)=   0.88689451740241818933
      X(35)=   0.90146063531585023110
      X(36)=   0.91507142312089592706
      X(37)=   0.92771245672230655266
      X(38)=   0.93937033975275308073
      X(39)=   0.95003271778443564022
      X(40)=   0.95968829144874048809
      X(41)=   0.96832682846326217918
      X(42)=   0.97593917458513455843
      X(43)=   0.98251726356301274934
      X(44)=   0.98805412632962202890
      X(45)=   0.99254390032376081654
      X(46)=   0.99598184298720747465
      X(47)=   0.99836437586317963722
      X(48)=   0.99968950388322870559
      W( 1)=   0.03255061449236316962
      W( 2)=   0.03251611871386883307
      W( 3)=   0.03244716371406427668
      W( 4)=   0.03234382256857594104
      W( 5)=   0.03220620479403026124
      W( 6)=   0.03203445623199267876
      W( 7)=   0.03182875889441101874
      W( 8)=   0.03158933077072719007
      W( 9)=   0.03131642559686137819
      W(10)=   0.03101033258631386231
      W(11)=   0.03067137612366917839
      W(12)=   0.03029991542082762553
      W(13)=   0.02989634413632842385
      W(14)=   0.02946108995816795100
      W(15)=   0.02899461415055528410
      W(16)=   0.02849741106508543861
      W(17)=   0.02797000761684838950
      W(18)=   0.02741296272602931385
      W(19)=   0.02682686672559184485
      W(20)=   0.02621234073567250055
      W(21)=   0.02557003600534944960
      W(22)=   0.02490063322248370695
      W(23)=   0.02420484179236479915
      W(24)=   0.02348339908592633665
      W(25)=   0.02273706965832950717
      W(26)=   0.02196664443874448477
      W(27)=   0.02117293989219144572
      W(28)=   0.02035679715433347898
      W(29)=   0.01951908114014518992
      W(30)=   0.01866067962741165898
      W(31)=   0.01778250231604547316
      W(32)=   0.01688547986424539715
      W(33)=   0.01597056290256253144
      W(34)=   0.01503872102699521608
      W(35)=   0.01409094177231515264
      W(36)=   0.01312822956696188190
      W(37)=   0.01215160467108866759
      W(38)=   0.01116210209983888144
      W(39)=   0.01016077053500880978
      W(40)=   0.00914867123078384552
      W(41)=   0.00812687692569928101
      W(42)=   0.00709647079115442616
      W(43)=   0.00605854550423662775
      W(44)=   0.00501420274292825661
      W(45)=   0.00396455433844564804
      W(46)=   0.00291073181793626202
      W(47)=   0.00185396078894924657
      W(48)=   0.00079679206555731759
      DO 1 I=1,48
      XI(I)=-X(49-I)
      WI(I)=W(49-I)
      XI(I+48)=X(I)
      WI(I+48)=W(I)
    1 CONTINUE
      DO 2 I=1,96
    2 XX(I)=0.5*(XI(I)+1.)
      XX(97)=1.0
      EXPON=1.0
      DO 3 I=1,96
      YI=2.*(0.5*(1.+XI(I)))**EXPON-1.
      WI(I)=WI(I)/(1.+XI(I))*(1.+YI)*EXPON
      XI(I)=YI
      XX(I)=0.5*(1.+YI)
    3 CONTINUE
      RETURN
      END

      real*8 function cheavy(i,z,eps)
      implicit real*8(a-h,o-z)

c     this function returns the values of C_g(z,Q^2) 
c     and the deriv. wrt log Q^2. Here eps=m^2/Q^2.
c     If i=1  C_g for F2.  If i=2 deriv of C_g for F2 
c     If i=3  C_g for FL.  If i=4 (1-m2/Q2)*beta*Clq(massless)

      if(i.gt.4) stop
      z1=1.-z
      z2=z*z
      eps2=eps*eps
      beta2=1.-4.*eps*z/z1
      if(beta2.lt.0.) go to 10
      beta=dsqrt(beta2)
      a=z2+z1*z1
      b=4.*z*(1.-3.*z)
      c=-8.*z2
      aa=8.*z*z1-1.
      bb=-4.*z*z1
      arg=(1.+beta)/(1.-beta)
      fac=dlog(arg)
      cf=4./3.
      go to (1,2,3,4) i
    1 cheavy=(a+b*eps+c*eps2)*fac+(aa+bb*eps)*beta
      return
    2 cheavy=(-b*eps-2.*c*eps2)*fac+(a+b*eps+c*eps2)/beta
     .      +(-bb*eps)*beta +(aa+bb*eps)*2.*z*eps/z1/beta
      return
    3 cheavy=-bb*beta+c*eps*fac
      return
    4 cheavy=(1.-eps)*beta*4.*cf*z
      return
   10 print 99
   99 format(1x,'x > x0')
      stop
      end

