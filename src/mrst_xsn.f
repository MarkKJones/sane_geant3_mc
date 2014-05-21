
cc      real*4 function mrst_xsn(Z,A,E,Ep,theta,f1,f2)
c      real*4 function mrst_xsn(Z,A,E,Ep,theta,f1,f2,q2,w2)
      real*4 function mrst_xsn(Z,A,E,Ep,theta,f1,f2,q2,w2,r)

      implicit none

      include 'sane.inc'

      real*8 E,x,theta,Mp
      real*8 Ep,Q2
      real*8 F1p,F2p,F1n,F2n
      real*8 F1p_2x,F1n_2x
      real*8 F1pIN,F2pIN,F1pQE,F2pQE

      parameter(Mp=0.9384d0)

      real*8 d2r
      parameter(d2r = 0.0174533d0)
      real*8 w1p,w2p,sigmot
      real*8 W2,r
      real*8 sigmatot 
      real*8 f1,f2

      integer*4 Z,A
      real*8  ZZZ,AAA
      ZZZ=Z
      AAA=A

      Q2 = 4.d0*E*Ep*(sin(theta/2.d0))**2
      x = Q2/2.d0/Mp/(E-Ep)
      W2 = 2.d0*Mp*(E-Ep) + Mp**2 - Q2

CCCC Include elastic for Hovik
c$$$      if (
c$$$c     ,     Q2.lt.1.25 .or. 
c$$$     ,     x.gt.1.0) then
c$$$         mrst_xsn = -1.d0
c$$$      else
        
!!! Switch to using F1F209 instead of MRST2001 NK 01/27/10       
c        call mrst_sub(Q2,x,F1p_2x,F2p,F1n_2x,F2n)
c        F1p = F1p_2x/2/x
c        F1n = F1n_2x/2/x

c      if (target_type.EQ.0) then    ! define polarized target
c         Z=1
c         A=1
c      elseif (target_type.EQ.1) then          ! define standard carbon target
c         Z=6
c         A=12
c      endif


c        call F1F2IN09(Z, A, Q2/1.d0, W2/1.d0, F1pIN, F2pIN, r) 
c        call F1F2QE09(Z, A, Q2/1.d0, W2/1.d0, F1pQE, F2pQE ) 

c        F1p = F1pIN + F1pQE
c        F2p = F2pIN + F2pQE


c        w1p = F1p/ Mp
c        w2p = F2p/(E-Ep)

c cross section comes out in fm2/GeV/sr, convert that to nb/GeV/sr

c        mrst_xsn = sigmot(E*1.d3,theta)*(2*w1p*tan(theta/2)**2 + w2p)*1.D+7

      call CROSS_TOT(ZZZ,AAA,E,Ep,theta,sigmatot,f1,f2,q2,w2,r)
      mrst_xsn = sigmatot
CCCC Include elastic for Hovik
c$$$      endif

      return
      end

