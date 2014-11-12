c By Jixie: this routine return the cross section for the whole
c nucleus, use mrst if x<0.3 otherwise use Bosted's model

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

C By Jixie @ Nov. 6, 2014
C According to a figure of XS vs x_bj provided by Whit,
C http://quarks.temple.edu/~whit/SANE/sane_meetings/10-29-2014/F2pCompare.png
C P. Bosted's Model fail to describe data if x<0.3
C Here I only use Bosted's model if x>=0.3

      if (x .lt. 0.3) then
!!!Use MRST2001 if x<0.3 

        call mrst_sub(Q2,x,F1p_2x,F2p,F1n_2x,F2n)
        F1p = F1p_2x/2/x
        F1n = F1n_2x/2/x
        w1p = F1p/ Mp
        w2p = F2p/(E-Ep)

c cross section comes out in fm2/GeV/sr, convert that to nb/GeV/sr
c also multiply by atomic number to get the xs for the whole nucleus
        mrst_xsn = AAA*sigmot(E*1.d3,theta)*(2*w1p*tan(theta/2)**2 + w2p)*1.D+7

      else
!!!Use Bosted's model  if x>=0.3
c    CROSS_TOT return xs in nb/GeV/sr for the whole nucleon
        call CROSS_TOT(ZZZ,AAA,E,Ep,theta,sigmatot,f1,f2,q2,w2,r)
        mrst_xsn = sigmatot
      endif

      return
      end

