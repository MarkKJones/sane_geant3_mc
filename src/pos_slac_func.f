      real*8 function pos_slac(E,th,ep,radlen)

      implicit none

      real*8 E,th,ep,radlen
      real*8 thrad,anu,q2,wsq,wp

      thrad = th*3.1415/180
      anu = e - ep
      q2 = 4.*e*ep*(sin(thrad/2.))**2
      wsq = .938**2 + 2.*.938*anu - q2

!           use Arie and Peter's paramaratization of e+/e-  data
!           for 2% and 6% radiation lengths

      WP=(SQRT(ABS(WSQ))-.15*E)
      IF(TH.GT.18.) WP=WP+.198*(TH-18.)/8.

      pos_slac =.003*(.5* RADLEN +0.6)/1.7 *10**( (WP-1.5)/0.5 )

C! empirically correct to match predictions for A1n.  
C! I admit that this is guess work.  
C
C
C      if (Ep.lt.1.5d0) then
C        pos_slac = pos_slac*(1-1.235*Ep-0.491*Ep**2+1.5487*Ep**3)*1.23d0
C      else
C        pos_slac = pos_slac*(-2.013*Ep+7.039)
C      endif

      end
