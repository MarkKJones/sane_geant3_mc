      REAL*8 FUNCTION SIGMOT(E,THR)

      implicit none


      real*8 E,thr
      real*8 alph,hbarc


      ALPH=1./137.03604
      HBARC=197.3286

      SIGMOT=(ALPH*HBARC*COS(THR/2.)/2./E/SIN(THR/2.)**2)**2

      RETURN
      END

