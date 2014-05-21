*CMZ :          29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GUTREV
      include 'beta_geom.inc'
      include 'geant.inc'
      include 'sane.inc'
      include 'sane_misc.inc'
      include 'materials.inc'
      include 'sane_cwn.inc' 
C.
C.    *
C.    *       User routine to control tracking of one event
C.    *
C.    *       Called by GRUN
C.    *
C.
c      write(*,*)'Start GUTREV',ee
      CALL GTREVE
c      write(*,*)'Stop GUTREV'
C
      END
