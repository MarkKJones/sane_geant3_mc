*CMZ :  3.21/02 29/03/94  15.41.33  by  S.Giani
*-- Author :
      PROGRAM GXINT
*
*     GEANT main program. To link with the MOTIF user interface
*     the routine GPAWPP(NWGEAN,NWPAW) should be called, whereas
*     the routine GPAW(NWGEAN,NWPAW) gives access to the basic
*     graphics version.
*
      PARAMETER (NWGEAN=30000000,NWPAW=10000000)
      COMMON/GCBANK/GEANT(NWGEAN)
      COMMON/PAWC/PAW(NWPAW)
      include 'beta_geom.inc'
      include 'geant.inc'
      include 'sane.inc'
      include 'sane_misc.inc'
      include 'materials.inc'

* Uncomment the following line if your operating system is an HP
*      ON REAL UNDERFLOW IGNORE
*
      magcnt=0
     
       CALL GPAW(NWGEAN,NWPAW)
*      CALL GPAWPP(NWGEAN,NWPAW)
*
      write(*,*) 'done with gpaw', magcnt
      END

      SUBROUTINE QNEXT
      END
      SUBROUTINE CZOPEN
      END
      SUBROUTINE CZTCP
      END
      SUBROUTINE CZCLOS
      END
      SUBROUTINE CZPUTA
      END
