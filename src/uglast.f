      SUBROUTINE UGLAST

      implicit none

      include 'sane.inc'
      include 'beta_geom.inc'
      include 'sane_accp.inc'
      include 'sane_misc.inc'
      save

      character*80 arg
      CHARACTER*29 setupfile
C
C Termination routine to print histograms and statistics
C
      integer*2 icycle
C
C      write(*,*) 'Howdy uglast has run'
      CALL GLAST
C
C Save histograms to file
C
C
      
      call hrout(10,icycle,' ')
      call hrend('SANE')

      CLOSE(20)
      CLOSE(30)
      call getarg(1,arg)
      read(arg,'(i3)') iteration
      call getarg(2,arg)
      read(arg,'(i1)') particle

!      write(setupfile, '("history/a1p_geant.",i3,".",i1,".setup")') 
      write(setupfile, '("a1p_geant.",i3,".",i1,".setup")') 
     1      iteration,particle

      open(unit=23,file=setupfile,status='unknown')
      write(*,*) 'e beam = ',E_beam,'ebeam2 ',ebeam2
      write(23,*) E_beam/1000.d0
      write(23,*) theta_0
      write(23,*) theta_Bfield
      write(23,*) theta_min
      write(23,*) theta_max
      write(23,*) phi_min
      write(23,*) phi_max
      write(23,*) p_min
      write(23,*) p_max
      write(23,*) cal_width
      write(23,*) cal_height
      write(23,*) cal_drift
      write(23,*) numevts
      write(23,*) 'mag hits = ',magcnt  ! JDM - 5/07
      write(23,*) 'kept events = ',keptevts  ! NK 02/01/11
      write(*,*) 'mag hits = ',magcnt

      close(23)

      END
