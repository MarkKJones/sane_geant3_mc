*CMZ :          30/08/94  16.21.00  by  S.Ravndal
*-- Author :
      SUBROUTINE GUKINE

      implicit none

      include 'geant.inc'
      include 'beta_geom.inc'
      include 'sane_misc.inc'
      include 'sane_cwn.inc'
      include 'sane.inc'
      include 'constants.inc'
      character*80 arg

C
C Generates Kinematics for primary track
C

      real*4 VERTEX(3),PLAB(3),RNDM(2)
      SAVE VERTEX,PLAB
      DATA VERTEX/3*0./
      DATA PLAB  /3*0./
C

      integer*4 i,j,ik
      real*4 phi,theta
      integer*4 nvert,nt
      real*4 pps
      common/PPP/pps
      integer ivpart
      common/positron/ivpart
      integer ipos

      num_gc = 0
      xsn=0.d0
      ratrad=0.d0
      normrate = 0.d0
      cwn_xsn = 0.d0
      cwn_xsnr = 0.d0
      cwn_xsnscal = 0.d0

      call vzero(gc_x,tot_gc)
      call vzero(gc_y,tot_gc)
      call vzero(gc_eng,tot_gc)
      call vzero(gc_part,tot_gc)

      call vzero(eloss,10)
      call vzero(dEBloc,vert_blocks*horz_blocks*2)
      call vzero(photBloc,vert_blocks*horz_blocks*2)

      photCer  = 0
      photGood = 0
      cwn_cergood = 0

      cwn_p_ng = 0
      cwn_p_ne = 0
      cwn_p_np = 0

      part = particle

      ipos=0
      if (part.eq.5)then
         
         call getarg(3,arg)
         read(arg,'(i1)') ipos
c     ipos=0 -pion ipos=1 elec
      endif
      if(part.gt.20.or.part.le.0)then
c      write(*,*) 'Events: ',numevts,'GenEvent',part
         call getarg(2,arg)
         read(arg,'(i1)') particle
         part=particle
c         part = iipart
c         write(*,*)particle
      endif
c      part = 1+4*rand(0)
c      write(*,*)part
c      if(part.gt.4)part=4
      call gen_evt(ipos)
c      write(*,*)'Event generated',2,numevts,part
     
      numevts = numevts + 1
     
      if (mod(numevts,1000).eq.0) write(*,*) 'Events: ',numevts

 999   continue
      if (part.eq.1) then
        IKINE = 3                 ! electron
c      else if (part.eq.5) then
c        IKINE = 1                 ! photon
      else if (part.eq.2) then
        IKINE = 2                 ! positron
      else if (part.eq.3) then 
        IKINE = 9                 ! pi-
      else if (part.eq.4) then
        IKINE = 8                 ! pi+
      else if (part.eq.5) then
         if(ivpart.eq.1)then
            IKINE = 3           !electron
         else
            IKINE = 7           ! pi0
         endif
      else if (part.eq.6) then
        IKINE = 14                ! p
      else if (part.eq.7) then
        IKINE = 13                ! n
      else if (part.eq.8) then
        IKINE = 11                ! k+
      else if (part.eq.9) then
        IKINE = 12                ! k-
      else
        STOP 'gukine: bad particle ID'
      endif

C use real vertex, not image.  A little wrong, but will do passage of 
C particles through cell better.
c      write(*,*)'GUKINE CHECK 1'
      if (.true.) then
C      if (.false.) then
        VERTEX(1) =  uu(2)         ! x_geant =  y_model
        VERTEX(2) = -uu(1)         ! y_geant = -x_model
        PHI = atan2(-uu(4),uu(5))
        THETA = acos(uu(6)/sqrt(uu(4)**2+uu(5)**2+uu(6)**2))
        PKINE(1) = pp
c        write(*,*)pp
      else                    ! Do test setup
        VERTEX(1) = 0.
        VERTEX(2) =0.
        PHI = 0.
        THETA = 0.
        ph = 0.
        th = 0.
        PKINE(1) = 5.00 
C        PKINE(1) = 5.00 
        EE = sqrt(pkine(1)**2+mass(part)**2)
        pp = pkine(1)
      endif
c      VERTEX(3) = uu(3)-182.5
      VERTEX(3) = uu(3)-237.5
      pps=pp
c      write(*,*)Vertex
c      write(*,*)'GUKINE CHECK 2'

*      write(*,*) '=============================================='
*      write(*,*) 'mom,tht,phi = ',pkine(1),th,ph
*      write(*,*) 'gunkine, uu: ',uu
*      write(*,*) 'gukine: ',theta*180/3.14,phi*180/3.14,pp,pkine(1),th*180/3.14,ph

C
C If the particle number is equal to the particle ID+100 the direction
C cosines are distributed randomly, otherwise the angles theta and phi
C given by the KINE data card a taken.
C
c         write(*,*)ivpart,IKINE,VERTEX(1),VERTEX(2),PKINE(1),PHI,THETA
      IK    = IKINE
C
C Calculating the momementum in the LAB-frame
C
      PLAB(1) = PKINE(1)*SIN(THETA)*COS(PHI)
      PLAB(2) = PKINE(1)*SIN(THETA)*SIN(PHI)
      PLAB(3) = PKINE(1)*COS(THETA)


c      write(*,*)'GUKINE CHECK 3'

C Storing the first vertex and retrieving the actual vertex number NVERT
C int the JVERTX data structure
C
      CALL GSVERT(VERTEX,0,0,0,0,NVERT)
c      write(*,*)'GUKINE CHECK 4'
C
C Store the particle IK with its given momentum in the particle stak
C and attach the primary vertex NVERT to it
C
       CALL GSKINE(PLAB,IK,NVERT,0,0,NT)
c      write(*,*)'GUKINE CHECK 5'
C
C Kinematics debug (controlled by ISWIT(1) )
C
      IF (IDEBUG.EQ.1) THEN
          IF (ISWIT(1).EQ.1) THEN
              CALL GPRINT('VERT',0)
              CALL GPRINT('KINE',0)
          END IF
      END IF
c      write(*,*)'GUKINE CHECK 6'
C
c      write(*,*)'Done with kinem'
      END
	
