C-- sane_cwn.inc       Glen Warren    8/03
C-- 
C-- Define variables for output ntuple.  Note that all output variables are 
C-- prefixed with 'cwn_'.

C-- Block to track particles entering a volume (currently setup for particles
C-- entering calorimeter.  All variables prefixed with cwn_p_

      integer*2 max_part                       ! max number of particles
      parameter(max_part    = 20)

      integer*4 cwn_p_ng,cwn_p_ne,cwn_p_np     ! number of gammas, e-,e+ 
      real*4 cwn_p_gx,cwn_p_gy,cwn_p_ge        ! gamma x,y and energy
      real*4 cwn_p_ex,cwn_p_ey,cwn_p_ee        ! e- x,y and energy
      real*4 cwn_p_px,cwn_p_py,cwn_p_pe        ! e+ x,y and energy

      COMMON /particles/cwn_p_ng,cwn_p_ne,cwn_p_np,
     1               cwn_p_gx(max_part),cwn_p_gy(max_part),cwn_p_ge(max_part),
     2               cwn_p_ex(max_part),cwn_p_ey(max_part),cwn_p_ee(max_part),
     3               cwn_p_px(max_part),cwn_p_py(max_part),cwn_p_pe(max_part)


C-- Variables related to the thrown particle

      real*4 cwn_E       ! energy of thrown particle
      real*4 cwn_p       ! momentum
      real*4 cwn_xsn     ! cross section
      real*4 cwn_xsnr     ! radiated cross section
      real*4 cwn_xsnscal     ! Oscar pi0 scaling cross section
      real*4 cwn_stopvol    ! volume number where track stopped
      real*4 cwn_th      ! polar angle w.r.t. beam
      real*4 cwn_ph      ! azimuthal angle w.r.t. beam
      real*4 cwn_u       ! array of vertex coordinates
      real*4 cwn_part    ! particle type using my notation, not GEANT
      real*4 cwn_z       ! protons of thrown target nucleus
      real*4 cwn_n       ! neutrons of thrown target nucleus
      real*4 SRx         ! Target x plot in LAB frame
      real*4 SRy         ! Target y plot in LAB frame
      real*4 normrate    ! Rates/yields
      real*4 cwn_dedl          ! Energy loss in volumes following Eloss
CC      real*4 F1tot   ! F1 
CC      real*4 Rtot    ! R=sigma_L/sigma_T 
      

      COMMON /thrown/cwn_E,cwn_p,cwn_xsn,cwn_xsnr,cwn_xsnscal
     1               ,cwn_stopvol,cwn_th,cwn_ph,cwn_u(6),
     1               cwn_part,cwn_z,cwn_n,SRx,SRy,normrate,cwn_dedl(6)
CC     1               cwn_part,cwn_z,cwn_n,SRx,SRy,normrate,F1tot,Rtot


C-- Variables related to the recon particle

      integer*4 nclust
      integer*4 ncell(5)
      real*4 cwn_E_m(5)       ! measured energy of thrown particle
      real*4 cwn_E_r(5)       ! reconstructed energy of thrown particle
      real*4 cwn_th_r(5)      ! polar angle w.r.t. beam
      real*4 cwn_ph_r(5)      ! azimuthal angle w.r.t. beam
      real*4 cwn_x_r(5)       ! corrected x-position 
      real*4 cwn_y_r(5)       ! corrected y-position
      real*4 cwn_z_r(5)       ! corrected z-position
      real*4 cwn_Q2_r(5)      ! corrected Q^2
      real*4 cwn_xb_r(5)      ! corrected x_Bjorken
      real*4 cwn_W_r(5)       ! corrected W
      real*4 cwn_x_ur(5)      ! uncorrected x-position
      real*4 cwn_y_ur(5)      ! uncorrected y-position
      real*4 cwn_z_ur(5)      ! uncorrected z-position
      real*4 cwn_th_ucr(5)    ! uncorrected polar angle w.r.t. beam 
      real*4 cwn_ph_ucr(5)    ! uncorrected azimuthal angle w.r.t. beam
      integer cer_h(5)        ! Cerenkov status

      COMMON /RECON/nclust,ncell,cwn_E_m,cwn_E_r,cwn_th_r,cwn_ph_r,
     1              cwn_x_r,cwn_y_r,cwn_z_r,
     1              cwn_Q2_r,cwn_xb_r,cwn_W_r,
     1              cwn_x_ur,cwn_y_ur,cwn_z_ur,
     1              cwn_th_ucr,cwn_ph_ucr,cer_h



C-- Energy loss and Cerenkovs generated in Pb-Glass, energy loss in various 
C-- detector volumes, Cerenkov photons in gas Cerenkov

      integer*4 max_blocks     ! Maximum number of Pb-Glass blocks changed to I4 OR 10/6
      parameter(max_blocks  = 2000)

      integer*4 cwn_nb         ! number of blocks in which energy was deposited
      integer*4 cwn_bx         ! x index of block that fired
      integer*4 cwn_by         ! y index of block that fired
      integer*4 cwn_bg         ! number of Cernkov photons in block that fired
      real*4 cwn_be            ! energy deposited in block that fired

      COMMON /blocks/cwn_nb,cwn_bx(max_blocks),
     1               cwn_by(max_blocks),cwn_bg(max_blocks),cwn_be(max_blocks)


      
C-- List of good Cerenkov photons in the gas Cerenkov as determined by the 
C-- ltrace code.  Do not confuse these arrays with the cg_ defined earlier.

      integer*2 max_cer          ! maximum number of Cerenkov photons 
      parameter(max_cer = 1000)
      
      integer*4 cwn_cerphot    ! number of Cerenkov photons in gas Cerenkov
      integer*4 cwn_cergood      ! number of good photons
      real*4 cwn_cg_x            ! x position of good Cerenkov photons
      real*4 cwn_cg_y            ! y position
      real*4 cwn_cg_xp           ! x "slope" (or tangent or angle?)
      real*4 cwn_cg_yp           ! y "slope"

      COMMON /cerenkov/cwn_cergood,cwn_cg_x(max_cer),cwn_cg_y(max_cer),
     1               cwn_cg_xp(max_cer),cwn_cg_yp(max_cer)

c
c ihodoLuc           - hit Lucite number
c ihodoHit           - Hit nuber in hodoscope 
c pHodo(ihodoHit)    - Momentum in Hodoscope
c cxHodo(ihodoHit)   - X normal vector
c cyHodo(ihodoHit)   - Y normal vector
c czHodo(ihodoHit)   - Z normal vector
c xHodo(ihodoHit)    - X coordinate 
c yHodo(ihodoHit)    - Y coordinate
c zHodo(ihodoHit)    - Z coordinate
c eLosHodo(ihodoHit) - Energy lost in Hodoscope
c tdcHLeft(max_part),tdcHRigh(max_part),
c adcHLeft(max_part),adcHRigh(max_part) -digitalization for right and left PMT
c

	integer*4  ihodoHit, ihodoLuc(20)  
	real*4 pHodo(20),cxHodo(20),cyHodo(20),czHodo(20)
	real*4 xHodo(20),yHodo(20),zHodo(20)
	real*4 timeHodo(20), eLosHodo(20)
	real*4 tdcHLeft(20),tdcHRigh(20),adcHLeft(20),adcHRigh(20)

	COMMON/HODO/ihodoHit, ihodoLuc,
     ,	 pHodo, cxHodo, cyHodo, czHodo,
     ,	 xHodo, yHodo, zHodo,timeHodo,
     ,	 eLosHodo, tdcHLeft, tdcHRigh,
     ,	 adcHLeft, adcHRigh

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer*4 itrackY1Hit,itrackY1Bar(20)
      real*4 y1Track(20),zY1Track(20)
      real*4 timeY1Track(20), tdcY1Track(20)
      COMMON /TRACKY1/itrackY1Hit,itrackY1Bar,y1Track,
     ,	     zY1Track,timeY1Track, tdcY1Track

      integer itrackY2Hit,itrackY2Bar(20)
      real*4 y2Track(20),zY2Track(20)
      real*4 timeY2Track(20), tdcY2Track(20)
      COMMON /TRACKY2/itrackY2Hit,itrackY2Bar,y2Track,
     ,	     zY2Track,timeY2Track, tdcY2Track

      integer itrackX1Hit,itrackX1Bar(20)
      real*4 x1Track(20),zX1Track(20),ptrack(20)
      real*4 timeX1Track(20), tdcX1Track(20)
      COMMON /TRACKX1/itrackX1Hit,itrackX1Bar,x1Track,
     ,	     zX1Track,ptrack,timeX1Track, tdcX1Track


      integer ipartc
      real*4 Ec,Xvr,Yvr
      integer nnc,nncA
      real*4 Emaxc,Etotc,Etot9c
      integer ixmaxc, iymaxc
      integer iE3pp,iE3pm,ie3mp,ie3mm

      common/ENCOR/ipartc,Ec, Xvr,Yvr,
     ,   nnca,nnc,Etotc,Etot9c,Emaxc,
     ,   ixmaxc,iymaxc,iE3pp,iE3pm,ie3mp,ie3mm

      integer ipartc1
      real*4 Ec1,Xvr1,Yvr1
      integer ixmaxc1, iymaxc1
      real*4 Eyxc(5,5)

      common/ENCOR1/ipartc1,Ec1, Xvr1,Yvr1,
     ,     ixmaxc1,iymaxc1,Eyxc
