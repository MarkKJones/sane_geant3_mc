C-- sane_misc.inc       Glen Warren    8/03
C-- 
C-- Miscellaneous variables used internally and not part of the beam line 
C-- simulation package.  Some of these will be copied to other variables 
C-- to be outputed to an ntuple.


C-- Define some volume numbers for certain elements.  BE CAREFUL IF YOU CHANGE
C-- THE GEOMETRY AS THIS MAY CHANGE THE VOLUME NUMBER.  This could be done
C-- more intelligently.

      integer*2 vol_luc,vol_hdiv
      integer*2 vol_pcal,vol_rcal,vol_pcol,vol_pblc,vol_rcol,vol_rblc
      integer*2 vol_gain
      integer*2 vol_ecal
      integer*2 vol_veto
      integer*2 vol_cgas
      integer*2 vol_magn        ! JDM
      integer*2 vol_fty1        ! JDM
      integer*2 vol_ftx1        ! JDM
      integer*2 vol_fty2        ! JDM
      integer*2 vol_fy1b        ! JDM
      integer*2 vol_fx1b        ! JDM
      integer*2 vol_fy2b        ! JDM

      COMMON /volnumber/vol_luc,vol_hdiv,vol_pcal,vol_rcal
     > ,vol_pcol,vol_pblc,vol_rcol,vol_rblc,vol_ftx1,vol_fty1,vol_fty2
     > ,vol_gain,vol_ecal,vol_veto,vol_cgas,vol_magn
     > ,vol_fy1b,vol_fx1b,vol_fy2b
C-- miscellaneous variables related to SANE physics geant that are not
C-- part of the beam line simulations package.

      real*4 EE,pp,xsn,ratrad,xsn_epc,xsn_wiser,uu(6),vv(6),th,ph,asym
      real rate
      integer*4 part,zz_t,nn_t,tgt_num,tgt_num1

      COMMON /UEVENT/EE,pp,part,zz_t,nn_t,xsn,xsn_epc,xsn_wiser
     >,ratrad,uu,vv,th,ph,asym,tgt_num
      COMMON /URATE/rate
      COMMON/TARGET_NUM/tgt_num1

      

C-- array of e-,e+, gammas that enter or leave a volume.  Array originally 
C-- developed for looking at particles leaving the gas Cerenkov, hence the gc_.
C-- Currently is looking at particles leaving the gain sheet.  These are used 
C-- to fill the ntuple in the PARTICLES block, which are particles that enter
C-- the calorimeter.

      integer tot_gc            ! max number of particles to track per event
      parameter(tot_gc=20)

      integer num_gc            ! actual number of events
      integer gc_part(tot_gc)   ! particle type
      real    gc_x(tot_gc)      ! particle x position
      real    gc_y(tot_gc)      ! particle y position
      real    gc_eng(tot_gc)    ! particle energy

      COMMON /GC/gc_eng,gc_x,gc_y,gc_part,num_gc


      integer tot_ftx1            ! max number of particles to track per event
      parameter(tot_ftx1=20)

      integer num_ftx1            ! actual number of events
      integer ftx1_part(tot_ftx1)   ! particle type
      real    ftx1_x(tot_ftx1)      ! particle x position
      real    ftx1_y(tot_ftx1)      ! particle y position
      real    ftx1_eng(tot_ftx1)    ! particle energy

      COMMON /FTX1/ftx1_eng,ftx1_x,ftx1_y,ftx1_part,num_ftx1

      integer tot_fty2            ! max number of particles to track per event
      parameter(tot_fty2=20)

      integer num_fty2            ! actual number of events
      integer fty2_part(tot_fty2)   ! particle type
      real    fty2_x(tot_fty2)      ! particle x position
      real    fty2_y(tot_fty2)      ! particle y position
      real    fty2_eng(tot_fty2)    ! particle energy

      COMMON /FTY2/fty2_eng,fty2_x,fty2_y,fty2_part,num_fty2

C-- General

      integer numevts
      integer magcnt      ! Counting hits killed due to hitting magnet - JDM
      integer keptevts    ! Counting kept events with real kinematics NK 02-01-11                             
      COMMON /MINE_I/numevts,magcnt,keptevts  ! magcnt added 5/18/07 -JDM

      integer*4 nt_geant,nt_pmc
      data nt_geant,nt_pmc/10,15/

      integer*4 iteration         ! iteration number of simulation run
      integer*4 particle

      common /runinfo/iteration,particle

C-- Target related

      integer*2 num_tgts
      parameter(num_tgts = 5)

      integer*2 z(num_tgts),n(num_tgts)
c      integer*2 z(3),n(3)

      data z/1,2,7,13,6/
C--      data n/0,2,14,8/
C-- Change to define as 14N
      data n/0,2,7,14,6/
 
c      COMMON /TGTSHT/z,n,num_tgts


C-- Energy loss and Cerenkov photons generated in Pb-Glass and
C-- photons generated in gas Cerenkov.
	integer vert_block,horz_block
       parameter (vert_block=56)
       parameter (horz_block=32)
      real*8 dEBloc(vert_block,horz_block)  ! energy loss in Pb-Glass blocks
      integer*4 photBloc(vert_block,horz_block) ! photons in Pb-Glass blocks

      integer*4 HitFrontY1(133)  ! Bar hits in first front tracker plane -JDM 
      integer*4 HitFrontX1(73)  ! Bar hits in third front tracker plane-6/19/07
      integer*4 HitFrontY2(133)  ! Bar hits in second front tracker plane 
      real*8    LocFrontY1      ! Z Location of first front tracker plane
      real*8    LocFrontX1      ! Z Location of third front tracker plane
      real*8    LocFrontY2      ! Z Location of second front tracker plane   -JDM
      integer*4 HitLucHodo(28)  ! Bar hit in Lucite hodoscope - JDM -6/20/07
C      real*8    XLucHodo        ! X position of Lucite hit
C      real*8    LocLucHodo      ! Z location of lucite hodoscope 
      
      
      integer*4 photGood                      ! photons in gas Cerenkov
      real*8 ELoss(10)                        ! energy loss in various volumes
            ! (1) = cerenkov gas
            ! (2) = hodoscope
            ! (3) = gain sheet
            ! (4) = Pb-glass
            ! (5) = pion veto wall 
            ! (6)  = all others                                 
            ! (7) = front hodoscope - JDM
      COMMON /MINE_RR/dEBloc,ELoss,LocFrontY1,LocFrontX1,LocFrontY2
      COMMON /MINE_II/photBloc,photGood,HitFrontY1,HitFrontX1,HitFrontY2
     +  ,HitLucHodo
