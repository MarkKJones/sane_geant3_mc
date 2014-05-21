C-- sane.inc           Glen Warren     8/03
C--
C-- miscellaneous variables related to SANE/BETA setup in Hall C

      real*8 omega          ! angle used in target field code
      real*8 theta_0        ! scattering angle of BETA, +ve is counterclockwise
      real*8 theta_Bfield   ! field angle, +ve is clockwise

      integer*4 photCer     ! number of cerenkov photons in gas Cerenkov

      integer*4 target_type ! =0, UVa tgt 
                            ! =1, solid carbon tgt & no magnet
      integer*4 field_type  ! =0, field code is bypassed
                            ! =1, field code is used
	
      real*8 E_beam,ebeam2
      real*8 bigcal_block_cut(1792),bigcal_block_cut_check(1792)

      COMMON /ANGLES/omega,theta_0,theta_Bfield,ebeam2,e_beam
      COMMON /CERENKOVPHOTONS/photCer
      COMMON /TYPES/target_type,field_type
!      COMMON /EB/E_beam

C-- Kinematics - goofy, better to use the KINE command, but this is it.
C-- Used only for physics simulation, not beamline simulation

!      real*8 E_beam

*      parameter (E_beam       = 4800.0d0)
!      parameter (E_beam       = 6000.0d0)	! must be read in from file OR 11/06           

!       COMMON /EB/E_beam
    

