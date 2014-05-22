C-- sane_accp.inc      Glen Warren   8/03
C--
C-- define acceptance over which to throw events.  Uncomment the desired setup.


C-- define some target dimensions related to throwing events

      real*8 cell_length
      real*8 raster_radius 
      real*8 beam_current

*      parameter(cell_length=3.0d0)
      parameter(cell_length=4.0d0)      !To account for LHe in outer edges
      parameter(raster_radius=1.0d0) 
      parameter(beam_current=0.100d0)   !100 nA

      real*4 raster_xoff,raster_yoff

      COMMON/SRAST/raster_xoff,raster_yoff 

*** Acceptance Parameters

      real*8 p_min         ! minimum momentum to throw
      real*8 p_max         ! maximum momentum
      real*8 theta_min     ! minimum scattering angle
      real*8 theta_max     ! maximum scattering angle
      real*8 phi_min       ! minimum azimuthal angle
      real*8 phi_max       ! maximum azimuthal angle


C-- For theta_Bfield = -80
*      parameter (p_min     =   0.18d0)
*      parameter (p_max     =   4.5d0)

cPI 0 setting
c      parameter (theta_min =  20.00d0 )
c      parameter (theta_max =  60.00d0 )
c      parameter (phi_min   = 20.0d0 )
c      parameter (phi_max   = 160.d0 )

ccc   Electron setting
c      parameter (theta_min =  25.00d0 )
c      parameter (theta_max =  60.00d0 )
c      parameter (phi_min   = 20.0d0 )
c      parameter (phi_max   = 140.d0 )
ccmkj
      parameter (theta_min =  30.00d0 )
      parameter (theta_max =  45.00d0 )
      parameter (phi_min   = 60.0d0 )
      parameter (phi_max   = 100.d0 )


C-- For theta_Bfield = -180
C--      parameter (p_min     =   0.d0)
C--      parameter (p_max     =   3.5d0)
      parameter (p_min     =   0.5d0)
      parameter (p_max     =   3.5d0)
c      parameter (phi_min   = 40.0d0 )
c      parameter (phi_max   = 160.d0 )
c      parameter (theta_min   = 25.0d0 )
c      parameter (theta_max   = 60.d0 )
C-- Hovik's numbers
*      parameter (p_min     =   0.4d0)
*      parameter (p_max     =   4.5d0)
*      parameter (phi_min   = 90.0d0 )
*      parameter (phi_max   = 91.d0 )
*      parameter (theta_min   = 20.0d0 )
*      parameter (theta_max   = 55.d0 )

C-- Point setup
*      parameter (p_min     =   0.49d0)
*      parameter (p_max     =   0.51d0)
*      parameter (phi_min   = 89.9d0 )
*      parameter (phi_max   = 90.1d0 )
*      parameter (theta_min   = theta_0-5.01d0)
*      parameter (theta_max   = theta_0+5.01d0 )

C-- wide acceptance
*      parameter (p_min     =   0.10d0)
*      parameter (p_max     =   2.5d0)
*      parameter (theta_min =  theta_0-40.00d0 )
*      parameter (theta_max =  theta_0+40.00d0 )
*      parameter (phi_min   = 0.0d0 )
*      parameter (phi_max   = 360.d0 )

C-- 4 pi low energy
* set theta_Bfield near -80.0
!      parameter (p_min     =   0.01d0)
!      parameter (p_max     =   0.2d0)
!      parameter (theta_min =   0.00d0 )
!      parameter (theta_max = 180.00d0 )
!      parameter (phi_min   =   0.0d0 )
!      parameter (phi_max   = 360.d0 )

