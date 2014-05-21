      real*8 function vmag(u)

      implicit none

      real*8 u(3)

      vmag = sqrt(u(1)**2 + u(2)**2 + u(3)**2)

      return
      end

C------------------------------------------------------------------------------

      real*8 function scat_angle(u)

      include 'constants.inc'
      include 'sane.inc'

      real*8 u(3)
      real*8 vmag
      real*8 tht0


* Rotate from calorimeter coordinates to beam coordinates

      tht0 = theta_0*d2r

      scat_angle = acos( (-sin(tht0)*u(2) + cos(tht0)*u(3))/vmag(u) )/d2r

          ! in degrees

      return
      end

C------------------------------------------------------------------------------

      real*8 function momentum(u,part)

      include 'constants.inc'

      real*8 u(3)
      integer*2 part

      real*8 beta,gamma
      real*8 vmag

      beta = vmag(u)/cc
      gamma = 1.d0/sqrt(1.d0-beta**2)
      momentum = gamma*mass(part)*beta

      return
      end
       
