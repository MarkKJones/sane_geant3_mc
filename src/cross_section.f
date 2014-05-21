c      real*8 function cross_section(zz,nn,u_vertex,part,f1,f2)
c      real*8 function cross_section(zz,nn,u_vertex,part,f1,f2,q2,wsq,r)
      real*8 function cross_section(zz,nn,u_vertex,part,f1,f2,q2,wsq,r)

      implicit none

      include 'constants.inc'
      include 'sane.inc'

      real*8 u_vertex(6)
      integer*2 part,tgt
      integer*4 zz,nn,aa
      real*8 mom,tht
      real*8 scat_angle,momentum
      real*8 vel(3)

      real*8 pos_slac
      real*8 mrst_xsn
      real*8 wiser_func
      real*8 epc_func

      real*8 f1,f2,r,q2,wsq

      character*1 fermi,mpi


      vel(1) = u_vertex(4)
      vel(2) = u_vertex(5)
      vel(3) = u_vertex(6)

      mom = momentum(vel,part)*1000.d0  ! in MeV
      tht = scat_angle(vel)             ! in degrees

      aa = zz + nn

      if (part.eq.1) then
CC F1F209 returns the F1 and F2 per nucleus(not per nucleon); do not need to multiply by aa.
c        cross_section = aa*mrst_xsn(zz,aa,E_beam/1000.d0,
        cross_section = mrst_xsn(zz,aa,E_beam/1000.d0,        
cc     ,        mom/1000.d0,tht*d2r,f1,f2)
c     ,        mom/1000.d0,tht*d2r,f1,f2,q2,wsq)
     ,        mom/1000.d0,tht*d2r,f1,f2,q2,wsq,r)
      elseif (part.eq.2) then
        cross_section = pos_slac(E_beam/1000.d0,tht,mom/1000.d0,5.4d0) *
     1        aa*mrst_xsn(zz,aa,E_beam/1000.d0,mom/1000.d0,tht*d2r,f1,f2)
      elseif (part.ge.3 .and. part.le.5) then
        cross_section = aa*wiser_func(part_id(part),E_beam,mom,tht)
      elseif (part.ge.6 .and. part.le.7) then 
         mpi = 'n'
         if (mom.gt.500.d0) mpi = 'Y'
         if (zz+aa.gt.2) then
            fermi = 'Y'
         else
            fermi = 'N'
         endif

         cross_section = epc_func(E_beam,float(zz),nn,fermi,mpi,
     1        part_id(part),mom,tht)
      else
!     cross_section = 1.d0
        cross_section = -1.d0	! To be consistent with mrst-xsn  OR 3/15/10
      endif

!      if(cross_section.le.0.d0) cross_section = 1.d0 ! OR 3/15/10

      return
      end








