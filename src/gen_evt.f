* simulation of a1p experiment
*
! Comment added By Jixie: pos=0 -pion ipos=1 elec
! ipos will be passed from caller
      subroutine gen_evt(ipos)

      implicit none

      include 'constants.inc'
      include 'beta_geom.inc'
      include 'sane_misc.inc'
      include 'sane.inc'
      include 'sane_cwn.inc'
      include 'sane_accp.inc'
      include 'materials.inc'

      real*8 u(6),u_vertex(6)

      integer*2 k,imat,ikin
      integer*2 tgt
      logical ok
      real*8 E,Ef,temp_ran
      logical*1 check_accp
      real*8 tht_scat,phi_scat,deltaomega_scat,deltaE_scat
      real*8 cross_section,calc_asym,get_xsnscal
      real*8 f1,f2,q2,wsq,nu,r,xb
      real*4 rate_norm(4)
      real*4 q2ce,beame,eplo,epce,ephi,xblo,xbce,xbhi,wlo,wce,whi
      real*4 el_p0
      integer flg,ivpart

      data flg /1/
      real*4 q2orig,worig
      common /orig_kin/ q2orig,worig
      
      real*8 w2,w1,sin2,cos2,rl,th_deg,sigpip,sigpim
      real sigelec,sigelech2,ratepi,e0

      real*8 rrand,rr
      integer ipos
      real*8 stots,sdelta,spion
      real*8 epc_func,z1,n1,ppp
      real*8 pionwiser,piondelta,epc_xn
      common/PIONDELTA/pionwiser,piondelta,epc_xn
      real p1,p2,p3,th_d,radh2,xsnh2

C Now cycle through events
      if(flg.eq.1) then
      write(*,*) 'E_beam',E_beam,'ebeam2 = ',ebeam2
      E_beam =  ebeam2
      write(*,*) 'E_beam',E_beam,'ebeam2 = ',ebeam2
      flg = 0
      endif
      
 700  continue


      if(target_type.eq.1)then
      tgt=5
      else
      temp_ran = rand()
      if ( temp_ran .le. .25) tgt=1
      if ( temp_ran .gt. .25 .and. temp_ran .le. .50) tgt=2
      if ( temp_ran .gt. .50 .and. temp_ran .le. .75) tgt=3
      if ( temp_ran .gt. .75) tgt=4
      endif  

      zz_t = z(tgt)
      nn_t = n(tgt)
      tgt_num = tgt
      tgt_num1 = tgt
      xsn=0
c      write(*,*)tgt,z(tgt),n(tgt)

! Comment added By Jixie: this part will generate eertex for 
! electron,(part==5 and ipos==1) pi0 (part==5 and ipos==0), 
! or other particles.

      if(part.eq.5)then
         el_p0=0 !rand()
         e0=E_beam/1000.
c         write(*,*)ipos
         if(ipos.eq.1)then
! Comment added By Jixie: this part is for electron
            ivpart=1
            call generate_event(u_vertex,E,ivpart,tht_scat,phi_scat
     +           ,deltaomega_scat,deltaE_scat)
            Ef = abs(E)
            nu = E0 - Ef
            q2 = 2.d0*Ef*E0*(1-cos(tht_scat))
!            write(*,*)  'e-: ',Ef,E0,tht_scat*180/3.1415

            xb = q2 / 2.d0 / 0.938d0 / nu 
            wsq = 0.938d0**2 + 2.d0*0.938d0*nu - q2
            CALL INEFT(Q2,sqrt(wsq),W1,W2,14.D0)
            SIN2 = SIN(tht_scat/2)**2
            COS2 = 1. - SIN2
            SIGELEC = 5.18 / (E0)**2 / SIN2**2 *
     >        (COS2 * W2 + 2. * SIN2 * W1)
     >        * 2.*3.14159 * sin(tht_scat)
            pp = sqrt(E**2-mass(ivpart)**2)
            th = tht_scat
            ph = phi_scat
            EE = abs(E)
! By Jixie: This is a tricky way to check NAN
            if(SIGELEC.ne.SIGELEC)SIGELEC=0
! By Jixie:  cross_section return xs in unit of nb/GeV/sr
            xsn = cross_section(7,7,u_vertex,1,f1,f2,q2,wsq,r)
c            write(*,*)'cros ', xsn/14.,SIGELEC,f1/14.,f2/14.,W1,W2
            SIGELEC = 5.18 / (E0)**2 / SIN2**2 *
     >        (COS2 * f2/14. + 2. * SIN2 * f1/14)
     >        * 2.*3.14159 * sin(tht_scat)

            xsnh2 = cross_section(1,0,u_vertex,1,f1,f2,q2,wsq,r)
            SIGELECh2 = 5.18 / (E0)**2 / SIN2**2 *
     >        (COS2 * f2 + 2. * SIN2 * f1)
     >        * 2.*3.14159 * sin(tht_scat)

c            write(*,*)Ef,th_d,SIGELEC,SIGELECh2
            ratrad=0
            if(e0.lt.5)then
               th_d=tht_scat*180./3.14159
               call radiated_xn_N_47(wsq,q2,ratrad)
               call radiated_xn_h2_47(wsq,q2,radh2)
               ratrad=3/17.*radh2+14/17.*ratrad
c               write(*,*)Ef,th_d,ratrad,radh2
            else 
               call radiated_xn_N_59(wsq,q2,ratrad)
               call radiated_xn_h2_59(wsq,q2,radh2)
               ratrad=3/17.*radh2+14/17.*ratrad
            endif


            xsn=SIGELEC*14/17.+3/17.*SIGELECh2
c               write(*,*)Ef,th_d,SIGELEC,SIGELECh2,xsn

c            if(SIGELEC.ne.SIGELEC)goto 898
c            write(*,*)'elec',E0,E,tht_scat*180./3.14159,phi_scat-90,W1,W2,SIGELEC 
         else
! Comment added By Jixie: this part is for pi0
            ivpart=5
            call generate_event(u_vertex,E,ivpart,tht_scat,phi_scat
     +           ,deltaomega_scat,deltaE_scat)
            rl = (0.0464/2.) * 100.
            th_deg=tht_scat*180./3.14159
c
c     PETERS WISER CODE
            CALL WISER_ALL_SIG (E_beam,E*1000,th_deg,rl,1,SIGPIP)
c            write(*,*)SIGPIP
            CALL WISER_ALL_SIG (E_beam,E*1000.,th_deg,rl,2,SIGPIM)
c            write(*,*)SIGPIM,(sigpip + sigpim)/2.

            ratepi = (sigpip + sigpim)/2. * 2.*3.14159 * sin(tht_scat) 
            pp = sqrt(E**2-mass(ivpart)**2)
            ppp=pp*1000

            z1=1
            n1=0
            xsn=0
!  By Jixie: epc now returns xs in unit of nb/(GeV/c)/sr
c xsn is  pi0 xsn with Wiser's fit 
            stots=epc_func(E_BEAM,z1,0,'N','Y','PI0','N',Ppp,TH_deg,rl,'N')
            xsn=epc_xn*0
c            write(*,*) ' scal N  xsn = ',epc_xn*(zz_t+nn_t)
            stots=epc_func(E_BEAM,z1,0,'N','Y','PI0','Y',Ppp,TH_deg,rl,'N')
            xsn=(xsn+epc_xn)* 2.*3.14159 *sin(tht_scat)*(zz_t+nn_t)
c            write(*,*) ' scal Y  xsn = ',epc_xn*(zz_t+nn_t)
c xsnscal is pi0 xsn with Oscar's scaling fit
            stots=epc_func(E_BEAM,z1,0,'N','Y','PI0','N',Ppp,TH_deg,rl,'Y')
            xsnscal=epc_xn*0
c            write(*,*) 'OR  scal N  xsn = ',epc_xn*(zz_t+nn_t)
            stots=epc_func(E_BEAM,z1,0,'N','Y','PI0','Y',Ppp,TH_deg,rl,'Y')
            xsnscal=(xsnscal+epc_xn)* 2.*3.14159 *sin(tht_scat)*(zz_t+nn_t)
c            write(*,*) 'OR  scal Y  xsn = ',epc_xn*(zz_t+nn_t)

            pp = sqrt(E**2-mass(ivpart)**2)
            th = tht_scat
            ph = phi_scat
            EE = abs(E)
            
c
c
c     Electron comp
c
c
c

            Ef = abs(E)
            nu = E0 - Ef
            q2 = 2.d0*Ef*E0*(1-cos(tht_scat)   )
!            write(*,*) 'pi0: ',Ef,E0,tht_scat*180/3.1415

            xb = q2 / 2.d0 / 0.938d0 / nu 
            wsq = 0.938d0**2 + 2.d0*0.938d0*nu - q2
            SIN2 = SIN(tht_scat/2.)**2
            COS2 = 1. - SIN2

            xsnh2 = cross_section(1,0,u_vertex,1,f1,f2,q2,wsq,r)
            SIGELECh2 = 5.18 / (E0)**2 / SIN2**2 *
     >        (COS2 * f2 + 2. * SIN2 * f1)
     >        * 2.*3.14159 * sin(tht_scat)
            CALL INEFT(Q2,sqrt(wsq),W1,W2,14.D0)
           SIGELEC = 5.18 / (E0)**2 / SIN2**2 *
     >        (COS2 * W2 + 2. * SIN2 * W1)
     >        * 2.*3.14159 * sin(tht_scat)
        endif

 338     goto 339
      endif
c
c
c     For not pi0 modify parameters in sane_accp
c
c
c

      
      call generate_event(u_vertex,E,part,tht_scat,phi_scat
     +                   ,deltaomega_scat,deltaE_scat)
c      write(*,*)'Event generateg suc'
c      write(*,*)E,part,tht_scat,phi_scat
c      write(*,*)part,deltaE_scat 

       Ef = abs(E)
       nu = E_beam/1000.d0 - Ef
       q2 = 2.d0*Ef*E_beam/1000.d0*(1-cos(tht_scat)   )
       xb = q2 / 2.d0 / 0.938d0 / nu 
       wsq = 0.938d0**2 + 2.d0*0.938d0*nu - q2
       q2orig=q2
       if ( wsq .gt.0 ) then
         worig = sqrt(wsq)
         else
         worig = 0
        endif
CCCCC Initialize 
       normrate = 0.d0
       xsn = 0.d0   
       ratrad = 0.d0   

      pp = sqrt(E**2-mass(part)**2)
      th = tht_scat
      ph = phi_scat
      EE = abs(E)
      if(wsq.le.0) goto 700

c      write(*,*)q2,wsq,tht_scat*180/3.14159,Ef,E_beam/1000.d0

      xsn = cross_section(zz_t,nn_t,u_vertex,part,f1,f2,q2,wsq,r)

      if ( abs(e_beam/1000.-4.7) .lt. .1 .and. tgt .eq. 1) then
         call radiated_xn_h2_47(wsq,q2,ratrad)
       endif
      if ( abs(e_beam/1000.-4.7) .lt. .1 .and. tgt .eq. 2) then
         call radiated_xn_he_47(wsq,q2,ratrad)
       endif
      if ( abs(e_beam/1000.-4.7) .lt. .1 .and. tgt .eq. 3) then
         call radiated_xn_N_47(wsq,q2,ratrad)
       endif
      if ( abs(e_beam/1000.-4.7) .lt. .1 .and. tgt .eq. 4) then
         call radiated_xn_N_47(wsq,q2,ratrad)
       endif
      if ( abs(e_beam/1000.-5.9) .lt. .1 .and. tgt .eq. 1) then
         call radiated_xn_h2_59(wsq,q2,ratrad)
       endif
      if ( abs(e_beam/1000.-5.9) .lt. .1 .and. tgt .eq. 2) then
         call radiated_xn_he_59(wsq,q2,ratrad)
       endif
      if ( abs(e_beam/1000.-5.9) .lt. .1 .and. tgt .eq. 3) then
         call radiated_xn_N_59(wsq,q2,ratrad)
       endif
      if ( abs(e_beam/1000.-5.9) .lt. .1 .and. tgt .eq. 4) then
         call radiated_xn_N_59(wsq,q2,ratrad)
       endif
c      write(*,*)'rc-intrpl',wsq,q2,ratrad,tgt 

c      endif
       if(xsn.le.0.0)goto 700
c       if(xsn*lumin(tgt).gt.25000)write(*,*)tgt,xsn*lumin(tgt)

       if(xsn.gt.0) then        !!Avoid unrealistic kinematics
          
          keptevts =  keptevts + 1 !!Count these events for normalizing
c$$$  write(*,*)keptevts
          
          rate_norm(tgt) = xsn*lumin(tgt)!*deltaE_scat*deltaomega_scat
          normrate = rate_norm(tgt)
c          write(*,*)rate_norm,xsn,lumin,tgt 
       endif      

! Comment Added by Jixie: the next few lines is trying to use rejection method to  
! require the thrown event match the realistic distribution
! please note that rrand should be a random number between 0 and maxXS, while
! this maxXS varies from channel to channel. It is very hard to hard code this number
! If this number is too large, it will cost too long to generate one event
! If this number is too small, the simulation will not generate a realistic distribution 
! For p(e,e')X,  maxXS=200
! For p(e,pi0)X, maxXS=3.7E6 
! If you are not sure what number maxXS should be, you should run the simulation with 
! small amount of thrown events then plot xsn to check
!
c      rrand =700*rand() 700 is maximal crossection (normrate) check xsn if it's more than 700 change it.
c
!       rrand =200*rand()
!       if(normrate.le.rrand)goto 700
c       WRITE(*,*)XSN,RRAND,"PASSED"


      if (part.eq.1.and.xsn.gt.0.d0) then
        asym = calc_asym(E_beam/1.d3,abs(E),tht_scat/d2r,theta_Bfield,f1,f2)
c        write(*,*)'Assym suc'
      else
        asym = 0.d0
      endif

 339   CONTINUE

      do k=1,6
        uu(k) = u_vertex(k)
      enddo

      return
      end

C------------------------------------------------------------------------------

      subroutine generate_event(u,E,part,tht,phi,deltaomega,deltaE)

*       Note: - the HMS routines use a right handed coord. system with
*                 x : pointing downwards
*                 y : perpendicular to x,z, 
*                     pointing to the left (if seen in z-direction)
*                 z : points toward calorimeter from target
*
*             - the B field map uses a cylindrical coordinate system
*               with z along the field axis and r perpendicular to it
*
*             - all length (x,y,z,dl,l,...) are measured in [cm]
*             - all velocities are measured in [cm/ns]
*             - all argument angles are in [rad]
*             - time is measured in [ns] 
*             - the B field is measured in [T]
* 
*       u   IO : coordinate vector (initial/final)
*                  u(1,2,3) : x, y, z [cm]
*                  u(4,5,6) : dx/dt, dy/dt, dz/dt [cm/ns] 
*       E   I  : particle energy [GeV] * sign of particle charge
*                (negative for electrons, positive for protons/deuterons)
*
* Beam coordinate system
*    x points down
*    y points to beam left
*    z points down beam (downstream)
* this coordinate system is then simply related to the spectrometer 
* coordinate systems by a rotation about x, but it is different from
* the more typical where x points horizontal.

      implicit none

      include 'sane_accp.inc'
      include 'constants.inc'
      include 'sane.inc'
      include 'sane_cwn.inc'

      real*8 c_lo,c_hi
      real*8 p,E,phi,c
      real*8 vel,u(6),v(6),tht,tht0
      real*8 cell_rad,cell_phi,cell_z
      real*8 deltaomega,deltaE
      real*8 fi_max,fi_min,epmin,epmax
      integer*2 i
      integer*4 part
      integer ifirst
      real*4 x_off,y_off
      COMMON/OFFSETS/x_off,y_off,ifirst
      
      if(ifirst.eq.0)then
         ifirst=1
         x_off = raster_xoff
         y_off = raster_yoff
         
      endif

      
      tht0 = theta_0*d2r
      c_lo = cos(theta_max*d2r)
      c_hi = cos(theta_min*d2r)

C Throw momentum, phi and cos(theta)
          
      p = p_min + (p_max - p_min)*rand()
      phi = phi_min + (phi_max - phi_min)*rand()
      c = c_lo + (c_hi - c_lo)*rand()
      tht = acos(c)

      E = sqrt(p**2+mass(part)**2)
      vel = cc*p/E

CCC Add in deltaE and deltaomega for rates NK (11/09/10)
      fi_min = phi_min*d2r
      fi_max = phi_max*d2r
      deltaomega = (fi_max - fi_min) * (c_hi - c_lo) 

      epmin = sqrt(p_min**2+mass(part)**2)
      epmax = sqrt(p_max**2+mass(part)**2)
      deltaE = epmax - epmin

      if (part_charge(part).ne.0) E = E*part_charge(part)

C     Generate vertex in target cell.  Assume distribution is a cylinder
C     of legnth cell_length and radius raster_radius.

      cell_z = cell_length*(rand()-0.5d0)

! By Jixie: to get an uniform distribution, 
! one should throw r in a distribution of y=kx, not y=sqrt(x)
!      cell_rad = raster_radius*sqrt(rand())  ! throw radius squared      
      cell_rad=raster_radius*sqrt(rand()**2+rand()**2)
      do while (cell_rad.gt.raster_radius)
        cell_rad=raster_radius*sqrt(rand()**2+rand()**2)
      end do

      cell_phi = 360.d0*rand()*d2r

      v(1) = cell_rad*cos(cell_phi)+x_off
      v(2) = cell_rad*sin(cell_phi)+y_off
      v(3) = cell_z

C     Plot target x and y in ntuple in LAB frame NK HB 06/15/10
!By Jixie: If SRx and SRy are in LAB system, they should be the following
      SRx =   v(2)
      SRy =  -v(1)

* generate angles in beam coordinate system

      v(4) = vel*sin(tht)*cos(phi*d2r)
      v(5) = vel*sin(tht)*sin(phi*d2r)
      v(6) = vel*cos(tht)

* Now, rotate angles so that z is pointing in direction of calorimeter

      u(1) = v(1)
      u(2) = cos(tht0)*v(2) - sin(tht0)*v(3)
      u(3) = sin(tht0)*v(2) + cos(tht0)*v(3)

      u(4) = v(4)
      u(5) = cos(tht0)*v(5) - sin(tht0)*v(6)
      u(6) = sin(tht0)*v(5) + cos(tht0)*v(6)

C      write(*,*) 'unrotated: ',v
C      write(*,*) '  rotated: ',u


      return
      end

C------------------------------------------------------------------------------

      logical*1 function check_accp(u)

      implicit none

      include 'beta_geom.inc'
      
      real*8 u(6)

      if ((abs(u(1)).le.cal_height/2.).and.
     1    (abs(u(2)).le.cal_width/2.)       ) then
        check_accp = .true.
      else
        check_accp = .false.
      endif

      return
      end

C------------------------------------------------------------------------------

      subroutine write_setup_file(nevts)

      implicit none

      include 'sane.inc'
      include 'beta_geom.inc'
      include 'sane_accp.inc'

      integer*4 nevts

      open(29,file='a1p_model_setup.dat',status='unknown')

      write(29,'(f8.1,t30,"! ",a20)') E_beam,      "Beam Energy"
      write(29,'(f8.1,t30,"! ",a20)') theta_0,     "Theta_e"
      write(29,'(f8.1,t30,"! ",a20)') theta_Bfield,"Theta_B"
      write(29,'(f8.1,t30,"! ",a20)') theta_min,   "Phi Maximum"
      write(29,'(f8.1,t30,"! ",a20)') theta_max,   "Theta Minimum"
      write(29,'(f8.1,t30,"! ",a20)') phi_min,     "Phi Minimum"
      write(29,'(f8.1,t30,"! ",a20)') phi_max,     "Phi Maximum"
      write(29,'(f9.2,t30,"! ",a20)') p_min,       "Momentum Minimum"
      write(29,'(f9.2,t30,"! ",a20)') p_max,       "Momentum Maximum"
      write(29,'(f8.1,t30,"! ",a20)') cal_width,   "Cal Width"
      write(29,'(f8.1,t30,"! ",a20)') cal_height,  "Cal Height"
      write(29,'(f8.1,t30,"! ",a20)') cal_drift,   "Cal Drift"
      write(29,'(i8,t30,"! ",a20)')   nevts,       "Num of thrown events"

      close(29)

      return
      end
