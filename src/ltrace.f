      subroutine ltrace(npesum)

      implicit none

      include 'geant.inc'

c     To trace light photons in the TF-1 Lead Glass module, produce
c     photoelectrons when hit PMT photocathode.

c     simplified version of code received from Vardan

      real wl,xp,yp,zp,up,vp,wp,rin
      integer los,npe    

      COMMON/CURRENT/WL,XP,YP,ZP,UP,VP,WP,RIN,LOS,NPE


      common/deb/ideb
      logical ideb
      data ideb/.false./
      integer npesum
      integer icp
      logical*1 first_call

c...  Loop over Cherenkov photons generated in the step.

      npesum=0
      do icp=1,ngphot

         CALL CHATCH(icp)       !get next photon from /GCKING/


         CALL ANALISER


         npesum=npesum+npe

 3       continue
      enddo

      if(ideb) print*,'LTRACE: npesum, ngphot =',npesum,ngphot

      end

c===========================================================================

      SUBROUTINE CHATCH(icp)

      implicit none

c...  Get a Geant generated Cherenkov photon from /GCKING/.

      include 'geant.inc'

      real wl,xp,yp,zp,up,vp,wp,rin
      integer los,npe    
      COMMON/CURRENT/WL,XP,YP,ZP,UP,VP,WP,RIN,LOS,NPE
      integer lun
      integer icp

      data lun/6/

      UP=xphot(4,icp)
      VP=xphot(5,icp)
      WP=xphot(6,icp)

      XP=xphot(1,icp)
      YP=xphot(2,icp)
      ZP=xphot(3,icp)
*      write(lun,'("chatch ",6f10.4)') xp,yp,zp,up,vp,wp

      los=0
      npe=0

      RETURN
      END

C======================================================================

      SUBROUTINE ANALISER

      implicit none
      include 'beta_geom.inc'
      include 'sane_cwn.inc'

      real wl,xp,yp,zp,up,vp,wp,rin
      integer los,npe    
      COMMON/CURRENT/WL,XP,YP,ZP,UP,VP,WP,RIN,LOS,NPE
      COMMON/DEB/ IDEB
      LOGICAL IDEB
 
      real x1,y1,z1
      real cer_back_height,cer_back_width
      real straight_xp,straight_yp

      z1 = -(cal_drift+cal_depth)/2.+cer_drift+cer_length
*      write(*,*) 'z1 = ',z1
      cer_back_height = (cer_drift+cer_length)/cal_drift*cal_height
      cer_back_width  = (cer_drift+cer_length)/cal_drift*cal_width

      X1 = XP + UP/WP*(z1-ZP)
      Y1 = YP + VP/WP*(z1-ZP)

* Test to see if Cerenkov photon hits back wall of Cerenkov Box

      if (abs(x1).lt.cer_back_width/2. .and.abs(y1).lt.cer_back_height/2.) then
        straight_xp = x1/(cer_drift+cer_length)
        straight_yp = y1/(cer_drift+cer_length)
*        write(*,'("back ",7f10.4)') x1,y1,up/wp,vp/wp,straight_xp,straight_yp,vect(7)
        if ((up/wp-straight_xp)**2+(vp/wp-straight_yp)**2.lt.0.0873**2) then
          cwn_cergood = cwn_cergood + 1
          cwn_cg_x(cwn_cergood) = x1
          cwn_cg_y(cwn_cergood) = y1
 1        cwn_cg_xp(cwn_cergood) = up/wp-straight_xp
          cwn_cg_yp(cwn_cergood) = vp/wp-straight_yp
*          write(*,*) cwn_cergood,cwn_cg_x(cwn_cergood),cwn_cg_y(cwn_cergood),cwn_cg_xp(cwn_cergood),cwn_cg_yp(cwn_cergood)
          npe=1
        endif
      endif


      return
      end

