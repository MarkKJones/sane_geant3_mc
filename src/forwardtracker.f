      Subroutine init_tracker()
      implicit none
c     Setup the materials and geomettry of Lucite Hodoscope
c
c
c     Include files and variables
c
      include 'constants.inc'
      include 'geant.inc'
      include 'sane.inc'
      include 'materials.inc'
      include 'beta_geom.inc'
      include 'sane_misc.inc'
      real*4 ZLuc(2), ALuc(2), WLuc(2)
      real*4 fieldmax,tmax_fd,ste_max,dee_max,epsilon,st_min
      integer*4 i_field
C
C Lucite
C
      DATA ALuc/1.00794,12.0107/
      DATA ZLuc/1.,6.0/
      DATA WLuc/0.1435,0.8565/
      
      FIELDMAX =  0.
      I_FIELD =  0
      TMAX_FD =  10.
      STE_MAX =  -1000.
      DEE_MAX =  -0.05
      EPSILON  =  0.001
      ST_MIN  =  -0.001
      
c
c     Setup medium parameters
c

      CALL GSTMED( NMED_Fx1, 'Front Hodo X1'   , 26 , 0 , I_FIELD,
     +     FIELDMAX,TMAX_FD,STE_MAX,DEE_MAX, 
     ,     EPSILON, ST_MIN, 0 , 0 )
      CALL GSTMED( NMED_Fy1, 'Front Hodo Y1'   , 26 , 0 , I_FIELD,
     +     FIELDMAX,TMAX_FD,STE_MAX,DEE_MAX, 
     ,     EPSILON, ST_MIN, 0 , 0 )
      CALL GSTMED( NMED_Fy2, 'Front Hodo Y2'   , 26 , 0 , I_FIELD,
     +     FIELDMAX,TMAX_FD,STE_MAX,DEE_MAX, 
     ,     EPSILON, ST_MIN, 0 , 0 )

      end

      Subroutine ugeom_tracker(ivol)
      implicit none

      
c
c     Include files and variables
c
      include 'constants.inc'
      include 'geant.inc'
      include 'sane.inc'
      include 'materials.inc'
      include 'beta_geom.inc'
      include 'sane_misc.inc'
      integer ivol
      real*4 par(5)
 
ccccccccccccccccccccccccccccccccccccccc
c     Setup Volume
c

c
c     Create Volumes of tracker
c
      PAR(1) = 10.95
      PAR(2) = 19.2
      PAR(3) = 0.15
      CALL GSVOLU( 'FTX1' , 'BOX ' , NMED_Fx1, PAR , 3 , IVOL ) 
c      CALL GSVOLU( 'FTX1' , 'BOX ' , NMED_Vac, PAR , 3 , IVOL ) 
c      vol_ftx1 = ivol

      PAR(1) = 11.
      PAR(2) = 19.2
      PAR(3) = 0.15
      CALL GSVOLU( 'FTY1' , 'BOX ' , NMED_Fy1, PAR , 3 , IVOL )
c      CALL GSVOLU( 'FTY1' , 'BOX ' , NMED_Vac, PAR , 3 , IVOL )
c      vol_fty1 = ivol

      
      PAR(1) = 11.
      PAR(2) = 19.35
      PAR(3) = 0.15
      CALL GSVOLU( 'FTY2' , 'BOX ' , NMED_Fy2, PAR , 3 , IVOL ) 
c      CALL GSVOLU( 'FTY2' , 'BOX ' , NMED_Vac, PAR , 3 , IVOL ) 
c      vol_fty2 = ivol
c
c     Position Volume traker into detector
c
      CALL GSPOS('FTX1',1,'EARM',0,0,-237.5+52.,0,'ONLY')
      CALL GSPOS('FTY1',1,'EARM',0,0,-237.5+52.+0.3,0,'ONLY')   
      CALL GSPOS('FTY2',1,'EARM',0,0-0.15,-237.5+52.+0.3*2.,0,'ONLY')


      end
      
      Subroutine divi_tracker()
c
c     Divide trackers  to appropriate size
c
CC One of the layers found shifted by 3mm, adjusted to make distriutions symmetric (HB).
      CALL GSDVN( 'FX1B' ,  'FTX1' , 73 , 1 )  
      CALL GSDVN( 'FY1B' ,  'FTY1' , 128 , 2 )  
      CALL GSDVN( 'FY2B' ,  'FTY2' , 129 , 2 )  
c      CALL GSDVN( 'FX1B' ,  'FTX1' , 64 , 1 )  
c      CALL GSDVN( 'FY1B' ,  'FTY1' , 128 , 2 )  
c      CALL GSDVN( 'FY2B' ,  'FTY2' , 128 , 2 )  
      
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Subroutine step_tracker()
      
c
c     Subroutine to fill the main responce of the detector.
c      
      implicit none

c
c     Include files 
c


      include 'beta_geom.inc'
      include 'geant.inc'
      include 'sane.inc'
      include 'sane_misc.inc'
      include 'materials.inc'
      include 'sane_cwn.inc' 
      
      INTEGER*4 Iy1,Iy2,Ix1,nX1,nx1Bar
      common/TEMP_TR/Iy1,Iy2,Ix1
      real*4 zX1(73),yX1(73),pX1(73),tX1(73)
      common/TEMP_X1/zX1,yX1,pX1,tX1

      real*8 Cspead, Prop_med
      parameter (Cspead = 29.9792458, Prop_med=1.49)
      
      if (lvolum(nlevel).eq.34.and.inwvol.ge.0.and.inwvol.le.2.and.
     ,     HitFrontY1(NUMBER(NLEVEL)).ne.1) then 
c     
c     Entered to FY1 part of forwards Tracker
c           

         HitFrontY1(NUMBER(NLEVEL)) = HitFrontY1(NUMBER(NLEVEL)) + 1
         Iy1            = Iy1+1
         itrackY1Hit    = Iy1
         if(itrackY1Hit.lt.25)then
            itrackY1Bar(itrackY1Hit)  = NUMBER(NLEVEL)
            
            y1Track(itrackY1Hit)     = (itrackY1Bar(itrackY1Hit)-64)*0.3-0.15
            zY1Track(itrackY1Hit)    = vert(3) !-129.55
            timeY1Track(itrackY1Hit) = TOFG*1E9
            tdcY1Track(itrackY1Hit)  = TOFG*1E9+(250-11-vect(1))/Cspead
         else
          itrackY1Hit=25
       endif
      endif
ccccccccccccccccccccccccccccccccccccc
      if (lvolum(nlevel).eq.35.and.inwvol.ge.0.and.inwvol.le.2) then 
c     
c     Entered to FY2 part of forwards Tracker
c     
         if(vert(3).lt.160)then
            HitFrontY2(NUMBER(NLEVEL)) = HitFrontY2(NUMBER(NLEVEL)) + 1
            Iy2            = Iy2+1
            itrackY2Hit    = Iy2
         if(itrackY2Hit.lt.25)then
            itrackY2Bar(itrackY2Hit)  = NUMBER(NLEVEL)
            
            y2Track(itrackY2Hit)     = (itrackY2Bar(itrackY2Hit)-65)*0.3-0.15
            zY2Track(itrackY2Hit)    = vert(3) !-129.25
            timeY2Track(itrackY2Hit) = TOFG*1E9
            tdcY2Track(itrackY2Hit)  = TOFG*1E9+(250-11-vect(1))/Cspead
         else
            itrackY2Hit=25
         endif
        endif
      endif
c         write(22,*)lvolum(nlevel)
cccccccccccccccccccccccccccccccccccccccc
      if (lvolum(nlevel).eq.33.and.inwvol.ge.0.and.inwvol.le.2) then 
c     
c     Entered to FX1 part of forwards Tracker
c     
         itrackX1Hit=0
         if(vert(3).lt.160)then
            HitFrontX1(NUMBER(NLEVEL)) = HitFrontX1(NUMBER(NLEVEL)) + 1
            Ix1            = Ix1+1
            nX1bar         = NUMBER(NLEVEL)
            
            zX1(nX1bar)    = vert(3) !-129.85
            yX1(nX1bar)    = vert(2) !-129.85
            tX1(nx1Bar)    = TOFG*1E9
            if(inwvol.eq.1)pX1(nx1Bar)=vect(7)
         endif
      endif
 
      
     
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine digi_tracker()
      implicit none

c
c     Include files 
c


      include 'beta_geom.inc'
      include 'geant.inc'
      include 'sane.inc'
      include 'sane_misc.inc'
      include 'materials.inc'
      include 'sane_cwn.inc' 
      INTEGER*4 i,Iy1,Iy2,Ix1
      common/TEMP_TR/Iy1,Iy2,Ix1
      real*4 zX1(73),yX1(73),pX1(73),tX1(73)
      common/TEMP_X1/zX1,yX1,pX1,tX1
      real*4 pps
      common/PPP/pps
      real*8 Cspead, Prop_med
      parameter (Cspead = 29.9792458, Prop_med=1.49)
      do i=1,73
         if (HitFrontX1(i).gt.0) then
            itrackX1Hit    = itrackX1Hit+1
            if(itrackX1Hit.lt.25)then
               itrackX1Bar(itrackX1Hit)   = i
               X1Track(itrackX1Hit)       = (i-36)*0.3
               tdcX1Track(itrackX1Hit)    = tX1(i)+(250-11-yX1(i))/Cspead
               ptrack(itrackX1Hit)        = pp
            else
              itrackX1Hit = 25
            endif
           
        else 
           
        endif
        pX1(i)=0
        tX1(i)=0
        yX1(i)=0
      enddo

      Iy1=0
      Iy2=0
      Ix1=0

      end
