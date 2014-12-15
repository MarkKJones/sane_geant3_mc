      Subroutine init_lucite()
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
      real tpar(5)
      data tpar/0.001, 0.001, 0.01, 0.01, 0.01/

C
C Lucite
C
      DATA ALuc/1.00794,12.0107/
      DATA ZLuc/1.,6.0/
      DATA WLuc/0.1435,0.8565/
      
      FIELDMAX =  0.
      I_FIELD =  0
      TMAX_FD =   10.
      STE_MAX =   0.5
      DEE_MAX =   0.05
      EPSILON  =  0.1
      ST_MIN  =   0.1

c
c     Setup Lucite medium parameters
c

      CALL GSTMED( NMED_Luc, 'Lucite Hodo'   , 26 , 0 , I_FIELD,
     +     FIELDMAX,TMAX_FD,STE_MAX,DEE_MAX, 
     +     EPSILON, ST_MIN, 0 , 0 )
c      call SetMedPar(NMED_Luc,tpar)
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine ugeom_lucite(ivol)

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
      integer ivol
      real*4 par(5)
      real*4 inRadL,ouRadL,hightL,phiMinL,phiMaxL


      parameter (inRadL = 240.0,ouRadL = 243.5, hightL=6.0, 
     ,     phiMinL = -11.5, phiMaxL = 11.5)

      

ccccccccccccccccccccccccccccccccccccccc
c     Setup Volume
c

c
c     Create Volume HODO
c
      PAR(1) = inRadL
      PAR(2) = ouRadL
      PAR(3) = hightL/2.*28
      PAR(4) = phiMinL
      PAR(5) = phiMaxL
      CALL GSVOLU( 'HODO' , 'TUBS ' ,NMED_Luc, PAR , 5 , ivol )
      vol_luc = ivol
c
c     Position Volume Hodo into detector
c
      CALL GSROTM(7,0,90.,-90.,0,0,0)
      write(*,*) ' position lucite'
      CALL GSPOS('HODO',1,'EARM',0.,0.,-187.5+240.,  7,'ONLY')      
c
c     Divide Detector volume to 28 separate lucite bars.
c      
      CALL GSDVN( 'HDIV' ,  'HODO' , 28 , 3 )  
      vol_hdiv = ivol+1
    
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine step_lucite()
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
      integer ihhit,lucphot(28)
      REAL*4 X_HOD(28),Y_HOD(28),Z_HOD(28),
     ,     T_HOD(28),E_HOD(28),p(28),cx(28),cy(28),cz(28)
      common/TEMP_LUC/ihhit,lucphot,X_HOD,Y_HOD,Z_HOD,T_HOD,E_HOD,p,cx,cy,cz
      
      if (lvolum(nlevel).eq.vol_hdiv.and.inwvol.ge.0.and.inwvol.le.2) then ! enter lucite hodo
c         IF(numed.eq.NMED_Luc)THEN
         HitLucHodo(NUMBER(NLEVEL)) = HitLucHodo(NUMBER(NLEVEL)) + 1 
         ihhit                = ihhit+1
         ihodoHit             = ihhit
         ihodoLuc(ihodoHit)   = NUMBER(NLEVEL)
         
         X_HOD(ihodoLuc(ihodoHit))  = X_HOD(ihodoLuc(ihodoHit))+vect(1)
         Y_HOD(ihodoLuc(ihodoHit))  = Y_HOD(ihodoLuc(ihodoHit))+vect(2)
         Z_HOD(ihodoLuc(ihodoHit))  = Z_HOD(ihodoLuc(ihodoHit))+vect(3)
         if(inwvol.eq.2)then
            p(ihodoLuc(ihodoHit))   = vect(7)
            cx(ihodoLuc(ihodoHit))  = vect(4)
            cy(ihodoLuc(ihodoHit))  = vect(5)
            cz(ihodoLuc(ihodoHit))  = vect(6)
         endif
c         write(22,*)ngphot
         if(ngphot.ge.0)lucphot(ihodoHit) = lucphot(ihodoHit)+ngphot
         T_HOD(ihodoLuc(ihodoHit))  = T_HOD(ihodoLuc(ihodoHit))+TOFG*1E9
         E_HOD(ihodoLuc(ihodoHit))  = E_HOD(ihodoLuc(ihodoHit))+destep*1000
      endif

      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Subroutine digi_lucite()
      include 'beta_geom.inc'
      include 'sane_cwn.inc' 
      include 'sane_misc.inc' 
      integer ihhit,lucphot(28)
      REAL*4 X_HOD(28),Y_HOD(28),Z_HOD(28),
     ,     T_HOD(28),E_HOD(28),p(28),cx(28),cy(28),cz(28)
      real*8 Cspead, Prop_med
      parameter (Cspead = 29.9792458, Prop_med=1.49)
      common/TEMP_LUC/ihhit,lucphot,X_HOD,Y_HOD,Z_HOD,T_HOD,E_HOD,p,cx,cy,cz
      ihodoHit=0
      do i=1,28
         if (HitLucHodo(i).gt.0.and.E_HOD(i).gt.0) then
            ihodoHit=ihodoHit+1
            ihodoLuc(ihodoHit)   = i
            xHodo(ihodoHit)      = X_HOD(i)/HitLucHodo(i)
            yHodo(ihodoHit)      = Y_HOD(i)/HitLucHodo(i)
            zHodo(ihodoHit)      = Z_HOD(i)/HitLucHodo(i)
            
            pHodo(ihodoHit)      = p(i)
            cxHodo(ihodoHit)     = cx(i)
            cyHodo(ihodoHit)     = cy(i)
            czHodo(ihodoHit)     = cz(i)
c      write(22,*)'1',i,p(i),cx(i),cy(i),cz(i),HitLucHodo(i)

            timeHodo(ihodoHit)   = t_HOD(i)/HitLucHodo(i)
            eLosHodo(ihodoHit)   = E_HOD(i)
            call TrackLuc(xHodo(ihodoHit),zHodo(ihodoHit),d1,d2)
            tdcHRigh(ihodoHit)   = timeHodo(ihodoHit)+d1/Cspead*Prop_med
            tdcHLeft(ihodoHit)   = timeHodo(ihodoHit)+d2/Cspead*Prop_med
c           if(zHodo(ihodoHit).gt.60)write(*,*)Z_HOD(i),HitLucHodo(i)
c            write(*,*)'photons in lucite =',i,lucphot(ihodoHit)
           
        else 
           
        endif
        X_HOD(i)   =0
        Y_HOD(i)   =0
        Z_HOD(i)   =0
        T_HOD(i)   =0
        E_HOD(I)   =0
        p(i)       =0
        cx(i)      =0 
        cy(i)      =0
        cz(i)      =0
        lucphot(i) =0
        HitLucHodo(i) =0
      enddo
      
      ihhit=0 
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine TrackLuc(x_0,z_0,d1,d2)
c
c     Subroutine to track the light through Lucite.
c     CAN BE IMPROVED
c
      
      distMin  = 240
      distMax  = 243.5
      dLigGui  = 6
      alpha0   = 11.5*3.14159/180.
      
      alphaC   = 3.5/distMax
      z1       = distMax*cos(alpha0-alphac)
      x1       = distMax*sin(alpha0-alphac)
      z        = z_0+184.
      x        = x_0
      z0       = 0.5*(distMax+distMin)
      zmirror  = 2*z0 -z
      if(x.lt.0)then
         d0       = dLigGui*sqrt(2.)+sqrt((x-x1)**2+(zmirror-z1)**2)
         d1       = dLigGui*sqrt(2.)+sqrt((x+x1)**2+(zmirror-z1)**2)
      endif

      end
