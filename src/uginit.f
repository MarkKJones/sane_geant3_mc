*CMZ :          30/08/94  16.03.40  by  S.Ravndal
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE UGINIT

      implicit none

      include 'beta_geom.inc'
      include 'sane_misc.inc'
      include 'sane.inc'
      include 'geant.inc'
      include 'materials.inc'
      include 'sane_accp.inc'

      real*4 thte_tmp,thtb_tmp
      real*4 ebeam_tmp	
      real*4 Hyddens_tmp,Heldens_tmp,Nitdens_tmp,Effdens_tmp	
      real*4 Pckfrc_tmp
      real*4 TargzOff_tmp,CdiscOff_tmp
      real*4 RastOff(2)
      integer ifirst
      external gudigi
      integer ifield_init,i
C
C (saw) Put FFKEY arguments in common block so that
C CHKLOC doesn't give LOCB/LOCF error
C
      common /common_for_tmp/ thte_tmp, thtb_tmp, ebeam_tmp, hyddens_tmp,
     $     heldens_tmp, nitdens_tmp, effdens_tmp, pckfrc_tmp,
     $     cdiscoff_tmp, targzoff_tmp, rastoff

C added by OR to read from a1p.dat like theta, theta B, etc.
      
C
C Open user files
C
      CALL UFILES
C
C Initialise GEANT
C
      CALL GINIT
*
*
*             GEN data cards
*

      igauto = 1      
      CALL FFKEY('TTYP',target_type,1,'INTEGER')
      CALL FFKEY('FTYP',field_type,1,'INTEGER')
      CALL FFKEY('THTE',thte_tmp,10,'REAL')
      CALL FFKEY('THTB',thtb_tmp,10,'REAL')
      CALL FFKEY('EBEA',ebeam_tmp,1,'REAL')	
      CALL FFKEY('HYDDENS',Hyddens_tmp,1,'REAL')	
      CALL FFKEY('HELDENS',Heldens_tmp,1,'REAL')	
      CALL FFKEY('NITDENS',Nitdens_tmp,1,'REAL')
      CALL FFKEY('EFFDENS',Effdens_tmp,1,'REAL')
      CALL FFKEY('PCKFRC',Pckfrc_tmp,1,'REAL')
      CALL FFKEY('CDISCOFF',CdiscOff_tmp,1,'REAL')
      CALL FFKEY('TARGZOFF',TargzOff_tmp,1,'REAL')
      CALL FFKEY('RASTOFF',RastOff,2,'REAL')

C! added by OR 11/06
C
C
C Define user FFREAD data cards (format free input)
C
      CALL FFSET('LINP',4)
C
C Read the data cards
C
      PKINE(4) = 12345.
      CALL GFFGO

* Change from single to double precision

      theta_0      = DBLE(thte_tmp)
      theta_Bfield = DBLE(thtb_tmp)
      E_beam       = DBLE(ebeam_tmp)	
      ebeam2 = e_beam
      e_beam = ebeam2
!      E_beam       = ebeam_tmp	
C!	added by OR 11/06
C
C! Read in densities for NH3 NK 03/31/10
c      if (target_type.EQ.0) then    ! define polarized target
       Hyddens       = Hyddens_tmp	
       Heldens       = Heldens_tmp	
       Nitdens       = Nitdens_tmp	
       Effdens       = Effdens_tmp	
       Pckfrc        = Pckfrc_tmp 

c       Effdens = (Hyddens + Nitdens)*Pckfrc + Heldens*(1 - Pckfrc) 

c       write(*,*)Hyddens,Nitdens,Heldens,Pckfrc,Effdens 
c       write(*,*)'Target Offset=',TargVrtzOff
c      endif 
c      if (target_type.EQ.1) then    ! define polarized target
       CdiscOff      = CdiscOff_tmp	
        write(*,*)'C disc Offset=',CdiscOff
c      endif 

       TargVrtzOff   = TargzOff_tmp
       write(*,*)'Target Offset=',TargVrtzOff

C! Read in E_min cut for each block  HB, NK 05/12/10
c      OPEN(7,FILE='energy_cut_parallel.dat',status='OLD')
c      do i=1,1792
c         read(7,*)bigcal_block_cut(i)
c         if(bigcal_block_cut(i).lt.0.01)bigcal_block_cut(i)=0.01
c         bigcal_block_cut_check(i)=bigcal_block_cut(i)
cc         bigcal_block_cut_check(i)=0. !!Try without cut
c         write(*,*)i,bigcal_block_cut(i)
cc         write(*,*)i,bigcal_block_cut_check(i)
c      enddo
c      close(7)
      do i=1,1792
         bigcal_block_cut_check(i)=0. !!Try without cut
      enddo

C! Read in Slow Raster radius and offsets NK 06/09/10
c      raster_radius = SRRad_tmp	
      raster_xoff   = RastOff(1)	
      raster_yoff   = RastOff(2)	
c      write(*,*)'SlowRaster',raster_xoff,raster_yoff,
c     ,          RastOff

C Initialise Zebra structure
C
      CALL GZINIT
C
C Geometry and materials description
C
      CALL UGEOM

C
C Print the defined materials, tracking media and volumes
C
      CALL GPRINT('MATE',0)
      CALL GPRINT('TMED',0)
      CALL GPRINT('VOLU',0)
C
C Define user histograms and n-tuple
C
      CALL UHINIT
C
C GAW - stuff for target field

*      if (theta_Bfield.lt.-180.d0.or.theta_Bfield.gt.0.d0) then
*        write(*,*) 'target field must point beam left'
*        stop
*      endif

      omega = theta_0 + theta_Bfield !!Original, correct one to use 11/15/10
c      omega = abs(theta_Bfield)-theta_0 !!New version 04/13/10

      write(*,*) '-----------------------------------------------------'
      write(*,*) 'Summary of GEANT Parameters'
      write(*,*) 'iteration = ',iteration
      write(*,*) 'particle = ',particle
      write(*,*) 'Energy = ',E_beam,' Angle = ',theta_0,' ebeam2 ',ebeam2
c      if(field_type.eq.1)then
       call trgInit('trg_field_map.dat',omega,0.)
c      endif
      END
