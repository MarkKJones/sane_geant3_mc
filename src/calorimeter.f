      Subroutine init_cal()
      implicit none
c     Setup the materials and geometry of Calorimeter
c
c     Include files and variables
c 
      include 'constants.inc'
      include 'geant.inc'
      include 'sane.inc'
      include 'materials.inc'
      include 'beta_geom.inc'
c      include 'sane_misc.inc'
      real*4 ZLG(5),ALG(5),WLG(5)
      real*4 fieldmax,tmax_fd,ste_max,dee_max,epsilon,st_min
      integer*4 i_field

C Lead glass mixture parameters nucleus charge, atomic wheight, rel. wheight
C of the different compounds
C
      DATA ALG/ 207.19,  15.999, 28.086, 39.098, 74.922/
      DATA ZLG/  82.00,   8.00,  14.00,  19.00,  33.00/
      DATA WLG/    .475,   .270,   .193,   .058,   .004/
      
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
      CALL GSTMED( NMED_LG,'Pb-Glass'              , 22 , 0 , I_FIELD,
     +     FIELDMAX,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
      end

cccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine def_calspace(ivol)
      implicit none

c
c     Include files and variables
c
      include 'constants.inc'
      include 'geant.inc'
      include 'sane.inc'
      include 'materials.inc'

      real*4 x,y
      integer ivol
      real*4 par(3)
      real*4 cal_depth
      parameter( cal_depth    =  40.d0 )
      real*4 cal_drift 
      parameter( cal_drift    = 335.d0 )
      real*4 z0

      x = 0.
      y = 0.
c
c     Create and position the Imaginary Calorimeter Air Box
c
      PAR(1) = 128./2.+0.0001
      PAR(2) = 248/2.+0.0001
      PAR(3) = 44./2.+0.0001
      CALL GSVOLU( 'ECAL' , 'BOX ' ,NMED_Air, PAR , 3 , IVOL )
      CALL GSPOS('ECAL',1,'EARM',x,y, cal_drift/2. ,0,'MANY')

      end

      Subroutine ugeom_cal(ivol)
      implicit none

      
c
c     Include files and variables
c
      include 'constants.inc'
      include 'geant.inc'
      include 'sane.inc'
      include 'materials.inc'
C     include 'beta_geom.inc'  Including parameters from this inc file below. JDM 
c      include 'sane_misc.inc'

      real*4 x,y
      integer ivol
      real*4 par(3)
      real*4 prot_block_height, prot_block_width
      real*4 rcs_block_height, rcs_block_width
      real*4 prot_cal_height,rcs_cal_height,prot_cal_width,rcs_cal_width

      real*4 cal_depth
      real*4 cer_length
      real*4 cal_start_x_Prot
      real*4 cal_start_y_Prot
      real*4 cal_start_x_Rcs
      real*4 cal_start_y_Rcs

      integer*4 prot_horzBl,prot_vertBl
      integer*4 rcs_horzBl,rcs_vertBl

      parameter( cal_start_x_Prot  =  -60.302    )
      parameter( cal_start_y_Prot  =  -107.78    )
      parameter( cal_start_x_Rcs  =  -60.983    )
      parameter( cal_start_y_Rcs  =  14.135    )
      parameter( prot_horzBl  =  32    )
      parameter( prot_vertBl  =  32    )
      parameter( rcs_horzBl  =  30    )
      parameter( rcs_vertBl  =  24    )

      parameter( prot_block_height =   3.8098d0 )
      parameter( prot_block_width  =   3.8098d0 )
      parameter( rcs_block_height =   4.02167 )
      parameter( rcs_block_width  =   4.02167 )

      parameter( prot_cal_height   = prot_vertBl*prot_block_height )
      parameter( prot_cal_width    = prot_horzBl*prot_block_width  )
      parameter( rcs_cal_height   = rcs_vertBl*rcs_block_height )
      parameter( rcs_cal_width    = rcs_horzBl*rcs_block_width  )

      parameter( cal_depth    =  40.d0 )
      parameter( cer_length   = 150.d0 )


      real*4 cal_drift 
      real*4 cer_drift

      parameter( cal_drift    = 335.d0 )
      parameter( cer_drift    =  55.d0 )

      real*4 z0
      parameter (  z0 = -(cal_drift/2.+cal_depth/2.) )
      integer ivol_p,ivol_r
      common/BIGCALVOL/ivol_p,ivol_r

c
c     Create Volume of Calorimeter
     
      x = 0.
      y = 0.
C     Replacing ECAL with PCAL, Protvino part, and RCAL, RCS part

      PAR(1) = prot_cal_width/2.
      PAR(2) = prot_cal_height/2.
      PAR(3) = cal_depth/2.
      CALL GSVOLU( 'PCAL' , 'BOX ' ,NMED_LG, PAR , 3 , IVOL )
      ivol_p = ivol

      PAR(1) = rcs_cal_width/2.
      PAR(2) = rcs_cal_height/2.
      PAR(3) = cal_depth/2.
      CALL GSVOLU( 'RCAL' , 'BOX ' ,NMED_LG, PAR , 3 , IVOL )
      ivol_r = ivol

c     Position Volume of Calorimeter into detector
c     
      x = cal_start_x_Prot+prot_cal_width/2.
      y = cal_start_y_Prot+prot_cal_height/2.

      CALL GSPOS('PCAL',1,'ECAL',x,y,0.,0,'ONLY')
      x = cal_start_x_Rcs+Rcs_cal_width/2.
      y = cal_start_y_Rcs+rcs_cal_height/2.

      CALL GSPOS('RCAL',1,'ECAL',x,y,
     ,     0.,0,'ONLY')

      end
      
ccccccccccccccccccccccccccccccccccccccc

      Subroutine divi_cal()
c
c     Divide trackers  to appropriate size
c

c     Protvino
      CALL GSDVN( 'PCOL' , 'PCAL' ,  32 , 1)
      CALL GSDVN( 'PBLC' , 'PCOL' ,  32 , 2)

c     RCS
      CALL GSDVN( 'RCOL' , 'RCAL' ,  30 , 1)
      CALL GSDVN( 'RBLC' , 'RCOL' ,  24 , 2)   
  
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine step_cal(IP)
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
      integer ivol_p,ivol_r
      common/BIGCALVOL/ivol_p,ivol_r
      integer bar1,bar2
      integer IP
      integer iycell,ixcell,ifirst
      real*4 Energy(32,56),Xv,Yv,Zv
      common/ENCORc/iycell,ixcell,ifirst,Energy,Xv,Yv,Zv
      real destep_sm
c      write(*,*)lvolum(nlevel),destep
      iycell=0
      ixcell=0
c      if(lvolum(nlevel).eq.4.and.inwvol.eq.1)write(*,*)IP,inwvol,DESTEP
      if(lvolum(nlevel).eq.37.and.DESTEP.gt.0)then
c         write(*,*)NLEVEL,NUMBER(1),NUMBER(2),NUMBER(3),
c     ,        NUMBER(4),NUMBER(5)
         iycell = NUMBER(NLEVEL)
         ixcell  = NUMBER(NLEVEL-1)
         destep_sm = destep+(rand()-0.5)*2*0.09*destep
         Energy(ixcell,iycell)=Energy(ixcell,iycell)+DESTEP
c         Energy(ixcell,iycell)=Energy(ixcell,iycell)+DESTEP_sm
c         write(*,*)"We hit P",ixcell,iycell,Energy(ixcell,iycell)
c         write(*,*)ifirst
c         write(*,*)IP,inwvol
         if(inwvol.ge.0.and.inwvol.le.2.and.ifirst.eq.0)then
c         write(*,*)ifirst,Vect(1),Vect(2)
            Xv=Vect(1)
            Yv=Vect(2)
            Zv=Vect(7)
            ifirst = ifirst+1
         endif
c         write(*,*)Xv,Yv,Zv,Vect(3),step
      endif
c      if(lvolum(nlevel).eq.4.and.inwvol.eq.1)write(*,*)IP,inwvol,DESTEP
      if(lvolum(nlevel).eq.39.and.DESTEP.gt.0)then
         iycell = NUMBER(NLEVEL)+32
         ixcell  = NUMBER(NLEVEL-1)
         destep_sm = destep+(rand()-0.5)*2*0.09*destep
c         Energy(ixcell,iycell)=Energy(ixcell,iycell)+DESTEP_sm
         Energy(ixcell,iycell)=Energy(ixcell,iycell)+DESTEP
c         write(*,*)IP,inwvol
c         write(*,*)"We hit R",ixcell,iycell,Energy(ixcell,iycell)
         if(inwvol.ge.0.and.inwvol.le.2.and.ifirst.eq.0)then
            Xv=Vect(1)
            Yv=Vect(2)
            Zv=Vect(7)
            ifirst = ifirst+1
         endif
c         if(ixcell.gt.0)then
c            write(*,*)EE,ixcell,iycell,Energy(ixcell,iycell)
c         endif
c         write(*,*)Xv,Yv,Zv,Vect(3),step

      endif
 

      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine digi_cal()
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
      integer bar1,bar2
      integer iycell,ixcell,ifirst
      real*4 Energy(32,56),Xv,Yv,Zv
      common/ENCORc/iycell,ixcell,ifirst,Energy,Xv,Yv,Zv

      integer i, j
      integer iclust 
      integer ix(25),iy(25)
      real Eyx(5,5),xx(5,5),yy(5,5),x(25),y(25),bsize,yc,xc
      real*4 coor(3),cooro(3)
      common/ENERGYYX/Eyx,xx,yy
      integer nnclust,nnclustA
      real Emax,Etot,Etot9
      real xmax,ymax,xmom,ymom
      integer ixmax,iymax
      real Etot3pp,Etot3pm,Etot3mp,Etot3mm
      common/gg/nnclustA,nnclust,Etot,Etot9, 
     ,     ixmax,iymax,Emax,xmax,ymax, 
     ,     Etot3pp,Etot3pm,Etot3mp,Etot3mm
          
c      write(*,*)'Entering CAL'
      Emax = 0
      nnclustA =0
      ixmax =0
      iymax =0
c      write(*,*)'GUOUT CALLED',EE
c      if(ixcell.ne.0)write(*,*)Energy(ixcell,iycell),th*180/3.141,ph
      if(EE.ne.EE)then
         goto 21
      endif

      do i=1,56
         do j=1,32
c            Energy(j,i) = Energy(j,i)+(rand()-0.5)*2*0.09*Energy(j,i)

c            write(*,*)Energy(j,i)
            if(Energy(j,i).lt.bigcal_block_cut_check((i-1)*32+j))
     ,           Energy(j,i)=0
            if(Energy(j,i).gt.Emax.and.Energy(j,i)
     >         .eq.Energy(j,i))then
               Emax=Energy(j,i)
               nnclustA = nnclustA+1
               ixmax = j
               iymax = i
               if(i.le.32)then
                  bsize=3.81
                  xmax= j*bsize-32*3.81/2.-bsize/2.
                  ymax= i*bsize-(32*3.81+24*4.02167)/2.-bsize/2.
               else
                  bsize=4.02167
                  xmax= j*bsize-32*3.81/2.-bsize/2.
                  ymax= 32*3.81+(i-32)*bsize-
     ,                 (32*3.81+24*4.02167)/2.-bsize/2.
               endif

            endif
         enddo
      enddo
c      write(*,*)ixmax
      if(Emax.lt.0.05.or.abs(EE).gt.5)then
c         write(*,*)Emax,EE
         goto 21
      endif
c      if(ixcell.ne.0)write(*,*)Emax
      coor(1)=Xv
      coor(2)=Yv
      coor(3)=Zv

      do i=1,25
         ix(i)=0
         iy(i)=0
      enddo
      Etot=0
      nnclust=0
      xmom=0
      ymom=0
 

       do i=1,5
         do j=1,5
            Eyx(i,j)=0

         enddo
       enddo


c      write(*,*)'done with CAL 0',iymax,ixmax
c      do iclust=1,5
      do i=iymax-2,iymax+2
         do j=ixmax-2,ixmax+2
            if(i.gt.0.and.j.gt.0.and.i.lt.57.and.j.lt.33)then
               Eyx(i-iymax+3,j-ixmax+3)=0
               yy(i-iymax+3,j-ixmax+3)=0
               xx(i-iymax+3,j-ixmax+3)=0
c$$$               if(bigcal_block_cut((i-1)*32+j).ne.
c$$$     ,          bigcal_block_cut_check((i-1)*32+j))
c$$$     ,          write(*,*)i,j,(i-1)*32+j,
c$$$     ,              bigcal_block_cut((i-1)*32+j),
c$$$     ,              bigcal_block_cut_check((i-1)*32+j)
               if(i.gt.0.and.j.gt.0.and.i.lt.57.and.j.lt.33.and.
     ,              Energy(j,i).gt.bigcal_block_cut_check((i-1)*32+j).and.
     ,              Energy(j,i).eq.Energy(j,i))then
                  Etot = Etot+Energy(j,i)
                  nnclust=nnclust+1
                  ix(nnclust)=j
                  iy(nnclust)=i
                  Eyx(i-iymax+3,j-ixmax+3)=0
                  xx(i-iymax+3,j-ixmax+3)=0
                  yy(i-iymax+3,j-ixmax+3)=0
cc                  if(j.le.32)then !! This was wrong HB, NK 12/15/10
                  if(i.le.32)then
                  bsize=3.81
               else
                  bsize=4.02167
               endif
               xc= ix(nnclust)*bsize-32*3.81/2.-bsize/2.
               if(iy(nnclust).lt.33)then
                  yc= iy(nnclust)*bsize-
     ,                 (32*3.81+24*4.02167)/2.-bsize/2.
               else
                  yc= (iy(nnclust)-32)*bsize+32*3.81-
     ,                 (32*3.81+24*4.02167)/2.-bsize/2.

               endif
c               write(*,*)iy(nnclust),bsize

               if(i.lt.33.and.i.gt.0.and.j.gt.0.and.j.lt.33)then
                  Eyx(i-iymax+3,j-ixmax+3)=Energy(j,i)
                  Energy(j,i)=0
                  xx(i-iymax+3,j-ixmax+3)=xc
                  yy(i-iymax+3,j-ixmax+3)=yc
c                  write(*,*)yy(i-iymax+3,j-ixmax+3),i-iymax+3,j-ixmax+3,yc
CCC Corrected cut on number of cells HB,NK 12/13/10 
c               elseif(i.lt.57.and.i.gt.32.and.j.gt.0.and.j.lt.30)then
               elseif(i.lt.57.and.i.gt.32.and.j.gt.0.and.j.lt.31)then 
                  Eyx(i-iymax+3,j-ixmax+3)=Energy(j,i)
                  Energy(j,i)=0
                  xx(i-iymax+3,j-ixmax+3)=xc
                  yy(i-iymax+3,j-ixmax+3)=yc
c                  write(*,*)yy(i-iymax+3,j-ixmax+3),i-iymax+3,j-ixmax+3,yc
               endif

c               write(*,*)i-iymax+3,j-ixmax+3,Energy(j,i)
c               if(i.ge.iymax-1.and.i.le.iymax+1.and.
c     ,              j.ge.ixmax-1.and.j.le.ixmax+1)then
c                  xmom = xmom+(x(nnclust)-xmax)*sqrt(Energy(j,i))
c                  ymom = xmom+(y(nnclust)-ymax)*sqrt(Energy(j,i))

c     endif
            endif
         endif
         enddo
      enddo
c      enddo

CCC Move to guout for looping over clusters
c      do i=1,32
c         do j=1,56
c            Energy(i,j)=0
c         enddo
c      enddo

 21   CONTINUE
c      write(*,*)'done with CAL'

      end
      subroutine clear_cal()
      IMPLICIT NONE
      integer iycell,ixcell,ifirst
      real*4 Energy(32,56),Xv,Yv,Zv
      common/ENCORc/iycell,ixcell,ifirst,Energy,Xv,Yv,Zv
      integer i,j
      do i=1,32

         do j=1,56
            Energy(i,j)=0
            
         enddo
      enddo
      
      end

      subroutine Rotate(P,phi,th,psi,P1)
      IMPLICIT NONE
c      iflag=1 -Rotate from P system to LAB
c      iflag=-1 -Rotate from LAB system to P
      real*4 P(3),P1(3)
      real*8 a(3,3)
      real phi,th,psi
      real pphi,pth,ppsi
      integer i,j
      pphi =phi
      pth  =th
      ppsi =psi
      if( abs(pphi).lt.0.002) pphi=0

      if(  abs(pth).le.0.002 ) pth=0
      
      if( abs(ppsi).le.0.002 ) ppsi=0

      a(1,1) = -sin(ppsi)*sin(pphi)+cos(pth)*cos(pphi)*cos(ppsi)
      a(1,2) = -sin(ppsi)*cos(pphi)+cos(pth)*sin(pphi)*cos(ppsi)
      a(1,3) = -sin(pth)*cos(ppsi)
      a(2,1) = -sin(pphi)*cos(ppsi)-cos(pth)*cos(pphi)*sin(ppsi)
      a(2,2) =  cos(pphi)*cos(ppsi)-cos(pth)*sin(pphi)*sin(ppsi)
      a(2,3) =  sin(pth)*sin(ppsi)
      a(3,1) =  sin(pth)*cos(pphi)
      a(3,2) =  sin(pth)*sin(pphi)
      a(3,3) =  cos(pth)
c      write(*,"(3F10.3)")a
      do 100 i=1,3
         P1(i)=0
         do 200 j=1,3
            if(abs(a(i,j)).lt.1e-7)a(i,j)=0
c     write(*,*)P1(i),a(i,j)
            P1(i)=P1(i)+a(i,j)*P(j)
 200     continue
         if(abs(P1(i)).lt.1e-3)P1(i)=0.
 100  continue
      end
