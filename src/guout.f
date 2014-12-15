      SUBROUTINE GUOUT

      implicit none

      include 'beta_geom.inc'
      include 'sane_misc.inc'
      include 'geant.inc'
      include 'sane_cwn.inc'
      include 'sane.inc'

      integer changes
      integer i,j
      integer iclust
      integer ii,jj

      real*4 q2orig,worig
      common /orig_kin/ q2orig,worig
      real phi_test
      common/PTEST/phi_test
      integer*4 cl,last_cl
      real*4 eng,d1,d2
      integer ihhit
      REAL*4 X_HOD(28),Y_HOD(28),Z_HOD(28),
     ,     T_HOD(28),E_HOD(28)
      common/TEMPR/ihhit,X_HOD,Y_HOD,Z_HOD,T_HOD,E_HOD
c      real Eyx(5,5),xx(5,5),yy(5,5)
      real Eyx(5,5),xx(5,5),yy(5,5)
      common/ENERGYYX/Eyx,xx,yy

      integer nnclust,nnclustA
      real Emax,Etot,Etot9
      real xmax,ymax,xmom,ymom
      integer ixmax,iymax
      real Etot3pp,Etot3pm,Etot3mp,Etot3mm
      common/gg/nnclustA,nnclust,Etot,Etot9, 
     ,     ixmax,iymax,Emax,xmax,ymax, 
     ,     Etot3pp,Etot3pm,Etot3mp,Etot3mm

      double precision FixX,FixY,FixE
      common/SNEU/FixX,FixY,FixE

      real X_coor,Y_coor,E_coor,Z_coor
      real X_coor_r,Y_coor_r,E_coor_r,Z_coor_r

      real V_coor(3),V_coor_r(3)
      real PHI_C,THETA_C
      real PHI_UC,THETA_UC

      real Emt,esum,nesum
      real xmomsqr,xmomsq,ymomsqr,ymomsq
c      double precision VectorN(12)
      double precision VectorN(27)
      double precision COORE,coorX2,coorY2
      character*80 arg
      double precision sane_n100xye      

      real*8 Cspead
      parameter (Cspead = 29.9792458)
      parameter (Z_coor=335)

      real q2,xb,wsq,nu
C
C Fill the total energy of the event in the histogram
C
      
c      write(*,*)'volume=',lvolum(nlevel),nlevel,vect(1),vect(2),vect(3),uu(1),uu(2),uu(3)
      if(xsn.eq.0)goto 1101
      cwn_p_ne =0
      cwn_p_ng =0
      cwn_p_np =0
      do i=1,num_gc
c         write(*,*)i,gc_part(i)
        if (gc_part(i).eq.1) then
c           write(*,*)gc_x(i),gc_y(i),gc_eng(i)
          cwn_p_ng = cwn_p_ng + 1
          cwn_p_gx(cwn_p_ng) = gc_x(i)
          cwn_p_gy(cwn_p_ng) = gc_y(i)
          cwn_p_ge(cwn_p_ng) = gc_eng(i)
        elseif (gc_part(i).eq.2) then
c           write(*,*)gc_x(i),gc_y(i),gc_eng(i)
          cwn_p_np = cwn_p_np + 1
          cwn_p_px(cwn_p_np) = gc_x(i)
          cwn_p_py(cwn_p_np) = gc_y(i)
          cwn_p_pe(cwn_p_np) = gc_eng(i)
        elseif (gc_part(i).eq.3) then
c           write(*,*)gc_x(i),gc_y(i),gc_eng(i),cwn_p_ne
          cwn_p_ne = cwn_p_ne + 1
          cwn_p_ex(cwn_p_ne) = gc_x(i)
          cwn_p_ey(cwn_p_ne) = gc_y(i)
          cwn_p_ee(cwn_p_ne) = gc_eng(i)
        endif
      enddo
c      if(part.gt.20.or.part.le.0)then
c      write(*,*) 'Events: ',numevts,'GenEvent',part
         call getarg(2,arg)
         read(arg,'(i1)') particle
         part=particle
c         write(*,*)EE,pp,part,tgt_num,tgt_num1
         if(tgt_num.ne.tgt_num1.and.tgt_num.gt.200)tgt_num=tgt_num1
         if(abs(tgt_num).gt.200)goto 1101
c         part = iipart
c         write(*,*)particle
c      endif

c      write(*,*)'part=',part
      cwn_E    = EE
      cwn_p    = pp
      cwn_part = part
c      write(*,*)z(tgt_num),n(tgt_num)
      cwn_z    = z(tgt_num)
      cwn_n    = n(tgt_num)
      cwn_xsn  = xsn
      cwn_xsnscal=xsnscal
      if(ratrad.eq.ratrad)then
         cwn_xsnr  = ratrad
      else
         cwn_xsnr  = 0
      endif
         
c      write(*,*)'CR in NTUPLE ',xsn
      cwn_stopvol = float(lvolum(nlevel))
      cwn_th   = th
      cwn_ph   = ph
      cwn_cerphot = asym

      call NANcheckF(xsn,12)
      call NANcheckF(ratrad,13)
      call NANcheckF(asym,14)

C      write(*,*)0,th*180/3.141,ph-90
*      cwn_cergood = photGood
c      write(*,*) ' photgood = ',photGood 

C    --- Front and Lucite Hososcopes -JDM - 6/20/07
      call digi_tracker()
      call digi_lucite()

CCC Loop over clusters to include other particles HB,NK 04/28/10
      nclust=0
c      write(*,*) ' eloss = ',eloss(7),eloss(8),eloss(9),eloss(10)
      do iclust=1,5 
         call digi_cal(esum,nesum)
            etot=0
            do i=1,5
               do j=1,5
                  etot=etot+eyx(i,j)
c                  write(*,*) ' gout = ',i,j,eyx(i,j)
               enddo
            enddo
        
c      write(*,*)2,part,eyx(3,3)
         if(Eyx(3,3).gt.0.and.etot.gt.0.15)then
            nclust=nclust+1
            VectorN(1)   = eyx(1,1)
            VectorN(2)   = eyx(2,1)
            VectorN(3)   = eyx(3,1)
            VectorN(4)   = eyx(4,1)
            VectorN(5)   = eyx(5,1)
            VectorN(6)   = eyx(1,2)
            VectorN(7)   = eyx(2,2)
            VectorN(8)   = eyx(3,2)
            VectorN(9)   = eyx(4,2)
            VectorN(10)  = eyx(5,2)
            VectorN(11)  = eyx(1,3)
            VectorN(12)  = eyx(2,3)
            VectorN(13)  = eyx(3,3)
            VectorN(14)  = eyx(4,3)
            VectorN(15)  = eyx(5,3)
            VectorN(16)  = eyx(1,4)
            VectorN(17)  = eyx(2,4)
            VectorN(18)  = eyx(3,4)
            VectorN(19)  = eyx(4,4)
            VectorN(20)  = eyx(5,4)
            VectorN(21)  = eyx(1,5)
            VectorN(22)  = eyx(2,5)
            VectorN(23)  = eyx(3,5)
            VectorN(24)  = eyx(4,5)
            VectorN(25)  = eyx(5,5)
            
c            ncell(iclust)=0
c            do ii=1,5
c               do jj=1,5
c                  if(eyx(ii,jj).gt.0.01)ncell(iclust)=ncell(iclust)+1
c                  
c               enddo
c            enddo
            ncell(iclust)=nesum
c            write(*,*) ' nesum = ',nesum,esum,etot
            VectorN(26) = DBLE(ixmax)
            VectorN(27) =DBLE(iymax)
            
            

            COORX2 = sane_n100xye(VectorN,0)

            COORY2 = sane_n100xye(VectorN,1)
            COORE  = sane_n100xye(VectorN,2)
            X_coor= XX(3,3)+COORX2
            Y_coor= YY(3,3)+COORY2
            E_coor= etot+CoorE
            V_coor(1) = X_coor
            V_coor(2) = Y_coor
            V_coor(3) = Z_coor
            call ROTATE(V_coor,0.,-40*3.1415926536/180.,0.,V_coor_r)
c     write(*,*)1,V_coor,YY(3,3)
c     write(*,*)2,V_coor_r,CoorE
            X_coor_r = V_coor_r(1)
            Y_coor_r = V_coor_r(2)
            Z_coor_r = V_coor_r(3)
            
            call NANcheckF(E_coor,1)
            call NANcheckF(x_coor,2)
            call NANcheckF(y_coor,3)
            call NANcheckF(Z_coor,4)
            call NANcheckF(X_coor_r,5)
            call NANcheckF(Y_coor_r,6)
            call NANcheckF(Z_coor_r,7)
            cwn_x_ur(iclust)=X_coor
            cwn_y_ur(iclust)=Y_coor
            cwn_z_ur(iclust)=Z_coor
c            write(*,*) ' call digi_cer'
            call digi_cer(iclust)
            
c      if(X_coor_r.gt.253.5.and.Y_coor_r.gt.12)then
c       write(*,*) X_coor_r  
c     ,           ,Y_coor_r  
c     ,           ,Z_coor_r
c     ,           ,E_coor*1000
c      endif
CCC Smear to get resolution similar to data HB NK 08/11/10
CCCC 12% shift seems to get it closest from pi0 mass peak. 
c            E_coor = E_coor +(rand()-0.5)*2*0.12*E_coor
c            E_coor = E_coor +(rand()-0.5)*2*0.09*E_coor

            call CORRECT_ANGLES(
     ,           X_coor_r,  
     ,           Y_coor_r,  
     ,           Z_coor_r,
     ,           E_coor,                
     ,           THETA_C ,
     ,           PHI_C,cer_h(iclust),srx,sry)
c            write(*,*) ' corr = ',e_coor,theta_c,phi_c 
            cwn_E_m(iclust)=Esum
            cwn_E_r(iclust)=E_coor
            cwn_th_r(iclust)=THETA_C
            cwn_ph_r(iclust)=PHI_C
            cwn_x_r(iclust)=X_coor_r
            cwn_y_r(iclust)=Y_coor_r
            cwn_z_r(iclust)=Z_coor_r

            call NANcheckF(THETA_C,8)
            call NANcheckF(PHI_C,9)

            nu = E_beam/1000.d0 - E_coor
            q2 = 2.d0*E_beam/1000.d0*E_coor*(1-cos(THETA_C/180.d0*3.14159d0))
            xb = q2/2.d0/0.938d0/nu
            wsq = 0.938d0**2 + 2.d0*0.938d0*nu - q2
            cwn_Q2_r(iclust)=q2
            cwn_xb_r(iclust)=xb
            if ( wsq .gt. 0) then
            cwn_W_r(iclust)=sqrt(wsq)
            else
            cwn_W_r(iclust)=0.
            endif


c      if(X_coor_r.gt.253.5)then
C      if(X_coor_r.gt.253.5.and.PHI_C.gt.10)then
c      if(X_coor_r.gt.253.5.and.THETA_C.gt.40)then
c       write(*,*) X_coor_r  
c     ,           ,Y_coor_r  
c     ,           ,THETA_C  
c     ,           ,PHI_C  
c      endif
            
         
            call UNCORRECT_ANGLES(
     ,           X_coor_r,  
     ,           Y_coor_r,  
     ,           Z_coor_r,
     ,           E_coor,                
     ,           THETA_UC ,
     ,           PHI_UC)
            
c      enddo !! Finish looping over clusters   
            
            call NANcheckF(THETA_UC,10)
            call NANcheckF(PHI_UC,11)
            cwn_th_ucr(iclust)=THETA_UC
            cwn_ph_ucr(iclust)=PHI_UC
            
            do i=1,5
               do j=1,5
                  Eyx(i,j)=0
                  XX(i,j)=0
                  YY(i,j)=0
               enddo
            enddo

         endif
      enddo !! Finish looping over clusters   

c     write(*,*)etot,coore,EE,etot+coorE,etot+FixE
         
C     ---- JDM
         
         if (photGood.gt.0) then
*     write(*,*) '# cerenkov photons:',photCer,photGood
            if (idebug.ne.0) call gdxyz(0)
         endif
         
*     write(*,*) part,cwn_part
         
         do i=1,6
            cwn_u(i) = vect(i)
            cwn_dedl(i) = ELoss(i)
         enddo
         
         cwn_nb = 0
         do i=1,vert_blocks
            do j=1,horz_blocks
               if (dEBloc(i,j).gt.0.001 .or. photBloc(i,j).gt.0) then
                  cwn_nb = cwn_nb + 1
                  cwn_bx(cwn_nb) = i
                  cwn_by(cwn_nb) = j
                  cwn_bg(cwn_nb) = photBloc(i,j)
                  cwn_be(cwn_nb) = dEBloc(i,j)
               endif
            enddo
         enddo
c         if (.not.(Eloss(4).eq.0.and.Eloss(1).eq.0.and.
c     1        cwn_nb.eq.0.and.photCer.eq.0))then
c         if(abs(ph-phi_test).gt.0.01)write(*,*)ph,phi_test
c         write(*,*) ' xsn = ',xsn,nclust,cwn_E_r(1),ph,phi_test,cer_h(1)
c         if(nclust.gt.0.and.abs(ph-90).lt.90.and.
c     ,        cer_h(1).gt.0.and.
c     ,        xsn.gt.0.and.cwn_E_r(1).gt.0.7)then
c            write(*,*)nclust
c            write(*,*) '# cerenkov photons:',photCer,photGood
c            write(56,*)zz_t,nn_t,0.938**2+
c     ,           2*0.938*(E_beam-E_coor)-
c     ,           2*E_beam*E_coor*(1-cos(THETA_C/180.*3.141)),
c     ,           xsn,E_coor,THETA_C,-EE,th*180/3.14159
c            write(*,*) 'pass  xsn = ',xsn,nclust,cer_h(1)
c               write(*,*) 'ntuple = ',nclust,cwn_th_r(1),cwn_ph_r(1)
            call hfnt(nt_geant)
c         endif
         call clear_cer()
         call clear_cal()
 
c         endif

c      write(*,*)'Next step'
cccc
cc
c     NEW NEURAL NETWORK PART
cc


c      write(*,*)EYX
c            write(*,*) VectorN(1),Emax

*      if (num_electrons+num_positrons.ge.2) write(*,*) electron_radius
*      if (num_gc.gt.0) then
*        do i=1,num_gc
*          write(*,*) gc_part(i),gc_x(i),gc_y(i),gc_eng(i)
*        enddo
*        write(*,*) 'gammas (n,r,<x>,<y>,eng)',num_gammas,gamma_radius,
*     1     gamma_ave_x,gamma_ave_y,gamma_eng
*        write(*,*) 'electrons (n,r,<x>,<y>,eng)',num_electrons,
*     1      electron_radius,electron_ave_x,electron_ave_y,electron_eng
*    cg*            write(*,*) 'positrons (n)',num_positrons
*      endif

C
 1101 CONTINUE
 111  format(2(1x,f6.3),3(1x,i2),1x,e13.5,8(f9.3,1x),5(f9.6,1x),3(i2,1x),2(f7.3,1x,f7.3,1x,f7.4,1x,f7.4,1x))
c      enddo 
      END
cccccccccccccccccccccccccccc
ccc
ccc
ccc
ccc
c$$$      subroutine NeuralParam(Emax,Emt,Etot9,Etot,
c$$$     ,     xmomsqr,xmom,xmomsq,ymomsqr,ymom,ymomsq,ixmax,iymax,Eyx,
c$$$     ,     XX,YY)    
c$$$      IMPLICIT NONE
c$$$c      include 'bigcal_data_structures.cmn'
c$$$c      include 'sane_data_structures.cmn'
c$$$c      include 'b_ntuple.cmn'
c$$$c      include 'sane_ntuple.cmn'
c$$$c      include 'gen_data_structures.cmn'
c$$$c      include 'gen_event_info.cmn'
c$$$
c$$$      real xmomsqr,xmom,xmomsq,ymomsqr,ymom,ymomsq
c$$$      integer jmax
c$$$      real emax
c$$$
c$$$c      real norma1(32,10),norma2(32,10),norma3(32,10),norma4(32,10),
c$$$c     ,     norma5(32,10),norma6(32,6),norm(56,32)
c$$$c      common/NOR1/norma1,norma2,norma3,norma4,norma5,norma6,norm
c$$$      real eyx(5,5),XX(5,5),YY(5,5),en,Emt,Etot9,Etot
c$$$      integer ixmax, iymax,jj,i,ii
c$$$      real Xmomf
c$$$      
c$$$
c$$$      emax=eyx(3,3)
c$$$c      write(*,*)"EMAX+ ",eyx(3,3)
c$$$      etot =0 
c$$$    
c$$$      etot9 =eyx(2,2)+eyx(2,3)+eyx(2,4)+
c$$$     ,     eyx(3,2)+eyx(3,3)+eyx(3,4)+
c$$$     ,     eyx(4,2)+eyx(4,3)+eyx(4,4)
c$$$      etot =eyx(1,1)+eyx(1,2)+eyx(1,3)+eyx(1,4)+eyx(1,5)+
c$$$     ,     eyx(2,1)+eyx(2,2)+eyx(2,3)+eyx(2,4)+eyx(2,5)+
c$$$     ,     eyx(3,1)+eyx(3,2)+eyx(3,3)+eyx(3,4)+eyx(3,5)+
c$$$     ,     eyx(4,1)+eyx(4,2)+eyx(4,3)+eyx(4,4)+eyx(4,5)+
c$$$     ,     eyx(5,1)+eyx(5,2)+eyx(5,3)+eyx(5,4)+eyx(5,5)
c$$$
c$$$      Emt = emax/Etot
c$$$    
c$$$      xmomsqr = XMomf(ixmax,iymax,eyx,xx,2);
c$$$      xmom    = XMomf(ixmax,iymax,eyx,xx,1);
c$$$      xmomsq  = XMomf(ixmax,iymax,eyx,xx,3);
c$$$      ymomsqr = XMomf(ixmax,iymax,eyx,yy,2);
c$$$      ymom    = XMomf(ixmax,iymax,eyx,yy,1);
c$$$      ymomsq  = XMomf(ixmax,iymax,eyx,yy,3);
c$$$c      write(*,*)xmomsqr,xmom,xmomsq,ymomsqr,ymom,ymomsq
c$$$
c$$$      end
c$$$
c$$$      real function XMomf(ix,iy,eyx,x,iflag)
c$$$      IMPLICIT NONE
c$$$c      include 'bigcal_data_structures.cmn'
c$$$c      include 'sane_data_structures.cmn'
c$$$c      include 'b_ntuple.cmn'
c$$$c      include 'sane_ntuple.cmn'
c$$$c      include 'gen_data_structures.cmn'
c$$$      real eyx(5,5),x(5,5)
c$$$      
c$$$      real w(5,5),Sum,SumW,Coor
c$$$      
c$$$      Integer i,j,icx,icy,ix,iy,iflag
c$$$      Sum=0
c$$$      do i=1,5
c$$$         do j=1,5
c$$$            icx = ix+(j-3)
c$$$            icy = iy+(i-3)
c$$$            if(icx.gt.0.and.icy.gt.0.and.
c$$$     ,           icy.lt.57.and.eyx(i,j).lt.5)then
c$$$               if(iflag.eq.1.and.eyx(i,j).lt.5)Sum = Sum+eyx(i,j)
c$$$               if(iflag.eq.2.and.eyx(i,j).lt.5)Sum = Sum+sqrt(eyx(i,j))
c$$$            if(iflag.eq.3.and.eyx(i,j).lt.5)Sum = Sum+eyx(i,j)*eyx(i,j)
c$$$               icx = ix+(j-3)
c$$$               icy = iy+(i-3)
c$$$               w(i,j) = x(i,j)-x(3,3)
c$$$               
c$$$            endif
c$$$         enddo
c$$$      enddo
c$$$      
c$$$      Coor=0
c$$$
c$$$      do i=1,5
c$$$         do j=1,5
c$$$            if(iflag.eq.1)Coor =Coor+w(i,j)*eyx(i,j)/Sum 
c$$$            if(iflag.eq.2)Coor =Coor+w(i,j)*sqrt(eyx(i,j))/Sum 
c$$$            if(iflag.eq.3)Coor =Coor+w(i,j)*eyx(i,j)*eyx(i,j)/Sum 
c$$$         enddo
c$$$      enddo
c$$$
c$$$      XMomf=Coor
c$$$
c$$$      end
c$$$
c$$$      include 'neurale.f'
c$$$      include 'sane_neurale.f'
c$$$      include 'sane_neuralx.f'
c$$$      include 'sane_neuraly.f'
      include 'sane_n100xye.f'
cccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine CORRECT_ANGLES(x,y,z,ee,th,phi,cer_stat,srx,sry)
      IMPLICIT NONE
c
c     X = X(Bigcal)-X(raster)
c     Y = Y(Bigcal)-Y(raster)
c     Z = Z(Bigcal)
c     EE - energy in GEV
c     RETURNS THeta and PHI In Degree.
c
cccccccccccc      
c      include 'sane_data_structures.cmn'
c      include 'gen_run_info.cmn'
      include 'sane.inc'
      include 'pmc.inc'
      real x,y,z
      real th,phi,thr,phr,ee,phk 
      real dist,SANE_BETA_OMEGA
      real*8 P_th(10),P_phi(10)
      real srx,sry

      integer cer_stat
      data P_th /
     ,     -2.199987805718,      1.312318933346,      0.644032653274,      
     ,     2.001711272282  ,    4.831055345667,
     ,     0.596870277140   ,   0.237530064696 ,    -0.444891749961,
     ,     -0.668604044519  ,   -1.988327254812/
      data P_Phi /
     ,     -1.206886920591,      3.898203794202,      1.409952555564 , 
     ,     -0.737821993549,      4.693839032660,
     ,     -0.853486677346,     -3.282568717839 ,     1.891695882259,    
     ,     1.158605334109 ,    -4.578605424909/

      dist = sqrt(x**2+y**2+z**2)
      thr  = acos(z/dist)
      phk = atan2(y/dist,x/dist)
      phr  = atan2(y/dist,x/dist)
c      write(*,*)1,thr*180/3.141,phr*180/3.141,y


c$$$         SANE_BETA_OMEGA = abs(theta_0 - abs(Theta_Bfield)) !! 1st attempt w/ ANN
c         SANE_BETA_OMEGA = 40!abs(theta_0 - abs(Theta_Bfield))
c         SANE_BETA_OMEGA = abs(theta_0 - Theta_Bfield)
         SANE_BETA_OMEGA = omega   !! Use definition from uginit.f
c      print*,X,Y,Z,DIST,thr,phr
c      print*,SANE_BETA_OMEGA
      if(cer_stat.gt.0)then

c      write(*,*)field_type       
c            write(*,*)SANE_BETA_OMEGA
         if(field_type.gt.0)then
c            write(*,*)SANE_BETA_OMEGA
            if(abs(SANE_BETA_OMEGA+40).lt.1.or.
     ,           abs(SANE_BETA_OMEGA-120).lt.1)then
c               write(*,*)'PERP CORRECTION'
               call POLYNOM_CORRECTION(SANE_TRANSFORM_MATRIX_THETA_140, 
     ,              SANE_TRANSFORM_MATRIX_PHI_140,thr,
     ,              phr,ee,th,phi,
     ,              srx,sry)
c               phi= -phi+phr*180/3.14159+phr*180/3.14159
c               th= -th+thr*180/3.14159+thr*180/3.14159
            elseif (abs(SANE_BETA_OMEGA+140).lt.1.or.
     ,              abs(SANE_BETA_OMEGA-40).lt.1)then 
c               write(*,*)'Para CORRECTION'
c               write(*,*)SANE_TRANSFORM_MATRIX_THETA_140

               call POLYNOM_CORRECTION(SANE_TRANSFORM_MATRIX_THETA_40, 
     ,              SANE_TRANSFORM_MATRIX_PHI_40,thr,
     ,              phr,ee,th,phi,
     ,              srx,sry)
c               phi= -phi+phr*180/3.14159+phr*180/3.14159
c               th= -th+thr*180/3.14159+thr*180/3.14159
            else
               WRITE(*,*)'WARNING : YOU DON T' 
               WRITE(*,*)'HAVE SANE_TRANSFORM_MATRIX CONSTANTS'
               
            endif
c            write(*,*)phi,phr*180/3.141
            
         else
            phi=phr*180/3.14159
            th = thr*180/3.14159
         endif
      else
c            phi=phr*180/3.14159
c            th = thr*180/3.14159
         th  = THR*180/3.1415926+
     ,        (P_th(1)+P_th(2)*phr+P_th(3)*thr+
     ,        P_th(4)*phr**2+P_th(5)*thr**2)/EE+
     ,        (P_th(6)+P_th(7)*thr+P_th(8)*phr+
     ,        P_th(9)*phr**2+P_th(10)*thr**2)/EE**2
         phi = phR*180/3.1415926
     ,        +(P_phi(1)+P_phi(2)*phr+P_phi(3)*phr**2+
     ,        P_phi(4)*phr**3+P_phi(5)*thr+P_phi(6)*thr**2+
     ,        P_phi(7)*thr**3+P_phi(8)*phr*thr+
     ,        P_phi(9)*phr**2*thr+
     ,        P_phi(10)*phr*thr**2)
         
      endif

      phi=phi
c      write(*,*)phr*180/3.141-90,phi,SANE_BETA_OMEGA
   
c      write(*,*)thr*180/3.1415926,th,phr*180/3.1415926
c      print*,thr*(180.d0/3.1415926),phr*(180.d0/3.1415926),th,phi
c     ,      ,SANE_BETA_OMEGA

      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine POLYNOM_CORRECTION(P_th,P_phi,thr,phr,eMev,th,phi,
     ,                              srx,sry)
      IMPLICIT NONE
      
c
c     Input patameters are P(26) -transformation Matrix
c     input thr and phr angles from CALORIMETER ,THr and PHr in radians
c     EE Energy in GEV
c     Output :TH and Phi Correctes in degrees
cc      
ccccccc
c      include 'sane.inc'
c      include 'pmc.inc'
      real th,phi,thr,phr,eMev,ee
      real omega_beta
      real P_th(15),P_phi(15),cosom,sinom,srx,sry
      ee=eMeV
c      cosom = cos(omega_beta*3.1415926/180.d0)
c      sinom = sin(omega_beta*3.1415926/180.d0)
c      write(*,*)ee
 
         th  = thr*180/3.1415926+
     ,           (p_th(1)+p_th(2)*thr+p_th(3)*phr+p_th(4)*thr**2+
     ,           p_th(5)*phr**2+p_th(6)*thr*phr)*
     ,           (p_th(7)+p_th(8)/EE+p_th(9)/EE**2)*
     ,           (p_th(10)+p_th(11)*srx+p_th(12)*srx**2)*
     ,           (p_th(13)+p_th(14)*sry+p_th(15)*sry**2)




         phi = phr*180/3.1415926+
     ,           (p_phi(1)+p_phi(2)*thr+p_phi(3)*phr+p_phi(4)*thr**2+
     ,           p_phi(5)*phr**2+p_phi(6)*thr*phr)*
     ,           (p_phi(7)+p_phi(8)/EE+p_phi(9)/EE**2)*
     ,           (p_phi(10)+p_phi(11)*srx+p_phi(12)*srx**2)*
     ,           (p_phi(13)+p_phi(14)*sry+p_phi(15)*sry**2)
              end


cccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc Uncorrected angles, for comparison

      Subroutine UNCORRECT_ANGLES(x,y,z,ee,thu,phiu)
      IMPLICIT NONE
c
c     X = X(Bigcal)-X(raster)
c     Y = Y(Bigcal)-Y(raster)
c     Z = Z(Bigcal)
c     EE - energy in GEV
c     RETURNS THeta and PHI In Degree.
c
cccccccccccc      
c      include 'sane_data_structures.cmn'
c      include 'gen_run_info.cmn'
      include 'sane.inc'
      include 'pmc.inc'
      real x,y,z
      real thu,phiu,thr,phr,ee 
      real dist

      dist = sqrt(x**2+y**2+z**2)
      thr  = acos(z/dist)
      phr  = atan2(y/dist,x/dist)
c      write(*,*)1,thr*180/3.141,phr*180/3.141,y

      thu  = thr*(180/3.141592) 
      phiu = phr*(180/3.141592) 
         
c      write(*,*)'uncorrected',thu,phiu 

      end

cccccccccccc      
cccccccccccc      
      subroutine NANcheckF(l,did)
      IMPLICIT NONE
      real*4 l
      integer did
      if(l.ne.l)then
         l=0
         write(*,*)'CHECK NAN ',did
      endif
      end

c      subroutine Blockzero
c$$$      do i=1,32
c$$$         do j=1,56
c$$$            Energy(i,j)=0
c$$$         enddo
c$$$      enddo
