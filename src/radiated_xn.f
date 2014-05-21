      subroutine radiated_xn(wsqtest,qsqtest,radval)
c
c
      implicit none
      real radval
      real*8 e,ep,theta,xbnuc,sigborn,sigrad_elas
      real*8 sigrad_qe,sigrad_inel,t5,t6,t1
      integer t2,t3,t4
      real*8 Mp,Wsq,wsqtest,wtest,qsqtest,radrat,sigrad_all
      integer i,j
      character*100 filename
c   extern_H2_4.73_W2.0-2.6_Q21.2-2.5.out
      integer wpt_h2_47_1,q2pt_h2_47_1
      parameter (wpt_h2_47_1=7)
      parameter (q2pt_h2_47_1=14)
      real*8 wlo_h2_47_1,whi_h2_47_1,q2lo_h2_47_1,q2hi_h2_47_1
      real*8 w_h2_47_1(wpt_h2_47_1),q2_h2_47_1(q2pt_h2_47_1),fac_h2_47_1(wpt_h2_47_1,q2pt_h2_47_1)
      real*8 y2_h2_47_1(wpt_h2_47_1,q2pt_h2_47_1)
c   extern_H2_4.73_W1.5-2.6_Q21.9-2.7.out
      integer wpt_h2_47_2,q2pt_h2_47_2
      parameter (wpt_h2_47_2=12)
      parameter (q2pt_h2_47_2=9)
      real*8 wlo_h2_47_2,whi_h2_47_2,q2lo_h2_47_2,q2hi_h2_47_2
      real*8 w_h2_47_2(wpt_h2_47_2),q2_h2_47_2(q2pt_h2_47_2),fac_h2_47_2(wpt_h2_47_2,q2pt_h2_47_2)
      real*8 y2_h2_47_2(wpt_h2_47_2,q2pt_h2_47_2)
c extern_files/extern_H2_4.73_W1.2-2.4_Q22.7-3.5.out
      integer wpt_h2_47_3,q2pt_h2_47_3
      parameter (wpt_h2_47_3=13)
      parameter (q2pt_h2_47_3=9)
      real*8 wlo_h2_47_3,whi_h2_47_3,q2lo_h2_47_3,q2hi_h2_47_3
      real*8 w_h2_47_3(wpt_h2_47_3),q2_h2_47_3(q2pt_h2_47_3),fac_h2_47_3(wpt_h2_47_3,q2pt_h2_47_3)
      real*8 y2_h2_47_3(wpt_h2_47_3,q2pt_h2_47_3)
c extern_H2_4.73_W1.2-1.9_Q23.5-5.0.out
      integer wpt_h2_47_4,q2pt_h2_47_4
      parameter (wpt_h2_47_4=8)
      parameter (q2pt_h2_47_4=16)
      real*8 wlo_h2_47_4,whi_h2_47_4,q2lo_h2_47_4,q2hi_h2_47_4
      real*8 w_h2_47_4(wpt_h2_47_4),q2_h2_47_4(q2pt_h2_47_4),fac_h2_47_4(wpt_h2_47_4,q2pt_h2_47_4)
      real*8 y2_h2_47_4(wpt_h2_47_4,q2pt_h2_47_4)
c
      logical first
      data first /.true./
      common /radxn_h2_47_1/ w_h2_47_1,q2_h2_47_1,fac_h2_47_1,y2_h2_47_1
     > ,wlo_h2_47_1,whi_h2_47_1,q2lo_h2_47_1,q2hi_h2_47_1,first
      common /radxn_h2_47_2/ w_h2_47_2,q2_h2_47_2,fac_h2_47_2,y2_h2_47_2
     > ,wlo_h2_47_2,whi_h2_47_2,q2lo_h2_47_2,q2hi_h2_47_2
      common /radxn_h2_47_3/ w_h2_47_3,q2_h2_47_3,fac_h2_47_3,y2_h2_47_3
     > ,wlo_h2_47_3,whi_h2_47_3,q2lo_h2_47_3,q2hi_h2_47_3
      common /radxn_h2_47_4/ w_h2_47_4,q2_h2_47_4,fac_h2_47_4,y2_h2_47_4
     > ,wlo_h2_47_4,whi_h2_47_4,q2lo_h2_47_4,q2hi_h2_47_4
c
      mp=0.93827
      radrat = 0. 
      if (first) then
      filename='extern_files/extern_H2_4.73_W2.0-2.6_Q21.2-2.5.out'
         WRITE(*,*) ' READING IN THE 1st RADIATED XN FILE'
         write(*,*) filename
      open(unit=21,file=filename)
      read(21,*)wlo_h2_47_1,whi_h2_47_1,q2lo_h2_47_1,q2hi_h2_47_1 
      write(*,*) wpt_h2_47_1,q2pt_h2_47_1,wlo_h2_47_1,whi_h2_47_1,q2lo_h2_47_1,q2hi_h2_47_1
       do i=1,wpt_h2_47_1
        do j=1,q2pt_h2_47_1
         read(21,'(1x,f7.3,f7.4,f7.3,2f7.4,6e11.4,3i3,2e11.4)') 
     &            e,ep,theta,xbnuc,q2_h2_47_1(j),sigborn,sigrad_all,
     &            sigrad_elas,sigrad_qe,sigrad_inel,t1,t2,t3,t4,t5,t6
        Wsq = Mp**2 + 2d0*Mp*(e-ep) - q2_h2_47_1(j)
        fac_h2_47_1(i,j) = sigrad_all/sigborn
         if(Wsq.gt.0) then
          W_h2_47_1(i) = sqrt(Wsq)
         else
          write(*,*) ' problem with W2 = ',Wsq
          stop
         endif
       enddo
       enddo    
      close(21)
c
         WRITE(*,*) ' READING IN THE 2nd RADIATED XN FILE'
      open(unit=21,file='extern_files/extern_H2_4.73_W1.5-2.6_Q21.9-2.7.out')
      read(21,*)wlo_h2_47_2,whi_h2_47_2,q2lo_h2_47_2,q2hi_h2_47_2 
      write(*,*) wpt_h2_47_2,q2pt_h2_47_2,wlo_h2_47_2,whi_h2_47_2,q2lo_h2_47_2,q2hi_h2_47_2 
       do i=1,wpt_h2_47_2
        do j=1,q2pt_h2_47_2
         read(21,'(1x,f7.3,f7.4,f7.3,2f7.4,6e11.4,3i3,2e11.4)') 
     &            e,ep,theta,xbnuc,q2_h2_47_2(j),sigborn,sigrad_all,
     &            sigrad_elas,sigrad_qe,sigrad_inel,t1,t2,t3,t4,t5,t6
       Wsq = Mp**2 + 2d0*Mp*(e-ep) - q2_h2_47_2(j)
        fac_h2_47_2(i,j) = sigrad_all/sigborn
         if(Wsq.gt.0) then
          W_h2_47_2(i) = sqrt(Wsq)
         else
          write(*,*) ' problem with W2 = ',Wsq
          stop
         endif
       enddo
       enddo    
c
         WRITE(*,*) ' READING IN THE 3rd RADIATED XN FILE'
      close(21)
      open(unit=21,file='extern_files/extern_H2_4.73_W1.2-2.4_Q22.7-3.5.out')
      read(21,*)wlo_h2_47_3,whi_h2_47_3,q2lo_h2_47_3,q2hi_h2_47_3 
      write(*,*) wpt_h2_47_3,q2pt_h2_47_3,wlo_h2_47_3,whi_h2_47_3,q2lo_h2_47_3,q2hi_h2_47_3 
       do i=1,wpt_h2_47_3
        do j=1,q2pt_h2_47_3
         read(21,'(1x,f7.3,f7.4,f7.3,2f7.4,6e11.4,3i3,2e11.4)') 
     &            e,ep,theta,xbnuc,q2_h2_47_3(j),sigborn,sigrad_all,
     &            sigrad_elas,sigrad_qe,sigrad_inel,t1,t2,t3,t4,t5,t6
        Wsq = Mp**2 + 2d0*Mp*(e-ep) - q2_h2_47_3(j)
        fac_h2_47_3(i,j) = sigrad_all/sigborn
         if(Wsq.gt.0) then
          W_h2_47_3(i) = sqrt(Wsq)
         else
          write(*,*) ' problem with W2 = ',Wsq
          stop
         endif
       enddo
       enddo    
c
         WRITE(*,*) ' READING IN THE 4th RADIATED XN FILE'
      close(21)
      open(unit=21,file='extern_files/extern_H2_4.73_W1.2-1.9_Q23.5-5.0.out')
      read(21,*)wlo_h2_47_4,whi_h2_47_4,q2lo_h2_47_4,q2hi_h2_47_4 
      write(*,*) wpt_h2_47_4,q2pt_h2_47_4,wlo_h2_47_4,whi_h2_47_4,q2lo_h2_47_4,q2hi_h2_47_4 
       do i=1,wpt_h2_47_4
        do j=1,q2pt_h2_47_4
         read(21,'(1x,f7.3,f7.4,f7.3,2f7.4,6e11.4,3i3,2e11.4)') 
     &            e,ep,theta,xbnuc,q2_h2_47_4(j),sigborn,sigrad_all,
     &            sigrad_elas,sigrad_qe,sigrad_inel,t1,t2,t3,t4,t5,t6
        Wsq = Mp**2 + 2d0*Mp*(e-ep) - q2_h2_47_4(j)
        fac_h2_47_4(i,j) = sigrad_all/sigborn
         if(Wsq.gt.0) then
          W_h2_47_4(i) = sqrt(Wsq)
         else
          write(*,*) ' problem with W2 = ',Wsq
          stop
         endif
       enddo
       enddo    
c
       FIRST=.FALSE.      
c  calc 2nd derivative in y2. Call only once.
       call splie2(w_h2_47_1,q2_h2_47_1,fac_h2_47_1,wpt_h2_47_1,q2pt_h2_47_1,y2_h2_47_1)     
       call splie2(w_h2_47_2,q2_h2_47_2,fac_h2_47_2,wpt_h2_47_2,q2pt_h2_47_2,y2_h2_47_2)     
       call splie2(w_h2_47_3,q2_h2_47_3,fac_h2_47_3,wpt_h2_47_3,q2pt_h2_47_3,y2_h2_47_3)     
       call splie2(w_h2_47_4,q2_h2_47_4,fac_h2_47_4,wpt_h2_47_4,q2pt_h2_47_4,y2_h2_47_4)     
      endif
C
      wtest=sqrt(wsqtest)
      if ( wtest .gt. wlo_h2_47_1 .and. wtest .lt. whi_h2_47_1 .and.
     >  qsqtest .gt. q2lo_h2_47_1 .and. qsqtest .lt. q2hi_h2_47_1) then
      call splin2(w_h2_47_1,q2_h2_47_1,fac_h2_47_1,y2_h2_47_1,wpt_h2_47_1,q2pt_h2_47_1,wtest,qsqtest,radrat)
      endif
      if ( wtest .gt. wlo_h2_47_2 .and. wtest .lt. whi_h2_47_2 .and.
     >  qsqtest .gt. q2lo_h2_47_2 .and. qsqtest .lt. q2hi_h2_47_2) then
      call splin2(w_h2_47_2,q2_h2_47_2,fac_h2_47_2,y2_h2_47_2,wpt_h2_47_2,q2pt_h2_47_2,wtest,qsqtest,radrat)
      endif
      if ( wtest .gt. wlo_h2_47_3 .and. wtest .lt. whi_h2_47_3 .and.
     >  qsqtest .gt. q2lo_h2_47_3 .and. qsqtest .lt. q2hi_h2_47_3) then
      call splin2(w_h2_47_3,q2_h2_47_3,fac_h2_47_3,y2_h2_47_3,wpt_h2_47_3,q2pt_h2_47_3,wtest,qsqtest,radrat)
      endif
      if ( wtest .gt. wlo_h2_47_4 .and. wtest .lt. whi_h2_47_4 .and.
     >  qsqtest .gt. q2lo_h2_47_4 .and. qsqtest .lt. q2hi_h2_47_4) then
      call splin2(w_h2_47_4,q2_h2_47_4,fac_h2_47_4,y2_h2_47_4,wpt_h2_47_4,q2pt_h2_47_4,wtest,qsqtest,radrat)
      endif
c
      radval=radrat
      return
      end
