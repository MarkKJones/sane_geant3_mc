      subroutine radiated_xn_he_47(wsqtest,qsqtest,radval)
c
c
      implicit none
      real radval
      real*8 e,ep,theta,xbnuc,sigborn,sigrad_elas
      real*8 sigrad_qe,sigrad_inel,t5,t6,t1
      integer t2,t3,t4
      real*8 Mp,Wsq,wsqtest,wtest,qsqtest,radrat,sigrad_all
      integer i,j
      character*100 filename1
      character*100 filename2
      character*100 filename3
      character*100 filename4
c   
      integer wpt_he_47_1,q2pt_he_47_1
      data filename1 /'extern_files/extern_He_4.73_W2.0-2.8_Q20.8-1.7.out'/
      parameter (wpt_he_47_1=9)
      parameter (q2pt_he_47_1=10)
      real*8 wlo_he_47_1,whi_he_47_1,q2lo_he_47_1,q2hi_he_47_1
      real*8 w_he_47_1(wpt_he_47_1),q2_he_47_1(q2pt_he_47_1),fac_he_47_1(wpt_he_47_1,q2pt_he_47_1)
      real*8 y2_he_47_1(wpt_he_47_1,q2pt_he_47_1)
c   
      integer wpt_he_47_2,q2pt_he_47_2
      data filename2 /'extern_files/extern_He_4.73_W1.5-2.6_Q21.6-2.7.out'/
      parameter (wpt_he_47_2=12)
      parameter (q2pt_he_47_2=12)
      real*8 wlo_he_47_2,whi_he_47_2,q2lo_he_47_2,q2hi_he_47_2
      real*8 w_he_47_2(wpt_he_47_2),q2_he_47_2(q2pt_he_47_2),fac_he_47_2(wpt_he_47_2,q2pt_he_47_2)
      real*8 y2_he_47_2(wpt_he_47_2,q2pt_he_47_2)
c 
      integer wpt_he_47_3,q2pt_he_47_3
      data filename3 /'extern_files/extern_He_4.73_W1.2-2.4_Q22.7-3.6.out'/
      parameter (wpt_he_47_3=13)
      parameter (q2pt_he_47_3=10)
      real*8 wlo_he_47_3,whi_he_47_3,q2lo_he_47_3,q2hi_he_47_3
      real*8 w_he_47_3(wpt_he_47_3),q2_he_47_3(q2pt_he_47_3),fac_he_47_3(wpt_he_47_3,q2pt_he_47_3)
      real*8 y2_he_47_3(wpt_he_47_3,q2pt_he_47_3)
c 
      integer wpt_he_47_4,q2pt_he_47_4
      data filename4 /'extern_files/extern_He_4.73_W1.1-2.0_Q23.5-5.0.out'/
      parameter (wpt_he_47_4=10)
      parameter (q2pt_he_47_4=16)
      real*8 wlo_he_47_4,whi_he_47_4,q2lo_he_47_4,q2hi_he_47_4
      real*8 w_he_47_4(wpt_he_47_4),q2_he_47_4(q2pt_he_47_4),fac_he_47_4(wpt_he_47_4,q2pt_he_47_4)
      real*8 y2_he_47_4(wpt_he_47_4,q2pt_he_47_4)
c
      logical first
      data first /.true./
      common /radxn_he_47_1/ w_he_47_1,q2_he_47_1,fac_he_47_1,y2_he_47_1
     > ,wlo_he_47_1,whi_he_47_1,q2lo_he_47_1,q2hi_he_47_1,first
      common /radxn_he_47_2/ w_he_47_2,q2_he_47_2,fac_he_47_2,y2_he_47_2
     > ,wlo_he_47_2,whi_he_47_2,q2lo_he_47_2,q2hi_he_47_2
      common /radxn_he_47_3/ w_he_47_3,q2_he_47_3,fac_he_47_3,y2_he_47_3
     > ,wlo_he_47_3,whi_he_47_3,q2lo_he_47_3,q2hi_he_47_3
      common /radxn_he_47_4/ w_he_47_4,q2_he_47_4,fac_he_47_4,y2_he_47_4
     > ,wlo_he_47_4,whi_he_47_4,q2lo_he_47_4,q2hi_he_47_4
c
      mp=0.93827
      radrat = 0. 
      if (first) then
         WRITE(*,*) ' READING IN THE 1st RADIATED XN FILE'
         write(*,*) filename1
      open(unit=21,file=filename1)
      read(21,*)wlo_he_47_1,whi_he_47_1,q2lo_he_47_1,q2hi_he_47_1 
      write(*,*) wpt_he_47_1,q2pt_he_47_1,wlo_he_47_1,whi_he_47_1,q2lo_he_47_1,q2hi_he_47_1
       do i=1,wpt_he_47_1
        do j=1,q2pt_he_47_1
         read(21,'(1x,f7.3,f7.4,f7.3,2f7.4,6e11.4,3i3,2e11.4)') 
     &            e,ep,theta,xbnuc,q2_he_47_1(j),sigborn,sigrad_all,
     &            sigrad_elas,sigrad_qe,sigrad_inel,t1,t2,t3,t4,t5,t6
        Wsq = Mp**2 + 2d0*Mp*(e-ep) - q2_he_47_1(j)
        fac_he_47_1(i,j) = sigrad_all/sigborn
         if(Wsq.gt.0) then
          W_he_47_1(i) = sqrt(Wsq)
         else
          write(*,*) ' problem with W2 = ',Wsq
          stop
         endif
         write(83,101) W_he_47_1(i),q2_he_47_1(j),fac_he_47_1(i,j)
     >,ep,theta,sigborn
       enddo
       enddo    
      close(21)
c
         WRITE(*,*) ' READING IN THE 2nd RADIATED XN FILE'
         write(*,*) filename2
      open(unit=21,file=filename2)
      read(21,*)wlo_he_47_2,whi_he_47_2,q2lo_he_47_2,q2hi_he_47_2 
      write(*,*) wpt_he_47_2,q2pt_he_47_2,wlo_he_47_2,whi_he_47_2,q2lo_he_47_2,q2hi_he_47_2 
       do i=1,wpt_he_47_2
        do j=1,q2pt_he_47_2
         read(21,'(1x,f7.3,f7.4,f7.3,2f7.4,6e11.4,3i3,2e11.4)') 
     &            e,ep,theta,xbnuc,q2_he_47_2(j),sigborn,sigrad_all,
     &            sigrad_elas,sigrad_qe,sigrad_inel,t1,t2,t3,t4,t5,t6
       Wsq = Mp**2 + 2d0*Mp*(e-ep) - q2_he_47_2(j)
        fac_he_47_2(i,j) = sigrad_all/sigborn
         if(Wsq.gt.0) then
          W_he_47_2(i) = sqrt(Wsq)
         else
          write(*,*) ' problem with W2 = ',Wsq
          stop
         endif
         write(83,101) W_he_47_2(i),q2_he_47_2(j),fac_he_47_2(i,j)
     >,ep,theta,sigborn
       enddo
       enddo    
c
         WRITE(*,*) ' READING IN THE 3rd RADIATED XN FILE'
      close(21)
         write(*,*) filename3
      open(unit=21,file=filename3)
      read(21,*)wlo_he_47_3,whi_he_47_3,q2lo_he_47_3,q2hi_he_47_3 
      write(*,*) wpt_he_47_3,q2pt_he_47_3,wlo_he_47_3,whi_he_47_3,q2lo_he_47_3,q2hi_he_47_3 
       do i=1,wpt_he_47_3
        do j=1,q2pt_he_47_3
         read(21,'(1x,f7.3,f7.4,f7.3,2f7.4,6e11.4,3i3,2e11.4)') 
     &            e,ep,theta,xbnuc,q2_he_47_3(j),sigborn,sigrad_all,
     &            sigrad_elas,sigrad_qe,sigrad_inel,t1,t2,t3,t4,t5,t6
        Wsq = Mp**2 + 2d0*Mp*(e-ep) - q2_he_47_3(j)
        fac_he_47_3(i,j) = sigrad_all/sigborn
         if(Wsq.gt.0) then
          W_he_47_3(i) = sqrt(Wsq)
         else
          write(*,*) ' problem with W2 = ',Wsq
          stop
         endif
         write(83,101) W_he_47_3(i),q2_he_47_3(j),fac_he_47_3(i,j)
     >,ep,theta,sigborn
       enddo
       enddo    
c
         WRITE(*,*) ' READING IN THE 4th RADIATED XN FILE'
      close(21)
         write(*,*) filename4
      open(unit=21,file=filename4)
      read(21,*)wlo_he_47_4,whi_he_47_4,q2lo_he_47_4,q2hi_he_47_4 
      write(*,*) wpt_he_47_4,q2pt_he_47_4,wlo_he_47_4,whi_he_47_4,q2lo_he_47_4,q2hi_he_47_4 
       do i=1,wpt_he_47_4
        do j=1,q2pt_he_47_4
         read(21,'(1x,f7.3,f7.4,f7.3,2f7.4,6e11.4,3i3,2e11.4)') 
     &            e,ep,theta,xbnuc,q2_he_47_4(j),sigborn,sigrad_all,
     &            sigrad_elas,sigrad_qe,sigrad_inel,t1,t2,t3,t4,t5,t6
        Wsq = Mp**2 + 2d0*Mp*(e-ep) - q2_he_47_4(j)
        fac_he_47_4(i,j) = sigrad_all/sigborn
         if(Wsq.gt.0) then
          W_he_47_4(i) = sqrt(Wsq)
         else
          write(*,*) ' problem with W2 = ',Wsq
          stop
         endif
         write(83,101) W_he_47_4(i),q2_he_47_4(j),fac_he_47_4(i,j)
     >,ep,theta,sigborn
 101     format(6(f10.5,','))
       enddo
       enddo    
c
       FIRST=.FALSE.      
c  calc 2nd derivative in y2. Call only once.
       call splie2(w_he_47_1,q2_he_47_1,fac_he_47_1,wpt_he_47_1,q2pt_he_47_1,y2_he_47_1)     
       call splie2(w_he_47_2,q2_he_47_2,fac_he_47_2,wpt_he_47_2,q2pt_he_47_2,y2_he_47_2)     
       call splie2(w_he_47_3,q2_he_47_3,fac_he_47_3,wpt_he_47_3,q2pt_he_47_3,y2_he_47_3)     
       call splie2(w_he_47_4,q2_he_47_4,fac_he_47_4,wpt_he_47_4,q2pt_he_47_4,y2_he_47_4)     
      endif
C
      wtest=sqrt(wsqtest)
      if ( wtest .gt. wlo_he_47_1 .and. wtest .lt. whi_he_47_1 .and.
     >  qsqtest .gt. q2lo_he_47_1 .and. qsqtest .lt. q2hi_he_47_1) then
      call splin2(w_he_47_1,q2_he_47_1,fac_he_47_1,y2_he_47_1,wpt_he_47_1,q2pt_he_47_1,wtest,qsqtest,radrat)
      endif
      if ( wtest .gt. wlo_he_47_2 .and. wtest .lt. whi_he_47_2 .and.
     >  qsqtest .gt. q2lo_he_47_2 .and. qsqtest .lt. q2hi_he_47_2) then
      call splin2(w_he_47_2,q2_he_47_2,fac_he_47_2,y2_he_47_2,wpt_he_47_2,q2pt_he_47_2,wtest,qsqtest,radrat)
      endif
      if ( wtest .gt. wlo_he_47_3 .and. wtest .lt. whi_he_47_3 .and.
     >  qsqtest .gt. q2lo_he_47_3 .and. qsqtest .lt. q2hi_he_47_3) then
      call splin2(w_he_47_3,q2_he_47_3,fac_he_47_3,y2_he_47_3,wpt_he_47_3,q2pt_he_47_3,wtest,qsqtest,radrat)
      endif
      if ( wtest .gt. wlo_he_47_4 .and. wtest .lt. whi_he_47_4 .and.
     >  qsqtest .gt. q2lo_he_47_4 .and. qsqtest .lt. q2hi_he_47_4) then
      call splin2(w_he_47_4,q2_he_47_4,fac_he_47_4,y2_he_47_4,wpt_he_47_4,q2pt_he_47_4,wtest,qsqtest,radrat)
      endif
c
      radval=radrat
      return
      end
