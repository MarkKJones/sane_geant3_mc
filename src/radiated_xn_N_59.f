      subroutine radiated_xn_N_59(wsqtest,qsqtest,radval)
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
      integer wpt_N_59_1,q2pt_N_59_1
      data filename1 /'extern_files/extern_N_5.89_W2.2-3.0_Q21.2-2.5.out'/
      parameter (wpt_N_59_1=9)
      parameter (q2pt_N_59_1=14)
      real*8 wlo_N_59_1,whi_N_59_1,q2lo_N_59_1,q2hi_N_59_1
      real*8 w_N_59_1(wpt_N_59_1),q2_N_59_1(q2pt_N_59_1),fac_N_59_1(wpt_N_59_1,q2pt_N_59_1)
      real*8 y2_N_59_1(wpt_N_59_1,q2pt_N_59_1)
c   
      integer wpt_N_59_2,q2pt_N_59_2
      data filename2 /'extern_files/extern_N_5.89_W2.1-2.9_Q21.9-3.2.out'/
      parameter (wpt_N_59_2=9)
      parameter (q2pt_N_59_2=14)
      real*8 wlo_N_59_2,whi_N_59_2,q2lo_N_59_2,q2hi_N_59_2
      real*8 w_N_59_2(wpt_N_59_2),q2_N_59_2(q2pt_N_59_2),fac_N_59_2(wpt_N_59_2,q2pt_N_59_2)
      real*8 y2_N_59_2(wpt_N_59_2,q2pt_N_59_2)
c 
      integer wpt_N_59_3,q2pt_N_59_3
      data filename3 /'extern_files/extern_N_5.89_W1.2-2.7_Q23.0-4.3.out'/
      parameter (wpt_N_59_3=16)
      parameter (q2pt_N_59_3=14)
      real*8 wlo_N_59_3,whi_N_59_3,q2lo_N_59_3,q2hi_N_59_3
      real*8 w_N_59_3(wpt_N_59_3),q2_N_59_3(q2pt_N_59_3),fac_N_59_3(wpt_N_59_3,q2pt_N_59_3)
      real*8 y2_N_59_3(wpt_N_59_3,q2pt_N_59_3)
c 
      integer wpt_N_59_4,q2pt_N_59_4
      data filename4 /'extern_files/extern_N_5.89_W1.2-2.2_Q24.3-6.2.out'/
      parameter (wpt_N_59_4=11)
      parameter (q2pt_N_59_4=20)
      real*8 wlo_N_59_4,whi_N_59_4,q2lo_N_59_4,q2hi_N_59_4
      real*8 w_N_59_4(wpt_N_59_4),q2_N_59_4(q2pt_N_59_4),fac_N_59_4(wpt_N_59_4,q2pt_N_59_4)
      real*8 y2_N_59_4(wpt_N_59_4,q2pt_N_59_4)
c
      logical first
      data first /.true./
      common /radxn_N_59_1/ w_N_59_1,q2_N_59_1,fac_N_59_1,y2_N_59_1
     > ,wlo_N_59_1,whi_N_59_1,q2lo_N_59_1,q2hi_N_59_1,first
      common /radxn_N_59_2/ w_N_59_2,q2_N_59_2,fac_N_59_2,y2_N_59_2
     > ,wlo_N_59_2,whi_N_59_2,q2lo_N_59_2,q2hi_N_59_2
      common /radxn_N_59_3/ w_N_59_3,q2_N_59_3,fac_N_59_3,y2_N_59_3
     > ,wlo_N_59_3,whi_N_59_3,q2lo_N_59_3,q2hi_N_59_3
      common /radxn_N_59_4/ w_N_59_4,q2_N_59_4,fac_N_59_4,y2_N_59_4
     > ,wlo_N_59_4,whi_N_59_4,q2lo_N_59_4,q2hi_N_59_4
c
      mp=0.93827
      radrat = 0. 
      if (first) then
         WRITE(*,*) ' READING IN THE 1st RADIATED XN FILE'
         write(*,*) filename1
      open(unit=21,file=filename1)
      read(21,*)wlo_N_59_1,whi_N_59_1,q2lo_N_59_1,q2hi_N_59_1 
      write(*,*) wpt_N_59_1,q2pt_N_59_1,wlo_N_59_1,whi_N_59_1,q2lo_N_59_1,q2hi_N_59_1
       do i=1,wpt_N_59_1
        do j=1,q2pt_N_59_1
         read(21,'(1x,f7.3,f7.4,f7.3,2f7.4,6e11.4,3i3,2e11.4)') 
     &            e,ep,theta,xbnuc,q2_N_59_1(j),sigborn,sigrad_all,
     &            sigrad_elas,sigrad_qe,sigrad_inel,t1,t2,t3,t4,t5,t6
        Wsq = Mp**2 + 2d0*Mp*(e-ep) - q2_N_59_1(j)
        fac_N_59_1(i,j) = sigrad_all/sigborn
         if(Wsq.gt.0) then
          W_N_59_1(i) = sqrt(Wsq)
         else
          write(*,*) ' problem with W2 = ',Wsq
          stop
         endif
         write(82,101) W_N_59_1(i),q2_N_59_1(j),fac_N_59_1(i,j)
     >,ep,theta,sigborn
 101     format(6(f10.5,','))
       enddo
       enddo    
      close(21)
c
         WRITE(*,*) ' READING IN THE 2nd RADIATED XN FILE'
         write(*,*) filename2
      open(unit=21,file=filename2)
      read(21,*)wlo_N_59_2,whi_N_59_2,q2lo_N_59_2,q2hi_N_59_2 
      write(*,*) wpt_N_59_2,q2pt_N_59_2,wlo_N_59_2,whi_N_59_2,q2lo_N_59_2,q2hi_N_59_2 
       do i=1,wpt_N_59_2
        do j=1,q2pt_N_59_2
         read(21,'(1x,f7.3,f7.4,f7.3,2f7.4,6e11.4,3i3,2e11.4)') 
     &            e,ep,theta,xbnuc,q2_N_59_2(j),sigborn,sigrad_all,
     &            sigrad_elas,sigrad_qe,sigrad_inel,t1,t2,t3,t4,t5,t6
       Wsq = Mp**2 + 2d0*Mp*(e-ep) - q2_N_59_2(j)
        fac_N_59_2(i,j) = sigrad_all/sigborn
         if(Wsq.gt.0) then
          W_N_59_2(i) = sqrt(Wsq)
         else
          write(*,*) ' problem with W2 = ',Wsq
          stop
         endif
         write(82,101) W_N_59_2(i),q2_N_59_2(j),fac_N_59_2(i,j)
     >,ep,theta,sigborn
       enddo
       enddo    
c
         WRITE(*,*) ' READING IN THE 3rd RADIATED XN FILE'
      close(21)
         write(*,*) filename3
      open(unit=21,file=filename3)
      read(21,*)wlo_N_59_3,whi_N_59_3,q2lo_N_59_3,q2hi_N_59_3 
      write(*,*) wpt_N_59_3,q2pt_N_59_3,wlo_N_59_3,whi_N_59_3,q2lo_N_59_3,q2hi_N_59_3 
       do i=1,wpt_N_59_3
        do j=1,q2pt_N_59_3
         read(21,'(1x,f7.3,f7.4,f7.3,2f7.4,6e11.4,3i3,2e11.4)') 
     &            e,ep,theta,xbnuc,q2_N_59_3(j),sigborn,sigrad_all,
     &            sigrad_elas,sigrad_qe,sigrad_inel,t1,t2,t3,t4,t5,t6
        Wsq = Mp**2 + 2d0*Mp*(e-ep) - q2_N_59_3(j)
        fac_N_59_3(i,j) = sigrad_all/sigborn
         if(Wsq.gt.0) then
          W_N_59_3(i) = sqrt(Wsq)
         else
          write(*,*) ' problem with W2 = ',Wsq
          stop
         endif
         write(82,101) W_N_59_3(i),q2_N_59_3(j),fac_N_59_3(i,j)
     >,ep,theta,sigborn
       enddo
       enddo    
c
         WRITE(*,*) ' READING IN THE 4th RADIATED XN FILE'
      close(21)
         write(*,*) filename4
      open(unit=21,file=filename4)
      read(21,*)wlo_N_59_4,whi_N_59_4,q2lo_N_59_4,q2hi_N_59_4 
      write(*,*) wpt_N_59_4,q2pt_N_59_4,wlo_N_59_4,whi_N_59_4,q2lo_N_59_4,q2hi_N_59_4 
       do i=1,wpt_N_59_4
        do j=1,q2pt_N_59_4
         read(21,'(1x,f7.3,f7.4,f7.3,2f7.4,6e11.4,3i3,2e11.4)') 
     &            e,ep,theta,xbnuc,q2_N_59_4(j),sigborn,sigrad_all,
     &            sigrad_elas,sigrad_qe,sigrad_inel,t1,t2,t3,t4,t5,t6
        Wsq = Mp**2 + 2d0*Mp*(e-ep) - q2_N_59_4(j)
        fac_N_59_4(i,j) = sigrad_all/sigborn
         if(Wsq.gt.0) then
          W_N_59_4(i) = sqrt(Wsq)
         else
          write(*,*) ' problem with W2 = ',Wsq
          stop
         endif
         write(82,101) W_N_59_4(i),q2_N_59_4(j),fac_N_59_4(i,j)
     >,ep,theta,sigborn
       enddo
       enddo    
c
       FIRST=.FALSE.      
c  calc 2nd derivative in y2. Call only once.
       call splie2(w_N_59_1,q2_N_59_1,fac_N_59_1,wpt_N_59_1,q2pt_N_59_1,y2_N_59_1)     
       call splie2(w_N_59_2,q2_N_59_2,fac_N_59_2,wpt_N_59_2,q2pt_N_59_2,y2_N_59_2)     
       call splie2(w_N_59_3,q2_N_59_3,fac_N_59_3,wpt_N_59_3,q2pt_N_59_3,y2_N_59_3)     
       call splie2(w_N_59_4,q2_N_59_4,fac_N_59_4,wpt_N_59_4,q2pt_N_59_4,y2_N_59_4)     
      endif
C
      wtest=sqrt(wsqtest)
      if ( wtest .gt. wlo_N_59_1 .and. wtest .lt. whi_N_59_1 .and.
     >  qsqtest .gt. q2lo_N_59_1 .and. qsqtest .lt. q2hi_N_59_1) then
      call splin2(w_N_59_1,q2_N_59_1,fac_N_59_1,y2_N_59_1,wpt_N_59_1,q2pt_N_59_1,wtest,qsqtest,radrat)
      endif
      if ( wtest .gt. wlo_N_59_2 .and. wtest .lt. whi_N_59_2 .and.
     >  qsqtest .gt. q2lo_N_59_2 .and. qsqtest .lt. q2hi_N_59_2) then
      call splin2(w_N_59_2,q2_N_59_2,fac_N_59_2,y2_N_59_2,wpt_N_59_2,q2pt_N_59_2,wtest,qsqtest,radrat)
      endif
      if ( wtest .gt. wlo_N_59_3 .and. wtest .lt. whi_N_59_3 .and.
     >  qsqtest .gt. q2lo_N_59_3 .and. qsqtest .lt. q2hi_N_59_3) then
      call splin2(w_N_59_3,q2_N_59_3,fac_N_59_3,y2_N_59_3,wpt_N_59_3,q2pt_N_59_3,wtest,qsqtest,radrat)
      endif
      if ( wtest .gt. wlo_N_59_4 .and. wtest .lt. whi_N_59_4 .and.
     >  qsqtest .gt. q2lo_N_59_4 .and. qsqtest .lt. q2hi_N_59_4) then
      call splin2(w_N_59_4,q2_N_59_4,fac_N_59_4,y2_N_59_4,wpt_N_59_4,q2pt_N_59_4,wtest,qsqtest,radrat)
      endif
c
      radval=radrat
      return
      end
