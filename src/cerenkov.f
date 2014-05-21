      subroutine digi_cer(iclust)
      implicit none
      include 'beta_geom.inc'
      include 'sane_cwn.inc'
      integer iclust
      integer i
      real predicted_x,predicted_y
      integer mirror(8)
      real CER_SANE_GEOM_CUT_LOW(8) 
      real CER_SANE_GEOM_CUT_HI(8) 
      integer CER_SANE_GEOM_CUT_X(8)
      data CER_SANE_GEOM_CUT_LOW /0,   0,  10,  10,  
     ,     22, 22,  35,  35/     
      data CER_SANE_GEOM_CUT_HI /22,  22,  42, 42, 
     ,     54,  54,  56, 56/
      data CER_SANE_GEOM_CUT_X /1,  -1,  1, -1, 
     ,	1,  -1,  1, -1/
      integer cer_n
      mirror = 0
      cer_h(iclust)=0
      predicted_x = cwn_x_ur(iclust)
      predicted_y = cwn_y_ur(iclust)
      do i=1,cwn_cergood
         do cer_n=1,8
         if(predicted_y.gt.(CER_SANE_GEOM_CUT_LOW(cer_n)*4-1)-120..and.
     ,        predicted_y.lt.(CER_SANE_GEOM_CUT_HI(cer_n)*4+1)-120.and.
     ,              predicted_x*CER_SANE_GEOM_CUT_X(cer_n).gt.-20)then
            if(cwn_cg_y(i).gt.
     ,           ((CER_SANE_GEOM_CUT_LOW(cer_n)*4-1)-120)*205/335..and.
     ,           cwn_cg_y(i).lt.
     ,           ((CER_SANE_GEOM_CUT_HI(cer_n)*4-1)-120)*205/335..and.
     , cwn_cg_x(i)*CER_SANE_GEOM_CUT_X(cer_n).gt.-20*205/335.
     ,       )then
c            write(*,*)predicted_y,predicted_x,
c     ,           cwn_cg_y(i),cwn_cg_x(i),
c     ,           cwn_cergood,i,cer_n
               cer_h(iclust)=cer_h(iclust)+1
            endif
         endif
      enddo
      enddo
      

      

      end
      subroutine clear_cer()
      include 'sane_cwn.inc'
      cwn_cergood=0

      do i=1,1000
         cwn_cg_y(i)=0
         cwn_cg_x(i)=0
         cwn_cg_xp(i)=0
         cwn_cg_yp(i)=0
      enddo
      end
