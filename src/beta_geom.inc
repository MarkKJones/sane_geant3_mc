C-- beta_geom.inc        Glen Warren  8/03
C--
C-- details geometry of BETA detector

C Detector geometry

      integer*4 horz_blocks,vert_blocks
      real*4 block_height, block_width
      real*4 cal_height,cal_width
      real*4 cal_depth
      real*4 cer_length

      parameter( block_height =   4.d0 )
      parameter( block_width  =   4.d0 )
      parameter( horz_blocks  =  32    )
      parameter( vert_blocks  =  58    )
      parameter( cal_depth    =  40.d0 )
      parameter( cer_length   = 150.d0 )

      parameter( cal_height   = vert_blocks*block_height )
      parameter( cal_width    = horz_blocks*block_width  )

      real*4 gain_thk

      parameter( gain_thk = 1./2.)

      real*4 cer_win_thk

      parameter( cer_win_thk = 0.0127 )

      real*4 hodo_thk

      parameter( hodo_thk = 3.75)

C Detector Setup

      real*4 eff_cal_drift
      real*4 cal_drift 
      real*4 cer_drift
      real*4 front_width
      real*4 fronthodo_drift

      parameter( eff_cal_drift = 335.d0 ) ! used in reconstruction
      parameter( cal_drift    = 335.d0 )
      parameter( cer_drift    =  55.d0 )
      parameter( front_width  =  0.3   )
      parameter( fronthodo_drift  =  52.0   )
