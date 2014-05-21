*CMZ :          18/08/94  17.07.20  by  S.Ravndal
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE UHINIT
C
C To book the user's histograms
C

*KEEP,PVOLUM.
      COMMON/PVOLUM/ NL,NR,IMAT,X0
*KEND.

C
C Histograms for shower development
C
      NBINZ=NL+1
      NBINR=NR+1
      ZMAX=NBINZ+1
      RMAX=NBINR+1
C
*      CALL HBOOK1(1,'Energy deposited in Air',500, 0.,30., 0.0)
*      CALL HBOOK1(2,'Energy deposited in Pb',500, 0.,70., 0.0)
*      CALL HBOOK1(3,'Energy deposited in Cal',500, 0.,2500., 0.0)
*      CALL HBOOK1(4,'Energy deposited in Veto',500, 0.,100., 0.0)
*      CALL HBOOK2(5,'Energy Deposited dE vs E',50,0.,100.,50,0.,2000.,0.0)
C
C Rescale bin size
C
      CALL HBIGBI(0,4)
C
      END
