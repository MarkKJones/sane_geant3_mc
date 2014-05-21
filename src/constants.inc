      real*8 cc
      real*8 d2r
      real*8 Q_E, N_A

      parameter (cc  = 29.9792458d0)
      parameter (d2r = 3.14159d0/180.d0)
      PARAMETER (Q_E = 1.602d00)            !e- charge in uCoul (*1E-13)
      PARAMETER (N_A = 6.022d00)            !Avogadro's number (*1E+23)

      integer*2 num_particles
      parameter(num_particles = 9)

      real*8 mass(num_particles)
      integer*2 part_charge(num_particles)
      character*3 part_id(num_particles)

      data mass/0.000511d0,0.000511d0,0.13957d0,0.13957d0,0.13498d0,
     1          0.93827d0,0.93957d0,0.493677d0,0.493677d0/
      data part_charge/-1,+1,-1,+1,0,+1,0,+1,-1/
      data part_id/'E-','E+','PI-','PI+','PI0','P','N','K+','K-'/
