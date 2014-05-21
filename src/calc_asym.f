      real*8 function calc_asym(E,Ep,theta,theta_N,f1p,f2p)

      implicit none

      real*8 E,Ep,theta,theta_N
      real*8 tht,thtn
      real*8 Q2,x,W2

      real*8 calc_a1p,calc_a2p

      real*8 eta,zeta,d,DD
      real*8 gamma,R,epsilon
      real*8 F1p,F2p,F1n,F2n
      real*8 F1p_2x,F1n_2x,a1p,a2p

      real*8 A1_coeff,A2_coeff
      real*8 Mp
      parameter(Mp=0.9384d0)


      tht  = theta*3.14159d0/180.d0
      thtn = theta_N*3.14159d0/180.d0

      
      Q2 = 4*E*Ep*(sin(tht/2))**2
      x  = Q2/2.d0/Mp/(E-Ep)
      W2 = 2.d0*Mp*(E-Ep) + Mp**2 - Q2

c      call mrst_sub(Q2,x,f1p_2x,f2p,f1n_2x,f2n)


c      f1p = f1p_2x/2/x
c      f1n = f1n_2x/2/x

      a1p = calc_a1p(x)
      a2p = calc_a2p(x)

      gamma = 2*Mp*x/sqrt(Q2)
      epsilon = 1/(1+2*(1+1/gamma**2)*(tan(tht/2))**2)

      calc_asym = 0
      if(F1p.ne.0)then
         R    = (1+gamma**2)*F2p / 2/x/F1p-1
         DD   = (1-Ep*epsilon/E) / (1+epsilon*R)
         d    = DD*sqrt(2*epsilon/(1+epsilon))
         eta  = epsilon*sqrt(Q2) / (E-Ep*epsilon)
         zeta = eta*(1+epsilon) / 2/epsilon
         

         
         A1_coeff = DD*cos(thtn)-d*zeta*sin(thtn)
         A2_coeff = DD*eta*cos(thtn)+d*sin(thtn)
         
         calc_asym = a1_coeff*A1p + a2_coeff*A2p
      endif
      return
      end


C------------------------------------------------------------------------------

      real*8 function calc_a1p(x)

      implicit none

      real*8 x

      calc_a1p = 0.0354 + 1.371*x - 0.427*x*x

      return
      end

C------------------------------------------------------------------------------

      real*8 function calc_a2p(x)

      implicit none

      real*8 x

      calc_a2p = -0.0051 + 0.241*x

      return
      end
