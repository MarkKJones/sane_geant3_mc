      program bbbrem

      implicit double precision(a-h,o-z)
      common / experc / roots,rk0
      common / labmom / p1(4),p2(4),q1(4),q2(4),qk(4),d1,d2,t,weight
      common / ranchn / nran
* this is the original file received from R. Kleiss
 
* read input parameters
      read(*,*) roots
      read(*,*) rk0
      read(*,*) nevent
      read(*,*) nran
 
* iitialize bookkeeping
      wmax = 0
      wnil = 0
      wneg = 0
      w0 = 0
      w1 = 0
      w2 = 0
 
* start of event generation loop
      do 9999 k = 1,nevent
 
* generate an event
        call bcube
 
* bookkeeping on the weight
        if(weight.gt.wmax) wmax = weight
        if(weight.eq.0d0) wnil = wnil+1
        if(weight.lt.0d0) wneg = wneg+1
        w0 = w0+1
        w1 = w1+weight
        w2 = w2+weight*weight
 
* end of event generation loop
 9999 continue
 
* analyze weight distribution, and print results
      smc = w1/w0
      ser = dsqrt(w2-w1*w1/w0)/w0
      ranchk = random(0d0)
      write(*,1) ranchk,w0,w1,w2,wmax,wnil,wneg,smc,ser
    1 format(' ******** cross section evaluation ********'/,
     .       ' random number check       ',f15.13/,
     .       ' total # of events         ',f15.0/,
     .       ' sum of weights            ',d15.6/,
     .       ' sum of (weight**2)s       ',d15.6/,
     .       ' max weight occurred       ',d15.6/,
     .       ' # of zero weights         ',f15.0/,
     .       ' # of negative weights     ',f15.0/,
     .       ' computed  cross section   ',d15.6,' millibarn'/,
     .       '                   +/-     ',d15.6,' millibarn'/,
     .       ' ******************************************')
      end
 
      subroutine bcube
      implicit logical(a-z)
      double precision
     . roots,rk0,p1,p2,q1,q2,qk,alpha,rme,tomb,pi,twopi,
     . s,rme2,rme2s,rls,z0,a1,a2,ac,sigapp,eb,pb,rin2pb,random,
     . z,y,q0,temp1,tmin,tmax,sy,w2,rlamx,b,t,rlam,eps,rl,vgam,
     . cgam,sgam,phi,phig,ql,qt,q(4),r0,w,rin2w,rinr0w,eta,phat1,
     . phat2,phat3,phatt,phatl,sfhat,cfhat,sthat,vthat,cthat,sfg,
     . cfg,temp2,veg,qkhat(4),c1,vnumx,vdenx,vnumn,vdenn,c2,rlabl,
     . rhat4,etar,rhat1,rhat2,rhat3,zz,s1,d1,d2,rind1,rind12,
     . rind2,rind22,aa0,aa1,aa2,aa3,aa4,rmex,rmap,weight
      integer init
      save
 
* experimental constants
      common / experc / roots,rk0
* momenta in the lab frame
      common / labmom / p1(4),p2(4),q1(4),q2(4),qk(4),d1,d2,t,weight
 
      data init/0/
* start of initialization
      if(init.eq.0) then
        init = 1
        write(*,901)
  901   format(' ******************************************'/,
     .         ' *** bbbrem : beam-beam bremsstrahlung  ***'/,
     .         ' *** authors r. kleiss and h. burkhardt ***'/,
     .         ' ******************************************')
 
* physical constants
        alpha = 1d0/137.036d0
        rme = 0.51099906d-03
        tomb = 3.8937966d05 /1d6
 
* mathematical constants
        pi = 4*datan(1d0)
        twopi = 2*pi
 
* derived constants
        write(*,902) roots,rk0
  902   format(' total energy              ',f15.6,' gev'/,
     .         ' minimum photon energy     ',f15.6,' * beam energy')
        if(rk0.le.0d0.or.rk0.ge.1d0) then
          write(*,*) 'wrong value for rk0'
          stop
        endif
        s = roots**2
        rme2 = rme**2
        rme2s = rme2/s
        rls = -dlog(rme2s)
        z0 = rk0/(1-rk0)
 
* approximate total cross section
        a1 = dlog((1+z0)/z0)
        a2 = (dlog(1+z0))/z0
        ac = a1/(a1+a2)
        sigapp = 8*alpha**3/rme2*(-dlog(rme2s))*(a1+a2)*tomb
        write(*,903) sigapp
  903   format(' approximate cross section ',d15.6,' millibarn')
 
* the initial-state momenta
        eb = roots*0.5d0
        pb = dsqrt(eb*eb-rme2)
        rin2pb = 0.5d0/pb
        p1(1) = 0
        p1(2) = 0
        p1(3) = -pb
        p1(4) = eb
        q1(1) = 0
        q1(2) = 0
        q1(3) = pb
        q1(4) = eb
 
* end of initialization
      endif
 
* generate z
      if(random(1).lt.ac) then
         temp1=random(2)
         z = 1d0/(temp1*(dexp(a1*random(3))-1))
      else
        z = z0/random(4)
      endif
 
* bounds on t
      y = rme2s*z
      q0 = eb*y
      temp1 = pb*pb-eb*q0
      temp2 = temp1*temp1-rme2-q0*q0
* exit if temp2<0 (very very rare): put weight to 0
* the `else' clause extends to the end of the routine
      if(temp2.lt.0d0) then
        write(*,904) temp2
  904   format(' y too large: delta_t^2 = ',d15.6)
        weight = 0d0
      else
        tmin = -2*(temp1+dsqrt(temp2))
        tmax = rme2*s*y*y/tmin
 
* generate t
        sy = s*y
        w2 = sy+rme2
        temp1 = sy+tmax
        rlamx = dsqrt(temp1*temp1-4*w2*tmax)
        if(temp1.le.0d0) then
          temp1 = rlamx-temp1
        else
          temp1 = -4*w2*tmax/(rlamx+temp1)
        endif
    1   continue
          b = dexp(random(5)*dlog(1+2*sy/temp1))
          t = -b*z*z*rme2/((b-1)*(b*z+b-1))
          if(t.lt.tmin) then
            write(*,905) t,tmin
  905       format(' t = ',d15.6,'   < t_min =',d15.6)
            goto 1
          endif
        continue
 
* generate cgam
        rlam = dsqrt((sy-t)*(sy-t)-4*rme2*t)
        eps = 4*rme2*w2/(rlam*(rlam+w2+rme2-t))
        rl = dlog((2+eps)/eps)
        vgam = eps*(dexp(random(6)*rl)-1)
        cgam = 1-vgam
        sgam = dsqrt(vgam*(2-vgam))
 
* generate azimuthal angles
        phi = twopi*random(7)
        phig = twopi*random(8)
 
* construct momentum transfer q(mu)
        ql = (2*eb*q0-t)*rin2pb
        qt = dsqrt((tmax-t)*(t-tmin))*rin2pb
        q(1) = qt*dsin(phi)
        q(2) = qt*dcos(phi)
        q(3) = ql
        q(4) = q0
 
* construct momentum of outgoing positron in lab frame
        q2(1) = q1(1)-q(1)
        q2(2) = q1(2)-q(2)
        q2(3) = q1(3)-q(3)
        q2(4) = q1(4)-q(4)
 
* find euler angles of p1(mu) in cm frame
        r0 = eb+q0
        w = dsqrt(w2)
        rin2w = 0.5d0/w
        rinr0w = 1d0/(r0+w)
        eta = -(sy+2*w*q0+t)*rin2w*rinr0w
        phat1 = -q(1)*(1+eta)
        phat2 = -q(2)*(1+eta)
        phat3 = pb*eta-ql*(1+eta)
        phatl = rlam*rin2w
        phatt = dsqrt(phat1*phat1+phat2*phat2)
        sfhat = phat1/phatt
        cfhat = phat2/phatt
        sthat = phatt/phatl
        if(phat3.gt.0d0) then
          vthat = sthat*sthat/(1-dsqrt(1-sthat*sthat))
        else
          vthat = sthat*sthat/(1+dsqrt(1-sthat*sthat))
        endif
        cthat = vthat-1
 
* rotate using these euler angles to get the qk direction in the cm
        sfg = dsin(phig)
        cfg = dcos(phig)
        temp1 = sgam*sfg
        temp2 = cthat*sgam*cfg+sthat*cgam
        veg = vthat+vgam-vthat*vgam-sthat*sgam*cfg
        qkhat(4) = sy*rin2w
        qkhat(1) = qkhat(4)*( cfhat*temp1+sfhat*temp2)
        qkhat(2) = qkhat(4)*(-sfhat*temp1+cfhat*temp2)
        qkhat(3) = qkhat(4)*(veg-1)
 
* boost the photon momentum to the lab frame
        temp1 = pb*qkhat(3)
        if(temp1.gt.0d0) then
          temp2 = (rme2*qkhat(4)*qkhat(4)
     .            +pb*pb*(qkhat(1)*qkhat(1)+qkhat(2)*qkhat(2)))
     .            /(eb*qkhat(4)+temp1)
        else
          temp2 = eb*qkhat(4)-temp1
        endif
        qk(4) = (temp2+qkhat(4)*q(4)+qkhat(1)*q(1)
     .                +qkhat(2)*q(2)+qkhat(3)*q(3))/w
        temp1 = (qk(4)+qkhat(4))*rinr0w
        qk(1) = qkhat(1)+temp1*q(1)
        qk(2) = qkhat(2)+temp1*q(2)
        qk(3) = qkhat(3)+temp1*(-pb+ql)
 
* construct p2 by momentum conservation
        p2(1) = -q2(1)-qk(1)
        p2(2) = -q2(2)-qk(2)
        p2(3) = -q2(3)-qk(3)
        p2(4) = -q2(4)-qk(4)+roots
 
* impose cut on the photon energy: qk(4)>eb*rk0
        if(qk(4).lt.eb*rk0) then
          weight = 0d0
        else
 
* the event is now accepted: compute matrix element and weight
* compute fudge factor c1
          c1 = dlog(1+z)/dlog((2+eps)/eps)
 
* compute fudge factor c2
          temp1 = sy-tmax
          vnumx = dsqrt(temp1*temp1-4*rme2*tmax)+temp1
          temp1 = sy+tmax
          if(temp1.lt.0d0) then
            vdenx = dsqrt(temp1*temp1-4*w2*tmax)-temp1
          else
            vdenx = -4*w2*tmax/(dsqrt(temp1*temp1-4*w2*tmax)+temp1)
          endif
          temp1 = sy-tmin
          vnumn = dsqrt(temp1*temp1-4*rme2*tmin)+temp1
          temp1 = sy+tmin
          if(temp1.lt.0d0) then
            vdenn = dsqrt(temp1*temp1-4*w2*tmin)-temp1
          else
            vdenn = -4*w2*tmin/(dsqrt(temp1*temp1-4*w2*tmin)+temp1)
          endif
          c2 = 2*rls/dlog((vnumx*vdenn)/(vdenx*vnumn))
 
* compute vector (small) r in cm frame, and (big) z
          rlabl = (t-2*rme2*y)*rin2pb
          rhat4 = -(2*rme2*y+(1-y)*t)*rin2w
          etar = rhat4*rinr0w
          rhat1 = -q(1)*(1+etar)
          rhat2 = -q(2)*(1+etar)
          rhat3 = rlabl+(pb-ql)*etar
          zz = s*(rhat4*qkhat(4)-rhat1*qkhat(1)
     .         -rhat2*qkhat(2)-rhat3*qkhat(3))
 
* the other invariants
          s1 = 4*eb*(eb-qk(4))
          d1 = sy*rlam*(eps+vgam)*rin2w*rin2w
          d2 = 0.5d0*sy
 
* the exact matrix element
          rind1 = 1d0/d1
          rind12 = rind1*rind1
          rind2 = 1d0/d2
          rind22 = rind2*rind2
c* kleiss-burkhardt cross section multiplied by t**2
          temp1 = s+t-2*d2
          temp2 = s1+t+2*d1
          aa0 = (s*s+s1*s1+temp1*temp1+temp2*temp2)*rind1*rind2*(-t)
          aa1 = -4*rme2*zz*zz*rind12*rind22
          aa2 = -8*rme2*(d1*d1+d2*d2)*rind1*rind2
          aa3 = 16*rme2*rme2*(d1-d2)*zz*rind12*rind22
          aa4 = -16*rme2*rme2*rme2*(d1-d2)*(d1-d2)*rind12*rind22
          rmex = aa0+aa1+aa2+aa3+aa4
 
* the approximate matrix element without c1,2, multiplied by t**2
          rmap = 4*s*s*rind1*rind2*(-t)*c1*c2
 
* the weight
          weight = rmex/rmap*sigapp
 
* the weight is now defined for both accepted and rejected events
        endif
      endif
 1001 format(3(a10,d15.6))
      end
 
      function random(r)
      implicit double precision(a-h,o-z)
      common / ranchn / nran
      save
      data init/0/
      if(init.eq.0) then
        init = 1
        if(nran.eq.1) then
          write(*,*) ' random: very crude random numbers chosen'
        elseif(nran.eq.2) then
          write(*,*) ' random: quasi-random numbers chosen'
        else
          write(*,*) ' random: error: nran = ',nran,' not allowed'
          stop
        endif
      endif
      if(nran.eq.1) then
        random = rando1(r)
      elseif(nran.eq.2) then
        random = rando2(r)
      endif
      end
 
      function rando1(r)
      implicit double precision(a-h,o-z)
      save
      data init/0/
      if(init.eq.0) then
        init = 1
        m = 2**20
        rm = 1d0/(1d0*m)
        ia = 2**9+5
        ic = 1
        iseed = 100461
      endif
    1 iseed = mod(ia*iseed+ic,m)
      if(iseed.eq.0) goto 1
      rando1 = iseed*rm
      end
 
      function rando2(r)
      implicit double precision(a-h,o-z)
      save
      parameter(n = 3)
      dimension nprime(10),s(10)
      data init/0/
      data nprime/2,3,5,7,11,13,17,19,23,29/
      if(init.eq.0) then
        init = 1
        do 1 k = 1,n
          s(k) = dsqrt(nprime(k)*1d0)
    1   continue
      endif
      do 3 k = 1,n
        ss = 0
        do 2 j=1,n
          ss = ss+s(j)
    2   continue
        s(k) = dmod(ss,1d0)
    3 continue
      rando2 = s(1)
      end
