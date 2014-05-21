      Subroutine nneurale(x, index)
      implicit double precision (a-h,n-z)
      double precision x(12),neural
      double precision FixX,FixY,FixE
      common/SNEU/FixX,FixY,FixE

C --- Last Layer
      if (index.eq.0) then
          neural=neuron0x949f748(x);
          FixE=neural 
      else
          neural=0.d0
      endif
      end
C --- First and Hidden layers
      double precision function neuron0x948f248(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x948f248 = (x(1) - 0d0)/1d0
      end
      double precision function neuron0x949d7e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949d7e8 = (x(2) - 0d0)/1d0
      end
      double precision function neuron0x949d8e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949d8e8 = (x(3) - 0d0)/1d0
      end
      double precision function neuron0x949da78(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949da78 = (x(4) - 0d0)/1d0
      end
      double precision function neuron0x949dc50(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949dc50 = (x(5) - 0d0)/1d0
      end
      double precision function neuron0x949de28(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949de28 = (x(6) - 0d0)/1d0
      end
      double precision function neuron0x949e000(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949e000 = (x(7) - 0d0)/1d0
      end
      double precision function neuron0x949e1f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949e1f8 = (x(8) - 0d0)/1d0
      end
      double precision function neuron0x949e3f0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949e3f0 = (x(9) - 0d0)/1d0
      end
      double precision function neuron0x949e5e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949e5e8 = (x(10) - 0d0)/1d0
      end
      double precision function neuron0x949e7e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949e7e0 = (x(11) - 0d0)/1d0
      end
      double precision function neuron0x949e9b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949e9b8 = (x(12) - 0d0)/1d0
      end
      double precision function neuron0x949ecc8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949ecc8 = -0.485176d0
      neuron0x949ecc8 = neuron0x949ecc8 + synapse0x948f0c8(x)
      neuron0x949ecc8 = neuron0x949ecc8 + synapse0x949ee58(x)
      neuron0x949ecc8 = neuron0x949ecc8 + synapse0x949ee80(x)
      neuron0x949ecc8 = neuron0x949ecc8 + synapse0x949eea8(x)
      neuron0x949ecc8 = neuron0x949ecc8 + synapse0x949eed0(x)
      neuron0x949ecc8 = neuron0x949ecc8 + synapse0x949eef8(x)
      neuron0x949ecc8 = neuron0x949ecc8 + synapse0x949ef20(x)
      neuron0x949ecc8 = neuron0x949ecc8 + synapse0x949ef48(x)
      neuron0x949ecc8 = neuron0x949ecc8 + synapse0x949ef70(x)
      neuron0x949ecc8 = neuron0x949ecc8 + synapse0x949ef98(x)
      neuron0x949ecc8 = neuron0x949ecc8 + synapse0x949efc0(x)
      neuron0x949ecc8 = neuron0x949ecc8 + synapse0x949efe8(x)
      neuron0x949ecc8= (exp(-neuron0x949ecc8*neuron0x949ecc8))
      end
      double precision function neuron0x949f010(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949f010 = -0.279117d0
      neuron0x949f010 = neuron0x949f010 + synapse0x949f1e8(x)
      neuron0x949f010 = neuron0x949f010 + synapse0x949f210(x)
      neuron0x949f010 = neuron0x949f010 + synapse0x949f238(x)
      neuron0x949f010 = neuron0x949f010 + synapse0x949f260(x)
      neuron0x949f010 = neuron0x949f010 + synapse0x949f288(x)
      neuron0x949f010 = neuron0x949f010 + synapse0x949f338(x)
      neuron0x949f010 = neuron0x949f010 + synapse0x949f360(x)
      neuron0x949f010 = neuron0x949f010 + synapse0x949f388(x)
      neuron0x949f010 = neuron0x949f010 + synapse0x949f3b0(x)
      neuron0x949f010 = neuron0x949f010 + synapse0x949f3d8(x)
      neuron0x949f010 = neuron0x949f010 + synapse0x949f400(x)
      neuron0x949f010 = neuron0x949f010 + synapse0x949f428(x)
      neuron0x949f010= (exp(-neuron0x949f010*neuron0x949f010))
      end
      double precision function neuron0x949f450(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949f450 = 0.713893d0
      neuron0x949f450 = neuron0x949f450 + synapse0x949f5e0(x)
      neuron0x949f450 = neuron0x949f450 + synapse0x949f608(x)
      neuron0x949f450 = neuron0x949f450 + synapse0x949f630(x)
      neuron0x949f450 = neuron0x949f450 + synapse0x949f658(x)
      neuron0x949f450 = neuron0x949f450 + synapse0x949f680(x)
      neuron0x949f450 = neuron0x949f450 + synapse0x949f6a8(x)
      neuron0x949f450 = neuron0x949f450 + synapse0x949f6d0(x)
      neuron0x949f450 = neuron0x949f450 + synapse0x949f6f8(x)
      neuron0x949f450 = neuron0x949f450 + synapse0x949f720(x)
      neuron0x949f450 = neuron0x949f450 + synapse0x949f2b0(x)
      neuron0x949f450 = neuron0x949f450 + synapse0x949f2d8(x)
      neuron0x949f450 = neuron0x949f450 + synapse0x949f300(x)
      neuron0x949f450= (exp(-neuron0x949f450*neuron0x949f450))
      end
      double precision function neuron0x949f850(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949f850 = -0.664393d0
      neuron0x949f850 = neuron0x949f850 + synapse0x949fa28(x)
      neuron0x949f850 = neuron0x949f850 + synapse0x949fa50(x)
      neuron0x949f850 = neuron0x949f850 + synapse0x949fa78(x)
      neuron0x949f850 = neuron0x949f850 + synapse0x949faa0(x)
      neuron0x949f850 = neuron0x949f850 + synapse0x949fac8(x)
      neuron0x949f850 = neuron0x949f850 + synapse0x949faf0(x)
      neuron0x949f850 = neuron0x949f850 + synapse0x949fb18(x)
      neuron0x949f850 = neuron0x949f850 + synapse0x949fb40(x)
      neuron0x949f850 = neuron0x949f850 + synapse0x949fb68(x)
      neuron0x949f850 = neuron0x949f850 + synapse0x949fb90(x)
      neuron0x949f850 = neuron0x949f850 + synapse0x949fbb8(x)
      neuron0x949f850 = neuron0x949f850 + synapse0x949fbe0(x)
      neuron0x949f850= (exp(-neuron0x949f850*neuron0x949f850))
      end
      double precision function neuron0x949fc08(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949fc08 = -4.12598d0
      neuron0x949fc08 = neuron0x949fc08 + synapse0x93d0398(x)
      neuron0x949fc08 = neuron0x949fc08 + synapse0x93d03c0(x)
      neuron0x949fc08 = neuron0x949fc08 + synapse0x949fe68(x)
      neuron0x949fc08 = neuron0x949fc08 + synapse0x949fe90(x)
      neuron0x949fc08 = neuron0x949fc08 + synapse0x949feb8(x)
      neuron0x949fc08 = neuron0x949fc08 + synapse0x949fee0(x)
      neuron0x949fc08 = neuron0x949fc08 + synapse0x949ff08(x)
      neuron0x949fc08 = neuron0x949fc08 + synapse0x949ff30(x)
      neuron0x949fc08 = neuron0x949fc08 + synapse0x949ff58(x)
      neuron0x949fc08 = neuron0x949fc08 + synapse0x949ff80(x)
      neuron0x949fc08 = neuron0x949fc08 + synapse0x949ffa8(x)
      neuron0x949fc08 = neuron0x949fc08 + synapse0x949ffd0(x)
      neuron0x949fc08= (exp(-neuron0x949fc08*neuron0x949fc08))
      end
      double precision function neuron0x949fff8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949fff8 = 0.643007d0
      neuron0x949fff8 = neuron0x949fff8 + synapse0x94a01d0(x)
      neuron0x949fff8 = neuron0x949fff8 + synapse0x94a01f8(x)
      neuron0x949fff8 = neuron0x949fff8 + synapse0x94a0220(x)
      neuron0x949fff8 = neuron0x949fff8 + synapse0x94a0248(x)
      neuron0x949fff8 = neuron0x949fff8 + synapse0x94a0270(x)
      neuron0x949fff8 = neuron0x949fff8 + synapse0x93d1a88(x)
      neuron0x949fff8 = neuron0x949fff8 + synapse0x9469a08(x)
      neuron0x949fff8 = neuron0x949fff8 + synapse0x948f010(x)
      neuron0x949fff8 = neuron0x949fff8 + synapse0x948f038(x)
      neuron0x949fff8 = neuron0x949fff8 + synapse0x93d1b00(x)
      neuron0x949fff8 = neuron0x949fff8 + synapse0x93d1b28(x)
      neuron0x949fff8 = neuron0x949fff8 + synapse0x93d1c60(x)
      neuron0x949fff8= (exp(-neuron0x949fff8*neuron0x949fff8))
      end
      double precision function neuron0x949f748(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x949f748 = 0.140815d0
      neuron0x949f748 = neuron0x949f748 + synapse0x949ec38(x)
      neuron0x949f748 = neuron0x949f748 + synapse0x949ec60(x)
      neuron0x949f748 = neuron0x949f748 + synapse0x949ec88(x)
      neuron0x949f748 = neuron0x949f748 + synapse0x93d1cf8(x)
      neuron0x949f748 = neuron0x949f748 + synapse0x94a04a0(x)
      neuron0x949f748 = neuron0x949f748 + synapse0x94a04c8(x)
      end
C --- Synapses
      double precision function synapse0x948f0c8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x948f0c8=neuron0x948f248(x)*-0.297918d0
      end

      double precision function synapse0x949ee58(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ee58=neuron0x949d7e8(x)*0.9383d0
      end

      double precision function synapse0x949ee80(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ee80=neuron0x949d8e8(x)*0.40334d0
      end

      double precision function synapse0x949eea8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949eea8=neuron0x949da78(x)*-0.150518d0
      end

      double precision function synapse0x949eed0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949eed0=neuron0x949dc50(x)*0.0224875d0
      end

      double precision function synapse0x949eef8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949eef8=neuron0x949de28(x)*-0.088012d0
      end

      double precision function synapse0x949ef20(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ef20=neuron0x949e000(x)*0.0393091d0
      end

      double precision function synapse0x949ef48(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ef48=neuron0x949e1f8(x)*-0.226367d0
      end

      double precision function synapse0x949ef70(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ef70=neuron0x949e3f0(x)*-0.267928d0
      end

      double precision function synapse0x949ef98(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ef98=neuron0x949e5e8(x)*0.359789d0
      end

      double precision function synapse0x949efc0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949efc0=neuron0x949e7e0(x)*-1.24531d0
      end

      double precision function synapse0x949efe8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949efe8=neuron0x949e9b8(x)*-0.000718517d0
      end

      double precision function synapse0x949f1e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f1e8=neuron0x948f248(x)*-0.0225109d0
      end

      double precision function synapse0x949f210(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f210=neuron0x949d7e8(x)*-0.513507d0
      end

      double precision function synapse0x949f238(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f238=neuron0x949d8e8(x)*0.2316d0
      end

      double precision function synapse0x949f260(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f260=neuron0x949da78(x)*0.125943d0
      end

      double precision function synapse0x949f288(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f288=neuron0x949dc50(x)*0.146633d0
      end

      double precision function synapse0x949f338(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f338=neuron0x949de28(x)*0.512455d0
      end

      double precision function synapse0x949f360(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f360=neuron0x949e000(x)*-0.0690183d0
      end

      double precision function synapse0x949f388(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f388=neuron0x949e1f8(x)*0.938853d0
      end

      double precision function synapse0x949f3b0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f3b0=neuron0x949e3f0(x)*-0.265296d0
      end

      double precision function synapse0x949f3d8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f3d8=neuron0x949e5e8(x)*-0.490927d0
      end

      double precision function synapse0x949f400(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f400=neuron0x949e7e0(x)*0.739219d0
      end

      double precision function synapse0x949f428(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f428=neuron0x949e9b8(x)*0.664236d0
      end

      double precision function synapse0x949f5e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f5e0=neuron0x948f248(x)*-0.71888d0
      end

      double precision function synapse0x949f608(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f608=neuron0x949d7e8(x)*1.63759d0
      end

      double precision function synapse0x949f630(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f630=neuron0x949d8e8(x)*0.0557781d0
      end

      double precision function synapse0x949f658(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f658=neuron0x949da78(x)*0.544146d0
      end

      double precision function synapse0x949f680(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f680=neuron0x949dc50(x)*-0.634192d0
      end

      double precision function synapse0x949f6a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f6a8=neuron0x949de28(x)*0.0928027d0
      end

      double precision function synapse0x949f6d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f6d0=neuron0x949e000(x)*0.0596124d0
      end

      double precision function synapse0x949f6f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f6f8=neuron0x949e1f8(x)*0.0692247d0
      end

      double precision function synapse0x949f720(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f720=neuron0x949e3f0(x)*-0.18096d0
      end

      double precision function synapse0x949f2b0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f2b0=neuron0x949e5e8(x)*0.110916d0
      end

      double precision function synapse0x949f2d8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f2d8=neuron0x949e7e0(x)*-0.0192076d0
      end

      double precision function synapse0x949f300(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949f300=neuron0x949e9b8(x)*-2.19586d0
      end

      double precision function synapse0x949fa28(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fa28=neuron0x948f248(x)*-0.0493279d0
      end

      double precision function synapse0x949fa50(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fa50=neuron0x949d7e8(x)*-0.73016d0
      end

      double precision function synapse0x949fa78(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fa78=neuron0x949d8e8(x)*-0.288676d0
      end

      double precision function synapse0x949faa0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949faa0=neuron0x949da78(x)*0.392588d0
      end

      double precision function synapse0x949fac8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fac8=neuron0x949dc50(x)*0.144786d0
      end

      double precision function synapse0x949faf0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949faf0=neuron0x949de28(x)*0.162987d0
      end

      double precision function synapse0x949fb18(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fb18=neuron0x949e000(x)*0.0958989d0
      end

      double precision function synapse0x949fb40(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fb40=neuron0x949e1f8(x)*0.0673942d0
      end

      double precision function synapse0x949fb68(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fb68=neuron0x949e3f0(x)*0.285892d0
      end

      double precision function synapse0x949fb90(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fb90=neuron0x949e5e8(x)*0.00509423d0
      end

      double precision function synapse0x949fbb8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fbb8=neuron0x949e7e0(x)*1.13814d0
      end

      double precision function synapse0x949fbe0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fbe0=neuron0x949e9b8(x)*1.15771d0
      end

      double precision function synapse0x93d0398(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x93d0398=neuron0x948f248(x)*0.0208782d0
      end

      double precision function synapse0x93d03c0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x93d03c0=neuron0x949d7e8(x)*-1.2671d0
      end

      double precision function synapse0x949fe68(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fe68=neuron0x949d8e8(x)*0.0602974d0
      end

      double precision function synapse0x949fe90(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fe90=neuron0x949da78(x)*0.0658844d0
      end

      double precision function synapse0x949feb8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949feb8=neuron0x949dc50(x)*-0.140505d0
      end

      double precision function synapse0x949fee0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949fee0=neuron0x949de28(x)*-0.218412d0
      end

      double precision function synapse0x949ff08(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ff08=neuron0x949e000(x)*0.343669d0
      end

      double precision function synapse0x949ff30(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ff30=neuron0x949e1f8(x)*0.266455d0
      end

      double precision function synapse0x949ff58(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ff58=neuron0x949e3f0(x)*-0.238994d0
      end

      double precision function synapse0x949ff80(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ff80=neuron0x949e5e8(x)*0.0145861d0
      end

      double precision function synapse0x949ffa8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ffa8=neuron0x949e7e0(x)*0.234391d0
      end

      double precision function synapse0x949ffd0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ffd0=neuron0x949e9b8(x)*-0.124874d0
      end

      double precision function synapse0x94a01d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x94a01d0=neuron0x948f248(x)*0.385692d0
      end

      double precision function synapse0x94a01f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x94a01f8=neuron0x949d7e8(x)*0.59426d0
      end

      double precision function synapse0x94a0220(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x94a0220=neuron0x949d8e8(x)*0.169389d0
      end

      double precision function synapse0x94a0248(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x94a0248=neuron0x949da78(x)*-0.412984d0
      end

      double precision function synapse0x94a0270(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x94a0270=neuron0x949dc50(x)*0.199907d0
      end

      double precision function synapse0x93d1a88(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x93d1a88=neuron0x949de28(x)*-0.0196444d0
      end

      double precision function synapse0x9469a08(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9469a08=neuron0x949e000(x)*-0.316896d0
      end

      double precision function synapse0x948f010(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x948f010=neuron0x949e1f8(x)*0.0978849d0
      end

      double precision function synapse0x948f038(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x948f038=neuron0x949e3f0(x)*-0.288264d0
      end

      double precision function synapse0x93d1b00(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x93d1b00=neuron0x949e5e8(x)*0.363236d0
      end

      double precision function synapse0x93d1b28(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x93d1b28=neuron0x949e7e0(x)*-1.00389d0
      end

      double precision function synapse0x93d1c60(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x93d1c60=neuron0x949e9b8(x)*-1.27953d0
      end

      double precision function synapse0x949ec38(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ec38=neuron0x949ecc8(x)*3.34377d0
      end

      double precision function synapse0x949ec60(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ec60=neuron0x949f010(x)*-0.701039d0
      end

      double precision function synapse0x949ec88(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x949ec88=neuron0x949f450(x)*1.91608d0
      end

      double precision function synapse0x93d1cf8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x93d1cf8=neuron0x949f850(x)*-1.69414d0
      end

      double precision function synapse0x94a04a0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x94a04a0=neuron0x949fc08(x)*0.141397d0
      end

      double precision function synapse0x94a04c8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x94a04c8=neuron0x949fff8(x)*-1.49859d0
      end

