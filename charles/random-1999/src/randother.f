! Version 1.3 of random number routines
! Author: Charles Karney <karney@princeton.edu>
! Date: 1999-10-05 11:15 -0400
!
      subroutine random_gauss(y,n,ri,ra)
      implicit none
      integer ri
      DOUBLE PRECISION ra(0:100-1)
      integer n
      DOUBLE PRECISION y(0:n-1)
      integer i
      DOUBLE PRECISION pi,theta,z
      DOUBLE PRECISION random
      external random_array,random
      data pi/3.14159265358979323846264338328d0/
      Real ys(0:n-1)
      Real spi,stheta,sz
      Real srandom
      external srandom_array,srandom
      data spi/3.14159265358979323846264338328/
      if(n.LE.0)return
      call random_array(y,n,ri,ra(0))
      do i=0,int(n/2)*2-1,2
      theta=pi*(2.0d0*y(i)-1.0d0)
      z=sqrt(-2.0d0*log(y(i+1)))
      y(i)=z*cos(theta)
      y(i+1)=z*sin(theta)
      end do
      if(mod(n,2).EQ.0)return
      theta=pi*(2.0d0*y(n-1)-1.0d0)
      z=sqrt(-2.0d0*random(ri,ra(0)))
      y(n-1)=z*cos(theta)
      return
      entry srandom_gauss(ys,n,ri,ra)
      if(n.LE.0)return
      call srandom_array(ys,n,ri,ra(0))
      do i=0,int(n/2)*2-1,2
      stheta=spi*(2.0*ys(i)-1.0)
      sz=sqrt(-2.0*log(ys(i+1)))
      ys(i)=sz*cos(stheta)
      ys(i+1)=sz*sin(stheta)
      end do
      if(mod(n,2).EQ.0)return
      stheta=spi*(2.0*ys(n-1)-1.0)
      sz=sqrt(-2.0*srandom(ri,ra(0)))
      ys(n-1)=sz*cos(stheta)
      return
      end
!
      subroutine random_isodist(v,n,ri,ra)
      implicit none
      integer ri
      DOUBLE PRECISION ra(0:100-1)
      integer n
      DOUBLE PRECISION v(0:3*n-1)
      integer i
      DOUBLE PRECISION pi,costheta,phi
      external random_array
      data pi/3.14159265358979323846264338328d0/
      Real vs(0:3*n-1)
      Real spi,scostheta,sphi
      external srandom_array
      data spi/3.14159265358979323846264338328/
      if(n.LE.0)return
      call random_array(v(n),2*n,ri,ra(0))
      do i=0,n-1
      costheta=2.0d0*v(n+2*i)-1.0d0
      phi=pi*(2.0d0*v(n+2*i+1)-1.0d0)
      v(3*i)=cos(phi)*sqrt(1.0d0-costheta**2)
      v(3*i+1)=sin(phi)*sqrt(1.0d0-costheta**2)
      v(3*i+2)=costheta
      end do
      return
      entry srandom_isodist(vs,n,ri,ra)
      if(n.LE.0)return
      call srandom_array(vs(n),2*n,ri,ra(0))
      do i=0,n-1
      scostheta=2.0*vs(n+2*i)-1.0
      sphi=spi*(2.0*vs(n+2*i+1)-1.0)
      vs(3*i)=cos(sphi)*sqrt(1.0-scostheta**2)
      vs(3*i+1)=sin(sphi)*sqrt(1.0-scostheta**2)
      vs(3*i+2)=scostheta
      end do
      return
      end
!
      subroutine random_cosdist(v,n,ri,ra)
      implicit none
      integer ri
      DOUBLE PRECISION ra(0:100-1)
      integer n
      DOUBLE PRECISION v(0:3*n-1)
      integer i
      DOUBLE PRECISION pi,costheta2,phi
      external random_array
      data pi/3.14159265358979323846264338328d0/
      Real vs(0:2*n-1)
      Real spi,scostheta2,sphi
      external srandom_array
      data spi/3.14159265358979323846264338328/
      if(n.LE.0)return
      call random_array(v(n),2*n,ri,ra(0))
      do i=0,n-1
      costheta2=v(n+2*i)
      phi=pi*(2.0d0*v(n+2*i+1)-1.0d0)
      v(3*i)=cos(phi)*sqrt(1.0d0-costheta2)
      v(3*i+1)=sin(phi)*sqrt(1.0d0-costheta2)
      v(3*i+2)=sqrt(costheta2)
      end do
      return
      entry srandom_cosdist(vs,n,ri,ra)
      if(n.LE.0)return
      call srandom_array(vs(n),2*n,ri,ra(0))
      do i=0,n-1
      scostheta2=vs(n+2*i)
      sphi=spi*(2.0*vs(n+2*i+1)-1.0)
      vs(3*i)=cos(sphi)*sqrt(1.0-scostheta2)
      vs(3*i+1)=sin(sphi)*sqrt(1.0-scostheta2)
      vs(3*i+2)=sqrt(scostheta2)
      end do
      return
      end
!
