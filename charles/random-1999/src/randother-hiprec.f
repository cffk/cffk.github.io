! Version 1.3 of random number routines
! Author: Charles Karney <karney@princeton.edu>
! Date: 1999-10-05 11:15 -0400
!
      subroutine random_gauss(y,n,ri,ra)
      implicit none
      integer ri
      REAL ra(0:100-1)
      integer n
      REAL y(0:n-1)
      integer i
      REAL pi,theta,z
      REAL random
      external random_array,random
      data pi/3.14159265358979323846264338328e0/
      entry srandom_gauss(y,n,ri,ra)
      if(n.LE.0)return
      call random_array(y,n,ri,ra(0))
      do i=0,int(n/2)*2-1,2
      theta=pi*(2.0e0*y(i)-1.0e0)
      z=sqrt(-2.0e0*log(y(i+1)))
      y(i)=z*cos(theta)
      y(i+1)=z*sin(theta)
      end do
      if(mod(n,2).EQ.0)return
      theta=pi*(2.0e0*y(n-1)-1.0e0)
      z=sqrt(-2.0e0*random(ri,ra(0)))
      y(n-1)=z*cos(theta)
      return
      end
!
      subroutine random_isodist(v,n,ri,ra)
      implicit none
      integer ri
      REAL ra(0:100-1)
      integer n
      REAL v(0:3*n-1)
      integer i
      REAL pi,costheta,phi
      external random_array
      data pi/3.14159265358979323846264338328e0/
      entry srandom_isodist(v,n,ri,ra)
      if(n.LE.0)return
      call random_array(v(n),2*n,ri,ra(0))
      do i=0,n-1
      costheta=2.0e0*v(n+2*i)-1.0e0
      phi=pi*(2.0e0*v(n+2*i+1)-1.0e0)
      v(3*i)=cos(phi)*sqrt(1.0e0-costheta**2)
      v(3*i+1)=sin(phi)*sqrt(1.0e0-costheta**2)
      v(3*i+2)=costheta
      end do
      return
      end
!
      subroutine random_cosdist(v,n,ri,ra)
      implicit none
      integer ri
      REAL ra(0:100-1)
      integer n
      REAL v(0:3*n-1)
      integer i
      REAL pi,costheta2,phi
      external random_array
      data pi/3.14159265358979323846264338328e0/
      entry srandom_cosdist(v,n,ri,ra)
      if(n.LE.0)return
      call random_array(v(n),2*n,ri,ra(0))
      do i=0,n-1
      costheta2=v(n+2*i)
      phi=pi*(2.0e0*v(n+2*i+1)-1.0e0)
      v(3*i)=cos(phi)*sqrt(1.0e0-costheta2)
      v(3*i+1)=sin(phi)*sqrt(1.0e0-costheta2)
      v(3*i+2)=sqrt(costheta2)
      end do
      return
      end
!
