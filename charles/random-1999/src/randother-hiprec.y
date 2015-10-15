! Version 1.3 of random number routines
! Author: Charles Karney <karney@princeton.edu>
! Date: 1999-10-05 11:15 -0400
!
      subroutine random_gauss(y,n,ri,ra)
      implicit none
      integer ri
      REAL8 ra(0:100-1)
      integer n
      REAL8 y(0:n-1)
      integer i
      REAL8 pi,theta,z
      REAL8 random
      external random_array,random
      data pi/PI/
      entry srandom_gauss(y,n,ri,ra)
      if(n.LE.0)return
      call random_array(y,n,ri,ra(0))
      do i=0,int(n/2)*2-1,2
      theta=pi*(TWO*y(i)-ONE)
      z=sqrt(-TWO*log(y(i+1)))
      y(i)=z*cos(theta)
      y(i+1)=z*sin(theta)
      end do
      if(mod(n,2).EQ.0)return
      theta=pi*(TWO*y(n-1)-ONE)
      z=sqrt(-TWO*random(ri,ra(0)))
      y(n-1)=z*cos(theta)
      return
      end
!
      subroutine random_isodist(v,n,ri,ra)
      implicit none
      integer ri
      REAL8 ra(0:100-1)
      integer n
      REAL8 v(0:3*n-1)
      integer i
      REAL8 pi,costheta,phi
      external random_array
      data pi/PI/
      entry srandom_isodist(v,n,ri,ra)
      if(n.LE.0)return
      call random_array(v(n),2*n,ri,ra(0))
      do i=0,n-1
      costheta=TWO*v(n+2*i)-ONE
      phi=pi*(TWO*v(n+2*i+1)-ONE)
      v(3*i)=cos(phi)*sqrt(ONE-costheta**2)
      v(3*i+1)=sin(phi)*sqrt(ONE-costheta**2)
      v(3*i+2)=costheta
      end do
      return
      end
!
      subroutine random_cosdist(v,n,ri,ra)
      implicit none
      integer ri
      REAL8 ra(0:100-1)
      integer n
      REAL8 v(0:3*n-1)
      integer i
      REAL8 pi,costheta2,phi
      external random_array
      data pi/PI/
      entry srandom_cosdist(v,n,ri,ra)
      if(n.LE.0)return
      call random_array(v(n),2*n,ri,ra(0))
      do i=0,n-1
      costheta2=v(n+2*i)
      phi=pi*(TWO*v(n+2*i+1)-ONE)
      v(3*i)=cos(phi)*sqrt(ONE-costheta2)
      v(3*i+1)=sin(phi)*sqrt(ONE-costheta2)
      v(3*i+2)=sqrt(costheta2)
      end do
      return
      end
!
